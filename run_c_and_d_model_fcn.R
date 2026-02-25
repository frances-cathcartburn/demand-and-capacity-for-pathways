  # This pipeline will:
  #   Load in configuration files (first for the model, then for the scenario)
  #   Perform some error checks on the config data
  #   Initialise dataframes for results
  #   Generate results
  #   Output results as CSVs
  
run_c_and_d_model <- function(wd = getwd()
                                       , model_subfolder = ""
                                       , scenario_subfolder = "") {

  ##########################################################################
  ## Load functions from source files ######################################
  ##########################################################################
  
  source(paste0(wd,"/setup_fcns.R"),local=TRUE)
  source(paste0(wd,"/forecast_fcns.R"),local=TRUE)
  source(paste0(wd,"/data_loading_fcns.R"),local=TRUE)
  
  ##########################################################################
  ## Set working directory #################################################
  ##########################################################################
  
  if (model_subfolder == ""){
    setwd(wd)
  } else {
    setwd(paste(wd,'/', model_subfolder, sep = ""))
  }
  
  ##########################################################################
  ## Define name of output files ###########################################
  ##########################################################################
  
  output_suffix <- generate_output_suffix()
  
  ##########################################################################
  ## Load config files - Model #############################################
  ##########################################################################
  
  model_statuses_raw                        <- read.csv("model_statuses.csv")
  model_events_raw                          <- read.csv("model_events.csv")
  model_event_recipient_raw                 <- read.csv("model_event_recipient.csv")
  model_event_outcome_raw                   <- read.csv("model_event_outcome.csv")
  if (file.exists("model_event_group.csv")) {
    model_event_group_raw <- read.csv("model_event_group.csv")
  }
  if (file.exists("model_pifu_percentages.csv")) {
    model_pifu_percentages_raw <- read.csv("model_pifu_percentages.csv")
  }
  if (file.exists("model_booking_rate.csv")) {
    model_booking_rate_raw <- read.csv("model_booking_rate.csv")
  }
  
  ##########################################################################
  ## Process data tables - Model - Mandatory files #########################
  ##########################################################################  
  
  statuses        <- model_statuses_raw
  events          <- model_events_raw
  event_recipient <- model_event_recipient_raw
  event_outcome   <- model_event_outcome_raw
  
  names(statuses)        <- c("status","exit","rott")
  names(events)          <- c("event")
  names(event_recipient) <- c("event","recipient")
  names(event_outcome)   <- c("event","outcome","likelihood")

  if (!("New Referral Received" %in% events$event)) { 
    events[nrow(events)+1,] <- c("New Referral Received")
  }
  
  events$event_short_name            <- make.names(events$event)
  statuses$status_short_name         <- make.names(statuses$status)
  exit_statuses <- statuses$status[tolower(statuses$exit) == "exit"]
  
  ##########################################################################
  ## Process data tables - Model - Optional files ##########################
  ##########################################################################  
  
  #Event Groups
  if (exists("model_event_group_raw")) {
    event_group_temp <- model_event_group_raw
    names(event_group_temp) <- c("event","eventgroup","priority")
    event_group <- create_event_group(events, event_group_temp)
  } else {
    event_group <- create_event_group(events)
  }
  event_group$eventgroup_short_name  <- make.names(event_group$eventgroup)
  
  #Pifu Percentages
  if (exists("model_pifu_percentages_raw")) {
    pifu_percentages <- model_pifu_percentages_raw
  } else {
    pifu_percentages <- data.frame(c(1:12),c(0.02,0.02,0.02,0.03,0.06,0.07,0.09,0.13,0.16,0.18,0.16,0.06))
  }
  names(pifu_percentages) <- c("n_months","proportion")
  
  #Booking rate
  booking_rate <- data.frame(unique(event_group$eventgroup),rep(1,length(unique(event_group$eventgroup))))
  names(booking_rate) <- c("eventgroup","rate")
  if (exists("model_booking_rate_raw")) {
    booking_rate_temp <- model_booking_rate_raw
    names(booking_rate_temp) <- c("eventgroup","rate")
    for (eg in booking_rate_temp$eventgroup) {
      booking_rate$rate[booking_rate$eventgroup == eg] <- booking_rate_temp$rate[booking_rate_temp$eventgroup == eg]
    }
  }
  
  ##########################################################################
  ## Error Checks - Model Inputs ###########################################
  ##########################################################################
  
  error_checks_model_config_files(
    wd
    ,events
    ,statuses
    ,event_recipient
    ,event_outcome
    ,event_group
    ,pifu_percentages
    ,output_suffix
    )
  
  ##########################################################################
  ## Load config files - Scenario ##########################################
  ##########################################################################
  
  if (scenario_subfolder != ""){
    setwd(paste(wd,'/', model_subfolder,'/', scenario_subfolder, sep = ""))
  }
  
  scenario_input_demand_initial_raw         <- read.csv("scenario_demand_initial.csv")
  scenario_input_capacity_and_referrals_raw <- read.csv("scenario_capacity_and_demand_referrals.csv")

  ##########################################################################
  ## Process data tables - Scenario ########################################
  ##########################################################################  
  
  demand_initial   <- scenario_input_demand_initial_raw
  demand_referrals <- scenario_input_capacity_and_referrals_raw[,c(1,2)]
  capacity_temp    <- scenario_input_capacity_and_referrals_raw[,c(1,3:length(scenario_input_capacity_and_referrals_raw))]
  
  names(demand_initial)   <- c("status","waiters")
  names(demand_referrals) <- c("date","new_referrals")
  names(capacity_temp)[1] <- "date"
  
  if ( "PIFU activated" %in% event_group$eventgroup ) {
    capacity_temp$PIFU.activated <- rep(10000000,length(capacity_temp$date))
  }
  
  ##########################################################################
  ## Error Checks - Scenario Inputs  #######################################
  ##########################################################################
  
  error_checks_scenario_config_files(
    wd
    ,events
    ,statuses
    ,event_group
    ,demand_initial
    ,capacity_temp
    ,output_suffix
  )

  ##########################################################################
  ## Apply DNA rate and initialise a capacity dataframe ####################
  ##########################################################################
  
  #If some statuses weren't given an initial demand, set to zero
  new_demand_vec       <- statuses$status[!(statuses$status %in% demand_initial$status)]
  new_demand_df        <- data.frame(new_demand_vec,rep(0,length(new_demand_vec)))
  names(new_demand_df) <- c("status","waiters")
  demand_initial       <- rbind(demand_initial,new_demand_df)
  
  distinct_eventgroups <- unique(event_group$eventgroup)
  
  dna_rate                                            <- 0.03
  booking_rate_and_dna                                <- data.frame(booking_rate$eventgroup,booking_rate$rate*(1-dna_rate))
  names(booking_rate_and_dna)                         <- c("eventgroup","rate")
  booking_rate_and_dna$eventgroup_short_name_capacity <-  paste("Capacity.",make.names(booking_rate_and_dna$eventgroup),sep='')
  for (c in names(capacity_temp)[names(capacity_temp) != "date" & names(capacity_temp) != "PIFU.activated"]){ 
    rate_for_c <- booking_rate_and_dna$rate[booking_rate_and_dna$eventgroup_short_name_capacity == c]
    capacity_temp[[c]] <- capacity_temp[[c]]*rate_for_c
  }
  capacity <- generate_capacity()
  
  ##########################################################################
  ## Define Parameters #####################################################
  ##########################################################################
  
  c <- nrow(capacity)  #number of months
  m1 <- nrow(events)   #number of events
  m2 <- nrow(statuses) #number of statuses
  start_date <- capacity$date[1]
  
  ##########################################################################
  ## Set up initial state - Initialise results dataframes to be populated ##
  ##########################################################################
  
  ##Patients waiting at the start of month
  df_waiters                    <- as.data.frame(matrix(0, ncol = m2+1, nrow = c))
  names(df_waiters)             <- c("date",statuses$status_short_name)
  df_waiters$date               <- capacity$date
  ##Event Demand
  df_event_demand               <- as.data.frame(matrix(0, ncol = m1+1-1, nrow = c))
  names(df_event_demand)        <- c("date",events$event_short_name[events$event_short_name != "New.Referral.Received"])
  df_event_demand$date          <- capacity$date
  ##Events Carried Out
  df_events_carried_out         <- as.data.frame(matrix(0, ncol = m1+1, nrow = c))
  names(df_events_carried_out)  <- c("date",events$event_short_name)
  df_events_carried_out$date    <- capacity$date
  ##Unseen waiters who had no events this month
  df_unseen                     <- as.data.frame(matrix(0, ncol = m2+1, nrow = c))
  names(df_unseen)              <- c("date",statuses$status_short_name)
  df_unseen$date                <- capacity$date
  ##Newly on this waiting list after an event this month
  df_new_wait                   <- as.data.frame(matrix(0, ncol = m2+1, nrow = c))
  names(df_new_wait)            <- c("date",statuses$status_short_name)
  df_new_wait$date              <- capacity$date
  
  ##########################################################################
  ## Set up initial state - Initialise PIFU holding areas ##################
  ##########################################################################
  
  pifu_statuses           <- event_recipient$recipient[event_recipient$event %in% event_group$event[event_group$eventgroup == "PIFU activated"]]
  
  df_pifu_holding         <- generate_df_pifu_holding()
  
  #add new columns to df_waiters to hold numbers of unactivated pifus
  for (ps in pifu_statuses) {
    pt <- statuses$status_short_name[statuses$status == ps]
    pt_u <- paste0(pt,".UnactivatedThisMonth")
    df_waiters[[pt_u]] <- rep(0,c)
  }

  df_pifu_holding_pre     <- generate_df_pifu_holding_pre()
  
  ##########################################################################
  ## Set up initial state - Put month 1 waiters into the results dataframes #
  ##########################################################################
  
  for (s in statuses$status[!(statuses$status %in% pifu_statuses)]) {
    t <- statuses$status_short_name[statuses$status == s]
    df_waiters[[t]][df_waiters$date == start_date] <- demand_initial$waiters[demand_initial$status == s]
  }

  ##########################################################################
  ## Set up initial state - Generate a distribution of outstanding PIFUs ###
  ##########################################################################
  
  df_waiters <- initialise_pifus(df_waiters)

  ##########################################################################
  ## Execute model for as many months as we've got capacity and referrals ##
  ##########################################################################
  
  for (M in c(1:nrow(capacity))) {
    
    current_date <- capacity$date[M]
    
    generate_event_demand()
    
    allocate_dynamic_capacity()

    generate_events_carried_out()
    
    generate_unseen_waiters()
    
    generate_new_waiters()
    
    if (M != nrow(capacity)) {
      generate_total_waiters_next_month()
    }
    
  }
  
  ##########################################################################
  ## Put outputs somewhere useful ##########################################
  ##########################################################################

  write.csv(df_waiters,paste(wd,"/out/df_waiters_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(df_events_carried_out,paste(wd,"/out/df_events_carried_out_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(df_event_demand,paste(wd,"/out/df_event_demand_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(df_new_wait,paste(wd,"/out/df_new_wait_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(df_unseen,paste(wd,"/out/df_unseen_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(capacity,paste(wd,"/out/capacity_",output_suffix,".csv",sep=""), row.names = FALSE)
}

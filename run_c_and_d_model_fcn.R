  # This pipeline will:
  #   Load in configuration files (first for the model, then for the scenario)
  #   Perform some error checks on the config data
  #   Initialise dataframes for results
  #   Generate results
  #   Output results as CSVs
  
run_c_and_d_model <- function(wd = getwd()
                                       , model_subfolder = ""
                                       , scenario_subfolder = "") {

  source(paste0(wd,"/forecast_fcns.R"),local=TRUE)
  source(paste0(wd,"/data_loading_fcns.R"),local=TRUE)
  
  if (model_subfolder == ""){
    setwd(wd)
  } else {
    setwd(paste(wd,'/', model_subfolder, sep = ""))
  }
  
  ##########################################################################
  ## Define a useful place to put outputs ##################################
  ##########################################################################
  
  dt_tm <- gsub(" ","_",gsub("-","",gsub(":","",substr(Sys.time(),1,19))))
  
  output_suffix <- if(model_subfolder == ""){
    paste("", dt_tm,sep="")
  } else if (scenario_subfolder == "") {
    paste("", dt_tm,"_",model_subfolder,sep="")
  } else {
    paste("", dt_tm,"_",model_subfolder,"_",scenario_subfolder,sep="")
  }
  
  ##########################################################################
  ## Load in source files - Model ##########################################
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
  
  
  #reformat model tables - I'll come back to this later ##make model_event_outcome_raw$likelihood into numeric if it isn't already
  statuses        <- model_statuses_raw
  events          <- model_events_raw
  event_recipient <- model_event_recipient_raw
  event_outcome   <- model_event_outcome_raw
  if (exists("model_event_group_raw")) {
    event_group_temp <- model_event_group_raw
  }
  if (exists("model_pifu_percentages_raw")) {
    pifu_percentages <- model_pifu_percentages_raw
  }
  
  names(statuses)        <- c("status","exit","rott")
  names(events)          <- c("event")
  names(event_recipient) <- c("event","recipient")
  names(event_outcome)   <- c("event","outcome","likelihood")
  
  if (exists("event_group_temp")) {
    names(event_group_temp) <- c("event","eventgroup","priority")
    event_group <- create_event_group(events, event_group_temp)
  } else {
    event_group <- create_event_group(events)
  }
  
  if (!exists("pifu_percentages")) {
    pifu_percentages <- data.frame(c(1:12),c(0.02,0.02,0.02,0.03,0.06,0.07,0.09,0.13,0.16,0.18,0.16,0.06))
  }
  names(pifu_percentages) <- c("n_months","proportion")
  ## Synthetic placeholder PIFU proportions are hardcoded but can be customised via an input file
  ##don't have less than 3% in any category because otherwise numbers get too small

  if (!("New Referral Received" %in% events$event)) { 
    events[nrow(events)+1,] <- c("New Referral Received")
  }
  
  events$event_short_name            <- make.names(events$event)
  statuses$status_short_name         <- make.names(statuses$status)
  event_group$eventgroup_short_name  <- make.names(event_group$eventgroup)
  
  exit_statuses <- statuses$status[tolower(statuses$exit) == "exit"]
  
  distinct_eventgroups            <- unique(event_group$eventgroup)
  
  rm(model_statuses_raw, model_events_raw, model_event_recipient_raw, model_event_outcome_raw)
  if (exists("event_group_temp")) {
    rm(model_event_group_raw)
    rm(event_group_temp)
  }
  
  ##########################################################################
  ## Basic Error Checks - Model ############################################
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
  ## Load in source files - Scenario #######################################
  ##########################################################################
  
  if (scenario_subfolder != ""){
    setwd(paste(wd,'/', model_subfolder,'/', scenario_subfolder, sep = ""))
  }
  
  scenario_input_demand_initial_raw         <- read.csv("scenario_demand_initial.csv")
  scenario_input_capacity_and_referrals_raw <- read.csv("scenario_capacity_and_demand_referrals.csv")
  
  #reformat input tables -  I'll come back to this later to deal with errors if event names aren't correct
  demand_initial   <- scenario_input_demand_initial_raw
  demand_referrals <- scenario_input_capacity_and_referrals_raw[,c(1,2)]
  capacity_temp    <- scenario_input_capacity_and_referrals_raw[,c(1,3:length(scenario_input_capacity_and_referrals_raw))]
  
  names(demand_initial)   <- c("status","waiters")
  names(demand_referrals) <- c("date","new_referrals")
  names(capacity_temp)[1] <- "date"
  
  if ( "PIFU activated" %in% event_group$eventgroup ) { ##THERE IS A PIFU EVENT)
    capacity_temp$PIFU.activated <- rep(10000000,length(capacity_temp$date))
  }
  
  rm(scenario_input_demand_initial_raw, scenario_input_capacity_and_referrals_raw) 
  
  ##########################################################################
  ## Basic Error Checks - Scenario #########################################
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
  ## Initialise a capacity dataframe #######################################
  ##########################################################################
  
  #If some statuses weren't given an initial demand, set to zero
  new_demand_vec       <- statuses$status[!(statuses$status %in% demand_initial$status)]
  new_demand_df        <- data.frame(new_demand_vec,rep(0,length(new_demand_vec)))
  names(new_demand_df) <- c("status","waiters")
  demand_initial       <- rbind(demand_initial,new_demand_df)
  #if there are the same number of events as event groups, no fiddling needed
  #if there aren't the same number of events as event groups, initialise 
  #capacity columns where all values are set to zero, so that capacity can be dynamically shared later
  #exception is PIFU - we don't limit the number of PIFUs that can be activated.
  
  dna_rate <- 0.03
  
  for (c in names(capacity_temp)[names(capacity_temp) != "date"]){
    capacity_temp[[c]] <- capacity_temp[[c]]*(1-dna_rate)
  }
  
  if (length(events$event) == length(distinct_eventgroups)) {
    capacity <- capacity_temp
  } else {
    capacity <- capacity_temp[1]
    for (eg in distinct_eventgroups[distinct_eventgroups != "New Referral Received"]) {
      if (eg == "PIFU activated") {
        ##effectively infinite capacity for PIFU
        evs <- event_group$event[event_group$eventgroup == eg]
        capacity_temp$Capacity.PIFU.activated <- rep(100000,length(capacity$date))
        for (ev in evs) {
          col_name <- paste("Capacity.",events$event_short_name[events$event == ev],sep='')
          capacity[[col_name]] <- rep(0,length(capacity$date))
        }
      } else {
        evs <- event_group$event[event_group$eventgroup == eg]
        #initialise a column
        for (ev in evs) {
          col_name <- paste("Capacity.",events$event_short_name[events$event == ev],sep='')
          capacity[[col_name]] <- rep(0,length(capacity$date))
        }
      }
    }
    rm(eg,ev,evs,col_name)
  }
  
  ##########################################################################
  ## Set up starting situation #############################################
  ##########################################################################
  
  c <- nrow(capacity)  #number of months
  m1 <- nrow(events)   #number of events
  m2 <- nrow(statuses) #number of statuses
  start_date <- capacity$date[1]
  
  ####Initialise dataframes to fill with results
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
  ## Set up PIFU holding areas #############################################
  ##########################################################################
  
  m1_pifu                 <- length(event_group$event[event_group$eventgroup == "PIFU activated"])   #number of events
  pifu_statuses           <- event_recipient$recipient[event_recipient$event %in% event_group$event[event_group$eventgroup == "PIFU activated"]]
  df_pifu_holding         <- as.data.frame(matrix(0, ncol = m1_pifu+1, nrow = c))
  names(df_pifu_holding)  <- c("date",statuses$status_short_name[statuses$status %in% pifu_statuses])
  df_pifu_holding$date    <- c(capacity$date)
  #add new columns to df_waiters to hold numbers of unactivated pifus
  for (ps in pifu_statuses) {
    pt <- statuses$status_short_name[statuses$status == ps]
    pt_u <- paste0(pt,".UnactivatedThisMonth")
    df_waiters[[pt_u]] <- rep(0,c)
  }
  #rm(m1_pifu)
  
  df_pifu_holding_pre        <- as.data.frame(matrix(0, ncol = m1_pifu+1, nrow = nrow(pifu_percentages)))
  names(df_pifu_holding_pre) <- c("date",statuses$status_short_name[statuses$status %in% pifu_statuses])
  df_pifu_holding_pre$date   <- paste("pre_",rev(pifu_percentages$n_months),sep='')
  
  ##########################################################################
  ## Put initial waiters into the results dataframe #############################################
  ##########################################################################
  
  #put initial waiters into the results dataframe
  for (s in statuses$status[!(statuses$status %in% pifu_statuses)]) {
    t <- statuses$status_short_name[statuses$status == s]
    df_waiters[[t]][df_waiters$date == start_date] <- demand_initial$waiters[demand_initial$status == s]
  }
  
  #initial PIFUs
  for (s in pifu_statuses) {
    open_pifus <- demand_initial$waiters[demand_initial$status == s]
    open_pifus_monthly <- open_pifus/nrow(pifu_percentages)
    t <- statuses$status_short_name[statuses$status == s]
    #for each of the last X months' initialised PIFUS
    for (i in (1:nrow(pifu_percentages))) {
      s_pifu_percentages <- pifu_percentages
      if (i == nrow(pifu_percentages)) {
        s_pifu_percentages$date_activated <- capacity$date[1:i]
      } else {
        s_pifu_percentages$date_activated <- c(df_pifu_holding_pre$date[(i+1):nrow(df_pifu_holding_pre)],capacity$date[1:i])
      }
      s_pifu_percentages$number_activated <- pifu_percentages$proportion*open_pifus_monthly
      #for each month that some activate before the start of our modelling period
      if (i < nrow(pifu_percentages)) {
        for (j in (1:(nrow(pifu_percentages)-i))) {
          df_pifu_holding_pre[[t]][i+j] <- df_pifu_holding_pre[[t]][i+j] + s_pifu_percentages$number_activated[j]
        }
      }
      #for each month that some activate after the start of our modelling period
      for (j in ((nrow(pifu_percentages)-i+1):nrow(pifu_percentages))) {
        df_pifu_holding[[t]][j-(nrow(pifu_percentages)-i)] <- df_pifu_holding[[t]][j-(nrow(pifu_percentages)-i)] + s_pifu_percentages$number_activated[j]
      }
    }
    df_waiters[[t]][df_waiters$date == start_date] <- df_pifu_holding[[t]][df_pifu_holding$date == start_date]
  }
  
  #initial UNACTIVATED PIFUs
  for (ps in pifu_statuses) {
    pt <- statuses$status_short_name[statuses$status == ps]
    pt_u <- paste0(pt,".UnactivatedThisMonth")
    df_waiters[[pt_u]][df_waiters$date == start_date] <- sum(df_pifu_holding[[pt]][-(1:1)])
  }
  
  rm(s,t)

  
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

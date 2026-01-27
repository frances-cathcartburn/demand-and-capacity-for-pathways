
run_c_and_d_model <- function(wd = getwd()
                                       , model_subfolder = ""
                                       , scenario_subfolder = "") {
  if (model_subfolder == ""){
    setwd(wd)
  } else {
    setwd(paste(wd,'/', model_subfolder, sep = ""))
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
  }
  if (exists("pifu_percentages")) {
    names(pifu_percentages) <- c("n_months","proportion")
  }
  
  event_group            <- events[1]
  event_group$eventgroup <- events$event
  event_group$priority   <- rep(1,length(event_group[1]))
  names(event_group)     <- c("event","eventgroup","priority")
  if (exists("event_group_temp")) {
    for (e in events$event) {
      if (e %in% event_group_temp$event && event_group_temp$eventgroup[event_group_temp$event == e] != "") {
        event_group$eventgroup[event_group$event == e] <- event_group_temp$eventgroup[event_group_temp$event == e]
        event_group$priority[event_group$event == e]   <- event_group_temp$priority[event_group_temp$event == e]
      }
    }
    rm(e)
  }
  
  if (!("New Referral Received" %in% events$event)) { 
    events[nrow(events)+1,] <- c("New Referral Received")
  }
  
  events$event_short_name            <- make.names(events$event)
  statuses$status_short_name         <- make.names(statuses$status)
  event_group$eventgroup_short_name  <- make.names(event_group$eventgroup)
  
  exit_statuses <- statuses$status[tolower(statuses$exit) == "exit"]
  
  distinct_eventgroups            <- unique(event_group$eventgroup)
  distinct_eventgroups_shortnames <- unique(event_group$eventgroup_short_name)
  
  
  rm(model_statuses_raw, model_events_raw, model_event_recipient_raw, model_event_outcome_raw)
  if (exists("event_group_temp")) {
    rm(model_event_group_raw)
    rm(event_group_temp)
  }
  
  
  ##########################################################################
  ## Basic Error Checks - Model ############################################
  ##########################################################################
  
  errors <- c()
  
  if (!prod(event_recipient$event %in% events$event)) {
    msg <- "Error - The event_recipient table contains at least one event that isn't in the event table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!("New Referral Received" %in% event_outcome$event)) { 
    msg <- "Error - The 'New Referral Received' event is missing from the event outcome table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!prod(event_outcome$event %in% events$event)) {
    msg <- "Error - The event_outcome table contains at least one event that isn't in the event table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!prod(event_recipient$recipient[event_recipient$recipient != "N/A"] %in% statuses$status)) {
    msg <- "Error - The event_recipient table contains at least one recipient that isn't in the status table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!prod(event_outcome$outcome %in% statuses$status)) {
    msg <- "Error - The event_outcome table contains at least one outcome that isn't in the status table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!prod(events$event %in% event_recipient$event)) {
    msg <- "Error - There is at least one event without a recipient specified in the event_recipient table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if ( sum(duplicated(events$event)) > 0 ) {
    msg <- paste0("This event is duplicated in the events table: ",events$event[duplicated(events$event)])
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!(isTRUE(all.equal(prod(by(event_outcome$likelihood,event_outcome$event,sum)),1)))) {
    msg <- "Error - In the event outcomes file there is at least one event where the likelihoods don't add up to 1!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!prod(event_group$event %in% events$event)) {
    msg <- "Error - The event group table contains at least one event that isn't in the events table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if ( sum(duplicated(event_group$event)) > 0 ) {
    msg <- paste0("This event is duplicated in the event group table: ",event_group$event[duplicated(event_group$event)])
    print(msg)
    errors <- c(errors,msg)
  }
  
  if ( sum(statuses$rott[!is.na(statuses$rott)]>0.5) >0 ) {
    if ( sum(statuses$rott[!is.na(statuses$rott)]>1) >0 ) {
      msg <- "Error - There is at least one status in the statuses table with a ROTT rate over 100%!"
    } else {
      msg <- "Warning - There is at least one status in the statuses table with a ROTT rate over 50% - please double check this!"
    }
    print(msg)
    errors <- c(errors,msg)
  }
  
  if(sum(is.na(event_group$priority)) > 0) {
    msg <- "Error - There is at least one event that has an event group but doesn't have a priority!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!(length(unique(event_group$eventgroup)) == length(unique(event_group$eventgroup[event_group$priority == 1])))) {
    msg <- "Error - There is at least one event group without a priority 1 event!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  
  if(prod(floor(event_group$priority) == event_group$priority) == 0) {
    msg <- "Error - There is at least one non-integer priority in the event group table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  for (eg in unique(event_group$eventgroup)){
    p_s <- sort(unique(event_group$priority[event_group$eventgroup == eg]))
    if (!prod(p_s == c(1:length(p_s)))){
      msg <- "Error - There is at least one event group with priorities that aren't consecutive positive integers!"
      print(msg)
      errors <- c(errors,msg)
    }
  }
  
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
  
  
  if ( sum(duplicated(statuses$status)) > 0 ) {
    msg <- paste0("This status is duplicated in the status table: ",statuses$status[duplicated(statuses$status)])
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!prod(demand_initial$status %in% statuses$status)) {
    msg <- "Error - The demand_initial table contains at least one status that isn't in the statuses table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!prod(statuses$status %in% demand_initial$status)) {
    msg <- "Error - The demand_initial table is missing at least one of the statuses from the statuses table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if ( sum(duplicated(demand_initial$status)) > 0 ) {
    msg <- paste0("This status is duplicated in the demand_initial table: ",demand_initial$status[duplicated(demand_initial$status)])
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!prod(gsub('Capacity.','',names(capacity_temp)[2:length(names(capacity_temp))]) %in% distinct_eventgroups_shortnames)) {
    msg <- "Error - The event_recipient table contains a column for at least one event/event group that isn't in the event/event group table!"
    print(msg)
    errors <- c(errors,msg)
  }
  
  if (!prod(distinct_eventgroups_shortnames[! distinct_eventgroups_shortnames %in% c("New.Referral.Received","PIFU.activated")] %in% gsub('Capacity.','',names(capacity_temp)[2:length(names(capacity_temp))]))) {
    msg <- "Error - The event_recipient table is missing a capacity column for at least one event/event group! Or, possibly, the New Referrals column was missed out."
    print(msg)
    errors <- c(errors,msg)
  }
  
  if ( sum(duplicated(capacity_temp$date)) > 0 ) {
    msg <- paste0("This date is duplicated in the capacity table: ",capacity_temp$date[duplicated(capacity_temp$date)])
    print(msg)
    errors <- c(errors,msg)
  }
  
  ##########################################################################
  ## Initialise PIFU Proportions ###########################################
  ##########################################################################
  ## Synthetic placeholder PIFU proportions are hardcoded
  ## Can be customised via an input file
  
  ##don't have less than 3% in any category because otherwise numbers get too small
  
    if (!exists("pifu_percentages")) {
    pifu_percentages <- data.frame(c(1:12),c(0.02,0.02,0.02,0.03,0.06,0.07,0.09,0.13,0.16,0.18,0.16,0.06))
    names(pifu_percentages) <- c("n_months","proportion")
  }  
  
  if ( sum(pifu_percentages$proportion) != 1 ) {
    msg <- "Error - The PIFU distribution proportions don't add up to 1"
    print(msg)
    errors <- c(errors,msg)
  }
  
  ##########################################################################
  ## Initialise a capacity dataframe #######################################
  ##########################################################################
  
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
  
  
  ##rm(capacity_temp)
  
  
  
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
  
  ##Everything below this should happen for every month - apart from new referrals into df_event_demand
  
  ##########################################################################
  ## Execute model for as many months as we've got capacity and referrals ##
  ##########################################################################
  #M<-1
  for (M in c(1:nrow(capacity))) {
    current_date <- capacity$date[M]
    rott_number <- 0
    ##EVENT DEMAND
    for (e in events$event) {
      f <- events$event_short_name[events$event == e]
      recip <- event_recipient$recipient[event_recipient$event == e]
      if (recip %in% statuses$status) {
        recip_short_name <- statuses$status_short_name[statuses$status == recip]
        recip_current_no <- df_waiters[[recip_short_name]][df_waiters$date == current_date]
        df_event_demand[[f]][df_event_demand$date == current_date] <- recip_current_no
      } 
    }
    ##DYNAMIC CAPACITY
    #eg <- "PIFU activated"
    for (eg in distinct_eventgroups[distinct_eventgroups != "New Referral Received"]) {
      evs <- event_group$event[event_group$eventgroup == eg]
      event_group_mini <- event_group[event_group$eventgroup == eg,]
      event_group_mini <- sort_by(event_group_mini, event_group_mini$priority)
      event_group_mini$col_name  <- paste("Capacity.",make.names(event_group_mini$event),sep='')
      for (q in event_group_mini$event) {
        p <- events$event_short_name[events$event == q]
        event_group_mini$dem[event_group_mini$event == q] <- df_event_demand[[p]][df_event_demand$date == current_date]
      }
      col_name_group <- min(event_group_mini$eventgroup_short_name)
      available_capacity_total <- capacity_temp[[paste("Capacity.",col_name_group,sep='')]][capacity_temp$date == current_date]
      available_capacity_remaining <- available_capacity_total
      used_capacity <- 0
      lowest_priority <- max(event_group_mini$priority)
      for (priority_n in  c(1:lowest_priority)) {
        evs_p <- event_group_mini$event[event_group_mini$priority == priority_n]
        ##events where there is only one of this priority
        if (length(evs_p) == 1) { 
          ev <- evs_p
          col_name_ev <- paste("Capacity.",events$event_short_name[events$event == ev],sep='')
          if (priority_n == lowest_priority) {
            capacity[[col_name_ev]][capacity$date == current_date] <- available_capacity_remaining
          } else {
            dem <- event_group_mini$dem[event_group_mini$event == ev]
            if(dem <= available_capacity_remaining) {
              capacity[[col_name_ev]][capacity$date == current_date] <- dem
              used_capacity <- used_capacity + dem
              available_capacity_remaining <- available_capacity_total - used_capacity
            } else if (dem > available_capacity_remaining) {
              capacity[[col_name_ev]][capacity$date == current_date] <- available_capacity_remaining
              used_capacity <- available_capacity_total
              available_capacity_remaining <- 0
            }
          }
        } else {
          #if there are multiple events with this priority
          total_demand <- sum (event_group_mini$dem[event_group_mini$event %in% event_group_mini$event[event_group_mini$priority==priority_n]])
          for (evv in event_group_mini$event[event_group_mini$priority==priority_n]) {
            fvv <- events$event_short_name[events$event == evv]
            col_name_evv <- paste("Capacity.",events$event_short_name[events$event == evv],sep='')
            dem <- event_group_mini$dem[event_group_mini$event == evv]
            proportion <- if(total_demand == 0) {0} else {dem/total_demand}
            if (priority_n == lowest_priority) {
              capacity[[col_name_evv]][capacity$date == current_date] <- proportion * available_capacity_remaining
            } else {
              if (total_demand <= available_capacity_remaining) {
                capacity[[col_name_evv]][capacity$date == current_date] <- dem
              } else if (total_demand > available_capacity_remaining) {
                capacity[[col_name_evv]][capacity$date == current_date] <- proportion * available_capacity_remaining
              }
            }
          }
          if (total_demand <= available_capacity_remaining) {
            used_capacity <- used_capacity + total_demand
            available_capacity_remaining <- available_capacity_total - used_capacity
          } else if (total_demand > available_capacity_remaining) {
            used_capacity <- available_capacity_total
            available_capacity_remaining <- 0 
          }
        }
      }
    }
    
    ##EVENTS CARRIED OUT
    for (e in events$event) {
      f <- events$event_short_name[events$event == e]
      if (e == "New Referral Received") {
        df_events_carried_out[[f]][df_events_carried_out$date == current_date] <- demand_referrals$new_referrals[demand_referrals$date == current_date]
      } else {
        recip <- event_recipient$recipient[event_recipient$event == e]
        dem <- df_event_demand[[f]][df_event_demand$date == current_date]
        cap <- capacity[[paste0("Capacity.",f)]][capacity$date == current_date]
        if (dem >= cap ){
          df_events_carried_out[[f]][df_events_carried_out$date == current_date] <- cap
        } else {
          df_events_carried_out[[f]][df_events_carried_out$date == current_date] <- dem
        }
      }
    }
    
    ##UNSEEN WAITERS
    for (s in statuses$status) {
      t <- statuses$status_short_name[statuses$status == s]
      waiting_at_month_start <- df_waiters[[t]][df_waiters$date == current_date]
      if (s %in% exit_statuses) {
        df_unseen[[t]][df_unseen$date == current_date] <- waiting_at_month_start
      } else {
        ev_this_status_receives       <- event_recipient$event[event_recipient$recipient == s]
        ev_this_status_receives_short <- events$event_short_name[events$event == ev_this_status_receives]
        seen_this_month <- df_events_carried_out[[ev_this_status_receives_short]][df_events_carried_out$date == current_date]
        df_unseen[[t]][df_unseen$date == current_date] <- waiting_at_month_start-seen_this_month
      }
    }
    
    ##NEWLY ON WL AFTER EVENT THIS MONTH
    for (s in statuses$status) {
      t <- statuses$status_short_name[statuses$status == s]
      evs <- event_outcome$event[event_outcome$outcome == s]
      new_waits <- c()
      for (ev in evs) {
        ev_short <- events$event_short_name[events$event == ev]
        x <- df_events_carried_out[[ev_short]][df_events_carried_out$date == current_date]
        y <- event_outcome$likelihood[event_outcome$outcome == s & event_outcome$event == ev]
        new_waits <- c(new_waits,x*y)
      }
      df_new_wait[[t]][df_new_wait$date == current_date] <- sum(new_waits)
      if (s %in% pifu_statuses) {
        ##if I wanted to count up the number of patients on unactivated PIFUs, I could do that here.
        #note that PIFUs who would return after the end of the model are lost, they never make it into s_pifu_percentages
        max_months <- min(length(capacity$date)-M+1,nrow(pifu_percentages))
        s_pifu_percentages <- pifu_percentages[1:max_months,]
        s_pifu_percentages$date_activated <- capacity$date[M+s_pifu_percentages$n_months-1]
        s_pifu_percentages$number_activated <- (pifu_percentages$proportion*df_new_wait[[t]][df_new_wait$date == current_date])[1:max_months]
        #if it's a PIFU, put the patients into df_pifu_holding, then remove them from the new waiters df
        for(dat in df_pifu_holding$date[df_pifu_holding$date %in% s_pifu_percentages$date_activated]){
          df_pifu_holding[[t]][df_pifu_holding$date == dat] <- df_pifu_holding[[t]][df_pifu_holding$date == dat] + s_pifu_percentages$number_activated[s_pifu_percentages$date_activated == dat]
        }
        df_new_wait[[t]][df_new_wait$date == current_date] <- df_pifu_holding[[t]][df_pifu_holding$date == current_date]
      }
      if (s %in% statuses$status[!is.na(statuses$rott)]) {
        rott_rate <- statuses$rott[statuses$status == s]
        rott_number_s <- df_new_wait[[t]][df_new_wait$date == current_date]*rott_rate
        rott_number <- rott_number + rott_number_s
        df_new_wait[[t]][df_new_wait$date == current_date] <- df_new_wait[[t]][df_new_wait$date == current_date] - rott_number_s
      }
    }
    ##ADDITIONAL DISCHARGES DUE TO ROTT
    df_new_wait$Discharged[df_new_wait$date == current_date] <- df_new_wait$Discharged[df_new_wait$date == current_date] + rott_number
    
    ##TOTAL WAITERS AT THE START OF NEXT MONTH
    if (M != nrow(capacity)) {
      for (s in statuses$status) {
        t <- statuses$status_short_name[statuses$status == s]
        df_waiters[[t]][df_waiters$date == capacity$date[M+1]] <- df_unseen[[t]][df_unseen$date == current_date] + df_new_wait[[t]][df_new_wait$date == current_date]
      }
      for (ps in pifu_statuses) {
        pt <- statuses$status_short_name[statuses$status == ps]
        pt_u <- paste0(pt,".UnactivatedThisMonth")
        df_waiters[[pt_u]][df_waiters$date == capacity$date[M+1]] <- sum(df_pifu_holding[[pt]][-(1:M)])
      }
    }
  }
  
  ##########################################################################
  ## Tidy up variables that we're not using again ##########################
  ##########################################################################
  
  ##some of these variables don't always exist so trying to remove them causes errors
  #rm(e,f,s,t,M,current_date,evv,fvv,eg,evs_p,lowest_priority,max_months,dat,p,priority_n,proportion,q,total_demand,used_capacity)
  #rm(col_name_ev,col_name_evv,col_name_group,available_capacity_remaining,available_capacity_total)
  #rm(recip,recip_short_name,recip_current_no,dem,cap,waiting_at_month_start,seen_this_month,ev_this_status_receives,ev_this_status_receives_short,evs,ev,ev_short,x,y,new_waits,s_pifu_percentages)
  
  ##########################################################################
  ## Put outputs somewhere useful ##########################################
  ##########################################################################
  dt_tm <- gsub(" ","_",gsub("-","",gsub(":","",substr(Sys.time(),1,19))))
  
  output_suffix <- if(model_subfolder == ""){
    paste("", dt_tm,sep="")
  } else if (scenario_subfolder == "") {
    paste("", dt_tm,"_",model_subfolder,sep="")
  } else {
    paste("", dt_tm,"_",model_subfolder,"_",scenario_subfolder,sep="")
  }
  
  
  if (length(errors)>=1){
    write.csv(errors,paste(wd,"/out/errors_",output_suffix,".csv",sep=""), row.names = FALSE)
  }
  write.csv(df_waiters,paste(wd,"/out/df_waiters_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(df_events_carried_out,paste(wd,"/out/df_events_carried_out_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(df_event_demand,paste(wd,"/out/df_event_demand_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(df_new_wait,paste(wd,"/out/df_new_wait_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(df_unseen,paste(wd,"/out/df_unseen_",output_suffix,".csv",sep=""), row.names = FALSE)
  write.csv(capacity,paste(wd,"/out/capacity_",output_suffix,".csv",sep=""), row.names = FALSE)
}

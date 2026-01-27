##########################################################################
## Basic Error Checks - Model ############################################
##########################################################################

error_checks_model_config_files <- function(
    wd
    ,events
    ,statuses
    ,event_recipient
    ,event_outcome
    ,event_group
    ,pifu_percentages
    ,output_suffix
  ) {
  
  errors_model <- c()
  
  if (!prod(event_recipient$event %in% events$event)) {
    msg <- "Error - The event_recipient table contains at least one event that isn't in the event table!"
    errors_model <- c(errors_model,msg)
  }
  
  if ( sum(duplicated(statuses$status)) > 0 ) {
    msg <- paste0("This status is duplicated in the status table: ",statuses$status[duplicated(statuses$status)])
    errors_model <- c(errors_model,msg)
  }
  
  if (!("New Referral Received" %in% event_outcome$event)) { 
    msg <- "Error - The 'New Referral Received' event is missing from the event outcome table!"
    errors_model <- c(errors_model,msg)
  }
  
  if (!prod(event_outcome$event %in% events$event)) {
    msg <- "Error - The event_outcome table contains at least one event that isn't in the event table!"
    errors_model <- c(errors_model,msg)
  }
  
  if (!prod(event_recipient$recipient[event_recipient$recipient != "N/A"] %in% statuses$status)) {
    msg <- "Error - The event_recipient table contains at least one recipient that isn't in the status table!"
    errors_model <- c(errors_model,msg)
  }
  
  if (!prod(event_outcome$outcome %in% statuses$status)) {
    msg <- "Error - The event_outcome table contains at least one outcome that isn't in the status table!"
    errors_model <- c(errors_model,msg)
  }
  
  if (!prod(events$event %in% event_recipient$event)) {
    msg <- "Error - There is at least one event without a recipient specified in the event_recipient table!"
    errors_model <- c(errors_model,msg)
  }
  
  if ( sum(duplicated(events$event)) > 0 ) {
    msg <- paste0("This event is duplicated in the events table: ",events$event[duplicated(events$event)])
    errors_model <- c(errors_model,msg)
  }
  
  if (!(isTRUE(all.equal(prod(by(event_outcome$likelihood,event_outcome$event,sum)),1)))) {
    msg <- "Error - In the event outcomes file there is at least one event where the likelihoods don't add up to 1!"
    errors_model <- c(errors_model,msg)
  }
  
  if (!prod(event_group$event %in% events$event)) {
    msg <- "Error - The event group table contains at least one event that isn't in the events table!"
    errors_model <- c(errors_model,msg)
  }
  
  if ( sum(duplicated(event_group$event)) > 0 ) {
    msg <- paste0("This event is duplicated in the event group table: ",event_group$event[duplicated(event_group$event)])
    errors_model <- c(errors_model,msg)
  }
  
  if ( sum(statuses$rott[!is.na(statuses$rott)]>0.5) >0 ) {
    if ( sum(statuses$rott[!is.na(statuses$rott)]>1) >0 ) {
      msg <- "Error - There is at least one status in the statuses table with a ROTT rate over 100%!"
    } else {
      msg <- "Warning - There is at least one status in the statuses table with a ROTT rate over 50% - please double check this!"
    }
    errors_model <- c(errors_model,msg)
  }
  
  if(sum(is.na(event_group$priority)) > 0) {
    msg <- "Error - There is at least one event that has an event group but doesn't have a priority!"
    errors_model <- c(errors_model,msg)
  } else if(prod(floor(event_group$priority) == event_group$priority) == 0) {
    msg <- "Error - There is at least one non-integer priority in the event group table!"
    errors_model <- c(errors_model,msg)
  }
  
  if (!(length(unique(event_group$eventgroup)) == length(unique(event_group$eventgroup[event_group$priority == 1])))) {
    msg <- "Error - There is at least one event group without a priority 1 event!"
    errors_model <- c(errors_model,msg)
  }
  
  for (eg in unique(event_group$eventgroup)){
    p_s <- sort(unique(event_group$priority[event_group$eventgroup == eg]))
    if (!prod(p_s == c(1:length(p_s)))){
      msg <- "Error - There is at least one event group with priorities that aren't consecutive positive integers!"
      errors_model <- c(errors_model,msg)
    }
  }
  
  if ( sum(pifu_percentages$proportion) != 1 ) {
    msg <- "Error - The PIFU distribution proportions don't add up to 1"
    errors_model <- c(errors_model,msg)
  }
  
  if (length(errors_model)>=1){
    write.csv(errors_model,paste(wd,"/out/errors_model_",output_suffix,".csv",sep=""), row.names = FALSE)
  }
  
}


error_checks_scenario_config_files <- function(
    wd
    ,events
    ,statuses
    ,event_group
    ,demand_initial
    ,capacity_temp
    ,output_suffix
) {
  
  errors_scenario <- c()
  
  distinct_eventgroups_shortnames <- unique(event_group$eventgroup_short_name)
  
  if (!prod(demand_initial$status %in% statuses$status)) {
    msg <- "Error - The demand_initial table contains at least one status that isn't in the statuses table!"
    errors_scenario <- c(errors_scenario,msg)
  }
  
  if (!prod(statuses$status %in% demand_initial$status)) {
    msg <- "Error - The demand_initial table is missing at least one of the statuses from the statuses table!"
    errors_scenario <- c(errors_scenario,msg)
  }
  
  if ( sum(duplicated(demand_initial$status)) > 0 ) {
    msg <- paste0("This status is duplicated in the demand_initial table: ",demand_initial$status[duplicated(demand_initial$status)])
    errors_scenario <- c(errors_scenario,msg)
  }
  
  if (!prod(gsub('Capacity.','',names(capacity_temp)[2:length(names(capacity_temp))]) %in% distinct_eventgroups_shortnames)) {
    msg <- "Error - The event_recipient table contains a column for at least one event/event group that isn't in the event/event group table!"
    errors_scenario <- c(errors_scenario,msg)
  }
  
  if (!prod(distinct_eventgroups_shortnames[! distinct_eventgroups_shortnames %in% c("New.Referral.Received","PIFU.activated")] %in% gsub('Capacity.','',names(capacity_temp)[2:length(names(capacity_temp))]))) {
    msg <- "Error - The event_recipient table is missing a capacity column for at least one event/event group! Or, possibly, the New Referrals column was missed out."
    errors_scenario <- c(errors_scenario,msg)
  }
  
  if ( sum(duplicated(capacity_temp$date)) > 0 ) {
    msg <- paste0("This date is duplicated in the capacity table: ",capacity_temp$date[duplicated(capacity_temp$date)])
    errors_scenario <- c(errors_scenario,msg)
  }
  
  if (length(errors_scenario)>=1){
    write.csv(errors_scenario,paste(wd,"/out/errors_scenario_",output_suffix,".csv",sep=""), row.names = FALSE)
  }
  
}


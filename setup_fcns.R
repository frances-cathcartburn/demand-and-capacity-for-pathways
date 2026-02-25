generate_output_suffix <- function () {
  dt_tm <- gsub(" ","_",gsub("-","",gsub(":","",substr(Sys.time(),1,19))))
  if(model_subfolder == ""){
    return(paste("", dt_tm,sep=""))
  } else if (scenario_subfolder == "") {
    return(paste("", dt_tm,"_",model_subfolder,sep=""))
  } else {
    return(paste("", dt_tm,"_",model_subfolder,"_",scenario_subfolder,sep=""))
  }
}

generate_capacity <- function() {
  #if there are the same number of events as event groups, no manipulation needed
  #if there aren't the same number of events as event groups, allow virtually
    #unlimited number of pifus, and for all other events start with 
    #placeholder capacity columns where all values are set to zero, so that 
    #capacity can be dynamically shared later
  if (length(events$event) == length(distinct_eventgroups)) {
    capacity <- capacity_temp
  } else {
    capacity <- capacity_temp[1]
    for (eg in distinct_eventgroups[distinct_eventgroups != "New Referral Received"]) {
      if (eg == "PIFU activated") {
        ##effectively infinite capacity for PIFU
        evs <- event_group$event[event_group$eventgroup == eg]
        capacity_temp$Capacity.PIFU.activated <<- rep(100000,length(capacity_temp$date))
        for (ev in evs) {
          col_name <- paste("Capacity.",events$event_short_name[events$event == ev],sep='')
          capacity[[col_name]] <- rep(0,length(capacity$date))
        }
      } else {
        evs <- event_group$event[event_group$eventgroup == eg]
        for (ev in evs) {
          col_name <- paste("Capacity.",events$event_short_name[events$event == ev],sep='')
          capacity[[col_name]] <- rep(0,length(capacity$date))
        }
      }
    }
  }
  return(capacity)
}

generate_df_pifu_holding <- function () {
  m1_pifu                 <- length(event_group$event[event_group$eventgroup == "PIFU activated"])   #number of events
  df_pifu_holding         <- as.data.frame(matrix(0, ncol = m1_pifu+1, nrow = c))
  names(df_pifu_holding)  <- c("date",statuses$status_short_name[statuses$status %in% pifu_statuses])
  df_pifu_holding$date    <- c(capacity$date)
  return(df_pifu_holding)
}

generate_df_pifu_holding_pre <- function() {
  m1_pifu                    <- length(event_group$event[event_group$eventgroup == "PIFU activated"])   #number of events
  df_pifu_holding_pre        <- as.data.frame(matrix(0, ncol = m1_pifu+1, nrow = nrow(pifu_percentages)))
  names(df_pifu_holding_pre) <- c("date",statuses$status_short_name[statuses$status %in% pifu_statuses])
  df_pifu_holding_pre$date   <- paste("pre_",rev(pifu_percentages$n_months),sep='')
  return(df_pifu_holding_pre)
}


initialise_pifus <- function(df_waiters) {
  df_waiters_updated <- df_waiters
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
          df_pifu_holding_pre[[t]][i+j] <<- df_pifu_holding_pre[[t]][i+j] + s_pifu_percentages$number_activated[j]
        }
      }
      #for each month that some activate after the start of our modelling period
      for (j in ((nrow(pifu_percentages)-i+1):nrow(pifu_percentages))) {
        df_pifu_holding[[t]][j-(nrow(pifu_percentages)-i)] <<- df_pifu_holding[[t]][j-(nrow(pifu_percentages)-i)] + s_pifu_percentages$number_activated[j]
      }
    }
    df_waiters_updated[[t]][df_waiters_updated$date == start_date] <- df_pifu_holding[[t]][df_pifu_holding$date == start_date]
  }
  #initial UNACTIVATED PIFUs
  for (ps in pifu_statuses) {
    pt <- statuses$status_short_name[statuses$status == ps]
    pt_u <- paste0(pt,".UnactivatedThisMonth")
    df_waiters_updated[[pt_u]][df_waiters_updated$date == start_date] <- sum(df_pifu_holding[[pt]][-(1:1)])
  }
  return(df_waiters_updated)
}
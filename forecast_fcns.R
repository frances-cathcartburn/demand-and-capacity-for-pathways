
generate_event_demand <- function() {
  for (e in events$event) {
    f <- events$event_short_name[events$event == e]
    recip <- event_recipient$recipient[event_recipient$event == e]
    if (recip %in% statuses$status) {
      recip_short_name <- statuses$status_short_name[statuses$status == recip]
      recip_current_no <- df_waiters[[recip_short_name]][df_waiters$date == current_date]
      df_event_demand[[f]][df_event_demand$date == current_date] <<- recip_current_no
    }
  }
}

allocate_dynamic_capacity <- function() {
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
          capacity[[col_name_ev]][capacity$date == current_date] <<- available_capacity_remaining
        } else {
          dem <- event_group_mini$dem[event_group_mini$event == ev]
          if(dem <= available_capacity_remaining) {
            capacity[[col_name_ev]][capacity$date == current_date] <<- dem
            used_capacity <- used_capacity + dem
            available_capacity_remaining <- available_capacity_total - used_capacity
          } else if (dem > available_capacity_remaining) {
            capacity[[col_name_ev]][capacity$date == current_date] <<- available_capacity_remaining
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
            capacity[[col_name_evv]][capacity$date == current_date] <<- proportion * available_capacity_remaining
          } else {
            if (total_demand <= available_capacity_remaining) {
              capacity[[col_name_evv]][capacity$date == current_date] <<- dem
            } else if (total_demand > available_capacity_remaining) {
              capacity[[col_name_evv]][capacity$date == current_date] <<- proportion * available_capacity_remaining
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
}

generate_events_carried_out <- function() {
  for (e in events$event) {
    f <- events$event_short_name[events$event == e]
    if (e == "New Referral Received") {
      df_events_carried_out[[f]][df_events_carried_out$date == current_date] <<- demand_referrals$new_referrals[demand_referrals$date == current_date]
    } else {
      recip <- event_recipient$recipient[event_recipient$event == e]
      dem <- df_event_demand[[f]][df_event_demand$date == current_date]
      cap <- capacity[[paste0("Capacity.",f)]][capacity$date == current_date]
      if (dem >= cap ){
        df_events_carried_out[[f]][df_events_carried_out$date == current_date] <<- cap
      } else {
        df_events_carried_out[[f]][df_events_carried_out$date == current_date] <<- dem
      }
    }
  }
}

generate_unseen_waiters <- function() {
  for (s in statuses$status) {
    t <- statuses$status_short_name[statuses$status == s]
    waiting_at_month_start <- df_waiters[[t]][df_waiters$date == current_date]
    if (s %in% exit_statuses) {
      df_unseen[[t]][df_unseen$date == current_date] <<- waiting_at_month_start
    } else {
      ev_this_status_receives       <- event_recipient$event[event_recipient$recipient == s]
      ev_this_status_receives_short <- events$event_short_name[events$event == ev_this_status_receives]
      seen_this_month <- df_events_carried_out[[ev_this_status_receives_short]][df_events_carried_out$date == current_date]
      df_unseen[[t]][df_unseen$date == current_date] <<- waiting_at_month_start-seen_this_month
    }
  }
}

generate_new_waiters <- function() {
  rott_number <- 0
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
    df_new_wait[[t]][df_new_wait$date == current_date] <<- sum(new_waits)
    
    if (s %in% pifu_statuses) {
      ##if I wanted to count up the number of patients on unactivated PIFUs, I could do that here.
      #note that PIFUs who would return after the end of the model are lost, they never make it into s_pifu_percentages
      max_months <- min(length(capacity$date)-M+1,nrow(pifu_percentages))
      pifu_percentages_temp <- pifu_percentages[1:max_months,]
      pifu_percentages_temp$date_activated <- capacity$date[M+pifu_percentages_temp$n_months-1]
      pifu_percentages_temp$number_activated <- (pifu_percentages$proportion*df_new_wait[[t]][df_new_wait$date == current_date])[1:max_months]
      #if it's a PIFU, put the patients into df_pifu_holding, then remove them from the new waiters df
      for(dat in df_pifu_holding$date[df_pifu_holding$date %in% pifu_percentages_temp$date_activated]){
        df_pifu_holding[[t]][df_pifu_holding$date == dat] <<- df_pifu_holding[[t]][df_pifu_holding$date == dat] + pifu_percentages_temp$number_activated[pifu_percentages_temp$date_activated == dat]
      }
      df_new_wait[[t]][df_new_wait$date == current_date] <<- df_pifu_holding[[t]][df_pifu_holding$date == current_date]
    }
    
    if (s %in% statuses$status[!is.na(statuses$rott)]) {
      rott_rate <- statuses$rott[statuses$status == s]
      rott_number_s <- df_new_wait[[t]][df_new_wait$date == current_date]*rott_rate
      rott_number <- rott_number + rott_number_s
      df_new_wait[[t]][df_new_wait$date == current_date] <<- df_new_wait[[t]][df_new_wait$date == current_date] - rott_number_s
    }
  }
  ##ADDITIONAL DISCHARGES DUE TO ROTT
  df_new_wait$Discharged[df_new_wait$date == current_date] <<- df_new_wait$Discharged[df_new_wait$date == current_date] + rott_number
}

generate_total_waiters_next_month <- function() {
  for (s in statuses$status) {
    t <- statuses$status_short_name[statuses$status == s]
    df_waiters[[t]][df_waiters$date == capacity$date[M+1]] <<- df_unseen[[t]][df_unseen$date == current_date] + df_new_wait[[t]][df_new_wait$date == current_date]
  }
  for (ps in pifu_statuses) {
    pt <- statuses$status_short_name[statuses$status == ps]
    pt_u <- paste0(pt,".UnactivatedThisMonth")
    df_waiters[[pt_u]][df_waiters$date == capacity$date[M+1]] <<- sum(df_pifu_holding[[pt]][-(1:M)])
  }
}

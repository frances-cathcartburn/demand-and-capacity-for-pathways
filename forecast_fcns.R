

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
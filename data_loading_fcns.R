create_event_group <- function(events,event_group_temp) {
  event_group            <- events[1]
  event_group$eventgroup <- events$event
  event_group$priority   <- rep(1,length(event_group[1]))
  names(event_group)     <- c("event","eventgroup","priority")
  if(!missing(event_group_temp)){
    for (e in events$event) {
      if (e %in% event_group_temp$event && event_group_temp$eventgroup[event_group_temp$event == e] != "") {
        event_group$eventgroup[event_group$event == e] <- event_group_temp$eventgroup[event_group_temp$event == e]
        event_group$priority[event_group$event == e]   <- event_group_temp$priority[event_group_temp$event == e]
      }
    }
  }
  return(event_group)
}
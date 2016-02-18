require(dplyr)
DATA_FILE = "data_private.csv"
# TODO: version control encrypted data, add key to .gitignore

load.data = function(file.name = DATA_FILE) {
  read.csv2(file = DATA_FILE, colClasses = c("Date","numeric"))
} 

# @param day as character, e.g. "1980-11-26", or "today"
# @param mins number of minutes
update = function(day, mins) {
  # load
  data = load.data()
  # update
  if(day == "today")
    day = as.character(Sys.Date())
  data = update_data(day, mins, data)
  # save
  write.csv2(data, file = DATA_FILE, row.names = FALSE)
  # re-create dash
  out(data)
}

# recursive-ish version
update_data = function(day, mins, data) {
  # change if it exists
  if((data %>% filter(as.character(date) == day) %>% summarize(n())) == 1) {
    data = data %>% 
      mutate(min = replace(min, as.character(date) == day, mins)) %>% 
      as.data.frame()
  }
  # add if not
  else {
    # zeroe all dates between last and given
    last = data %>% summarize(last(date))
    last = last$`last(date)`
    diff = as.numeric(as.Date(day) - last)
    for (i in 1:diff) {
      data = rbind(data, data.frame(date=(last + i), min=0))
    }
    # add new date
    data = update_data(day, mins, data)
  }
  return(data)
}

# wrangle daily & montthly
get.tidy.data = function(data) {
  daily = data %>%  mutate(rawweek = as.numeric(format(data$date, format = "%W")),
                      year = as.numeric(format(data$date, format = "%Y"))) %>%
                  mutate(week = (year - 2015) * 52 + rawweek - 49)
  
  weekly = daily %>% group_by(week) %>% summarize(ndays = length(date[min>0]), 
                                                  totmin = sum(min),
                                                  avgmin = totmin / ndays,
                                                  fte = totmin / (60 * 8)
                                                  )
  
  top5weekly = daily %>% group_by(week) %>% filter(min > 0)  %>% top_n(5, min) %>% summarise(top5mean = mean(min)) %>% select(top5mean)
  weekly = weekly %>% mutate(top5avgmin = top5weekly$top5mean) %>% select(week:avgmin, top5avgmin, fte)
  list(daily, weekly)
}

out = function(data = NULL) {
  if(is.null(data))
    data = load.data()
  tidy = get.tidy.data(data)
  daily = tidy[[1]]
  weekly = tidy[[2]]
  
  print(daily)
  print(weekly)
  
  # plot
  par(mfrow=c(2,2))
  # daily
  # TODO: paint bars on weekends
  with(daily, 
       plot(date, min, type = "l",
            ylim = c(0, 10 * 60), main = "daily (minutes)"))
  lines(lty = 3, with(daily %>% slice(1:(n()-1)), # ignore current day
                      lowess(date, min)))
  abline(h = 8 * 60, lty = 2)
  abline(h = 4 * 60, lty = 2)
  # weekly
  with(weekly , plot(week, fte, type = "l", ylim = c(0, 6), main = "weekly (full time equiv.)"))
  abline(h = 5, lty = 2)
  abline(h = 2.5, lty = 2)
  lines(lty = 3,with(weekly %>% slice(1:(n()-1)), lowess(week, fte))) # ignore this week
  
  # avg daily per week
  with(weekly , plot(week, top5avgmin, type = "b", ylim = c(0, 10 * 60), main = "avg daily per week (top 5 days)"))
  abline(h = 8 * 60, lty = 2)
  abline(h = 4 * 60, lty = 2)
  
  # TODO: montly
  with(weekly , plot(week, ndays, type = "b", ylim = c(0, 7), main = "days per week"))
  abline(h = 5, lty = 2)
}

# show dash on load
out()


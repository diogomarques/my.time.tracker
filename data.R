library(dplyr)
DATA_FILE = "data_private.csv"

load.data = function(file.name = DATA_FILE) {
  read.csv2(file = DATA_FILE, colClasses = c("Date","numeric"))
} 

update = function(day, mins) {
  # TODO: add days at 0 mins when new days are added that are not the next day
  # to the last of the recorded
  
  # load
  data = load.data()
  # change if it exists
  if((data %>% filter(as.character(date) == day) %>% summarize(n())) == 1) {
    data = data %>% 
      mutate(min = replace(min, as.character(date) == day, mins)) %>% 
      as.data.frame()
  }
  # add if not
  else {
    data = rbind(data, data.frame(date=as.Date(day), min=mins))
  }
  # save
  write.csv2(data, file = DATA_FILE, row.names = FALSE)
  # re-create dash
  out(data)
}

# wrangle daily & montthly
get.tidy.data = function(data) {
  daily = data %>%  mutate(rawweek = as.numeric(format(data$date, format = "%W")),
                      year = as.numeric(format(data$date, format = "%Y"))) %>%
            mutate(week = (year - 2015) * 52 + rawweek - 49)
  weekly = daily %>% group_by(week) %>% summarize(ndays = length(date[min>0]), 
                                                  totmin = sum(min),
                                                  avgmin = totmin / ndays,
                                                  fte = totmin / (60 * 8))
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
  with(daily %>% filter(min >= 60) , 
       plot(date, min, type = "l",
            ylim = c(0, 10 * 60), main = "daily (>= 60)"))
  lines(lty = 3, with(daily %>% filter(min >= 60) %>% slice(1:(n()-1)), # ignore current day
                      lowess(date, min)))
  abline(h = 8 * 60, lty = 2)
  abline(h = 4 * 60, lty = 2)
  #abline(lm(min ~ date, data = daily %>% filter(min >= 60) %>% slice(1:(n()-1))), lty = 3)
  # with(weekly , plot(week, totmin, type = "b", ylim = c(0, 10 * 60 * 5), main = "weekly"))
  # abline(h = 8 * 60 * 5, lty = 2)
  # abline(h = 4 * 60 * 5, lty = 2)
  with(weekly , plot(week, ndays, type = "b", ylim = c(0, 7), main = "days per week"))
  abline(h = 5, lty = 2)
  with(weekly , plot(week, avgmin, type = "b", ylim = c(0, 10 * 60), main = "avg daily per week"))
  abline(h = 8 * 60, lty = 2)
  abline(h = 4 * 60, lty = 2)
  with(weekly , plot(week, fte, type = "l", ylim = c(0, 6), main = "fte per week"))
  abline(h = 5, lty = 2)
  abline(h = 2.5, lty = 2)
  lines(lty = 3,with(weekly %>% slice(1:(n()-1)), lowess(week, fte))) # ignore this week
  #abline(lm(fte ~ week, data = weekly[1:(dim(weekly)[1]-1),]), lty = 3)
}

# show dash on load
out()


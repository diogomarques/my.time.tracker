library(dplyr)

# load data
# write.csv2(data, file = "data_private.csv", row.names = FALSE)
data = read.csv2(file = "data_private.csv", colClasses = c("Date","numeric"))

# TODO: update(date, time)

# wrangle daily and monthy tables
daily = data %>% mutate(rawweek = as.numeric(format(data$date, format = "%W")),
                         year = as.numeric(format(data$date, format = "%Y"))) %>%
  mutate(week = (year - 2015) * 52 + rawweek - 49)

weekly = daily %>% group_by(week) %>% summarize(ndays = length(date[min>0]), 
                                                totmin = sum(min),
                                                avgmin = totmin / ndays,
                                                fte = totmin / (60 * 8))
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

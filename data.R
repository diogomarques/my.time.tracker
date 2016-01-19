library(dplyr)

# create empty dataset
daily = data.frame()


# add days / mins
daily = rbind(
  daily,
  data.frame(date = as.Date("14-12-2015", "%d-%m-%Y"), min = 25 * 5),
  data.frame(date = as.Date("15-12-2015", "%d-%m-%Y"), min = 35 + 25 * 4),
  data.frame(date = as.Date("16-12-2015", "%d-%m-%Y"), min = 30 + 25 * 6),
  data.frame(date = as.Date("17-12-2015", "%d-%m-%Y"), min = 60 + 25 * 4 + 5 + 37),
  data.frame(date = as.Date("18-12-2015", "%d-%m-%Y"), min = 25 * 2 + 5 + 42),
  data.frame(date = as.Date("21-12-2015", "%d-%m-%Y"), min = 80 + 30 * 4),
  data.frame(date = as.Date("22-12-2015", "%d-%m-%Y"), min = 30 * 5 + 45),
  data.frame(date = as.Date("28-12-2015", "%d-%m-%Y"), min = 25 * 6 + 15 + 35 + 35),
  data.frame(date = as.Date("29-12-2015", "%d-%m-%Y"), min = 10 + 27 * 4 + 70 + 40),
  data.frame(date = as.Date("30-12-2015", "%d-%m-%Y"), min = 126 + 30),
  data.frame(date = as.Date("3-1-2016", "%d-%m-%Y"), min = 29),
  data.frame(date = as.Date("4-1-2016", "%d-%m-%Y"), min = 50 + 5 + 103),
  data.frame(date = as.Date("5-1-2016", "%d-%m-%Y"), min = 60 * 3 + 42 + 5 + 11),
  data.frame(date = as.Date("6-1-2016", "%d-%m-%Y"), min = 60 * 3 + 56 + 65),
  data.frame(date = as.Date("7-1-2016", "%d-%m-%Y"), min = 20 + 130 + 60 + 30),
  data.frame(date = as.Date("8-1-2016", "%d-%m-%Y"), min = 60 + 37 + 83),
  data.frame(date = as.Date("11-1-2016", "%d-%m-%Y"), min = 52 + 25 * 3 + 17 + 74 + 40),
  data.frame(date = as.Date("12-1-2016", "%d-%m-%Y"), min = 60 * 2 + 24 + 15 + 22 + 32 + 31),
  data.frame(date = as.Date("13-1-2016", "%d-%m-%Y"), min = 60 * 3 + 22),
  data.frame(date = as.Date("14-1-2016", "%d-%m-%Y"), min = 35 + 60 * 2 + 86 + 21),
  data.frame(date = as.Date("15-1-2016", "%d-%m-%Y"), min = 151 + 49 + 33 + 22),
  )

# add week
daily = daily %>% mutate(rawweek = as.numeric(format(daily$date, format = "%W")),
                         year = as.numeric(format(daily$date, format = "%Y"))) %>%
  mutate(week = (year - 2015) * 52 + rawweek - 49)
daily
weekly = daily %>% group_by(week) %>% summarize(ndays = length(date), 
                                                totmin = sum(min),
                                                avgmin = totmin / ndays,
                                                fte = totmin / (60 * 8))
weekly

# plot
print(daily)
print(weekly)
par(mfrow=c(2,2))
with(daily , plot(date, min, type = "b", ylim = c(0, 10 * 60), main = "daily"))
abline(h = 8 * 60, lty = 2)
abline(h = 4 * 60, lty = 2)
abline(lm(min ~ date, data = daily[1:(dim(daily)[1]-1),]), lty = 3)
# with(weekly , plot(week, totmin, type = "b", ylim = c(0, 10 * 60 * 5), main = "weekly"))
# abline(h = 8 * 60 * 5, lty = 2)
# abline(h = 4 * 60 * 5, lty = 2)
with(weekly , plot(week, ndays, type = "b", ylim = c(0, 7), main = "days per week"))
abline(h = 5, lty = 2)
with(weekly , plot(week, avgmin, type = "b", ylim = c(0, 10 * 60), main = "avg daily per week"))
abline(h = 8 * 60, lty = 2)
abline(h = 4 * 60, lty = 2)
with(weekly , plot(week, fte, type = "b", ylim = c(0, 6), main = "fte per week"))
abline(h = 5, lty = 2)
abline(h = 2.5, lty = 2)
abline(lm(fte ~ week, data = weekly[1:(dim(weekly)[1]-1),]), lty = 3)

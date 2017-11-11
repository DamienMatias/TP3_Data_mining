library(openxlsx)
library(plyr)
library(arules)

df = read.xlsx("userdata.xlsx")
df$Weekday <- as.factor(weekdays(as.Date(df$Time, origin="1899-12-30")))
df$TypeFactor <- as.factor(df$Type)
df$UserFactor <- as.factor(df$User)
df$NewTime <- as.POSIXct(df$Time*(60*60*24), origin="1899-12-30", tz="GMT")
df$DayIntervals <- discretize(df$Time - floor(df$Time), method = "fixed", categories = c(0, 0.25, 0.5, 0.75, 1), labels = c("Night", "Morning", "Afternoon", "Evening"))

# Mean and standard deviation of the number of consumed cigarettes per weekday (Mondays, Tuesdays,...) with the corresponding plot
FirstDay <- as.Date(min(df$Time), origin="1899-12-30")
LastDay <- as.Date(max(df$Time), origin="1899-12-30")

myDates <-seq(from = FirstDay, to = LastDay, by = "days")
mean <- count(df$Weekday)
numberinyear <- c()
for (day in mean$x) {
  numberinyear <- c(numberinyear, length(which(weekdays(myDates)==day)))
}
mean$numberinyear <- numberinyear
mean$avg <- mean$freq/numberinyear
mean$avgbyuser <- mean$avg / max(df$User)
orderweek <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
mean <- mean[match(orderweek, mean$x),]
rownames(mean) <- NULL
plot(x = mean$x, y = mean$avgbyuser, xlab = "Weekdays", ylab ="Average by User" )


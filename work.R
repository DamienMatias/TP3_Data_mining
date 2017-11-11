library(openxlsx)
library(plyr)
library(arules)

df = read.xlsx("userdata.xlsx")
df$Weekday <- as.factor(weekdays(as.Date(df$Time, origin="1899-12-30")))
df$TypeFactor <- as.factor(df$Type)
df$UserFactor <- as.factor(df$User)
df$NewTime <- as.POSIXct(df$Time*(60*60*24), origin="1899-12-30", tz="GMT")
df$DayIntervals <- discretize(df$Time - floor(df$Time), method = "fixed", categories = c(0, 0.25, 0.5, 0.75, 1), labels = c("Nuit", "Matin", "Apres midi", "Soir"))

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

# Finding the week period with the most and least smoking density (for example, Wednesday between 12 and 17h59)
smoking_density = table(df$Weekday, df$DayIntervals)
max_colname <- colnames(smoking_density)[apply(smoking_density, 2, function(u) any(u==max(smoking_density)))]
max_rowname <- row.names(smoking_density)[apply(smoking_density, 1, function(u) any(u==max(smoking_density)))]
max_density <- paste(max_rowname, max_colname, sep=" ")

min_colname <- colnames(smoking_density)[apply(smoking_density, 2, function(u) any(u==min(smoking_density)))]
min_rowname <- row.names(smoking_density)[apply(smoking_density, 1, function(u) any(u==min(smoking_density)))]
min_density <- paste(min_rowname, min_colname, sep=" ")

# Users classified number of smoking cigarettes intervals, by smoking history time (smokers tend to smoke the most in 
# what time interval, to use the cheat mode in what time interval, etc.)
Day_intervals <- count(df$DayIntervals)
mode_density <- table(df$TypeFactor, df$DayIntervals)


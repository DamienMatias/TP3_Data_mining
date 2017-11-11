df = read.xlsx("userdata.xlsx")
df$Type <- as.factor(df$Type)
df$User <- as.factor(df$User)
df$Time <- as.POSIXct(df$Time*(60*60*24), origin="1899-12-30", tz="GMT")


length(df$Type)
summary(df$Type)
df_without_hour=df
df_without_hour$Time <- as.Date(df_without_hour$Time)
nrow(df)
last_seven = df[FALSE,]


for (i in 1 : nrow(df) ){
  if (as.numeric(difftime(max(df_without_hour$Time) ,df_without_hour$Time[i] , units = c("days"))) < 6.999 ){
      last_seven=rbind(last_seven,df_without_hour[i,])
  }
}

nrow(last_seven)
last_seven
toto= data.frame(table(last_seven$Time))
toto
sum(toto$Freq)
plot (toto)

summary(df$Type)

multi.fun <- function(x) {cbind(freq = table(x), percentage = prop.table(table(x))*100)}
multi.fun(df$Type)
install.packages("data.table", type="source", dependencies=TRUE)
library(openxlsx)
df = read.xlsx("userdata.xlsx")
df$Type <- as.factor(df$Type)
df$User <- as.factor(df$User)
df$Time <- as.POSIXct(df$Time*(60*60*24), origin="1899-12-30", tz="GMT")


length(df$Type)
summary(df$Type)
df_without_hour=df
df_without_hour$Time <- as.Date(df_without_hour$Time) #Removing the hour
last_seven = df[FALSE,]

for (i in 1 : nrow(df) ){  #Take the last 7 days
  if (as.numeric(difftime(max(df_without_hour$Time) ,df_without_hour$Time[i] ,
                          units = c("days"))) < 6.999 ){
      last_seven=rbind(last_seven,df_without_hour[i,])
  }
}

seven= data.frame(table(last_seven$Time)) #Count the freq for each date
seven
sum(seven$Freq)#Count the total number of cigarettes smoked the last seven days
plot (seven)

#question statistic on modes
summary(df$Type)
stat.mode <- function(x) {cbind(freq = table(x), percentage = prop.table(table(x))*100)}
stat.mode(df$Type)


#question percentage of improvement
df_without_friend_mode=df_without_hour[FALSE,]
df_without_friend_mode$Time <- as.Date(df_without_friend_mode$Time)
for (i in 1 : nrow(df_without_hour) ){
  if (df[i,2]!='Friend'){
    df_without_friend_mode=rbind(df_without_friend_mode,df_without_hour[i,])
  }
}
observation_week = df_without_friend_mode[FALSE,]
week1 = df_without_friend_mode[FALSE,]
week2 = df_without_friend_mode[FALSE,]
for (i in 1 : nrow(df_without_friend_mode) ){
  if (round(as.numeric(difftime(df_without_friend_mode$Time[i],min(df_without_friend_mode$Time)  , 
                                   units = c("days"))) ,0) <= 7.0 ){
    week1=rbind(week1,df_without_friend_mode[i,])
  }
  else if ((round(as.numeric(difftime(df_without_friend_mode$Time[i],min(df_without_friend_mode$Time)  ,
                                units = c("days")))  ,0) >= 8.0)
           &&(round(as.numeric(difftime(df_without_friend_mode$Time[i],min(df_without_friend_mode$Time)  , 
                                  units = c("days"))),0) < 15.0) )
    week2=rbind(week2,df_without_friend_mode[i,])
  if (df_without_friend_mode[i,2]=='Observation week' )
    observation_week=rbind(observation_week,df_without_friend_mode[i,])
}
nrow(observation_week)
nrow(week1)
nrow(week2)
table(week1$Type)
table(week2$Type)

df_without_friend_mode$Time <- as.Date(df_without_friend_mode$Time)
min(df_without_friend_mode$Time)<"2017-06-06 GMT"

for (i in 1:15) print (df[i,2])

nbuser=nrow(unique(df[1])) #number of user
nbuser
userid=unique(df[1])
userid[1]
user_id=as.numeric(rownames(userid))
user_id[2]

question=setNames(data.frame(matrix(ncol = 3, nrow = nbuser)), c("Week_observation", "Week1", "Week2"))
question[1,2]=question[1,2]+1
for (i in 1:3) question[i]=0


for (j in 1:nbuser){
  for (i in user_id[j] :user_id[j+1]){
    if (df_without_friend_mode[i,2]=='Week observation'){
      question[j,1]=question[j,1]+1
    }
    
  }
    
}
df____test=df
df____test <- as.data.table(df____test)

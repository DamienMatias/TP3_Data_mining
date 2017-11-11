library(openxlsx)
df = read.xlsx("userdata.xlsx")
df$Type <- as.factor(df$Type)
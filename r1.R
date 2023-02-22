

df <- read_csv("Insurance_data0.csv")
summary(df)
str(df)

histogram(~age,data = df,col=c('red','yellow','green'))
summary(df$age)
dfage=filter(df,age >=29 & age <=47)

histogram(~bmi,data = dfage,breaks = 50,col=c('red','yellow','green'))
histogram(~bmi|gender,data = dfage,breaks = 50,col=c('red','yellow','green'))
summary(df$bmi)
summary(df$bmi & df$gender=="male")
dfm=subset(df,gender=="male")
dffm=subset(df,gender=="female")
summary(dfm$bmi)
summary(dffm$bmi)
histogram(~bmi,data = dfm,breaks = 50,col=c('red','yellow','green'))
histogram(~bmi,data = dffm,breaks = 50,col=c('red','yellow','green'))

histogram(~bloodpressure|diabetic,data = dfage,breaks = 50,col=c('red','yellow','green'))


histogram(~claim,data = df,breaks = 50,col=c('red','yellow','green'))
histogram(~claim|region,data =df,breaks = 50,col=c('red','yellow','green'))
summary(df$claim & df$region=="northeast")
summary(df$claim & df$region=="northwest")
summary(df$claim & df$region=="southeast")
summary(df$claim & df$region=="southwest")
dfreg=subset(df,region =='northwest'|region =='southeast')

histogram(~claim|smoker,data = dfage,breaks = 50,col=c('red','yellow','green'))
summary(dfage$claim & dfage$smoker =="Yes")
dfsmoker=subset(dfage,smoker=="Yes")

histogram(~claim|diabetic,data = dfage,breaks = 50,col=c('red','yellow','green'))
summary(dfage$claim & dfage$diabetic =="Yes")
dfsugar=subset(dfage,diabetic=="Yes")

histogram(~claim|gender,data = df,breaks = 50,col=c('red','yellow','green'))

histogram(~bmi|diabetic,data =df,breaks = 50,col=c('red','yellow','green'))

df %>% ggplot(aes(gender))+geom_bar(stat = "count")
df %>% ggplot(aes(region))+geom_bar(stat = "count")
df %>% ggplot(aes(smoker))+geom_bar(stat = "count")
df %>% ggplot(aes(diabetic))+geom_bar(stat = "count")

dfage %>% ggplot(aes(region))+geom_bar(stat = "count")
dfage %>% ggplot(aes(smoker))+geom_bar(stat = "count")
dfage %>% ggplot(aes(diabetic))+geom_bar(stat = "count")

dfreg %>% ggplot(aes(smoker))+geom_bar(stat = "count")
dfreg %>% ggplot(aes(diabetic))+geom_bar(stat = "count")

dfsugar %>% ggplot(aes(smoker))+geom_bar(stat = "count")

summary(df$claim)
boxplot(claim~region,data = df)
boxplot(bloodpressure~region,data = df)
Q1 <- quantile(df$bloodpressure, .25)
Q3 <- quantile(df$bloodpressure, .75)
IQR <- IQR(df$bloodpressure)
df2 <- subset(df, df$bloodpressure > (Q1 - 1.5*IQR) & df$bloodpressure < (Q3 + 1.5*IQR))
boxplot(bloodpressure~region,data = df2)

boxplot(claim~gender,data = dfreg)
boxplot(claim~gender,data = dfsmoker)
boxplot(claim~region,data = dfsugar)

plot(x=df$claim,y=df$age)
plot(x=dfage$claim,y=dfage$bloodpressure)
plot(x=dfsugar$claim,y=dfsugar$bmi)
plot(x=dfsmoker$claim,y=dfsmoker$bmi)
plot(x=df$claim,y=df$bmi)

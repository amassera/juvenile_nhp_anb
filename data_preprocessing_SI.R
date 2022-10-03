library("doBy")
library("lme4")
library("car")
library("predictmeans")
library("lsmeans")
library("dplyr")
library("ggplot2")
library("ggeffects")


#DATA PREPROCESSING 

#CHECK IF coded behaviours that occurred during either a trial or the ITI were affecting the ability of the model to predict the responses of the subjects
#dataset with model predictions and all behaviours 
y2_data<-read.csv(file='../output/year2/model_predictions-run_all_behavs_best.csv',header=TRUE,sep=',')
y2_data$Year<-2
y2_data$Task<-'classic'
summary(y2_data)

y3_data<-read.csv(file='../output/year3/model_predictions-run_all_behavs_best.csv',header=TRUE,sep=',')
y3_data$Year<-3
y3_data$Task<-'classic'
summary(y3_data)

y3r_data<-read.csv(file='../output/year3/random/model_predictions-run_all_behavs_best.csv',header=TRUE,sep=',')
y3r_data<-subset(y3r_data, select=-c(Var29))
y3r_data$Year<-3
y3r_data$Task<-'random'
y3r_data$Change<-1
summary(y3r_data)

all_data<-rbind(y2_data, y3_data, y3r_data)



#Linear Mixed Models with behaviours predicting the ability of the model to predict the performance on the task
#All behaviours are coded during trial (e.g. Locomotion) or during ITI period (e.g. Locomotion_ITI)
#permmodels = permutation tests with 10000 permutations
#rand_effects= compute the slope 


#Locomotion
model <- lmer(RespProb ~ Locomotion*Group+ (1+Locomotion|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Locomotion)) + geom_histogram(bins=30)
print(g)


model <- lmer(RespProb ~ Locomotion_ITI*Group+ (1+Locomotion_ITI|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Locomotion_ITI)) + geom_histogram(bins=30)
print(g)


#Pace

model <- lmer(RespProb ~ Pace_ITI*Group+ (1+Pace_ITI|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Pace_ITI)) + geom_histogram(bins=30)
print(g)


#Vocalization

model <- lmer(RespProb ~ Vocalization*Group+ (1+Vocalization|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Vocalization)) + geom_histogram(bins=30)
print(g)


model <- lmer(RespProb ~ Vocalization_ITI*Group+ (1+Vocalization_ITI|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Vocalization_ITI)) + geom_histogram(bins=30)
print(g)

#Anxiety

model <- lmer(RespProb ~ Anxiety_ITI*Group+ (1+Anxiety_ITI|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Anxiety_ITI)) + geom_histogram(bins=30)
print(g)

#Fear

model <- lmer(RespProb ~ Fear*Group+ (1+Fear|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Fear)) + geom_histogram(bins=30)
print(g)


model <- lmer(RespProb ~ Fear_ITI*Group+ (1+Fear_ITI|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Fear_ITI)) + geom_histogram(bins=30)
print(g)

#Threat 

model <- lmer(RespProb ~ Threat*Group+ (1+Threat|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Threat)) + geom_histogram(bins=30)
print(g)


model <- lmer(RespProb ~ Threat_ITI*Group+ (1+Threat_ITI|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Threat_ITI)) + geom_histogram(bins=30)
print(g)

#Affiliative

model <- lmer(RespProb ~ Affiliative*Group+ (1+Affiliative|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Affiliative)) + geom_histogram(bins=30)
print(g)


model <- lmer(RespProb ~ Affiliative_ITI*Group+ (1+Affiliative_ITI|Session/Year/ID), data = all_data)
print(model)
permmodels(model,all_data,group="ID",block=c('Session', 'Year'), nsim= 10000)

rand_effects<-coef(model)$ID
dev.new()
g<-ggplot(rand_effects, aes(x=Affiliative_ITI)) + geom_histogram(bins=30)
print(g)






##################

#To investigate potential differences between rearing groups in terms of ITI duration 

#year 2
data_y2<-read.csv(file='../data/year2.csv',header=TRUE,sep=',')
summary(data_y2)

#median+2.5sd over all ITIs from that year/task (before removing locomotion trials)
m<-median(data_y2$ITI, na.rm=TRUE)
sd<-sd(data_y2$ITI, na.rm=TRUE)
maxi<-m +2.5*sd
data_y2<-subset(data_y2, ITI < maxi | is.na(ITI))

#removing Locomotion trials
data_y2<-subset(data_y2, Locomotion == 0 | is.na(Locomotion))

#removing subjects with less then 25 trials 
data_y2<-data_y2[with(data_y2, ID %in% names(which(table(ID)>=25))), ]

data_y2$ID<-as.factor(data_y2$ID)
data_y2$Year<-2
data_y2$Year<-as.factor(data_y2$Year)
data_y2$Task<-'classic'
data_y2$Task<-as.factor(data_y2$Task)
data_y2$Group<-as.factor(data_y2$Group)
data_y2$Sex<-as.factor(data_y2$Sex)

params_y2<-read.csv(file="../output/year2/fitted_coefficients-run_5.csv",header=TRUE,sep=',')
names(params_y2)[names(params_y2) == 'subject'] <- 'ID'
names(params_y2)[names(params_y2) == 'group'] <- 'Group'


#year 3 classic
data_y3<-read.csv(file='../data/year3.csv',header=TRUE,sep=',')
summary(data_y3)

#median+2.5sd over all ITIs from that year/task (before removing locomotion trials)
m<-median(data_y3$ITI, na.rm=TRUE)
sd<-sd(data_y3$ITI, na.rm=TRUE)
maxi<-m +2.5*sd
data_y3<-subset(data_y3, ITI < maxi | is.na(ITI))

#removing Locomotion trials
data_y3<-subset(data_y3, Locomotion == 0 | is.na(Locomotion))

#removing subjects with less then 25 trials 
data_y3<-data_y3[with(data_y3, ID %in% names(which(table(ID)>=25))), ]

data_y3$ID<-as.factor(data_y3$ID)
data_y3$Year<-3
data_y3$Year<-as.factor(data_y3$Year)
data_y3$Task<-'classic'
data_y3$Task<-as.factor(data_y3$Task)
data_y3$Group<-as.factor(data_y3$Group)
data_y3$Sex<-as.factor(data_y3$Sex)

params_y3<-read.csv(file="../output/year3/fitted_coefficients-run_5.csv",header=TRUE,sep=',')
names(params_y3)[names(params_y3) == 'subject'] <- 'ID'
names(params_y3)[names(params_y3) == 'group'] <- 'Group'


#year 3 random
data_y3r<-read.csv(file='../data/year3_random.csv',header=TRUE,sep=',')
data_y3r<-subset(data_y3r, select=-c(X))
summary(data_y3r)


#median+2.5sd over all ITIs from that year/task (before removing locomotion trials)
m<-median(data_y3r$ITI, na.rm=TRUE)
sd<-sd(data_y3r$ITI, na.rm=TRUE)
maxi<-m +2.5*sd
data_y3r<-subset(data_y3r, ITI < maxi | is.na(ITI))

#removing Locomotion trials
data_y3r<-subset(data_y3r, Locomotion == 0 | is.na(Locomotion))

#removing subjects with less then 25 trials 
data_y3r<-data_y3r[with(data_y3r, ID %in% names(which(table(ID)>=25))), ]

data_y3r$ID<-as.factor(data_y3r$ID)
data_y3r$Year<-3
data_y3r$Year<-as.factor(data_y3r$Year)
data_y3r$Task<-'random'
data_y3r$Task<-as.factor(data_y3r$Task)
data_y3r$Group<-as.factor(data_y3r$Group)
data_y3r$Sex<-as.factor(data_y3r$Sex)
data_y3r$Change<-1

params_y3r<-read.csv(file="../output/year3/random/fitted_coefficients-run_5.csv",header=TRUE,sep=',')
names(params_y3r)[names(params_y3r) == 'subject'] <- 'ID'
names(params_y3r)[names(params_y3r) == 'group'] <- 'Group'



#binding dataframe year 2 and 3 classic task
data_classic<-rbind(data_y2, data_y3)
summary(data_classic)


#binding dataframe year 3 classic and random 
data_random<-rbind(data_y3, data_y3r)
summary(data_random)




#ITI predictred by Group and year 

model <- lmer(ITI ~ Group*Year+ (1|Session/Year/ID), data = data_classic)
permmodels(model,data_classic,group="ID",block=c('Session', 'Year'), nsim=10000)


#ITI predictred by Group and Task

model <- lmer(ITI ~ Group*Task+ (1|Session/Task/ID), data = data_random)
permmodels(model,data_random,group="ID",block=c('Session','Task'), nsim=10000)



############


#To check whether the computational model correctly predicted a different amount of responses between the two groups

#adding outcomes parameters of computational model
df_y2<-full_join(data_y2, params_y2)
df_y3<-full_join(data_y3, params_y3)
df_y3r<-full_join(data_y3r, params_y3r)

#total
data_total<-bind_rows(df_y2,df_y3,df_y3r)
data_total$Group<-as.factor(data_total$Group)
data_total$Year<-as.factor(data_total$Year)
data_total$Task<-as.factor(data_total$Task)
summary(data_total)

#Predicitng proportion of responses correctly predicted by the computational model with the Group
model <- lmer(prop_predicted ~ Group+ (1|ID), data = data_total)
permmodels(model,data_total,group="ID", nsim=10000)








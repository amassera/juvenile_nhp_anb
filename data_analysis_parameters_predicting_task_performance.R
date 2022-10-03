library("doBy")
library("lme4")
library("car")
library("predictmeans")
library("lsmeans")
library("dplyr")
source('compute_cumulative_score.R')

#Relationship between fitted model parameters and performance measures

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


#PERCENTAGE OF CORRECT RESPONSES
#year2
perc_corr_y2<-summaryBy(CorrectIncorrect+ITI~ ID, data=data_y2, na.rm=TRUE, keep.names = TRUE)

#binding parameters of computational model
df_y2<-full_join(params_y2, perc_corr_y2)
df_y2$Year<-2
df_y2$Task<-'classic'


#year 3
perc_corr_y3<-summaryBy(CorrectIncorrect+ITI~ ID, data=data_y3, na.rm=TRUE, keep.names = TRUE)

#binding parameters of computational model
df_y3<-full_join(params_y3, perc_corr_y3)
df_y3$Year<-3
df_y3$Task<-'classic'


#year 3 random
perc_corr_y3r<-summaryBy(CorrectIncorrect+ITI~ ID, data=data_y3r, na.rm=TRUE, keep.names = TRUE)

#binding parameters of computational model
df_y3r<-full_join(params_y3r, perc_corr_y3r)
df_y3r$Year<-3
df_y3r$Task<-'random'


#COMBINING dataframe year 2 and year 3 classic
perc_corr_classic<-bind_rows(df_y2,df_y3)
perc_corr_classic$Group<-as.factor(perc_corr_classic$Group)
perc_corr_classic$Year<-as.factor(perc_corr_classic$Year)
summary(perc_corr_classic)


# combining year 3 classic and random 
perc_corr_year3<-bind_rows(df_y3,df_y3r)
perc_corr_year3$Group<-as.factor(perc_corr_year3$Group)
perc_corr_year3$Task<-as.factor(perc_corr_year3$Task)
summary(perc_corr_year3)




#models

#w_1
model <- lmer(CorrectIncorrect ~ w_1+Group*Year+ (1|ID), data = perc_corr_classic)
permmodels(model,perc_corr_classic,group="ID", nsim=10000)


model <- lmer(CorrectIncorrect ~ w_1+Group*Task+ (1|ID), data = perc_corr_year3)
permmodels(model,perc_corr_year3,group="ID", nsim=10000)

#lambda_1


model <- lmer(CorrectIncorrect ~ lambda_1+Group*Year+ (1|ID), data = perc_corr_classic)
permmodels(model,perc_corr_classic,group="ID", nsim=10000)


model <- lmer(CorrectIncorrect ~ lambda_1+Group*Task+ (1|ID), data = perc_corr_year3)
permmodels(model,perc_corr_year3,group="ID", nsim=10000)

#w_2


model <- lmer(CorrectIncorrect ~ w_2+Group*Year+ (1|ID), data = perc_corr_classic)
permmodels(model,perc_corr_classic,group="ID", nsim=10000)

model <- lmer(CorrectIncorrect ~ w_2+Group*Task+ (1|ID), data = perc_corr_year3)
permmodels(model,perc_corr_year3,group="ID", nsim=10000)


#lambda_2

model <- lmer(CorrectIncorrect ~ lambda_2+Group*Year+ (1|ID), data = perc_corr_classic)
permmodels(model,perc_corr_classic,group="ID", nsim=10000)

model <- lmer(CorrectIncorrect ~ lambda_2+Group*Task+ (1|ID), data = perc_corr_year3)
permmodels(model,perc_corr_year3,group="ID", nsim=10000)

#beta

model <- lmer(CorrectIncorrect ~ beta+Group*Year+ (1|ID), data = perc_corr_classic)
permmodels(model,perc_corr_classic,group="ID", nsim=10000)

model <- lmer(CorrectIncorrect ~ beta+Group*Task+ (1|ID), data = perc_corr_year3)
permmodels(model,perc_corr_year3,group="ID", nsim=10000)



####################################

#CUMULATIVE SCORE 


#year 2 classic
y2_score_df<-compute_cumulative_score(data_y2)
y2_score_df$Year<-2

#binding output computational model 
df_y2<-full_join(y2_score_df, params_y2)
df_y2$Task<-'classic'




#year 3 classic
y3_score_df<-compute_cumulative_score(data_y3)
y3_score_df$Year<-3

#binding output computational model 
df_y3<-full_join(y3_score_df, params_y3)
df_y3$Task<-'classic'



#combine dataframe year 2 and year 3
all_data_score<-rbind(df_y2, df_y3)
all_data_score$Group<-as.factor(all_data_score$Group)
all_data_score$Year<-as.factor(all_data_score$Year)
summary(all_data_score)



#models
#w_1
model <- lmer(Score ~ w_1+Group*Year+ (1|ID), data = all_data_score)
permmodels(model,all_data_score,group="ID", nsim=10000)


#lambda_1
model <- lmer(Score ~ lambda_1+Group*Year+ (1|ID), data = all_data_score)
permmodels(model,all_data_score,group="ID", nsim=10000)

#w_2
model <- lmer(Score ~ w_2+Group*Year+ (1|ID), data = all_data_score)
permmodels(model,all_data_score,group="ID", nsim=10000)

#lambda_2
model <- lmer(Score ~ lambda_2+Group*Year+ (1|ID), data = all_data_score)
permmodels(model,all_data_score,group="ID", nsim=10000)


#beta
model <- lmer(Score ~ beta+Group*Year+ (1|ID), data = all_data_score)
permmodels(model,all_data_score,group="ID", nsim=10000)



####################################

#MAXIMUM DELAY

#year 2 classic
#include only trials where there is a change in position
data_y2_change<-filter(data_y2, Change  == 1 )

#extract maximum delay period on the total number of trials for each subject
data_y2_change<-data_y2_change %>%
  group_by(ID,Group,Year) %>%
  summarize(max.delay = max(Delay))

#binding outcomes parameters of computational model 
df_y2<-full_join(data_y2_change, params_y2)
df_y2$Task<-'classic'


#year 3 classic
#include only trials where there is a change in position
data_y3_change<-filter(data_y3, Change  == 1 )

#extract maximum delay period on the total number of trials for each subject
data_y3_change<-data_y3_change %>%
  group_by(ID,Group,Year) %>%
  summarize(max.delay = max(Delay))

#binding outcomes parameters of computational model 
df_y3<-full_join(data_y3_change, params_y3)
df_y3$Task<-'classic'



#combining dataframe year 2 and 3
all_data_delay<-rbind(df_y2, df_y3)
all_data_delay$Group<-as.factor(all_data_delay$Group)
all_data_delay$Year<-as.factor(all_data_delay$Year)


#models

#w_1
model <- lmer(max.delay ~ w_1+Group*Year+ (1|ID), data = all_data_delay)
permmodels(model,all_data_delay,group="ID", nsim= 10000)


#lambda_1
model <- lmer(max.delay ~ lambda_1+Group*Year+ (1|ID), data = all_data_delay)
permmodels(model,all_data_delay,group="ID", nsim= 10000)


#w_2
model <- lmer(max.delay ~ w_2+Group*Year+ (1|ID), data = all_data_delay)
permmodels(model,all_data_delay,group="ID", nsim= 10000)


#lambda_2
model <- lmer(max.delay ~ lambda_2+Group*Year+ (1|ID), data = all_data_delay)
permmodels(model,all_data_delay,group="ID", nsim= 10000)


#beta
model <- lmer(max.delay ~ beta+Group*Year+ (1|ID), data = all_data_delay)
permmodels(model,all_data_delay,group="ID", nsim= 10000)




library("doBy")
library("lme4")
library("car")
library("predictmeans")
library("lsmeans")
library("dplyr")
library("ggplot2")
library("ggeffects")
source('compute_cumulative_score.R')


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



#binding dataframe year 2 and 3 classic task
data_classic<-rbind(data_y2, data_y3)
summary(data_classic)


#binding dataframe year 3 classic and random 
data_random<-rbind(data_y3, data_y3r)
summary(data_random)


#TASK PERFORMANCE - PERCENTAGE OF CORRECT RESPONSES 

#MODELS

#correct incorrect year 2 and 3 classic
model <- glmer(CorrectIncorrect~ Group*Year + (1 | Session/Year/ID), data = data_classic, family = binomial(link = "logit"),na.action=na.omit, control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))
glmer_results2<-Anova(model, type = 3)
print(glmer_results2)


#plot 
summary_data<-summaryBy(CorrectIncorrect~ID+Group+Year, data = data_classic, keep.names = TRUE)
g <- ggplot(summary_data, aes(x=Group,y=CorrectIncorrect, group=Group, fill=Group))+
  geom_boxplot(position = position_dodge()) +
  geom_point(aes(x=Group, y=CorrectIncorrect, colour=Group), size=3)+
  facet_grid(~Year) +
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)


#correct incorrect year 3 classic and random

model <- glmer(CorrectIncorrect~ Group*Task + (1 | Session/Year/ID), data = data_random, family = binomial(link = "logit"),na.action=na.omit, control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6)))
glmer_results2<-Anova(model, type = 3)
print(glmer_results2)


#plot 

summary_data<-summaryBy(CorrectIncorrect~ID+Group+Task, data = data_random, keep.names = TRUE)
g <- ggplot(summary_data, aes(x=Group,y=CorrectIncorrect, fill=Group))+
  geom_boxplot(position = position_dodge()) +
  geom_point(data=summary_data, aes(x=Group, y=CorrectIncorrect, colour=Group), size=3)+
  facet_grid(~Task) +
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)


################


#TASK PERFORMANCE - CUMULATIVE SCORE 


#year 2 classic
y2_score_df<-compute_cumulative_score(data_y2)
y2_score_df$Year<-2


#year 3 classic
y3_score_df<-compute_cumulative_score(data_y3)
y3_score_df$Year<-3



#binding year 2 and 3 dataframe
all_data_score<-full_join(y2_score_df, y3_score_df)
all_data_score$Group<-as.factor(all_data_score$Group)
all_data_score$Year<-as.factor(all_data_score$Year)
summary(all_data_score)


#model
model <- lmer(Score ~ Group*Year+ (1|ID), data = all_data_score)
permmodels(model,all_data_score,group="ID", nsim=10000)


#SCORE PLOT
summary_data<-summaryBy(Score~ID+Group+Year, data = all_data_score)
g <- ggplot(all_data_score, aes(x=Group,y=Score, fill=Group))+
  geom_boxplot(position = position_dodge()) +
  geom_point(data=summary_data, aes(x=Group, y=Score.mean, colour=Group), size=3)+
  facet_grid(~Year) +
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)



##################

#TASK PERFORMANCE - MAXIMUM DELAY


#year 2 classic
#include only trials where there is a change in position
data_y2_change<-filter(data_y2, Change  == 1 )

#extract maximum delay period on the total number of trials for each subject
data_y2_change<-data_y2_change %>%
  group_by(ID,Group,Year) %>%
  summarize(max.delay = max(Delay))


#year 3 classic
#include only trials where there is a change in position
data_y3_change<-filter(data_y3, Change  == 1 )

#extract maximum delay period on the total number of trials for each subject
data_y3_change<-data_y3_change %>%
  group_by(ID,Group,Year) %>%
  summarize(max.delay = max(Delay))

all_data_max_delay<-rbind(data_y2_change, data_y3_change)

all_data_max_delay$Group<-as.factor(all_data_max_delay$Group)
all_data_max_delay$Year<-as.factor(all_data_max_delay$Year)
summary(all_data_max_delay)


#model 
model <- lmer(max.delay ~ Group*Year+ (1|ID), data = all_data_max_delay)
permmodels(model,all_data_max_delay,group="ID", nsim= 10000)


#plot max delay
summary_data<-summaryBy(max.delay~ID+Group+Year, data = all_data_max_delay)
g <- ggplot(all_data_max_delay, aes(x=Group,y=max.delay, fill=Group))+
  geom_boxplot(position = position_dodge()) +
  geom_point(data=summary_data, aes(x=Group, y=max.delay.mean, colour=Group), size=3)+
  facet_grid(~Year) +
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)

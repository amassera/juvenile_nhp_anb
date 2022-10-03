library("doBy")
library("lme4")
library("car")
library("predictmeans")
library("lsmeans")
library("dplyr")
library("reshape2")
library("ggplot2")
library("Rmisc")
source('compute_cumulative_score.R')

#year 2
params_y2<-read.csv(file="../output/year2/fitted_coefficients-run_5.csv",header=TRUE,sep=',')
names(params_y2)[names(params_y2) == 'subject'] <- 'ID'
names(params_y2)[names(params_y2) == 'group'] <- 'Group'
params_y2$Year<-2
params_y2$Task<-'classic'


#year 3
params_y3<-read.csv(file="../output/year3/fitted_coefficients-run_5.csv",header=TRUE,sep=',')
names(params_y3)[names(params_y3) == 'subject'] <- 'ID'
names(params_y3)[names(params_y3) == 'group'] <- 'Group'
params_y3$Year<-3
params_y3$Task<-'classic'


#random
params_y3r<-read.csv(file="../output/year3/random/fitted_coefficients-run_5.csv",header=TRUE,sep=',')
names(params_y3r)[names(params_y3r) == 'subject'] <- 'ID'
names(params_y3r)[names(params_y3r) == 'group'] <- 'Group'
params_y3r$Year<-3
params_y3r$Task<-'random'



#binding dataframe year2 and 3 classic
params_classic<-bind_rows(params_y2,params_y3)
params_classic$Group<-as.factor(params_classic$Group)
params_classic$Year<-as.factor(params_classic$Year)
summary(params_classic)


#binding dataframe year 3 classic and random 
params_year3<-bind_rows(params_y3,params_y3r)
params_year3$Group<-as.factor(params_year3$Group)
params_year3$Task<-as.factor(params_year3$Task)
summary(params_year3)



#models 

#w_1
model <- lmer(w_1 ~ Group*Year+ (1|ID), data = params_classic)
permmodels(model,params_classic,group="ID", nsim= 10000)

model <- lmer(w_1 ~ Group*Task+ (1|ID), data = params_year3)
permmodels(model,params_year3,group="ID", nsim= 10000)


#lambda_1
model <- lmer(lambda_1 ~ Group*Year+ (1|ID), data = params_classic)
permmodels(model,params_classic,group="ID", nsim= 10000)

model <- lmer(lambda_1 ~ Group*Task+ (1|ID), data = params_year3)
permmodels(model,params_year3,group="ID", nsim=10000)


#w_2
model <- lmer(w_2 ~ Group*Year+ (1|ID), data = params_classic)
permlme <- permmodels(model,params_classic,group="ID", nsim=10000)

predictmeans(model=model, modelterm="Group:Year", atvar="Year", adj="BH", permlist=permlme, plot=TRUE)
predictmeans(model=model, modelterm="Group:Year", atvar="Group", adj="BH", permlist=permlme, plot=TRUE)


model <- lmer(w_2 ~ Group*Task+ (1|ID), data = params_year3)
permmodels(model,params_year3,group="ID",nsim=10000)


#lambda_2
model <- lmer(lambda_2 ~ Group*Year+ (1|ID), data = params_classic)
permmodels(model,params_classic,group="ID", nsim=10000)


model <- lmer(lambda_2 ~ Group*Task+ (1|ID), data = params_year3)
permmodels(model,params_year3,group="ID", nsim= 10000)


#beta
model <- lmer(beta ~ Group*Year+ (1|ID), data = params_classic)
permlme <-permmodels(model,params_classic,group="ID", nsim=10000)

model <- lmer(beta ~ Group*Task+ (1|ID), data = params_year3)
permmodels(model,params_year3,group="ID", nsim=10000)



#PLOTS OF THE PARAMETERS OF THE COMPUTATIONAL MODEL 


#YEAR 2 classic = df

#w1 and lambda 1
#delay = from 1 to 9 seconds
pred_df<-data.frame()
subjs<-unique(params_y2$ID)
for(subj in subjs) {
  subj_df<-params_y2[params_y2$ID==subj,]
  subj_pred_df<-data.frame(delay=seq(from = 1, to = 9, by = .1))
  subj_pred_df$pred<-subj_df$w_1*exp(-subj_df$lambda_1*(subj_pred_df$delay-1) )
  subj_pred_df$ID<-subj
  subj_pred_df$group<-subj_df$Group
  pred_df<-rbind(pred_df, subj_pred_df)
}

integer_breaks <- function(n = 9, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
} 

summary_df<-summarySE(pred_df,'pred',groupvars=c('delay','group') )
g <- ggplot(summary_df, aes(x=delay,y=pred, fill=group)) +
  geom_line(aes(colour=group), size=1) +
  geom_ribbon(aes(ymin=pred-se,ymax=pred+se, colour=group), alpha=.3) +
  scale_x_continuous(breaks = integer_breaks())+
  ylim(0,0.7)+
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)


#w2 and lambda 2 with 35s ITI
pred_df<-data.frame()
subjs<-unique(params_y2$ID)
for(subj in subjs) {
  subj_df<-params_y2[params_y2$ID==subj,]
  subj_pred_df<-data.frame(ITI=seq(from = 1, to = 35, by = .1))
  subj_pred_df$pred<-subj_df$w_2*exp(-subj_df$lambda_2*(subj_pred_df$ITI-1) )
  subj_pred_df$ID<-subj
  subj_pred_df$group<-subj_df$Group
  pred_df<-rbind(pred_df, subj_pred_df)
}

summary_df<-summarySE(pred_df,'pred',groupvars=c('ITI','group') )
g <- ggplot(summary_df, aes(x=ITI,y=pred, fill=group)) +
  geom_line(aes(colour=group), size=1) +
  geom_ribbon(aes(ymin=pred-se,ymax=pred+se, colour=group), alpha=.3) +
  ylim(-0.25,0.65)+
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)


#YEAR 3 classic = df2

#w1 and lambda 1
#delay = from 1 to 9 seconds
pred_df<-data.frame()
subjs<-unique(params_y3$ID)
for(subj in subjs) {
  subj_df<-params_y3[params_y3$ID==subj,]
  subj_pred_df<-data.frame(delay=seq(from = 1, to = 9, by = .1))
  subj_pred_df$pred<-subj_df$w_1*exp(-subj_df$lambda_1*(subj_pred_df$delay-1) )
  subj_pred_df$ID<-subj
  subj_pred_df$group<-subj_df$Group
  pred_df<-rbind(pred_df, subj_pred_df)
}

summary_df<-summarySE(pred_df,'pred',groupvars=c('delay','group') )
g <- ggplot(summary_df, aes(x=delay,y=pred, fill=group)) +
  geom_line(aes(colour=group), size=1) +
  geom_ribbon(aes(ymin=pred-se,ymax=pred+se, colour=group), alpha=.3) +
  scale_x_continuous(breaks = integer_breaks())+
  ylim(0,0.7)+
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)


#w2 and lambda 2 with 35s ITI
pred_df<-data.frame()
subjs<-unique(params_y3$ID)
for(subj in subjs) {
  subj_df<-params_y3[params_y3$ID==subj,]
  subj_pred_df<-data.frame(ITI=seq(from = 1, to = 35, by = .1))
  subj_pred_df$pred<-subj_df$w_2*exp(-subj_df$lambda_2*(subj_pred_df$ITI-1) )
  subj_pred_df$ID<-subj
  subj_pred_df$group<-subj_df$Group
  pred_df<-rbind(pred_df, subj_pred_df)
}

summary_df<-summarySE(pred_df,'pred',groupvars=c('ITI','group') )
g <- ggplot(summary_df, aes(x=ITI,y=pred, fill=group)) +
  geom_line(aes(colour=group), size=1) +
  geom_ribbon(aes(ymin=pred-se,ymax=pred+se, colour=group), alpha=.3) +
  ylim(-0.25,0.65)+
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)


#YEAR 3 random = df3

#w1 and lambda1
pred_df<-data.frame()
subjs<-unique(params_y3r$ID)
for(subj in subjs) {
  subj_df<-params_y3r[params_y3r$ID==subj,]
  subj_pred_df<-data.frame(delay=seq(from = 1, to = 9, by = .1))
  subj_pred_df$pred<-subj_df$w_1*exp(-subj_df$lambda_1*(subj_pred_df$delay-1) )
  subj_pred_df$ID<-subj
  subj_pred_df$group<-subj_df$Group
  pred_df<-rbind(pred_df, subj_pred_df)
}

summary_df<-summarySE(pred_df,'pred',groupvars=c('delay','group') )
g <- ggplot(summary_df, aes(x=delay,y=pred, fill=group)) +
  geom_line(aes(colour=group), size=1) +
  geom_ribbon(aes(ymin=pred-se,ymax=pred+se, colour=group), alpha=.3) +
  scale_x_continuous(breaks = integer_breaks())+
  ylim(0,0.7)+
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)


#w2 and lambda 2 with 35s ITI
pred_df<-data.frame()
subjs<-unique(params_y3r$ID)
for(subj in subjs) {
  subj_df<-params_y3r[params_y3r$ID==subj,]
  subj_pred_df<-data.frame(ITI=seq(from = 1, to = 35, by = .1))
  subj_pred_df$pred<-subj_df$w_2*exp(-subj_df$lambda_2*(subj_pred_df$ITI-1) )
  subj_pred_df$ID<-subj
  subj_pred_df$group<-subj_df$Group
  pred_df<-rbind(pred_df, subj_pred_df)
}

summary_df<-summarySE(pred_df,'pred',groupvars=c('ITI','group') )
g <- ggplot(summary_df, aes(x=ITI,y=pred, fill=group)) +
  geom_line(aes(colour=group), size=1) +
  geom_ribbon(aes(ymin=pred-se,ymax=pred+se, colour=group), alpha=.3) +
  ylim(-0.25,0.65)+
  scale_colour_brewer(palette='Set1')+theme_bw()
print(g)
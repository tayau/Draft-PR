#************************************************
#                                                    
#  Trevor Yau
#  
#  Last Revision: 06/18/17
#                                                    
#
#*****************************************************

rm(list=ls())
library(ggplot2)
library(plyr)
library(R2jags)
library(ggthemes)
library(reshape2)


#Reading in Data
setwd("C:/Users/Trevor/Desktop/R Programs/Personal Projects/Football/Draft PR and Jimmy G")
draft_data<-read.csv('1998-2015 NFL Drafts.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
colnames(draft_data)[1]<-'Round'
colnames(draft_data)[2]<-'Pick'
#Changing Rounds Variable to Ordinal Categorical Variables
draft_data$Round<-factor(draft_data$Round, levels=c('1','2','3','4','5','6','7'), labels=c('R1','R2','R3','R4','R5','R6','R7'), order=TRUE)

#Creating missing data table
mis.data<-cbind(draft_data$Round, is.na(draft_data$PasserRating))
colnames(mis.data)<-c('Round', 'Missing')
mis.table<-xtabs(Missing~Round, data=mis.data)
mis.table
plot(mis.table, main='Missing Data Histogram', ylab='Count',xlab='Draft Round')


#Note: this invalidates the notion of MCAR. If it were, we would expect missing data to be distributed uniformly
#Note: Missing values indicate the following:
#     No data avaliable on QB: did not play or played in a different position
#     Data missing because back up QB never had a chance, seen unfit to play in NFL level competition during training camp
#     QB had a negligible playing time
#     We should establish weighting to missing values to lower the Passer Rating


#Getting rid of missing values
new_draft<-na.omit(draft_data)
new_draft$Ratio<-new_draft$TD/new_draft$INT

#Plotting Passer Rating by Rounds and Pick
draft_plot.round<-ggplot(data=new_draft, aes(Round,PasserRating))+
  geom_point()+
  theme_fivethirtyeight()+
  ggtitle('Passer Rating by Draft Round')+
  ylab('Passer Rating')+xlab('Draft Round')+
  theme(axis.title=element_text(size=10))
draft_plot.round

#colfunc <- colorRampPalette(c("red", "lightblue")) 
#colfunc(7)
#creates list of 7 color spectrums from red to lightblue 

library(RColorBrewer)
#display.brewer.all() ##shows all color palettes in the package
#display.brewer.pal(n = 7, name = 'YlOrRd')
#brewer.pal(n = 7, name = "YlOrRd") #creates a hexidecimal list of colors sequentially from that palette

brewer.pal(n = 8, name = "YlOrRd")
rev(brewer.pal(n = 7, name = "YlOrRd"))#reverses order of list


draft_plot.pick<-ggplot(data=new_draft, aes(Pick,PasserRating,color=new_draft$Round))+
  geom_point()+
  scale_colour_manual(name='Rounds', values = rev(brewer.pal(n = 8, name = "YlOrRd"))[1:7]) +
  ggtitle('Passer Ratings According to their Pick Number ')+
  theme_fivethirtyeight()+ ylab('Passer Rating')+
  theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.title=element_text(size=10))

draft_plot.pick

model.Round<-glm(PasserRating~Round, data=new_draft)
model.Round
summary(model.Round)

#Using boxplot to identify outliers

library(plotly)
draft.box<-ggplot(data=new_draft,aes(Round, PasserRating))+  
  geom_boxplot()+
  annotate('text', x=3.05, y=151.3, label='Ryan Nassib', color='red')+
  annotate('text', x=5.75, y=123, label='Craig Nall', color='red')+
  ggtitle('Box Plot of PR vs Rounds')+
  ylab('Passer Rating')+
  theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.title=element_text(size=10))+
  theme_fivethirtyeight()

draft.box

ggplotly(draft.box)

#Ryan Nassib outlier, played 2 games
#Craig Nall outlier - 7 games 5 TD's zero interceptions

#Removing outlier observations
new_draft<-new_draft[!new_draft$Player == "Ryan Nassib", ]
new_draft<-new_draft[!new_draft$Player == "Craig Nall", ]



### Looking only at the first 4 rounds
new_draft
r4_draft<-subset(new_draft, Round!="R5")
r4_draft<-subset(r4_draft, Round!='R6')
r4_draft<-subset(r4_draft, Round!='R7')

model.fit1<-glm(PasserRating~Pick, data=r4_draft)
summary(model.fit1) #fitting model to see if there is evidence that PR gets worse as pick number increases

#no evidence 

ggplot(r4_draft, aes(x=Pick, y=PasserRating))+
  geom_point()+
  ggtitle('Regression of PR and Draft Pick' )+
  #stat_smooth(method='lm', col='red')+
  geom_smooth(method = "lm", color='red')+
  theme_fivethirtyeight()


###Performing ANOVA
library(car)
Anova(model.Round) # Performing LRT test
library(lsmeans)
glm.lsmean<-lsmeans::lsmeans(model.Round,~Round)
glm.lsmean
summary(glm.lsmean)


#install.packages('plotly')
library(plotly)

ci.plot.Round<-ggplot(data=summary(glm.lsmean),
                        aes(x=Round, y=lsmean, group=Round))+
  ggtitle("Comparison of PR between Draft Rounds")+
  xlab("Draft Round")+ylab("Passer Rating Estimates and their 95% CI")+
  geom_point()+ #geom_point adds the points to the graph
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),width=0.2)+
  theme_fivethirtyeight()
ci.plot.Round















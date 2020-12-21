#Reading the data
data=read.csv('DeathData.csv',na.strings = "(S)",stringsAsFactors = T)
#Removing 'District of Columbia' from the states
subdata=subset(data,X!='District of Columbia')
#Making a new column for region
subdata$Region=state.region
#A new column for death rate = total death/ total population
subdata$rate=subdata$TOTAL/subdata$TTLPOP.K./1000
#Death rate for different regions

mean(subdata$rate[subdata$Region=='South'])
mean(subdata$rate[subdata$Region=='Northeast'])
mean(subdata$rate[subdata$Region=='West'])
mean(subdata$rate[subdata$Region=='North Central'])

#Making the categorical data numerical and performing aov for computing the p-value which was 0.53 
#and thus the null hypothesis was not rejected
levels(subdata$Region)
levels(subdata$Region)=c(1,2,3,4)
#1:Northeast 2:South 3:North Central 4:West
model1=aov(subdata$rate~subdata$Region)
summary(model1)
#Computing the death rate which is population/total death for each region
pop1 = sum(subdata$TTLPOP.K.[subdata$Region==1]*1000)
rate1=sum(subdata$TOTAL)/pop1

pop2 = sum(subdata$TTLPOP.K.[subdata$Region==2]*1000)
rate2=sum(subdata$TOTAL)/pop2

pop3 = sum(subdata$TTLPOP.K.[subdata$Region==3]*1000)
rate3=sum(subdata$TOTAL)/pop3

pop4 = sum(subdata$TTLPOP.K.[subdata$Region==4]*1000)
rate4=sum(subdata$TOTAL)/pop4
###Computing death in south for each reason
reasons=colnames(subdata)[3:15]
south=subset(subdata,subdata$Region==2)
south_reasons=south[reasons]
south_tot_death=sum(colSums(south_reasons))
prepie=colSums(south_reasons)/south_tot_death
####Making th epie chart
jpeg(filename = 'mix.jpg',height = 1078, width=539)
par(mfrow=c(2,1))
par(mar=c(4,4,3,4))


pie(prepie,labels=round(prepie,digits=4)*100,col=rainbow(length(reasons)),radius=0.8)
legend("topleft",reasons,fill=rainbow(length(reasons)),cex=0.5,bty="n")
title('Reasons of dying in south')

#Naming each regional subset
Northeast=subset(subdata,subdata$Region==1)
Midwest=subset(subdata,subdata$Region==3)
West=subset(subdata,subdata$Region==4)
#Making the boxplot of reagions for heart attack death
par(mar=c(4,4,3,4))

a=paste(c('Northeast','South','Midwest','West'),c(length(Northeast[,1]),length(south[,1]),length(Midwest[,1]), length(West[,1])))
boxplot(x=Northeast$HEART,south$HEART,Midwest$HEART,West$HEART,names=a)
pval=summary(aov(subdata$HEART~subdata$Region))[1]
t=paste('P-Value of Anova:',signif(pval[[1]][["Pr(>F)"]][1],2))
mtext(t,side=3,col='red')
title('Box Plot of Heart Attack Deaths In Different Regions')

dev.off()

#checking the heart attack death of regions
mean(subdata$HEART[subdata$Region==1])
mean(subdata$HEART[subdata$Region==2])
mean(subdata$HEART[subdata$Region==3])
mean(subdata$HEART[subdata$Region==4])
#Drawing the pie chart of the death rates
pie(x=c(rate1,rate2,rate3,rate4),labels = c('Northeast','South','North Central','West'),col=rainbow(4),radius=0.8)
legend("topleft",c('Northeast','South','North Central','West'),fill=rainbow(4),cex=0.7,bty="n")
title('Death rate in different regions')


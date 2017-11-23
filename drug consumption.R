#clearing environment
rm(list = ls())

#settiing path
setwd("H:/insofe internship")

#loading required libraries
#install.packages("sjPlot")
#install.packages("nsprcomp")
library(psych)
library(ggplot2)
library(lattice)
library(caret)
library(sjPlot)
library(cluster)
library(dummies)
library(arules)
library(dplyr)
library(scales)
library(cluster)
library(sqldf)
library(nsprcomp)
library(DMwR)

#load data
drug<-read.table("drug_consumption.data.txt",header=F,sep = ",")

write.csv(drug,file = "raw_drug.csv",row.names = F)
write.csv(drug,file = "dr.csv",row.names = F)

#data preprocessing

sum(is.na(drug))

colnames(drug)<-c("ID","Age","Gender","Education","Country","Ethnicity","Nscore","Escore","Oscore","Ascore","Cscore","Impulsive","SS","Alcohol","Amphet","Amyl","Benzos","Caffeine","Cannabis","Chocolate","Cocaine","Crack","Ecstasy","Heroin","Ketamine","Legalh","LSD","Meth","Mushroom","Nicotine","Semer","VSA")

drug<-subset(drug,select = -c(1))

#observations
summary(drug)
str(drug)
hist(drug$Age)
hist(drug$Gender)
hist(drug$Education)
hist(drug$Country)
hist(drug$Ethnicity)
hist(drug$Nscore)
hist(drug$Escore)
hist(drug$Oscore)
hist(drug$Ascore)
hist(drug$Cscore)
hist(drug$Impulsive)
hist(drug$SS)

#check outliers
boxplot(drug$Nscore)
boxplot(drug$Escore)
boxplot(drug$Oscore)
boxplot(drug$Ascore)
boxplot(drug$Cscore)

#age column
table(drug$Age)

drug$Age[drug$Age==-0.95197]<-"18-24"
drug$Age[drug$Age==-0.07854]<-"25-34"
drug$Age[drug$Age==0.49788]<-"35-44"
drug$Age[drug$Age==1.09449]<-"45-54"
drug$Age[drug$Age==1.82213]<-"55-64"
drug$Age[drug$Age==2.59171]<-"65-"

drug$Age<-as.factor(drug$Age)
barplot(table(drug$Age),xlab = "Age",ylab = "count")

#gender column
table(drug$Gender)

drug$Gender[drug$Gender==-0.48246]<-"male"
drug$Gender[drug$Gender==0.48246]<-"female"

drug$Gender<-as.factor(drug$Gender)
barplot(table(drug$Gender),xlab = "Gender",ylab = "count")

#education column
table(drug$Education)

drug$Education[drug$Education==-2.43591]<-"before16"
drug$Education[drug$Education==-1.7379]<-"at16"
drug$Education[drug$Education==-1.43719]<-"at17"
drug$Education[drug$Education==-1.22751]<-"at18"
drug$Education[drug$Education==-0.61113]<-"somecollegenocerti"
drug$Education[drug$Education==-0.05921]<-"profcerti_diploma"
drug$Education[drug$Education==0.45468]<-"unidegree"
drug$Education[drug$Education==1.16365]<-"mast_degree"
drug$Education[drug$Education==1.98437]<-"doct_degree"

drug$Education<-as.factor(drug$Education)
names(table(drug$Education))
barplot(table(drug$Education),xlab = "education",ylab = "count")

#country column
table(drug$Country)

drug$Country[drug$Country==-0.57009]<-"USA"
drug$Country[drug$Country==-0.46841]<-"New Zealand"
drug$Country[drug$Country==-0.28519]<-"Other"
drug$Country[drug$Country==-0.09765]<-"Australia"
drug$Country[drug$Country==0.21128]<-"Republic of Ireland"
drug$Country[drug$Country==0.24923]<-"Canada"
drug$Country[drug$Country==0.96082]<-"UK"

drug$Country<-as.factor(drug$Country)
barplot(table(drug$Country),xlab = "Country",ylab = "Count")

#ethnicity column
table(drug$Ethnicity)

drug$Ethnicity[drug$Ethnicity==-1.10702]<-"Black"
drug$Ethnicity[drug$Ethnicity==-0.50212]<-"Asian"
drug$Ethnicity[drug$Ethnicity==-0.31685]<-"White"
drug$Ethnicity[drug$Ethnicity==-0.22166]<-"White/Black"
drug$Ethnicity[drug$Ethnicity==0.1144]<-"Other"
drug$Ethnicity[drug$Ethnicity==0.126]<-"White/Asian"
drug$Ethnicity[drug$Ethnicity==1.90725]<-"Black/Asian"

drug$Ethnicity<-as.factor(drug$Ethnicity)
barplot(table(drug$Ethnicity),xlab = "Ethnicity",ylab = "Count")

#alcohol column
table(drug$Alcohol)

levels(drug$Alcohol)<-c(levels(drug$Alcohol),"0","1")

drug$Alcohol[drug$Alcohol=="CL0"]<-0
drug$Alcohol[drug$Alcohol=="CL1"]<-0
drug$Alcohol[drug$Alcohol=="CL2"]<-1
drug$Alcohol[drug$Alcohol=="CL3"]<-1
drug$Alcohol[drug$Alcohol=="CL4"]<-1
drug$Alcohol[drug$Alcohol=="CL5"]<-1
drug$Alcohol[drug$Alcohol=="CL6"]<-1

drug$Alcohol<-factor(drug$Alcohol,levels = c(0,1))
drug$Alcohol<-as.factor(drug$Alcohol)

#amphet column
table(drug$Amphet)

levels(drug$Amphet)<-c(levels(drug$Amphet),"0","1")

drug$Amphet[drug$Amphet=="CL0"]<-0
drug$Amphet[drug$Amphet=="CL1"]<-0
drug$Amphet[drug$Amphet=="CL2"]<-1
drug$Amphet[drug$Amphet=="CL3"]<-1
drug$Amphet[drug$Amphet=="CL4"]<-1
drug$Amphet[drug$Amphet=="CL5"]<-1
drug$Amphet[drug$Amphet=="CL6"]<-1

drug$Amphet<-factor(drug$Amphet,levels = c(0,1))
drug$Amphet<-as.factor(drug$Amphet)

#amyl column
table(drug$Amyl)

levels(drug$Amyl)<-c(levels(drug$Amyl),"0","1")

drug$Amyl[drug$Amyl=="CL0"]<-0
drug$Amyl[drug$Amyl=="CL1"]<-0
drug$Amyl[drug$Amyl=="CL2"]<-1
drug$Amyl[drug$Amyl=="CL3"]<-1
drug$Amyl[drug$Amyl=="CL4"]<-1
drug$Amyl[drug$Amyl=="CL5"]<-1
drug$Amyl[drug$Amyl=="CL6"]<-1

drug$Amyl<-factor(drug$Amyl,levels = c(0,1))
drug$Amyl<-as.factor(drug$Amyl)

#benzos column
table(drug$Benzos)

levels(drug$Benzos)<-c(levels(drug$Benzos),"0","1")

drug$Benzos[drug$Benzos=="CL0"]<-0
drug$Benzos[drug$Benzos=="CL1"]<-0
drug$Benzos[drug$Benzos=="CL2"]<-1
drug$Benzos[drug$Benzos=="CL3"]<-1
drug$Benzos[drug$Benzos=="CL4"]<-1
drug$Benzos[drug$Benzos=="CL5"]<-1
drug$Benzos[drug$Benzos=="CL6"]<-1

drug$Benzos<-factor(drug$Benzos,levels = c(0,1))
drug$Benzos<-as.factor(drug$Benzos)

#caffeine column
table(drug$Caffeine)

levels(drug$Caffeine)<-c(levels(drug$Caffeine),"0","1")

drug$Caffeine[drug$Caffeine=="CL0"]<-0
drug$Caffeine[drug$Caffeine=="CL1"]<-0
drug$Caffeine[drug$Caffeine=="CL2"]<-1
drug$Caffeine[drug$Caffeine=="CL3"]<-1
drug$Caffeine[drug$Caffeine=="CL4"]<-1
drug$Caffeine[drug$Caffeine=="CL5"]<-1
drug$Caffeine[drug$Caffeine=="CL6"]<-1

drug$Caffeine<-factor(drug$Caffeine,levels = c(0,1))
drug$Caffeine<-as.factor(drug$Caffeine)

#cannabis column
table(drug$Cannabis)

levels(drug$Cannabis)<-c(levels(drug$Cannabis),"0","1")

drug$Cannabis[drug$Cannabis=="CL0"]<-0
drug$Cannabis[drug$Cannabis=="CL1"]<-0
drug$Cannabis[drug$Cannabis=="CL2"]<-1
drug$Cannabis[drug$Cannabis=="CL3"]<-1
drug$Cannabis[drug$Cannabis=="CL4"]<-1
drug$Cannabis[drug$Cannabis=="CL5"]<-1
drug$Cannabis[drug$Cannabis=="CL6"]<-1

drug$Cannabis<-factor(drug$Cannabis,levels = c(0,1))
drug$Cannabis<-as.factor(drug$Cannabis)

#chocolate column
table(drug$Chocolate)

levels(drug$Chocolate)<-c(levels(drug$Chocolate),"0","1")

drug$Chocolate[drug$Chocolate=="CL0"]<-0
drug$Chocolate[drug$Chocolate=="CL1"]<-0
drug$Chocolate[drug$Chocolate=="CL2"]<-1
drug$Chocolate[drug$Chocolate=="CL3"]<-1
drug$Chocolate[drug$Chocolate=="CL4"]<-1
drug$Chocolate[drug$Chocolate=="CL5"]<-1
drug$Chocolate[drug$Chocolate=="CL6"]<-1

drug$Chocolate<-factor(drug$Chocolate,levels = c(0,1))
drug$Chocolate<-as.factor(drug$Chocolate)

#cocaine column
table(drug$Cocaine)

levels(drug$Cocaine)<-c(levels(drug$Cocaine),"0","1")

drug$Cocaine[drug$Cocaine=="CL0"]<-0
drug$Cocaine[drug$Cocaine=="CL1"]<-0
drug$Cocaine[drug$Cocaine=="CL2"]<-1
drug$Cocaine[drug$Cocaine=="CL3"]<-1
drug$Cocaine[drug$Cocaine=="CL4"]<-1
drug$Cocaine[drug$Cocaine=="CL5"]<-1
drug$Cocaine[drug$Cocaine=="CL6"]<-1

drug$Cocaine<-factor(drug$Cocaine,levels = c(0,1))
drug$Cocaine<-as.factor(drug$Cocaine)

#crack column
table(drug$Crack)

levels(drug$Crack)<-c(levels(drug$Crack),"0","1")

drug$Crack[drug$Crack=="CL0"]<-0
drug$Crack[drug$Crack=="CL1"]<-0
drug$Crack[drug$Crack=="CL2"]<-1
drug$Crack[drug$Crack=="CL3"]<-1
drug$Crack[drug$Crack=="CL4"]<-1
drug$Crack[drug$Crack=="CL5"]<-1
drug$Crack[drug$Crack=="CL6"]<-1

drug$Crack<-factor(drug$Crack,levels = c(0,1))
drug$Crack<-as.factor(drug$Crack)

#ecstasy column
table(drug$Ecstasy)

levels(drug$Ecstasy)<-c(levels(drug$Ecstasy),"0","1")

drug$Ecstasy[drug$Ecstasy=="CL0"]<-0
drug$Ecstasy[drug$Ecstasy=="CL1"]<-0
drug$Ecstasy[drug$Ecstasy=="CL2"]<-1
drug$Ecstasy[drug$Ecstasy=="CL3"]<-1
drug$Ecstasy[drug$Ecstasy=="CL4"]<-1
drug$Ecstasy[drug$Ecstasy=="CL5"]<-1
drug$Ecstasy[drug$Ecstasy=="CL6"]<-1

drug$Ecstasy<-factor(drug$Ecstasy,levels = c(0,1))
drug$Ecstasy<-as.factor(drug$Ecstasy)

#heroin column
table(drug$Heroin)

levels(drug$Heroin)<-c(levels(drug$Heroin),"0","1")

drug$Heroin[drug$Heroin=="CL0"]<-0
drug$Heroin[drug$Heroin=="CL1"]<-0
drug$Heroin[drug$Heroin=="CL2"]<-1
drug$Heroin[drug$Heroin=="CL3"]<-1
drug$Heroin[drug$Heroin=="CL4"]<-1
drug$Heroin[drug$Heroin=="CL5"]<-1
drug$Heroin[drug$Heroin=="CL6"]<-1

drug$Heroin<-factor(drug$Heroin,levels = c(0,1))
drug$Heroin<-as.factor(drug$Heroin)

#ketamine column
table(drug$Ketamine)

levels(drug$Ketamine)<-c(levels(drug$Ketamine),"0","1")

drug$Ketamine[drug$Ketamine=="CL0"]<-0
drug$Ketamine[drug$Ketamine=="CL1"]<-0
drug$Ketamine[drug$Ketamine=="CL2"]<-1
drug$Ketamine[drug$Ketamine=="CL3"]<-1
drug$Ketamine[drug$Ketamine=="CL4"]<-1
drug$Ketamine[drug$Ketamine=="CL5"]<-1
drug$Ketamine[drug$Ketamine=="CL6"]<-1

drug$Ketamine<-factor(drug$Ketamine,levels = c(0,1))
drug$Ketamine<-as.factor(drug$Ketamine)

#legalh
table(drug$Legalh)

levels(drug$Legalh)<-c(levels(drug$Legalh),"0","1")

drug$Legalh[drug$Legalh=="CL0"]<-0
drug$Legalh[drug$Legalh=="CL1"]<-0
drug$Legalh[drug$Legalh=="CL2"]<-1
drug$Legalh[drug$Legalh=="CL3"]<-1
drug$Legalh[drug$Legalh=="CL4"]<-1
drug$Legalh[drug$Legalh=="CL5"]<-1
drug$Legalh[drug$Legalh=="CL6"]<-1

drug$Legalh<-factor(drug$Legalh,levels = c(0,1))
drug$Legalh<-as.factor(drug$Legalh)

#lsd column
table(drug$LSD)

levels(drug$LSD)<-c(levels(drug$LSD),"0","1")

drug$LSD[drug$LSD=="CL0"]<-0
drug$LSD[drug$LSD=="CL1"]<-0
drug$LSD[drug$LSD=="CL2"]<-1
drug$LSD[drug$LSD=="CL3"]<-1
drug$LSD[drug$LSD=="CL4"]<-1
drug$LSD[drug$LSD=="CL5"]<-1
drug$LSD[drug$LSD=="CL6"]<-1

drug$LSD<-factor(drug$LSD,levels = c(0,1))
drug$LSD<-as.factor(drug$LSD)

#meth column
table(drug$Meth)

levels(drug$Meth)<-c(levels(drug$Meth),"0","1")

drug$Meth[drug$Meth=="CL0"]<-0
drug$Meth[drug$Meth=="CL1"]<-0
drug$Meth[drug$Meth=="CL2"]<-1
drug$Meth[drug$Meth=="CL3"]<-1
drug$Meth[drug$Meth=="CL4"]<-1
drug$Meth[drug$Meth=="CL5"]<-1
drug$Meth[drug$Meth=="CL6"]<-1

drug$Meth<-factor(drug$Meth,levels = c(0,1))
drug$Meth<-as.factor(drug$Meth)

#mushroom column
table(drug$Mushroom)

levels(drug$Mushroom)<-c(levels(drug$Mushroom),"0","1")

drug$Mushroom[drug$Mushroom=="CL0"]<-0
drug$Mushroom[drug$Mushroom=="CL1"]<-0
drug$Mushroom[drug$Mushroom=="CL2"]<-1
drug$Mushroom[drug$Mushroom=="CL3"]<-1
drug$Mushroom[drug$Mushroom=="CL4"]<-1
drug$Mushroom[drug$Mushroom=="CL5"]<-1
drug$Mushroom[drug$Mushroom=="CL6"]<-1

drug$Mushroom<-factor(drug$Mushroom,levels = c(0,1))
drug$Mushroom<-as.factor(drug$Mushroom)

#nicotine column
table(drug$Nicotine)

levels(drug$Nicotine)<-c(levels(drug$Nicotine),"0","1")

drug$Nicotine[drug$Nicotine=="CL0"]<-0
drug$Nicotine[drug$Nicotine=="CL1"]<-0
drug$Nicotine[drug$Nicotine=="CL2"]<-1
drug$Nicotine[drug$Nicotine=="CL3"]<-1
drug$Nicotine[drug$Nicotine=="CL4"]<-1
drug$Nicotine[drug$Nicotine=="CL5"]<-1
drug$Nicotine[drug$Nicotine=="CL6"]<-1

drug$Nicotine<-factor(drug$Nicotine,levels = c(0,1))
drug$Nicotine<-as.factor(drug$Nicotine)

#semer column
table(drug$Semer)

levels(drug$Semer)<-c(levels(drug$Semer),"0","1")

drug$Semer[drug$Semer=="CL0"]<-0
drug$Semer[drug$Semer=="CL1"]<-0
drug$Semer[drug$Semer=="CL2"]<-1
drug$Semer[drug$Semer=="CL3"]<-1
drug$Semer[drug$Semer=="CL4"]<-1

drug$Semer<-factor(drug$Semer,levels = c(0,1))
drug$Semer<-as.factor(drug$Semer)

#vsa column
table(drug$VSA)

levels(drug$VSA)<-c(levels(drug$VSA),"0","1")

drug$VSA[drug$VSA=="CL0"]<-0
drug$VSA[drug$VSA=="CL1"]<-0
drug$VSA[drug$VSA=="CL2"]<-1
drug$VSA[drug$VSA=="CL3"]<-1
drug$VSA[drug$VSA=="CL4"]<-1
drug$VSA[drug$VSA=="CL5"]<-1
drug$VSA[drug$VSA=="CL6"]<-1

drug$VSA<-factor(drug$VSA,levels = c(0,1))
drug$VSA<-as.factor(drug$VSA)

#visualization
ggplot(drug,aes(x=Age,fill=Gender))+geom_bar()
ggplot(drug,aes(x=Alcohol,fill=Cscore))+geom_bar()
ggplot(drug,aes(x=Caffeine,fill=Oscore))+geom_bar()
ggplot(drug,aes(x=Gender,fill=Education))+geom_bar()
ggplot(drug,aes(x=Education,fill=Country))+geom_bar()
ggplot(drug,aes(x=Country,fill=Ethnicity))+geom_bar()
ggplot(drug,aes(x=Ethnicity,fill=Nscore))+geom_bar()
ggplot(drug,aes(x=Age,fill=Nscore))+geom_bar()
#ggsave("Ethnicity(nscore).png")
ggplot(drug,aes(x=Ethnicity,fill=Escore))+geom_bar()
#ggsave("Ethnicity(escore).png")
barplot(table(drug$Alcohol),xlab = "Alcohol levels",ylab = "Count",axes = T,ylim = c(0,900))

scores<-subset(drug,select = c(Nscore,Escore,Oscore,Ascore,Cscore))

ggplot(drug,aes(x=Age,fill=(Nicotine)))+geom_bar()
ggplot(drug,aes(x=Gender,fill=Nicotine))+geom_bar()
ggplot(drug,aes(x=Education,fill=Nicotine))+geom_bar()

n1<-filter(drug,Nicotine==1)
a1<-filter(drug,Alcohol==1)
c1<-filter(drug,Cannabis==1)

ggplot(n1,aes(x=Age,fill=(Nicotine)))+geom_bar(aes(y=(..count..)/sum(..count..)*100))+ylab("%Users")
ggplot(n1,aes(x=Education,fill=(Nicotine)))+geom_bar(aes(y=(..count..)/sum(..count..)*100))+ylab("%Users")


######Relation of five traits with drugs

#Alcohol
ggplot(drug,aes(x=Nscore,fill=(Alcohol)))+geom_bar()
ggplot(drug,aes(x=Escore,fill=(Alcohol)))+geom_bar()
ggplot(drug,aes(x=Oscore,fill=(Alcohol)))+geom_bar()
ggplot(drug,aes(x=Ascore,fill=(Alcohol)))+geom_bar()
ggplot(drug,aes(x=Cscore,fill=(Alcohol)))+geom_bar()

#Amphet
ggplot(drug,aes(x=Nscore,fill=(Amphet)))+geom_bar()
ggplot(drug,aes(x=Escore,fill=(Amphet)))+geom_bar()
ggplot(drug,aes(x=Oscore,fill=(Amphet)))+geom_bar()
ggplot(drug,aes(x=Ascore,fill=(Amphet)))+geom_bar()
ggplot(drug,aes(x=Cscore,fill=(Amphet)))+geom_bar()

#Amyl
ggplot(drug,aes(x=Nscore,fill=(Amyl)))+geom_bar()
ggplot(drug,aes(x=Escore,fill=(Amyl)))+geom_bar()
ggplot(drug,aes(x=Oscore,fill=(Amyl)))+geom_bar()
ggplot(drug,aes(x=Ascore,fill=(Amyl)))+geom_bar()
ggplot(drug,aes(x=Cscore,fill=(Amyl)))+geom_bar()

#Benzos
ggplot(drug,aes(x=Nscore,fill=(Benzos)))+geom_bar()
ggplot(drug,aes(x=Escore,fill=(Benzos)))+geom_bar()
ggplot(drug,aes(x=Oscore,fill=(Benzos)))+geom_bar()
ggplot(drug,aes(x=Ascore,fill=(Benzos)))+geom_bar()
ggplot(drug,aes(x=Cscore,fill=(Benzos)))+geom_bar()

#caffeine
ggplot(drug,aes(x=Nscore,fill=(Caffeine)))+geom_bar()
ggplot(drug,aes(x=Escore,fill=(Caffeine)))+geom_bar()
ggplot(drug,aes(x=Oscore,fill=(Caffeine)))+geom_bar()
ggplot(drug,aes(x=Ascore,fill=(Caffeine)))+geom_bar()
ggplot(drug,aes(x=Cscore,fill=(Caffeine)))+geom_bar()

#Cannabis
ggplot(drug,aes(x=Nscore,fill=(Cannabis)))+geom_bar()
ggplot(drug,aes(x=Escore,fill=(Cannabis)))+geom_bar()
ggplot(drug,aes(x=Oscore,fill=(Cannabis)))+geom_bar()
ggplot(drug,aes(x=Ascore,fill=(Cannabis)))+geom_bar()
ggplot(drug,aes(x=Cscore,fill=(Cannabis)))+geom_bar()

#LSD
ggplot(drug,aes(x=Nscore,fill=(LSD)))+geom_bar()
ggplot(drug,aes(x=Escore,fill=(LSD)))+geom_bar()
ggplot(drug,aes(x=Oscore,fill=(LSD)))+geom_bar()
ggplot(drug,aes(x=Ascore,fill=(LSD)))+geom_bar()
ggplot(drug,aes(x=Cscore,fill=(LSD)))+geom_bar()
ggplot(drug,aes(x=Impulsive,fill=(LSD)))+geom_bar()


ggplot(drug,aes(x=Country,fill=Alcohol))+geom_bar()
ggplot(drug,aes(x=Country,fill=Caffeine))+geom_bar()

ggplot(drug,aes(x=Alcohol))+geom_bar(stat = "count")
ggplot(drug,aes(x=Amphet))+geom_bar()
ggplot(drug,aes(x=Amyl))+geom_bar()
ggplot(drug,aes(x=Benzos))+geom_bar()
ggplot(drug,aes(x=Caffeine))+geom_bar()
ggplot(drug,aes(x=Cannabis))+geom_bar()
ggplot(drug,aes(x=Chocolate))+geom_bar()
ggplot(drug,aes(x=Cocaine))+geom_bar()
ggplot(drug,aes(x=Crack))+geom_bar()
ggplot(drug,aes(x=Ecstasy))+geom_bar()
ggplot(drug,aes(x=Heroin))+geom_bar()
ggplot(drug,aes(x=Ketamine))+geom_bar()
ggplot(drug,aes(x=Legalh))+geom_bar()
ggplot(drug,aes(x=LSD))+geom_bar()
ggplot(drug,aes(x=Meth))+geom_bar()
ggplot(drug,aes(x=Mushroom))+geom_bar()
ggplot(drug,aes(x=Nicotine))+geom_bar()
ggplot(drug,aes(x=Semer))+geom_bar()

ggplot(drug,aes(x=Gender,fill=Alcohol))+geom_bar()
ggplot(drug,aes(x=Alcohol,fill=Nscore))+geom_bar()

ggplot(drug,aes(x=Gender,fill=Caffeine))+geom_bar()
ggplot(drug,aes(x=Caffeine,fill=Gender))+geom_bar()

ggplot(drug,aes(x=Gender,fill=Chocolate))+geom_bar()
ggplot(drug,aes(x=Chocolate,fill=Gender))+geom_bar()

ggplot(drug,aes(x=Gender,fill=Cannabis))+geom_bar()
ggplot(drug,aes(x=Cannabis,fill=Gender))+geom_bar()

ggplot(drug,aes(x=Nscore,y=Alcohol))+geom_point()
ggplot(drug,aes(x=Heroin,fill=Gender))+geom_bar()


#correlation between drugs using chi-squared test
a<-chisq.test(drug$Alcohol,drug$Amphet)
b<-chisq.test(drug$Alcohol,drug$Amyl)
c<-chisq.test(drug$Alcohol,drug$Amphet)
d<-chisq.test(drug$Alcohol,drug$Benzos)
e<-chisq.test(drug$Alcohol,drug$Caffeine)
f<-chisq.test(drug$Alcohol,drug$Cannabis)
g<-chisq.test(drug$Alcohol,drug$Nicotine)
h<-chisq.test(drug$Nicotine,drug$Mushroom)
i<-chisq.test(drug$Nicotine,drug$Caffeine)
j<-chisq.test(drug$Nicotine,drug$Cocaine)

#correlation matrix for drugs
output<-subset(drug,select = c(13:31))
output[]<-lapply(output,as.integer)
str(output)

scale(output)->o
o<-as.data.frame(o)

sjp.corr(output)
sjt.corr(output)



l<-length(drug$Amphet[drug$Amphet=="1"])
l<-as.data.frame(l)

f<-sqldf("select Education ,((count(Cannabis)*100)/1265) as co from drug where Cannabis==1 group by Education" )
ggplot(f,aes(x=Education,y=co))+geom_bar(stat = "identity")



#clearing environment
rm(list = ls())

#settiing path
setwd("H:/insofe internship")

#loading required libraries
library(psych)
library(ggplot2)
library(lattice)
#load data
drug<-read.table("drug_consumption.data.txt",header=F,sep = ",")

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

#visualization
ggplot(drug,aes(x=Age,fill=Gender))+geom_bar()
ggplot(drug,aes(x=Gender,fill=Education))+geom_bar()
ggplot(drug,aes(x=Education,fill=Country))+geom_bar()


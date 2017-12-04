#clearing environment
rm(list = ls())

#setting path
setwd("H:/insofe internship")

#loading required libraries
#install.packages("sjPlot")
#install.packages("nsprcomp")
#install.packages("FSelector")
#library(arules)
library(FSelector)
library(psych)
library(ggplot2)
library(lattice)
library(caret)
library(sjPlot)
library(cluster)
library(dummies)
library(dplyr)
library(scales)
library(cluster)
library(sqldf)
library(nsprcomp)
library(DMwR)
library(e1071)
library(C50)
library(randomForest)

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

#Nscore column
table(drug$Nscore)

drug$Nscore[drug$Nscore==-3.46436]<-"1"
drug$Nscore[drug$Nscore==-3.15735]<-"1"
drug$Nscore[drug$Nscore==-2.75696]<-"7"
drug$Nscore[drug$Nscore==-2.52197]<-"4"
drug$Nscore[drug$Nscore==-2.42317]<-"3"
drug$Nscore[drug$Nscore==-2.3436]<-"4"
drug$Nscore[drug$Nscore==-2.21844]<-"10"
drug$Nscore[drug$Nscore==-2.05048]<-"16"
drug$Nscore[drug$Nscore==-1.86962]<-"24"
drug$Nscore[drug$Nscore==-1.69163]<-"31"
drug$Nscore[drug$Nscore==-1.55078]<-"26"
drug$Nscore[drug$Nscore==-1.43907]<-"29"
drug$Nscore[drug$Nscore==-1.32828]<-"35"
drug$Nscore[drug$Nscore==-1.1943]<-"56"
drug$Nscore[drug$Nscore==-1.05308]<-"57"
drug$Nscore[drug$Nscore==-0.92104]<-"65"
drug$Nscore[drug$Nscore==-0.79151]<-"70"
drug$Nscore[drug$Nscore==-0.67825]<-"60"
drug$Nscore[drug$Nscore==-0.58016]<-"61"
drug$Nscore[drug$Nscore==-0.46725]<-"87"
drug$Nscore[drug$Nscore==-0.34799]<-"78"
drug$Nscore[drug$Nscore==-0.24649]<-"68"
drug$Nscore[drug$Nscore==-0.14882]<-"76"
drug$Nscore[drug$Nscore==-0.05188]<-"69"
drug$Nscore[drug$Nscore==0.04257]<-"73"
drug$Nscore[drug$Nscore==0.13606]<-"67"
drug$Nscore[drug$Nscore==0.22393]<-"63"
drug$Nscore[drug$Nscore==0.31287]<-"66"
drug$Nscore[drug$Nscore==0.41667]<-"80"
drug$Nscore[drug$Nscore==0.52135]<-"61"
drug$Nscore[drug$Nscore==0.62967]<-"77"
drug$Nscore[drug$Nscore==0.73545]<-"49"
drug$Nscore[drug$Nscore==0.82562]<-"51"
drug$Nscore[drug$Nscore==0.91093]<-"37"
drug$Nscore[drug$Nscore==1.02119]<-"67"
drug$Nscore[drug$Nscore==1.13281]<-"27"
drug$Nscore[drug$Nscore==1.23461]<-"49"
drug$Nscore[drug$Nscore==1.37297]<-"40"
drug$Nscore[drug$Nscore==1.49158]<-"24"
drug$Nscore[drug$Nscore==1.60383]<-"27"
drug$Nscore[drug$Nscore==1.72012]<-"17"
drug$Nscore[drug$Nscore==1.8399]<-"20"
drug$Nscore[drug$Nscore==1.98437]<-"15"
drug$Nscore[drug$Nscore==2.127]<-"11"
drug$Nscore[drug$Nscore==2.28554]<-"10"
drug$Nscore[drug$Nscore==2.46262]<-"6"
drug$Nscore[drug$Nscore==2.61139]<-"3"
drug$Nscore[drug$Nscore==2.82196]<-"5"
drug$Nscore[drug$Nscore==3.27393]<-"2"

drug$Nscore<-as.factor(drug$Nscore)

#Escore column
table(drug$Escore)

drug$Escore[drug$Escore==-3.27393]<-"2"
drug$Escore[drug$Escore==-3.00537]<-"1"
drug$Escore[drug$Escore==-2.72827]<-"6"
drug$Escore[drug$Escore==-2.5383]<-"3"
drug$Escore[drug$Escore==-2.44904]<-"3"
drug$Escore[drug$Escore==-2.32338]<-"8"
drug$Escore[drug$Escore==-2.21069]<-"5"
drug$Escore[drug$Escore==-2.11437]<-"9"
drug$Escore[drug$Escore==-2.03972]<-"4"
drug$Escore[drug$Escore==-1.92173]<-"21"
drug$Escore[drug$Escore==-1.7625]<-"23"
drug$Escore[drug$Escore==-1.6334]<-"23"
drug$Escore[drug$Escore==-1.50796]<-"32"
drug$Escore[drug$Escore==-1.37639]<-"38"
drug$Escore[drug$Escore==-1.23177]<-"55"
drug$Escore[drug$Escore==-1.09207]<-"52"
drug$Escore[drug$Escore==-0.94779]<-"77"
drug$Escore[drug$Escore==-0.80615]<-"68"
drug$Escore[drug$Escore==-0.69509]<-"58"
drug$Escore[drug$Escore==-0.57545]<-"89"
drug$Escore[drug$Escore==-0.43999]<-"90"
drug$Escore[drug$Escore==-0.30033]<-"106"
drug$Escore[drug$Escore==-0.15487]<-"107"
drug$Escore[drug$Escore==0.00332]<-"130"
drug$Escore[drug$Escore==0.16767]<-"116"
drug$Escore[drug$Escore==0.32197]<-"109"
drug$Escore[drug$Escore==0.47617]<-"105"
drug$Escore[drug$Escore==0.63779]<-"103"
drug$Escore[drug$Escore==0.80523]<-"91"
drug$Escore[drug$Escore==0.96248]<-"69"
drug$Escore[drug$Escore==1.11406]<-"64"
drug$Escore[drug$Escore==1.2861]<-"62"
drug$Escore[drug$Escore==1.45421]<-"37"
drug$Escore[drug$Escore==1.58487]<-"25"
drug$Escore[drug$Escore==1.74091]<-"34"
drug$Escore[drug$Escore==1.93886]<-"21"
drug$Escore[drug$Escore==2.127]<-"15"
drug$Escore[drug$Escore==2.32338]<-"10"
drug$Escore[drug$Escore==2.57309]<-"9"
drug$Escore[drug$Escore==2.8595]<-"2"
drug$Escore[drug$Escore==3.00537]<-"1"
drug$Escore[drug$Escore==3.27393]<-"2"

drug$Escore<-as.factor(drug$Escore)

#oscore column
table(drug$Oscore)

drug$Oscore[drug$Oscore==-3.27393]<-"2"
drug$Oscore[drug$Oscore==-2.8595]<-"4"
drug$Oscore[drug$Oscore==-2.63199]<-"4"
drug$Oscore[drug$Oscore==-2.39883]<-"11"
drug$Oscore[drug$Oscore==-2.21069]<-"9"
drug$Oscore[drug$Oscore==-2.09015]<-"9"
drug$Oscore[drug$Oscore==-1.97495]<-"13"
drug$Oscore[drug$Oscore==-1.82919]<-"23"
drug$Oscore[drug$Oscore==-1.68062]<-"25"
drug$Oscore[drug$Oscore==-1.55521]<-"26"
drug$Oscore[drug$Oscore==-1.42424]<-"39"
drug$Oscore[drug$Oscore==-1.27553]<-"51"
drug$Oscore[drug$Oscore==-1.11902]<-"64"
drug$Oscore[drug$Oscore==-0.97631]<-"60"
drug$Oscore[drug$Oscore==-0.84732]<-"68"
drug$Oscore[drug$Oscore==-0.71727]<-"76"
drug$Oscore[drug$Oscore==-0.58331]<-"87"
drug$Oscore[drug$Oscore==-0.45174]<-"86"
drug$Oscore[drug$Oscore==-0.31776]<-"101"
drug$Oscore[drug$Oscore==-0.17779]<-"103"
drug$Oscore[drug$Oscore==-0.01928]<-"134"
drug$Oscore[drug$Oscore==0.14143]<-"107"
drug$Oscore[drug$Oscore==0.29338]<-"116"
drug$Oscore[drug$Oscore==0.44585]<-"98"
drug$Oscore[drug$Oscore==0.58331]<-"83"
drug$Oscore[drug$Oscore==0.7233]<-"87"
drug$Oscore[drug$Oscore==0.88309]<-"87"
drug$Oscore[drug$Oscore==1.06238]<-"81"
drug$Oscore[drug$Oscore==1.24033]<-"57"
drug$Oscore[drug$Oscore==1.43533]<-"63"
drug$Oscore[drug$Oscore==1.65653]<-"38"
drug$Oscore[drug$Oscore==1.88511]<-"34"
drug$Oscore[drug$Oscore==2.15324]<-"19"
drug$Oscore[drug$Oscore==2.44904]<-"13"
drug$Oscore[drug$Oscore==2.90161]<-"7"

drug$Oscore<-as.factor(drug$Oscore)

#Ascore column
table(drug$Ascore)

drug$Ascore[drug$Ascore==-3.46436]<-"1"
drug$Ascore[drug$Ascore==-3.15735]<-"1"
drug$Ascore[drug$Ascore==-3.00537]<-"1"
drug$Ascore[drug$Ascore==-2.90161]<-"1"
drug$Ascore[drug$Ascore==-2.78793]<-"2"
drug$Ascore[drug$Ascore==-2.70172]<-"1"
drug$Ascore[drug$Ascore==-2.5383]<-"7"
drug$Ascore[drug$Ascore==-2.35413]<-"7"
drug$Ascore[drug$Ascore==-2.21844]<-"8"
drug$Ascore[drug$Ascore==-2.07848]<-"13"
drug$Ascore[drug$Ascore==-1.92595]<-"18"
drug$Ascore[drug$Ascore==-1.772]<-"24"
drug$Ascore[drug$Ascore==-1.6209]<-"30"
drug$Ascore[drug$Ascore==-1.47955]<-"34"
drug$Ascore[drug$Ascore==-1.34289]<-"42"
drug$Ascore[drug$Ascore==-1.21213]<-"45"
drug$Ascore[drug$Ascore==-1.07533]<-"62"
drug$Ascore[drug$Ascore==-0.91699]<-"83"
drug$Ascore[drug$Ascore==-0.76096]<-"82"
drug$Ascore[drug$Ascore==-0.60633]<-"102"
drug$Ascore[drug$Ascore==-0.45321]<-"98"
drug$Ascore[drug$Ascore==-0.30172]<-"114"
drug$Ascore[drug$Ascore==-0.15487]<-"101"
drug$Ascore[drug$Ascore==-0.01729]<-"105"
drug$Ascore[drug$Ascore==0.13136]<-"118"
drug$Ascore[drug$Ascore==0.28783]<-"112"
drug$Ascore[drug$Ascore==0.43852]<-"100"
drug$Ascore[drug$Ascore==0.59042]<-"100"
drug$Ascore[drug$Ascore==0.76096]<-"104"
drug$Ascore[drug$Ascore==0.94156]<-"85"
drug$Ascore[drug$Ascore==1.11406]<-"68"
drug$Ascore[drug$Ascore==1.2861]<-"58"
drug$Ascore[drug$Ascore==1.45039]<-"39"
drug$Ascore[drug$Ascore==1.61108]<-"36"
drug$Ascore[drug$Ascore==1.81866]<-"36"
drug$Ascore[drug$Ascore==2.03972]<-"16"
drug$Ascore[drug$Ascore==2.23427]<-"14"
drug$Ascore[drug$Ascore==2.46262]<-"8"
drug$Ascore[drug$Ascore==2.75696]<-"7"
drug$Ascore[drug$Ascore==3.15735]<-"1"
drug$Ascore[drug$Ascore==3.46436]<-"1"

drug$Ascore<-as.factor(drug$Ascore)

#cscore column
table(drug$Cscore)

drug$Cscore[drug$Cscore==-3.46436]<-"1"
drug$Cscore[drug$Cscore==-3.15735]<-"1"
drug$Cscore[drug$Cscore==-2.90161]<-"3"
drug$Cscore[drug$Cscore==-2.72827]<-"2"
drug$Cscore[drug$Cscore==-2.57309]<-"5"
drug$Cscore[drug$Cscore==-2.42317]<-"5"
drug$Cscore[drug$Cscore==-2.30408]<-"6"
drug$Cscore[drug$Cscore==-2.18109]<-"9"
drug$Cscore[drug$Cscore==-2.04506]<-"13"
drug$Cscore[drug$Cscore==-1.92173]<-"13"
drug$Cscore[drug$Cscore==-1.78169]<-"25"
drug$Cscore[drug$Cscore==-1.64101]<-"24"
drug$Cscore[drug$Cscore==-1.5184]<-"29"
drug$Cscore[drug$Cscore==-1.38502]<-"41"
drug$Cscore[drug$Cscore==-1.25773]<-"39"
drug$Cscore[drug$Cscore==-1.13788]<-"49"
drug$Cscore[drug$Cscore==-1.0145]<-"55"
drug$Cscore[drug$Cscore==-0.89891]<-"55"
drug$Cscore[drug$Cscore==-0.78155]<-"69"
drug$Cscore[drug$Cscore==-0.65253]<-"81"
drug$Cscore[drug$Cscore==-0.52745]<-"77"
drug$Cscore[drug$Cscore==-0.40581]<-"87"
drug$Cscore[drug$Cscore==-0.27607]<-"97"
drug$Cscore[drug$Cscore==-0.14277]<-"99"
drug$Cscore[drug$Cscore==-0.00665]<-"105"
drug$Cscore[drug$Cscore==0.12331]<-"90"
drug$Cscore[drug$Cscore==0.25953]<-"111"
drug$Cscore[drug$Cscore==0.41594]<-"111"
drug$Cscore[drug$Cscore==0.58489]<-"113"
drug$Cscore[drug$Cscore==0.7583]<-"95"
drug$Cscore[drug$Cscore== 0.93949]<-"95"
drug$Cscore[drug$Cscore==1.13407]<-"76"
drug$Cscore[drug$Cscore==1.30612]<-"47"
drug$Cscore[drug$Cscore==1.46191]<-"43"
drug$Cscore[drug$Cscore==1.63088]<-"34"
drug$Cscore[drug$Cscore==1.81175]<-"28"
drug$Cscore[drug$Cscore==2.04506 ]<-"27"
drug$Cscore[drug$Cscore==2.33337]<-"13"
drug$Cscore[drug$Cscore==2.63199]<-"8"
drug$Cscore[drug$Cscore==3.00537]<-"3"
drug$Cscore[drug$Cscore==3.46436]<-"1"

drug$Cscore<-as.factor(drug$Cscore)

#impulsive column
table(drug$Impulsive)

drug$Impulsive[drug$Impulsive==-2.55524]<-"20"
drug$Impulsive[drug$Impulsive==-1.37983]<-"276"
drug$Impulsive[drug$Impulsive==-0.71126]<-"307"
drug$Impulsive[drug$Impulsive==-0.21712]<-"355"
drug$Impulsive[drug$Impulsive==0.19268]<-"257"
drug$Impulsive[drug$Impulsive==0.52975]<-"216"
drug$Impulsive[drug$Impulsive==0.88113]<-"195"
drug$Impulsive[drug$Impulsive==1.29221]<-"148"
drug$Impulsive[drug$Impulsive==1.86203]<-"104"
drug$Impulsive[drug$Impulsive==2.90161]<-"7"

drug$Impulsive<-as.factor(drug$Impulsive)

#SS column
table(drug$SS)

drug$SS[drug$SS==-2.07848]<-"71"
drug$SS[drug$SS==-1.54858]<-"87"
drug$SS[drug$SS==-1.18084]<-"132"
drug$SS[drug$SS==-0.84637]<-"169"
drug$SS[drug$SS==-0.52593]<-"211"
drug$SS[drug$SS==-0.21575]<-"223"
drug$SS[drug$SS==0.07987]<-"219"
drug$SS[drug$SS==0.40148 ]<-"249"
drug$SS[drug$SS==0.7654]<-"211"
drug$SS[drug$SS==1.2247]<-"210"
drug$SS[drug$SS==1.92173]<-"103"

drug$SS<-as.factor(drug$SS)


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
ggplot(drug,aes(x=Nscore,fill=(Alcohol)))+geom_bar()

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


x<-c(6:12)
drug[,x]<-lapply(drug[,x], as.numeric)
str(drug)
####Model building

set.seed(1000)

tr<-sample(1:nrow(drug),0.6*nrow(drug))
train<-drug[tr,]
temp<-drug[-tr,]

set.seed(2000)

tt<-sample(1:nrow(temp),0.5*nrow(temp))
val<-temp[tt,]
test<-temp[-tt,]

#alcohol
names(train)
#f<-Age+Gender+Education+Country+Ethnicity+as.numeric(Nscore)+as.numeric(Escore)+as.numeric(Oscore)+as.numeric(Ascore)+as.numeric(Cscore)+as.numeric(Impulsive)+as.numeric(SS)
#levels(val$Ascore)->levels(train$Ascore)

str(train)
str(val)
#logistic model
# 1) Alcohol(88.33%)

glm.model<-glm(Alcohol~. ,data = train,family = "binomial")
summary(glm.model)

pr<-predict(glm.model,newdata = val,type = "response")
pr<-round(pr)

confusionMatrix(pr,val$Alcohol)

# 2) Amphet(76.13%)
glm.model<-glm(Amphet~.,data = train,family = "binomial")
summary(glm.model)

pr<-predict(glm.model,newdata = val,type = "response")
pr<-round(pr)

confusionMatrix(pr,val$Amphet)

# 3) Amyl(80.11%)
glm.model<-glm(Amyl~.,data = train,family = "binomial")
summary(glm.model)

pr<-predict(glm.model,newdata = val,type = "response")
pr<-round(pr)

confusionMatrix(pr,val$Amyl)

# 4) Benzos(74.01%)
glm.model<-glm(Benzos~.,data = train,family = "binomial")
summary(glm.model)

pr<-predict(glm.model,newdata = val,type = "response")
pr<-round(pr)

confusionMatrix(pr,val$Benzos)

# 5) caffeine(95.49%)
glm.model<-glm(Caffeine~.,data = train,family = "binomial")
summary(glm.model)

pr<-predict(glm.model,newdata = val,type = "response")
pr<-round(pr)

confusionMatrix(pr,val$Caffeine)

# 6) cannabis(84.88%)
glm.model<-glm(Cannabis~.,data = train,family = "binomial")
summary(glm.model)


pr<-predict(glm.model,newdata = val,type = "response")
pr<-round(pr)

confusionMatrix(pr,val$Cannabis)


#x<-list()
#y<-list(sprintf("model_%s",13:31))

#dvnames <- paste("DV", 1:6, sep='')
#ivnames <- paste("IV", 1:n, sep='') ## for some value of n



models <- list()
DV<-colnames(drug[,13:31])
IV<-colnames(drug)


for (y in DV){
  form <- formula(paste(y, "~", "."))
  models[[y]] <- glm(form, data=train, family='binomial') 
 
}
models[1]
lapply(models,summary)



for (i in 1:19) {
  
  
  pr[[i]]<-predict(models[i],newdata = val,type = "response")
 
  
}

acc<-list()
for (i in 1:19) {
  l<-as.data.frame(pr[i])
  
  l<-round(l)
  
  acc[[i]]<-confusionMatrix(unlist(l),val[,12+i])
  
  }

acc[17]


# for (j in 1:19) {
#   confusionMatrix(p[j],test[,12+j])
# }
# 
# confusionMatrix(p[1],test[,13])

###Naive Bayes

#1)Alcohol(85.41%)
naive.model<-naiveBayes(Alcohol~.,data = train)
summary(naive.model)

n.pr<-predict(naive.model,val)

confusionMatrix(n.pr,val$Alcohol)

#2)amphet(81.43%)
naive.model<-naiveBayes(Amphet~.,data = train)
summary(naive.model)

n.pr<-predict(naive.model,val)

confusionMatrix(n.pr,val$Amphet)

#3)amyl(70.56%)
naive.model<-naiveBayes(Amyl~.,data = train)
summary(naive.model)

n.pr<-predict(naive.model,val)

confusionMatrix(n.pr,val$Amyl)

#4)Benzos(75.6%)
naive.model<-naiveBayes(Benzos~.,data = train)
summary(naive.model)

n.pr<-predict(naive.model,val)

confusionMatrix(n.pr,val$Benzos)

#5)caffeine(94.16%)
naive.model<-naiveBayes(Caffeine~.,data = train)
summary(naive.model)

n.pr<-predict(naive.model,val)

confusionMatrix(n.pr,val$Caffeine)

#6)Cannabis(84.88%)
naive.model<-naiveBayes(Cannabis~.,data = train)
summary(naive.model)

n.pr<-predict(naive.model,val)

confusionMatrix(n.pr,val$Cannabis)

###Decision tree C5.0 model

#1) Alcohol(96.02%)
c.model<-C5.0(x=train[,-13],y=train[,13])
summary(c.model)
plot(c.model)

c.pr<-predict(c.model,val[,-13])

confusionMatrix(c.pr,val$Alcohol)

#2) amphet(81.7%)
c.model<-C5.0(x=train[,-14],y=train[,14])
summary(c.model)
plot(c.model)

c.pr<-predict(c.model,val[,-14])

confusionMatrix(c.pr,val[,14])

#3) amyl(83.55%)
c.model<-C5.0(x=train[,-15],y=train[,15])
summary(c.model)
plot(c.model)

c.pr<-predict(c.model,val[,-15])

confusionMatrix(c.pr,val[,15])

#4) Benzos (77.77%)
c.model<-C5.0(x=train[,-16],y=train[,16])
summary(c.model)
plot(c.model)

c.pr<-predict(c.model,val[,-16])

confusionMatrix(c.pr,val[,16])

#5) caffeine(98.14%)
c.model<-C5.0(x=train[,-17],y=train[,17])
summary(c.model)
plot(c.model)

c.pr<-predict(c.model,val[,-17])

confusionMatrix(c.pr,val[,17])

#6) cannabis(86.21%)
c.model<-C5.0(x=train[,-18],y=train[,18])
summary(c.model)
plot(c.model)

c.pr<-predict(c.model,val[,-18])

confusionMatrix(c.pr,val[,18])




m<- list()
c.p<-list()


for (i in 13:31){

  m[[i]] <- C5.0(x=train[,-i],y=train[,i],trials = 5) 
  
}
m[13]
lapply(m,summary)




for (i in 13:31) {
  
  
  c.p[[i]]<-predict(m[i],val[,-i])
  
  
}

c.p


c.acc<-list()
for (i in 13:31) {
  c.l<-as.data.frame(c.p[i])
  c.acc[[i]]<-confusionMatrix(unlist(c.l),val[,i])
  
}

c.acc[13]
c.acc[14]
c.acc[15]
c.acc[16]
c.acc[17]
c.acc[18]
c.acc[19]
c.acc[20]
c.acc[21]
c.acc[22]
c.acc[23]
c.acc[24]
c.acc[25]
c.acc[26]
c.acc[27]
c.acc[28]
c.acc[29]
c.acc[31]


###Random forest

#1)Alcohol(61.54%)

rf.model<-randomForest(Alcohol~.,data = train,ntree=5)

rf.p<-predict(rf.model,val)

confusionMatrix(rf.p,val$Alcohol)

#2)Amphet(68.97%)

rf.model<-randomForest(Amphet~.,data = train,ntree=5)

rf.p<-predict(rf.model,val)

confusionMatrix(rf.p,val$Amphet)

#3)Amyl(83.55%)

rf.model<-randomForest(Amyl~.,data = train,ntree=5)

rf.p<-predict(rf.model,val)

confusionMatrix(rf.p,val$Amyl)

#4)Benzos(71.88%)

rf.model<-randomForest(Benzos~.,data = train,ntree=5)

rf.p<-predict(rf.model,val)

confusionMatrix(rf.p,val$Benzos)

#5)caffeine(89.12%)

rf.model<-randomForest(Caffeine~.,data = train,ntree=5)

rf.p<-predict(rf.model,val)

confusionMatrix(rf.p,val$Caffeine)

#6)cannabis(77.45%)

rf.model<-randomForest(Cannabis~.,data = train,ntree=5)

rf.p<-predict(rf.model,val)

confusionMatrix(rf.p,val$Cannabis)

information.gain(Cannabis~.,train)


rf.model <- list()
rf.p<-list()
DV1<-colnames(drug[,13:31])
IV1<-colnames(drug)


for (y in DV1){
  form <- formula(paste(y, "~", "."))
  rf.model[[y]] <- randomForest(form, data=train,ntree=5) 
  
}
rf.model[1]
lapply(rf.model,summary)



for (i in 1:19) {
  
  
  rf.p[[i]]<-predict(rf.model[i],newdata = val)
  
  
}
rf.p

rf.acc<-list()
for (i in 1:19) {
  l<-as.data.frame(rf.p[i])
  
  
  rf.acc[[i]]<-confusionMatrix(unlist(l),val[,12+i])
  
}

rf.acc[1]
rf.acc[2]
rf.acc[3]
rf.acc[4]
rf.acc[5]
rf.acc[6]
rf.acc[7]
rf.acc[8]
rf.acc[9]
rf.acc[10]
rf.acc[11]
rf.acc[12]
rf.acc[13]
rf.acc[14]
rf.acc[15]
rf.acc[16]
rf.acc[17]
rf.acc[19]

##############  SVM

#1)alcohol(96.02%)
svm.model<-svm(Alcohol~.,data = train)

svm.pr<-predict(svm.model,val)

confusionMatrix(svm.pr,val$Alcohol)

#2)amphet(82.49%)
svm.model<-svm(Amphet~.,data = train)

svm.pr<-predict(svm.model,val)

confusionMatrix(svm.pr,val$Amphet)

#3)amyl(82.49%)
svm.model<-svm(Amyl~.,data = train)

svm.pr<-predict(svm.model,val)

confusionMatrix(svm.pr,val$Amyl)

#4)benzos(79.31%)
svm.model<-svm(Benzos~.,data = train)

svm.pr<-predict(svm.model,val)

confusionMatrix(svm.pr,val$Benzos)

#5)caffeine(98.14%)
svm.model<-svm(Caffeine~.,data = train)

svm.pr<-predict(svm.model,val)

confusionMatrix(svm.pr,val$Caffeine)

#6)cannabis(85.15%)
svm.model<-svm(Cannabis~.,data = train)

svm.pr<-predict(svm.model,val)

confusionMatrix(svm.pr,val$Cannabis)

#7)chocolate(98.94%)
svm.model<-svm(Chocolate~.,data = train)

svm.pr<-predict(svm.model,val)

confusionMatrix(svm.pr,val$Chocolate)


svm.model <- list()
svm.p<-list()
DV1<-colnames(drug[,13:31])
IV1<-colnames(drug)


for (y in DV1){
  form <- formula(paste(y, "~", "."))
  svm.model[[y]] <- svm(form, data=train) 
  
}
svm.model[1]
lapply(svm.model,summary)



for (i in 1:19) {
  
  
  svm.p[[i]]<-predict(svm.model[i],newdata = val)
  
  
}
svm.p

svm.acc<-list()
for (i in 1:19) {
  l<-as.data.frame(svm.p[i])

  
  svm.acc[[i]]<-confusionMatrix(unlist(l),val[,12+i])
  
}

svm.acc[1]
svm.acc[2]
svm.acc[3]
svm.acc[4]
svm.acc[5]
svm.acc[6]
svm.acc[7]
svm.acc[8]
svm.acc[9]
svm.acc[10]
svm.acc[11]
svm.acc[12]
svm.acc[13]
svm.acc[14]
svm.acc[15]
svm.acc[16]
svm.acc[17]
svm.acc[19]


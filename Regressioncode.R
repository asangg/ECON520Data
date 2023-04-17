library(tidyverse)  # library for dealing with tabular data
library(readxl)
library(utils)
library("writexl")
library(dplyr)
library(lmtest)
library(jtools)
setwd("C:/Users/alexs/documents/econ520/paper stuff")

data=read_excel("30countries.xls") 

data=data[data$`Country Name` %in%c("Argentina","Australia", "Brazil", "Canada", "Switzerland", "Chile", "China", "Belgium", "Germany", "Euro area", "France", "Hong Kong SAR, China", "Indonesia", "Ireland", "India", "Italy", "Japan", "Korea, Rep.", "United States", "Netherlands", "Russian Federation", "Saudi Arabia", "Singapore", "United Kingdom", "South Africa", "Turkiye", "Mexico", "Luxembourg", "Cayman Islands", "Spain"),]


data2002=subset(data, 2002>0,select=c('Country Name', '2002'))

data2020=subset(data, 2020>0,select=c('Country Name', '2020'))


#Merge both data
newdata <- merge(data2002,      # Spatial Data layer
                 data2020,         # Table Data layer
                 by = "Country Name")

newdata=na.omit(newdata) #Removes countries with NA values, Cayman Island removed

GDPGrowth=c(0)
newdata$GDPGrowth<-GDPGrowth #Creates New GDPGrowth Column

as.numeric(newdata$GDPGrowth) #Changes GDPGrowth Column to Numeric

#For Loop to calculate the GDP growth rates using formula (2020GDP-2002GDP)/2002GDP
for(x in 1:29){
  newdata$GDPGrowth[x]=(newdata[x,3]-newdata[x,2])/newdata[x,2] 
}


#Load in Bank assets for banks and shadow banks
bankassets=read_excel("BankAssets.xlsx") 

bankassets=bankassets[bankassets$Jurisdiction %in%c("Turkey", "South Korea","Hong Kong", "Argentina","Australia", "Brazil", "Canada", "Switzerland", "Chile", "China", "Belgium", "Germany", "Euro area", "France", "China", "Indonesia", "Ireland", "India", "Italy", "Japan", "United States", "Netherlands", "Russian Federation", "Saudi Arabia", "Singapore", "United Kingdom", "South Africa", "Mexico", "Luxembourg", "Spain"),]


bankassets=bankassets[bankassets$`Entity/Enconomic function` %in%c("Banks","Munfi"),]

bankassets=bankassets[,-(7:9)]

shadowbanks=bankassets[bankassets$`Entity/Enconomic function`=="Munfi",]

banks=bankassets[bankassets$`Entity/Enconomic function`=="Banks",]

#Shadow Bank Total percentage 2002
shadowbanks2002=shadowbanks[shadowbanks$Year==2002,]
shadowbanks2002$Jurisdiction #Missing South Korea, Russia, Cayman Islands

#remove unnecessary columns
shadow_total=shadowbanks2002[,-(3:6),]
shadow_total=shadow_total[,-1,]

colnames(shadow_total)<-c('Country Name','Shadow_Bank_Percentage_2002')

#Banks Total Percentage 2002
banks2002=banks[banks$Year==2002,]

banktotal=banks2002[,-(3:6),]
banktotal=banktotal[,-1,]

colnames(banktotal)<-c('Country Name','Total_Bank_Percentage_2002')

#2020

#Shadow Bank Total percentage 2020
shadowbanks2020=shadowbanks[shadowbanks$Year==2020,]
shadowbanks2020$Jurisdiction #Missing South Korea, Russia, Cayman Islands

#remove unnecessary columns
shadow_total2020=shadowbanks2020[,-(3:6),]
shadow_total2020=shadow_total2020[,-1,]

colnames(shadow_total2020)<-c('Country Name','Shadow_Bank_Percentage_2020')

#Banks Total Percentage 2020
banks2020=banks[banks$Year==2020,]

banktotal2020=banks2020[,-(3:6),]
banktotal2020=banktotal2020[,-1,]

colnames(banktotal2020)<-c('Country Name','Total_Bank_Percentage_2020')


#Recode Country Names
newdata$`Country Name`=recode(newdata$`Country Name`,
                              "Korea, Rep."= "South Korea",
                              "Hong Kong SAR, China" = "Hong Kong",
                              "Turkiye"="Turkey")

#Merge with main data
newdata <- merge(newdata,      # Spatial Data layer
                 shadow_total,         # Table Data layer
                 by = "Country Name")

newdata <- merge(newdata,      # Spatial Data layer
                 shadow_total2020,         # Table Data layer
                 by = "Country Name")

newdata <- merge(newdata,      # Spatial Data layer
                 banktotal,         # Table Data layer
                 by = "Country Name")


newdata <- merge(newdata,      # Spatial Data layer
                 banktotal2020,         # Table Data layer
                 by = "Country Name")

#Change total financial into percentages
# newdata$`Total Shadow Bank Percentage in 2002`[1]
# newdata[2,5]=newdata[2,5]/100



#Nested Loop to Change Columns into percentages
for(x in 5:8){
  for(y in 1:26){
    newdata[y,x]=newdata[y,x]/100
  }
  
}
#for loop to calculate percentage change in shadow banks
newdata$`Total_Shadow_Banks_Percentage_Change`=0
for(x in 1:26){
  newdata$`Total_Shadow_Banks_Percentage_Change`[x]=(newdata[x,6]-newdata[x,5])/newdata[x,5] 
  
}


#for loop to calculate percentage change in banks
newdata$`Total_Banks_Percentage_Change`=0
for(x in 1:26){
  newdata$`Total_Banks_Percentage_Change`[x]=(newdata[x,8]-newdata[x,7])/newdata[x,7] 
  
}

#Change column names

colnames(newdata)[2]="GDP_2002"
colnames(newdata)[3]="GDP_2020"

#REGRESSION ANALYSIS


#Regression of the effects of shadow Banks on GDP for 2002
model <- lm(GDP_2002~Shadow_Bank_Percentage_2002, newdata)
summ(model)

ggplot(newdata, aes(x = Shadow_Bank_Percentage_2002, y = GDP_2002)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Regression of the effects of shadow Banks on GDP for 2020
model <- lm(GDP_2020~Shadow_Bank_Percentage_2020, newdata)
summary(model)

ggplot(newdata, aes(x = Shadow_Bank_Percentage_2020, y = GDP_2020)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#Regression of the growth rate of Shadow Banks on the GDP Growth Rate 
model <- lm(GDPGrowth~Total_Shadow_Banks_Percentage_Change, newdata)
summary(model)

ggplot(newdata, aes(x = Total_Shadow_Banks_Percentage_Change, y = GDPGrowth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#Regression of the effects of banks on GDP for 2002
model <- lm(GDP_2002~Total_Bank_Percentage_2002, newdata)
summary(model)

ggplot(newdata, aes(x = Total_Bank_Percentage_2002, y = GDP_2002)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Regression of the effects of banks on GDP for 2020
model <- lm(GDP_2020~Total_Bank_Percentage_2020, newdata)
summary(model)

ggplot(newdata, aes(x = Total_Bank_Percentage_2020, y = GDP_2020)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Regression of the growth rate of  Banks on the GDP Growth Rate 
model <- lm(GDPGrowth~Total_Banks_Percentage_Change, newdata)
summary(model)

ggplot(newdata, aes(x = Total_Banks_Percentage_Change, y = GDPGrowth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#Regression of the effects of Shadow Banks and Banks for 2002
model <- lm(GDP_2002~Total_Bank_Percentage_2002+Shadow_Bank_Percentage_2002, newdata)
summary(model)

#Regression of the effects of Shadow Banks and Banks for 2020
model <- lm(GDP_2020~Total_Bank_Percentage_2020+Shadow_Bank_Percentage_2020, newdata)
summary(model)


#Add in control variables

#load inflation data
inflation=read_excel("Inflation.xls") 
inflation=inflation[inflation$`Country Name` %in%c("Argentina","Australia", "Brazil", "Canada", "Switzerland", "Chile", "China", "Belgium", "Germany", "Euro area", "France", "Hong Kong SAR, China", "Indonesia", "Ireland", "India", "Italy", "Japan", "Korea, Rep.", "United States", "Netherlands", "Russian Federation", "Saudi Arabia", "Singapore", "United Kingdom", "South Africa", "Turkiye", "Mexico", "Luxembourg", "Cayman Islands", "Spain"),]
inflation2002=subset(inflation, 2002>0,select=c('Country Name', '2002'))

inflation2020=subset(inflation, 2020>0,select=c('Country Name', '2020'))

inflationmerge= merge(inflation2002,      
                      inflation2020,         
                      by = "Country Name")

inflationmerge=na.omit(inflationmerge)
inflationmerge$InflationChange<-0
for(x in 1:29){
  inflationmerge$InflationChange[x]=(inflationmerge[x,3]-inflationmerge[x,2])/inflationmerge[x,2] 
}

colnames(inflationmerge)[2]="Inflation_2002"
colnames(inflationmerge)[3]="Inflation_2020"

inflationmerge$`Country Name`=recode(inflationmerge$`Country Name`,
                                        "Korea, Rep."= "South Korea",
                                        "Hong Kong SAR, China" = "Hong Kong",
                                        "Turkiye"="Turkey")

testnewdata <- merge(newdata,      # Spatial Data layer
                      inflationmerge,         # Table Data layer
                      by = "Country Name")

#Unemployment
unemployment=read_excel("Unemployment.xls") 
unemployment=unemployment[unemployment$`Country Name` %in%c("Argentina","Australia", "Brazil", "Canada", "Switzerland", "Chile", "China", "Belgium", "Germany", "Euro area", "France", "Hong Kong SAR, China", "Indonesia", "Ireland", "India", "Italy", "Japan", "Korea, Rep.", "United States", "Netherlands", "Russian Federation", "Saudi Arabia", "Singapore", "United Kingdom", "South Africa", "Turkiye", "Mexico", "Luxembourg", "Cayman Islands", "Spain"),]
unemployment2002=subset(unemployment, 2002>0,select=c('Country Name', '2002'))

unemployment2020=subset(unemployment, 2020>0,select=c('Country Name', '2020'))

unemployment2020[14,2]=4.47

unemployment2002[17,2]=5.53

unemployment2002[26,2]=6.9

unemploymentmerge= merge(unemployment2002,      
                         unemployment2020,         
                      by = "Country Name")

unemploymentmerge=na.omit(unemploymentmerge)
unemploymentmerge$UnemploymentChange<-0
for(x in 1:30){
  unemploymentmerge$UnemploymentChange[x]=(unemploymentmerge[x,3]-unemploymentmerge[x,2])/unemploymentmerge[x,2] 
}


colnames(unemploymentmerge)[2]="Unemployment_2002"
colnames(unemploymentmerge)[3]="Unemployment_2020"

unemploymentmerge$`Country Name`=recode(unemploymentmerge$`Country Name`,
                                        "Korea, Rep."= "South Korea",
                                        "Hong Kong SAR, China" = "Hong Kong",
                                        "Turkiye"="Turkey")

testnewdata <- merge(testnewdata,      # Spatial Data layer
                     unemploymentmerge,         # Table Data layer
                     by = "Country Name")


#Regression with the added controls
model <- lm(GDPGrowth~Total_Banks_Percentage_Change+Total_Shadow_Banks_Percentage_Change+InflationChange+UnemploymentChange, testnewdata)
summary(model)

write.csv(testnewdata, "CountryGDP.csv")


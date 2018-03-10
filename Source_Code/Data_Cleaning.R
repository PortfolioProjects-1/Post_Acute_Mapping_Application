###--------Import libraries--------###
library(readxl)
library(tidyverse)
library(tidyr)
library(shiny)
library(maps)
library(ggmap)
library(ggplot2)
library(leaflet)
library(stringr)

##global.R
##saveRDS file

###--------------------------------###

###############################################################Get Data###############################################################

##--Read in 2013, 2014, 2015 Provider Aggregate Reports for SNF and HH
setwd('C:/Users/kenne/GIT/ShinyDashboard/Data')
##getwd()

SNF_Location <- read_excel('Inspection_Cycle/Inspection_Cycle_Deficiencies_SNF.xlsx')
SNF_Location  <- SNF_Location[c(1,2,13)]
names(SNF_Location) <- c("Provider_ID","Provider_Name","Location")
SNF_Location <- unique(SNF_Location[c("Provider_ID","Provider_Name","Location")])

##View(SNF_Location)

SNF_Report_2013 <- read_excel('SNF/SNF_aggregate_report_2013.xlsx', sheet = "Provider")
SNF_Report_2014 <- read_excel('SNF/SNF_aggregate_report_2014.xlsx', sheet = "Provider")
SNF_Report_2014 <- SNF_Report_2014[c(1:25,28,34,35)]
SNF_Report_2015 <- read_excel('SNF/SNF_aggregate_report_2015.xlsx', sheet = "SNF_2015")
SNF_Report_2015 <- SNF_Report_2015[c(1:25,28,34,35)]

#HH_Report_2013 <- read_excel('HH/HH_aggregate_report_2013.xlsx', sheet = "Provider")
#HH_Report_2013 <- HH_Report_2013[c(1:33,36,42,43)]

#HH_Report_2014 <- read_excel('HH/HH_aggregate_report_2014.xlsx', sheet = "Provider")
#HH_Report_2014 <- HH_Report_2014[c(1:8,16:33,36,42,43)]

##--Read in latest Star_Ratings 

##--Sanity Check data frames 
##View(SNF_Report_2013)
##View(SNF_Report_2014)
##View(SNF_Report_2015)
##View(HH_Report_2013)
##View(HH_Report_2014)

###############################################################Clean data################################################################
#HH_Report_2013[HH_Report_2013=="*"]<-NA
#HH_Report_2014[HH_Report_2014=="*"]<-NA

SNF_Report_2013[SNF_Report_2013=="*"]<-NA
SNF_Report_2014[SNF_Report_2014=="*"]<-NA
SNF_Report_2015[SNF_Report_2015=="*"]<-NA

#names(HH_Report_2013) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","ALOS_Days","HH_Amount","HH_Medicare_Payment_Amount","HH_Medicare_Standard_Payment_Amount",
#  "Pct_Outlier_Payments","LUPA_Episodes","HH_Medicare_Payment_Amount_LUPA")

#names(HH_Report_2014) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","Total_Beneficiaries","HH_Amount","HH_Medicare_Allowed_Amount","HH_Medicare_Payment_Amount","HH_Medicare_Standard_Payment_Amount",
#                           "Pct_Outlier_Payments","LUPA_Episodes","Avg_Age","Male_Beneficiaries","Female_Beneficiaries","NonDual_Beneficiaries","Dual_Beneficiaries","White_Beneficiaries","Black_Beneficiaries","Asian_Beneficiaries","Hispanic_Beneficiaries","Native_Beneficiaries","Other_Beneficiaries","Average_HCC_Score","Pct_Beneficiaries_Asthma","Pct_Beneficiaries_Diabetes","Pct_Beneficiaries_Hyperlipidemia")

names(SNF_Report_2013) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","Total_Beneficiaries","ALOS_Days","SNF_Amount","SNF_Medicare_Allowed_Amount","SNF_Medicare_Payment_Amount","SNF_Medicare_Standard_Payment_Amount")
names(SNF_Report_2014) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","Total_Beneficiaries","ALOS_Days","SNF_Amount","SNF_Medicare_Allowed_Amount","SNF_Medicare_Payment_Amount","SNF_Medicare_Standard_Payment_Amount",
                            "Avg_Age","Male_Beneficiaries","Female_Beneficiaries","NonDual_Beneficiaries","Dual_Beneficiaries","White_Beneficiaries","Black_Beneficiaries","Asian_Beneficiaries","Hispanic_Beneficiaries","Native_Beneficiaries","Other_Beneficiaries","Average_HCC_Score","Pct_Beneficiaries_Asthma","Pct_Beneficiaries_Diabetes","Pct_Beneficiaries_Hyperlipidemia")

names(SNF_Report_2015) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","Total_Beneficiaries","ALOS_Days","SNF_Amount","SNF_Medicare_Allowed_Amount","SNF_Medicare_Payment_Amount","SNF_Medicare_Standard_Payment_Amount",
                            "Avg_Age","Male_Beneficiaries","Female_Beneficiaries","NonDual_Beneficiaries","Dual_Beneficiaries","White_Beneficiaries","Black_Beneficiaries","Asian_Beneficiaries","Hispanic_Beneficiaries","Native_Beneficiaries","Other_Beneficiaries","Average_HCC_Score","Pct_Beneficiaries_Asthma","Pct_Beneficiaries_Diabetes","Pct_Beneficiaries_Hyperlipidemia")

##---Round 2014 columns 
SNF_Report_2014$Pct_Beneficiaries_Asthma <- round(as.numeric(SNF_Report_2014$Pct_Beneficiaries_Asthma), digits = 2)

SNF_Report_2014$Pct_Beneficiaries_Diabetes <- round(as.numeric(SNF_Report_2014$Pct_Beneficiaries_Diabetes), digits = 2)

SNF_Report_2014$Pct_Beneficiaries_Hyperlipidemia <- round(as.numeric(SNF_Report_2014$Pct_Beneficiaries_Hyperlipidemia), digits = 2)

##---Round 2015 columns
SNF_Report_2015$Pct_Beneficiaries_Asthma <- round(as.numeric(SNF_Report_2015$Pct_Beneficiaries_Asthma), digits = 2)

SNF_Report_2015$Pct_Beneficiaries_Diabetes <- round(as.numeric(SNF_Report_2015$Pct_Beneficiaries_Diabetes), digits = 2)

SNF_Report_2015$Pct_Beneficiaries_Hyperlipidemia <- round(as.numeric(SNF_Report_2015$Pct_Beneficiaries_Hyperlipidemia), digits = 2)

#View(SNF_Report_2013)

#View(SNF_Report_2014)

#View(SNF_Report_2015)

#View(HH_Report_2013)

#View(HH_Report_2014)

###############################################################Combine Data##############################################################
SNF_Report_2013 <- merge(SNF_Report_2013,SNF_Location,by="Provider_ID")
SNF_Report_2013 <- SNF_Report_2013[c("Provider_ID", "Facility_Name", "Street_Address", "City", "State", "Zip_Code", "Total_Stays","Total_Beneficiaries",
                                     "ALOS_Days", "SNF_Amount", "SNF_Medicare_Allowed_Amount", "SNF_Medicare_Payment_Amount", "SNF_Medicare_Standard_Payment_Amount",
                                     "Location")]
#View(SNF_Report_2013)


SNF_Report_2014 <- merge(SNF_Report_2014,SNF_Location,by="Provider_ID")
SNF_Report_2014 <- SNF_Report_2014[c("Provider_ID", "Facility_Name", "Street_Address", "City", "State", "Zip_Code", "Total_Stays","Total_Beneficiaries",
                                     "ALOS_Days", "SNF_Amount", "SNF_Medicare_Allowed_Amount", "SNF_Medicare_Payment_Amount", "SNF_Medicare_Standard_Payment_Amount",
                                     "Location")]
#View(SNF_Report_2014)

SNF_Report_2015 <- merge(SNF_Report_2015,SNF_Location,by="Provider_ID")
SNF_Report_2015 <- SNF_Report_2015[c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","Total_Beneficiaries","ALOS_Days","SNF_Amount","SNF_Medicare_Allowed_Amount","SNF_Medicare_Payment_Amount","SNF_Medicare_Standard_Payment_Amount",
                                     "Avg_Age","Male_Beneficiaries","Female_Beneficiaries","NonDual_Beneficiaries","Dual_Beneficiaries","White_Beneficiaries","Black_Beneficiaries","Asian_Beneficiaries","Hispanic_Beneficiaries","Native_Beneficiaries","Other_Beneficiaries","Average_HCC_Score","Pct_Beneficiaries_Asthma","Pct_Beneficiaries_Diabetes","Pct_Beneficiaries_Hyperlipidemia","Location")]

SNF_Report_2015 <- separate(SNF_Report_2015, Location, into = c("long", "lat"), sep = ",")
SNF_Report_2015$long <- as.numeric(gsub("\\(", "", SNF_Report_2015$long))
SNF_Report_2015$lat <- as.numeric(gsub("\\)", "", SNF_Report_2015$lat))

#View(SNF_TX)
#View(SNF_Report_2015)

#leaflet(SNF_TX) %>% addTiles() %>% addMarkers(lat = ~long, lng = ~lat) %>% setView(lng = -99.7331, lat = 32.4487, zoom = 5)

#df <- SNF_Report_2015 %>% 
  #group_by(State) %>%
  #summarise(number = n())

#View(df)

###############################################################Define Caclulated Fields###############################################################
##--Define calculated measures (total visits per disease, total cost per disease, avg cost per day at facility)

##--HH_Report_2014
##HH_Report_2014$Cost_Per_Stay <- (HH_Report_2014$Total_Stays)/(HH_Report_2014$HH_Amount)

#HH_Report_2014$Total_Stays_Asthma <- HH_Report_2013$Total_Stays*HH_Report_2014$Pct_Beneficiaries_Asthma
  
#HH_Report_2014$Total_Stays_Diabetes <- HH_Report_2014$Total_Stays*HH_Report_2014$Pct_Beneficiaries_Diabetes
  
#HH_Report_2014$Hyperlipidemia <- HH_Report_2014$Total_Stays*HH_Report_2014$Pct_Beneficiaries_Hyperlipidemia
  

##--SNF_Report_2014
SNF_Report_2014$Cost_Per_Stay <- NA

SNF_Report_2014$Cost_Per_Day <- NA

SNF_Report_2014$Total_Stays_Asthma <- NA
  
SNF_Report_2014$Diabetes <- NA
  
SNF_Report_2014$Hyperlipidemia <- NA

SNF_Report_2014$Year <- '2014'
  
#View(SNF_Report_2014)

##--SNF_Report_2015
SNF_Report_2015$Cost_Per_Stay <- (SNF_Report_2015$SNF_Amount)/(SNF_Report_2015$Total_Stays)

SNF_Report_2015$Cost_Per_Day <- (SNF_Report_2015$SNF_Amount)/(SNF_Report_2015$Total_Stays*SNF_Report_2015$ALOS_Days)

SNF_Report_2015$Total_Stays_Asthma <- SNF_Report_2015$Total_Stays*SNF_Report_2015$Pct_Beneficiaries_Asthma
  
SNF_Report_2015$Total_Stays_Diabetes <- SNF_Report_2015$Total_Stays*SNF_Report_2015$Pct_Beneficiaries_Diabetes
  
SNF_Report_2015$Total_Stays_Hyperlipidemia <- SNF_Report_2015$Total_Stays*SNF_Report_2015$Pct_Beneficiaries_Hyperlipidemia

SNF_Report_2015$Year <- '2015'

SNF_2015_TX <- subset(SNF_Report_2015, State == "TX")

SNF_2015_TX$Total_Stays_Asthma[is.na(SNF_2015_TX$Total_Stays_Asthma)] <- 0

SNF_2015_TX$Total_Stays_Diabetes[is.na(SNF_2015_TX$Total_Stays_Diabetes)] <- 0

SNF_2015_TX$Total_Stays_Hyperlipidemia[is.na(SNF_2015_TX$Total_Stays_Hyperlipidemia)] <- 0

#View(SNF_2015_TX)

###############################################################Define Aggregate Dfs###############################################################
saveRDS(SNF_2015_TX, file = 'SNF_2015_TX.rds')


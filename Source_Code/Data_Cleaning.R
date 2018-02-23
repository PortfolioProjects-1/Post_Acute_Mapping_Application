###--------Import libraries--------###
library(readxl)
library(tidyverse)

###--------------------------------###

###############################################################Get Data###############################################################

##--Read in 2013, 2014, 2015 Provider Aggregate Reports for SNF and HH
setwd('C:/Users/kenne/GIT/ShinyDashboard/Data')
##getwd()

SNF_Report_2013 <- read_excel('SNF/SNF_aggregate_report_2013.xlsx', sheet = "Provider")
SNF_Report_2014 <- read_excel('SNF/SNF_aggregate_report_2014.xlsx', sheet = "Provider")
SNF_Report_2014 <- SNF_Report_2014[c(1:25,28,34,35)]
SNF_Report_2015 <- read_excel('SNF/SNF_aggregate_report_2015.xlsx', sheet = "SNF_2015")
SNF_Report_2015 <- SNF_Report_2015[c(1:25,28,34,35)]

HH_Report_2013 <- read_excel('HH/HH_aggregate_report_2013.xlsx', sheet = "Provider")
HH_Report_2013 <- HH_Report_2013[c(1:33,36,42,43)]

HH_Report_2014 <- read_excel('HH/HH_aggregate_report_2014.xlsx', sheet = "Provider")
HH_Report_2014 <- HH_Report_2014[c(1:8,16:33,36,42,43)]

##--Read in latest Star_Ratings 

##--Sanity Check data frames 
##View(SNF_Report_2013)
##View(SNF_Report_2014)
##View(SNF_Report_2015)
##View(HH_Report_2013)
##View(HH_Report_2014)

###############################################################Combine Data##############################################################


###############################################################Clean data################################################################
HH_Report_2013[HH_Report_2013=="*"]<-NA
HH_Report_2014[HH_Report_2014=="*"]<-NA

SNF_Report_2013[SNF_Report_2013=="*"]<-NA
SNF_Report_2014[SNF_Report_2014=="*"]<-NA
SNF_Report_2015[SNF_Report_2015=="*"]<-NA

names(HH_Report_2013) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","ALOS_(Days)","HH_Amount","HH_Medicare_Payment_Amount","HH_Medicare_Standard_Payment_Amount",
  "Pct_Outlier_Payments","LUPA_Episodes","HH_Medicare_Payment_Amount_LUPA")

names(HH_Report_2014) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","Total_Beneficiaries","HH_Amount","HH_Medicare_Allowed_Amount","HH_Medicare_Payment_Amount","HH_Medicare_Standard_Payment_Amount",
                           "Pct_Outlier_Payments","LUPA_Episodes","Avg_Age","Male_Beneficiaries","Female_Beneficiaries","NonDual_Beneficiaries","Dual_Beneficiaries","White_Beneficiaries","Black_Beneficiaries","Asian_Beneficiaries","Hispanic_Beneficiaries","Native_Beneficiaries","Other_Beneficiaries","Average_HCC_Score","Pct_Beneficiaries_Asthma","Pct_Beneficiaries_Diabetes","Pct_Beneficiaries_Hyperlipidemia")

names(SNF_Report_2013) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","Total_Beneficiaries","ALOS_(Days)","SNF_Amount","SNF_Medicare_Allowed_Amount","SNF_Medicare_Payment_Amount","SNF_Medicare_Standard_Payment_Amount")
names(SNF_Report_2014) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","Total_Beneficiaries","ALOS_(Days)","SNF_Amount","SNF_Medicare_Allowed_Amount","SNF_Medicare_Payment_Amount","SNF_Medicare_Standard_Payment_Amount",
                            "Avg_Age","Male_Beneficiaries","Female_Beneficiaries","NonDual_Beneficiaries","Dual_Beneficiaries","White_Beneficiaries","Black_Beneficiaries","Asian_Beneficiaries","Hispanic_Beneficiaries","Native_Beneficiaries","Other_Beneficiaries","Average_HCC_Score","Pct_Beneficiaries_Asthma","Pct_Beneficiaries_Diabetes","Pct_Beneficiaries_Hyperlipidemia")

names(SNF_Report_2015) <- c("Provider_ID", "Facility_Name", "Street_Address", "City", "State","Zip_Code","Total_Stays","Total_Beneficiaries","ALOS_(Days)","SNF_Amount","SNF_Medicare_Allowed_Amount","SNF_Medicare_Payment_Amount","SNF_Medicare_Standard_Payment_Amount",
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

###############################################################Define Caclulated Fields###############################################################

##--Define calculated measures (total visits per disease, total cost per disease, avg cost per day at facility)

HH_Report_2013$Total_Stays_Asthma <-

HH_Report_2013$Diabetes <-

HH_Report_2013$Hyperlipidemia <- 

  
HH_Report_2014$Total_Stays_Asthma <-
  
HH_Report_2014$Diabetes <-
  
HH_Report_2014$Hyperlipidemia <-
  

SNF_Report_2014$Total_Stays_Asthma <-
  
SNF_Report_2014$Diabetes <-
  
SNF_Report_2014$Hyperlipidemia <-
  
  
SNF_Report_2015$Total_Stays_Asthma <-
  
SNF_Report_2015$Diabetes <-
  
SNF_Report_2015$Hyperlipidemia <-

###############################################################Define Aggregate Dfs###############################################################

##--Define Aggregate data sets 



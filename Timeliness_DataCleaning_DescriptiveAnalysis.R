#clear the environment

rm(list=ls())

#set working directory

wdir<-setwd("C:/Users/arman/OneDrive/ECV Paper writing/Base_ECV/Base des données ECVs")

library(tidyr)
library(tidyverse)
library(data.table)
library(dplyr)

#import all 3 datasets
BD2021<-read.csv("Base ECV 2021.csv")
BD2022<-read.csv("Base ECV 2022.csv")
BD2023<-read.csv("Base ECV 2023.csv")

# compare Columns that are common to all 3 datasets
Reduce(intersect, list(names(BD2021), names(BD2022), names(BD2023)))

# All unique column names across all datasets (union)
Reduce(union, list(names(BD2021), names(BD2022), names(BD2023)))

#Columns in one but not in others

all_columns <- Reduce(union, list(
  names(BD2021),
  names(BD2022),
  names(BD2023)
))

column_comparison <- tibble(
  column = all_columns,
  in_BD2021 = column %in% names(BD2021),
  in_BD2022 = column %in% names(BD2022),
  in_BD2023 = column %in% names(BD2023)
)

print(column_comparison)

#create a csv_file for the comparison 
write.csv(column_comparison,"comparaison_variables_bd_ECV2123.csv")

#read the file comparing ECV variables names
compare_ecvs<-read.csv("comparaison_variables_bd_ECV2123.csv")

#explore the variable related to the possession of vaccination card
#table(BD2020$im5)
table(BD2021$vs27)
table(BD2022$vs27)
table(BD2023$vs27)

#subset each of the dataset according to the presence/possession of a vaccination card

#BD2020
#table(BD2020$im5)

#bd2020<-subset(BD2020, 
 #              im5=="oui, carnet/carte et autre documents vu"|
  #               im5=="oui, seulement un autre document vu"|
   #              im5=="oui, seulement un carnet/carte vu")

#BD2021
table(BD2021$vs27)
bd2021<-subset(BD2021, 
               vs27=="OUI, AUTRE DOCUMENT SEULEMENT VU"|
                 vs27=="OUI, CARNET/CARTE ET AUTRE DOCUMENTS VU"|
                 vs27=="OUI, CARNET/CARTE SEULEMENT VU")

#BD2022
table(bd2022$vs27)
bd2022<-subset(BD2022, 
               vs27=="OUI, AUTRE DOCUMENT SEULEMENT VU"|
                 vs27=="OUI, CARNET/CARTE ET AUTRE DOCUMENTS VU"|
                 vs27=="OUI, CARNET/CARTE SEULEMENT VU")

#BD2023
table(bd2023$vs27)
bd2023<-subset(BD2023, 
               vs27=="Oui, a seulement un autre document vu"|
                 vs27=="Oui, a seulement un carnet ou une carte de vaccination vu"|
                 vs27=="Oui, a un carte ou une carte et un autre document vu")



#create variable date of birth 2020 from debutub1
#bd2020$birthdate<-bd2020$debutub1

#create variable date of birth from vs23(year)-vs22(month)-vs21(day)
bd2021 <- bd2021 %>%
  mutate(birthdate = paste(vs23, vs22, vs21, sep = "-"))

bd2022 <- bd2022 %>%
  mutate(birthdate = paste(vs23, vs22, vs21, sep = "-"))

bd2023 <- bd2023 %>%
  mutate(birthdate = paste(vs23, vs22, vs21, sep = "-"))

#check class of birthdate

#class(bd2020$birthdate)
class(bd2021$birthdate)
class(bd2022$birthdate)
class(bd2023$birthdate)

#table(bd2020$birthdate)
table(bd2021$birthdate)
table(bd2022$birthdate)
table(bd2023$birthdate)

library(lubridate)

# Apply to the birthdate column in 2020 dataset
#bd2020$birthdate <- parse_date_time(bd2020$birthdate, orders = c("d b y", "Y-m-d"))
#class(bd2020$birthdate)

#bd2020$birthdate<-as.Date(bd2020$birthdate)
bd2021$birthdate<-as.Date(bd2021$birthdate)
bd2022$birthdate<-as.Date(bd2022$birthdate)
bd2023$birthdate<-as.Date(bd2023$birthdate)

# Replace birthdate values with NA if out of range
#bd2020$birthdate[bd2020$birthdate < as.Date("2018-10-10") | bd2020$birthdate > as.Date("2021-11-24")] <- NA
bd2021$birthdate[bd2021$birthdate < as.Date("2019-09-06") | bd2021$birthdate > as.Date("2022-02-27")] <- NA
bd2022$birthdate[bd2022$birthdate < as.Date("2021-02-06") | bd2022$birthdate > as.Date("2023-04-29")] <- NA
bd2023$birthdate[bd2023$birthdate < as.Date("2022-04-01") | bd2023$birthdate > as.Date("2024-06-15")] <- NA


#Create Variables date of visit

#bd2020$date_visit<-bd2020$datevis
bd2021$date_visit<-bd2021$q117
bd2022$date_visit<-bd2022$q117
bd2023$date_visit<-bd2023$q117

#class(bd2020$date_visit)
class(bd2021$date_visit)
class(bd2022$date_visit)
class(bd2023$date_visit)

#bd2020$date_visit<- parse_date_time(bd2020$date_visit, orders = c("d b y", "Y-m-d"))
bd2023$date_visit<- parse_date_time(bd2023$date_visit, orders = c("d b y", "Y-m-d"))

#convert date of visit into a date format
#bd2020$date_visit<-as.Date(bd2020$date_visit)
bd2021$date_visit<-as.Date(bd2021$date_visit)
bd2022$date_visit<-as.Date(bd2022$date_visit)
bd2023$date_visit<-as.Date(bd2023$date_visit)

#(bd2020$date_visit)
table(bd2021$date_visit)
table(bd2022$date_visit)
table(bd2023$date_visit)

# Replace date_visit values with NA if out of range
#bd2020$date_visit[bd2020$date_visit < as.Date("2020-10-10") | bd2020$date_visit > as.Date("2021-11-24")] <- NA
bd2021$date_visit[bd2021$date_visit < as.Date("2021-09-06") | bd2021$date_visit > as.Date("2022-02-27")] <- NA
bd2022$date_visit[bd2022$date_visit < as.Date("2023-02-06") | bd2022$date_visit > as.Date("2023-04-29")] <- NA
bd2023$date_visit[bd2023$date_visit < as.Date("2024-04-01") | bd2023$date_visit > as.Date("2024-06-15")] <- NA


#Create variable age at which the child actually received each vaccine
#actual age = date of visit - date of birth
#bd2020$age_days<- as.numeric(difftime(bd2020$date_visit, bd2020$birthdate, units = "days"))
#bd2020$age_months<-bd2020$age_days/30.4375

bd2021$age_days<- as.numeric(difftime(bd2021$date_visit, bd2021$birthdate, units = "days"))
bd2021$age_months<-bd2021$age_days/30.4375

bd2022$age_days<- as.numeric(difftime(bd2022$date_visit, bd2022$birthdate, units = "days"))
bd2022$age_months<-bd2022$age_days/30.4375

bd2023$age_days<- as.numeric(difftime(bd2023$date_visit, bd2023$birthdate, units = "days"))
bd2023$age_months<-bd2023$age_days/30.4375

#BASE DES DONNEES DES ENFANTS A PARTIR DE 12 MOIS
#bd2020<-subset(bd2020, age_months>=12)
bd2021<-subset(bd2021, age_months>=12)
bd2022<-subset(bd2022, age_months>=12)
bd2023<-subset(bd2023, age_months>=12)

###########################################################################

#N°1 For BCG_2023

##########Timeliness_bcg##################################################

class(bd2023$date_bcg)
bd2023$date_bcg <- parse_date_time(bd2023$date_bcg, orders = c("d b y", "Y-m-d"))
bd2023$date_bcg<-as.Date(bd2023$date_bcg)
table(bd2023$date_bcg)

# Extract the date range and replace out-of-range dates with NA
start_date_2023 <- as.Date("2022-04-01")
end_date_2023 <- as.Date("2024-04-01")

bd2023 <- bd2023 %>%
  mutate(date_bcg = ifelse(date_bcg >= start_date_2023 & date_bcg <= end_date_2023, date_bcg, as.Date(NA)))

bd2023$date_bcg<-as.Date(bd2023$date_bcg)

#Check the range of dates
table(bd2023$date_bcg)
table(bd2023$birthdate)

#for age_bcg (age at which child received BCG) is less to 0
# CONDITION 1. Transform 'date_bcg' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2023$age_bcg <- as.numeric(difftime(bd2023$date_bcg, bd2023$birthdate, units = "days"))
bd2023$date_bcg <- ifelse(bd2023$date_bcg < bd2023$birthdate & bd2023$age_bcg < -365, NA, bd2023$date_bcg)
bd2023$date_bcg<-as.Date(bd2023$date_bcg)

# condition 2. Transform 'date_bcg' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2023$date_bcg <- ifelse(
  bd2023$date_bcg < bd2023$birthdate & bd2023$age_bcg >= -365,  # Condition to check
  bd2023$date_bcg + lubridate::years(1),                  # Value if condition is true
  bd2023$date_bcg                                         # Value if condition is false
)


bd2023$date_bcg<-as.Date(bd2023$date_bcg)
bd2023$date_bcgT<-bd2023$date_bcg

#CALCULATE Number of days at which the child received BGC vaccine
bd2023$age_bcg <- as.numeric(difftime(bd2023$date_bcg, bd2023$birthdate, units = "days"))
bd2023$age_bcg_w <- as.numeric(difftime(bd2023$date_bcg, bd2023$birthdate, units = "weeks"))

###########################################################################

#N°1 For BCG_2022

##########Timeliness_bcg##################################################

class(bd2022$date_bcg)
bd2022$date_bcg <- parse_date_time(bd2022$date_bcg, orders = c("d b y", "Y-m-d"))
bd2022$date_bcg<-as.Date(bd2022$date_bcg)
table(bd2022$date_bcg)

# Extract the date range and replace out-of-range dates with NA
table(bd2022$q117)
start_date_2022 <- as.Date("2021-02-06")
end_date_2022 <- as.Date("2023-02-06")

bd2022 <- bd2022 %>%
  mutate(date_bcg = ifelse(date_bcg >= start_date_2022 & date_bcg <= end_date_2022, date_bcg, as.Date(NA)))

bd2022$date_bcg<-as.Date(bd2022$date_bcg)

#Check the range of dates
table(bd2022$date_bcg)
table(bd2022$birthdate)

#for age_bcg (age at which child received BCG) is less to 0
# CONDITION 1. Transform 'date_bcg' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2022$age_bcg <- as.numeric(difftime(bd2022$date_bcg, bd2022$birthdate, units = "days"))
bd2022$date_bcg <- ifelse(bd2022$date_bcg < bd2022$birthdate & bd2022$age_bcg < -365, NA, bd2022$date_bcg)
bd2022$date_bcg<-as.Date(bd2022$date_bcg)

# condition 2. Transform 'date_bcg' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2022$date_bcg <- ifelse(
  bd2022$date_bcg < bd2022$birthdate & bd2022$age_bcg >= -365,  # Condition to check
  bd2022$date_bcg + lubridate::years(1),                  # Value if condition is true
  bd2022$date_bcg                                         # Value if condition is false
)


bd2022$date_bcg<-as.Date(bd2022$date_bcg)
bd2022$date_bcgT<-bd2022$date_bcg

#CALCULATE Number of days at which the child received BGC vaccine
bd2022$age_bcg <- as.numeric(difftime(bd2022$date_bcg, bd2022$birthdate, units = "days"))
bd2022$age_bcg_w <- as.numeric(difftime(bd2022$date_bcg, bd2022$birthdate, units = "weeks"))

###########################################################################

#N°1 For BCG_2021

##########Timeliness_bcg##################################################

class(bd2021$date_bcg)
bd2021$date_bcg <- parse_date_time(bd2021$date_bcg, orders = c("d b y", "Y-m-d"))
bd2021$date_bcg<-as.Date(bd2021$date_bcg)
table(bd2021$date_bcg)

# Extract the date range and replace out-of-range dates with NA
start_date_2021 <- as.Date("2019-09-06")
end_date_2021 <- as.Date("2021-09-06")

bd2021 <- bd2021 %>%
  mutate(date_bcg = ifelse(date_bcg >= start_date_2021 & date_bcg <= end_date_2021, date_bcg, as.Date(NA)))

bd2021$date_bcg<-as.Date(bd2021$date_bcg)

#Check the range of dates
table(bd2021$date_bcg)
table(bd2021$birthdate)

#for age_bcg (age at which child received BCG) is less to 0
# CONDITION 1. Transform 'date_bcg' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2021$age_bcg <- as.numeric(difftime(bd2021$date_bcg, bd2021$birthdate, units = "days"))
bd2021$date_bcg <- ifelse(bd2021$date_bcg < bd2021$birthdate & bd2021$age_bcg < -365, NA, bd2021$date_bcg)
bd2021$date_bcg<-as.Date(bd2021$date_bcg)

# condition 2. Transform 'date_bcg' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2021$date_bcg <- ifelse(
  bd2021$date_bcg < bd2021$birthdate & bd2021$age_bcg >= -365,  # Condition to check
  bd2021$date_bcg + lubridate::years(1),                  # Value if condition is true
  bd2021$date_bcg                                         # Value if condition is false
)


bd2021$date_bcg<-as.Date(bd2021$date_bcg)
bd2021$date_bcgT<-bd2021$date_bcg

#CALCULATE Number of days at which the child received BGC vaccine
bd2021$age_bcg <- as.numeric(difftime(bd2021$date_bcg, bd2021$birthdate, units = "days"))
bd2021$age_bcg_w <- as.numeric(difftime(bd2021$date_bcg, bd2021$birthdate, units = "weeks"))

###########################################################################

#N°1 For BCG_2020

##########Timeliness_bcg##################################################

# class(bd2020$date_bcg)
# bd2020$date_bcg <- parse_date_time(bd2020$date_bcg, orders = c("d b y", "Y-m-d"))
# bd2020$date_bcg<-as.Date(bd2020$date_bcg)
# table(bd2020$date_bcg)
# 
# # Extract the date range and replace out-of-range dates with NA
# start_date_2020 <- as.Date("2018-10-10")
# end_date_2020 <- as.Date("2020-10-10")
# 
# bd2020 <- bd2020 %>%
#   mutate(date_bcg = ifelse(date_bcg >= start_date_2020 & date_bcg <= end_date_2020, date_bcg, as.Date(NA)))
# 
# bd2020$date_bcg<-as.Date(bd2020$date_bcg)
# 
# #Check the range of dates
# table(bd2020$date_bcg)
# table(bd2020$birthdate)

#for age_bcg (age at which child received BCG) is less to 0
# CONDITION 1. Transform 'date_bcg' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
# bd2020$age_bcg <- as.numeric(difftime(bd2020$date_bcg, bd2020$birthdate, units = "days"))
# bd2020$date_bcg <- ifelse(bd2020$date_bcg < bd2020$birthdate & bd2020$age_bcg < -365, NA, bd2020$date_bcg)
# bd2020$date_bcg<-as.Date(bd2020$date_bcg)
# 
# # condition 2. Transform 'date_bcg' if conditions are met
# # condition 2. Transform 'date_vpob0' if conditions are met
# bd2020$date_bcg <- ifelse(
#   bd2020$date_bcg < bd2020$birthdate & bd2020$age_bcg >= -365,  # Condition to check
#   bd2020$date_bcg + lubridate::years(1),                  # Value if condition is true
#   bd2020$date_bcg                                         # Value if condition is false
# )
# 
# 
# bd2020$date_bcg<-as.Date(bd2020$date_bcg)
# bd2020$date_bcgT<-bd2020$date_bcg
# 
# #CALCULATE Number of days at which the child received BGC vaccine
# bd2020$age_bcg <- as.numeric(difftime(bd2020$date_bcg, bd2020$birthdate, units = "days"))
# bd2020$age_bcg_w <- as.numeric(difftime(bd2020$date_bcg, bd2020$birthdate, units = "weeks"))

###########################################################################

#N°2 For VPOB0_2023

##########Timeliness_vpob0##################################################
table(bd2023$date_vpob0)
class(bd2023$date_vpob0)
bd2023$date_vpob0 <- parse_date_time(bd2023$date_vpob0, orders = c("d b y", "Y-m-d"))
bd2023$date_vpob0<-as.Date(bd2023$date_vpob0)
table(bd2023$date_vpob0)

# Extract the date range and replace out-of-range dates with NA

bd2023 <- bd2023 %>%
  mutate(date_vpob0 = ifelse(date_vpob0 >= start_date_2023 & date_vpob0 <= end_date_2023, date_vpob0, as.Date(NA)))

bd2023$date_vpob0<-as.Date(bd2023$date_vpob0)

#Check the range of dates
table(bd2023$date_vpob0)
table(bd2023$birthdate)

#for age_vpob0 (age at which child received VPOB0) is less to 0
# CONDITION 1. Transform 'date_vpob0' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2023$age_vpob0 <- as.numeric(difftime(bd2023$date_vpob0, bd2023$birthdate, units = "days"))
bd2023$date_vpob0 <- ifelse(bd2023$date_vpob0 < bd2023$birthdate & bd2023$age_vpob0 < -365, NA, bd2023$date_vpob0)
bd2023$date_vpob0<-as.Date(bd2023$date_vpob0)

# condition 2. Transform 'date_vpob0' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2023$date_vpob0 <- ifelse(
  bd2023$date_vpob0 < bd2023$birthdate & bd2023$age_vpob0 >= -365,  # Condition to check
  bd2023$date_vpob0 + lubridate::years(1),                  # Value if condition is true
  bd2023$date_vpob0                                         # Value if condition is false
)


bd2023$date_vpob0<-as.Date(bd2023$date_vpob0)
bd2023$date_vpob0T<-bd2023$date_vpob0

#CALCULATE Number of days at which the child received VPOB0 vaccine
bd2023$age_vpob0 <- as.numeric(difftime(bd2023$date_vpob0, bd2023$birthdate, units = "days"))
bd2023$age_vpob0_w <- as.numeric(difftime(bd2023$date_vpob0, bd2023$birthdate, units = "weeks"))

###########################################################################

#N°2 For VPOB0_2022

##########Timeliness_vpob0##################################################

class(bd2022$date_vpob0)
bd2022$date_vpob0 <- parse_date_time(bd2022$date_vpob0, orders = c("d b y", "Y-m-d"))
bd2022$date_vpob0<-as.Date(bd2022$date_vpob0)
table(bd2022$date_vpob0)

# Extract the date range and replace out-of-range dates with NA
table(bd2022$q117)
start_date_2022 <- as.Date("2021-02-06")
end_date_2022 <- as.Date("2023-02-06")

bd2022 <- bd2022 %>%
  mutate(date_vpob0 = ifelse(date_vpob0 >= start_date_2022 & date_vpob0 <= end_date_2022, date_vpob0, as.Date(NA)))

bd2022$date_vpob0<-as.Date(bd2022$date_vpob0)

#Check the range of dates
table(bd2022$date_vpob0)
table(bd2022$birthdate)

#for age_vpob0 (age at which child received VPOB0) is less to 0
# CONDITION 1. Transform 'date_vpob0' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2022$age_vpob0 <- as.numeric(difftime(bd2022$date_vpob0, bd2022$birthdate, units = "days"))
bd2022$date_vpob0 <- ifelse(bd2022$date_vpob0 < bd2022$birthdate & bd2022$age_vpob0 < -365, NA, bd2022$date_vpob0)
bd2022$date_vpob0<-as.Date(bd2022$date_vpob0)

# condition 2. Transform 'date_vpob0' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2022$date_vpob0 <- ifelse(
  bd2022$date_vpob0 < bd2022$birthdate & bd2022$age_vpob0 >= -365,  # Condition to check
  bd2022$date_vpob0 + lubridate::years(1),                  # Value if condition is true
  bd2022$date_vpob0                                         # Value if condition is false
)


bd2022$date_vpob0<-as.Date(bd2022$date_vpob0)
bd2022$date_vpob0T<-bd2022$date_vpob0

#CALCULATE Number of days at which the child received VPOB0 vaccine
bd2022$age_vpob0 <- as.numeric(difftime(bd2022$date_vpob0, bd2022$birthdate, units = "days"))
bd2022$age_vpob0_w <- as.numeric(difftime(bd2022$date_vpob0, bd2022$birthdate, units = "weeks"))

###########################################################################

#N°2 For VPOB0_2021

##########Timeliness_vpob0##################################################

class(bd2021$date_vpob0)
bd2021$date_vpob0 <- parse_date_time(bd2021$date_vpob0, orders = c("d b y", "Y-m-d"))
bd2021$date_vpob0<-as.Date(bd2021$date_vpob0)
table(bd2021$date_vpob0)

# Extract the date range and replace out-of-range dates with NA
start_date_2021 <- as.Date("2019-09-06")
end_date_2021 <- as.Date("2021-09-06")

bd2021 <- bd2021 %>%
  mutate(date_vpob0 = ifelse(date_vpob0 >= start_date_2021 & date_vpob0 <= end_date_2021, date_vpob0, as.Date(NA)))

bd2021$date_vpob0<-as.Date(bd2021$date_vpob0)

#Check the range of dates
table(bd2021$date_vpob0)
table(bd2021$birthdate)

#for age_vpob0 (age at which child received VPOB0) is less to 0
# CONDITION 1. Transform 'date_vpob0' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2021$age_vpob0 <- as.numeric(difftime(bd2021$date_vpob0, bd2021$birthdate, units = "days"))
bd2021$date_vpob0 <- ifelse(bd2021$date_vpob0 < bd2021$birthdate & bd2021$age_vpob0 < -365, NA, bd2021$date_vpob0)
bd2021$date_vpob0<-as.Date(bd2021$date_vpob0)

# condition 2. Transform 'date_vpob0' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2021$date_vpob0 <- ifelse(
  bd2021$date_vpob0 < bd2021$birthdate & bd2021$age_vpob0 >= -365,  # Condition to check
  bd2021$date_vpob0 + lubridate::years(1),                  # Value if condition is true
  bd2021$date_vpob0                                         # Value if condition is false
)


bd2021$date_vpob0<-as.Date(bd2021$date_vpob0)
bd2021$date_vpob0T<-bd2021$date_vpob0

#CALCULATE Number of days at which the child received VPOB0 vaccine
bd2021$age_vpob0 <- as.numeric(difftime(bd2021$date_vpob0, bd2021$birthdate, units = "days"))
bd2021$age_vpob0_w <- as.numeric(difftime(bd2021$date_vpob0, bd2021$birthdate, units = "weeks"))

###########################################################################

#N°2 For VPOB0_2020

##########Timeliness_vpob0##################################################

# class(bd2020$date_vpob0)
# bd2020$date_vpob0 <- parse_date_time(bd2020$date_vpob0, orders = c("d b y", "Y-m-d"))
# bd2020$date_vpob0<-as.Date(bd2020$date_vpob0)
# table(bd2020$date_vpob0)
# 
# # Extract the date range and replace out-of-range dates with NA
# start_date_2020 <- as.Date("2018-10-10")
# end_date_2020 <- as.Date("2020-10-10")
# 
# bd2020 <- bd2020 %>%
#   mutate(date_vpob0 = ifelse(date_vpob0 >= start_date_2020 & date_vpob0 <= end_date_2020, date_vpob0, as.Date(NA)))
# 
# bd2020$date_vpob0<-as.Date(bd2020$date_vpob0)
# 
# #Check the range of dates
# table(bd2020$date_vpob0)
# table(bd2020$birthdate)

#for age_vpob0 (age at which child received VPOB0) is less to 0
# CONDITION 1. Transform 'date_vpob0' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
# bd2020$age_vpob0 <- as.numeric(difftime(bd2020$date_vpob0, bd2020$birthdate, units = "days"))
# bd2020$date_vpob0 <- ifelse(bd2020$date_vpob0 < bd2020$birthdate & bd2020$age_vpob0 < -365, NA, bd2020$date_vpob0)
# bd2020$date_vpob0<-as.Date(bd2020$date_vpob0)
# 
# # condition 2. Transform 'date_vpob0' if conditions are met
# # condition 2. Transform 'date_vpob0' if conditions are met
# bd2020$date_vpob0 <- ifelse(
#   bd2020$date_vpob0 < bd2020$birthdate & bd2020$age_vpob0 >= -365,  # Condition to check
#   bd2020$date_vpob0 + lubridate::years(1),                  # Value if condition is true
#   bd2020$date_vpob0                                         # Value if condition is false
# )
# 
# bd2020$date_vpob0<-as.Date(bd2020$date_vpob0)
# bd2020$date_vpob0T<-bd2020$date_vpob0
# 
# #CALCULATE Number of days at which the child received VPOB0 vaccine
# bd2020$age_vpob0 <- as.numeric(difftime(bd2020$date_vpob0, bd2020$birthdate, units = "days"))
# bd2020$age_vpob0_w <- as.numeric(difftime(bd2020$date_vpob0, bd2020$birthdate, units = "weeks"))

###########################################################################

#N°3  For DTC_HEPB_HIB1_2023

##########Timeliness_dtc_hepb_hib1##################################################

class(bd2023$date_dtc_hepb_hib1)
bd2023$date_dtc_hepb_hib1 <- parse_date_time(bd2023$date_dtc_hepb_hib1, orders = c("d b y", "Y-m-d"))
bd2023$date_dtc_hepb_hib1<-as.Date(bd2023$date_dtc_hepb_hib1)
table(bd2023$date_dtc_hepb_hib1)

# Extract the date range and replace out-of-range dates with NA

bd2023 <- bd2023 %>%
  mutate(date_dtc_hepb_hib1 = ifelse(date_dtc_hepb_hib1 >= start_date_2023 & date_dtc_hepb_hib1 <= end_date_2023, date_dtc_hepb_hib1, as.Date(NA)))

bd2023$date_dtc_hepb_hib1<-as.Date(bd2023$date_dtc_hepb_hib1)

#Check the range of dates
table(bd2023$date_dtc_hepb_hib1)
table(bd2023$birthdate)

#for age_dtc_hepb_hib1 (age at which child received DTC_HEPB_HIB1) is less to 0
# CONDITION 1. Transform 'date_dtc_hepb_hib1' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2023$age_dtc_hepb_hib1 <- as.numeric(difftime(bd2023$date_dtc_hepb_hib1, bd2023$birthdate, units = "days"))
bd2023$date_dtc_hepb_hib1 <- ifelse(bd2023$date_dtc_hepb_hib1 < bd2023$birthdate & bd2023$age_dtc_hepb_hib1 < -365, NA, bd2023$date_dtc_hepb_hib1)
bd2023$date_dtc_hepb_hib1<-as.Date(bd2023$date_dtc_hepb_hib1)

# condition 2. Transform 'date_dtc_hepb_hib1' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2023$date_dtc_hepb_hib1 <- ifelse(
  bd2023$date_dtc_hepb_hib1 < bd2023$birthdate & bd2023$age_dtc_hepb_hib1 >= -365,  # Condition to check
  bd2023$date_dtc_hepb_hib1 + lubridate::years(1),                  # Value if condition is true
  bd2023$date_dtc_hepb_hib1                                         # Value if condition is false
)

bd2023$date_dtc_hepb_hib1<-as.Date(bd2023$date_dtc_hepb_hib1)
bd2023$date_dtc_hepb_hib1T<-bd2023$date_dtc_hepb_hib1

#CALCULATE Number of days at which the child received PENTA1 vaccine
bd2023$age_dtc_hepb_hib1 <- as.numeric(difftime(bd2023$date_dtc_hepb_hib1, bd2023$birthdate, units = "days"))
bd2023$age_dtc_hepb_hib1_w <- as.numeric(difftime(bd2023$date_dtc_hepb_hib1, bd2023$birthdate, units = "weeks"))

###########################################################################

#N°3 For DTC_HEPB_HIB1_2022

##########Timeliness_dtc_hepb_hib1##################################################

class(bd2022$date_dtc_hepb_hib1)
bd2022$date_dtc_hepb_hib1 <- parse_date_time(bd2022$date_dtc_hepb_hib1, orders = c("d b y", "Y-m-d"))
bd2022$date_dtc_hepb_hib1<-as.Date(bd2022$date_dtc_hepb_hib1)
table(bd2022$date_dtc_hepb_hib1)

# Extract the date range and replace out-of-range dates with NA

bd2022 <- bd2022 %>%
  mutate(date_dtc_hepb_hib1 = ifelse(date_dtc_hepb_hib1 >= start_date_2022 & date_dtc_hepb_hib1 <= end_date_2022, date_dtc_hepb_hib1, as.Date(NA)))

bd2022$date_dtc_hepb_hib1<-as.Date(bd2022$date_dtc_hepb_hib1)

#Check the range of dates
table(bd2022$date_dtc_hepb_hib1)
table(bd2022$birthdate)

#for age_dtc_hepb_hib1 (age at which child received DTC_HEPB_HIB1) is less to 0
# CONDITION 1. Transform 'date_dtc_hepb_hib1' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2022$age_dtc_hepb_hib1 <- as.numeric(difftime(bd2022$date_dtc_hepb_hib1, bd2022$birthdate, units = "days"))
bd2022$date_dtc_hepb_hib1 <- ifelse(bd2022$date_dtc_hepb_hib1 < bd2022$birthdate & bd2022$age_dtc_hepb_hib1 < -365, NA, bd2022$date_dtc_hepb_hib1)
bd2022$date_dtc_hepb_hib1<-as.Date(bd2022$date_dtc_hepb_hib1)

# condition 2. Transform 'date_dtc_hepb_hib1' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2022$date_dtc_hepb_hib1 <- ifelse(
  bd2022$date_dtc_hepb_hib1 < bd2022$birthdate & bd2022$age_dtc_hepb_hib1 >= -365,  # Condition to check
  bd2022$date_dtc_hepb_hib1 + lubridate::years(1),                  # Value if condition is true
  bd2022$date_dtc_hepb_hib1                                         # Value if condition is false
)


bd2022$date_dtc_hepb_hib1<-as.Date(bd2022$date_dtc_hepb_hib1)
bd2022$date_dtc_hepb_hib1T<-bd2022$date_dtc_hepb_hib1

#CALCULATE Number of days at which the child received BGC vaccine
bd2022$age_dtc_hepb_hib1 <- as.numeric(difftime(bd2022$date_dtc_hepb_hib1, bd2022$birthdate, units = "days"))
bd2022$age_dtc_hepb_hib1_w <- as.numeric(difftime(bd2022$date_dtc_hepb_hib1, bd2022$birthdate, units = "weeks"))

###########################################################################

#N°3 For DTC_HEPB_HIB1_2021

##########Timeliness_dtc_hepb_hib1##################################################

class(bd2021$date_dtc_hepb_hib1)
bd2021$date_dtc_hepb_hib1 <- parse_date_time(bd2021$date_dtc_hepb_hib1, orders = c("d b y", "Y-m-d"))
bd2021$date_dtc_hepb_hib1<-as.Date(bd2021$date_dtc_hepb_hib1)
table(bd2021$date_dtc_hepb_hib1)

# Extract the date range and replace out-of-range dates with NA

bd2021 <- bd2021 %>%
  mutate(date_dtc_hepb_hib1 = ifelse(date_dtc_hepb_hib1 >= start_date_2021 & date_dtc_hepb_hib1 <= end_date_2021, date_dtc_hepb_hib1, as.Date(NA)))

bd2021$date_dtc_hepb_hib1<-as.Date(bd2021$date_dtc_hepb_hib1)

#Check the range of dates
table(bd2021$date_dtc_hepb_hib1)
table(bd2021$birthdate)

#for age_dtc_hepb_hib1 (age at which child received DTC_HEPB_HIB1) is less to 0
# CONDITION 1. Transform 'date_dtc_hepb_hib1' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2021$age_dtc_hepb_hib1 <- as.numeric(difftime(bd2021$date_dtc_hepb_hib1, bd2021$birthdate, units = "days"))
bd2021$date_dtc_hepb_hib1 <- ifelse(bd2021$date_dtc_hepb_hib1 < bd2021$birthdate & bd2021$age_dtc_hepb_hib1 < -365, NA, bd2021$date_dtc_hepb_hib1)
bd2021$date_dtc_hepb_hib1<-as.Date(bd2021$date_dtc_hepb_hib1)

# condition 2. Transform 'date_dtc_hepb_hib1' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2021$date_dtc_hepb_hib1 <- ifelse(
  bd2021$date_dtc_hepb_hib1 < bd2021$birthdate & bd2021$age_dtc_hepb_hib1 >= -365,  # Condition to check
  bd2021$date_dtc_hepb_hib1 + lubridate::years(1),                  # Value if condition is true
  bd2021$date_dtc_hepb_hib1                                         # Value if condition is false
)


bd2021$date_dtc_hepb_hib1<-as.Date(bd2021$date_dtc_hepb_hib1)
bd2021$date_dtc_hepb_hib1T<-bd2021$date_dtc_hepb_hib1

#CALCULATE Number of days at which the child received PENTA1 vaccine
bd2021$age_dtc_hepb_hib1 <- as.numeric(difftime(bd2021$date_dtc_hepb_hib1, bd2021$birthdate, units = "days"))
bd2021$age_dtc_hepb_hib1_w <- as.numeric(difftime(bd2021$date_dtc_hepb_hib1, bd2021$birthdate, units = "weeks"))

###########################################################################

#N°3 For DTC_HEPB_HIB1_2020

# ##########Timeliness_dtc_hepb_hib1##################################################
# 
# class(bd2020$date_dtc_hepb_hib1)
# bd2020$date_dtc_hepb_hib1 <- parse_date_time(bd2020$date_dtc_hepb_hib1, orders = c("d b y", "Y-m-d"))
# bd2020$date_dtc_hepb_hib1<-as.Date(bd2020$date_dtc_hepb_hib1)
# table(bd2020$date_dtc_hepb_hib1)
# 
# # Extract the date range and replace out-of-range dates with NA
# 
# bd2020 <- bd2020 %>%
#   mutate(date_dtc_hepb_hib1 = ifelse(date_dtc_hepb_hib1 >= start_date_2020 & date_dtc_hepb_hib1 <= end_date_2020, date_dtc_hepb_hib1, as.Date(NA)))
# 
# bd2020$date_dtc_hepb_hib1<-as.Date(bd2020$date_dtc_hepb_hib1)
# 
# #Check the range of dates
# table(bd2020$date_dtc_hepb_hib1)
# table(bd2020$birthdate)

#for age_dtc_hepb_hib1 (age at which child received DTC_HEPB_HIB1) is less to 0
# CONDITION 1. Transform 'date_dtc_hepb_hib1' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
# bd2020$age_dtc_hepb_hib1 <- as.numeric(difftime(bd2020$date_dtc_hepb_hib1, bd2020$birthdate, units = "days"))
# bd2020$date_dtc_hepb_hib1 <- ifelse(bd2020$date_dtc_hepb_hib1 < bd2020$birthdate & bd2020$age_dtc_hepb_hib1 < -365, NA, bd2020$date_dtc_hepb_hib1)
# bd2020$date_dtc_hepb_hib1<-as.Date(bd2020$date_dtc_hepb_hib1)
# 
# # condition 2. Transform 'date_dtc_hepb_hib1' if conditions are met
# # condition 2. Transform 'date_vpob0' if conditions are met
# bd2020$date_dtc_hepb_hib1 <- ifelse(
#   bd2020$date_dtc_hepb_hib1 < bd2020$birthdate & bd2020$age_dtc_hepb_hib1 >= -365,  # Condition to check
#   bd2020$date_dtc_hepb_hib1 + lubridate::years(1),                  # Value if condition is true
#   bd2020$date_dtc_hepb_hib1                                         # Value if condition is false
# )
# 
# bd2020$date_dtc_hepb_hib1<-as.Date(bd2020$date_dtc_hepb_hib1)
# bd2020$date_dtc_hepb_hib1T<-bd2020$date_dtc_hepb_hib1
# 
# #CALCULATE Number of days at which the child received PENTA1 vaccine
# bd2020$age_dtc_hepb_hib1 <- as.numeric(difftime(bd2020$date_dtc_hepb_hib1, bd2020$birthdate, units = "days"))
# bd2020$age_dtc_hepb_hib1_w <- as.numeric(difftime(bd2020$date_dtc_hepb_hib1, bd2020$birthdate, units = "weeks"))
# 
###########################################################################

#N°4  For DTC_HEPB_HIB3_2023

##########Timeliness_dtc_hepb_hib3##################################################

class(bd2023$date_dtc_hepb_hib3)
bd2023$date_dtc_hepb_hib3 <- parse_date_time(bd2023$date_dtc_hepb_hib3, orders = c("d b y", "Y-m-d"))
bd2023$date_dtc_hepb_hib3<-as.Date(bd2023$date_dtc_hepb_hib3)
table(bd2023$date_dtc_hepb_hib3)

# Extract the date range and replace out-of-range dates with NA
start_date_2023 <- as.Date("2022-04-01")
end_date_2023 <- as.Date("2024-04-01")

bd2023 <- bd2023 %>%
  mutate(date_dtc_hepb_hib3 = ifelse(date_dtc_hepb_hib3 >= start_date_2023 & date_dtc_hepb_hib3 <= end_date_2023, date_dtc_hepb_hib3, as.Date(NA)))

bd2023$date_dtc_hepb_hib3<-as.Date(bd2023$date_dtc_hepb_hib3)

#Check the range of dates
table(bd2023$date_dtc_hepb_hib3)
table(bd2023$birthdate)

#for age_dtc_hepb_hib3 (age at which child received DTC_HEPB_HIB3) is less to 0
# CONDITION 1. Transform 'date_dtc_hepb_hib3' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2023$age_dtc_hepb_hib3 <- as.numeric(difftime(bd2023$date_dtc_hepb_hib3, bd2023$birthdate, units = "days"))
bd2023$date_dtc_hepb_hib3 <- ifelse(bd2023$date_dtc_hepb_hib3 < bd2023$birthdate & bd2023$age_dtc_hepb_hib3 < -365, NA, bd2023$date_dtc_hepb_hib3)
bd2023$date_dtc_hepb_hib3<-as.Date(bd2023$date_dtc_hepb_hib3)

# condition 2. Transform 'date_dtc_hepb_hib3' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2023$date_dtc_hepb_hib3 <- ifelse(
  bd2023$date_dtc_hepb_hib3 < bd2023$birthdate & bd2023$age_dtc_hepb_hib3 >= -365,  # Condition to check
  bd2023$date_dtc_hepb_hib3 + lubridate::years(1),                  # Value if condition is true
  bd2023$date_dtc_hepb_hib3                                         # Value if condition is false
)

bd2023$date_dtc_hepb_hib3<-as.Date(bd2023$date_dtc_hepb_hib3)
bd2023$date_dtc_hepb_hib3T<-bd2023$date_dtc_hepb_hib3

#CALCULATE Number of days at which the child received PENTA3 vaccine
bd2023$age_dtc_hepb_hib3 <- as.numeric(difftime(bd2023$date_dtc_hepb_hib3, bd2023$birthdate, units = "days"))
bd2023$age_dtc_hepb_hib3_w <- as.numeric(difftime(bd2023$date_dtc_hepb_hib3, bd2023$birthdate, units = "weeks"))

###########################################################################

#N°4 For DTC_HEPB_HIB3_2022

##########Timeliness_dtc_hepb_hib3##################################################

class(bd2022$date_dtc_hepb_hib3)
bd2022$date_dtc_hepb_hib3 <- parse_date_time(bd2022$date_dtc_hepb_hib3, orders = c("d b y", "Y-m-d"))
bd2022$date_dtc_hepb_hib3<-as.Date(bd2022$date_dtc_hepb_hib3)
table(bd2022$date_dtc_hepb_hib3)

# Extract the date range and replace out-of-range dates with NA
table(bd2022$q117)
start_date_2022 <- as.Date("2021-02-06")
end_date_2022 <- as.Date("2023-02-06")

bd2022 <- bd2022 %>%
  mutate(date_dtc_hepb_hib3 = ifelse(date_dtc_hepb_hib3 >= start_date_2022 & date_dtc_hepb_hib3 <= end_date_2022, date_dtc_hepb_hib3, as.Date(NA)))

bd2022$date_dtc_hepb_hib3<-as.Date(bd2022$date_dtc_hepb_hib3)

#Check the range of dates
table(bd2022$date_dtc_hepb_hib3)
table(bd2022$birthdate)

#for age_dtc_hepb_hib3 (age at which child received DTC_HEPB_HIB3) is less to 0
# CONDITION 1. Transform 'date_dtc_hepb_hib3' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2022$age_dtc_hepb_hib3 <- as.numeric(difftime(bd2022$date_dtc_hepb_hib3, bd2022$birthdate, units = "days"))
bd2022$date_dtc_hepb_hib3 <- ifelse(bd2022$date_dtc_hepb_hib3 < bd2022$birthdate & bd2022$age_dtc_hepb_hib3 < -365, NA, bd2022$date_dtc_hepb_hib3)
bd2022$date_dtc_hepb_hib3<-as.Date(bd2022$date_dtc_hepb_hib3)

# condition 2. Transform 'date_dtc_hepb_hib3' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2022$date_dtc_hepb_hib3 <- ifelse(
  bd2022$date_dtc_hepb_hib3 < bd2022$birthdate & bd2022$age_dtc_hepb_hib3 >= -365,  # Condition to check
  bd2022$date_dtc_hepb_hib3 + lubridate::years(1),                  # Value if condition is true
  bd2022$date_dtc_hepb_hib3                                         # Value if condition is false
)


bd2022$date_dtc_hepb_hib3<-as.Date(bd2022$date_dtc_hepb_hib3)
bd2022$date_dtc_hepb_hib3T<-bd2022$date_dtc_hepb_hib3

#CALCULATE Number of days at which the child received PENTA3 vaccine
bd2022$age_dtc_hepb_hib3 <- as.numeric(difftime(bd2022$date_dtc_hepb_hib3, bd2022$birthdate, units = "days"))
bd2022$age_dtc_hepb_hib3_w <- as.numeric(difftime(bd2022$date_dtc_hepb_hib3, bd2022$birthdate, units = "weeks"))

###########################################################################

#N°4 For DTC_HEPB_HIB3_2021

##########Timeliness_dtc_hepb_hib3##################################################

class(bd2021$date_dtc_hepb_hib3)
bd2021$date_dtc_hepb_hib3 <- parse_date_time(bd2021$date_dtc_hepb_hib3, orders = c("d b y", "Y-m-d"))
bd2021$date_dtc_hepb_hib3<-as.Date(bd2021$date_dtc_hepb_hib3)
table(bd2021$date_dtc_hepb_hib3)

# Extract the date range and replace out-of-range dates with NA
start_date_2021 <- as.Date("2019-09-06")
end_date_2021 <- as.Date("2021-09-06")

bd2021 <- bd2021 %>%
  mutate(date_dtc_hepb_hib3 = ifelse(date_dtc_hepb_hib3 >= start_date_2021 & date_dtc_hepb_hib3 <= end_date_2021, date_dtc_hepb_hib3, as.Date(NA)))

bd2021$date_dtc_hepb_hib3<-as.Date(bd2021$date_dtc_hepb_hib3)

#Check the range of dates
table(bd2021$date_dtc_hepb_hib3)
table(bd2021$birthdate)

#for age_dtc_hepb_hib3 (age at which child received DTC_HEPB_HIB3) is less to 0
# CONDITION 1. Transform 'date_dtc_hepb_hib3' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2021$age_dtc_hepb_hib3 <- as.numeric(difftime(bd2021$date_dtc_hepb_hib3, bd2021$birthdate, units = "days"))
bd2021$date_dtc_hepb_hib3 <- ifelse(bd2021$date_dtc_hepb_hib3 < bd2021$birthdate & bd2021$age_dtc_hepb_hib3 < -365, NA, bd2021$date_dtc_hepb_hib3)
bd2021$date_dtc_hepb_hib3<-as.Date(bd2021$date_dtc_hepb_hib3)

# condition 2. Transform 'date_dtc_hepb_hib3' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2021$date_dtc_hepb_hib3 <- ifelse(
  bd2021$date_dtc_hepb_hib3 < bd2021$birthdate & bd2021$age_dtc_hepb_hib3 >= -365,  # Condition to check
  bd2021$date_dtc_hepb_hib3 + lubridate::years(1),                  # Value if condition is true
  bd2021$date_dtc_hepb_hib3                                         # Value if condition is false
)


bd2021$date_dtc_hepb_hib3<-as.Date(bd2021$date_dtc_hepb_hib3)
bd2021$date_dtc_hepb_hib3T<-bd2021$date_dtc_hepb_hib3

#CALCULATE Number of days at which the child received PENTA3 vaccine
bd2021$age_dtc_hepb_hib3 <- as.numeric(difftime(bd2021$date_dtc_hepb_hib3, bd2021$birthdate, units = "days"))
bd2021$age_dtc_hepb_hib3_w <- as.numeric(difftime(bd2021$date_dtc_hepb_hib3, bd2021$birthdate, units = "weeks"))

###########################################################################

#N°4 For DTC_HEPB_HIB3_2020

##########Timeliness_dtc_hepb_hib3##################################################

# class(bd2020$date_dtc_hepb_hib3)
# bd2020$date_dtc_hepb_hib3 <- parse_date_time(bd2020$date_dtc_hepb_hib3, orders = c("d b y", "Y-m-d"))
# bd2020$date_dtc_hepb_hib3<-as.Date(bd2020$date_dtc_hepb_hib3)
# table(bd2020$date_dtc_hepb_hib3)
# 
# # Extract the date range and replace out-of-range dates with NA
# start_date_2020 <- as.Date("2018-10-10")
# end_date_2020 <- as.Date("2020-10-10")
# 
# bd2020 <- bd2020 %>%
#   mutate(date_dtc_hepb_hib3 = ifelse(date_dtc_hepb_hib3 >= start_date_2020 & date_dtc_hepb_hib3 <= end_date_2020, date_dtc_hepb_hib3, as.Date(NA)))
# 
# bd2020$date_dtc_hepb_hib3<-as.Date(bd2020$date_dtc_hepb_hib3)
# 
# #Check the range of dates
# table(bd2020$date_dtc_hepb_hib3)
# table(bd2020$birthdate)
# 
# #for age_dtc_hepb_hib3 (age at which child received DTC_HEPB_HIB3) is less to 0
# # CONDITION 1. Transform 'date_dtc_hepb_hib3' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
# bd2020$age_dtc_hepb_hib3 <- as.numeric(difftime(bd2020$date_dtc_hepb_hib3, bd2020$birthdate, units = "days"))
# bd2020$date_dtc_hepb_hib3 <- ifelse(bd2020$date_dtc_hepb_hib3 < bd2020$birthdate & bd2020$age_dtc_hepb_hib3 < -365, NA, bd2020$date_dtc_hepb_hib3)
# bd2020$date_dtc_hepb_hib3<-as.Date(bd2020$date_dtc_hepb_hib3)

# condition 2. Transform 'date_dtc_hepb_hib3' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
# bd2020$date_dtc_hepb_hib3 <- ifelse(
#   bd2020$date_dtc_hepb_hib3 < bd2020$birthdate & bd2020$age_dtc_hepb_hib3 >= -365,  # Condition to check
#   bd2020$date_dtc_hepb_hib3 + lubridate::years(1),                  # Value if condition is true
#   bd2020$date_dtc_hepb_hib3                                         # Value if condition is false
# )
# 
# bd2020$date_dtc_hepb_hib3<-as.Date(bd2020$date_dtc_hepb_hib3)
# bd2020$date_dtc_hepb_hib3T<-bd2020$date_dtc_hepb_hib3
# 
# #CALCULATE Number of days at which the child received PENTA3 vaccine
# bd2020$age_dtc_hepb_hib3 <- as.numeric(difftime(bd2020$date_dtc_hepb_hib3, bd2020$birthdate, units = "days"))
# bd2020$age_dtc_hepb_hib3_w <- as.numeric(difftime(bd2020$date_dtc_hepb_hib3, bd2020$birthdate, units = "weeks"))
# 
###########################################################################

#N°5  For VAR_2023

##########Timeliness_var##################################################

class(bd2023$date_var)
bd2023$date_var <- parse_date_time(bd2023$date_var, orders = c("d b y", "Y-m-d"))
bd2023$date_var<-as.Date(bd2023$date_var)
table(bd2023$date_var)

# Extract the date range and replace out-of-range dates with NA
start_date_2023 <- as.Date("2022-04-01")
end_date_2023 <- as.Date("2024-04-01")

bd2023 <- bd2023 %>%
  mutate(date_var = ifelse(date_var >= start_date_2023 & date_var <= end_date_2023, date_var, as.Date(NA)))

bd2023$date_var<-as.Date(bd2023$date_var)

#Check the range of dates
table(bd2023$date_var)
table(bd2023$birthdate)

#for age_var (age at which child received VAR) is less to 0
# CONDITION 1. Transform 'date_var' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2023$age_var <- as.numeric(difftime(bd2023$date_var, bd2023$birthdate, units = "days"))
bd2023$date_var <- ifelse(bd2023$date_var < bd2023$birthdate & bd2023$age_var < -365, NA, bd2023$date_var)
bd2023$date_var<-as.Date(bd2023$date_var)

# condition 2. Transform 'date_var' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2023$date_var <- ifelse(
  bd2023$date_var < bd2023$birthdate & bd2023$age_var >= -365,  # Condition to check
  bd2023$date_var + lubridate::years(1),                  # Value if condition is true
  bd2023$date_var                                         # Value if condition is false
)

bd2023$date_var<-as.Date(bd2023$date_var)
bd2023$date_varT<-bd2023$date_var

#CALCULATE Number of days at which the child received VAR vaccine
bd2023$age_var <- as.numeric(difftime(bd2023$date_var, bd2023$birthdate, units = "days"))
bd2023$age_var_w <- as.numeric(difftime(bd2023$date_var, bd2023$birthdate, units = "weeks"))
bd2023$age_var_m<- bd2023$age_var/30.4375
bd2023$age_var_m <- ifelse(bd2023$age_var_m < 9, NA, bd2023$age_var_m)

###########################################################################

#N°5 For VAR_2022

##########Timeliness_var##################################################

class(bd2022$date_var)
bd2022$date_var <- parse_date_time(bd2022$date_var, orders = c("d b y", "Y-m-d"))
bd2022$date_var<-as.Date(bd2022$date_var)
table(bd2022$date_var)

# Extract the date range and replace out-of-range dates with NA
table(bd2022$q117)
start_date_2022 <- as.Date("2021-02-06")
end_date_2022 <- as.Date("2023-02-06")

bd2022 <- bd2022 %>%
  mutate(date_var = ifelse(date_var >= start_date_2022 & date_var <= end_date_2022, date_var, as.Date(NA)))

bd2022$date_var<-as.Date(bd2022$date_var)

#Check the range of dates
table(bd2022$date_var)
table(bd2022$birthdate)

#for age_var (age at which child received VAR) is less to 0
# CONDITION 1. Transform 'date_var' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2022$age_var <- as.numeric(difftime(bd2022$date_var, bd2022$birthdate, units = "days"))
bd2022$date_var <- ifelse(bd2022$date_var < bd2022$birthdate & bd2022$age_var < -365, NA, bd2022$date_var)
bd2022$date_var<-as.Date(bd2022$date_var)

# condition 2. Transform 'date_var' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2022$date_var <- ifelse(
  bd2022$date_var < bd2022$birthdate & bd2022$age_var >= -365,  # Condition to check
  bd2022$date_var + lubridate::years(1),                  # Value if condition is true
  bd2022$date_var                                         # Value if condition is false
)
bd2022$date_var<-as.Date(bd2022$date_var)
bd2022$date_varT<-bd2022$date_var

#CALCULATE Number of days at which the child received VAR  vaccine
bd2022$age_var <- as.numeric(difftime(bd2022$date_var, bd2022$birthdate, units = "days"))
bd2022$age_var_w <- as.numeric(difftime(bd2022$date_var, bd2022$birthdate, units = "weeks"))
bd2022$age_var_m<- bd2022$age_var/30.4375
bd2022$age_var_m <- ifelse(bd2022$age_var_m < 9, NA, bd2022$age_var_m)

###########################################################################

#N°5 For VAR_2021

##########Timeliness_var##################################################

class(bd2021$date_var)
bd2021$date_var <- parse_date_time(bd2021$date_var, orders = c("d b y", "Y-m-d"))
bd2021$date_var<-as.Date(bd2021$date_var)
table(bd2021$date_var)

# Extract the date range and replace out-of-range dates with NA
start_date_2021 <- as.Date("2019-09-06")
end_date_2021 <- as.Date("2021-09-06")

bd2021 <- bd2021 %>%
  mutate(date_var = ifelse(date_var >= start_date_2021 & date_var <= end_date_2021, date_var, as.Date(NA)))

bd2021$date_var<-as.Date(bd2021$date_var)

#Check the range of dates
table(bd2021$date_var)
table(bd2021$birthdate)

#for age_var (age at which child received VAR) is less to 0
# CONDITION 1. Transform 'date_var' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2021$age_var <- as.numeric(difftime(bd2021$date_var, bd2021$birthdate, units = "days"))
bd2021$date_var <- ifelse(bd2021$date_var < bd2021$birthdate & bd2021$age_var < -365, NA, bd2021$date_var)
bd2021$date_var<-as.Date(bd2021$date_var)

# condition 2. Transform 'date_var' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2021$date_var <- ifelse(
  bd2021$date_var < bd2021$birthdate & bd2021$age_var >= -365,  # Condition to check
  bd2021$date_var + lubridate::years(1),                  # Value if condition is true
  bd2021$date_var                                         # Value if condition is false
)
bd2021$date_var<-as.Date(bd2021$date_var)
bd2021$date_varT<-bd2021$date_var

#CALCULATE Number of days at which the child received VAR  vaccine
bd2021$age_var <- as.numeric(difftime(bd2021$date_var, bd2021$birthdate, units = "days"))
bd2021$age_var_w <- as.numeric(difftime(bd2021$date_var, bd2021$birthdate, units = "weeks"))
bd2021$age_var_m<- bd2021$age_var/30.4375
bd2021$age_var_m <- ifelse(bd2021$age_var_m < 9, NA, bd2021$age_var_m)

###########################################################################

#N°5 For VAR_2020

##########Timeliness_var##################################################

# class(bd2020$date_var)
# bd2020$date_var <- parse_date_time(bd2020$date_var, orders = c("d b y", "Y-m-d"))
# bd2020$date_var<-as.Date(bd2020$date_var)
# table(bd2020$date_var)
# 
# # Extract the date range and replace out-of-range dates with NA
# start_date_2020 <- as.Date("2018-10-10")
# end_date_2020 <- as.Date("2020-10-10")
# 
# bd2020 <- bd2020 %>%
#   mutate(date_var = ifelse(date_var >= start_date_2020 & date_var <= end_date_2020, date_var, as.Date(NA)))
# 
# bd2020$date_var<-as.Date(bd2020$date_var)
# 
# #Check the range of dates
# table(bd2020$date_var)
# table(bd2020$birthdate)
# 
# #for age_var (age at which child received VAR) is less to 0
# # CONDITION 1. Transform 'date_var' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
# bd2020$age_var <- as.numeric(difftime(bd2020$date_var, bd2020$birthdate, units = "days"))
# bd2020$date_var <- ifelse(bd2020$date_var < bd2020$birthdate & bd2020$age_var < -365, NA, bd2020$date_var)
# bd2020$date_var<-as.Date(bd2020$date_var)

# condition 2. Transform 'date_var' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
# bd2020$date_var <- ifelse(
#   bd2020$date_var < bd2020$birthdate & bd2020$age_var >= -365,  # Condition to check
#   bd2020$date_var + lubridate::years(1),                  # Value if condition is true
#   bd2020$date_var                                         # Value if condition is false
# )
# bd2020$date_var<-as.Date(bd2020$date_var)
# bd2020$date_varT<-bd2020$date_var
# 
# #CALCULATE Number of days at which the child received VAR vaccine
# bd2020$age_var <- as.numeric(difftime(bd2020$date_var, bd2020$birthdate, units = "days"))
# bd2020$age_var_w <- as.numeric(difftime(bd2020$date_var, bd2020$birthdate, units = "weeks"))
# bd2020$age_var_m<- bd2020$age_var/30.4375
# bd2020$age_var_m <- ifelse(bd2020$age_var_m < 9, NA, bd2020$age_var_m)
# 
# ###########################################################################

#N°6  For VAA_2023

##########Timeliness_vaa##################################################

class(bd2023$date_vaa)
bd2023$date_vaa <- parse_date_time(bd2023$date_vaa, orders = c("d b y", "Y-m-d"))
bd2023$date_vaa<-as.Date(bd2023$date_vaa)
table(bd2023$date_vaa)

# Extract the date range and replace out-of-range dates with NA
start_date_2023 <- as.Date("2022-04-01")
end_date_2023 <- as.Date("2024-04-01")

bd2023 <- bd2023 %>%
  mutate(date_vaa = ifelse(date_vaa >= start_date_2023 & date_vaa <= end_date_2023, date_vaa, as.Date(NA)))

bd2023$date_vaa<-as.Date(bd2023$date_vaa)

#Check the range of dates
table(bd2023$date_vaa)
table(bd2023$birthdate)

#for age_vaa (age at which child received VAA) is less to 0
# CONDITION 1. Transform 'date_vaa' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2023$age_vaa <- as.numeric(difftime(bd2023$date_vaa, bd2023$birthdate, units = "days"))
bd2023$date_vaa <- ifelse(bd2023$date_vaa < bd2023$birthdate & bd2023$age_vaa < -365, NA, bd2023$date_vaa)
bd2023$date_vaa<-as.Date(bd2023$date_vaa)

# condition 2. Transform 'date_vaa' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2023$date_vaa <- ifelse(
  bd2023$date_vaa < bd2023$birthdate & bd2023$age_vaa >= -365,  # Condition to check
  bd2023$date_vaa + lubridate::years(1),                  # Value if condition is true
  bd2023$date_vaa                                         # Value if condition is false
)

bd2023$date_vaa<-as.Date(bd2023$date_vaa)
bd2023$date_vaaT<-bd2023$date_vaa

#CALCULATE Number of days at which the child received VAA vaccine
bd2023$age_vaa <- as.numeric(difftime(bd2023$date_vaa, bd2023$birthdate, units = "days"))
bd2023$age_vaa_w <- as.numeric(difftime(bd2023$date_vaa, bd2023$birthdate, units = "weeks"))
bd2023$age_vaa_m<- bd2023$age_vaa/30.4375
bd2023$age_vaa_m <- ifelse(bd2023$age_vaa_m < 9, NA, bd2023$age_vaa_m)

###########################################################################

#N°6 For VAA_2022

##########Timeliness_vaa##################################################

class(bd2022$date_vaa)
bd2022$date_vaa <- parse_date_time(bd2022$date_vaa, orders = c("d b y", "Y-m-d"))
bd2022$date_vaa<-as.Date(bd2022$date_vaa)
table(bd2022$date_vaa)

# Extract the date range and replace out-of-range dates with NA
table(bd2022$q117)
start_date_2022 <- as.Date("2021-02-06")
end_date_2022 <- as.Date("2023-02-06")

bd2022 <- bd2022 %>%
  mutate(date_vaa = ifelse(date_vaa >= start_date_2022 & date_vaa <= end_date_2022, date_vaa, as.Date(NA)))

bd2022$date_vaa<-as.Date(bd2022$date_vaa)

#Check the range of dates
table(bd2022$date_vaa)
table(bd2022$birthdate)

#for age_vaa (age at which child received VAA) is less to 0
# CONDITION 1. Transform 'date_vaa' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2022$age_vaa <- as.numeric(difftime(bd2022$date_vaa, bd2022$birthdate, units = "days"))
bd2022$date_vaa <- ifelse(bd2022$date_vaa < bd2022$birthdate & bd2022$age_vaa < -365, NA, bd2022$date_vaa)
bd2022$date_vaa<-as.Date(bd2022$date_vaa)

# condition 2. Transform 'date_vaa' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2022$date_vaa <- ifelse(
  bd2022$date_vaa < bd2022$birthdate & bd2022$age_vaa >= -365,  # Condition to check
  bd2022$date_vaa + lubridate::years(1),                  # Value if condition is true
  bd2022$date_vaa                                         # Value if condition is false
)
bd2022$date_vaa<-as.Date(bd2022$date_vaa)
bd2022$date_vaaT<-bd2022$date_vaa

#CALCULATE Number of days at which the child received VAA  vaccine
bd2022$age_vaa <- as.numeric(difftime(bd2022$date_vaa, bd2022$birthdate, units = "days"))
bd2022$age_vaa_w <- as.numeric(difftime(bd2022$date_vaa, bd2022$birthdate, units = "weeks"))
bd2022$age_vaa_m<- bd2022$age_vaa/30.4375
bd2022$age_vaa_m <- ifelse(bd2022$age_vaa_m < 9, NA, bd2022$age_vaa_m)

###########################################################################

#N°6  For VAA_2021

##########Timeliness_vaa##################################################

class(bd2021$date_vaa)
bd2021$date_vaa <- parse_date_time(bd2021$date_vaa, orders = c("d b y", "Y-m-d"))
bd2021$date_vaa<-as.Date(bd2021$date_vaa)
table(bd2021$date_vaa)

# Extract the date range and replace out-of-range dates with NA
start_date_2021 <- as.Date("2019-09-06")
end_date_2021 <- as.Date("2021-09-06")

bd2021 <- bd2021 %>%
  mutate(date_vaa = ifelse(date_vaa >= start_date_2021 & date_vaa <= end_date_2021, date_vaa, as.Date(NA)))

bd2021$date_vaa<-as.Date(bd2021$date_vaa)

#Check the range of dates
table(bd2021$date_vaa)
table(bd2021$birthdate)

#for age_vaa (age at which child received VAA) is less to 0
# CONDITION 1. Transform 'date_vaa' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
bd2021$age_vaa <- as.numeric(difftime(bd2021$date_vaa, bd2021$birthdate, units = "days"))
bd2021$date_vaa <- ifelse(bd2021$date_vaa < bd2021$birthdate & bd2021$age_vaa < -365, NA, bd2021$date_vaa)
bd2021$date_vaa<-as.Date(bd2021$date_vaa)

# condition 2. Transform 'date_vaa' if conditions are met
# condition 2. Transform 'date_vpob0' if conditions are met
bd2021$date_vaa <- ifelse(
  bd2021$date_vaa < bd2021$birthdate & bd2021$age_vaa >= -365,  # Condition to check
  bd2021$date_vaa + lubridate::years(1),                  # Value if condition is true
  bd2021$date_vaa                                         # Value if condition is false
)
bd2021$date_vaa<-as.Date(bd2021$date_vaa)
bd2021$date_vaaT<-bd2021$date_vaa

#CALCULATE Number of days at which the child received VAA  vaccine
bd2021$age_vaa <- as.numeric(difftime(bd2021$date_vaa, bd2021$birthdate, units = "days"))
bd2021$age_vaa_w <- as.numeric(difftime(bd2021$date_vaa, bd2021$birthdate, units = "weeks"))
bd2021$age_vaa_m<- bd2021$age_vaa/30.4375
bd2021$age_vaa_m <- ifelse(bd2021$age_vaa_m < 9, NA, bd2021$age_vaa_m)

###########################################################################

#N°6 For VAA_2020

##########Timeliness_vaa##################################################

# class(bd2020$date_vaa)
# bd2020$date_vaa <- parse_date_time(bd2020$date_vaa, orders = c("d b y", "Y-m-d"))
# bd2020$date_vaa<-as.Date(bd2020$date_vaa)
# table(bd2020$date_vaa)
# 
# # Extract the date range and replace out-of-range dates with NA
# start_date_2020 <- as.Date("2018-10-10")
# end_date_2020 <- as.Date("2020-10-10")
# 
# bd2020 <- bd2020 %>%
#   mutate(date_vaa = ifelse(date_vaa >= start_date_2020 & date_vaa <= end_date_2020, date_vaa, as.Date(NA)))
# 
# bd2020$date_vaa<-as.Date(bd2020$date_vaa)
# 
# #Check the range of dates
# table(bd2020$date_vaa)
# table(bd2020$birthdate)
# 
# #for age_vaa (age at which child received VAA) is less to 0
# # CONDITION 1. Transform 'date_vaa' to NA if it is earlier than 'birthdate' and the difference is more than 365 days
# bd2020$age_vaa <- as.numeric(difftime(bd2020$date_vaa, bd2020$birthdate, units = "days"))
# bd2020$date_vaa <- ifelse(bd2020$date_vaa < bd2020$birthdate & bd2020$age_vaa < -365, NA, bd2020$date_vaa)
# bd2020$date_vaa<-as.Date(bd2020$date_vaa)
# 
# # condition 2. Transform 'date_vaa' if conditions are met
# # condition 2. Transform 'date_vpob0' if conditions are met
# bd2020$date_vaa <- ifelse(
#   bd2020$date_vaa < bd2020$birthdate & bd2020$age_vaa >= -365,  # Condition to check
#   bd2020$date_vaa + lubridate::years(1),                  # Value if condition is true
#   bd2020$date_vaa                                         # Value if condition is false
# )
# bd2020$date_vaa<-as.Date(bd2020$date_vaa)
# bd2020$date_vaaT<-bd2020$date_vaa
# 
# #CALCULATE Number of days at which the child received VAA vaccine
# bd2020$age_vaa <- as.numeric(difftime(bd2020$date_vaa, bd2020$birthdate, units = "days"))
# bd2020$age_vaa_w <- as.numeric(difftime(bd2020$date_vaa, bd2020$birthdate, units = "weeks"))
# bd2020$age_vaa_m<- bd2020$age_vaa/30.4375
# bd2020$age_vaa_m <- ifelse(bd2020$age_vaa_m < 9, NA, bd2020$age_vaa_m)


############################################################################

summary(bd2021$age_bcg)
summary(bd2022$age_bcg)
summary(bd2023$age_bcg)

summary(bd2021$age_vpob0)
summary(bd2022$age_vpob0)
summary(bd2023$age_vpob0)

summary(bd2021$age_dtc_hepb_hib1_w)
summary(bd2022$age_dtc_hepb_hib1_w)
summary(bd2023$age_dtc_hepb_hib1_w)

summary(bd2021$age_dtc_hepb_hib3_w)
summary(bd2022$age_dtc_hepb_hib3_w)
summary(bd2023$age_dtc_hepb_hib3_w)

summary(bd2021$age_var_m)
summary(bd2022$age_var_m)
summary(bd2023$age_var_m)

summary(bd2021$age_vaa_m)
summary(bd2022$age_vaa_m)
summary(bd2023$age_vaa_m)

##########################################################################


#Rename variables

#2021
bd2021$province <-	bd2021$q101
bd2021$healthzone <-	bd2021$q103
bd2021$age_mother <-	bd2021$qa103
bd2021$education_mother <- bd2021$qa104
bd2021$religion	<-bd2021$qa207
bd2021$knowledge_vdp<-bd2021$noteqa501
bd2021$confiance_mother_MoH	<-bd2021$BeSD6
bd2021$vaccins_sur	<-bd2021$BeSD5
bd2021$child_civil_registration	<-bd2021$vs25e
bd2021$complete_vaccination	<-bd2021$vs69
bd2021$paid_vaccination	<-bd2021$vs85m
bd2021$child_sex	<-bd2021$vs20
bd2021$marital_status	<-bd2021$qa101
bd2021$area_residency	<-bd2021$q108
bd2021$using_telephone	<-bd2021$qa212
bd2021$wealth	<-bd2021$NivSocEco


#2022
bd2022$province <-	bd2022$q101
bd2022$healthzone <-	bd2022$q103
bd2022$age_mother <-	bd2022$qa103
bd2022$education_mother <- bd2022$qa104
bd2022$religion	<-bd2022$qa207
bd2022$knowledge_vdp<-bd2022$noteqa501
bd2022$confiance_mother_MoH	<-bd2022$BeSD6
bd2022$vaccins_sur	<-bd2022$BeSD5
bd2022$child_civil_registration	<-bd2022$vs25e
bd2022$complete_vaccination	<-bd2022$vs69
bd2022$paid_vaccination	<-bd2022$vs85m
bd2022$child_sex	<-bd2022$vs20
bd2022$marital_status	<-bd2022$qa101
bd2022$area_residency	<-bd2022$q108
bd2022$using_telephone	<-bd2022$qa212
bd2022$wealth	<-bd2022$SES

#2023
bd2023$province <-	bd2023$q101
bd2023$healthzone <-	bd2023$q103
bd2023$age_mother <-	bd2023$qa103
bd2023$education_mother <- bd2023$qa104
bd2023$religion	<-bd2023$qa207
bd2023$knowledge_vdp<-bd2023$noteqa501
bd2023$confiance_mother_MoH	<-bd2023$BeSD6
bd2023$vaccins_sur	<-bd2023$BeSD5
bd2023$child_civil_registration	<-bd2023$vs25e
bd2023$complete_vaccination	<-bd2023$vs69
bd2023$paid_vaccination	<-bd2023$vs85m
bd2023$child_sex	<-bd2023$vs20
bd2023$marital_status	<-bd2023$qa101
bd2023$area_residency	<-bd2023$q108
bd2023$using_telephone	<-bd2023$qa212
bd2023$wealth	<-bd2023$SES

############################################################################
table(bd2021$wealth)
table(bd2022$wealth)
table(bd2023$wealth)

bd2021$wealth[bd2021$wealth == "Q1"] <- "Lowest quintile"
bd2021$wealth[bd2021$wealth == "Q2"] <- "Lower quintile"
bd2021$wealth[bd2021$wealth == "Q3"] <- "Middle quintile"
bd2021$wealth[bd2021$wealth == "Q4"] <- "Higher quintile"
bd2021$wealth[bd2021$wealth == "Q5"] <- "Highest quintile"

table(bd2021$bcg)
table(bd2022$bcg)
table(bd2023$bcg)

bd2021$bcg[bd2021$bcg == "2"] <- "Non"
bd2022$bcg[bd2022$bcg == "2"] <- "Non"
bd2023$bcg[bd2023$bcg == "2"] <- "Non"

table(bd2021$dtc_hepb_hib1)
table(bd2022$dtc_hepb_hib1)
table(bd2023$dtc_hepb_hib1)

bd2021$dtc_hepb_hib1[bd2021$dtc_hepb_hib1 == "2"] <- "Non"
bd2022$dtc_hepb_hib1[bd2022$dtc_hepb_hib1 == "2"] <- "Non"
bd2023$dtc_hepb_hib1[bd2023$dtc_hepb_hib1 == "2"] <- "Non"
bd2021$dtc_hepb_hib1[bd2021$dtc_hepb_hib1 == "1"] <- "Oui"
bd2022$dtc_hepb_hib1[bd2022$dtc_hepb_hib1 == "1"] <- "Oui"
bd2023$dtc_hepb_hib1[bd2023$dtc_hepb_hib1 == "1"] <- "Oui"

table(bd2021$dtc_hepb_hib3)
table(bd2022$dtc_hepb_hib3)
table(bd2023$dtc_hepb_hib3)

bd2021$dtc_hepb_hib3[bd2021$dtc_hepb_hib3 == "2"] <- "Non"
bd2022$dtc_hepb_hib3[bd2022$dtc_hepb_hib3 == "2"] <- "Non"
bd2023$dtc_hepb_hib3[bd2023$dtc_hepb_hib3 == "2"] <- "Non"
bd2021$dtc_hepb_hib3[bd2021$dtc_hepb_hib3 == "1"] <- "Oui"
bd2022$dtc_hepb_hib3[bd2022$dtc_hepb_hib3 == "1"] <- "Oui"
bd2023$dtc_hepb_hib3[bd2023$dtc_hepb_hib3 == "1"] <- "Oui"

#CHECK MISSING OF DATES FOR EACH VACCINE "BCG","PENTA1","PENTA3","VAR"
sum(is.na(bd2021$date_bcg) & bd2021$bcg == "Oui", na.rm = TRUE)

library(binom)  # for binom.confint

# Define vaccines and associated variables
vaccines <- c("bcg", "dtc_hepb_hib1", "dtc_hepb_hib3", "var","complete_vaccination")
date_vars <- paste0("date_", vaccines)
age_vars <- c("age_bcg", "age_dtc_hepb_hib1_w", "age_dtc_hepb_hib3_w", "age_var_m","age_var_m")

# Define age ranges
age_min <- c(0, 6, 14, 9,9)
age_max <- c(14, 9.999, 17.999, 9.999,9.999)

library(binom)

###2021
Date_na_timely2021 <- do.call(rbind, lapply(seq_along(vaccines), function(i) {
  vax <- vaccines[i]
  date_vax <- date_vars[i]
  age_vax <- age_vars[i]
  
  total <- length(bd2021[[vax]])
  n_oui <- sum(bd2021[[vax]] == "Oui", na.rm = TRUE)
  missing_date_if_oui <- sum(is.na(bd2021[[date_vax]]) & bd2021[[vax]] == "Oui", na.rm = TRUE)
  oui_in_age <- sum(
    bd2021[[vax]] == "Oui" &
      bd2021[[age_vax]] >= age_min[i] &
      bd2021[[age_vax]] <= age_max[i],
    na.rm = TRUE
  )
  
  # CI for n_oui out of total
  ci1 <- binom.confint(n_oui, total, method = "exact")
  
  # CI for oui_in_age out of n_oui
  if (n_oui > 0) {
    ci2 <- binom.confint(oui_in_age, n_oui, method = "exact")
  } else {
    ci2 <- data.frame(mean = NA, lower = NA, upper = NA)
  }
  
  data.frame(
    vaccine = vax,
    total = total,
    n_oui = n_oui,
    pct_oui = round(100 * n_oui / total, 1),
    ci_oui_lower = round(100 * ci1$lower, 1),
    ci_oui_upper = round(100 * ci1$upper, 1),
    missing_date_if_oui = missing_date_if_oui,
    oui_in_target_age = oui_in_age,
    pct_in_age = round(100 * oui_in_age / n_oui, 1),
    ci_age_lower = round(100 * ci2$lower, 1),
    ci_age_upper = round(100 * ci2$upper, 1)
  )
}))

###2022
Date_na_timely2022 <- do.call(rbind, lapply(seq_along(vaccines), function(i) {
  vax <- vaccines[i]
  date_vax <- date_vars[i]
  age_vax <- age_vars[i]
  
  total <- length(bd2022[[vax]])
  n_oui <- sum(bd2022[[vax]] == "Oui", na.rm = TRUE)
  missing_date_if_oui <- sum(is.na(bd2022[[date_vax]]) & bd2022[[vax]] == "Oui", na.rm = TRUE)
  oui_in_age <- sum(
    bd2022[[vax]] == "Oui" &
      bd2022[[age_vax]] >= age_min[i] &
      bd2022[[age_vax]] <= age_max[i],
    na.rm = TRUE
  )
  
  # CI for n_oui out of total
  ci1 <- binom.confint(n_oui, total, method = "exact")
  
  # CI for oui_in_age out of n_oui
  if (n_oui > 0) {
    ci2 <- binom.confint(oui_in_age, n_oui, method = "exact")
  } else {
    ci2 <- data.frame(mean = NA, lower = NA, upper = NA)
  }
  
  data.frame(
    vaccine = vax,
    total = total,
    n_oui = n_oui,
    pct_oui = round(100 * n_oui / total, 1),
    ci_oui_lower = round(100 * ci1$lower, 1),
    ci_oui_upper = round(100 * ci1$upper, 1),
    missing_date_if_oui = missing_date_if_oui,
    oui_in_target_age = oui_in_age,
    pct_in_age = round(100 * oui_in_age / n_oui, 1),
    ci_age_lower = round(100 * ci2$lower, 1),
    ci_age_upper = round(100 * ci2$upper, 1)
  )
}))

###2023
Date_na_timely2023 <- do.call(rbind, lapply(seq_along(vaccines), function(i) {
  vax <- vaccines[i]
  date_vax <- date_vars[i]
  age_vax <- age_vars[i]
  
  total <- length(bd2023[[vax]])
  n_oui <- sum(bd2023[[vax]] == "Oui", na.rm = TRUE)
  missing_date_if_oui <- sum(is.na(bd2023[[date_vax]]) & bd2023[[vax]] == "Oui", na.rm = TRUE)
  oui_in_age <- sum(
    bd2023[[vax]] == "Oui" &
      bd2023[[age_vax]] >= age_min[i] &
      bd2023[[age_vax]] <= age_max[i],
    na.rm = TRUE
  )
  
  # CI for n_oui out of total
  ci1 <- binom.confint(n_oui, total, method = "exact")
  
  # CI for oui_in_age out of n_oui
  if (n_oui > 0) {
    ci2 <- binom.confint(oui_in_age, n_oui, method = "exact")
  } else {
    ci2 <- data.frame(mean = NA, lower = NA, upper = NA)
  }
  
  data.frame(
    vaccine = vax,
    total = total,
    n_oui = n_oui,
    pct_oui = round(100 * n_oui / total, 1),
    ci_oui_lower = round(100 * ci1$lower, 1),
    ci_oui_upper = round(100 * ci1$upper, 1),
    missing_date_if_oui = missing_date_if_oui,
    oui_in_target_age = oui_in_age,
    pct_in_age = round(100 * oui_in_age / n_oui, 1),
    ci_age_lower = round(100 * ci2$lower, 1),
    ci_age_upper = round(100 * ci2$upper, 1)
  )
}))


print(Date_na_timely2021)
print(Date_na_timely2022)
print(Date_na_timely2023)


library(writexl)
write_xlsx(Date_na_timely2021,"Date_na_timely2021.xlsx")
write_xlsx(Date_na_timely2022,"Date_na_timely2022.xlsx")
write_xlsx(Date_na_timely2023,"Date_na_timely2023.xlsx")

#CHAQUE PROVINCE

#2021
bd2021$province[bd2021$province == "bu Bas Uele Province"] <- "Bas-Uele"
bd2021$province[bd2021$province == "eq Equateur Province"] <- "Equateur"
bd2021$province[bd2021$province == "hk Haut Katanga Province"] <- "Haut-Katanga"
bd2021$province[bd2021$province == "hl Haut Lomami Province"] <- "Haut-Lomami"
bd2021$province[bd2021$province == "hu Haut Uele Province"] <- "Haut-Uele"
bd2021$province[bd2021$province == "it Ituri Province"] <- "Ituri"
bd2021$province[bd2021$province == "kl Kwilu Province"] <- "Kwilu"
bd2021$province[bd2021$province == "kn Kinshasa Province"] <- "Kinshasa"
bd2021$province[bd2021$province == "kg Kwango Province"] <- "Kwango"
bd2021$province[bd2021$province == "kc Kongo Central Province"] <- "Kongo Central"
bd2021$province[bd2021$province == "ke Kasai Oriental Province"] <- "Kasai Oriental"
bd2021$province[bd2021$province == "kr Kasai Central Province"] <- "Kasai Central"
bd2021$province[bd2021$province == "ks Kasai Province"] <- "Kasai"
bd2021$province[bd2021$province == "ll Lualaba Province"] <- "Lualaba"
bd2021$province[bd2021$province == "lm Lomami Province"] <- "Lomami"
bd2021$province[bd2021$province == "md Maindombe Province"] <- "Mai-Ndombe"
bd2021$province[bd2021$province == "mg Mongala Province"] <- "Mongala"
bd2021$province[bd2021$province == "mn Maniema Province"] <- "Maniema"
bd2021$province[bd2021$province == "nk Nord Kivu Province"] <- "Nord-Kivu"
bd2021$province[bd2021$province == "nu Nord Ubangi Province"] <- "Nord-Ubangi"
bd2021$province[bd2021$province == "sk Sud Kivu Province"] <- "Sud-Kivu"
bd2021$province[bd2021$province == "sn Sankuru Province"] <- "Sankuru"
bd2021$province[bd2021$province == "su Sud Ubangi Province"] <- "Sud-Ubangi"
bd2021$province[bd2021$province == "tn Tanganyika Province"] <- "Tanganyika"
bd2021$province[bd2021$province == "tp Tshopo Province"] <- "Tshopo"
bd2021$province[bd2021$province == "tu Tshuapa Province"] <- "Tshuapa"
bd2021$province[bd2021$province == "bu Bas Uele Province"] <- "Bas-Uele"

#2022
bd2022$province[bd2022$province == "bu Bas Uele Province"] <- "Bas-Uele"
bd2022$province[bd2022$province == "eq Equateur Province"] <- "Equateur"
bd2022$province[bd2022$province == "hk Haut Katanga Province"] <- "Haut-Katanga"
bd2022$province[bd2022$province == "hl Haut Lomami Province"] <- "Haut-Lomami"
bd2022$province[bd2022$province == "hu Haut Uele Province"] <- "Haut-Uele"
bd2022$province[bd2022$province == "it Ituri Province"] <- "Ituri"
bd2022$province[bd2022$province == "kl Kwilu Province"] <- "Kwilu"
bd2022$province[bd2022$province == "kn Kinshasa Province"] <- "Kinshasa"
bd2022$province[bd2022$province == "kg Kwango Province"] <- "Kwango"
bd2022$province[bd2022$province == "kc Kongo Central Province"] <- "Kongo Central"
bd2022$province[bd2022$province == "ke Kasai Oriental Province"] <- "Kasai Oriental"
bd2022$province[bd2022$province == "kr Kasai Central Province"] <- "Kasai Central"
bd2022$province[bd2022$province == "ks Kasai Province"] <- "Kasai"
bd2022$province[bd2022$province == "ll Lualaba Province"] <- "Lualaba"
bd2022$province[bd2022$province == "lm Lomami Province"] <- "Lomami"
bd2022$province[bd2022$province == "md Maindombe Province"] <- "Mai-Ndombe"
bd2022$province[bd2022$province == "mg Mongala Province"] <- "Mongala"
bd2022$province[bd2022$province == "mn Maniema Province"] <- "Maniema"
bd2022$province[bd2022$province == "nk Nord Kivu Province"] <- "Nord-Kivu"
bd2022$province[bd2022$province == "nu Nord Ubangi Province"] <- "Nord-Ubangi"
bd2022$province[bd2022$province == "sk Sud Kivu Province"] <- "Sud-Kivu"
bd2022$province[bd2022$province == "sn Sankuru Province"] <- "Sankuru"
bd2022$province[bd2022$province == "su Sud Ubangi Province"] <- "Sud-Ubangi"
bd2022$province[bd2022$province == "tn Tanganyika Province"] <- "Tanganyika"
bd2022$province[bd2022$province == "tp Tshopo Province"] <- "Tshopo"
bd2022$province[bd2022$province == "tu Tshuapa Province"] <- "Tshuapa"
bd2022$province[bd2022$province == "bu Bas Uele Province"] <- "Bas-Uele"

#2023
bd2023$province[bd2023$province == "bu Bas Uele Province"] <- "Bas-Uele"
bd2023$province[bd2023$province == "eq Equateur Province"] <- "Equateur"
bd2023$province[bd2023$province == "hk Haut Katanga Province"] <- "Haut-Katanga"
bd2023$province[bd2023$province == "hl Haut Lomami Province"] <- "Haut-Lomami"
bd2023$province[bd2023$province == "hu Haut Uele Province"] <- "Haut-Uele"
bd2023$province[bd2023$province == "it Ituri Province"] <- "Ituri"
bd2023$province[bd2023$province == "kl Kwilu Province"] <- "Kwilu"
bd2023$province[bd2023$province == "kn Kinshasa Province"] <- "Kinshasa"
bd2023$province[bd2023$province == "kg Kwango Province"] <- "Kwango"
bd2023$province[bd2023$province == "kc Kongo Central Province"] <- "Kongo Central"
bd2023$province[bd2023$province == "ke Kasai Oriental Province"] <- "Kasai Oriental"
bd2023$province[bd2023$province == "kr Kasai Central Province"] <- "Kasai Central"
bd2023$province[bd2023$province == "ks Kasai Province"] <- "Kasai"
bd2023$province[bd2023$province == "ll Lualaba Province"] <- "Lualaba"
bd2023$province[bd2023$province == "lm Lomami Province"] <- "Lomami"
bd2023$province[bd2023$province == "md Maindombe Province"] <- "Mai-Ndombe"
bd2023$province[bd2023$province == "mg Mongala Province"] <- "Mongala"
bd2023$province[bd2023$province == "mn Maniema Province"] <- "Maniema"
bd2023$province[bd2023$province == "nk Nord Kivu Province"] <- "Nord-Kivu"
bd2023$province[bd2023$province == "nu Nord Ubangi Province"] <- "Nord-Ubangi"
bd2023$province[bd2023$province == "sk Sud Kivu Province"] <- "Sud-Kivu"
bd2023$province[bd2023$province == "sn Sankuru Province"] <- "Sankuru"
bd2023$province[bd2023$province == "su Sud Ubangi Province"] <- "Sud-Ubangi"
bd2023$province[bd2023$province == "tn Tanganyika Province"] <- "Tanganyika"
bd2023$province[bd2023$province == "tp Tshopo Province"] <- "Tshopo"
bd2023$province[bd2023$province == "tu Tshuapa Province"] <- "Tshuapa"
bd2023$province[bd2023$province == "bu Bas Uele Province"] <- "Bas-Uele"

table(bd2021$dtc_hepb_hib1)
table(bd2022$dtc_hepb_hib1)
table(bd2023$dtc_hepb_hib1)
table(bd2021$dtc_hepb_hib3)
table(bd2022$dtc_hepb_hib3)
table(bd2023$dtc_hepb_hib3)
table(bd2021$var)
table(bd2022$var)
table(bd2023$var)

bd2021$dtc_hepb_hib3[bd2021$dtc_hepb_hib3 == "1"] <- "Oui"
bd2021$dtc_hepb_hib3[bd2021$dtc_hepb_hib3 == "2"] <- "Non"
bd2022$dtc_hepb_hib3[bd2022$dtc_hepb_hib3 == "1"] <- "Oui"
bd2022$dtc_hepb_hib3[bd2022$dtc_hepb_hib3 == "2"] <- "Non"
bd2023$dtc_hepb_hib3[bd2023$dtc_hepb_hib3 == "1"] <- "Oui"
bd2023$dtc_hepb_hib3[bd2023$dtc_hepb_hib3 == "2"] <- "Non"

bd2021$dtc_hepb_hib1[bd2021$dtc_hepb_hib1 == "1"] <- "Oui"
bd2021$dtc_hepb_hib1[bd2021$dtc_hepb_hib1 == "2"] <- "Non"
bd2022$dtc_hepb_hib1[bd2022$dtc_hepb_hib1 == "1"] <- "Oui"
bd2022$dtc_hepb_hib1[bd2022$dtc_hepb_hib1 == "2"] <- "Non"
bd2023$dtc_hepb_hib1[bd2023$dtc_hepb_hib1 == "1"] <- "Oui"
bd2023$dtc_hepb_hib1[bd2023$dtc_hepb_hib1 == "2"] <- "Non"

bd2021$var[bd2021$var == "1"] <- "Oui"
bd2021$var[bd2021$var == "2"] <- "Non"
bd2022$var[bd2022$var == "1"] <- "Oui"
bd2022$var[bd2022$var == "2"] <- "Non"
bd2023$var[bd2023$var == "1"] <- "Oui"
bd2023$var[bd2023$var == "2"] <- "Non"


table(bd2021$qa209a)
table(bd2022$qa209a)
table(bd2023$qa209a)

bd2022$qa209a[bd2022$qa209a == "0"] <- "Non"
bd2023$qa209a[bd2023$qa209a == "0"] <- "Non"
bd2021$qa209a[bd2021$qa209a == "oui"] <- "Oui"
bd2021$qa209a[bd2021$qa209a == "non"] <- "Non"
bd2022$qa209a[bd2022$qa209a == "oui"] <- "Oui"

table(bd2021$qa209b)
table(bd2021$qa209b)
table(bd2021$qa209b)

bd2021$television<-bd2021$qa209a
bd2022$television<-bd2022$qa209a
bd2023$television<-bd2023$qa209a

bd2021$radio<-bd2021$qa209b
bd2022$radio<-bd2022$qa209b
bd2023$radio<-bd2023$qa209b

bd2022$qa209b[bd2022$qa209b == "0"] <- "Non"
bd2023$qa209b[bd2023$qa209b == "0"] <- "Non"
bd2021$qa209b[bd2021$qa209b == "oui"] <- "Oui"
bd2021$qa209b[bd2021$qa209b == "non"] <- "Non"
bd2022$qa209b[bd2022$qa209b == "oui"] <- "Oui"


#2021 Knowledge mothers/guardians
table(bd2021$qa501_1)
bd2021$qa501_1[bd2021$qa501_1 == "oui"] <- "1"
table(bd2021$qa501_2)
bd2021$qa501_2[bd2021$qa501_2 == "oui"] <- "1"
table(bd2021$qa501_3)
bd2021$qa501_3[bd2021$qa501_3 == "oui"] <- "1"
table(bd2021$qa501_4)
bd2021$qa501_4[bd2021$qa501_4 == "oui"] <- "1"
table(bd2021$qa501_5)
bd2021$qa501_5[bd2021$qa501_5 == "oui"] <- "1"
table(bd2021$qa501_6)
bd2021$qa501_6[bd2021$qa501_6 == "oui"] <- "1"
table(bd2021$qa501_7)
bd2021$qa501_7[bd2021$qa501_7 == "oui"] <- "1"
table(bd2021$qa501_8)
bd2021$qa501_8[bd2021$qa501_8 == "oui"] <- "1"
table(bd2021$qa501_9)
bd2021$qa501_9[bd2021$qa501_9 == "oui"] <- "1"
table(bd2021$qa501_10)
bd2021$qa501_10[bd2021$qa501_10 == "oui"] <- "1"
table(bd2021$qa501_11)
bd2021$qa501_11[bd2021$qa501_11 == "oui"] <- "1"
table(bd2021$qa501_12)
bd2021$qa501_12[bd2021$qa501_12 == "oui"] <- "1"

bd2021$Tuberculose<-bd2021$qa501_1
bd2021$Diphterie<-bd2021$qa501_2
bd2021$Coqueluche<-bd2021$qa501_3
bd2021$Hepatite<-bd2021$qa501_4
bd2021$Hemophilis<-bd2021$qa501_5
bd2021$Poliomyelite<-bd2021$qa501_6
bd2021$Rougeole<-bd2021$qa501_7
bd2021$Fievre_j<-bd2021$qa501_8
bd2021$Tetanos<-bd2021$qa501_9
bd2021$Pneumonie<- bd2021$qa501_10
bd2021$Meningite<- bd2021$qa501_11
bd2021$Diarrhee<- bd2021$qa501_11


#2022 Knowledge mothers/guardians
table(bd2022$qa501_1)
bd2022$qa501_1[bd2022$qa501_1 == "oui"] <- "1"
table(bd2022$qa501_2)
bd2022$qa501_2[bd2022$qa501_2 == "oui"] <- "1"
table(bd2022$qa501_3)
bd2022$qa501_3[bd2022$qa501_3 == "oui"] <- "1"
table(bd2022$qa501_4)
bd2022$qa501_4[bd2022$qa501_4 == "oui"] <- "1"
table(bd2022$qa501_5)
bd2022$qa501_5[bd2022$qa501_5 == "oui"] <- "1"
table(bd2022$qa501_6)
bd2022$qa501_6[bd2022$qa501_6 == "oui"] <- "1"
table(bd2022$qa501_7)
bd2022$qa501_7[bd2022$qa501_7 == "oui"] <- "1"
table(bd2022$qa501_8)
bd2022$qa501_8[bd2022$qa501_8 == "oui"] <- "1"
table(bd2022$qa501_9)
bd2022$qa501_9[bd2022$qa501_9 == "oui"] <- "1"
table(bd2022$qa501_10)
bd2022$qa501_10[bd2022$qa501_10 == "oui"] <- "1"
table(bd2022$qa501_11)
bd2022$qa501_11[bd2022$qa501_11 == "oui"] <- "1"
table(bd2022$qa501_12)
bd2022$qa501_12[bd2022$qa501_12 == "oui"] <- "1"

bd2022$Tuberculose<-bd2022$qa501_1
bd2022$Diphterie<-bd2022$qa501_2
bd2022$Coqueluche<-bd2022$qa501_3
bd2022$Hepatite<-bd2022$qa501_4
bd2022$Hemophilis<-bd2022$qa501_5
bd2022$Poliomyelite<-bd2022$qa501_6
bd2022$Rougeole<-bd2022$qa501_7
bd2022$Fievre_j<-bd2022$qa501_8
bd2022$Tetanos<-bd2022$qa501_9
bd2022$Pneumonie<- bd2022$qa501_10
bd2022$Meningite<- bd2022$qa501_11
bd2022$Diarrhee<- bd2022$qa501_11

#2023 Knowledge mothers/guardians
table(bd2023$qa501_1)
bd2023$qa501_1[bd2023$qa501_1 == "Oui"] <- "1"
table(bd2023$qa501_2)
bd2023$qa501_2[bd2023$qa501_2 == "Oui"] <- "1"
table(bd2023$qa501_3)
bd2023$qa501_3[bd2023$qa501_3 == "Oui"] <- "1"
table(bd2023$qa501_4)
bd2023$qa501_4[bd2023$qa501_4 == "Oui"] <- "1"
table(bd2023$qa501_5)
bd2023$qa501_5[bd2023$qa501_5 == "Oui"] <- "1"
table(bd2023$qa501_6)
bd2023$qa501_6[bd2023$qa501_6 == "Oui"] <- "1"
table(bd2023$qa501_7)
bd2023$qa501_7[bd2023$qa501_7 == "Oui"] <- "1"
table(bd2023$qa501_8)
bd2023$qa501_8[bd2023$qa501_8 == "Oui"] <- "1"
table(bd2023$qa501_9)
bd2023$qa501_9[bd2023$qa501_9 == "Oui"] <- "1"
table(bd2023$qa501_10)
bd2023$qa501_10[bd2023$qa501_10 == "Oui"] <- "1"
table(bd2023$qa501_11)
bd2023$qa501_11[bd2023$qa501_11 == "Oui"] <- "1"
table(bd2023$qa501_12)
bd2023$qa501_12[bd2023$qa501_12 == "Oui"] <- "1"

bd2023$Tuberculose<-bd2023$qa501_1
bd2023$Diphterie<-bd2023$qa501_2
bd2023$Coqueluche<-bd2023$qa501_3
bd2023$Hepatite<-bd2023$qa501_4
bd2023$Hemophilis<-bd2023$qa501_5
bd2023$Poliomyelite<-bd2023$qa501_6
bd2023$Rougeole<-bd2023$qa501_7
bd2023$Fievre_j<-bd2023$qa501_8
bd2023$Tetanos<-bd2023$qa501_9
bd2023$Pneumonie<- bd2023$qa501_10
bd2023$Meningite<- bd2023$qa501_11
bd2023$Diarrhee<- bd2023$qa501_11


#Create knowlege score

#2021
bd2021$Tuberculose<-as.numeric(bd2021$Tuberculose)
bd2021$Diphterie<-as.numeric(bd2021$Diphterie)
bd2021$Coqueluche<-as.numeric(bd2021$Coqueluche)
bd2021$Hepatite<-as.numeric(bd2021$Hepatite)
bd2021$Hemophilis<-as.numeric(bd2021$Hemophilis)
bd2021$Poliomyelite<-as.numeric(bd2021$Poliomyelite)
bd2021$Rougeole<-as.numeric(bd2021$Rougeole)
bd2021$Fievre_j<-as.numeric(bd2021$Fievre_j)
bd2021$Tetanos<-as.numeric(bd2021$Tetanos)
bd2021$Pneumonie<- as.numeric(bd2021$Pneumonie)
bd2021$Meningite<- as.numeric(bd2021$Meningite)
bd2021$Diarrhee<- as.numeric(bd2021$Diarrhee)

bd2021$knowledge_score <- rowSums(bd2021[, c("Tuberculose",
                                             "Diphterie",
                                             "Coqueluche",
                                             "Hepatite",
                                             "Hemophilis",
                                             "Poliomyelite",
                                             "Rougeole",
                                             "Fievre_j",
                                             "Tetanos",
                                             "Diarrhee",
                                             "Pneumonie",
                                             "Meningite")], na.rm = TRUE)

#2022
bd2022$Tuberculose<-as.numeric(bd2022$Tuberculose)
bd2022$Diphterie<-as.numeric(bd2022$Diphterie)
bd2022$Coqueluche<-as.numeric(bd2022$Coqueluche)
bd2022$Hepatite<-as.numeric(bd2022$Hepatite)
bd2022$Hemophilis<-as.numeric(bd2022$Hemophilis)
bd2022$Poliomyelite<-as.numeric(bd2022$Poliomyelite)
bd2022$Rougeole<-as.numeric(bd2022$Rougeole)
bd2022$Fievre_j<-as.numeric(bd2022$Fievre_j)
bd2022$Tetanos<-as.numeric(bd2022$Tetanos)
bd2022$Pneumonie<- as.numeric(bd2022$Pneumonie)
bd2022$Meningite<- as.numeric(bd2022$Meningite)
bd2022$Diarrhee<- as.numeric(bd2022$Diarrhee)

bd2022$knowledge_score <- rowSums(bd2022[, c("Tuberculose",
                                             "Diphterie",
                                             "Coqueluche",
                                             "Hepatite",
                                             "Hemophilis",
                                             "Poliomyelite",
                                             "Rougeole",
                                             "Fievre_j",
                                             "Tetanos",
                                             "Diarrhee",
                                             "Pneumonie",
                                             "Meningite")], na.rm = TRUE)

#2023
bd2023$Tuberculose<-as.numeric(bd2023$Tuberculose)
bd2023$Diphterie<-as.numeric(bd2023$Diphterie)
bd2023$Coqueluche<-as.numeric(bd2023$Coqueluche)
bd2023$Hepatite<-as.numeric(bd2023$Hepatite)
bd2023$Hemophilis<-as.numeric(bd2023$Hemophilis)
bd2023$Poliomyelite<-as.numeric(bd2023$Poliomyelite)
bd2023$Rougeole<-as.numeric(bd2023$Rougeole)
bd2023$Fievre_j<-as.numeric(bd2023$Fievre_j)
bd2023$Tetanos<-as.numeric(bd2023$Tetanos)
bd2023$Pneumonie<- as.numeric(bd2023$Pneumonie)
bd2023$Meningite<- as.numeric(bd2023$Meningite)
bd2023$Diarrhee<- as.numeric(bd2023$Diarrhee)

bd2023$knowledge_score <- rowSums(bd2023[, c("Tuberculose",
                                             "Diphterie",
                                             "Coqueluche",
                                             "Hepatite",
                                             "Hemophilis",
                                             "Poliomyelite",
                                             "Rougeole",
                                             "Fievre_j",
                                             "Tetanos",
                                             "Diarrhee",
                                             "Pneumonie",
                                             "Meningite")], na.rm = TRUE)

bd2021$knowledge_score<-as.numeric(bd2021$knowledge_score)
bd2022$knowledge_score<-as.numeric(bd2022$knowledge_score)
bd2023$knowledge_score<-as.numeric(bd2023$knowledge_score)

summary(bd2021$knowledge_score)
summary(bd2022$knowledge_score)
summary(bd2023$knowledge_score)

#knowledge categories

#2021
# Calculate the median of knowledge_score (excluding NAs)
median_score2021 <- median(bd2021$knowledge_score, na.rm = TRUE)

# Create the categorical knowledge variable
bd2021$knowledge_cat <- ifelse(
  bd2021$knowledge_score > median_score2021, "Good_knowledge",
  ifelse(bd2021$knowledge_score < median_score2021, "Poor_knowledge", NA)
)
#2022
# Calculate the median of knowledge_score (excluding NAs)
median_score2022 <- median(bd2022$knowledge_score, na.rm = TRUE)

# Create the categorical knowledge variable
bd2022$knowledge_cat <- ifelse(
  bd2022$knowledge_score > median_score2022, "Good_knowledge",
  ifelse(bd2022$knowledge_score < median_score2022, "Poor_knowledge", NA)
)
#2023
# Calculate the median of knowledge_score (excluding NAs)
median_score2023 <- median(bd2023$knowledge_score, na.rm = TRUE)

# Create the categorical knowledge variable
bd2023$knowledge_cat <- ifelse(
  bd2023$knowledge_score > median_score2023, "Good_knowledge",
  ifelse(bd2023$knowledge_score < median_score2023, "Poor_knowledge", NA)
)

#CHILDREN under 5 in the household
table(bd2021$qa223)
table(bd2022$qa223)
table(bd2023$qa223)

bd2021$children_u5<-as.numeric(bd2021$qa223)
bd2022$children_u5<-as.numeric(bd2022$qa223)
bd2023$children_u5<-as.numeric(bd2023$qa223)

bd2021$children_u5_cat <- ifelse(
  bd2021$children_u5 %in% c(1, 2), "1 or 2",
  ifelse(bd2021$children_u5 >= 3, "3 or more", NA)
)
bd2022$children_u5_cat <- ifelse(
  bd2022$children_u5 %in% c(1, 2), "1 or 2",
  ifelse(bd2022$children_u5 >= 3, "3 or more", NA)
)
bd2023$children_u5_cat <- ifelse(
  bd2023$children_u5 %in% c(1, 2), "1 or 2",
  ifelse(bd2023$children_u5 >= 3, "3 or more", NA)
)

### SUBSET DATASET 2021 2022 2023 #######

bd2021vf<-subset(bd2021, select=c("province",
                                  "healthzone",
                                  "area_residency",
                                  "wealth",
                                  "television",
                                  "radio",
                                  "child_sex",
                                  "pb",
                                  "children_u5_cat",
                                  "marital_status",
                                  "using_telephone",
                                  "age_mother",
                                  "education_mother",
                                  "religion",
                                  "knowledge_cat",
                                  "child_civil_registration",
                                  "complete_vaccination",
                                  "paid_vaccination",
                                  "bcg",
                                  "vpob0",
                                  "vpob1",
                                  "dtc_hepb_hib1",
                                  "dtc_hepb_hib3",
                                  "var",
                                  "vaa",
                                  "date_visit",
                                  "age_months",
                                  "date_bcgT",
                                  "age_dtc_hepb_hib1",
                                  "age_dtc_hepb_hib1_w",
                                  "date_dtc_hepb_hib3T",
                                  "age_vpob0",
                                  "age_vpob0_w",
                                  "date_bcg",
                                  "date_vpob0",
                                  "date_dtc_hepb_hib1",
                                  "date_dtc_hepb_hib3",
                                  "date_var",
                                  "date_vaa",
                                  "birthdate",
                                  "age_days",
                                  "age_bcg",
                                  "age_bcg_w",
                                  "date_dtc_hepb_hib1T",
                                  "age_dtc_hepb_hib3",
                                  "age_dtc_hepb_hib3_w",
                                  "date_varT",
                                  "age_var_m",
                                  "date_vaaT",
                                  "age_vaa_m",
                                  "date_vpob0T"))

#undefined columns ? how to find them?

vars <- c("province",
          "healthzone",
          "area_residency",
          "wealth",
          "television",
          "radio",
          "child_sex",
          "pb",
          "children_u5_cat",
          "marital_status",
          "using_telephone",
          "age_mother",
          "education_mother",
          "religion",
          "knowledge_cat",
          "child_civil_registration",
          "complete_vaccination",
          "paid_vaccination",
          "bcg",
          "vpob0",
          "vpob1",
          "dtc_hepb_hib1",
          "dtc_hepb_hib3",
          "var",
          "vaa",
          "date_visit",
          "age_months",
          "date_bcgT",
          "age_dtc_hepb_hib1",
          "age_dtc_hepb_hib1_w",
          "date_dtc_hepb_hib3T",
          "age_vpob0",
          "age_vpob0_w",
          "date_bcg",
          "date_vpob0",
          "date_dtc_hepb_hib1",
          "date_dtc_hepb_hib3",
          "date_var",
          "date_vaa",
          "birthdate",
          "age_days",
          "age_bcg",
          "age_bcg_w",
          "date_dtc_hepb_hib1T",
          "age_dtc_hepb_hib3",
          "age_dtc_hepb_hib3_w",
          "date_varT",
          "age_var_m",
          "date_vaaT",
          "age_vaa_m",
          "date_vpob0T")

# List variables you asked for but that are missing from bd2021
setdiff(vars, names(bd2022))

bd2021vf<-subset(bd2021, select=c("province",
                                  "healthzone",
                                  "area_residency",
                                  "wealth",
                                  "television",
                                  "radio",
                                  "child_sex",
                                  "pb",
                                  "children_u5_cat",
                                  "marital_status",
                                  "using_telephone",
                                  "age_mother",
                                  "education_mother",
                                  "religion",
                                  "knowledge_cat",
                                  "child_civil_registration",
                                  "complete_vaccination",
                                  "paid_vaccination",
                                  "bcg",
                                  "vpob0",
                                  "vpob1",
                                  "dtc_hepb_hib1",
                                  "dtc_hepb_hib3",
                                  "var",
                                  "vaa",
                                  "date_visit",
                                  "age_months",
                                  "date_bcgT",
                                  "age_dtc_hepb_hib1",
                                  "age_dtc_hepb_hib1_w",
                                  "date_dtc_hepb_hib3T",
                                  "age_vpob0",
                                  "age_vpob0_w",
                                  "date_bcg",
                                  "date_vpob0",
                                  "date_dtc_hepb_hib1",
                                  "date_dtc_hepb_hib3",
                                  "date_var",
                                  "date_vaa",
                                  "birthdate",
                                  "age_days",
                                  "age_bcg",
                                  "age_bcg_w",
                                  "date_dtc_hepb_hib1T",
                                  "age_dtc_hepb_hib3",
                                  "age_dtc_hepb_hib3_w",
                                  "date_varT",
                                  "age_var_m",
                                  "date_vaaT",
                                  "age_vaa_m",
                                  "date_vpob0T"))

bd2022vf<-subset(bd2022, select=c("province",
                                  "healthzone",
                                  "area_residency",
                                  "wealth",
                                  "television",
                                  "radio",
                                  "child_sex",
                                  "pb",
                                  "children_u5_cat",
                                  "marital_status",
                                  "using_telephone",
                                  "age_mother",
                                  "education_mother",
                                  "religion",
                                  "knowledge_cat",
                                  "child_civil_registration",
                                  "complete_vaccination",
                                  "paid_vaccination",
                                  "bcg",
                                  "vpob0",
                                  "vpob1",
                                  "dtc_hepb_hib1",
                                  "dtc_hepb_hib3",
                                  "var",
                                  "vaa",
                                  "date_visit",
                                  "age_months",
                                  "date_bcgT",
                                  "age_dtc_hepb_hib1",
                                  "age_dtc_hepb_hib1_w",
                                  "date_dtc_hepb_hib3T",
                                  "age_vpob0",
                                  "age_vpob0_w",
                                  "date_bcg",
                                  "date_vpob0",
                                  "date_dtc_hepb_hib1",
                                  "date_dtc_hepb_hib3",
                                  "date_var",
                                  "date_vaa",
                                  "birthdate",
                                  "age_days",
                                  "age_bcg",
                                  "age_bcg_w",
                                  "date_dtc_hepb_hib1T",
                                  "age_dtc_hepb_hib3",
                                  "age_dtc_hepb_hib3_w",
                                  "date_varT",
                                  "age_var_m",
                                  "date_vaaT",
                                  "age_vaa_m",
                                  "date_vpob0T"))

bd2023vf<-subset(bd2023, select=c("province",
                                  "healthzone",
                                  "area_residency",
                                  "wealth",
                                  "television",
                                  "radio",
                                  "child_sex",
                                  "pb",
                                  "children_u5_cat",
                                  "marital_status",
                                  "using_telephone",
                                  "age_mother",
                                  "education_mother",
                                  "religion",
                                  "knowledge_cat",
                                  "child_civil_registration",
                                  "complete_vaccination",
                                  "paid_vaccination",
                                  "bcg",
                                  "vpob0",
                                  "vpob1",
                                  "dtc_hepb_hib1",
                                  "dtc_hepb_hib3",
                                  "var",
                                  "vaa",
                                  "date_visit",
                                  "age_months",
                                  "date_bcgT",
                                  "age_dtc_hepb_hib1",
                                  "age_dtc_hepb_hib1_w",
                                  "date_dtc_hepb_hib3T",
                                  "age_vpob0",
                                  "age_vpob0_w",
                                  "date_bcg",
                                  "date_vpob0",
                                  "date_dtc_hepb_hib1",
                                  "date_dtc_hepb_hib3",
                                  "date_var",
                                  "date_vaa",
                                  "birthdate",
                                  "age_days",
                                  "age_bcg",
                                  "age_bcg_w",
                                  "date_dtc_hepb_hib1T",
                                  "age_dtc_hepb_hib3",
                                  "age_dtc_hepb_hib3_w",
                                  "date_varT",
                                  "age_var_m",
                                  "date_vaaT",
                                  "age_vaa_m",
                                  "date_vpob0T"))
                                   
##############END DATA MANAGEMENT##############################################

#AGE OF VACCINATION

summary(bd2021vf$age_bcg)

# Load necessary libraries
library(dplyr)
library(ggplot2)


# Combine the datasets and add the 'year' column
combined_data <- bind_rows(
  mutate(bd2021vf, year = "2021"),
  mutate(bd2022vf, year = "2022"),
  mutate(bd2023vf, year = "2023")
)

# View the combined data (optional)
head(combined_data)

bd2021vf$pb<-as.numeric(bd2021vf$pb)
bd2022vf$pb<-as.numeric(bd2022vf$pb)
bd2023vf$pb<-as.numeric(bd2023vf$pb)

bd2021vf$nutritional_status <- ifelse(
  bd2021vf$pb > 124, "125 cm and above",
  ifelse(bd2021vf$pb > 114 & bd2021vf$pb <= 124, "115 to 124 cm", 
         ifelse(bd2021vf$pb < 115, "below 115 cm", NA)))

bd2022vf$nutritional_status <- ifelse(
  bd2022vf$pb > 124, "125 cm and above",
  ifelse(bd2022vf$pb > 114 & bd2022vf$pb <= 124, "115 to 124 cm", 
         ifelse(bd2022vf$pb < 115, "below 115 cm", NA)))

bd2023vf$nutritional_status <- ifelse(
  bd2023vf$pb > 124, "125 cm and above",
  ifelse(bd2023vf$pb > 114 & bd2023vf$pb <= 124, "115 to 124 cm", 
         ifelse(bd2023vf$pb < 115, "below 115 cm", NA)))

table(bd2022vf$pb)
class(bd2022vf$pb)

table(bd2021vf$nutritional_status)
table(bd2022vf$nutritional_status)
table(bd2023vf$nutritional_status)

bd2021vf$age_mother<-as.numeric(bd2021vf$age_mother)
bd2022vf$age_mother<-as.numeric(bd2022vf$age_mother)
bd2023vf$age_mother<-as.numeric(bd2023vf$age_mother)

bd2021vf$age_mother_cat <- ifelse(
  bd2021vf$age_mother > 34, "35 years +",
  ifelse(bd2021vf$age_mother > 19 & bd2021vf$age_mother <= 34, "20-34 years", 
         ifelse(bd2021vf$age_mother < 20, "below 20 years", NA)))

bd2022vf$age_mother_cat <- ifelse(
  bd2022vf$age_mother > 34, "35 years +",
  ifelse(bd2022vf$age_mother > 19 & bd2022vf$age_mother <= 34, "20-34 years", 
         ifelse(bd2022vf$age_mother < 20, "below 20 years", NA)))

bd2023vf$age_mother_cat <- ifelse(
  bd2023vf$age_mother > 34, "35 years +",
  ifelse(bd2023vf$age_mother > 19 & bd2023vf$age_mother <= 34, "20-34 years", 
         ifelse(bd2023vf$age_mother < 20, "below 20 years", NA)))

table(bd2021vf$age_mother_cat)

################# COVERAGE RATES ##############################

library(binom)              


#1 BCG
#2021

# Group by province and compute statistics
bcg_2021 <- bd2021vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(bcg == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(bcg == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically

#1 BCG
#2022
bcg_2022 <- bd2022vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(bcg == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(bcg == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically


#1 BCG
#2023
bcg_2023 <- bd2023vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(bcg == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(bcg == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically


print(bcg_2021)
print(bcg_2022)
print(bcg_2023)


#2 PENTA1
#2021

# Group by province and compute statistics
penta1_2021 <- bd2021vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(dtc_hepb_hib1 == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(dtc_hepb_hib1 == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically

#2 PENTA1
#2022
penta1_2022 <- bd2022vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(dtc_hepb_hib1 == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(dtc_hepb_hib1 == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically


#2 PENTA1
#2023
penta1_2023 <- bd2023vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(dtc_hepb_hib1 == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(dtc_hepb_hib1 == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically

print(penta1_2021)
print(penta1_2022)
print(penta1_2023)


#3 PENTA3
#2021

# Group by province and compute statistics
penta3_2021 <- bd2021vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(dtc_hepb_hib3 == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(dtc_hepb_hib3 == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically

#3 PENTA3
#2022
penta3_2022 <- bd2022vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(dtc_hepb_hib3 == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(dtc_hepb_hib3 == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically


#3 PENTA3
#2023
penta3_2023 <- bd2023vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(dtc_hepb_hib3 == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(dtc_hepb_hib3 == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically

print(penta3_2021)
print(penta3_2022)
print(penta3_2023)


#4 VAR
#2021

# Group by province and compute statistics
var_2021 <- bd2021vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(var == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(var == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically

#4 VAR
#2022
var_2022 <- bd2022vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(var == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(var == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically


#4 VAR
#2023
var_2023 <- bd2023vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(var == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(var == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically

print(var_2021)
print(var_2022)
print(var_2023)

table(bd2021vf$complete_vaccination)
table(bd2022vf$complete_vaccination)
table(bd2023vf$complete_vaccination)

#N°5 COMPLETE VACCINATION
#2021

# Group by province and compute statistics
complete_vaccination_2021 <- bd2021vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(complete_vaccination == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(complete_vaccination == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically

#5 COMPLETE_VACCINATION
#2022
complete_vaccination_2022 <- bd2022vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(complete_vaccination == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(complete_vaccination == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically


#5 COMPLETE_VACCINATION
#2023
complete_vaccination_2023 <- bd2023vf %>%
  group_by(province) %>%
  summarise(
    n_success = sum(complete_vaccination == "Oui", na.rm = TRUE),    # number of "Oui"
    n_failure = sum(complete_vaccination == "Non", na.rm = TRUE),     # number of "Non"
    n_total = n_success + n_failure                  # total "Oui" + "Non"
  ) %>%
  filter(n_total > 0) %>%                            # Keep only provinces with data
  rowwise() %>%
  mutate(
    prop_success = n_success / n_total,              # proportion of success
    ci = list(binom.confint(n_success, n_total, method = "exact")),  # exact CI
    lower = ci$lower,
    upper = ci$upper
  ) %>%
  select(province, n_success, n_failure, n_total, prop_success, lower, upper) %>%
  arrange(province)   # Optional: sort alphabetically

print(complete_vaccination_2021)
print(complete_vaccination_2022)
print(complete_vaccination_2023)


write_xlsx(bcg_2021, "bcg_2021.xlsx")
write_xlsx(bcg_2022, "bcg_2022.xlsx")
write_xlsx(bcg_2023, "bcg_2023.xlsx")

write_xlsx(penta1_2021, "penta1_2021.xlsx")
write_xlsx(penta1_2022, "penta1_2022.xlsx")
write_xlsx(penta1_2023, "penta1_2023.xlsx")

write_xlsx(penta3_2021, "penta3_2021.xlsx")
write_xlsx(penta3_2022, "penta3_2022.xlsx")
write_xlsx(penta3_2023, "penta3_2023.xlsx")

write_xlsx(var_2021, "var_2021.xlsx")
write_xlsx(var_2022, "var_2022.xlsx")
write_xlsx(var_2023, "var_2023.xlsx")

write_xlsx(complete_vaccination_2021, "complete_vaccination_2021.xlsx")
write_xlsx(complete_vaccination_2022, "complete_vaccination_2022.xlsx")
write_xlsx(complete_vaccination_2023, "complete_vaccination_2023.xlsx")

##################################################################################


####### CREATING TIMELY VARIABLES ####################################

##Timely variables

#2021
bd2021vf <- bd2021vf %>%
  mutate(bcg_timely = ifelse(bcg == "Oui"  & age_bcg >= 0 & age_bcg <= 14, "Oui",
                             ifelse(bcg == "Oui", "Non", "Non")))
bd2021vf <- bd2021vf %>%
  mutate(dtc_hepb_hib1_timely = ifelse(dtc_hepb_hib1 == "Oui" & age_dtc_hepb_hib1_w >= 6 & age_dtc_hepb_hib1_w <=9.999, "Oui",
                             ifelse(dtc_hepb_hib1 == "Oui", "Non", "Non")))
bd2021vf <- bd2021vf %>%
  mutate(dtc_hepb_hib3_timely = ifelse(dtc_hepb_hib3 == "Oui" & age_dtc_hepb_hib3_w  >= 14 & age_dtc_hepb_hib3_w <=17.999, "Oui",
                                       ifelse(dtc_hepb_hib3 == "Oui", "Non", "Non")))
bd2021vf <- bd2021vf %>%
  mutate(var_timely = ifelse(var == "Oui" & age_var_m  >= 9.0 & age_var_m <=9.999, "Oui",
                                       ifelse(var == "Oui", "Non", "Non")))
bd2021vf <- bd2021vf %>%
  mutate(complete_vaccination_timely = ifelse(complete_vaccination == "Oui" & age_var_m  >= 9.0 & age_var_m <=9.999, "Oui",
                                       ifelse(complete_vaccination == "Oui", "Non", "Non")))

#2022
bd2022vf <- bd2022vf %>%
  mutate(bcg_timely = ifelse(bcg == "Oui"  & age_bcg >= 0 & age_bcg <= 14, "Oui",
                             ifelse(bcg == "Oui", "Non", "Non")))
bd2022vf <- bd2022vf %>%
  mutate(dtc_hepb_hib1_timely = ifelse(dtc_hepb_hib1 == "Oui" & age_dtc_hepb_hib1_w >= 6 & age_dtc_hepb_hib1_w <=9.999, "Oui",
                                       ifelse(dtc_hepb_hib1 == "Oui", "Non", "Non")))
bd2022vf <- bd2022vf %>%
  mutate(dtc_hepb_hib3_timely = ifelse(dtc_hepb_hib3 == "Oui" & age_dtc_hepb_hib3_w  >= 14 & age_dtc_hepb_hib3_w <=17.999, "Oui",
                                       ifelse(dtc_hepb_hib3 == "Oui", "Non", "Non")))
bd2022vf <- bd2022vf %>%
  mutate(var_timely = ifelse(var == "Oui" & age_var_m  >= 9.0 & age_var_m <=9.999, "Oui",
                             ifelse(var == "Oui", "Non", "Non")))
bd2022vf <- bd2022vf %>%
  mutate(complete_vaccination_timely = ifelse(complete_vaccination == "Oui" & age_var_m  >= 9.0 & age_var_m <=9.999, "Oui",
                                              ifelse(complete_vaccination == "Oui", "Non", "Non")))

#2023
bd2023vf <- bd2023vf %>%
  mutate(bcg_timely = ifelse(bcg == "Oui"  & age_bcg >= 0 & age_bcg <= 14, "Oui",
                             ifelse(bcg == "Oui", "Non", "Non")))
bd2023vf <- bd2023vf %>%
  mutate(dtc_hepb_hib1_timely = ifelse(dtc_hepb_hib1 == "Oui" & age_dtc_hepb_hib1_w >= 6 & age_dtc_hepb_hib1_w <=9.999, "Oui",
                                       ifelse(dtc_hepb_hib1 == "Oui", "Non", "Non")))
bd2023vf <- bd2023vf %>%
  mutate(dtc_hepb_hib3_timely = ifelse(dtc_hepb_hib3 == "Oui" & age_dtc_hepb_hib3_w  >= 14 & age_dtc_hepb_hib3_w <=17.999, "Oui",
                                       ifelse(dtc_hepb_hib3 == "Oui", "Non", "Non")))
bd2023vf <- bd2023vf %>%
  mutate(var_timely = ifelse(var == "Oui" & age_var_m  >= 9.0 & age_var_m <=9.999, "Oui",
                             ifelse(var == "Oui", "Non", "Non")))
bd2023vf <- bd2023vf %>%
  mutate(complete_vaccination_timely = ifelse(complete_vaccination == "Oui" & age_var_m  >= 9.0 & age_var_m <=9.999, "Oui",
                                              ifelse(complete_vaccination == "Oui", "Non", "Non")))


# Combine the datasets and add the 'year' column
combined_data <- bind_rows(
  mutate(bd2021vf, year = "2021"),
  mutate(bd2022vf, year = "2022"),
  mutate(bd2023vf, year = "2023")
)

# View the combined data (optional)
head(combined_data)


# Function to compute TIMELY proportions 
timely_proportions <- function(data, var_name) {
  data %>%
    group_by(province) %>%
    summarise(
      n_success = sum(.data[[var_name]] == "Oui", na.rm = TRUE),
      n_failure = sum(.data[[var_name]] == "Non", na.rm = TRUE),
      n_total = n_success + n_failure,
      .groups = "drop"
    ) %>%
    filter(n_total > 0) %>%
    rowwise() %>%
    mutate(
      prop_success = n_success / n_total,
      ci = binom.confint(n_success, n_total, method = "exact"),
      lower = ci$lower,
      upper = ci$upper,
      variable = var_name
    ) %>%
    select(province, variable, prop_success, lower, upper)
}

# Apply the function for each variable
var_2023_timely   <- timely_proportions(bd2023vf, "var")
bcg_2023_timely   <- timely_proportions(bd2023vf, "bcg")
penta1_2023_timely <- timely_proportions(bd2023vf, "dtc_hepb_hib1")
penta3_2023_timely <- timely_proportions(bd2023vf, "dtc_hepb_hib3")
complete_vaccination_2023_timely <- timely_proportions(bd2023vf, "complete_vaccination")
var_2022_timely   <- timely_proportions(bd2022vf, "var")
bcg_2022_timely   <- timely_proportions(bd2022vf, "bcg")
penta1_2022_timely <- timely_proportions(bd2022vf, "dtc_hepb_hib1")
penta3_2022_timely <- timely_proportions(bd2022vf, "dtc_hepb_hib3")
complete_vaccination_2022_timely <- timely_proportions(bd2022vf, "complete_vaccination")
var_2021_timely   <- timely_proportions(bd2021vf, "var")
bcg_2021_timely   <- timely_proportions(bd2021vf, "bcg")
penta1_2021_timely <- timely_proportions(bd2021vf, "dtc_hepb_hib1")
penta3_2021_timely <- timely_proportions(bd2021vf, "dtc_hepb_hib3")
complete_vaccination_2021_timely <- timely_proportions(bd2021vf, "complete_vaccination")

table(bd2021vf$bcg)
table(bd2021vf$bcg_timely)

write_xlsx(bcg_2021_timely, "bcg_2021_timely.xlsx")
write_xlsx(bcg_2022_timely, "bcg_2022_timely.xlsx")
write_xlsx(bcg_2023_timely, "bcg_2023_timely.xlsx")

write_xlsx(penta1_2021_timely, "penta1_2021_timely.xlsx")
write_xlsx(penta1_2022_timely, "penta1_2022_timely.xlsx")
write_xlsx(penta1_2023_timely, "penta1_2023_timely.xlsx")

write_xlsx(penta3_2021_timely, "penta3_2021_timely.xlsx")
write_xlsx(penta3_2022_timely, "penta3_2022_timely.xlsx")
write_xlsx(penta3_2023_timely, "penta3_2023_timely.xlsx")

write_xlsx(var_2021_timely, "var_2021_timely.xlsx")
write_xlsx(var_2022_timely, "var_2022_timely.xlsx")
write_xlsx(var_2023_timely, "var_2023_timely.xlsx")

write_xlsx(complete_vaccination_2021_timely, "complete_vaccination_2021_timely.xlsx")
write_xlsx(complete_vaccination_2022_timely, "complete_vaccination_2022_timely.xlsx")
write_xlsx(complete_vaccination_2023_timely, "complete_vaccination_2023_timely.xlsx")

write_xlsx(bd2021vf, "bd2021vf.xlsx")
write_xlsx(bd2022vf, "bd2022vf.xlsx")
write_xlsx(bd2023vf, "bd2023vf.xlsx")



################################# PLOTING #######################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(binom)
library(purrr)
library(scales)

wdir <- setwd("C:/Users/HP/OneDrive/ECV Paper writing/Base_ECV/Base des données ECVs")

# Load the datasets
bd2021vf <- read_excel("bd2021_vf.xlsx")
bd2022vf <- read_excel("bd2022_vf.xlsx")
bd2023vf <- read_excel("bd2023_vf.xlsx")

#2021 both bcg at any age and bcg_timely
bd_bcg_2021 <- bd2021vf %>%
  pivot_longer(cols = c(bcg, bcg_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
bcg_summary2021 <- bd_bcg_2021 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, bcg = "BCG vaccination at any age", bcg_timely = "BGC Timely vaccination")
  )

# Plot
ggplot(bcg_summary2021, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "BCG Vaccination Coverage and Timeliness by Province in 2021",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "BCG Coverage Type",
    shape = "BCG Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2022 both bcg at any age and bcg_timely
bd_bcg_2022 <- bd2022vf %>%
  pivot_longer(cols = c(bcg, bcg_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
bcg_summary2022 <- bd_bcg_2022 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, bcg = "BCG vaccination at any age", bcg_timely = "BGC Timely vaccination")
  )

# Plot
ggplot(bcg_summary2022, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "BCG Vaccination Coverage and Timeliness by Province in 2022",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "BCG Coverage Type",
    shape = "BCG Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2023 both bcg at any age and bcg_timely
bd_bcg_2023 <- bd2023vf %>%
  pivot_longer(cols = c(bcg, bcg_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
bcg_summary2023 <- bd_bcg_2023 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, bcg = "BCG vaccination at any age", bcg_timely = "BGC Timely vaccination")
  )

# Plot
ggplot(bcg_summary2023, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "BCG Vaccination Coverage and Timeliness by Province in 2023",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "BCG Coverage Type",
    shape = "BCG Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#############################################################################

#2021 both dtc_hepb_hib1 at any age and dtc_hepb_hib1_timely
bd_dtc_hepb_hib1_2021 <- bd2021vf %>%
  pivot_longer(cols = c(dtc_hepb_hib1, dtc_hepb_hib1_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
dtc_hepb_hib1_summary2021 <- bd_dtc_hepb_hib1_2021 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, dtc_hepb_hib1 = "PENTA-1 vaccination at any age", dtc_hepb_hib1_timely = "PENTA-1 Timely vaccination")
  )

# Plot
ggplot(dtc_hepb_hib1_summary2021, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "PENTA-1 Vaccination Coverage and Timeliness by Province in 2021",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "PENTA-1 Coverage Type",
    shape = "PENTA-1 Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2022 both dtc_hepb_hib1 at any age and dtc_hepb_hib1_timely
bd_dtc_hepb_hib1_2022 <- bd2022vf %>%
  pivot_longer(cols = c(dtc_hepb_hib1, dtc_hepb_hib1_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
dtc_hepb_hib1_summary2022 <- bd_dtc_hepb_hib1_2022 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, dtc_hepb_hib1 = "PENTA-1 vaccination at any age", dtc_hepb_hib1_timely = "PENTA-1 Timely vaccination")
  )

# Plot
ggplot(dtc_hepb_hib1_summary2022, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "PENTA-1 Vaccination Coverage and Timeliness by Province in 2022",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "PENTA-1 Coverage Type",
    shape = "PENTA-1 Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2023 both dtc_hepb_hib1 at any age and dtc_hepb_hib1_timely
bd_dtc_hepb_hib1_2023 <- bd2023vf %>%
  pivot_longer(cols = c(dtc_hepb_hib1, dtc_hepb_hib1_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
dtc_hepb_hib1_summary2023 <- bd_dtc_hepb_hib1_2023 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, dtc_hepb_hib1 = "PENTA-1 vaccination at any age", dtc_hepb_hib1_timely = "PENTA-1 Timely vaccination")
  )

# Plot
ggplot(dtc_hepb_hib1_summary2023, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "PENTA-1 Vaccination Coverage and Timeliness by Province in 2023",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "PENTA-1 Coverage Type",
    shape = "PENTA-1 Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

############################################################################

#2021 both dtc_hepb_hib3 at any age and dtc_hepb_hib3_timely
bd_dtc_hepb_hib3_2021 <- bd2021vf %>%
  pivot_longer(cols = c(dtc_hepb_hib3, dtc_hepb_hib3_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
dtc_hepb_hib3_summary2021 <- bd_dtc_hepb_hib3_2021 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, dtc_hepb_hib3 = "PENTA-3 vaccination at any age", dtc_hepb_hib3_timely = "PENTA-3 Timely vaccination")
  )

# Plot
ggplot(dtc_hepb_hib3_summary2021, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "PENTA-3 Vaccination Coverage and Timeliness by Province in 2021",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "PENTA-3 Coverage Type",
    shape = "PENTA-3 Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2022 both dtc_hepb_hib3 at any age and dtc_hepb_hib3_timely
bd_dtc_hepb_hib3_2022 <- bd2022vf %>%
  pivot_longer(cols = c(dtc_hepb_hib3, dtc_hepb_hib3_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
dtc_hepb_hib3_summary2022 <- bd_dtc_hepb_hib3_2022 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, dtc_hepb_hib3 = "PENTA-3 vaccination at any age", dtc_hepb_hib3_timely = "PENTA-3 Timely vaccination")
  )

# Plot
ggplot(dtc_hepb_hib3_summary2022, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "PENTA-3 Vaccination Coverage and Timeliness by Province in 2022",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "PENTA-3 Coverage Type",
    shape = "PENTA-3 Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2023 both dtc_hepb_hib3 at any age and dtc_hepb_hib3_timely
bd_dtc_hepb_hib3_2023 <- bd2023vf %>%
  pivot_longer(cols = c(dtc_hepb_hib3, dtc_hepb_hib3_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
dtc_hepb_hib3_summary2023 <- bd_dtc_hepb_hib3_2023 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, dtc_hepb_hib3 = "PENTA-3 vaccination at any age", dtc_hepb_hib3_timely = "PENTA-3 Timely vaccination")
  )

# Plot
ggplot(dtc_hepb_hib3_summary2023, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "PENTA-3 Vaccination Coverage and Timeliness by Province in 2023",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "PENTA-3 Coverage Type",
    shape = "PENTA-3 Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

##########################################################################

#2021 both var at any age and var_timely
bd_var_2021 <- bd2021vf %>%
  pivot_longer(cols = c(var, var_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
var_summary2021 <- bd_var_2021 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, var = "MCV vaccination at any age", var_timely = "MCV Timely vaccination")
  )
#TO BE EXPLAINED
table(bd2021vf$var)
table(bd2021vf$var_timely)

# Plot
ggplot(var_summary2021, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "MCV Vaccination Coverage and Timeliness by Province in 2021",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "MCV Coverage Type",
    shape = "MCV Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2022 both var at any age and var_timely
bd_var_2022 <- bd2022vf %>%
  pivot_longer(cols = c(var, var_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
var_summary2022 <- bd_var_2022 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, var = "MCV vaccination at any age", var_timely = "MCV Timely vaccination")
  )

# Plot
ggplot(var_summary2022, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "MCV Vaccination Coverage and Timeliness by Province in 2022",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "MCV Coverage Type",
    shape = "MCV Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2023 both var at any age and var_timely
bd_var_2023 <- bd2023vf %>%
  pivot_longer(cols = c(var, var_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
var_summary2023 <- bd_var_2023 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, var = "MCV vaccination at any age", var_timely = "MCV Timely vaccination")
  )

# Plot
ggplot(var_summary2023, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "MCV Vaccination Coverage and Timeliness by Province in 2023",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "MCV Coverage Type",
    shape = "MCV Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

###############################################################################

#2021 both complete_vaccination at any age and complete_vaccination_timely
bd_complete_vaccination_2021 <- bd2021vf %>%
  pivot_longer(cols = c(complete_vaccination, complete_vaccination_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
complete_vaccination_summary2021 <- bd_complete_vaccination_2021 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, complete_vaccination = "COMPLETE_VACCINATION at any age", complete_vaccination_timely = "COMPLETE_VACCINATION Timely")
  )

# Plot
ggplot(complete_vaccination_summary2021, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "COMPLETE_VACCINATION Coverage and Timeliness by Province in 2021",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "COMPLETE Coverage Type",
    shape = "COMPLETE Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2022 both complete_vaccination at any age and complete_vaccination_timely
bd_complete_vaccination_2022 <- bd2022vf %>%
  pivot_longer(cols = c(complete_vaccination, complete_vaccination_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
complete_vaccination_summary2022 <- bd_complete_vaccination_2022 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, complete_vaccination = "COMPLETE_VACCINATION at any age", complete_vaccination_timely = "COMPLETE_VACCINATION Timely")
  )

# Plot
ggplot(complete_vaccination_summary2022, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "COMPLETE_VACCINATION Coverage and Timeliness by Province in 2022",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "COMPLETE Coverage Type",
    shape = "COMPLETE Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

#2023 both complete_vaccination at any age and complete_vaccination_timely
bd_complete_vaccination_2023 <- bd2023vf %>%
  pivot_longer(cols = c(complete_vaccination, complete_vaccination_timely), names_to = "type", values_to = "status") %>%
  filter(status %in% c("Oui", "Non"))  # Keep only valid responses

# Summarize and calculate CI
complete_vaccination_summary2023 <- bd_complete_vaccination_2023 %>%
  group_by(province, type) %>%
  summarise(
    n_success = sum(status == "Oui", na.rm = TRUE),
    n_total = sum(status %in% c("Oui", "Non")),
    .groups = "drop"
  ) %>%
  mutate(
    prop_success = n_success / n_total,
    ci = purrr::pmap(list(n_success, n_total), ~ binom.confint(..1, ..2, method = "exact")),
    lower = purrr::map_dbl(ci, ~ .x$lower),
    upper = purrr::map_dbl(ci, ~ .x$upper),
    type = recode(type, complete_vaccination = "COMPLETE_VACCINATION at any age", complete_vaccination_timely = "COMPLETE_VACCINATION Timely")
  )

# Plot
ggplot(complete_vaccination_summary2023, aes(
  y = reorder(province, prop_success),
  x = prop_success,
  color = type,
  shape = type
)) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  ) +
  labs(
    title = "COMPLETE_VACCINATION Coverage and Timeliness by Province in 2023",
    x = "Proportion Vaccinated",
    y = "DRC's Provinces",
    color = "COMPLETE Coverage Type",
    shape = "COMPLETE Coverage Type",
    caption = "Dots and lines represent proportions with 95% exact binomial confidence intervals"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

##########################################################################
##########################################################################

#removing unecessary variables

bd2021vf <- bd2021vf %>% select(-c(knowledge_vdp,
                                   vpob0,
                                   vpob1,
                                   vpob2,
                                   vpob3,
                                   vpi,
                                   pneumo1,
                                   pneumo2,
                                   pneumo3,
                                   rotavirus1,
                                   rotavirus2,
                                   rotavirus3,
                                   dtc_hepb_hib2,
                                   vaa,
                                   qa504,
                                   notesec5,
                                   age_bcg_w,
                                   age_vpob0,
                                   age_vpob0_w,
                                   age_vaa,
                                   age_vaa_w,
                                   age_vaa_m,
                                   notesec4,
                                   date_vpob0,
                                   date_vpob0T,
                                   date_vpob1,
                                   date_vpob2,
                                   date_vpob3,
                                   date_pneumo1,
                                   date_pneumo2,
                                   date_pneumo3,
                                   date_rotavirus1,
                                   date_rotavirus2,
                                   date_rotavirus3,
                                   date_dtc_hepb_hib2,
                                   date_vaa,
                                   date_vaaT,
                                   qa50313_autre))

bd2022vf <- bd2022vf %>% select(-c(knowledge_vdp,
                                   vpob0,
                                   vpob1,
                                   vpob2,
                                   vpob3,
                                   vpi,
                                   pneumo1,
                                   pneumo2,
                                   pneumo3,
                                   rotavirus1,
                                   rotavirus2,
                                   rotavirus3,
                                   dtc_hepb_hib2,
                                   vaa,
                                   qa504,
                                   notesec5,
                                   age_bcg_w,
                                   age_vpob0,
                                   age_vpob0_w,
                                   age_vaa,
                                   age_vaa_w,
                                   age_vaa_m,
                                   notesec4,
                                   date_vpob0,
                                   date_vpob0T,
                                   date_vpob1,
                                   date_vpob2,
                                   date_vpob3,
                                   date_pneumo1,
                                   date_pneumo2,
                                   date_pneumo3,
                                   date_rotavirus1,
                                   date_rotavirus2,
                                   date_rotavirus3,
                                   date_dtc_hepb_hib2,
                                   date_vaa,
                                   date_vaaT,
                                   qa50313_autre))

bd2023vf <- bd2023vf %>% select(-c(knowledge_vdp,
                                   vpob0,
                                   vpob1,
                                   vpob2,
                                   vpob3,
                                   vpi,
                                   pneumo1,
                                   pneumo2,
                                   pneumo3,
                                   rotavirus1,
                                   rotavirus2,
                                   rotavirus3,
                                   dtc_hepb_hib2,
                                   vaa,
                                   qa504,
                                   notesec5,
                                   age_bcg_w,
                                   age_vpob0,
                                   age_vpob0_w,
                                   age_vaa,
                                   age_vaa_w,
                                   age_vaa_m,
                                   notesec4,
                                   date_vpob0,
                                   date_vpob0T,
                                   date_vpob1,
                                   date_vpob2,
                                   date_vpob3,
                                   date_pneumo1,
                                   date_pneumo2,
                                   date_pneumo3,
                                   date_rotavirus1,
                                   date_rotavirus2,
                                   date_rotavirus3,
                                   date_dtc_hepb_hib2,
                                   date_vaa,
                                   date_vaaT,
                                   qa50313_autre))

bd2021vf <- bd2021vf %>% select(-c(calc1,calc2))
bd2022vf <- bd2022vf %>% select(-c(calc1,calc2))
bd2023vf <- bd2023vf %>% select(-c(calc1,calc2))

write_xlsx(bd2021vf, "bd2021_vf.xlsx")
write_xlsx(bd2022vf, "bd2022_vf.xlsx")
write_xlsx(bd2023vf, "bd2023_vf.xlsx")


# List of categorical variables

table(bd2021vf$child_sex)
table(bd2021vf$nutritional_status)
table(bd2021vf$child_civil_registration)
table(bd2021vf$age_mother_cat)
table(bd2021vf$education_mother)
table(bd2021vf$religion)
table(bd2021vf$marital_status)
table(bd2021vf$area_residency)
table(bd2021vf$using_telephone)
table(bd2021vf$paid_vaccination)
table(bd2021vf$province)
table(bd2021vf$healthzone)
table(bd2021vf$children_u5_cat)
table(bd2021vf$knowledge_cat)
table(bd2021vf$television)
table(bd2021vf$radio)
table(bd2021vf$wealth)

summary(bd2021vf$age_var_m)



bd2021vf$complete_vaccination[bd2021vf$complete_vaccination=="Oui"]<-"Yes"
bd2021vf$complete_vaccination[bd2021vf$complete_vaccination=="Non"]<-"No"
bd2021vf$complete_vaccination[bd2021vf$complete_vaccination=="NSP"]<-"No"

bd2022vf$complete_vaccination[bd2022vf$complete_vaccination=="Oui"]<-"Yes"
bd2022vf$complete_vaccination[bd2022vf$complete_vaccination=="Non"]<-"No"
bd2022vf$complete_vaccination[bd2022vf$complete_vaccination=="NSP"]<-"No"

bd2023vf$complete_vaccination[bd2023vf$complete_vaccination=="Oui"]<-"Yes"
bd2023vf$complete_vaccination[bd2023vf$complete_vaccination=="Non"]<-"No"
bd2023vf$complete_vaccination[bd2023vf$complete_vaccination=="NSP"]<-"No"

class(bd2021vf$complete_vaccination)
class(bd2022vf$complete_vaccination)
class(bd2023vf$complete_vaccination)

bd2021vf$complete_vaccination<-as.factor(bd2021vf$complete_vaccination)
bd2022vf$complete_vaccination<-as.factor(bd2022vf$complete_vaccination)
bd2023vf$complete_vaccination<-as.factor(bd2023vf$complete_vaccination)

table(bd2021vf$complete_vaccination)
table(bd2022vf$complete_vaccination)
table(bd2023vf$complete_vaccination)

#####################################################################

#RECODING VARIABLES & BIVARIATES ANALYSIS

#2021
table(bd2021vf$wealth)
table(bd2022vf$wealth)
table(bd2023vf$wealth)

table(bd2021vf$child_sex)
bd2021vf$child_sex[bd2021vf$child_sex=="feminin"]<-"Female"
bd2021vf$child_sex[bd2021vf$child_sex=="masculin"]<-"Male"

table(bd2021vf$nutritional_status)
bd2021vf$nutritional_status[bd2021vf$nutritional_status=="115 to 124 cm"]<-"PB<125cm"
bd2021vf$nutritional_status[bd2021vf$nutritional_status=="below 115 cm"]<-"PB<125cm"
bd2021vf$nutritional_status[bd2021vf$nutritional_status=="125 cm and above"]<-"PB>=125cm"

table(bd2021vf$child_civil_registration)
bd2021vf$child_civil_registration[bd2021vf$child_civil_registration=="Non"]<-"No"
bd2021vf$child_civil_registration[bd2021vf$child_civil_registration=="NSP"]<-"No"
bd2021vf$child_civil_registration[bd2021vf$child_civil_registration=="Oui"]<-"Yes"

table(bd2021vf$age_mother_cat)
bd2021vf$age_mother_cat[bd2021vf$age_mother_cat=="below 20 years"]<-"< 20yrs"
bd2021vf$age_mother_cat[bd2021vf$age_mother_cat=="35 years +"]<-">= 20yrs"
bd2021vf$age_mother_cat[bd2021vf$age_mother_cat=="20-34 years"]<-">= 20yrs"

table(bd2021vf$education_mother)
bd2021vf$education_mother[bd2021vf$education_mother=="N’a jamais été à l’école"]<-"No School/primary"
bd2021vf$education_mother[bd2021vf$education_mother=="Primaire"]<-"No School/primary"
bd2021vf$education_mother[bd2021vf$education_mother=="Ne sait pas"]<-"No School/primary"
bd2021vf$education_mother[bd2021vf$education_mother=="Non-réponse"]<-"No School/primary"
bd2021vf$education_mother[bd2021vf$education_mother=="Secondaire"]<-"Secondary/tertiary"
bd2021vf$education_mother[bd2021vf$education_mother=="Supérieur"]<-"Secondary/tertiary"

table(bd2021vf$religion)
bd2021vf$religion[bd2021vf$religion=="Autre religion (à préciser)"]<-"Others"
bd2021vf$religion[bd2021vf$religion=="Pas de religion"]<-"Don't pray"
bd2021vf$religion[bd2021vf$religion=="Ne sait pas"]<-"Don't pray"
bd2021vf$religion[bd2021vf$religion=="Pas de réponse"]<-"Don't pray"
bd2021vf$religion[bd2021vf$religion=="Musulmane"]<-"Islam"
bd2021vf$religion[bd2021vf$religion=="Eglise de réveil/indépendante"]<-"Protestant/Revival"
bd2021vf$religion[bd2021vf$religion=="Protestante"]<-"Protestant/Revival"
bd2021vf$religion[bd2021vf$religion=="Catholique"]<-"Catholic"
bd2021vf$religion[bd2021vf$religion=="Others"]<-"Traditional/Kimbaguist/others"
bd2021vf$religion[bd2021vf$religion=="Traditional/Kimbaguist"]<-"Traditional/Kimbaguist/others"


table(bd2021vf$marital_status)
bd2021vf$marital_status[bd2021vf$marital_status=="Mariée"]<-"Married/Common-law"
bd2021vf$marital_status[bd2021vf$marital_status=="Union de fait"]<-"Married/Common-law"
bd2021vf$marital_status[bd2021vf$marital_status=="Célibataire"]<-"Single/separated"
bd2021vf$marital_status[bd2021vf$marital_status=="Séparée"]<-"Single/separated"
bd2021vf$marital_status[bd2021vf$marital_status=="Divorcé"]<-"Divorced/Widow"
bd2021vf$marital_status[bd2021vf$marital_status=="Veuve"]<-"Divorced/Widow"

table(bd2021vf$area_residency)
bd2021vf$area_residency[bd2021vf$area_residency=="Rurale"]<-"Rural"
bd2021vf$area_residency[bd2021vf$area_residency=="Urbain"]<-"Urbain"

table(bd2021vf$using_telephone)
bd2021vf$using_telephone[bd2021vf$using_telephone=="non"]<-"No"
bd2021vf$using_telephone[bd2021vf$using_telephone=="oui"]<-"Yes"

table(bd2021vf$paid_vaccination)
bd2021vf$paid_vaccination[bd2021vf$paid_vaccination=="Non"]<-"No"
bd2021vf$paid_vaccination[bd2021vf$paid_vaccination=="NSP"]<-"No"
bd2021vf$paid_vaccination[bd2021vf$paid_vaccination=="Oui"]<-"Yes"

#2022
table(bd2022vf$child_sex)
bd2022vf$child_sex[bd2022vf$child_sex=="feminin"]<-"Female"
bd2022vf$child_sex[bd2022vf$child_sex=="masculin"]<-"Male"

table(bd2022vf$nutritional_status)
bd2022vf$nutritional_status[bd2022vf$nutritional_status=="115 to 124 cm"]<-"PB<125cm"
bd2022vf$nutritional_status[bd2022vf$nutritional_status=="below 115 cm"]<-"PB<125cm"
bd2022vf$nutritional_status[bd2022vf$nutritional_status=="125 cm and above"]<-"PB>=125cm"

table(bd2022vf$child_civil_registration)
bd2022vf$child_civil_registration[bd2022vf$child_civil_registration=="Non"]<-"No"
bd2022vf$child_civil_registration[bd2022vf$child_civil_registration=="NSP"]<-"No"
bd2022vf$child_civil_registration[bd2022vf$child_civil_registration=="Oui"]<-"Yes"

table(bd2022vf$age_mother_cat)
bd2022vf$age_mother_cat[bd2022vf$age_mother_cat=="below 20 years"]<-"< 20yrs"
bd2022vf$age_mother_cat[bd2022vf$age_mother_cat=="35 years +"]<-">= 20yrs"
bd2022vf$age_mother_cat[bd2022vf$age_mother_cat=="20-34 years"]<-">= 20yrs"

table(bd2022vf$education_mother)
bd2022vf$education_mother[bd2022vf$education_mother=="N’a jamais été à l’école"]<-"No School/primary"
bd2022vf$education_mother[bd2022vf$education_mother=="Primaire"]<-"No School/primary"
bd2022vf$education_mother[bd2022vf$education_mother=="Ne sait pas"]<-"No School/primary"
bd2022vf$education_mother[bd2022vf$education_mother=="Non-réponse"]<-"No School/primary"
bd2022vf$education_mother[bd2022vf$education_mother=="Secondaire"]<-"Secondary/tertiary"
bd2022vf$education_mother[bd2022vf$education_mother=="Supérieur"]<-"Secondary/tertiary"

table(bd2022vf$religion)
bd2022vf$religion[bd2022vf$religion=="Autre religion (à préciser)"]<-"Others"
bd2022vf$religion[bd2022vf$religion=="Pas de religion"]<-"Don't pray"
bd2022vf$religion[bd2022vf$religion=="Ne sait pas"]<-"Don't pray"
bd2022vf$religion[bd2022vf$religion=="Pas de réponse"]<-"Don't pray"
bd2022vf$religion[bd2022vf$religion=="Musulmane"]<-"Islam"
bd2022vf$religion[bd2022vf$religion=="Eglise de réveil/indépendante"]<-"Protestant/Revival"
bd2022vf$religion[bd2022vf$religion=="Protestante"]<-"Protestant/Revival"
bd2022vf$religion[bd2022vf$religion=="Catholique"]<-"Catholic"
bd2022vf$religion[bd2022vf$religion=="Others"]<-"Traditional/Kimbaguist/others"
bd2022vf$religion[bd2022vf$religion=="Traditional/Kimbaguist"]<-"Traditional/Kimbaguist/others"

table(bd2022vf$marital_status)
bd2022vf$marital_status[bd2022vf$marital_status=="Mariée"]<-"Married/Common-law"
bd2022vf$marital_status[bd2022vf$marital_status=="Union de fait"]<-"Married/Common-law"
bd2022vf$marital_status[bd2022vf$marital_status=="Célibataire"]<-"Single/separated"
bd2022vf$marital_status[bd2022vf$marital_status=="Séparée"]<-"Single/separated"
bd2022vf$marital_status[bd2022vf$marital_status=="Divorcé"]<-"Divorced/Widow"
bd2022vf$marital_status[bd2022vf$marital_status=="Veuve"]<-"Divorced/Widow"

table(bd2022vf$area_residency)
bd2022vf$area_residency[bd2022vf$area_residency=="Rurale"]<-"Rural"
bd2022vf$area_residency[bd2022vf$area_residency=="Urbain"]<-"Urbain"

table(bd2022vf$using_telephone)
bd2022vf$using_telephone[bd2022vf$using_telephone=="0"]<-"No"
bd2022vf$using_telephone[bd2022vf$using_telephone=="oui"]<-"Yes"

table(bd2022vf$paid_vaccination)
bd2022vf$paid_vaccination[bd2022vf$paid_vaccination=="Non"]<-"No"
bd2022vf$paid_vaccination[bd2022vf$paid_vaccination=="NSP"]<-"No"
bd2022vf$paid_vaccination[bd2022vf$paid_vaccination=="Oui"]<-"Yes"

#2023
table(bd2023vf$child_sex)
bd2023vf$child_sex[bd2023vf$child_sex=="Féminin"]<-"Female"
bd2023vf$child_sex[bd2023vf$child_sex=="Masculin"]<-"Male"

table(bd2023vf$nutritional_status)
bd2023vf$nutritional_status[bd2023vf$nutritional_status=="115 to 124 cm"]<-"PB<125cm"
bd2023vf$nutritional_status[bd2023vf$nutritional_status=="below 115 cm"]<-"PB<125cm"
bd2023vf$nutritional_status[bd2023vf$nutritional_status=="125 cm and above"]<-"PB>=125cm"

table(bd2023vf$child_civil_registration)
bd2023vf$child_civil_registration[bd2023vf$child_civil_registration=="Non"]<-"No"
bd2023vf$child_civil_registration[bd2023vf$child_civil_registration=="NSP"]<-"No"
bd2023vf$child_civil_registration[bd2023vf$child_civil_registration=="Oui"]<-"Yes"

table(bd2023vf$age_mother_cat)
bd2023vf$age_mother_cat[bd2023vf$age_mother_cat=="below 20 years"]<-"< 20yrs"
bd2023vf$age_mother_cat[bd2023vf$age_mother_cat=="35 years +"]<-">= 20yrs"
bd2023vf$age_mother_cat[bd2023vf$age_mother_cat=="20-34 years"]<-">= 20yrs"

table(bd2023vf$education_mother)
bd2023vf$education_mother[bd2023vf$education_mother=="N’a jamais été à l’école"]<-"No School/primary"
bd2023vf$education_mother[bd2023vf$education_mother=="Primaire"]<-"No School/primary"
bd2023vf$education_mother[bd2023vf$education_mother=="Ne sait pas"]<-"No School/primary"
bd2023vf$education_mother[bd2023vf$education_mother=="Non-réponse"]<-"No School/primary"
bd2023vf$education_mother[bd2023vf$education_mother=="Secondaire"]<-"Secondary/tertiary"
bd2023vf$education_mother[bd2023vf$education_mother=="Supérieur"]<-"Secondary/tertiary"

table(bd2023vf$religion)
bd2023vf$religion[bd2023vf$religion=="Autre religion (à préciser)"]<-"Others"
bd2023vf$religion[bd2023vf$religion=="Pas de religion"]<-"Don't pray"
bd2023vf$religion[bd2023vf$religion=="Ne sait pas"]<-"Don't pray"
bd2023vf$religion[bd2023vf$religion=="Pas de réponse"]<-"Don't pray"
bd2023vf$religion[bd2023vf$religion=="Musulmane"]<-"Islam"
bd2023vf$religion[bd2023vf$religion=="Eglise de réveil/indépendante"]<-"Protestant/Revival"
bd2023vf$religion[bd2023vf$religion=="Protestante"]<-"Protestant/Revival"
bd2023vf$religion[bd2023vf$religion=="Catholique"]<-"Catholic"
bd2023vf$religion[bd2023vf$religion=="Others"]<-"Traditional/Kimbaguist/others"
bd2023vf$religion[bd2023vf$religion=="Traditional/Kimbaguist"]<-"Traditional/Kimbaguist/others"

table(bd2023vf$marital_status)
bd2023vf$marital_status[bd2023vf$marital_status=="Marié(e)"]<-"Married/Common-law"
bd2023vf$marital_status[bd2023vf$marital_status=="Union de fait"]<-"Married/Common-law"
bd2023vf$marital_status[bd2023vf$marital_status=="Célibataire"]<-"Single/separated"
bd2023vf$marital_status[bd2023vf$marital_status=="Séparé(e)"]<-"Single/separated"
bd2023vf$marital_status[bd2023vf$marital_status=="Divorcé(e)"]<-"Divorced/Widow"
bd2023vf$marital_status[bd2023vf$marital_status=="Veuve"]<-"Divorced/Widow"

table(bd2023vf$area_residency)
bd2023vf$area_residency[bd2023vf$area_residency=="Rurale"]<-"Rural"
bd2023vf$area_residency[bd2023vf$area_residency=="Urbain"]<-"Urbain"

table(bd2023vf$using_telephone)
bd2023vf$using_telephone[bd2023vf$using_telephone=="0"]<-"No"
bd2023vf$using_telephone[bd2023vf$using_telephone=="Oui"]<-"Yes"

table(bd2023vf$paid_vaccination)
bd2023vf$paid_vaccination[bd2023vf$paid_vaccination=="Non"]<-"No"
bd2023vf$paid_vaccination[bd2023vf$paid_vaccination=="NSP"]<-"No"
bd2023vf$paid_vaccination[bd2023vf$paid_vaccination=="Oui"]<-"Yes"

table(bd2021vf$complete_vaccination)
bd2021vf$complete_vaccination[bd2021vf$complete_vaccination=="1"]<-"No"
bd2021vf$complete_vaccination[bd2021vf$complete_vaccination=="2"]<-"Yes"

table(bd2022vf$complete_vaccination)
bd2022vf$complete_vaccination[bd2022vf$complete_vaccination=="0"]<-"No"
bd2022vf$complete_vaccination[bd2022vf$complete_vaccination=="1"]<-"Yes"

table(bd2023vf$complete_vaccination)
bd2023vf$complete_vaccination[bd2023vf$complete_vaccination=="0"]<-"No"
bd2023vf$complete_vaccination[bd2023vf$complete_vaccination=="1"]<-"Yes"


##########################################################################
#create a xlsx_file for the comparison 
library(writexl)
library(readxl)

#export 3 datasets
write_xlsx(bd2021vf, "bd2021_vf.xlsx")
write_xlsx(bd2022vf, "bd2022_vf.xlsx")
write_xlsx(bd2023vf, "bd2023_vf.xlsx")

#import all 3 datasets
bd2021vf<-read_excel("bd2021_vf.xlsx")
bd2022vf<-read_excel("bd2022_vf.xlsx")
bd2023vf<-read_excel("bd2023_vf.xlsx")


#Descriptive Analysis

library(dplyr)
library(DescTools)
library(tidyr)
library(purrr)
library(gtsummary)

vars <- c("child_sex",
          "wealth",
          "television",
          "radio",
          "nutritional_status",
          "child_civil_registration",
          "children_u5_cat",
          "age_mother_cat", 
          "education_mother",
          "knowledge_cat",
          "religion", 
          "marital_status",
          "area_residency", 
          "using_telephone", 
          "paid_vaccination",
          "province")

setdiff(vars, names(bd2021vf))

# Function to calculate proportion and 95% CI
get_prop_ci <- function(varname, data) {
  tab <- table(data[[varname]])
  total <- sum(tab)
  
  result <- data.frame(
    variable = varname,
    value = names(tab),
    n = as.integer(tab),
    proportion = as.numeric(tab) / total
  )
  
  # Calculate Wilson 95% CI
  ci <- BinomCI(x = result$n, n = total, conf.level = 0.95, method = "wilson")
  
  result$lwr.ci <- ci[, "lwr.ci"]
  result$upr.ci <- ci[, "upr.ci"]
  
  return(result)
}

# Apply to all variables
descriptive2021 <- map_dfr(vars, ~ get_prop_ci(.x, bd2021vf))
descriptive2022 <- map_dfr(vars, ~ get_prop_ci(.x, bd2022vf))
descriptive2023 <- map_dfr(vars, ~ get_prop_ci(.x, bd2023vf))


# View the table
print(descriptive2021)
print(descriptive2022)
print(descriptive2023)

#Export
write_xlsx(descriptive2021,"Descriptive2021b.xlsx")
write_xlsx(descriptive2022,"Descriptive2022b.xlsx")
write_xlsx(descriptive2023,"Descriptive2023b.xlsx")

summary(bd2021vf$age_mother)
summary(bd2022vf$age_mother)
summary(bd2023vf$age_mother)

#NORMALITY DISTRIBUTION TEST
hist(bd2021vf$age_mother, breaks = 30, probability = TRUE,
     main = "Histogram of Mother's Age", xlab = "Age")
curve(dnorm(x, mean = mean(bd2021vf$age_mother, na.rm = TRUE),
            sd = sd(bd2021vf$age_mother, na.rm = TRUE)),
      col = "red", lwd = 2, add = TRUE)

hist(bd2022vf$age_mother, breaks = 30, probability = TRUE,
     main = "Histogram of Mother's Age", xlab = "Age")
curve(dnorm(x, mean = mean(bd2022vf$age_mother, na.rm = TRUE),
            sd = sd(bd2022vf$age_mother, na.rm = TRUE)),
      col = "red", lwd = 2, add = TRUE)

hist(bd2023vf$age_mother, breaks = 30, probability = TRUE,
     main = "Histogram of Mother's Age", xlab = "Age")
curve(dnorm(x, mean = mean(bd2023vf$age_mother, na.rm = TRUE),
            sd = sd(bd2023vf$age_mother, na.rm = TRUE)),
      col = "red", lwd = 2, add = TRUE)

#KHI2 SQUARE

# Define outcome and predictor variables
outcome <- "complete_vaccination"
predictors <- c("child_sex",
                "wealth",
                "television",
                "radio",
                "nutritional_status",
                "child_civil_registration",
                "children_u5_cat",
                "age_mother_cat", 
                "education_mother",
                "knowledge_cat",
                "religion", 
                "marital_status",
                "area_residency", 
                "using_telephone", 
                "paid_vaccination",
                "province")

# Loop through each predictor and run chi-square test
#2021
Khi2_2021 <- data.frame(
  Variable = character(),
  ChiSq = numeric(),
  df = integer(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in predictors) {
  tbl <- table(bd2021vf[[outcome]], bd2021vf[[var]])
  if (all(dim(tbl) > 1)) {
    test <- chisq.test(tbl)
    Khi2_2021 <- rbind(Khi2_2021, data.frame(
      Variable = var,
      ChiSq = round(test$statistic, 2),
      df = test$parameter,
      p_value = round(test$p.value, 4)
    ))
  }
}
#2022
Khi2_2022 <- data.frame(
  Variable = character(),
  ChiSq = numeric(),
  df = integer(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in predictors) {
  tbl <- table(bd2022vf[[outcome]], bd2022vf[[var]])
  if (all(dim(tbl) > 1)) {
    test <- chisq.test(tbl)
    Khi2_2022 <- rbind(Khi2_2022, data.frame(
      Variable = var,
      ChiSq = round(test$statistic, 2),
      df = test$parameter,
      p_value = round(test$p.value, 4)
    ))
  }
}
#2023
Khi2_2023 <- data.frame(
  Variable = character(),
  ChiSq = numeric(),
  df = integer(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in predictors) {
  tbl <- table(bd2023vf[[outcome]], bd2023vf[[var]])
  if (all(dim(tbl) > 1)) {
    test <- chisq.test(tbl)
    Khi2_2023 <- rbind(Khi2_2023, data.frame(
      Variable = var,
      ChiSq = round(test$statistic, 2),
      df = test$parameter,
      p_value = round(test$p.value, 4)
    ))
  }
}

print(Khi2_2021)
print(Khi2_2022)
print(Khi2_2023)

write_xlsx(Khi2_2021,"Khi2_2021b.xlsx")
write_xlsx(Khi2_2022,"Khi2_2022b.xlsx")
write_xlsx(Khi2_2023,"Khi2_2023b.xlsx")

#############################################################################

############ COX REGRESSION #################################################

library(coxme)
library(survival)

#2021
table(bd2021vf$complete_vaccination)
class(bd2021vf$complete_vaccination)

class(bd2021vf$age_var_m)

bd2021vf$complete_vaccination[bd2021vf$complete_vaccination=="Yes"]<-1
bd2021vf$complete_vaccination[bd2021vf$complete_vaccination=="No"]<-0
bd2021vf$complete_vaccination<-as.numeric(bd2021vf$complete_vaccination)

# Create a survival object
surv_obj2021 <- Surv(time = bd2021vf$age_var_m, event = bd2021vf$complete_vaccination)

# Fit the mixed-effects Cox model using coxme

coxme_model2021 <- coxme(
  surv_obj2021 ~ wealth+nutritional_status +children_u5_cat+knowledge_cat+
    child_civil_registration + age_mother_cat + education_mother + religion +
    area_residency + using_telephone + paid_vaccination +television+radio+
    (1 | province/healthzone),
  data = bd2021vf
)
summary(coxme_model2021)

#2022
table(bd2022vf$complete_vaccination)
class(bd2022vf$complete_vaccination)

class(bd2022vf$age_var_m)

bd2022vf$complete_vaccination[bd2022vf$complete_vaccination=="Yes"]<-1
bd2022vf$complete_vaccination[bd2022vf$complete_vaccination=="No"]<-0
bd2022vf$complete_vaccination<-as.numeric(bd2022vf$complete_vaccination)

# Create a survival object
surv_obj2022 <- Surv(time = bd2022vf$age_var_m, event = bd2022vf$complete_vaccination)

# Fit the mixed-effects Cox model using coxme

coxme_model2022 <- coxme(
  surv_obj2022 ~ wealth+nutritional_status +children_u5_cat+knowledge_cat+
    child_civil_registration + age_mother_cat + education_mother + religion +
    marital_status + area_residency + using_telephone + paid_vaccination +television+radio+
    (1 | province/healthzone),
  data = bd2022vf
)
summary(coxme_model2022)

#2023
table(bd2023vf$complete_vaccination)
class(bd2023vf$complete_vaccination)

class(bd2023vf$age_var_m)

bd2023vf$complete_vaccination[bd2023vf$complete_vaccination=="Yes"]<-1
bd2023vf$complete_vaccination[bd2023vf$complete_vaccination=="No"]<-0
bd2023vf$complete_vaccination<-as.numeric(bd2023vf$complete_vaccination)

# Create a survival object
surv_obj2023 <- Surv(time = bd2023vf$age_var_m, event = bd2023vf$complete_vaccination)

# Fit the mixed-effects Cox model using coxme

coxme_model2023 <- coxme(
  surv_obj2023 ~ wealth+child_civil_registration + age_mother_cat + education_mother + religion +children_u5_cat+knowledge_cat+
    area_residency + using_telephone + paid_vaccination +television+radio+
    (1 | province/healthzone),
  data = bd2023vf
)
summary(coxme_model2023)

#######################################################################

####### COMPARING STANDARD TO MIXED EFFECT COX-MODELS #################

library(survival)
library(coxme)

# Fit both models
#2021
cox_fixed2021 <- coxph(surv_obj2021 ~ nutritional_status + child_civil_registration + age_mother_cat + 
                         education_mother + religion + area_residency + 
                         using_telephone + paid_vaccination, 
                       data = bd2021vf)

cox_mixed2021 <- coxme(surv_obj2021 ~ nutritional_status + child_civil_registration + age_mother_cat + 
                         education_mother + religion + area_residency + 
                         using_telephone + paid_vaccination + 
                         (1 | province/healthzone), 
                       data = bd2021vf)

#2022
cox_fixed2022 <- coxph(surv_obj2022 ~ nutritional_status + child_civil_registration + age_mother_cat + 
                         education_mother + religion + area_residency + 
                         marital_status + using_telephone + paid_vaccination, 
                       data = bd2022vf)

cox_mixed2022 <- coxme(surv_obj2022 ~ nutritional_status + child_civil_registration + age_mother_cat + 
                         education_mother + religion + area_residency + 
                         marital_status + using_telephone + paid_vaccination + 
                         (1 | province/healthzone), 
                       data = bd2022vf)

#2023
cox_fixed2023 <- coxph(surv_obj2023 ~ child_civil_registration + age_mother_cat + 
                     education_mother + religion + area_residency + 
                     using_telephone + paid_vaccination, 
                   data = bd2023vf)

cox_mixed2023 <- coxme(surv_obj2023 ~ child_civil_registration + age_mother_cat + 
                     education_mother + religion + area_residency + 
                     using_telephone + paid_vaccination + 
                     (1 | province/healthzone), 
                   data = bd2023vf)

# Likelihood Ratio Test
model_fiting2021<-anova(cox_fixed2021, cox_mixed2021)
model_fiting2022<-anova(cox_fixed2022, cox_mixed2022)
model_fiting2023<-anova(cox_fixed2023, cox_mixed2023)

model_fiting_All<-list(model_fiting2021,model_fiting2022,model_fiting2023)

write_xlsx(model_fiting_All, "model_fiting_All.xlsx")


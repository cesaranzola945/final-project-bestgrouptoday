
rm(list = ls())
cat("\014")

# ------------------------Data Process-----------------
# authors: Cesar Anzola and Ahyaan

# Note: this code transform the data set from Refinitiv to be used for the 
# project sections

# call the libraries
library(tidyverse)
library(lubridate)
library(readxl)
# set path

setwd("/Users/Agha/Documents/GitHub/final-project-bestgrouptoday")
#setwd("D:/Users/ASUS/Documents/GitHub/Data and programing for PP 2/Final Project")

dates_refinitiv <- c(2021:2017)

dataset <- read_xlsx("Data/esg_public_energy.xlsx")

# Change column names for pivoting the dataset:
names1 <- colnames(dataset)
names2 <- as.character(dataset[1,])
pos    <- sum(is.na(names2))

# variables available and dates vector
vars   <- str_subset(names1,"[a-zA-Z]")[-c(1:pos)]
nvars  <- length(vars)
dvec   <- str_c(rep(str_c("vars", c(1:nvars),"_"), each = 5), dates_refinitiv)

# combine both names to build data frame names
names <- c(names1[1:pos], dvec)

# subset data set 
dataset <- dataset[-c(1:2),]

# rename columns
colnames(dataset) <- names

# pivot longer the data set
dat_long <- 
dataset %>% 
  pivot_longer(cols = dvec,names_to =  c("var","year"),names_sep = "_",values_to = "value")

# pivot wider the dataset
data_fin <-
dat_long %>% 
  pivot_wider(names_from = c("var"), values_from = "value")

var_list <- str_c("vars", c(1:nvars))

# make sure all variables are numeric

data_fin$year <- as.double(data_fin$year)

data_fin[,var_list] <- lapply(data_fin[,var_list] , as.numeric)

# clear memory
rm(dat_long, dataset)

# rename variables with short names
dict <- read_xlsx("Data/dictionary.xlsx")

colnames(data_fin) <- dict$variable

# rename some countries
data_fin <-
  data_fin %>% 
  mutate(country = ifelse(country == "United States of America" ,"United States", country),
         country = ifelse(country == "Korea; Republic (S. Korea)" ,"South Korea", country))

# save database
save(data_fin, file = "Data/data_refinitiv.Rda")

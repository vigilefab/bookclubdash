#*****************************************
#*** BULILIT BOOKDASH 
#*** Author: Vigile Fabella
#*** Date: Aug 2025
#*****************************************





#*****************************************
#----- preliminaries -----
#*****************************************
# deactivate scientific notation
options(scipen=999)


# set wd
setwd("C:/Users/user/OneDrive/Personal/Kada/bookclubdash")


# setup libraries
library(lubridate)
library(dplyr)
library(magrittr)
library(data.table)
library(googlesheets4)
library(tidyr)
library(stopwords)
library(ggwordcloud)
library(tidytext)
library(stringr)
library(rvest)
library(polite)




# clear workspace
rm(list = ls())






#*****************************************
#----- import dataset -----
#*****************************************

data <- read_sheet("https://docs.google.com/spreadsheets/d/120Refw_a4SNzoUTlnp7KSYnqoN1J5n1bN9_h3m_b8ZY",
                             sheet = "books") 
# %>% 
#   filter(!is.na(Author)) %>%
#   select(c("Title", "Author", "Date", "G", "V", "B", "F", "Recommended\n by", "Genre")) %>%
#   rename("Recommender" = "Recommended\n by")
# 









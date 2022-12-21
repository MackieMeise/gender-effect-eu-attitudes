# load packages ####
library(readstata13)
library(tidyverse)
library(dplyr)
library(stringr)
library(forcats)

# load data ####
rawdata <- read.dta13("data/eb94.dta")

# select variables ####
df <- select(rawdata, c("country", "d10", "d11", "d11r1", "qa6b_8", "qa6b_10", "qb6_6", "d1r2", "d7", "d8r2", "d78", "sd18b", "eu27b"))

# filter df for eu countries ####
df <- filter(df, eu27b=="EU27b (No UK)")

# rename columns ####
colnames(df) <- c("country", "gender", "age.exact", "age.grouped", "trust.nat", "trust.eu", "eu.enlar", "pol.place","hh.comp", "educ.grouped", "eu.image", "sat.demo.eu", "cntry.group")

# clean up country data ####
df$country <- word(df$country, 1, sep=" - ")

#clean values for DE and ES
df$country <- fct_collapse(df$country, DE = c("DE-W", "DE-E Germany East"), ES=c("ES -Spain"))

# prepare DVs ####
df <- df %>%
  mutate(trust.eu.dk=ifelse(trust.eu=="Don't know (SPONTANEOUS)",0,1),
         trust.eu=ifelse(trust.eu=="Don't know (SPONTANEOUS)",0, ifelse(trust.eu=="Tend to trust",1,0)),
         trust.nat.dk=ifelse(trust.nat=="Don't know (SPONTANEOUS)",0,1),
         trust.nat=ifelse(trust.nat=="Don't know (SPONTANEOUS)",0, ifelse(trust.nat=="Tend to trust",1,0)),
         eu.enlar.dk=ifelse(eu.enlar=="Don't know (SPONTANEOUS)",0,1),
         eu.enlar=ifelse(eu.enlar=="For",1, ifelse(eu.enlar=="Refusal",NA,0)),
         eu.image.dk=ifelse(eu.image=="DK (SPONT.)",0,1),
         eu.image=ifelse(eu.image=="Fairly positive",4,ifelse(eu.image=="Neutral",3,ifelse(eu.image=="Fairly negative",2,ifelse(eu.image=="Very positive",5, ifelse(eu.image=="Very negative", 1,0))))),
         sat.demo.eu.dk=ifelse(sat.demo.eu=="Don't know (SPONT.)",0,1),
         sat.demo.eu=ifelse(sat.demo.eu=="Fairly satisfied",3,ifelse(sat.demo.eu=="Very satisfied",4,ifelse(sat.demo.eu=="Not very satisfied",2,ifelse(sat.demo.eu=="Not at all satisfied",1, 0)))))

# include macro-level data ####
macrodata <- read.csv("data/genderindex.csv", sep=";")

# match macro-level data w/ individual data ####
df <- left_join(df, macrodata)

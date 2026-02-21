setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

# Core packages
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(stringr)
library(lubridate)
library(randomForest)
library(styler)
library(splitstackshape)
library(sos)
library(readr)
library(anytime)
library(styler)
library(openxlsx)
library(data.table)

load("ur_all.rda")
data <- data %>%
  select(-CBG,-Spec,-tipe)
#========================================================================
P560ranap <- data %>%
  subset(Nmtkp == "RITL") %>%
  subset(CMG == "N")

P560ranap <- P560ranap %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
P560ranap <- P560ranap %>% mutate(Diagsekunder = na_if(Diagsekunder, ""))
P560ranap <- P560ranap %>% mutate(Procedure = na_if(Procedure, "-"))
P560ranap <- P560ranap %>% mutate(Procedure = na_if(Procedure, ""))



a099ranap$Flag_1 <- with(a099ranap,
                         ifelse(Kddiagprimer == "A099", "Utama","Penyulit"))

P560ranap <- P560ranap %>%
  mutate(
    Kddiagprimer1 = case_when(
      Kddiagprimer == "A099" ~ NA,
      Kddiagprimer == "Z014" ~ NA,
      Kddiagprimer == "Z016" ~ NA,
      Kddiagprimer == "Z030" ~ NA,
      Kddiagprimer == "Z031" ~ NA,
      Kddiagprimer == "Z038" ~ NA,
      Kddiagprimer == "Z039" ~ NA,
      Kddiagprimer == "Z041" ~ NA,
      Kddiagprimer == "Z291" ~ NA,
      Kddiagprimer == "Z340" ~ NA,
      Kddiagprimer == "Z349" ~ NA,
      Kddiagprimer == "Z358" ~ NA,
      Kddiagprimer == "Z359" ~ NA,
      Kddiagprimer == "Z370" ~ NA,
      Kddiagprimer == "Z390" ~ NA,
      Kddiagprimer == "Z392" ~ NA,
      Kddiagprimer == "Z419" ~ NA,
      Kddiagprimer == "Z429" ~ NA,
      Kddiagprimer == "Z488" ~ NA,
      Kddiagprimer == "Z489" ~ NA,
      Kddiagprimer == "Z490" ~ NA,
      Kddiagprimer == "Z491" ~ NA,
      Kddiagprimer == "Z492" ~ NA,
      Kddiagprimer == "Z941" ~ NA,
      Kddiagprimer == "Z501" ~ NA,
      Kddiagprimer == "Z504" ~ NA,
      Kddiagprimer == "Z505" ~ NA,
      Kddiagprimer == "Z507" ~ NA,
      Kddiagprimer == "Z509" ~ NA,
      Kddiagprimer == "Z511" ~ NA,
      Kddiagprimer == "Z549" ~ NA,
      Kddiagprimer == "Z719" ~ NA,
      Kddiagprimer == "Z048" ~ NA,
      Kddiagprimer == "Z080" ~ NA,
      Kddiagprimer == "Z088" ~ NA,
      Kddiagprimer == "Z089" ~ NA,
      Kddiagprimer == "Z090" ~ NA,
      Kddiagprimer == "Z091" ~ NA,
      Kddiagprimer == "Z092" ~ NA,
      Kddiagprimer == "Z093" ~ NA,
      Kddiagprimer == "Z094" ~ NA,
      Kddiagprimer == "Z095" ~ NA,
      Kddiagprimer == "Z096" ~ NA,
      Kddiagprimer == "Z097" ~ NA,
      Kddiagprimer == "Z098" ~ NA,
      Kddiagprimer == "Z099" ~ NA,
      Kddiagprimer == "Z209" ~ NA,
      Kddiagprimer == "Z301" ~ NA,
      Kddiagprimer == "Z348" ~ NA,
      Kddiagprimer == "Z351" ~ NA,
      Kddiagprimer == "Z470" ~ NA,
      Kddiagprimer == "Z478" ~ NA,
      Kddiagprimer == "Z479" ~ NA,
      Kddiagprimer == "Z480" ~ NA,
      Kddiagprimer == "Z500" ~ NA,
      Kddiagprimer == "Z502" ~ NA,
      Kddiagprimer == "Z506" ~ NA,
      Kddiagprimer == "Z508" ~ NA,
      Kddiagprimer == "Z510" ~ NA,
      Kddiagprimer == "Z516" ~ NA,
      Kddiagprimer == "Z590" ~ NA,
      Kddiagprimer == "Z596" ~ NA,
      Kddiagprimer == "Z760" ~ NA,
      Kddiagprimer == "Z801" ~ NA,
      Kddiagprimer == "Z898" ~ NA,
      Kddiagprimer == "Z908" ~ NA,
      Kddiagprimer == "Z955" ~ NA,
      Kddiagprimer == "Z961" ~ NA,
      Kddiagprimer == "Z988" ~ NA,
      TRUE ~ Kddiagprimer))
P560ranap$Nmdiagprimer1 <- with(P560ranap,
                                ifelse(is.na(P560ranap$Kddiagprimer1),NA,Nmdiagprimer))
P560ranap <- P560ranap %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
P560ranap$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,P560ranap$Diagprimer)), "both")
P560ranap <- P560ranap %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
P560ranap$Diagnosa <- as.character(trimws(gsub(";NA|NA;|NA","",P560ranap$Diagnosa)), "both")

P560ranap <- P560ranap %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)

P560ranap <- P560ranap %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
P560ranap <- P560ranap %>% mutate(Diagnosa = na_if(Diagnosa, ""))
P560ranap <- P560ranap %>% mutate(Procedure = na_if(Procedure, "-"))
P560ranap$Diagnosa <- as.character(trimws(P560ranap$Diagnosa), "both")

library(splitstackshape)
a099ranap1 <- concat.split(P560ranap, "Diagnosa", ";")
colnames(a099ranap1)

a099ranap1$Diagnosa_01 <- as.character(trimws(a099ranap1$Diagnosa_01))
a099ranap1$Diagnosa_02 <- as.character(trimws(a099ranap1$Diagnosa_02))
a099ranap1$Diagnosa_03 <- as.character(trimws(a099ranap1$Diagnosa_03))
a099ranap1$Diagnosa_04 <- as.character(trimws(a099ranap1$Diagnosa_04))
a099ranap1$Diagnosa_05 <- as.character(trimws(a099ranap1$Diagnosa_05))
a099ranap1$Diagnosa_06 <- as.character(trimws(a099ranap1$Diagnosa_06))
a099ranap1$Diagnosa_07 <- as.character(trimws(a099ranap1$Diagnosa_07))
a099ranap1$Diagnosa_08 <- as.character(trimws(a099ranap1$Diagnosa_08))
a099ranap1$Diagnosa_09 <- as.character(trimws(a099ranap1$Diagnosa_09))
a099ranap1$Diagnosa_10 <- as.character(trimws(a099ranap1$Diagnosa_10))
a099ranap1$Diagnosa_11 <- as.character(trimws(a099ranap1$Diagnosa_11))

a099ranap1$Diagnosa_01 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_01),NA,Diagnosa_01))
a099ranap1$Diagnosa_02 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_02),NA,Diagnosa_02))
a099ranap1$Diagnosa_03 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_03),NA,Diagnosa_03))
a099ranap1$Diagnosa_04 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_04),NA,Diagnosa_04))
a099ranap1$Diagnosa_05 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_05),NA,Diagnosa_05))
a099ranap1$Diagnosa_06 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_06),NA,Diagnosa_06))
a099ranap1$Diagnosa_07 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_07),NA,Diagnosa_07))
a099ranap1$Diagnosa_08 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_08),NA,Diagnosa_08))
a099ranap1$Diagnosa_09 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_09),NA,Diagnosa_09))
a099ranap1$Diagnosa_10 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_10),NA,Diagnosa_10))
a099ranap1$Diagnosa_11 <- with(a099ranap1,ifelse(is.na(a099ranap1$Diagnosa_11),NA,Diagnosa_11))


a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_1 = case_when(
      Diagnosa_01 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_01 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_01 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_01 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_01 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_01 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_01 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_01 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_01 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_01 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_01 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_01 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_01 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_01 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_01 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_01 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_01 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_01 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_01 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_01 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_01 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_01 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_01 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_01 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_01 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_01 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_01 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_01)) %>%
  select(-Diagnosa_01)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_2 = case_when(
      Diagnosa_02 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_02 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_02 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_02 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_02 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_02 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_02 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_02 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_02 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_02 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_02 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_02 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_02 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_02 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_02 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_02 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_02 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_02 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_02 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_02 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_02 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_02 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_02 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_02 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_02 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_02 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_02 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_02)) %>%
  select(-Diagnosa_02)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_3 = case_when(
      Diagnosa_03 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_03 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_03 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_03 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_03 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_03 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_03 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_03 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_03 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_03 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_03 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_03 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_03 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_03 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_03 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_03 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_03 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_03 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_03 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_03 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_03 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_03 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_03 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_03 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_03 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_03 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_03 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_03)) %>%
  select(-Diagnosa_03)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_4 = case_when(
      Diagnosa_04 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_04 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_04 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_04 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_04 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_04 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_04 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_04 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_04 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_04 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_04 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_04 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_04 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_04 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_04 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_04 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_04 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_04 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_04 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_04 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_04 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_04 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_04 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_04 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_04 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_04 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_04 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_04)) %>%
  select(-Diagnosa_04)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_5 = case_when(
      Diagnosa_05 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_05 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_05 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_05 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_05 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_05 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_05 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_05 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_05 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_05 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_05 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_05 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_05 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_05 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_05 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_05 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_05 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_05 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_05 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_05 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_05 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_05 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_05 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_05 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_05 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_05 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_05 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_05)) %>%
  select(-Diagnosa_05)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_6 = case_when(
      Diagnosa_06 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_06 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_06 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_06 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_06 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_06 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_06 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_06 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_06 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_06 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_06 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_06 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_06 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_06 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_06 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_06 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_06 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_06 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_06 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_06 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_06 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_06 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_06 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_06 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_06 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_06 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_06 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_06)) %>%
  select(-Diagnosa_06)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_7 = case_when(
      Diagnosa_07 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_07 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_07 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_07 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_07 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_07 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_07 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_07 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_07 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_07 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_07 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_07 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_07 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_07 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_07 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_07 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_07 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_07 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_07 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_07 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_07 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_07 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_07 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_07 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_07 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_07 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_07 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_07)) %>%
  select(-Diagnosa_07)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_8 = case_when(
      Diagnosa_08 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_08 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_08 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_08 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_08 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_08 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_08 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_08 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_08 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_08 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_08 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_08 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_08 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_08 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_08 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_08 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_08 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_08 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_08 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_08 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_08 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_08 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_08 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_08 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_08 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_08 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_08 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_08)) %>%
  select(-Diagnosa_08)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_9 = case_when(
      Diagnosa_09 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_09 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_09 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_09 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_09 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_09 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_09 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_09 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_09 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_09 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_09 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_09 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_09 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_09 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_09 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_09 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_09 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_09 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_09 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_09 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_09 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_09 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_09 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_09 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_09 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_09 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_09 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_09)) %>%
  select(-Diagnosa_09)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_010 = case_when(
      Diagnosa_10 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_10 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_10 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_10 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_10 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_10 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_10 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_10 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_10 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_10 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_10 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_10 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_10 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_10 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_10 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_10 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_10 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_10 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_10 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_10 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_10 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_10 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_10 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_10 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_10 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_10 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_10 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_10)) %>%
  select(-Diagnosa_10)

a099ranap1 <- a099ranap1 %>%
  mutate(
    Diagnosa_011 = case_when(
      Diagnosa_11 == "A099 - Gastroenteritis and colitis of unspecified origin" ~ NA,
      Diagnosa_11 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_11 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_11 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_11 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_11 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_11 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_11 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_11 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_11 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_11 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_11 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_11 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_11 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_11 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_11 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_11 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_11 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_11 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_11 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_11 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_11 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_11 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_11 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_11 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_11 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_11 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_11)) %>%
  select(-Diagnosa_11)

a099ranap1 <- a099ranap1 %>%
  mutate(Diagakhir = paste0(as.character(Diagnosa_1),";",
                            as.character(Diagnosa_2),";",
                            as.character(Diagnosa_3),";",
                            as.character(Diagnosa_4),";",
                            as.character(Diagnosa_5),";",
                            as.character(Diagnosa_6),";",
                            as.character(Diagnosa_7),";",
                            as.character(Diagnosa_8),";",
                            as.character(Diagnosa_9),";",
                            as.character(Diagnosa_010),";",
                            as.character(Diagnosa_011)))

a099ranap1$Diagakhir <- as.character(trimws(gsub(";NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA;|NA","",a099ranap1$Diagakhir)),"both")
a099ranap1 <- a099ranap1 %>%
  select(-Diagnosa_1,-Diagnosa_2,-Diagnosa_3,-Diagnosa_4,-Diagnosa_5,-Diagnosa_6,
         -Diagnosa_7,-Diagnosa_8,-Diagnosa_9,-Diagnosa_010,-Diagnosa_011,-Diagnosa)
a099ranap1 <- a099ranap1 %>% mutate(Diagakhir = na_if(Diagakhir, ""))
a099ranap1 <- a099ranap1 %>% rename(Diagnosa = Diagakhir)

a099ranap1$Kel_kunjungan  <- with(lowbp1,
                              ifelse(kunjung_ke > 0 & kunjung_ke <= 5, "Kunjungan 1-5x",
                                     ifelse(kunjung_ke > 10 & kunjung_ke <= 20, "Kunjungan 11-20x",
                                            ifelse(kunjung_ke > 20 & kunjung_ke <= 30, "Kunjungan 21-30x",
                                                   ifelse(kunjung_ke > 30 & kunjung_ke <= 40, "Kunjungan 31-40x",
                                                          ifelse(kunjung_ke > 40 & kunjung_ke <= 50, "Kunjungan 41-50x",
                                                                 ifelse(kunjung_ke > 50, "Kunjungan >50x",
                                                                        kunjung_ke)))))))







lowbp1$Diagnosa <- with(lowbp1,
                        ifelse(Diagnosa == "M179 - narthrosis, unspecified;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M179 - narthrosis, unspecified",
                                      ifelse(Diagnosa == "M179 - narthrosis, unspecified;M545 - Low back pain", "M545 - Low back pain;M179 - narthrosis, unspecified",
                                             ifelse(Diagnosa == "R252 - Cramp and spasm;M545 - Low back pain", "M545 - Low back pain;R252 - Cramp and spasm",
                                                    ifelse(Diagnosa == "I10 - Essential (primary) hypertension;M5457 - Low back pain, lumbosacral region", "M5457 - Low back pain, lumbosacral region;I10 - Essential (primary) hypertension",
                                                           ifelse(Diagnosa == "I10 - Essential (primary) hypertension;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;I10 - Essential (primary) hypertension",
                                                                  ifelse(Diagnosa == "M1999 - Arthrosis, unspecified, unspecified site;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M1999 - Arthrosis, unspecified, unspecified site",
                                                                         ifelse(Diagnosa == "I10 - Essential (primary) hypertension;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;I10 - Essential (primary) hypertension",
                                                                                ifelse(Diagnosa == "M1996 - Arthrosis, unspecified, lower leg;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M1996 - Arthrosis, unspecified, lower leg",
                                                                                       ifelse(Diagnosa == "M1999 - Arthrosis, unspecified, unspecified site;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M1999 - Arthrosis, unspecified, unspecified site",
                                                                                              ifelse(Diagnosa == "G629 - Polyneuropathy, unspecified;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;G629 - Polyneuropathy, unspecified",
                                                                                                     ifelse(Diagnosa == "G542 - Cervical root disorders, not elsewhere classified;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;G542 - Cervical root disorders, not elsewhere classified",
                                                                                                            ifelse(Diagnosa == "M512 - Other specified intervertebral disc displacement;M545 - Low back pain", "M545 - Low back pain;M512 - Other specified intervertebral disc displacement",
                                                                                                                   ifelse(Diagnosa == "M1386 - Other specified arthritis, lower leg;M5456 - Low back pain, lumbar region", "M5456 - Low back pain, lumbar region;M1386 - Other specified arthritis, lower leg",
                                                                                                                          ifelse(Diagnosa == "M1999 - Arthrosis, unspecified, unspecified site;M5457 - Low back pain, lumbosacral region", "M5457 - Low back pain, lumbosacral region;M1999 - Arthrosis, unspecified, unspecified site",
                                                                                                                                 ifelse(Diagnosa == "M1399 - Arthritis, unspecified, unspecified site;M5459 - Low back pain, site unspecified", "M5459 - Low back pain, site unspecified;M1399 - Arthritis, unspecified, unspecified site",
                                                                                                                                        ifelse(Diagnosa == "K30 - Dyspepsia;M545 - Low back pain", "M545 - Low back pain;K30 - Dyspepsia",
                                                                                                                                               ifelse(Diagnosa == "I10 - Essential (primary) hypertension;M545 - Low back pain", "M545 - Low back pain;I10 - Essential (primary) hypertension",
                                                                                                                                                      Diagnosa))))))))))))))))))

a099ranap1$Procedure <- as.character(trimws(a099ranap1$Procedure), "both")
a099ranap1 <- concat.split(a099ranap1, "Procedure", ";")
colnames(a099ranap1)

a099ranap1$Procedure_1 <- as.character(trimws(a099ranap1$Procedure_1))
a099ranap1$Procedure_2 <- as.character(trimws(a099ranap1$Procedure_2))
a099ranap1$Procedure_3 <- as.character(trimws(a099ranap1$Procedure_3))
a099ranap1$Procedure_4 <- as.character(trimws(a099ranap1$Procedure_4))
a099ranap1$Procedure_5 <- as.character(trimws(a099ranap1$Procedure_5))
a099ranap1$Procedure_6 <- as.character(trimws(a099ranap1$Procedure_6))
a099ranap1$Procedure_7 <- as.character(trimws(a099ranap1$Procedure_7))
a099ranap1$Procedure_8 <- as.character(trimws(a099ranap1$Procedure_8))
a099ranap1$Procedure_9 <- as.character(trimws(a099ranap1$Procedure_9))

a099ranap1$Procedure_1 <- with(a099ranap1,ifelse(is.na(a099ranap1$Procedure_1),NA,Procedure_1))
a099ranap1$Procedure_2 <- with(a099ranap1,ifelse(is.na(a099ranap1$Procedure_2),NA,Procedure_2))
a099ranap1$Procedure_3 <- with(a099ranap1,ifelse(is.na(a099ranap1$Procedure_3),NA,Procedure_3))
a099ranap1$Procedure_4 <- with(a099ranap1,ifelse(is.na(a099ranap1$Procedure_4),NA,Procedure_4))
a099ranap1$Procedure_5 <- with(a099ranap1,ifelse(is.na(a099ranap1$Procedure_5),NA,Procedure_5))
a099ranap1$Procedure_6 <- with(a099ranap1,ifelse(is.na(a099ranap1$Procedure_6),NA,Procedure_6))
a099ranap1$Procedure_7 <- with(a099ranap1,ifelse(is.na(a099ranap1$Procedure_7),NA,Procedure_7))
a099ranap1$Procedure_8 <- with(a099ranap1,ifelse(is.na(a099ranap1$Procedure_8),NA,Procedure_8))
a099ranap1$Procedure_9 <- with(a099ranap1,ifelse(is.na(a099ranap1$Procedure_9),NA,Procedure_9))

a099ranap1 <- a099ranap1 %>%
  mutate(Procedure_new = paste0(as.character(Procedure_1),";",
                                as.character(Procedure_2),";",
                                as.character(Procedure_3),";",
                                as.character(Procedure_4),";",
                                as.character(Procedure_5),";",
                                as.character(Procedure_6),";",
                                as.character(Procedure_7),";",
                                as.character(Procedure_8),";",
                                as.character(Procedure_9)))

a099ranap1$Procedure_new <- as.character(trimws(gsub("NA;NA;NA;NA;NA;NA;NA;NA;NA|NA;NA;NA;NA;NA;NA;NA;NA|NA;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA",
                                                   "",a099ranap1$Procedure_new)),"both")
a099ranap1 <- a099ranap1 %>%
  select(-Procedure,-Procedure_1,-Procedure_2,-Procedure_3,-Procedure_4,-Procedure_5,-Procedure_6,-Procedure_7,-Procedure_8,-Procedure_9)

lowbp1$Procedure_new <- with(lowbp1,
                             ifelse(Procedure_new == "9339 - Other physical therapy;9334 - Diathermy", "9334 - Diathermy;9339 - Other physical therapy",
                                    ifelse(Procedure_new == "9335 - Other heat therapy;9339 - Other physical therapy", "9339 - Other physical therapy;9335 - Other heat therapy",
                                           ifelse(Procedure_new == "9339 - Other physical therapy;9334 - Diathermy;9313 - Resistive exercise", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                  ifelse(Procedure_new == "9339 - Other physical therapy;9313 - Resistive exercise;9334 - Diathermy", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                         ifelse(Procedure_new == "9334 - Diathermy;9313 - Resistive exercise;9339 - Other physical therapy", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                                ifelse(Procedure_new == "9313 - Resistive exercise;9334 - Diathermy;9339 - Other physical therapy", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                                              ifelse(Procedure_new == "9313 - Resistive exercise;9339 - Other physical therapy;9334 - Diathermy", "9334 - Diathermy;9339 - Other physical therapy;9313 - Resistive exercise",
                                                                                     Procedure_new))))))))

a099ranap1 <- a099ranap1 %>% mutate(Diagnosa = na_if(Diagnosa, ""))
a099ranap1 <- a099ranap1 %>% mutate(Procedure_new = na_if(Procedure_new, ""))

a099ranap1 <- a099ranap1[order(a099ranap1$Nokapst,a099ranap1$Tglplgsjp),]
a099ranap1 <- a099ranap1 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
a099ranap1$jeda <- as.integer(a099ranap1$Tgldtgsjp) - as.integer(a099ranap1$tgl_before)
a099ranap1$jeda <- as.numeric(a099ranap1$jeda)

a099ranap1 <- a099ranap1[order(a099ranap1$Nokapst,a099ranap1$Tglplgsjp),]
a099ranap1 <- a099ranap1 %>%
  group_by(Nokapst) %>%
  mutate(kunjung_ke = sequence(n()))


a099ranap1$kunjung_ke <- abs(a099ranap1$kunjung_ke )

lowbp1$Kel_kunjungan  <- with(lowbp1,
                              ifelse(kunjung_ke > 0 & kunjung_ke <= 5, "Kunjungan 1-5x",
                                     ifelse(kunjung_ke > 10 & kunjung_ke <= 20, "Kunjungan 11-20x",
                                            ifelse(kunjung_ke > 20 & kunjung_ke <= 30, "Kunjungan 21-30x",
                                                   ifelse(kunjung_ke > 30 & kunjung_ke <= 40, "Kunjungan 31-40x",
                                                          ifelse(kunjung_ke > 40 & kunjung_ke <= 50, "Kunjungan 41-50x",
                                                                 ifelse(kunjung_ke > 50, "Kunjungan >50x",
                                                                        kunjung_ke)))))))



lowbp1$Kel_kunjungan  <- with(lowbp1,
                              ifelse(kunjung_ke > 0 & kunjung_ke <= 8, "Kunjungan 1-8x",
                                     ifelse(kunjung_ke > 8 & kunjung_ke <= 16, "Kunjungan 9-16x",
                                            ifelse(kunjung_ke > 16 & kunjung_ke <= 24, "Kunjungan 17-24x",
                                                   ifelse(kunjung_ke > 24 & kunjung_ke <= 32, "Kunjungan 25-32x",
                                                          ifelse(kunjung_ke > 32 & kunjung_ke <= 40, "Kunjungan 33-40x",
                                                                 ifelse(kunjung_ke > 40 & kunjung_ke <= 48, "Kunjungan 41-48x",
                                                                        ifelse(kunjung_ke > 48 & kunjung_ke <= 56, "Kunjungan 49-56x",
                                                                               ifelse(kunjung_ke > 56 & kunjung_ke <= 64, "Kunjungan 57-64x",
                                                                                      ifelse(kunjung_ke > 64 & kunjung_ke <= 72, "Kunjungan 65-72x",
                                                                                             ifelse(kunjung_ke > 72 & kunjung_ke <= 80, "Kunjungan 73-80x",
                                                                                                    ifelse(kunjung_ke > 80 & kunjung_ke <= 88, "Kunjungan 81-88x",
                                                                                                           ifelse(kunjung_ke > 88 & kunjung_ke <= 96, "Kunjungan 89-96x",
                                                                                                                  ifelse(kunjung_ke > 96 & kunjung_ke <= 104, "Kunjungan 97-104x",
                                                                                                                         ifelse(kunjung_ke > 104 & kunjung_ke <= 112, "Kunjungan 105-112x",
                                                                                                                                ifelse(kunjung_ke > 112 & kunjung_ke <= 120, "Kunjungan 113-120x",
                                                                                                                                       ifelse(kunjung_ke > 120 & kunjung_ke <= 128, "Kunjungan 121-128x",
                                                                                                                                              ifelse(kunjung_ke > 128 & kunjung_ke <= 136, "Kunjungan 129-136x",
                                                                                                                                                     ifelse(kunjung_ke > 136 & kunjung_ke <= 144, "Kunjungan 137-144x",
                                                                                                                                                            ifelse(kunjung_ke > 144 & kunjung_ke <= 152, "Kunjungan 145-152x",
                                                                                                                                                                   ifelse(kunjung_ke > 152 & kunjung_ke <= 160, "Kunjungan 153-160x",
                                                                                                                                                                          ifelse(kunjung_ke > 160 & kunjung_ke <= 168, "Kunjungan 161-168x",
                                                                                                                                                                                 ifelse(kunjung_ke > 168 & kunjung_ke <= 176, "Kunjungan 169-176x",
                                                                                                                                                                                        ifelse(kunjung_ke > 176 & kunjung_ke <= 184, "Kunjungan 177-184x",
                                                                                                                                                                                               ifelse(kunjung_ke > 184 & kunjung_ke <= 192, "Kunjungan 185-192x",
                                                                                                                                                                                                      ifelse(kunjung_ke > 192 & kunjung_ke <= 200, "Kunjungan 193-200x",
                                                                                                                                                                                                             ifelse(kunjung_ke > 200 & kunjung_ke <= 208, "Kunjungan 201-208x",
                                                                                                                                                                                                                    ifelse(kunjung_ke > 208 & kunjung_ke <= 216, "Kunjungan 209-216x",
                                                                                                                                                                                                                           ifelse(kunjung_ke > 216 & kunjung_ke <= 224, "Kunjungan 217-224x",
                                                                                                                                                                                                                                  ifelse(kunjung_ke > 224 & kunjung_ke <= 232, "Kunjungan 225-232x",
                                                                                                                                                                                                                                         ifelse(kunjung_ke > 232 & kunjung_ke <= 240, "Kunjungan 233-240x",
                                                                                                                                                                                                                                                ifelse(kunjung_ke > 240 & kunjung_ke <= 248, "Kunjungan 241-248x",
                                                                                                                                                                                                                                                       ifelse(kunjung_ke > 248 & kunjung_ke <= 256, "Kunjungan 249-256x",
                                                                                                                                                                                                                                                              ifelse(kunjung_ke > 256 & kunjung_ke <= 264, "Kunjungan 257-264x",
                                                                                                                                                                                                                                                                     ifelse(kunjung_ke > 264 & kunjung_ke <= 272, "Kunjungan 265-272x",
                                                                                                                                                                                                                                                                            ifelse(kunjung_ke > 272 & kunjung_ke <= 280, "Kunjungan 273-280x",
                                                                                                                                                                                                                                                                                   ifelse(kunjung_ke > 280 & kunjung_ke <= 288, "Kunjungan 281-288x",
                                                                                                                                                                                                                                                                                          ifelse(kunjung_ke > 288 & kunjung_ke <= 296, "Kunjungan 289-296x",
                                                                                                                                                                                                                                                                                                 ifelse(kunjung_ke > 296 & kunjung_ke <= 304, "Kunjungan 297-304x",
                                                                                                                                                                                                                                                                                                        ifelse(kunjung_ke > 304 & kunjung_ke <= 312, "Kunjungan 305-312x",
                                                                                                                                                                                                                                                                                                               ifelse(kunjung_ke > 312 & kunjung_ke <= 320, "Kunjungan 313-320x",
                                                                                                                                                                                                                                                                                                                      ifelse(kunjung_ke > 320 & kunjung_ke <= 328, "Kunjungan 321-328x",
                                                                                                                                                                                                                                                                                                                             ifelse(kunjung_ke > 328 & kunjung_ke <= 336, "Kunjungan 329-336x",
                                                                                                                                                                                                                                                                                                                                    ifelse(kunjung_ke > 336 & kunjung_ke <= 344, "Kunjungan 337-344x",
                                                                                                                                                                                                                                                                                                                                           ifelse(kunjung_ke > 344 & kunjung_ke <= 352, "Kunjungan 345-352x",
                                                                                                                                                                                                                                                                                                                                                  ifelse(kunjung_ke > 352 & kunjung_ke <= 360, "Kunjungan 353-360x",
                                                                                                                                                                                                                                                                                                                                                         ifelse(kunjung_ke > 360 & kunjung_ke <= 368, "Kunjungan 361-368x",
                                                                                                                                                                                                                                                                                                                                                                ifelse(kunjung_ke > 368 & kunjung_ke <= 376, "Kunjungan 369-376x",
                                                                                                                                                                                                                                                                                                                                                                       ifelse(kunjung_ke > 376 & kunjung_ke <= 384, "Kunjungan 377-384x",
                                                                                                                                                                                                                                                                                                                                                                              ifelse(kunjung_ke > 384, "Kunjungan >384x",
                                                                                                                                                                                                                                                                                                                                                                                     "Salah"))))))))))))))))))))))))))))))))))))))))))))))))))


lowbp1$asesmen  <- with(lowbp1,
                        ifelse(kunjung_ke > 0 & kunjung_ke <= 8, 1,
                               ifelse(kunjung_ke > 8 & kunjung_ke <= 16, 2,
                                      ifelse(kunjung_ke > 16 & kunjung_ke <= 24, 3,
                                             ifelse(kunjung_ke > 24 & kunjung_ke <= 32, 4,
                                                    ifelse(kunjung_ke > 32 & kunjung_ke <= 40, 5,
                                                           ifelse(kunjung_ke > 40 & kunjung_ke <= 48, 6,
                                                                  ifelse(kunjung_ke > 48 & kunjung_ke <= 56, 7,
                                                                         ifelse(kunjung_ke > 56 & kunjung_ke <= 64, 8,
                                                                                ifelse(kunjung_ke > 64 & kunjung_ke <= 72, 9,
                                                                                       ifelse(kunjung_ke > 72 & kunjung_ke <= 80, 10,
                                                                                              ifelse(kunjung_ke > 80 & kunjung_ke <= 88, 11,
                                                                                                     ifelse(kunjung_ke > 88 & kunjung_ke <= 96, 12,
                                                                                                            ifelse(kunjung_ke > 96 & kunjung_ke <= 104, 13,
                                                                                                                   ifelse(kunjung_ke > 104 & kunjung_ke <= 112, 14,
                                                                                                                          ifelse(kunjung_ke > 112 & kunjung_ke <= 120, 15,
                                                                                                                                 ifelse(kunjung_ke > 120 & kunjung_ke <= 128, 16,
                                                                                                                                        ifelse(kunjung_ke > 128 & kunjung_ke <= 136, 17,
                                                                                                                                               ifelse(kunjung_ke > 136 & kunjung_ke <= 144, 18,
                                                                                                                                                      ifelse(kunjung_ke > 144 & kunjung_ke <= 152, 19,
                                                                                                                                                             ifelse(kunjung_ke > 152 & kunjung_ke <= 160, 20,
                                                                                                                                                                    ifelse(kunjung_ke > 160 & kunjung_ke <= 168, 21,
                                                                                                                                                                           ifelse(kunjung_ke > 168 & kunjung_ke <= 176, 22,
                                                                                                                                                                                  ifelse(kunjung_ke > 176 & kunjung_ke <= 184, 23,
                                                                                                                                                                                         ifelse(kunjung_ke > 184 & kunjung_ke <= 192, 24,
                                                                                                                                                                                                ifelse(kunjung_ke > 192 & kunjung_ke <= 200, 25,
                                                                                                                                                                                                       ifelse(kunjung_ke > 200 & kunjung_ke <= 208, 26,
                                                                                                                                                                                                              ifelse(kunjung_ke > 208 & kunjung_ke <= 216, 27,
                                                                                                                                                                                                                     ifelse(kunjung_ke > 216 & kunjung_ke <= 224, 28,
                                                                                                                                                                                                                            ifelse(kunjung_ke > 224 & kunjung_ke <= 232, 29,
                                                                                                                                                                                                                                   ifelse(kunjung_ke > 232 & kunjung_ke <= 240, 30,
                                                                                                                                                                                                                                          ifelse(kunjung_ke > 240 & kunjung_ke <= 248, 31,
                                                                                                                                                                                                                                                 ifelse(kunjung_ke > 248 & kunjung_ke <= 256, 32,
                                                                                                                                                                                                                                                        ifelse(kunjung_ke > 256 & kunjung_ke <= 264, 33,
                                                                                                                                                                                                                                                               ifelse(kunjung_ke > 264 & kunjung_ke <= 272, 34,
                                                                                                                                                                                                                                                                      ifelse(kunjung_ke > 272 & kunjung_ke <= 280, 35,
                                                                                                                                                                                                                                                                             ifelse(kunjung_ke > 280 & kunjung_ke <= 288, 36,
                                                                                                                                                                                                                                                                                    ifelse(kunjung_ke > 288 & kunjung_ke <= 296, 37,
                                                                                                                                                                                                                                                                                           ifelse(kunjung_ke > 296 & kunjung_ke <= 304, 38,
                                                                                                                                                                                                                                                                                                  ifelse(kunjung_ke > 304 & kunjung_ke <= 312, 39,
                                                                                                                                                                                                                                                                                                         ifelse(kunjung_ke > 312 & kunjung_ke <= 320, 40,
                                                                                                                                                                                                                                                                                                                ifelse(kunjung_ke > 320 & kunjung_ke <= 328, 41,
                                                                                                                                                                                                                                                                                                                       ifelse(kunjung_ke > 328 & kunjung_ke <= 336, 42,
                                                                                                                                                                                                                                                                                                                              ifelse(kunjung_ke > 336 & kunjung_ke <= 344, 43,
                                                                                                                                                                                                                                                                                                                                     ifelse(kunjung_ke > 344 & kunjung_ke <= 352, 44,
                                                                                                                                                                                                                                                                                                                                            ifelse(kunjung_ke > 352 & kunjung_ke <= 360, 45,
                                                                                                                                                                                                                                                                                                                                                   ifelse(kunjung_ke > 360 & kunjung_ke <= 368, 46,
                                                                                                                                                                                                                                                                                                                                                          ifelse(kunjung_ke > 368 & kunjung_ke <= 376, 47,
                                                                                                                                                                                                                                                                                                                                                                 ifelse(kunjung_ke > 376 & kunjung_ke <= 384, 48,
                                                                                                                                                                                                                                                                                                                                                                        ifelse(kunjung_ke > 384, 49,
                                                                                                                                                                                                                                                                                                                                                                               "Salah"))))))))))))))))))))))))))))))))))))))))))))))))))




cek <- necrosispulp1 %>% subset(kunjung_ke == "Salah")
#kunjungan ke-1
procedure apa saja
#kunjungan ke-2
procedure apa saja dst

%>%
  replace(is.na(.), 0)

nonspes1$Procedure_1 [is.na(nonspes1$Procedure_1)] <- ""
nonspes1$Procedure_2 [is.na(nonspes1$Procedure_2)] <- ""
nonspes1$Procedure_3 [is.na(nonspes1$Procedure_3)] <- ""
nonspes1$Procedure_4 [is.na(nonspes1$Procedure_4)] <- ""
nonspes1$Procedure_5 [is.na(nonspes1$Procedure_5)] <- ""
nonspes1$Procedure_6 [is.na(nonspes1$Procedure_6)] <- ""
nonspes1$Procedure_7 [is.na(nonspes1$Procedure_7)] <- ""
nonspes1$Procedure_8 [is.na(nonspes1$Procedure_8)] <- ""


necrosispulp1$Nokapst <- as.factor(necrosispulp1$Nokapst)
necrosispulp2 <- necrosispulp1 %>%
  dplyr::group_by(Nokapst) %>%
  dplyr::count(n()) %>%
  rename(kunj_noka = n) %>%
  select(kunj_noka)

Procedure_1 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_1) %>% unique() %>% na.omit()
Procedure_2 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_2) %>% unique() %>% na.omit()
Procedure_3 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_3) %>% unique() %>% na.omit()
Procedure_4 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_4) %>% unique() %>% na.omit()
Procedure_5 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_5) %>% unique() %>% na.omit()
Procedure_6 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_6) %>% unique() %>% na.omit()
Procedure_7 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_7) %>% unique() %>% na.omit()
Procedure_8 <- nonspes %>% 
  subset(Diagnosa == "K041 - Necrosis of pulp") %>%
  select(Procedure_8) %>% unique() %>% na.omit()

colnames(Procedure_1)=colnames(Procedure_2)=colnames(Procedure_3)=colnames(Procedure_4)=colnames(Procedure_5)=colnames(Procedure_6)=colnames(Procedure_7)=colnames(Procedure_8)
Procedure <- rbind(Procedure_1,Procedure_2,Procedure_3,Procedure_4,
                   Procedure_5,Procedure_6,Procedure_7,Procedure_8) %>%
  unique()
write.xlsx(Procedure, file = "Procedure K041.xlsx")

write.csv(a099ranap1, "D://data gresik//MTF KC GRESIK//gresik_2020//560_Transurethral removal of obstruction from ureter and renal pelvis.csv",
          na="", row.names = FALSE)
write.xlsx(necrosispulp2, file = "Normal_curve.xlsx")
#======================================================================================
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")
library(excel.link)

refpoli <- xl.read.file("01082020 referensi poli.xlsx",
                        header = TRUE,
                        row.names = NULL,
                        col.names = NULL,
                        xl.sheet = "Sheet1",
                        top.left.cell = "A1",
                        na = "",
                        excel.visible = FALSE) %>%
  select(1,4)

data0 <- left_join(nonspes,refpoli, by = c("Politujsjp"="KDPOLI")) %>%
  select(-Politujsjp)
data0 <- data0 %>% rename(Politujsjp = NMPOLI)
rm(refpoli)
setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")

data0 <- data0 %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
data0 <- data0 %>% mutate(Procedure = na_if(Procedure, "-"))

setwd("D:/data gresik/MTF KC GRESIK/gresik_2020")
load("ur_all.rda")

data0 <- data0 %>%
  select(Nosjp,Flagspesialistik,Flagtacc,Jenisppkperujuk,Typeppkperujuk,
         Typeppklayan,Diagnosa,Pstprb,Tmtpstprb,Pstprolanis,
         Tmtpstprolanis) %>% unique()

data0 <- left_join(data0,data) %>%
  select(-CMG,-CBG,-Spec,-Sevel,-tipe)
#===============================SKIP======================================
data0 <- data0 %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
data0 <- data0 %>% mutate(Procedure = na_if(Procedure, "-"))
data0$Diagnosa <- with(data0,
                         ifelse(Diagnosa == "I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension",
                                ifelse(Diagnosa == "E785 - Hyperlipidaemia, unspecified;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                       "E119 - Non-insulin-dependent diabetes mellitus without complications;I119 - Hypertensive heart disease without (congestive) heart failure",
                                       ifelse(Diagnosa == "I119 - Hypertensive heart disease without (congestive) heart failure;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                              "E119 - Non-insulin-dependent diabetes mellitus without complications;I119 - Hypertensive heart disease without (congestive) heart failure",
                                              ifelse(Diagnosa == "K30 - Dyspepsia;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                     "E119 - Non-insulin-dependent diabetes mellitus without complications;K30 - Dyspepsia",
                                                     ifelse(Diagnosa == "E119 - Non-insulin-dependent diabetes mellitus without complications;E785 - Hyperlipidaemia, unspecified;I10 - Essential (primary) hypertension",
                                                            "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                            ifelse(Diagnosa == "I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications;E785 - Hyperlipidaemia, unspecified",
                                                                   "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                                   ifelse(Diagnosa == "I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                          "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                                          ifelse(Diagnosa == "E785 - Hyperlipidaemia, unspecified;I10 - Essential (primary) hypertension;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                 "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                                                 ifelse(Diagnosa == "E785 - Hyperlipidaemia, unspecified;E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension",
                                                                                        "E119 - Non-insulin-dependent diabetes mellitus without complications;I10 - Essential (primary) hypertension;E785 - Hyperlipidaemia, unspecified",
                                                                                        ifelse(Diagnosa == "I630 - Cerebral infarction due to thrombosis of precerebral arteries;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                               "E119 - Non-insulin-dependent diabetes mellitus without complications;I630 - Cerebral infarction due to thrombosis of precerebral arteries",
                                                                                               ifelse(Diagnosa == "E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                                      "E119 - Non-insulin-dependent diabetes mellitus without complications;E790 - Hyperuricaemia without signs of inflammatory arthritis and tophaceous disease",
                                                                                                      ifelse(Diagnosa == "I251 - Atherosclerotic heart disease;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                                             "E119 - Non-insulin-dependent diabetes mellitus without complications;I251 - Atherosclerotic heart disease",
                                                                                                             ifelse(Diagnosa == "I633 - Cerebral infarction due to thrombosis of cerebral arteries;E119 - Non-insulin-dependent diabetes mellitus without complications",
                                                                                                                    "E119 - Non-insulin-dependent diabetes mellitus without complications;I633 - Cerebral infarction due to thrombosis of cerebral arteries",
                                                                                                                    Diagnosa))))))))))))))
write.csv(P560ranap, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_CMG_N.csv",
          na="", row.names = FALSE)
#===============================SKIP======================================
#agar bisa SUBSET
data$Kdppklayan <- as.character(trimws(substr(data$Nosjp,1,8)), "both")
data <- data %>%


#========================================================================
write.xlsx(readmisi_5, file = "readmisi_rajal.xlsx")
#========================================================================
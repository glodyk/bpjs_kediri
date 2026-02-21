setwd("D:/data gresik/MTF KC GRESIK/gresik_2020/tmp")

if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")


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
  select(-CMG,-CBG,-Spec,-Sevel,-tipe)
#========================================================================
necrosispulp <- data %>%
  subset(Nmtkp == "RJTL") %>%
  mutate(Diagnosa = paste0(as.character(Kddiagprimer)," - ",
                           as.character(Nmdiagprimer),";",
                           as.character(Diagsekunder))) %>%
  filter(str_detect(Diagnosa,"K041|K040")) %>%
  select(-Diagnosa,-Nmtkp)

necrosispulp <- necrosispulp %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
necrosispulp <- necrosispulp %>% mutate(Diagsekunder = na_if(Diagsekunder, ""))
necrosispulp <- necrosispulp %>% mutate(Procedure = na_if(Procedure, "-"))
necrosispulp <- necrosispulp %>% mutate(Procedure = na_if(Procedure, ""))

necrosispulp <- necrosispulp %>%
  mutate(
    Kddiagprimer1 = case_when(
      Kddiagprimer == "Z039" ~ NA,
      Kddiagprimer == "Z041" ~ NA,
      Kddiagprimer == "Z359" ~ NA,
      Kddiagprimer == "Z491" ~ NA,
      Kddiagprimer == "Z501" ~ NA,
      Kddiagprimer == "Z719" ~ NA,
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
      Kddiagprimer == "Z340" ~ NA,
      Kddiagprimer == "Z349" ~ NA,
      Kddiagprimer == "Z504" ~ NA,
      Kddiagprimer == "Z509" ~ NA,
      Kddiagprimer == "Z549" ~ NA,
      Kddiagprimer == "Z898" ~ NA,
      Kddiagprimer == "Z908" ~ NA,
      Kddiagprimer == "Z961" ~ NA,
      Kddiagprimer == "Z988" ~ NA,
      Kddiagprimer == "Z489" ~ NA,
      TRUE ~ Kddiagprimer))
necrosispulp$Nmdiagprimer1 <- with(necrosispulp,
                              ifelse(is.na(necrosispulp$Kddiagprimer1),NA,Nmdiagprimer))
necrosispulp <- necrosispulp %>%
  mutate(Diagprimer = paste0(as.character(Kddiagprimer1)," - ",
                             as.character(Nmdiagprimer1)))
necrosispulp$Diagprimer <- as.character(trimws(gsub("NA - NA",NA,necrosispulp$Diagprimer)), "both")
necrosispulp <- necrosispulp %>%
  mutate(Diagnosa = paste0(as.character(Diagprimer),";",
                           as.character(Diagsekunder)))
necrosispulp$Diagnosa <- as.character(trimws(gsub(";NA|NA;","",necrosispulp$Diagnosa)), "both")

necrosispulp <- necrosispulp %>% select(-Kddiagprimer1,-Nmdiagprimer1,-Diagprimer)

necrosispulp <- necrosispulp %>% mutate(Diagsekunder = na_if(Diagsekunder, "-"))
necrosispulp <- necrosispulp %>% mutate(Procedure = na_if(Procedure, "-"))
necrosispulp$Diagnosa <- as.character(trimws(necrosispulp$Diagnosa), "both")

library(splitstackshape)
necrosispulp1 <- concat.split(necrosispulp, "Diagnosa", ";")
colnames(necrosispulp1)

necrosispulp1$Diagnosa_1 <- as.character(trimws(necrosispulp1$Diagnosa_1))
necrosispulp1$Diagnosa_2 <- as.character(trimws(necrosispulp1$Diagnosa_2))
necrosispulp1$Diagnosa_3 <- as.character(trimws(necrosispulp1$Diagnosa_3))
necrosispulp1$Diagnosa_4 <- as.character(trimws(necrosispulp1$Diagnosa_4))
necrosispulp1$Diagnosa_5 <- as.character(trimws(necrosispulp1$Diagnosa_5))
necrosispulp1$Diagnosa_6 <- as.character(trimws(necrosispulp1$Diagnosa_6))

necrosispulp1$Diagnosa_1 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Diagnosa_1),NA,Diagnosa_1))
necrosispulp1$Diagnosa_2 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Diagnosa_2),NA,Diagnosa_2))
necrosispulp1$Diagnosa_3 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Diagnosa_3),NA,Diagnosa_3))
necrosispulp1$Diagnosa_4 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Diagnosa_4),NA,Diagnosa_4))
necrosispulp1$Diagnosa_5 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Diagnosa_5),NA,Diagnosa_5))
necrosispulp1$Diagnosa_6 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Diagnosa_6),NA,Diagnosa_6))

necrosispulp1 <- necrosispulp1 %>%
  mutate(
    Diagnosa_01 = case_when(
      Diagnosa_1 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_1 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_1 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_1 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_1 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_1 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_1 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_1 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_1 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_1 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_1 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_1 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_1 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_1 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_1 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_1 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_1 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_1 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_1 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_1 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_1 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_1 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_1 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_1 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_1 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_1 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_1)) %>%
  select(-Diagnosa_1)

necrosispulp1 <- necrosispulp1 %>%
  mutate(
    Diagnosa_02 = case_when(
      Diagnosa_2 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_2 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_2 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_2 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_2 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_2 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_2 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_2 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_2 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_2 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_2 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_2 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_2 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_2 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_2 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_2 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_2 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_2 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_2 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_2 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_2 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_2 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_2 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_2 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_2 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_2 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_2)) %>%
  select(-Diagnosa_2)

necrosispulp1 <- necrosispulp1 %>%
  mutate(
    Diagnosa_03 = case_when(
      Diagnosa_3 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_3 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_3 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_3 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_3 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_3 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_3 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_3 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_3 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_3 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_3 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_3 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_3 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_3 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_3 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_3 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_3 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_3 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_3 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_3 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_3 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_3 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_3 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_3 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_3 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_3 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_3)) %>%
  select(-Diagnosa_3)

necrosispulp1 <- necrosispulp1 %>%
  mutate(
    Diagnosa_04 = case_when(
      Diagnosa_4 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_4 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_4 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_4 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_4 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_4 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_4 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_4 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_4 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_4 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_4 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_4 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_4 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_4 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_4 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_4 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_4 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_4 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_4 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_4 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_4 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_4 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_4 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_4 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_4 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_4 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_4)) %>%
  select(-Diagnosa_4)

necrosispulp1 <- necrosispulp1 %>%
  mutate(
    Diagnosa_05 = case_when(
      Diagnosa_5 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_5 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_5 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_5 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_5 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_5 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_5 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_5 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_5 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_5 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_5 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_5 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_5 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_5 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_5 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_5 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_5 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_5 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_5 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_5 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_5 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_5 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_5 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_5 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_5 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_5 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_5)) %>%
  select(-Diagnosa_5)

necrosispulp1 <- necrosispulp1 %>%
  mutate(
    Diagnosa_06 = case_when(
      Diagnosa_6 == "Z039 - Observation for suspected disease or condition, unspecified" ~ NA,
      Diagnosa_6 == "Z041 - Examination and observation following transport accident" ~ NA,
      Diagnosa_6 == "Z359 - Supervision of high-risk pregnancy, unspecified" ~ NA,
      Diagnosa_6 == "Z491 - Extracorporeal dialysis" ~ NA,
      Diagnosa_6 == "Z501 - Other physical therapy" ~ NA,
      Diagnosa_6 == "Z719 - Counselling, unspecified" ~ NA,
      Diagnosa_6 == "Z088 - Follow-up examination after other treatment for malignant neoplasm" ~ NA,
      Diagnosa_6 == "Z089 - Follow-up examination after unspecified treatment for malignant neoplasm" ~ NA,
      Diagnosa_6 == "Z090 - Follow-up examination after surgery for other conditions" ~ NA,
      Diagnosa_6 == "Z091 - Follow-up examination after radiotherapy for other conditions" ~ NA,
      Diagnosa_6 == "Z092 - Follow-up examination after chemotherapy for other conditions" ~ NA,
      Diagnosa_6 == "Z093 - Follow-up examination after psychotherapy" ~ NA,
      Diagnosa_6 == "Z094 - Follow-up examination after treatment of fracture" ~ NA,
      Diagnosa_6 == "Z097 - Follow-up examination after combined treatment for other conditions" ~ NA,
      Diagnosa_6 == "Z098 - Follow-up examination after other treatment for other conditions" ~ NA,
      Diagnosa_6 == "Z099 - Follow-up examination after unspecified treatment for other conditions" ~ NA,
      Diagnosa_6 == "Z340 - Supervision of normal first pregnancy" ~ NA,
      Diagnosa_6 == "Z349 - Supervision of normal pregnancy, unspecified" ~ NA,
      Diagnosa_6 == "Z504 - Psychotherapy, not elsewhere classified" ~ NA,
      Diagnosa_6 == "Z509 - Care involving use of rehabilitation procedure, unspecified" ~ NA,
      Diagnosa_6 == "Z549 - Convalescence following unspecified treatment" ~ NA,
      Diagnosa_6 == "Z898 - Acquired absence of upper and lower limbs [any level]" ~ NA,
      Diagnosa_6 == "Z908 - Acquired absence of other organs" ~ NA,
      Diagnosa_6 == "Z961 - Presence of intraocular lens" ~ NA,
      Diagnosa_6 == "Z988 - Other specified postsurgical states" ~ NA,
      Diagnosa_6 == "Z489 - Surgical follow-up care, unspecified" ~ NA,
      TRUE ~ Diagnosa_6)) %>%
  select(-Diagnosa_6)

necrosispulp1 <- necrosispulp1 %>%
  mutate(Diagakhir = paste0(as.character(Diagnosa_01),";",
                            as.character(Diagnosa_02),";",
                            as.character(Diagnosa_03),";",
                            as.character(Diagnosa_04),";",
                            as.character(Diagnosa_05),";",
                            as.character(Diagnosa_06)))

necrosispulp1$Diagakhir <- as.character(trimws(gsub(";NA;NA;NA;NA;NA;NA|;NA;NA;NA;NA;NA|;NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA;","",necrosispulp1$Diagakhir)),"both")
necrosispulp1 <- necrosispulp1 %>%
  select(-Diagnosa_01,-Diagnosa_02,-Diagnosa_03,-Diagnosa_04,-Diagnosa_05,
         -Diagnosa_06,-Diagnosa)
necrosispulp1 <- necrosispulp1 %>% mutate(Diagakhir = na_if(Diagakhir, ""))
necrosispulp1 <- necrosispulp1 %>% rename(Diagnosa = Diagakhir)

necrosispulp1$Diagnosa <- with(necrosispulp1,
                               ifelse(Diagnosa == "K052 - Acute periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K052 - Acute periodontitis",
                                      ifelse(Diagnosa == "K045 - Chronic apical periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K045 - Chronic apical periodontitis",
                                             ifelse(Diagnosa == "K011 - Impacted teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K011 - Impacted teeth",
                                                    ifelse(Diagnosa == "S025 - Fracture of tooth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;S025 - Fracture of tooth",
                                                           ifelse(Diagnosa == "K040 - Pulpitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K040 - Pulpitis",
                                                                  ifelse(Diagnosa == "K010 - Embedded teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth",
                                                                         ifelse(Diagnosa == "G442 - Tension-type headache;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;G442 - Tension-type headache",
                                                                                ifelse(Diagnosa == "K053 - Chronic periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K053 - Chronic periodontitis",
                                                                                       ifelse(Diagnosa == "J329 - Chronic sinusitis, unspecified;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;J329 - Chronic sinusitis, unspecified",
                                                                                              ifelse(Diagnosa == "K047 - Periapical abscess without sinus;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K047 - Periapical abscess without sinus",
                                                                                                     ifelse(Diagnosa == "K010 - Embedded teeth;K041 - Necrosis of pulp;K052 - Acute periodontitis", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                            ifelse(Diagnosa == "K010 - Embedded teeth;K052 - Acute periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                   ifelse(Diagnosa == "K010 - Embedded teeth;K052 - Acute periodontitis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                          ifelse(Diagnosa == "K041 - Necrosis of pulp;K052 - Acute periodontitis;K010 - Embedded teeth", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                                 ifelse(Diagnosa == "K052 - Acute periodontitis;K041 - Necrosis of pulp;K010 - Embedded teeth", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                                        ifelse(Diagnosa == "K052 - Acute periodontitis;K010 - Embedded teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K010 - Embedded teeth;K052 - Acute periodontitis",
                                                                                                                                               ifelse(Diagnosa == "K029 - Dental caries, unspecified;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K029 - Dental caries, unspecified",
                                                                                                                                                      ifelse(Diagnosa == "K054 - Periodontosis;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K054 - Periodontosis",
                                                                                                                                                             ifelse(Diagnosa == "K011 - Impacted teeth;K041 - Necrosis of pulp;K122 - Cellulitis and abscess of mouth", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                    ifelse(Diagnosa == "K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                           ifelse(Diagnosa == "K041 - Necrosis of pulp;K122 - Cellulitis and abscess of mouth;K011 - Impacted teeth", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                  ifelse(Diagnosa == "K122 - Cellulitis and abscess of mouth;K041 - Necrosis of pulp;K011 - Impacted teeth", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                         ifelse(Diagnosa == "K122 - Cellulitis and abscess of mouth;K011 - Impacted teeth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K011 - Impacted teeth;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                                ifelse(Diagnosa == "K122 - Cellulitis and abscess of mouth;K041 - Necrosis of pulp", "K041 - Necrosis of pulp;K122 - Cellulitis and abscess of mouth",
                                                                                                                                                                                                       Diagnosa)))))))))))))))))))))))))

necrosispulp1$Procedure <- as.character(trimws(necrosispulp1$Procedure), "both")
necrosispulp1 <- concat.split(necrosispulp1, "Procedure", ";")
colnames(necrosispulp1)

necrosispulp1$Procedure_1 <- as.character(trimws(necrosispulp1$Procedure_1))
necrosispulp1$Procedure_2 <- as.character(trimws(necrosispulp1$Procedure_2))
necrosispulp1$Procedure_3 <- as.character(trimws(necrosispulp1$Procedure_3))
necrosispulp1$Procedure_4 <- as.character(trimws(necrosispulp1$Procedure_4))

necrosispulp1$Procedure_1 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Procedure_1),NA,Procedure_1))
necrosispulp1$Procedure_2 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Procedure_2),NA,Procedure_2))
necrosispulp1$Procedure_3 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Procedure_3),NA,Procedure_3))
necrosispulp1$Procedure_4 <- with(necrosispulp1,ifelse(is.na(necrosispulp1$Procedure_4),NA,Procedure_4))

necrosispulp1 <- necrosispulp1 %>%
  mutate(Procedure_new = paste0(as.character(Procedure_1),";",
                                as.character(Procedure_2),";",
                                as.character(Procedure_3),";",
                                as.character(Procedure_4)))

necrosispulp1$Procedure_new <- as.character(trimws(gsub("NA;NA;NA;NA|;NA;NA;NA|;NA;NA|;NA|NA",
                                                   "",necrosispulp1$Procedure_new)),"both")
necrosispulp1 <- necrosispulp1 %>%
  select(-Procedure_1,-Procedure_2,-Procedure_3,-Procedure_4)

necrosispulp1$Procedure_new <- with(necrosispulp1,
                                    ifelse(Procedure_new == "2349 - Other dental restoration;2370 - Root canal, not otherwise specified;8712 - Other dental x-ray",
                                           "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                           ifelse(Procedure_new == "2349 - Other dental restoration;8712 - Other dental x-ray;2370 - Root canal, not otherwise specified",
                                                  "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                                  ifelse(Procedure_new == "2370 - Root canal, not otherwise specified;8712 - Other dental x-ray;2349 - Other dental restoration",
                                                         "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                                         ifelse(Procedure_new == "8712 - Other dental x-ray;2370 - Root canal, not otherwise specified;2349 - Other dental restoration",
                                                                "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                                                ifelse(Procedure_new == "8712 - Other dental x-ray;2349 - Other dental restoration;2370 - Root canal, not otherwise specified",
                                                                       "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8712 - Other dental x-ray",
                                                                       ifelse(Procedure_new == "2349 - Other dental restoration;2370 - Root canal, not otherwise specified;8711 - Full-mouth x-ray of teeth",
                                                                              "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                              ifelse(Procedure_new == "2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth;2370 - Root canal, not otherwise specified",
                                                                                     "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                     ifelse(Procedure_new == "2370 - Root canal, not otherwise specified;8711 - Full-mouth x-ray of teeth;2349 - Other dental restoration",
                                                                                            "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                            ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2370 - Root canal, not otherwise specified;2349 - Other dental restoration",
                                                                                                   "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                                   ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2349 - Other dental restoration;2370 - Root canal, not otherwise specified",
                                                                                                          "2370 - Root canal, not otherwise specified;2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                                          ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2349 - Other dental restoration",
                                                                                                                 "2349 - Other dental restoration;8711 - Full-mouth x-ray of teeth",
                                                                                                                 ifelse(Procedure_new == "8712 - Other dental x-ray;2349 - Other dental restoration",
                                                                                                                        "2349 - Other dental restoration;8712 - Other dental x-ray",
                                                                                                                        ifelse(Procedure_new == "2349 - Other dental restoration;2370 - Root canal, not otherwise specified",
                                                                                                                               "2370 - Root canal, not otherwise specified;2349 - Other dental restoration",
                                                                                                                               ifelse(Procedure_new == "2349 - Other dental restoration;2371 - Root canal therapy with irrigation",
                                                                                                                                      "2371 - Root canal therapy with irrigation;2349 - Other dental restoration",
                                                                                                                                      ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2370 - Root canal, not otherwise specified",
                                                                                                                                             "2370 - Root canal, not otherwise specified;8711 - Full-mouth x-ray of teeth",
                                                                                                                                             ifelse(Procedure_new == "8711 - Full-mouth x-ray of teeth;2371 - Root canal therapy with irrigation",
                                                                                                                                                    "2371 - Root canal therapy with irrigation;8711 - Full-mouth x-ray of teeth",
                                                                                                                                                    ifelse(Procedure_new == "8712 - Other dental x-ray;2370 - Root canal, not otherwise specified",
                                                                                                                                                           "2370 - Root canal, not otherwise specified;8712 - Other dental x-ray",
                                                                                                                                                           ifelse(Procedure_new == "8712 - Other dental x-ray;2371 - Root canal therapy with irrigation",
                                                                                                                                                                  "2371 - Root canal therapy with irrigation;8712 - Other dental x-ray",
                                                                                                                                                                  Procedure_new)))))))))))))))))))

necrosispulp1 <- necrosispulp1 %>% mutate(Diagnosa = na_if(Diagnosa, ""))
necrosispulp1 <- necrosispulp1 %>% mutate(Procedure_new = na_if(Procedure_new, ""))

necrosispulp1 <- necrosispulp1[order(necrosispulp1$Nokapst,necrosispulp1$Tglplgsjp),]
necrosispulp1 <- necrosispulp1 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
necrosispulp1$jeda <- as.integer(necrosispulp1$Tgldtgsjp) - as.integer(necrosispulp1$tgl_before)
necrosispulp1$jeda <- as.numeric(necrosispulp1$jeda)

necrosispulp1 <- necrosispulp1[order(necrosispulp1$Nokapst,necrosispulp1$Tglplgsjp),]
necrosispulp1 <- necrosispulp1 %>%
  group_by(Nokapst) %>%
  mutate(kunjung_ke = sequence(n()))

necrosispulp1 <- necrosispulp1[order(necrosispulp1$Nokapst,necrosispulp1$Tglplgsjp),]
necrosispulp1 <- necrosispulp1 %>%
  group_by(Norjkawalsep) %>%
  mutate(rujuk_ke = sequence(n()))

necrosispulp1$kunjung_ke <- abs(necrosispulp1$kunjung_ke )

necrosispulp1$keterangan <- with(necrosispulp1,
                                 ifelse(is.na(necrosispulp1$Procedure_new),NA,
                                        ifelse(str_detect(Procedure_new,"Root canal|root canal"),"Perawatan saluran akar gigi",
                                               ifelse(str_detect(Procedure_new,"Restoration of tooth by filling"),"Tambal gigi",
                                                      ifelse(str_detect(Procedure_new,"dental x-ray|x-ray of teeth"),"Rontgen gigi",
                                                             ifelse(str_detect(Procedure_new,"dental restoration"),"Perbaikan gigi",
                                                                    ifelse(str_detect(Procedure_new,"Extraction|extraction"),"Pencabutan gigi",
                                                                           ifelse(str_detect(Procedure_new,"Dental examination"),"Pemeriksaan gigi",
                                                                                  ifelse(str_detect(Procedure_new,"Fitting of denture"),"Pemasangan gigi palsu",
                                                                                         "Prosedur lain")))))))))


necrosispulp2 <- necrosispulp1 %>% subset(keterangan  == "Perawatan saluran akar gigi")
necrosispulp2 <- necrosispulp2[order(necrosispulp2$Nokapst,necrosispulp2$Tglplgsjp),]
necrosispulp2 <- necrosispulp2 %>%
  group_by(Nokapst) %>%
  mutate(kunjung_ke = sequence(n()))

necrosispulp2$Kel_kunjungan  <- with(necrosispulp2,
                                     ifelse(kunjung_ke > 0 & kunjung_ke <= 5, "Kunjungan 1-5x",
                                            ifelse(kunjung_ke > 10 & kunjung_ke <= 20, "Kunjungan 11-20x",
                                                   ifelse(kunjung_ke > 20 & kunjung_ke <= 30, "Kunjungan 21-30x",
                                                          ifelse(kunjung_ke > 30 & kunjung_ke <= 40, "Kunjungan 31-40x",
                                                                 ifelse(kunjung_ke > 40 & kunjung_ke <= 50, "Kunjungan 41-50x",
                                                                        ifelse(kunjung_ke > 50, "Kunjungan >50x",
                                                                               kunjung_ke)))))))




necrosispulp2 <- necrosispulp2 %>% subset(Kel_kunjungan  != "Kunjungan 1-5x") %>%
  select(Nokapst,Norjkawalsep) %>% unique()
necrosispulp2 <- left_join(necrosispulp2,necrosispulp,by = c("Nokapst"="Nokapst")) %>%
  select(-Norjkawalsep.x,-Jenisppkperujuk,-Typeppkperujuk,-Nmppkperujuk,
         -Tglstjkeu,-Norjkawalsep.y,-Diagflag,-Flag)

necrosispulp2 <- necrosispulp2[order(necrosispulp2$Nokapst,necrosispulp2$Tglplgsjp),]
necrosispulp2 <- necrosispulp2 %>% group_by(Nokapst) %>%
  mutate(tgl_before = lag(Tglplgsjp, n=1))
necrosispulp2$jeda <- as.integer(necrosispulp2$Tgldtgsjp) - as.integer(necrosispulp2$tgl_before)
necrosispulp2$jeda <- as.numeric(necrosispulp2$jeda)


necrosispulp2 <- necrosispulp2[order(necrosispulp2$Nokapst,necrosispulp2$Tglplgsjp),]
necrosispulp2 <- necrosispulp2 %>%
  group_by(Nokapst) %>%
  mutate(kunjung_ke = sequence(n()))

necrosispulp2$Kel_kunjungan  <- with(necrosispulp2,
                                     ifelse(kunjung_ke > 0 & kunjung_ke <= 5, "Kunjungan 1-5x",
                                            ifelse(kunjung_ke > 5 & kunjung_ke <= 6, "Kunjungan 6x",
                                                   ifelse(kunjung_ke > 6 & kunjung_ke <= 7, "Kunjungan 7x",
                                                          ifelse(kunjung_ke > 7 & kunjung_ke <= 8, "Kunjungan 8x",
                                                                 ifelse(kunjung_ke > 8 & kunjung_ke <= 9, "Kunjungan 9x",
                                                                               ifelse(kunjung_ke > 9 & kunjung_ke <= 10, "Kunjungan 10x",
                                                                                      ifelse(kunjung_ke > 10 & kunjung_ke <= 12, "Kunjungan 11-12x",
                                                                                             ifelse(kunjung_ke > 12 & kunjung_ke <= 15, "Kunjungan 13-15x",
                                                                                                    ifelse(kunjung_ke > 15 & kunjung_ke <= 20, "Kunjungan 16-20x",
                                                                                                           ifelse(kunjung_ke > 20 & kunjung_ke <= 30, "Kunjungan 21-30x",
                                                                                                                  ifelse(kunjung_ke > 30 & kunjung_ke <= 40, "Kunjungan 31-40x",
                                                                                                                         ifelse(kunjung_ke > 40 & kunjung_ke <= 50, "Kunjungan 41-50x",
                                                                                                                                ifelse(kunjung_ke > 50, "Kunjungan >50x",
                                                                                                                                       "Salah"))))))))))))))

necrosispulp2$Kel_kunjungan  <- with(necrosispulp2,
                                     ifelse(kunjung_ke == 1, "Kunjungan 1x",
                                            ifelse(kunjung_ke > 1 & kunjung_ke <= 4, "Kunjungan 2-4x",
                                                   ifelse(kunjung_ke > 4 & kunjung_ke <= 6, "Kunjungan 5-6x",
                                                          ifelse(kunjung_ke > 6 & kunjung_ke <= 8, "Kunjungan 7-8x",
                                                                 ifelse(kunjung_ke > 8 & kunjung_ke <= 10, "Kunjungan 9-10x",
                                                                        ifelse(kunjung_ke > 10 & kunjung_ke <= 12, "Kunjungan 11-12x",
                                                                               ifelse(kunjung_ke > 12 & kunjung_ke <= 15, "Kunjungan 13-15x",
                                                                                      ifelse(kunjung_ke > 15 & kunjung_ke <= 20, "Kunjungan 16-20x",
                                                                                             ifelse(kunjung_ke > 20 & kunjung_ke <= 30, "Kunjungan 21-30x",
                                                                                                    ifelse(kunjung_ke > 30 & kunjung_ke <= 40, "Kunjungan 31-40x",
                                                                                                           ifelse(kunjung_ke > 40 & kunjung_ke <= 50, "Kunjungan 41-50x",
                                                                                                                  ifelse(kunjung_ke > 50, "Kunjungan >50x",
                                                                                                                         "Salah")))))))))))))


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


necrosispulp2$Nokapst <- as.factor(necrosispulp2$Nokapst)
necrosispulp3 <- necrosispulp2 %>%
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
write.xlsx(necrosispulp2, file = "K041 lebih dari 5 kali.xlsx")

write.csv(necrosispulp2, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_K041_necrosis of pulp.csv",
          na="", row.names = FALSE)
write.xlsx(necrosispulp3, file = "Normal_curve.xlsx")
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
write.csv(data0, "D://data gresik//MTF KC GRESIK//gresik_2020//ur_E119_DM tanpa komplikasi.csv",
          na="", row.names = FALSE)
#===============================SKIP======================================
#agar bisa SUBSET
data$Kdppklayan <- as.character(trimws(substr(data$Nosjp,1,8)), "both")
data <- data %>%


#========================================================================
write.xlsx(readmisi_5, file = "readmisi_rajal.xlsx")
#========================================================================
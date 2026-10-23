library(readxl)
library(lubridate)
library(writexl)
library(dplyr)

setwd("~/Desktop")

mekonnen<-read_xlsx("Mekonnen.xlsx")
template<-read_xlsx("Template.xlsx")

names(mekonnen)
names(template)
table(mekonnen$If_OI_type, useNA ="ifany")

# Select variables
mekonnen_extracted<-mekonnen %>%
  select(
    ID, Age_yr, Sex,
    Edu_status, OI, If_OI_type,
    status_not_OIs, Date_confirmed,
    statusofdate, art_started,
    Initial_Drug, pre_OI_prophy_IPT,  
    CD4count, HGB, Anemia_status,           
    weight,height                   
  )

# Identify TB
mekonnen_extracted<-mekonnen_extracted %>%
  mutate(
  tbc2 = case_when(If_OI_type %in% c("PTB", "EPTB")~1,TRUE~0)
  ) %>%
  filter(tbc2 %in% c(0,1))

# Convert dates
mekonnen_extracted<-mekonnen_extracted %>%
  mutate(Date_confirmed_clean=gsub(" UTC$", "", Date_confirmed),
  baseline_date=as.Date(Date_confirmed_clean),
  end_date=mdy(statusofdate),
  art_start_date=as.Date(art_started))

# Create template
mekonnen_template<-as.data.frame(
  matrix(
    NA,
    nrow=nrow(mekonnen_extracted),
    ncol=ncol(template)))
colnames(mekonnen_template)<-colnames(template)

# Map variables
template_map<-list(
  'Patient ID'=function(df) paste0("mekonnen_", df$ID),
  Country=function(df) "Ethiopia",
  'Age (years)'=function(df) df$Age_yr,
  'Sex (male/female)'=function(df)
    case_when(
      df$Sex == "Male"~"male",
      df$Sex == "Female"~"female",
      TRUE~NA_character_),
  
  # follow-up
  'Date of baseline evaluation'=function(df) as.character(df$baseline_date),
  'Date of outcome or last follow-up'=function(df) as.character(df$end_date),
  'Total follow-up time (days)'=function(df) {
    out<-as.numeric(df$end_date - df$baseline_date)
    ifelse(out>=0, out, NA_real_)
    },
  
  # Outcome
  'Outcome (incident TB, died, censored)'=function(df)
    case_when(
    df$If_OI_type %in% c("PTB", "EPTB")~"incident TB",
    df$status_not_OIs=="Death"~"died",
    TRUE~"censored"),

  'Censored due to lost to follow-up (yes/no)'=function(df)
    ifelse(df$status_not_OIs=="lost to follow-up", "yes", "no"),
  
  'Reason for loss to follow-up'=function(df)
    case_when(
    df$status_not_OIs=="lost to follow-up"~"lost to follow-up",
    df$status_not_OIs=="Transfer out"~"transfer out",
      TRUE~NA_character_),
  
  # ART
  'Receiving ART (yes/no)'=function(df) 
    ifelse(!is.na(df$art_start_date), "yes", "no"),
  
  'Date of ART initiation'=function(df)           
  as.character(df$art_start_date),
  
  'Type of ART'=function(df)                    
  df$Initial_Drug,
  
  'Duration of ART (days)'=function(df) {
    out<-as.numeric(df$end_date - df$art_start_date)
    ifelse(out>=0, out, NA_real_)
    },
  
  # TPT
  'Received TPT (yes/no)'=function(df)
    case_when(
    df$pre_OI_prophy_IPT==1~"yes",
    df$pre_OI_prophy_IPT==0~"no",
      TRUE~NA_character_),
  
  'Type of TPT (regimen)'=function(df)
    ifelse(df$pre_OI_prophy_IPT==1, "isoniazid (IPT)", NA),
  
  # Laboratory results
  'CD4 count at baseline (cells/µL)'=function(df)
    df$CD4count,
  
  'Hgb (g/dL)'=function(df)                       
    df$HGB,
  
  'Anemia (yes/no)'=function(df)                  
    case_when(
      df$Anemia_status==1~"yes",
      df$Anemia_status==2~"no",
      TRUE~NA_character_),
  
  # Weight and height
  'Weight (kg)'=function(df)                        
    df$weight,
  
  'Height (cm)'=function(df)                        
    df$height,
  
  'Body mass index (kg/m²)'=function(df) {
    bmi<-df$weight / (df$height/100)^2
    ifelse(is.finite(bmi), round(bmi, 2), NA)
    },
  
  # Education
  'Educational level (categorical)'=function(df)   
    df$Edu_status
)

# APPLY MAPPING
for (col in names(template_map)) {
  if (col %in% colnames(mekonnen_template)) {
    cat("\n--- Mapping column:", col, "---\n")
    mekonnen_template[[col]]<-template_map[[col]](mekonnen_extracted)
  }
}

# EXPORT
mekonnen_template<-mekonnen_template %>%
  mutate(across(everything(), as.character))

write_xlsx(
  mekonnen_template,
  "Mekonnen_TB_only_template_mapped.xlsx"
)






# Jennifer Collister
# 22/09/20

library(glue)
library(lubridate)
library(readxl)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

# Source the function definitions
# XL: Remove 'Reorganise' in the file path
source(file.path(config$scripts$cleaning, "basic_functions.R"), local = TRUE)

# makeEnum <- function(inputList) {
#   # Borrowed from https://stackoverflow.com/a/41509345
#   myEnum <- as.list(inputList)
#   enumNames <- names(myEnum)
#   if (is.null(enumNames)) {
#     names(myEnum) <- myEnum
#   } else if ("" %in% enumNames) {
#     stop("The inputList has some but not all names assigned. They must be all assigned or none assigned")
#   }
#   return(myEnum)
# }
# visits <- makeEnum(list(baseline = c("0", "baseline assessment"), 
#                     repeat_visit = c("1", "repeat visit"), 
#                     imaging = c("2", "imaging visit"), 
#                     repeat_imaging = c("3","repeat imaging visit")))

# Formatting of existing UKB variables

ID <- function() {
  list(
    name = "ID",
    source = "ID",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "ID",
    description = "The unique participant identifier"
  )
}

PsF_VisitFreq <- function(instance = visits$baseline) {
  list(
    name = glue("PsF_VisitFreq.{instance[1]}.0"),
    source = glue("PsF_VisitFreq.{instance[1]}.0"),
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = glue("Family/friend visit frequency at {instance[2]}"),
    description = glue("Frequency of family/friend visits (recorded at {instance[2]})")
  )
}

BaC_Sex <- function() {
  list(
    name = "BaC_Sex",
    source = "BaC_Sex.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "Gender",
    description = "Participant's self-reported gender"
  )
}

Rec_DateAssess <- function() {
  list(
    name = "Rec_DateAssess",
    source = c("Rec_DateAssess.0.0"),
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = "DateBaselineAssess",
    description = "Date of baseline assessment"
  )
}

Eth_Ethnicity <- function() {
  list(
    name = "Eth_Ethnicity",
    source = "Eth_Ethnicity.0.0",
    mapper = FN_factor(
      levelorder = c(
        "White",
        "British",
        "Irish",
        "Any other white background",
        "Mixed",
        "White and Black Caribbean",
        "White and Black African",
        "White and Asian",
        "Any other mixed background",
        "Asian or Asian British",
        "Indian",
        "Pakistani",
        "Bangladeshi",
        "Any other Asian background",
        "Black or Black British",
        "Caribbean",
        "African",
        "Any other Black background",
        "Chinese",
        "Other ethnic group",
        "Do not know",
        "Prefer not to answer"
      )
    ),
    post_exclusion = FALSE,
    display_name = "ethnicity",
    description = "The participant's self-reported ethnicity (raw UKB categories)"
  )
}

BaC_RsnLostFU <- function() {
  list(
    name = "BaC_RsnLostFU.0.0",
    source = "BaC_RsnLostFU.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "lfu_reason",
    description = "The reported reason for loss to follow-up"
  )
}

TEU_BaC_DateOfBirth <- function() {
  list(
    name = "TEU_BaC_DateOfBirth",
    source = c("BaC_BirthMonth.0.0", "BaC_BirthYear.0.0"),
    mapper = FN_MYtoDate(
      day = 15,
      monthField = "BaC_BirthMonth.0.0",
      yearField = "BaC_BirthYear.0.0"
    ),
    post_exclusion = FALSE,
    display_name = "DateOfBirth",
    description = "The participant's approximate date of birth, derived from self-reported month and year with date estimated as 15th"
  )
}

TEU_BaC_AgeAtRec <- function() {
  list(
    name = "TEU_BaC_AgeAtRec",
    source = c("TEU_BaC_DateOfBirth", "Rec_DateAssess"),
    mapper = function(data) {
      as.numeric(round(difftime(data[["Rec_DateAssess"]], data[["TEU_BaC_DateOfBirth"]], unit =
                                  "days") / 365.25,
                       digits = 2))
    },
    post_exclusion = FALSE,
    display_name = "Age at enrolment",
    description = "The participant's approximate age at recruitment, derived from date of assessment centre visit and self-reported month and year of birth (date of birth estimated as 15th of the month)"
  )
}

TEU_BlP_SBP.0.0 <- function() {
  list(
    name = "TEU_BlP_SBP.0.0",
    source = c("BlP_SBPAuto.0.0", "BlP_SBPMan.0.0"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.0.0"]], data[["BlP_SBPMan.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First SBP at baseline",
    description = "First SBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_SBP.0.1 <- function() {
  list(
    name = "TEU_BlP_SBP.0.1",
    source = c("BlP_SBPAuto.0.1", "BlP_SBPMan.0.1"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.0.1"]], data[["BlP_SBPMan.0.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second SBP at baseline",
    description = "Second SBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_DBP.0.0 <- function() {
  list(
    name = "TEU_BlP_DBP.0.0",
    source = c("BlP_DBPAuto.0.0", "BlP_DBPMan.0.0"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.0.0"]], data[["BlP_DBPMan.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First DBP at baseline",
    description = "First DBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_DBP.0.1 <- function() {
  list(
    name = "TEU_BlP_DBP.0.1",
    source = c("BlP_DBPAuto.0.1", "BlP_DBPMan.0.1"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.0.1"]], data[["BlP_DBPMan.0.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second DBP at baseline",
    description = "Second DBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_nSBP <- function() {
  list(
    name = "TEU_BlP_nSBP",
    source = c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. SBP",
    description = "Number of SBP measurements taken at baseline"
  )
}

TEU_BlP_nDBP <- function() {
  list(
    name = "TEU_BlP_nDBP",
    source = c("TEU_BlP_DBP.0.0", "TEU_BlP_DBP.0.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_DBP.0.0", "TEU_BlP_DBP.0.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. DBP",
    description = "Number of DBP measurements taken at baseline"
  )
}

TEU_BlP_SBP.avg <- function() {
  list(
    name = "TEU_BlP_SBP.avg",
    source = c("TEU_BlP_SBP.0.0",
               "TEU_BlP_SBP.0.1"),
    mapper = FN_average(colnames = c("TEU_BlP_SBP.0.0",
                                     "TEU_BlP_SBP.0.1")),
    post_exclusion = FALSE,
    display_name = "SBP",
    description = "The average systolic blood pressure at baseline"
  )
}

TEU_BlP_DBP.avg <- function() {
  list(
    name = "TEU_BlP_DBP.avg",
    source = c("TEU_BlP_DBP.0.0",
               "TEU_BlP_DBP.0.1"),
    mapper = FN_average(colnames = c("TEU_BlP_DBP.0.0",
                                     "TEU_BlP_DBP.0.1")),
    post_exclusion = FALSE,
    display_name = "DBP",
    description = "The average diastolic blood pressure at baseline"
  )
}

TEU_BlP_measuredHTN <- function(SBPthreshold = 140, DBPthreshold = 90) {
  list(
    name = "TEU_BlP_measuredHTN",
    source = c("TEU_BlP_SBP.avg", "TEU_BlP_DBP.avg"),
    mapper = function(data) {
      data[["TEU_BlP_SBP.avg"]] >= SBPthreshold |
        data[["TEU_BlP_DBP.avg"]] >= DBPthreshold
    },
    post_exclusion = FALSE,
    display_name = "measuredHTN",
    description = paste0(
      "Whether the participant had hypertensive BP (>=",
      SBPthreshold,
      "/",
      DBPthreshold,
      ") measured at baseline"
    )
  )
}

Alc_Status <- function() {
  list(
    name = "Alc_Status",
    source = "Alc_Status.0.0",
    mapper = FN_factor(
      levelorder = c("Never", "Previous", "Current", "Prefer not to answer")
    ),
    post_exclusion = FALSE,
    display_name = "Alc_Status",
    description = "Self-reported alcohol status"
  )
}

Smo_Status <- function() {
  list(
    name = "Smo_Status",
    source = "Smo_Status.0.0",
    mapper = FN_factor(
      levelorder = c("Never", "Previous", "Current", "Prefer not to answer")
    ),
    post_exclusion = FALSE,
    display_name = "Smo_Status",
    description = "Self-reported smoking status"
  )
}

TEU_HoH_PreTaxInc <- function() {
  list(
    name = "TEU_HoH_PreTaxInc",
    source = c("HoH_PreTaxInc.0.0", "HoH_PreTaxInc_P.0.0"),
    mapper = function(data) {
      y <- ifelse(is.na(data[["HoH_PreTaxInc.0.0"]]),
                  as.character(data[["HoH_PreTaxInc_P.0.0"]]),
                  as.character(data[["HoH_PreTaxInc.0.0"]]))
      y <- fct_collapse(
        y,
        "Less than 18,000" = "Less than 18,000",
        "18,000 to 30,999" = c("18,000 to 30,999", "18,000 to 31,000"),
        "31,000 to 51,999" = c("31,000 to 51,999", "31,000 to 52,000"),
        "52,000 to 100,000" = "52,000 to 100,000",
        "Greater than 100,000" = "Greater than 100,000",
        "Do not know" = "Do not know",
        "Prefer not to answer" = "Prefer not to answer"
      )
      y <-
        factor(
          y,
          levels = c(
            "Less than 18,000",
            "18,000 to 30,999",
            "31,000 to 51,999",
            "52,000 to 100,000",
            "Greater than 100,000",
            "Do not know",
            "Prefer not to answer"
          )
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "HouseholdIncome",
    description = "Participant's pre-tax household income"
  )
}

Sle_Duration <- function() {
  list(
    name = "Sle_Duration",
    source = "Sle_Duration.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "SleepDuration_h",
    description = "Participant's self-reported average sleep duration in hours"
  )
}

BSM_BMI<-function(){
  list(
    name = 'BSM_BMI',
    source = 'BSM_BMI.0.0',
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = 'BMI',
    description = 'Body mass index (BMI) Kg/m2'
  )
}


TEU_BSM_BMIcat <- function() {
  list(
    name = "TEU_BSM_BMIcat",
    source = "BSM_BMI.0.0",
    mapper = function(x) {
      y <-
        as.character(cut(x, breaks = c(0, 18.5, 25, 30, 200), right = FALSE))
      #y[is.na(y)] <- "Unknown"
      y <-
        factor(
          y,
          levels = c("[18.5,25)", "[0,18.5)", "[25,30)", "[30,200)"),
          labels = c("Normal", "Underweight", "Overweight", "Obese")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "BMI",
    description = "BMI below 18.5 was considered “underweight”, between 18.5 and 25 was “normal”, between 25 and 30 was “overweight” and above 30 was “obese”"
  )
}

Dem_BSM_BMIcat<-function(){
  list(
    name = "Dem_BSM_BMIcat",
    source = "BSM_BMI.0.0",
    mapper = function(x) {
      y <-
        as.character(cut(x, breaks = c(0, 25, 30, 200), right = FALSE))
      #y[is.na(y)] <- "Unknown"
      y <-
        factor(
          y,
          levels = c("[0,25)", "[25,30)", "[30,200)"),
          labels = c("Underweight/Normal", "Overweight", "Obese")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "BMI",
    description = "Same as TEU_BSM_BMIcat above but bundled normal and underweight categories."
  )
}

BSM_WaistCirc<-function(){
  list(
    name = 'BSM_WaistCirc',
    source = 'BSM_Waist.0.0',
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = 'Waist circumference (cm)',
    description = 'Waist circumference (cm)'
  )
}


TEU_BSM_WaistCircCat <- function() {
  list(
    name = "TEU_BSM_WaistCircCat",
    source = c("BSM_Waist.0.0", "BaC_Sex.0.0"),
    mapper = function(data) {
      # Categorise waist circ into labelled categories
      y <- dplyr::case_when(
        data[["BaC_Sex.0.0"]] == "Female" & data[["BSM_Waist.0.0"]] >= 88 ~ "Obese",
        data[["BaC_Sex.0.0"]] == "Female" & data[["BSM_Waist.0.0"]] >= 80 ~ "Overweight",
        data[["BaC_Sex.0.0"]] == "Male" & data[["BSM_Waist.0.0"]] >= 102 ~ "Obese",
        data[["BaC_Sex.0.0"]] == "Male" & data[["BSM_Waist.0.0"]] >= 94 ~ "Overweight",
        is.na(data[["BSM_Waist.0.0"]]) ~ "Unknown",
        TRUE ~ "Normal"
      )
      y <-
        factor(y, levels = c("Normal", "Overweight", "Obese", "Unknown"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "WaistCirc",
    description = "Categorised waist circumference.\nFemales with a waist circumference between 80 and 88cm were considered overweight, greater than 88cm was considered obese.\nMales with a waist circumference between 94 and 102cm were considered overweight, greater than 102cm was considered obese."
  )
}

PhA_METsWkAllAct <- function() {
  list(
    name = "PhA_METsWkAllAct",
    source = "PhA_METsWkAllAct.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "WeeklyMETs",
    description = paste0("Summed MET minutes per week for all activity, derived from participant self-reported weekly exercise. This variable was generated as part of ",
                         text_spec("UKB application 12184", link = "http://bmjopen.bmj.com/content/6/3/e010038"), 
                         " and made available on the Data Showcase.")
  )
}

CoF_RTTTimeID <- function() {
  list(
    name = "CoF_RTTTimeID",
    source = "CoF_RTTTimeID.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "ReactionTime",
    description = "Reaction time in a game of snap, in seconds"
  )
}

TEU_Edu_HighestQual <- function() {
  list(
    name = "TEU_Edu_HighestQual",
    source = c(paste0("Edu_Qualif.0.", seq(0, 5, by=1)),
               paste0("Edu_Qualif_p.0.", seq(0, 4, by=1))),
    mapper = function(data) {
      qual_list <- c(
        "None of the above",
        "CSEs or equivalent",
        "O levels/GCSEs or equivalent",
        "A levels/AS levels or equivalent",
        "Other professional qualifications eg: nursing, teaching",
        "NVQ or HND or HNC or equivalent",
        "College or University degree"
      )
      data<-data%>%mutate_all(~as.character(.))
      
      for(i in seq(1, length(qual_list), by=1)) {
        data[data == qual_list[i]] <- as.character(i)
      }
      y <- do.call(pmax, c(data, list(na.rm=TRUE)))
      #y[is.na(y)] <- 1
      y[y=="Prefer not to answer"]=NA
      y <- factor(y,
                  levels = seq(1, length(qual_list), by=1),
                  labels = qual_list
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Highest Qualification",
    description = "Highest of a participant's self-reported educational qualifications"
  )
}


TEU_Edu_Age<-function() {
  list(
    name="TEU_Edu_Age",
    source="Edu_Age.0.0",
    mapper=FN_toNA(values = c(-1,-3,-2)),
    post_exclusion=FALSE,
    display_name="Age completed full education",
    description="Age completed full education"
  )
}

Dem_Edu_Age<-function(){
  list(
    name="Dem_Edu_Age",
    source=c("TEU_Edu_Age","TEU_Edu_HighestQual"),
    mapper=function(data){
      
      data<-data%>%
        mutate(age_binary=factor(case_when(
          !is.na(TEU_Edu_Age) & TEU_Edu_Age<=16 ~ "Up to age 16",
          TEU_Edu_HighestQual=="College or University degree" |(!is.na(TEU_Edu_Age) & TEU_Edu_Age>16)~"Higher than age 16",
          is.na(TEU_Edu_Age) ~ NA_character_#,
          #TRUE ~ "Error"
        ),levels=c("Up to age 16","Higher than age 16")))
      
      return(data$age_binary)
      
    },
    post_exclusion=FALSE,
    display_name="Age at leaving full-time education",
    description="Whether age at leaving full-time education of participants were up to age 16 or higher (Mukadam2022)"
  )
}

Dem_Edu_Qual<-function(){
  list(
    name="Dem_Edu_Qual",
    source="TEU_Edu_HighestQual",
    mapper=function(x){
      y<-factor(case_when(is.na(x)~NA_character_,
                   x %in% c("None of the above","CSEs or equivalent") ~ "Below GCSE",
                   TRUE ~ "Equivalent or above GCSE"),levels=c("Below GCSE","Equivalent or above GCSE"))
    },
    post_exclusion=FALSE,
    display_name="Education",
    description="Whether education of participants were below GCSE (Mukadam2022)"
  )
}

GAC_AideMem <- function() {
  list(
    name = "GAC_AideMem",
    source = "GAC_AideMem.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "AideMemoir",
    description = "Did the participant bring the requested aide-memoir with a note of their medications and previous operations?"
  )
}

TEU_Rec_AssessCentre <- function() {
  list(
    name = "TEU_Rec_AssessCentre",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding10_AssessmentCentre.csv"))
      # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=10
      y <- merge(x,
                 map,
                 by.x = "x",
                 by.y = "Code",
                 all.x = TRUE)
      y <- y[["meaning"]]
    },
    post_exclusion = FALSE,
    display_name = "AssessCentre",
    description = "Which assessment centre did the participant attend"
  )
}

TEU_Rec_Country <- function() {
  list(
    name = "TEU_Rec_Country",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c(
          10003,
          11001,
          11002,
          11006,
          11007,
          11008,
          11009,
          11010,
          11011,
          11012,
          11013,
          11014,
          11016,
          11017,
          11018,
          11020,
          11021,
          11024,
          11025,
          11026,
          11027,
          11028
        ) ~ "England",
        x %in% c(11004, 11005) ~ "Scotland",
        x %in% c(11003, 11022, 11023) ~ "Wales",
        TRUE ~ x
      )
      if (!all(y %in% c("England", "Scotland", "Wales"))) {
        warning(paste0("Unrecognised centre code: ", y[!y %in% c("England", "Scotland", "Wales")]))
      }
      y <- factor(y, levels=c("England", "Scotland", "Wales"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Country of residence",
    description = "Which country does the participant live in"
  )
}

TownsendDepInd <- function(){
  list(
    name = 'TownsendDepInd',
    source = c("BaC_DeprivInd.0.0"),
    mapper = FN_id,
    post_exclusion =TRUE,
    display_name = 'Townsend Deprivation Index',
    description = 'Townsend Deprivation Index'
  )
}

TEU_TownsendDepInd_Quint <- function() {
  list(
    name = "TEU_TownsendDepInd_Quint",
    source = c("BaC_DeprivInd.0.0"),
    mapper = FN_quantiles(
      quant = 5,
      labels = c("Q1: least deprived", "Q2", "Q3", "Q4", "Q5: most deprived")
    ),
    post_exclusion = TRUE,
    display_name = "Townsend Deprivation Index, quintiles",
    description = "Quintiles of the Townsend Deprivation Index: 1st is least deprived, 5th is most deprived."
  )
}

TEU_BaC_AgeCat <- function() {
  list(
    name = "TEU_BaC_AgeCat",
    source = "TEU_BaC_AgeAtRec",
    mapper = FN_buckets(
      breaks = c(40, 50, 60, 70),
      labels = c("40-49", "50-59", "60-69"),
      right = FALSE
    ),
    post_exclusion = FALSE,
    display_name = "Age at recruitment, years",
    description = "Categorised age in years"
  )
}

# XL add: 17/11/2020
HMH_Meds_any <- function() {
  list(
    name = "HMH_Meds_any",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
    ),
    mapper = FN_HMHmeds_any_raw,
    post_exclusion = FALSE,
    display_name = "HMH_Meds",
    description = "Did the participant self-report taking medication for cholesterol, blood pressure, diabetes or HRT?"
  )
}



TEU_HMH_Meds_any <- function() {
  list(
    name = "TEU_HMH_Meds_any",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
               ),
    mapper = FN_HMHmeds_any,
    post_exclusion = FALSE,
    display_name = "HMH_Meds",
    description = "Did the participant self-report taking medication for cholesterol, blood pressure, diabetes or HRT?"
  )
}

TEU_HMH_Meds_BP <- function() {
  list(
    name = "TEU_HMH_Meds_BP",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
    ),
    mapper = FN_HMHmeds_type(medtype = "Blood pressure medication", string = "BP meds"),
    post_exclusion = FALSE,
    display_name = "Self-reported BP medication (TQ)",
    description = "Participant self-reported taking BP medication in the touchscreen questionnaire"
  )
}

Dem_HTN<-function() {
  list(
    name="Dem_HTN",
    source=c("TEU_HMH_prevHTN","TEU_HMH_Meds_BP"),
    mapper=function(data){
      # Both source variables do not have NA, but have "Unanswered" category
      data<-data%>%
        mutate(y=factor(case_when(
          TEU_HMH_prevHTN=="Unanswered" & TEU_HMH_Meds_BP=="Unanswered" ~ NA_character_,
          TEU_HMH_prevHTN=="Self-reported hypertension" | TEU_HMH_Meds_BP=="Self-reported BP meds" ~ "Yes",
          TRUE ~ "No"
        ),levels=c("No","Yes")))
      return(data$y)
    },
    post_exclusion=FALSE,
    display_name="Hypertension",
    description="Hypertension (Mukadam2022)"
  )
}


TEU_HMH_Meds_Chol <- function() {
  list(
    name = "TEU_HMH_Meds_Chol",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
    ),
    mapper = FN_HMHmeds_type(medtype = "Cholesterol lowering medication", string = "cholesterol meds"),
    post_exclusion = FALSE,
    display_name = "Cholmeds",
    description = "Participant self-reported taking cholesterol lowering medication in the touchscreen questionnaire"
  )
}

TEU_HMH_Meds_Diab <- function() {
  list(
    name = "TEU_HMH_Meds_Diab",
    source = c(paste0("HMH_MedCholBPDiabHorm.0.", c(0:3)),
               paste0("HMH_MedCholBPDiab.0.", c(0:2))
    ),
    mapper = FN_HMHmeds_type(medtype = "Insulin", string = "Insulin"),
    post_exclusion = FALSE,
    display_name = "Self-reported insulin (TQ)",
    description = "Participant self-reported taking insulin medication in the touchscreen questionnaire"
  )
}

Dem_diab<-function(){
  list(
    name="Dem_diab",
    source=c("HMH_Diabetes.0.0","TEU_HMH_Meds_Diab"),
    mapper=function(data){
      
      data<-data%>%
        mutate_all(~FN_toNA(values=c("Prefer not to answer","Do not know","Unanswered"))(.))%>%
        mutate(y=factor(case_when(
          is.na(HMH_Diabetes.0.0) & is.na(TEU_HMH_Meds_Diab) ~ NA_character_,
          HMH_Diabetes.0.0=="Yes" | TEU_HMH_Meds_Diab=="Self-reported Insulin" ~ "Yes",
          TRUE ~ "No"
        ),levels=c("No","Yes")))
      return(data$y)
    },
    post_exclusion=FALSE,
    display_name="Diabetes",
    description="Self-reported dx of diabetes or use of insulin from TQ"
  )
}


TEU_ethnicgrp <- function() {
  list(
    name = "TEU_ethnicgrp",
    source = "Eth_Ethnicity.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        x %in% c(
          "Mixed",
          "White and Black Caribbean",
          "White and Black African",
          "White and Asian",
          "Any other mixed background"
        ) ~ "Mixed",
        x %in% c("Indian", "Pakistani", "Bangladeshi") ~ "S. Asian",
        x %in% c(
          "Black or Black British",
          "Caribbean",
          "African",
          "Any other Black background"
        ) ~ "Black",
        x %in% c(
          "Other ethnic group",
          "Asian or Asian British",
          "Any other Asian background",
          "Chinese"
        ) ~ "Other",
        x %in% c("Do not know", "Prefer not to answer") ~ NA_character_,
        is.na(x) ~ NA_character_,
        TRUE ~ "Error"
      )
      y <-
        factor(
          y,
          ordered = FALSE,
          levels = c("White", "Black", "S. Asian", "Mixed",
                     "Other")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Self-reported ethnicity",
    description = "The participant's self-reported ethnicity, condensed into categories.\n'White', 'British', 'Irish' and 'Any other white background' were coded as 'White'.\n'Indian', 'Pakinstani' and 'Bangladeshi' were coded as 'S. Asian'.\n'Black or Black British', 'Carribean', 'African' and 'Any other Black background' were coded as 'Black'.\n'Mixed', 'White and Black Caribbean', 'White and Black African', 'White and Asian' and 'Any other mixed background' were coded as 'Mixed'.\n'Other ethnic group', 'Asian or Asian British', 'Any other Asian background' and 'Chinese' were coded as 'Other'"
  )
}

# Binary ethnicity variable
Ethnicity<-function(){
  list(
    name="Ethnicity",
    source="TEU_ethnicgrp",
    mapper=function(x){
      y=factor(case_when(x=="White" ~ "White",
                         is.na(x) ~ NA_character_,
                         TRUE~"Other ethnicity"),levels=c("White","Other ethnicity"))
      return(y)
    },
    post_exclusion=FALSE,
    display_name="Ethnicity",
    description="Whether participants self-reported as White ethnicity"
  )
}


# XL add: Same as above except leave 'Do not know' 'Prefer not to answer' and NA as it is 
Eth_ethnicgrp<-function() {
  list(
    name = "Eth_ethnicgrp",
    source = "Eth_Ethnicity.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        x %in% c(
          "Mixed",
          "White and Black Caribbean",
          "White and Black African",
          "White and Asian",
          "Any other mixed background"
        ) ~ "Mixed",
        x %in% c("Indian", "Pakistani", "Bangladeshi") ~ "S. Asian",
        x %in% c(
          "Black or Black British",
          "Caribbean",
          "African",
          "Any other Black background"
        ) ~ "Black",
        x %in% c(
          "Other ethnic group",
          "Asian or Asian British",
          "Any other Asian background",
          "Chinese"
        ) ~ "Other",
        x == 'Do not know' ~ 'Do not know',
        x == 'Prefer not to answer' ~ 'Prefer not to answer',
        is.na(x) ~ NA_character_,
        TRUE ~ "Error"
      )
      y <-
        factor(
          y,
          ordered = FALSE,
          levels = c("White", "Black", "S. Asian", "Mixed",
                     "Other", "Do not know", "Prefer not to answer")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "ethnic_group",
    description = "The participant's self-reported ethnicity, condensed into categories.\n'White', 'British', 'Irish' and 'Any other white background' were coded as 'White'.\n'Indian', 'Pakinstani' and 'Bangladeshi' were coded as 'S. Asian'.\n'Black or Black British', 'Carribean', 'African' and 'Any other Black background' were coded as 'Black'.\n'Mixed', 'White and Black Caribbean', 'White and Black African', 'White and Asian' and 'Any other mixed background' were coded as 'Mixed'.\n'Other ethnic group', 'Asian or Asian British', 'Any other Asian background' and 'Chinese' were coded as 'Other'"
  )
}

TEU_Alc_Status <- function() {
  list(
    name = "TEU_Alc_Status",
    source = "Alc_Status.0.0",
    mapper = FN_MissingCategory(
      missingvals = c("Prefer not to answer"),
      categ_name = "Unanswered"
    ),
    post_exclusion = FALSE,
    display_name = "AlcoholStatus",
    description = "Self-reported alcohol drinking status"
  )
}

TEU_Alc_Freq <- function() {
  list(
    name = "TEU_Alc_Freq",
    source = "Alc_Freq.0.0",
    mapper = FN_labelfactor(levels=c("Never","Special occasions only","One to three times a month",
                                     "Once or twice a week","Three or four times a week","Daily or almost daily"),
                            labels=c("Never","Special occasions only","One to three times a month",
                                     "Once or twice a week","Three or four times a week","Daily or almost daily"),
                            recodeNA="Prefer not to answer"),
    post_exclusion = FALSE,
    display_name = "Alcohol intake frequency",
    description = "Self-reported alcohol drinking frequency"
  )
}

TEU_Smo_Status <- function() {
  list(
    name = "TEU_Smo_Status",
    source = "Smo_Status.0.0",
    mapper = FN_labelfactor(levels=c("Never","Previous","Current"),
                            labels=c("Never","Previous","Current")),
    post_exclusion = FALSE,
    display_name = "Smoking status",
    description = "Self-reported smoking status"
  )
}


Dem_Smo_Status<-function(){
  list(
    name="Dem_Smo_Status",
    source="Smo_Status.0.0",
    mapper=function(x){
      # Categorise into binary variable
      y<-factor(case_when(x=="Current" ~ "Current",
                   x%in% c("Never","Previous") ~ "Non-current",
                   TRUE ~ NA_character_),levels=c("Non-current","Current"))
    },
    post_exclusion=FALSE,
    display_name="Smoking",
    description="Smoking (Mukadam2022)"
  )
}

TEU_Alc_WeeklyAlcUnits <- function() {
  list(
    name = "TEU_Alc_WeeklyAlcUnits",
    source = c(
      "Alc_RedWineWk.0.0",
      "Alc_WhiteWineWk.0.0",
      "Alc_BeerCiderWk.0.0",
      "Alc_SpiritsWk.0.0",
      "Alc_FortWineWk.0.0",
      "Alc_OtherAlcWk.0.0"
    ),
    mapper = function(data) {
      alcservings <- data
      for (alc in c(
        "Alc_RedWineWk.0.0",
        "Alc_WhiteWineWk.0.0",
        "Alc_BeerCiderWk.0.0",
        "Alc_SpiritsWk.0.0",
        "Alc_FortWineWk.0.0",
        "Alc_OtherAlcWk.0.0"
      )) {
        alcservings[[alc]][alcservings[[alc]] < 0 | is.na(alcservings[[alc]])] <-  NA
      }
      
      weekly_alcunits <-
        #	Red wine (1 glass, 125ml, ABV 12% = 1.5 units)
        (1.5 * alcservings$Alc_RedWineWk.0.0) +
        # White wine, champagne (1 glass, 125ml, ABV 12% = 1.5 units)
        (1.5 * alcservings$Alc_WhiteWineWk.0.0) +
        #	Fortified wines: e.g. sherry, port (1 measure, 50ml, ABV 20% = 1 unit)
        (1.0 * alcservings$Alc_FortWineWk.0.0) +
        #	Beer, cider including bitter, lager, stout, ale, Guinness (1 pint, 568ml, ABV 3.6% = 2 units)
        (2.0 * alcservings$Alc_BeerCiderWk.0.0) +
        #	Spirits, liquors (1 measure or shot, 25ml, ABV 40% = 1 unit)
        (1.0 * alcservings$Alc_SpiritsWk.0.0) +
        #	For "other" types of alcohol, will use alcopops as proxy ( 1 drink, 275ml, ABV 5.5% = 1.5 units)
        (1.5 * alcservings$Alc_OtherAlcWk.0.0)
      
      # Truncate alcohol consumption at upper 95th percentile
      #upper95 <- quantile(weekly_alcunits, 0.95, na.rm = TRUE)
      #weekly_alcunits[weekly_alcunits > upper95 & !is.na(weekly_alcunits)] <- upper95
      #weekly_alcunits[is.na(weekly_alcunits)] <- 0
      
      return(weekly_alcunits)
    },
    post_exclusion = FALSE,
    display_name = "WeeklyAlcUnits",
    description = "Total weekly units of alcohol, derived from self-reported average weekly consumption of each different type alcohol and truncated at the upper 95th percentile. This data was available for participants who said they drank alcohol more than once or twice a week."
  )
}

Dem_Alc_WeeklyCat<-function(){
  list(
    name="Dem_Alc_WeeklyCat",
    source=c("Alc_Freq.0.0","TEU_Alc_WeeklyAlcUnits"),
    mapper=function(data){
      # Beer and cider weekly intake was asked to people who reported drinking > one to three times a month
      # other types of alc intake (e.g. red wine) were all asked to ppl who reported drinking at least once or twice a week.
      data<-data%>%mutate(alc=factor(case_when(
        Alc_Freq.0.0=="Never" | (!is.na(TEU_Alc_WeeklyAlcUnits) &TEU_Alc_WeeklyAlcUnits==0) ~ "Drinking no alcohol",
        Alc_Freq.0.0 %in%c("Special occasions only","One to three times a month") | (!is.na(TEU_Alc_WeeklyAlcUnits) & TEU_Alc_WeeklyAlcUnits>0 &TEU_Alc_WeeklyAlcUnits<=21) ~ "<=21 units",
        !is.na(TEU_Alc_WeeklyAlcUnits) & TEU_Alc_WeeklyAlcUnits>21 ~ ">21 units",
        # People who reported drinking "one to three times a month" and above but without actually units reported.
        TRUE ~ NA_character_
      ),levels=c("Drinking no alcohol","<=21 units",">21 units")))
      
      return(data$alc)
    },
    post_exclusion=FALSE,
    display_name="Excessive alcohol intake",
    description="Excessive alcohol intake (Mukadam2022)"
  )
}


TEU_Alc_Binge <- function() {
  list(
    name = "TEU_Alc_Binge",
    source = c("TEU_Alc_WeeklyAlcUnits", "BaC_Sex"),
    mapper = function(data) {
      y <- dplyr::case_when(data[["BaC_Sex"]] == "Female" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 7 ~ TRUE,
                            data[["BaC_Sex"]] == "Male" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 14 ~ TRUE,
                            TRUE ~ FALSE)
    },
    post_exclusion = FALSE,
    display_name = "HarmfulAlcohol",
    description = "Does the patient's self-reported weekly alcohol consumption exceed the threshold for binge drinking. Data on weekly alcohol consumption was available for participants who said they drank alcohol more than once or twice a week, those who drank less frequently were not considered to have harmful alcohol consumption."
  )
}

# XL add: 19/11/2020
PhA_METsWkAllAct <- function(){
  list(
    name = 'PhA_METsWkAllAct',
    source = "PhA_METsWkAllAct.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = 'Summed_MET',
    description = 'Summed MET minutes per week for all activity'
  )
}

TEU_Pha_METsover1200 <- function() {
  list(
    name = "TEU_Pha_METsover1200",
    source = "PhA_METsWkAllAct.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        # XL add: Change 'Unknown' to 'Unanswered'
        is.na(x) ~ "Unanswered",
        x > 1200 ~ "High (MET minutes > 1200)",
        x <= 1200 ~ "Low (MET minutes <= 1200)",
        TRUE ~ "Other"
      )
      y <-
        factor(y,
               levels = c("High (MET minutes > 1200)", "Low (MET minutes <= 1200)", "Unanswered"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Sufficient_METs",
    description = paste0("Indicates whether the participant is exceeding 1200 MET minutes per week. The source variable (summed MET minutes per week for all activity, derived from participant self-reported weekly exercise) was generated as part of ",
                         text_spec("UKB application 12184", link = "http://bmjopen.bmj.com/content/6/3/e010038"), 
                         " and made available on the Data Showcase.")
  )
}

Dem_InPhA_WHO<-function(){
  list(
    name="Dem_InPhA_WHO",
    source="PhA_AboveModVigRec.0.0",
    mapper=function(x){
      y<-factor(case_when(is.na(x) ~ NA_character_,
                   x=="Yes" ~ "No",
                   x=="No" ~ "Yes"),levels=c("No","Yes"))
    },
    post_exclusion=FALSE,
    display_name="Physical inactivity",
    description="Physical inactivity (Mukadam2022)"
  )
}

TEU_Edu_ISCED <- function() {
  list(
    name = "TEU_Edu_ISCED",
    source = "TEU_Edu_HighestQual",
    mapper = function(x) {
      # Convert UKB qualification categories into ISCED education categories
      y <- dplyr::case_when(
        x == "College or University degree" ~ "5: Tertiary",
        x == "NVQ or HND or HNC or equivalent" ~ "5: Tertiary",
        x == "Other professional qualifications eg: nursing, teaching" ~ "4: Post-secondary non-tertiary",
        x == "A levels/AS levels or equivalent" ~ "2-3: Secondary",
        x == "O levels/GCSEs or equivalent" ~ "2-3: Secondary",
        x == "CSEs or equivalent" ~ "2-3: Secondary",
        x == "None of the above" ~ "1: Primary",
        x == "Prefer not to answer" ~ "Unanswered",
        is.na(x) ~ "Unanswered"
      )
      y <- factor(
        y,
        levels = c(
          "5: Tertiary",
          "4: Post-secondary non-tertiary",
          "2-3: Secondary" ,
          "1: Primary" ,
          "Unanswered"
        )
      )
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "ISCED",
    description = "ISCED category of participant's highest attained qualification"
  )
}

TEU_BlP_HTNseverity <- function() {
  list(
    name = "TEU_BlP_HTNseverity",
    source = c("TEU_BlP_SBP.avg", "TEU_BlP_DBP.avg"),
    mapper = function(data) {
      y <- dplyr::case_when(
        is.na(data[["TEU_BlP_SBP.avg"]]) |
          is.na(data[["TEU_BlP_DBP.avg"]]) ~ "Unmeasured",
        data[["TEU_BlP_SBP.avg"]] >= 180 |
          data[["TEU_BlP_DBP.avg"]] >= 110 ~ "Stage 3",
        between(data[["TEU_BlP_SBP.avg"]], 160, 180) |
          between(data[["TEU_BlP_DBP.avg"]], 100, 110) ~ "Stage 2",
        between(data[["TEU_BlP_SBP.avg"]], 140, 160) |
          between(data[["TEU_BlP_DBP.avg"]], 90, 100) ~ "Stage 1",
        TRUE ~ "Normotensive"
      )
      y <-
        factor(y,
               levels = c("Normotensive", "Stage 1", "Stage 2", "Stage 3", "Unmeasured"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Hypertension severity",
    description = "Stage I corresponds to systolic BP 140-159 mmHg or diastolic BP 90-99 mmHg, stage II mean systolic BP 160-179 mmHg or diastolic BP 100-110 mmHg, and stage III mean systolic BP>180 mmHg or diastolic BP>110 mmHg (also referred to as hypertensive crisis)."
  )
}

TEU_HMH_BowelCancerScreen <- function() {
  list(
    name = "TEU_HMH_BowelCancerScreen",
    source = "HMH_BowelSc.0.0",
    mapper = function(x) {
      y <- as.character(x)
      y[y %in% c("Prefer not to answer", "Do not know") |
          is.na(y)] <- "Unanswered"
      y <- factor(
        y,
        levels = c("Yes", "No", "Unanswered"),
        labels = c(
          "Screened for bowel cancer",
          "Not screened for bowel cancer",
          "Unanswered"
        ),
        ordered = FALSE
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "BowelCancerScreen",
    description = "Whether the individual has been screened for bowel cancer - used as a proxy for engagement with healthcare"
  )
}

TEU_FaH_CVD <- function() {
  list(
    name = "TEU_FaH_CVD",
    source = c(
      paste0("FaH_FatherIll.0.", c(0:9)),
      paste0("FaH_MotherIll.0.", c(0:10)),
      paste0("FaH_SibIll.0.", c(0:11))
    ),
    mapper = FN_FamHist(
      conditions = c("Heart disease", "High blood pressure", "Stroke"),
      label = "CVD"
    ),
    post_exclusion = FALSE,
    display_name = "FamilyHistoryCVD",
    description = "Family history of CVD (Heart disease, high blood pressure, stroke), derived by combining reported medical history of father, mother and siblings (adopted relatives were not included)"
  )
}

TEU_FaH_diab <- function() {
  list(
    name = "TEU_FaH_diab",
    source = c(
      paste0("FaH_FatherIll.0.", c(0:9)),
      paste0("FaH_MotherIll.0.", c(0:10)),
      paste0("FaH_SibIll.0.", c(0:11))
    ),
    mapper = FN_FamHist(
      conditions = c("Diabetes"),
      label = "Diab"
    ),
    post_exclusion = FALSE,
    display_name = "Family history of diabetes",
    description = "Family history of diabetes, derived by combining reported medical history of father, mother and siblings (adopted relatives were not included)"
  )
}

TEU_FaH_dem <- function() {
  list(
    name = "TEU_FaH_dem",
    source = c(
      paste0("FaH_FatherIll.0.", c(0:9)),
      paste0("FaH_MotherIll.0.", c(0:10)),
      paste0("FaH_SibIll.0.", c(0:11))
    ),
    mapper = FN_FamHist(
      conditions = c("Alzheimer's disease/dementia"),
      label = "dem"
    ),
    post_exclusion = FALSE,
    display_name = "Family history of Alzheimer's disease/dementia",
    description = "Family history of Alzheimer's disease/dementia, derived by combining reported medical history of father, mother and siblings (adopted relatives were not included)"
  )
}

TEU_FaH_PD <- function() {
  list(
    name = "TEU_FaH_PD",
    source = c(
      paste0("FaH_FatherIll.0.", c(0:9)),
      paste0("FaH_MotherIll.0.", c(0:10)),
      paste0("FaH_SibIll.0.", c(0:11))
    ),
    mapper = FN_FamHist(
      conditions = c("Parkinson's disease"),
      label = "PD"
    ),
    post_exclusion = FALSE,
    display_name = "Family history of PD",
    description = "Family history of Parkinson's disease, derived by combining reported medical history of father, mother and siblings (adopted relatives were not included)"
  )
}

VeI_PregnantNow <- function() {
  list(
    name = "VeI_PregnantNow", 
    source = c("VeI_PregnantNow.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Pregnant",
    description = "Was the participant pregnant at baseline"
  )
}

TEU_SBP_PRS <- function() {
  list(
    name = "TEU_SBP_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/htn-evangelou2018/outputs/htn-evangelou2018_PRS_QC1.rds",
                        colname="PRS_SBP"),
    post_exclusion = FALSE,
    display_name = "SBP polygenic risk score",
    description = "SBP polygenic risk score from Evangelou 2018 paper"
  )
}

TEU_DBP_PRS <- function() {
  list(
    name = "TEU_DBP_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/htn-evangelou2018/outputs/htn-evangelou2018_PRS_QC1.rds",
                        colname="PRS_DBP"),
    post_exclusion = FALSE,
    display_name = "DBP polygenic risk score",
    description = "DBP polygenic risk score from Evangelou 2018 paper"
  )
}

TEU_BP_PRS <- function() {
  list(
    name = "TEU_BP_PRS", 
    source = c("TEU_DBP_PRS", "TEU_SBP_PRS"), 
    mapper = FN_average(colnames=c("TEU_DBP_PRS", "TEU_SBP_PRS")),
    post_exclusion = FALSE,
    display_name = "Mean BP PRS score",
    description = "Average of SBP and DBP PRS scores"
  )
}

TEU_BP_PRS_quintiles <- function() {
  list(
    name = "TEU_BP_PRS_quintiles", 
    source = c("TEU_BP_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "BP PRS Quintiles",
    description = "Quintiles of the BP PRS score"
  )
}

TEU_LDL_C_PRS <- function() {
  list(
    name = "TEU_LDL_C_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/PGS000115/outputs/PGS000115_PRS_QC1.rds",
                        colname="PRS"),
    post_exclusion = FALSE,
    display_name = "LDL Cholesterol polygenic risk score",
    description = "LDL Cholesterol polygenic risk score, from Trinder 2020 paper"
  )
}

TEU_LDL_C_PRS_quintiles <- function() {
  list(
    name = "TEU_LDL_C_PRS_quintiles", 
    source = c("TEU_LDL_C_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "LDL Cholesterol PRS Quintiles",
    description = "Quintiles of the LDL Cholesterol PRS score"
  )
}

TEU_LDL_C_PRS_deciles <- function() {
  list(
    name = "TEU_LDL_C_PRS_deciles", 
    source = c("TEU_LDL_C_PRS"), 
    mapper = FN_quantiles(quant=10),
    post_exclusion = TRUE,
    display_name = "LDL Cholesterol PRS deciles",
    description = "Deciles of the LDL Cholesterol PRS score"
  )
}

TEU_LDL_C_PRS_centiles <- function() {
  list(
    name = "TEU_LDL_C_PRS_centiles", 
    source = c("TEU_LDL_C_PRS"), 
    mapper = FN_quantiles(quant=100),
    post_exclusion = TRUE,
    display_name = "LDL Cholesterol PRS centiles",
    description = "Centiles of the LDL Cholesterol PRS score"
  )
}

TEU_T2DM_PRS <- function() {
  list(
    name = "TEU_T2DM_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/diab-mahajan2018/outputs/diab-mahajan2018_PRS_sampleQC.rds",
                        colname="PRS"),
    post_exclusion = FALSE,
    display_name = "T2DM PRS",
    description = "Type 2 Diabetes Mellitus polygenic risk score from Mahajan 2018 paper"
  )
}

GPLC_std_T2DM_PRS <- function() {
  list(
    name = "GPLC_std_T2DM_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:\\TEU\\UKB33952_Data\\UKB_RAP\\Data\\20220902\\R\\genomicsplc_scores.rds",
                        colname="GPLC_stdPRS_T2D"),
    post_exclusion = FALSE,
    display_name = "Genomics PLC std T2DM PRS",
    description = "Type 2 Diabetes Mellitus polygenic risk score from Genomics PLC (standard)"
  )
}

GPLC_enh_T2DM_PRS <- function() {
  list(
    name = "GPLC_enh_T2DM_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:\\TEU\\UKB33952_Data\\UKB_RAP\\Data\\20220902\\R\\genomicsplc_scores.rds",
                        colname="GPLC_enhPRS_T2D"),
    post_exclusion = FALSE,
    display_name = "Genomics PLC enh T2DM PRS",
    description = "Type 2 Diabetes Mellitus polygenic risk score from Genomics PLC (enhanced)"
  )
}

TEU_T2DM_PRS_quintiles <- function() {
  list(
    name = "TEU_T2DM_PRS_quintiles", 
    source = c("TEU_T2DM_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "T2DM PRS Quintiles",
    description = "Quintiles of the Type 2 Diabetes Mellitus PRS score"
  )
}


TEU_T2DM_582_PRS <- function() {
  list(
    name = "TEU_T2DM_582_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:\\TEU\\UKB_Genetic_Data\\PRS_Pipeline\\prs\\projects\\diab582_Polfus\\outputs\\prs_20220118.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "T2DM 582 PRS (Polfus2021)",
    description = "Type 2 Diabetes polygenic risk score from Polfus2021"
  )
}

TEU_T2DM_582_PRS_quintiles <- function() {
  list(
    name = "TEU_T2DM_582_PRS_quintiles", 
    source = c("TEU_T2DM_582_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "T2DM 582 PRS (Polfus2021) Quintiles",
    description = "Quintiles of the Type 2 Diabetes PRS score from Polfus2021"
  )
}

TEU_T2DM_180k_PRS <- function() {
  list(
    name = "TEU_T2DM_180k_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:\\TEU\\UKB_Genetic_Data\\PRS_Pipeline\\prs\\projects\\diab180k_SinnottArmstrong\\outputs\\prs_20220118.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "T2DM 180k PRS (Sinnott-Armstrong2021)",
    description = "Type 2 Diabetes polygenic risk score from Sinnott-Armstrong2021"
  )
}

TEU_T2DM_180k_PRS_quintiles <- function() {
  list(
    name = "TEU_T2DM_180k_PRS_quintiles", 
    source = c("TEU_T2DM_180k_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "T2DM 180k PRS (Sinnott-Armstrong2021) Quintiles",
    description = "Quintiles of the Type 2 Diabetes PRS score from Sinnott-Armstrong2021"
  )
}

GPLC_std_PD_PRS <- function() {
  list(
    name = "GPLC_std_PD_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:\\TEU\\UKB33952_Data\\UKB_RAP\\Data\\20220902\\R\\genomicsplc_scores.rds",
                        colname="GPLC_stdPRS_PD"),
    post_exclusion = FALSE,
    display_name = "Genomics PLC std PD PRS",
    description = "Parkinson's disease polygenic risk score from Genomics PLC (standard)"
  )
}

TEU_dementia_PRS <- function() {
  list(
    name = "TEU_dementia_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:\\TEU\\UKB_Genetic_Data\\PRS_Pipeline\\prs\\projects\\alz39_Ebenau\\outputs\\prs_20211102.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "Dementia PRS (Ebenau2021)",
    description = "Dementia polygenic risk score from Ebenau2021 (not including APOE SNPs)"
  )
}


# XL add: 17/11/2020
HMH_VascCond <- function() {
  list(
    name = "HMH_VascCond", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_any_raw,
    post_exclusion = FALSE,
    display_name = "Vascular condition",
    description = "Did the participant self-report any vascular conditions in the touchscreen questionnaire"
  )
}



TEU_HMH_VascCond <- function() {
  list(
    name = "TEU_HMH_VascCond", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_any,
    post_exclusion = FALSE,
    display_name = "Vascular condition",
    description = "Did the participant self-report any vascular conditions in the touchscreen questionnaire"
  )
}

TEU_HMH_prevHTN <- function() {
  list(
    name = "TEU_HMH_prevHTN", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_condition(condition="High blood pressure", string="hypertension"),
    post_exclusion = FALSE,
    display_name = "Self-reported HTN on TQ",
    description = "Self-reported high blood pressure on touchscreen questionnaire"
  )
}

TEU_HMH_prevstroke <- function() {
  list(
    name = "TEU_HMH_prevstroke", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_condition(condition="Stroke", string="stroke"),
    post_exclusion = FALSE,
    display_name = "Self-reported stroke on TQ",
    description = "Self-reported prior stroke on touchscreen questionnaire"
  )
}

TEU_HMH_prevCVD <- function() {
  list(
    name = "TEU_HMH_prevCVD", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = FN_Vascular_condition(condition=c("Angina", "Heart attack"), string="cardiovascular disease"),
    post_exclusion = FALSE,
    display_name = "Self-reported CVD on TQ",
    description = "Self-reported prior angina or MI on touchscreen questionnaire"
  )
}

HMH_IllDisab <- function() {
  list(
    name = "HMH_IllDisab", 
    source = c("HMH_IllDisab.0.0"), 
    mapper = FN_factor(levelorder=c("No", "Yes", "Do not know", "Prefer not to answer")),
    post_exclusion = FALSE,
    display_name = "Self-reported illness or disability",
    description = "Participant self-reported longstanding illness, disability or infirmity on the touchscreen questionnaire"
  )
}

HMH_Diabetes <- function() {
  list(
    name = "HMH_Diabetes", 
    source = c("HMH_Diabetes.0.0"), 
    mapper = FN_factor(levelorder=c("No", "Yes", "Do not know", "Prefer not to answer")),
    post_exclusion = FALSE,
    display_name = "Self-reported diabetes",
    description = "Participant self-reported diabetes on the touchscreen questionnaire"
  )
}

HMH_DiabetesAge <- function() {
  list(
    name = "HMH_DiabetesAge",
    source = c("HMH_DiabetesAge.0.0"),
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Self-reported age at diagnosis of diabetes",
    description = "Participant self-reported age at diagnosis of diabetes on the touchscreen questionnaire"
  )
}

HMH_Insulin1Yr <- function(){
  list(
    name = "HMH_Insulin1Yr", 
    source = c("HMH_Insulin1Yr.0.0"), 
    mapper = FN_factor(levelorder=c("No", "Yes", "Do not know", "Prefer not to answer")),
    post_exclusion = FALSE,
    display_name = "Self-report insulin <12 months post diagnosis",
    description = "Started insulin within one year diagnosis of diabetes on the touchscreen questionnaire"
  )
}

HMH_HTNAge <- function() {
  list(
    name = "HTN_HTNAge", 
    source = c("HMH_HBPAge.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Age HTN diagnosed",
    description = "Participant self-reported age of hypertension diagnosis on the touchscreen questionnaire"
  )
}

# XL add: 17/11/2020
HMH_BowelSc <- function(){
  list(
    name='HMH_BowelSc',
    source=c('HMH_BowelSc.0.0'),
    mapper= FN_factor(levelorder=c("No", "Yes", "Do not know", "Prefer not to answer")),
    post_exclusion=FALSE,
    display_name='Self-reported ever had bowel cancer screening',
    description='Participant self-reported ever had bowel cancer screening on the touchscreen questionnaire'
  )
}

BBC_CHOL_Result <- function() {
  list(
    name = "BBC_CHOL_Result", 
    source = c("BBC_CHOL_Result.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Total cholesterol level at baseline",
    description = "Total cholesterol assay from baseline blood serum"
  )
}

Dem_HighChol<-function() {
  list(
    name="Dem_HighChol",
    source="BBC_CHOL_Result.0.0",
    mapper=function(x){
      y <-
        as.character(cut(x, breaks = c(0, 6.5, 30), right = TRUE))
      #y[is.na(y)] <- "Unknown"
      y <-
        factor(
          y,
          levels = c("(0,6.5]", "(6.5,30]"),
          labels = c("Normal", "High")
        )
      return(y)
    },
    post_exclusion=FALSE,
    display_name="High total cholesterol",
    description="High total cholesterol (> 6.5 mmol/L)"
  )
}

BBC_HDL_Result <- function() {
  list(
    name = "BBC_HDL_Result", 
    source = c("BBC_HDL_Result.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "HDL cholesterol level at baseline",
    description = "HDL cholesterol assay from baseline blood serum"
  )
}

BBC_LDL_Result <- function() {
  list(
    name = "BBC_LDL_Result", 
    source = c("BBC_LDL_Result.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "LDL cholesterol level at baseline",
    description = "LDL cholesterol assay from baseline blood serum"
  )
}

BBC_HBA1C_Result <- function(){
  list(
    name = "BBC_HBA1C_Result", 
    source = c("BBC_HBA1C_Result.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "HbA1c at baseline",
    description = "Glycated haemoglobin (HbA1c) from baseline blood serum"
  )
}

BBC_HBA1C_ResultFU <- function(){
  list(
    name = "BBC_HBA1C_ResultFU", 
    source = c("BBC_HBA1C_Result.1.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "HbA1c at FU",
    description = "Glycated haemoglobin (HbA1c) from follow-up blood serum"
  )
}

GeP_PC <- function(pc=1) {
  list(
    name = paste0("GeP_PC_", pc), 
    source = glue("GeP_PC.0.{pc}"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = glue("Principal component {pc}"),
    description = glue("Genetic principal component {pc}, from Bycroft")
  )
}

GPLC_PC <- function(pc=0) { #Note that Genomics PLC PC starts with 0, which is different from UKB PC
  list(
    name = paste0("GPLC_PC_", pc), 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:\\TEU\\UKB33952_Data\\UKB_RAP\\Data\\20220902\\R\\genomicsplc_scores.rds",
                        colname=paste0("GPLC_principalcomponents.",pc)),
    post_exclusion = FALSE,
    display_name = glue("GPLC principal component {pc}"),
    description = glue("Genetic principal component {pc}, from Genomics PLC")
  )
}

GeP_Batch <- function() {
  list(
    name = "GeP_Batch", 
    source = "GeP_Batch.0.0", 
    mapper = FN_toNumeric,
    post_exclusion = FALSE,
    display_name = "Genotype measurement batch",
    description = "Genotype measurement batch"
  )
}

GeP_Array <- function() {
  list(
    name = "GeP_Array", 
    source = "GeP_Batch.0.0", 
    mapper = function(x){
      coding <- read.csv_kdrive(file.path(config$cleaning$coding, "coding22000_flat_GenotypingArray.csv"))
      y <- factor(coding$L0[match(x, coding$Code)], levels=c("Axiom", "BiLEVE"))
    },
    post_exclusion = FALSE,
    display_name = "Genotype array",
    description = "Genotype array - UK BiLEVE or Biobank Axiom Array"
  )
}



GeP_ethnic <- function() {
  list(
    name = "GeP_ethnic", 
    source = "GeP_ethnic.0.0", 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Genotype ethnic grouping",
    description = "Genotype ethnic grouping"
  )
}


GeP_UsedInPCA <- function() {
  list(
    name = "GeP_UsedInPCA", 
    source = c("GeP_UsedInPCA.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Outliers <- function() {
  list(
    name = "GeP_Outliers", 
    source = c("GeP_Outliers.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Plate <- function() {
  list(
    name = "GeP_Plate", 
    source = c("GeP_Plate.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Sex <- function() {
  list(
    name = "GeP_Sex", 
    source = c("GeP_Sex.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Kinship <- function() {
  list(
    name = "GeP_Kinship", 
    source = c("GeP_Kinship.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Missingness <- function() {
  list(
    name = "GeP_Missingness", 
    source = c("GeP_Missingness.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

TEU_VeI_HTN_prevalent <- function(dx_codes = c(1065, 1072)) {
  list(
    list(
      name = "TEU_VeI_HTN",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported HTN, verbal interview",
      description = "Whether hypertension was reported in verbal interview at baseline"
    ),
    list(
      name = "TEU_VeI_HTN_dur",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0, 
                                 return_label = "duration",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported duration of hypertension, verbal interview",
      description = "Duration of hypertension reported in verbal interview at baseline"
    )
  )
}

TEU_selfrepHTN_dx <- function() {
  list(
    name = "TEU_selfrepHTN_dx", 
    source = c("TEU_VeI_HTN", "TEU_HMH_prevHTN"), 
    mapper = function(data) {
      VI <- !is.na(data[["TEU_VeI_HTN"]])
      TQ <- (data[["TEU_HMH_prevHTN"]] == "Self-reported hypertension")
      y <- (VI | TQ)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Self-reported hypertension",
    description = "Whether the participant self-reported a previous diagnosis of hypertension, either in the touchscreen questionnaire or verbal interview"
  )
}

TEU_selfrepHTN_meds <- function() {
  list(
    name = "TEU_selfrepHTN_meds", 
    source = c("TEU_VeI_HTNmeds_rubric", "TEU_HMH_Meds_BP"), 
    mapper = function(data) {
      VI <- data[["TEU_VeI_HTNmeds_rubric"]]
      TQ <- (data[["TEU_HMH_Meds_BP"]] == "Self-reported BP meds")
      y <- (VI | TQ)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Self-reported hypertension medication",
    description = "Whether the participant self-reported that they were taking 'blood pressure medication' either in the touchscreen questionnaire or verbal interview"
  )
}

TEU_VeI_HTNmeds_rubric <- function() {
  list(
    name = "TEU_VeI_HTNmeds_rubric",
    source = "ID",
    mapper = function(x) {
      rubric <- readRDS("K:\\TEU\\2021\\Hypertension\\Git_Repo\\Data\\Derived\\HTNMedsRubric.rds")
      y <- rubric[["HTN_probablemeds"]][match(x, rubric$ID)]
    },
    post_exclusion = FALSE,
    display_name = "Self-reported BP meds in VI",
    description = "Self-reported medications that correspond to a hypertension treatment pathway under our rubric"
  )
}


TEU_evidenceHTN <- function() {
  list(
    name = "TEU_evidenceHTN", 
    source = c("TEU_selfrepHTN_dx", "TEU_selfrepHTN_meds", "TEU_BlP_measuredHTN"), 
    mapper = function(data) {
      
      # XL: Below assigns the combo of FALSE, FALSE, NA as NA, we want FALSE instead.
      #y <- (data[["TEU_selfrepHTN_dx"]] | data[["TEU_selfrepHTN_meds"]] | data[["TEU_BlP_measuredHTN"]])
      data<-data%>%
        mutate(
          y=factor(case_when(
            is.na(TEU_selfrepHTN_dx) & is.na(TEU_selfrepHTN_meds) & is.na(TEU_BlP_measuredHTN) ~ NA_character_,
            TEU_selfrepHTN_dx==TRUE | TEU_selfrepHTN_meds==TRUE | TEU_BlP_measuredHTN==TRUE ~ "Yes",
            TRUE ~ "No"
          ),levels=c("No","Yes"))
        )
      return(data$y)
    },
    post_exclusion = FALSE,
    display_name = "Hypertension",
    description = "Did the participant have evidence of hypertension, defined as self-reported HTN, self-reported HTN meds, or measured HTN at baseline"
  )
}

TEU_awareHTN <- function() {
  list(
    name = "TEU_awareHTN", 
    source = c("TEU_selfrepHTN_dx", "TEU_selfrepHTN_meds", "TEU_evidenceHTN"), 
    mapper = function(data) {
      y <- (data[["TEU_selfrepHTN_dx"]] | data[["TEU_selfrepHTN_meds"]])
      y[(data[["TEU_evidenceHTN"]] == FALSE | is.na(data[["TEU_evidenceHTN"]]))] <- NA
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Awareness of hypertension",
    description = "Was the participant aware of their hypertension, defined as self-reported HTN or self-reported HTN meds among those with evidence of hypertension"
  )
}

TEU_treatedHTN <- function() {
  list(
    name = "TEU_treatedHTN", 
    source = c("TEU_selfrepHTN_meds", "TEU_awareHTN"), 
    mapper = function(data) {
      y <- data[["TEU_selfrepHTN_meds"]]
      y[(data[["TEU_awareHTN"]] == FALSE | is.na(data[["TEU_awareHTN"]]))] <- NA
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Treated hypertension",
    description = "Was the participant taking treatment for their hypertension, defined as self-reported HTN meds among those who were aware of their hypertension"
  )
}

TEU_VeI_CVD_prevalent <- function() {
  list(
    list(
      name = "TEU_VeI_CVD_prevalent_type",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = c(1065, 1074),
                                    colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Category of CVD reported in verbal interview at baseline",
      description = "Type of CVD"
    ),
    list(
      name = "TEU_VeI_CVD_prevalent_dur",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = c(1065, 1074),
                                    colname = "VeI_NonCancer",
                                   instance = 0, 
                                   return_label = "duration",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Duration of CVD reported in verbal interview at baseline",
      description = "Time since CVD diagnosis"
    )
  )
}

Dem_VeI_strokeTIA_prevalent <- function() {
    list(
      name = "Dem_VeI_strokeTIA_prevalent",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = c(1086,1491,1583,1081,1082),
                                 colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Stroke or TIA reported in verbal interview at baseline",
      description = "Prior stroke/TIA"
    )
}

TEU_HES_CVD_prevalent <- function(ICD10_codes, ICD9_codes) {
  list(
    list(
      name = "TEU_HES_CVD_prevalent_type",
      source = c("ID", "Rec_DateAssess",
                 paste0("HES_ICD10Diag.0.", seq(0, 65, by=1)),
                 paste0("HES_ICD9Diag.0.", seq(0, 27, by=1))),
      mapper = FN_HES_first(ICD10_codes = ICD10_codes,
                            ICD9_codes = ICD9_codes,
                            instance = 0,
                            return_label = "dx",
                            record_level = FALSE),
      post_exclusion = FALSE,
      display_name = "Category of CVD recorded in HES data prior to baseline",
      description = "Type of CVD"
    ),
    list(
      name = "TEU_HES_CVD_prevalent_dur",
      source = c("ID", "Rec_DateAssess",
                 paste0("HES_ICD10Diag.0.", seq(0, 65, by=1)),
                 paste0("HES_ICD9Diag.0.", seq(0, 27, by=1))),
      mapper = FN_HES_first(ICD10_codes = ICD10_codes,
                            ICD9_codes = ICD9_codes,
                            instance = 0,
                            return_label = "duration",
                            record_level = FALSE),
      post_exclusion = FALSE,
      display_name = "Duration of CVD recorded in HES data prior to baseline",
      description = "Time since CVD diagnosis"
    )
  )
}

ADO_DateFirstMI <- function() {
  list(
    name = "ADO_DateFirstMI", 
    source = c("ADO_DateFirstMI.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Date of first MI",
    description = paste0("Date of first myocardial infarction, algorithmically defined by UKB. See ",
                         text_spec("Resource 461", link = "https://biobank.ndph.ox.ac.uk/showcase/showcase/docs/alg_outcome_mi.pdf"),
                         " for more information. Note values of 1900-01-01 are unknown.")
  )
}

ADO_DateFirstIStroke <- function() {
  list(
    name = "ADO_DateFirstIStroke", 
    source = c("ADO_DateFirstIStroke.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Date of first ischaemic stroke",
    description = paste0("Date of first ischaemic stroke, algorithmically defined by UKB. See ",
                         text_spec("Resource 462", link = "https://biobank.ndph.ox.ac.uk/showcase/showcase/docs/alg_outcome_stroke.pdf"),
                         " for more information. Note values of 1900-01-01 are unknown.")
  )
}

TEU_VeI_CVD_operation <- function(dx_codes) {
  list(
    list(
      name = "TEU_VeI_CVD_operation_type",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_OperationCode.0.", seq(0, 31, by=1)),
                 paste0("VeI_OperationYear.0.", seq(0, 31, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                    colname = "VeI_Operation",
                                    instance = 0,
                                    return_label = "dx",
                                    mapper = file.path(config$cleaning$coding,"coding5_flat_Operation.csv")
                                    ),
      post_exclusion = FALSE,
      display_name = "Category of CVD operation reported in verbal interview at baseline",
      description = "Type of CVD operation"
    ),
    list(
      name = "TEU_VeI_CVD_operation_dur",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_OperationCode.0.", seq(0, 31, by=1)),
                 paste0("VeI_OperationYear.0.", seq(0, 31, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                    colname = "VeI_Operation",
                                    instance = 0, 
                                    return_label = "duration",
                                    mapper = file.path(config$cleaning$coding,"coding5_flat_Operation.csv")
                                    ),
      post_exclusion = FALSE,
      display_name = "Duration of CVD operation reported in verbal interview at baseline",
      description = "Time since CVD operation"
    )
  )
}

# XL add: 19/11/2020
TEU_LDLctrl_v1 <- function(threshold=3) {
  list(
    name = "TEU_LDLctrl_v1", 
    source = c("BBC_LDL_Result.0.0"), 
    mapper = function(x) {
      y <- ifelse(x<threshold, 1, 0)
      y <- factor(as.numeric(y), levels=c(0,1), 
                  labels=c("Not controlled", "Controlled"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "LDL control status",
    description = paste0("Whether participant had LDL controlled (i.e. LDL<",threshold,"mmol/L at baseline)")
  )
}

# XL add: statin taken status and number of statin taken at baseline
TEU_VeI_statin <- function(){
  list(
    list(
      name = 'TEU_VeI_statin',
      source = c('ID',paste0("VeI_MedCode.0.",c(0:47))),
      mapper = FN_VImed_filtercodes(med_codes = c(1140861958, 1140888594, 1140888648,
                                                  1141146234, 1141192410),
                                    med_name = 'statin',
                                    colname = "VeI_Med", 
                                    instance = 0,
                                    return_label = 'statin',
                                    mapper = file.path(config$cleaning$coding,'coding4_Treatments.csv')),
      post_exclusion = FALSE,
      display_name = 'Statin taken status at baseline',
      description = 'Whether people self reported taking statin at baseline during verbal interview (statin includes simvastatin, fluvastatin, pravastatin, atorvastatin, rosuvastatin)'
    ),
    list(
      name = 'TEU_VeI_statin_num',
      source = c('ID',paste0("VeI_MedCode.0.",c(0:47))),
      mapper = FN_VImed_filtercodes(med_codes = c(1140861958, 1140888594, 1140888648,
                                                  1141146234, 1141192410),
                                    med_name = 'statin',
                                    colname = "VeI_Med", 
                                    instance = 0,
                                    return_label = 'statin_num',
                                    mapper = file.path(config$cleaning$coding,'coding4_Treatments.csv')),
      post_exclusion = FALSE,
      display_name = 'Number of statin taken at baseline',
      description = 'Number of statin people self reported taking at baseline during verbal interview'
    )
  )
  
}

# XL add: serious comorbidities (exclusion criteria)
TEU_VeI_seriouscomb <- function(){
  list(
    name = 'TEU_VeI_seriouscomb',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = function(data){
      # read in finalised excel mapping
      noncancer=read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))
      # Coding list for serious comorbidities
      serious_comb<-noncancer[which(noncancer$Exclude=='Yes'),]$coding
      y<- FN_VI_filtercodes(dx_codes = serious_comb,
                        colname = "VeI_NonCancer",
                        instance = 0,
                        return_label = "dx",
                        mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
      
      return(y)
    },      
    post_exclusion = FALSE,
    display_name = "Type of serious comorbidities",
    description = "Category of serious comorbidities reported in verbal interview at baseline"
  )
}

# XL add: cancer except skin cancer (exclusion criteria)
TEU_VeI_cancer<- function(){
  list(
    name = 'TEU_VeI_cancer',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_CancerCode.0.", c(0:5)),
               paste0("VeI_CancerYear.0.",c(0:5))),
    mapper =function(data){
      
      cancer_mapper=read.csv_kdrive(file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))
      cancer_codes=cancer_mapper[-which(cancer_mapper$L0%in% c('skin cancer')),]$Code
      
      y<- FN_VI_filtercodes(dx_codes = cancer_codes,
                            colname = "VeI_Cancer",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding3_flat_Cancer.csv"))(data)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Type of cancer (except skin cancer)',
    description = 'Category of cancer (except skin cancer) reported in verbal interview at baseline'
  )
}


# XL add: each comorbidity condition
## CVD
TEU_VeI_CVD <- function(condition='CVD'){
  list(
    name = 'TEU_VeI_CVD',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
  ,
  post_exclusion=FALSE,
  display_name = 'CVD status at baseline (VI)',
  description = 'Whether participant self reported CVD at baseline in verbal interview'
  )
}


## diabetes
TEU_VeI_diab <- function(condition='diabetes'){
  list(
    name = 'TEU_VeI_diab',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'diabetes status at baseline (VI)',
    description = 'Whether participant self reported diabetes at baseline in verbal interview'
  )
}

## Prevalent Diabetes: TEU_VeI_diab above was defined using HTN mapping whereas below was based on Eastwood2016
Estw_VeI_diab_prevalent <- function(dx_codes = c(1220, 1223, 1222, 1521, 1221)){
  list(
    list(
      name = "Estw_VeI_diab",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported diabetes, verbal interview",
      description = "Types of diabetes self-reported in verbal interview at baseline"
    ),
    list(
      name = "Estw_VeI_diab_age",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerAge.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0, 
                                 return_label = "Age",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported age at diagnosis of diabetes, verbal interview",
      description = "Age at diagnosis of diabetes reported in verbal interview at baseline"
    )
  )
  
}


TEU_VeI_T2DM_prevalent <- function(dx_codes = c(1220, 1223)) {
  list(
    list(
      name = "TEU_VeI_T2DM",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported Type 2 diabetes, verbal interview",
      description = "Whether type 2 diabetes was reported in verbal interview at baseline"
    ),
    list(
      name = "TEU_VeI_T2DM_age",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerAge.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0, 
                                 return_label = "Age",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported age at diagnosis of Type 2 diabetes, verbal interview",
      description = "Age at diagnosis of Type 2 diabetes reported in verbal interview at baseline"
    )
  )
}

# ## Type 2 diabetes
# TEU_VeI_T2DM <- function(condition='T2DM'){
#   list(
#     name = 'TEU_VeI_T2DM',
#     source = c("ID", "Rec_DateAssess",
#                paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
#                paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
#     mapper = FN_VI_comorb(condition=condition,
#                           returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'coding6_flat_NonCancerIllness.xlsx'))%>%
#                             rename(Conditions=T2DM))
#     ,
#     post_exclusion=FALSE,
#     display_name = 'Type 2 diabetes status at baseline (VI)',
#     description = 'Whether participant self reported type 2 diabetes at baseline in verbal interview'
#   )
# }

## Type 1 diabetes
TEU_VeI_T1DM_prevalent <- function(dx_codes=c(1222)){
  list(
    list(
      name = "TEU_VeI_T1DM",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported Type 1 diabetes, verbal interview",
      description = "Whether type 1 diabetes was reported in verbal interview at baseline"
    ),
    list(
      name = "TEU_VeI_T1DM_age",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerAge.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0, 
                                 return_label = "Age",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported age at diagnosis of Type 1 diabetes, verbal interview",
      description = "Age at diagnosis of Type 1 diabetes reported in verbal interview at baseline"
    )
  )
  
}

## Gestational diabetes
TEU_VeI_gest_prevalent <- function(dx_codes=c(1221)){
  list(
    list(
      name = "TEU_VeI_gest",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported gestational diabetes, verbal interview",
      description = "Whether gestational diabetes was reported in verbal interview at baseline"
    ),
    list(
      name = "TEU_VeI_gest_age",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerAge.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0, 
                                 return_label = "Age",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported age at diagnosis of gestational diabetes, verbal interview",
      description = "Age at diagnosis of gestational diabetes reported in verbal interview at baseline"
    )
  )
  
}

## afib or aflutter
TEU_VeI_arrhy <- function(condition="afib or aflutter"){
  list(
    name = 'TEU_VeI_arrhy',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Arrhythmia status at baseline (VI)',
    description = 'Whether participant self reported Arrhythmia (afib/flutter) at baseline in verbal interview'
  )
}

## Osteoarthritis
TEU_VeI_osteo <- function(condition="Osteoarthritis"){
  list(
    name = 'TEU_VeI_osteo',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Osteoarthritis status at baseline (VI)',
    description = 'Whether participant self reported Osteoarthritis at baseline in verbal interview'
  )
}



## Other joint disorder
TEU_VeI_joint <- function(condition="Other joint disorder"){
  list(
    name = 'TEU_VeI_joint',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Other joint disorder status at baseline (VI)',
    description = 'Whether participant self reported other joint disorder at baseline in verbal interview'
  )
}


## Epilepsy
TEU_VeI_epil <- function(condition="epilepsy"){
  list(
    name = 'TEU_VeI_epil',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Epilepsy status at baseline (VI)',
    description = 'Whether participant self reported epilepsy at baseline in verbal interview'
  )
}

## Migraine
TEU_VeI_mig <- function(condition="migraine"){
  list(
    name = 'TEU_VeI_mig',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Migraine status at baseline (VI)',
    description = 'Whether participant self reported migraine at baseline in verbal interview'
  )
}


## Anxiety or stress
TEU_VeI_anx <-function(condition="anxiety or stress"){
  list(
    name = 'TEU_VeI_anx',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Anxiety or stress status at baseline (VI)',
    description = 'Whether participant self reported anxiety or stress at baseline in verbal interview'
  )
}

## depression or bipolar
TEU_VeI_dep <-function(condition="depression or bipolar"){
  list(
    name = 'TEU_VeI_dep',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Depression or bipolar status at baseline (VI)',
    description = 'Whether participant self reported depression or bipolar at baseline in verbal interview'
  )
}


## asthma or COPD
TEU_VeI_asthCOPD <- function(condition="asthma or COPD"){
  list(
    name = 'TEU_VeI_asthCOPD',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_comorb(condition=condition,
                          returned_mapping = read.xlsx_kdrive(file.path(config$cleaning$mapping,'UKBHtn_NonCancerIllness_Mapping.xlsx'))%>%
                            rename(Conditions=ComorbidityCondition))
    ,
    post_exclusion=FALSE,
    display_name = 'Asthma or COPD status at baseline (VI)',
    description = 'Whether participant self reported asthma or COPD at baseline in verbal interview'
  )
}


# XL add: Number of comorbidities
HTN_comorb_num<- function(){
  list(
    name = 'HTN_comorb_num',
    source = c('TEU_VeI_CVD','TEU_VeI_diab','TEU_VeI_arrhy','TEU_VeI_asthCOPD','TEU_VeI_mig','TEU_VeI_epil',
               'TEU_VeI_anx','TEU_VeI_dep','TEU_VeI_osteo','TEU_VeI_joint'),
    mapper = function(data){
      rowSums(sapply(select(data,everything()),function(x) grepl("Yes",x)))
    },
    post_exclusion = FALSE,
    display_name = 'Number of comorbidities',
    description = 'Number of comorbidities self-reported at baseline'
  )
}

HTN_comorb_numcat<- function(){
  list(
    name = 'HTN_comorb_numcat',
    source = c('HTN_comorb_num'),
    mapper = function(x){
      factor(ifelse(x>=3,'>=3',x),levels = c('0','1','2','>=3'),ordered = FALSE)
    },
    post_exclusion = FALSE,
    display_name = 'Number of comorbidities (Categorical)',
    description = 'Number of comorbidities participants self-reported at baseline'
  )
}

Prosp_comorb_num<- function(){
  list(
    name = 'Prosp_comorb_num',
    source = c('TEU_VeI_diab','TEU_VeI_arrhy','TEU_VeI_asthCOPD','TEU_VeI_mig','TEU_VeI_epil',
               'TEU_VeI_anx','TEU_VeI_dep','TEU_VeI_osteo','TEU_VeI_joint'),
    mapper = function(data){
      rowSums(sapply(select(data,everything()),function(x) grepl("Yes",x)))
    },
    post_exclusion = FALSE,
    display_name = 'Number of comorbidities',
    description = 'Number of comorbidities self-reported at baseline (CVD not included as comorbidity)'
  )
}

Prosp_comorb_numcat<- function(){
  list(
    name = 'Prosp_comorb_numcat',
    source = c('Prosp_comorb_num'),
    mapper = function(x){
      factor(ifelse(x>=3,'>=3',x),levels = c('0','1','2','>=3'),ordered = FALSE)
    },
    post_exclusion = FALSE,
    display_name = 'Number of comorbidities (Categorical)',
    description = 'Number of comorbidities participants self-reported at baseline (CVD not included as comorbidity)'
  )
}


# From JC: Job variable 
TEU_Emp_CurrStat <- function() {
  list(
    name = "TEU_Emp_CurrStat",
    source = c("Emp_CurrStatUnc.0.0", "Emp_CurrStat.0.0"),
    mapper = function(data) {
      # XL change: Changed the order of uncorrected and corrected
      y <- factor(coalesce(as.character(data[["Emp_CurrStat.0.0"]]), 
                           as.character(data[["Emp_CurrStatUnc.0.0"]])),
                  ordered=FALSE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Employment status",
    description = "Employment status at baseline, derived by taking the employment status from TQ and applying corrections made by UKB in light of participant jobs self-reported in VI"
  )
}

TEU_Emp_JobCode_v2 <- function() {
  list(
    name = "Emp_JobCode.0.0",
    source = "ID",
    mapper = function(x) {
      v2_emp <- DBfunc$DB_extract(extract_cols = c("ID", "Emp_JobCode.0.0"),
                                  db = "K:/TEU/UKB33952_Data/Data_Downloads/V2_database_duckdb0.2.1/ukb_v2.db",
                                  name_map = "K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv")
      y <- v2_emp[["Emp_JobCode.0.0"]][match(x, v2_emp$ID)]
      y <- as.numeric(y)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Job code",
    description = "Job code of participant at baseline self-reported in verbal interview"
  )
}

TEU_HTN_Emp_category <- function() {
  list(
    name = "TEU_Emp_category",
    source = c("Emp_JobCode.0.0", "TEU_Emp_CurrStat"),
    mapper = function(data) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding2_flat_Employment.csv"))
      data <- left_join(data, map[,c("Code", "L0")], by=c("Emp_JobCode.0.0" = "Code"))
      data$TEU_EmpCat <- coalesce(as.character(data[["L0"]]), 
                                  as.character(data[["TEU_Emp_CurrStat"]]))
      
      y <- dplyr::case_when(
        is.na(data$TEU_EmpCat) ~ "Unemployed/unanswered",
        data$TEU_EmpCat %in% c("Managers and Senior Officials", "Professional Occupations",
                               "Associate Professional and Technical Occupations",
                               "Administrative and Secretarial Occupations") ~ "White collar",
        data$TEU_EmpCat == "Skilled Trades Occupations" ~ "Skilled trades",
        data$TEU_EmpCat %in% c("Personal Service Occupations",
                               "Sales and Customer Service Occupations") ~ "Services",
        data$TEU_EmpCat %in% c("Process, Plant and Machine Operatives",
                               "Elementary Occupations") ~ "Blue collar",
        data$TEU_EmpCat %in% c("Other job (free text entry)",
                               "In paid employment or self-employed") ~ "Other employment",
        data$TEU_EmpCat == "Retired" ~ "Retired",
        data$TEU_EmpCat == "Unable to work because of sickness or disability" ~ "Disability",
        data$TEU_EmpCat %in% c("Looking after home and/or family",
                               "Unemployed", "Full or part-time student",
                               "Doing unpaid or voluntary work",
                               "None of the above", "Prefer not to answer") ~ "Unemployed/unanswered",
        TRUE ~ "Error?"
      )
      y <- factor(y, 
                  levels=c("White collar", "Skilled trades", "Services", 
                           "Blue collar", "Other employment", "Retired", 
                           "Disability", "Unemployed/unanswered"), 
                  labels=c("Professional and Administrative", "Skilled trades", "Services", 
                           "Manual and Industrial", "Other employment", "Retired", 
                           "Unable to work because of sickness or disability", 
                           "Unemployed/unanswered"),
                  ordered=FALSE)
      
    },
    post_exclusion = FALSE,
    display_name = "Employment category",
    description = "Employment category at baseline, collapsed from job codes self-reported in verbal interview"
  )
}

# XL add: Same as above except leave NA as it is and leave prefer not to answer as it is
Emp_category <- function() {
  list(
    name = "Emp_category",
    source = c("Emp_JobCode.0.0", "TEU_Emp_CurrStat"),
    mapper = function(data) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding2_flat_Employment.csv"))
      data <- left_join(data, map[,c("Code", "L0")], by=c("Emp_JobCode.0.0" = "Code"))
      data$TEU_EmpCat <- coalesce(as.character(data[["L0"]]), 
                                  as.character(data[["TEU_Emp_CurrStat"]]))
      
      y <- dplyr::case_when(
        is.na(data$TEU_EmpCat) ~ NA_character_,
        data$TEU_EmpCat %in% c("Managers and Senior Officials", "Professional Occupations",
                               "Associate Professional and Technical Occupations",
                               "Administrative and Secretarial Occupations") ~ "White collar",
        data$TEU_EmpCat == "Skilled Trades Occupations" ~ "Skilled trades",
        data$TEU_EmpCat %in% c("Personal Service Occupations",
                               "Sales and Customer Service Occupations") ~ "Services",
        data$TEU_EmpCat %in% c("Process, Plant and Machine Operatives",
                               "Elementary Occupations") ~ "Blue collar",
        data$TEU_EmpCat %in% c("Other job (free text entry)",
                               "In paid employment or self-employed") ~ "Other employment",
        data$TEU_EmpCat == "Retired" ~ "Retired",
        data$TEU_EmpCat == "Unable to work because of sickness or disability" ~ "Disability",
        data$TEU_EmpCat %in% c("Looking after home and/or family",
                               "Unemployed", "Full or part-time student",
                               "Doing unpaid or voluntary work",
                               "None of the above") ~ "Unemployed",
        data$TEU_EmpCat == "Prefer not to answer" ~ "Prefer not to answer",
        TRUE ~ "Error?"
      )
      y <- factor(y, 
                  levels=c("White collar", "Skilled trades", "Services", 
                           "Blue collar", "Other employment", "Retired", 
                           "Disability", "Unemployed","Prefer not to answer"), 
                  labels=c("Professional and Administrative", "Skilled trades", "Services", 
                           "Manual and Industrial", "Other employment", "Retired", 
                           "Unable to work because of sickness or disability", 
                           "Unemployed", "Prefer not to answer"),
                  ordered=FALSE)
      
    },
    post_exclusion = FALSE,
    display_name = "Employment category",
    description = "Employment category at baseline, collapsed from job codes self-reported in verbal interview"
  )
}

# XL add: Country of birth (by income)
TEU_CountryIncome=function(){
  list(
    name = 'TEU_CountryIncome',
    source = c('ELF_BirthCountry.0.0','VeI_BirthCountry.0.0'),
    mapper = function(data){
      
      # Generate mapping first 
      UKB_mapper=read.csv_kdrive(file.path(config$cleaning$coding,'coding89_flat_CountryOfBirth.csv'))
      Income_mapper=read.xlsx_kdrive(file.path(config$cleaning$mapping,'CountryIncomeCategory.xlsx'),sheet = 'UKB_CountryNames')
      
      mapper=left_join(UKB_mapper[,c('Code','meaning')],Income_mapper[,c('coding','IncomeLevel')],by=c('Code'='coding'))%>%
        mutate(TEU_Incomelevel=case_when(meaning=='United Kingdom' ~ 'UK',
                                         IncomeLevel=='H' ~ 'Other high income',
                                         IncomeLevel %in% c('UM','LM') ~ 'Middle income',
                                         IncomeLevel=='L' ~ 'Low income'))
      
      # Merge with data
      data=data%>%
        rename(TQ=ELF_BirthCountry.0.0,VI=VeI_BirthCountry.0.0)%>%
        mutate(VI=as.numeric(VI))%>%
        left_join(.,mapper,by=c('VI'='Code'))%>%
        # create column to derive Country of birth (by income)
        mutate(CountryIncome=case_when(!is.na(VI) ~ TEU_Incomelevel,
                                       is.na(VI) & TQ %in% c('England','Wales','Scotland','Northern Ireland') ~ 'UK',
                                       is.na(VI) & TQ=='Republic of Ireland' ~ mapper[which(mapper$meaning=='Ireland'),]$TEU_Incomelevel,
                                       is.na(VI) & TQ %in% c('Elsewhere','Prefer not to answer','Do not know') ~ 'Unknown',
                                       is.na(VI) & is.na(TQ) ~ 'Unknown',
                                       TRUE ~ 'Error?'))
      
      y<-factor(data[['CountryIncome']],levels = c('UK','Other high income','Middle income','Low income','Unknown'),
                labels = c('UK','Other high income','Middle income','Low income','Unknown'),ordered = FALSE)
      
      return(y)
      
      
    },
    post_exclusion = FALSE,
    display_name = 'Country of birth, by income level',
    description = 'Categorise self-reported country of birth from touchscreen and verbal interview by income level'
  )
}

#--------------------------------------------------------------------------------------------------------------
# XL add: MACE (status) at baseline (TEU_MACE_prev) 

# 1. HES source 
TEU_HES_MACE_prev<-function(record_level=FALSE){
  list(
    name='TEU_HES_MACE_prev',
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping,'MACE/HES_ICD9_Mapping_20210128.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),
                        OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = 'MACE',
                        return_label = 'baseline',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name='MACE status identified from HES data prior to or at baseline',
    description='MACE status identified from HES (ICD-9, ICD-10, OPCS-4) data prior to or at baseline'
  )
}


# 2. Verbal Interview (VI)
TEU_VeI_MACE_nonc<-function(condition='MACE'){
  list(
    name = 'TEU_VeI_MACE_nonc',
    source=c("ID", "Rec_DateAssess",
      paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
      paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper= function(data){
      # read in analysis codings xlsx
      mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/VI_NonCancerIllness_Mapping_20210128.xlsx'),col_types = c('text'))
      
      dx_codes<-as.numeric(mapping[which(mapping$Conditions==condition),]$Code)
      
      y<- FN_VI_filtercodes(dx_codes = dx_codes,
                            colname = "VeI_NonCancer",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
      # If not blank, assign yes
      y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
      
      return(y)
    },      
    post_exclusion = FALSE,
    display_name = "Self-reported MACE from Verbal interview at baseline",
    description = "Self-reported MACE from Verbal interview (Non-cancer illness) at baseline"
  )
}


TEU_VeI_MACE_op<-function(condition='MACE'){
  list(
    name = 'TEU_VeI_MACE_op',
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_OperationCode.0.", seq(0, 31, by=1)),
               paste0("VeI_OperationYear.0.", seq(0, 31, by=1))),
    mapper = function(data){
      
      mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/VI_Operations_Mapping_20210128.xlsx'),col_types = c('text'))
      
      dx_codes<-as.numeric(mapping[which(mapping$Conditions==condition),]$Code)
      y<- FN_VI_filtercodes(dx_codes = dx_codes,
                            colname = "VeI_Operation",
                            instance = 0,
                            return_label = "dx",
                            mapper = file.path(config$cleaning$coding,"coding5_flat_Operation.csv"))(data)
      # If not blank, assign yes
      y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Self-reported MACE operation from Verbal interview at baseline',
    description = 'Self-reported MACE operation from Verbal interview (Operations) at baseline'
  )
  
}


# XL add: MACE (TQ)
TEU_HMH_MACE_prev <- function() {
  list(
    name = "TEU_HMH_MACE_prev", 
    source = c(paste0("HMH_HeartProbs.0.", c(0:3))), 
    mapper = function(data){
      y<-FN_Vascular_condition(condition=c("Stroke", "Heart attack"), string="MACE")(data)
      levels(y)<-c('Yes','No','Unanswered')
      return(y)
      },
    post_exclusion = FALSE,
    display_name = "Self-reported MACE on TQ at baseline",
    description = "Self-reported MACE (Stroke or Heart attack) on touchscreen questionnaire at baseline"
  )
}

# MACE at baseline (HES + VI + TQ)
TEU_MACE_prev <- function(){
  list(
    name = 'TEU_MACE_prev',
    source = c('TEU_HES_MACE_prev','TEU_VeI_MACE_nonc','TEU_VeI_MACE_op','TEU_HMH_MACE_prev'),
    mapper = function(data){
      y<-factor(apply(data,1, function(x) any(x %in% 'Yes')),levels = c('FALSE','TRUE'),labels = c('No','Yes'))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'MACE status at baseline',
    description = 'MACE status at baseline identified from HES, Verbal Interview (VI), Touchscreen (TQ)'
  )
}





#--------------------------------------------------------------------------------------------------------------
# XL add: MACE outcome (status + time) (TEU_MACE_outcome())

#####################
# MACE Event date (HES follow-up + Dth)
####################

# MACE HES date (follow-up)
TEU_HES_MACE_fudate<-function(record_level=FALSE){
  list(
    name='TEU_HES_MACE_fudate',
    source=if(record_level){
      c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
        },
    mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping,'MACE/HES_ICD9_Mapping_20210128.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),
                        OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = 'MACE',
                        return_label = 'followup_date',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name='MACE date from HES data at follow-up',
    description=paste0('We keep first occurrence date of MACE identified from HES (ICD-9, ICD-10, OPCS-4) and this field only returns the first occurrence dates that are after baseline.',
                       'The HES data used was ', if(record_level) {'record-level data from the UKB data portal.'} 
                       else {'summary data from the UKB data showcase'})
  )
}


# MACE subtypes (from HES ONLY)
TEU_HES_MACE_fucomp<-function(record_level=FALSE){
  list(
    name='TEU_HES_MACE_fucomp',
    source=if(record_level){
      c("ID", "Rec_DateAssess")
      } else {
        c('ID','Rec_DateAssess',
             paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
             paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
             paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
             paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
             paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
             paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
        },
    mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping,'MACE/HES_ICD9_Mapping_20210128.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),
                        OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = 'MACE',
                        return_label = 'followup_comp',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name='MACE subtypes from HES data at follow-up',
    description=paste0('MACE subtypes (Nonfatal MI/Nonfatal Stroke) from HES data at follow-up',
                       'The HES data used was ', if(record_level) {'record-level data from the UKB data portal.'} 
                       else {'summary data from the UKB data showcase'})
  )
}


# MACE Dth date
TEU_Dth_MACE_dthdate <-function(record_level=FALSE){
    list(
      name = 'TEU_Dth_MACE_dthdate',
      source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
      mapper = function(data){
        mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),col_types = c('text'))
        ICD10_codes<-mapping[which(mapping$Conditions=='MACE'),]$Code
        
        y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level)(data)
        
        return(y)
      },
      post_exclusion = FALSE,
      display_name = 'MACE death date',
      description = paste0('Death date caused by MACE from Death Registry data',
                           'The data used was ', if(record_level) {'record-level data from the UKB data portal.'} 
                           else {'summary data from the UKB data showcase'})
    )
}


# MACE event date (Based on MACE HES date (fu) + MACE Dth date)
TEU_MACE_eventdate<-function(){
  list(
    name = 'TEU_MACE_eventdate',
    source = c('TEU_HES_MACE_fudate','TEU_Dth_MACE_dthdate'),
    mapper = function(data){
      y<-pmin(data$TEU_HES_MACE_fudate,data$TEU_Dth_MACE_dthdate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'MACE event date',
    description = 'MACE event date at follow-up based on HES and Death Registry data'
  )
  
}

#####################
# MACE censoring date (Other Death date, Admin censoring date, lost to follow-up date)
####################

# Other cause dth date
TEU_Dth_NotMACE_dthdate <-function(record_level=FALSE){
    list(
      name = 'TEU_Dth_NotMACE_dthdate',
      source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
      mapper = function(data){
        mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'MACE/HES_ICD10_Mapping_20210128.xlsx'),col_types = c('text'))
        ICD10_codes<-mapping[which(is.na(mapping$Conditions)),]$Code
        
        y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level)(data)
        
        return(y)
      },
      post_exclusion = FALSE,
      display_name = 'Non MACE death date',
      description = 'Death date caused by non MACE from Death Registry data'
    )
}


# Admin censoring date
#https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates#:~:text=Censoring%20dates,that%20provider%20is%20mostly%20complete.
# England 31/03/2017, Scotland 31/10/2016, Wales 29/02/2016
Admin_CensorDate<-function(record_level=FALSE){
  list(
    name = 'Admin_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        HES <- read_yaml(file.path(config$data$portal$HES, "censoring.yml"))
        deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml"))
        datelist <- lapply(x, FUN = function(z) {min(FN_toDate(HES[[z]]), FN_toDate(deaths[[z]]))})
        y <- do.call(c, datelist)
      }
      else {
        y <- dplyr::case_when(
          x=='England' ~ FN_toDate('2017-03-31'),
          x=='Scotland' ~ FN_toDate('2016-10-31'),
          x=='Wales' ~ FN_toDate('2016-02-29')
        )
      } 
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date from UKB',
    description = 'Censoring date according to origin of hospital data'
  )
}


# Lost to follow-up
BaC_LostFUDate<-function(){
  list(
    name = 'BaC_LostFUDate',
    source = 'BaC_DateLostFU.0.0',
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = 'Date lost to follow-up',
    description = 'Date lost to follow-up'
  )
}


# MACE censoring date (Based on Death date by non MACE + Admin censoring date + lost to follow-up)
TEU_MACE_censordate<-function(){
  list(
    name = 'TEU_MACE_censordate',
    source = c('TEU_Dth_NotMACE_dthdate','Admin_CensorDate','BaC_LostFUDate'),
    mapper = function(data){
      y<-pmin(data$TEU_Dth_NotMACE_dthdate,data$Admin_CensorDate,data$BaC_LostFUDate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'MACE censoring date',
    description = 'Censoring date for MACE outcome'
  )
}



# MACE censoring status (0=censored 1=MACE event)
TEU_MACE_status<-function(){
  list(
    name = 'TEU_MACE_status',
    source = c('TEU_MACE_censordate','TEU_MACE_eventdate'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$TEU_MACE_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(TEU_MACE_eventdate) & TEU_MACE_eventdate<=TEU_MACE_censordate ~ 1,
          is.na(TEU_MACE_eventdate) |(!is.na(TEU_MACE_eventdate)&TEU_MACE_eventdate>TEU_MACE_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'MACE censoring status',
    description = 'Censoring status of MACE (0=censored, 1=MACE event)'
    
  )
}

# MACE subtypes at follow up
# Note: If MACE dx date is the same as MACE dth date, we treat it as MACE dth instead of dx
TEU_MACE_fucomp<-function(){
  list(
    name = 'TEU_MACE_fucomp',
    source = c('TEU_MACE_status','TEU_HES_MACE_fucomp','TEU_MACE_eventdate','TEU_HES_MACE_fudate','TEU_Dth_MACE_dthdate'),
    mapper = function(data){
      data=data%>%
        mutate(TEU_MACE_fucomp=case_when(TEU_MACE_status==0  ~ NA_character_,
                                          TEU_MACE_status==1 & !is.na(TEU_MACE_eventdate) & TEU_MACE_eventdate==TEU_Dth_MACE_dthdate ~ 'CVD death',
                                          TRUE ~ TEU_HES_MACE_fucomp
        ))
      return(data$TEU_MACE_fucomp)
    },
    post_exclusion = FALSE,
    display_name = 'MACE subtypes at follow-up',
    description = 'MACE subtypes at follow-up (Nonfatal MI/Nonfatal Stroke/Cardiovascular Death)'
  )
}



# MACE follow-up time
TEU_MACE_time<-function(){
  list(
    name = 'TEU_MACE_time',
    source = c('TEU_MACE_status','TEU_MACE_censordate','TEU_MACE_eventdate','Rec_DateAssess'),
    mapper = function(data){
      
      data=data%>%
        mutate(time=case_when(
          TEU_MACE_status==0 ~ as.numeric(difftime(TEU_MACE_censordate, Rec_DateAssess, unit='days')),
          TEU_MACE_status==1 ~ as.numeric(difftime(TEU_MACE_eventdate, Rec_DateAssess, unit='days'))))
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'MACE follow up time',
    description = 'If censoring status=0, this fields returns time difference in days between censoring date and baseline date.
    If censoring status=1, this fields returns time to MACE event.'
  )
}

# MACE follow-up time (in years)
TEU_MACE_time_yrs<-function(){
  list(
    name = 'TEU_MACE_time_yrs',
    source = 'TEU_MACE_time',
    mapper = function(x){
      as.numeric(round(x/365.25, digits = 2)) 
    },
    post_exclusion = FALSE,
    display_name = 'MACE follow up time in years',
    description = 'Transfer TEU_MACE_time variable in years'
  )
}

TEU_HMH_gest_diabetes <- function() {
  list(
    name = "TEU_HMH_gest_diabetes", 
    source = c("HMH_DiabetesGest.0.0", "HMH_DiabetesGest_p.0.0"), 
    mapper = function(data) {
      coalesce(data[["HMH_DiabetesGest.0.0"]], data[["HMH_DiabetesGest_p.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "Gestational diabetes",
    description = "Did the participant self-report gestational diabetes"
  )
}


TEU_HES_T2DM_base<-function(record_level=FALSE){
  list(
    name='TEU_HES_T2DM_base',
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'coding87_ICD9_Diabetes.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20220128.xlsx'),
                        #OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = 'T2DM',
                        return_label = 'baseline',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name='T2DM diagnoses at baseline',
    description='T2DM diagnoses identified from HES (ICD-9, ICD-10) data prior to or at baseline'
  )
}

TEU_HES_diab_excl<-function(record_level=FALSE){
  list(
    name='TEU_HES_diab_excl',
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20220128.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20220128.xlsx'),
                        #OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = c('Exclude','T2DM'),
                        return_label = 'baseline',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name='T1/T2D exclusions at baseline from HES',
    description='T1/T2D status identified from HES (ICD-9, ICD-10) data prior to or at baseline'
  )
}


TEU_VeI_T2DM_meds <- function(){
  list(
    list(
      name = 'TEU_VeI_T2DM_meds',
      source = c('ID',paste0("VeI_MedCode.0.",c(0:47))),
      mapper = FN_VImed_filtercodes(med_codes = read.xlsx_kdrive(file.path(config$cleaning$mapping, "coding4_VImedications.xlsx")) %>%
                                    filter(!is.na(Drug_type)) %>%
                                    pull(value),
                                  med_name = 'T2DM_meds',
                                  colname = "VeI_Med", 
                                  instance = 0,
                                  return_label = 'T2DM_meds',
                                  mapper = file.path(config$cleaning$coding,'coding4_Treatments.csv')),
      post_exclusion = FALSE,
      display_name = 'T2DM meds',
      description = 'Whether people self reported taking T2DM_meds at baseline during verbal interview (Does NOT include insulin, glucagon)'
    ),
    list(
      name = 'TEU_VeI_T2DM_medsnum',
      source = c('ID',paste0("VeI_MedCode.0.",c(0:47))),
      mapper = FN_VImed_filtercodes(med_codes = read.xlsx_kdrive(file.path(config$cleaning$mapping, "coding4_VImedications.xlsx")) %>%
                                           filter(!is.na(Drug_type)) %>%
                                           pull(value),
                                         med_name = 'T2DM_meds',
                                         colname = "VeI_Med", 
                                         instance = 0,
                                         return_label = 'T2DM_meds_num',
                                         mapper = file.path(config$cleaning$coding,'coding4_Treatments.csv')),
      post_exclusion = FALSE,
      display_name = 'Number of T2DM_meds taken at baseline',
      description = 'Number of T2DM_meds people self reported taking at baseline during verbal interview'
    ),
    list(
      name = 'TEU_VeI_T1D_exclmeds',
      source = c('ID',paste0("VeI_MedCode.0.",c(0:47))),
      mapper = FN_VImed_filtercodes(med_codes = read.xlsx_kdrive(file.path(config$cleaning$mapping, "coding4_VImedications.xlsx")) %>%
                                      filter(!is.na(Insulin)) %>%
                                      pull(value),
                                  med_name = 'Insulin',
                                  colname = "VeI_Med", 
                                  instance = 0,
                                  return_label = 'Insulin',
                                  mapper = file.path(config$cleaning$coding,'coding4_Treatments.csv')),
      post_exclusion = FALSE,
      display_name = 'Insulin',
      description = 'Whether people self reported taking insulin at baseline during verbal interview (Does NOT include glucagon)'
    ),
    list(
      name = 'TEU_VeI_nonmet_meds',
      source = c('ID',paste0("VeI_MedCode.0.",c(0:47))),
      mapper = FN_VImed_filtercodes(med_codes = read.xlsx_kdrive(file.path(config$cleaning$mapping, "coding4_VImedications.xlsx")) %>%
                                      filter(!is.na(`Active 1`) & !`Active 1`%in%c("Metformin","Metformin hydrochloride")) %>%
                                      pull(value),
                                    med_name = 'nonmet_meds',
                                    colname = "VeI_Med", 
                                    instance = 0,
                                    return_label = 'nonmet_meds',
                                    mapper = file.path(config$cleaning$coding,'coding4_Treatments.csv')),
      post_exclusion = FALSE,
      display_name = 'Non-metformin meds',
      description = 'Whether people self reported taking non-metformin T2D meds at baseline during verbal interview (Does NOT include insulin, glucagon)'
    )
  )
  
}

# Prevalent T2D (self-report (VI) dx + med + HES)
TEU_T2DM_prevalent <- function(){
  list(
    name = "TEU_T2DM_prevalent",
    source = c("TEU_VeI_T2DM","TEU_HES_T2DM_base","TEU_VeI_T2DM_meds"),
    mapper = function(data){
      
      data=data%>%
        mutate(TEU_VeI_T2DM_prev=ifelse(is.na(TEU_VeI_T2DM),'No','Yes'))%>%
        # create a union indicator
        mutate(T2D_prev=ifelse(TEU_VeI_T2DM_prev=="Yes"|TEU_VeI_T2DM_meds=="Yes"|TEU_HES_T2DM_base=="Yes","Yes","No"))
      
      return(data$T2D_prev)
    },
    post_exclusion = FALSE,
    display_name = "Prevalent T2D status at baseline",
    description = "Prevalent Type 2 diabetes status at baseline from self-report VI diagnosis, medications and hospital inpatient record"
  )
}

# Prevalent diab types (From Eastwood2016 Alg1)
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0162388
Estw_diab_prevalent <- function(){
  list(
    name = "Estw_diab_prevalent",
    source = c("ID","TEU_HMH_gest_diabetes","TEU_HMH_Meds_Diab","HMH_Insulin1Yr",
               "Estw_VeI_diab","Estw_VeI_diab_age","TEU_VeI_T1D_exclmeds","TEU_VeI_T2DM_meds",
               "TEU_VeI_gest","TEU_VeI_gest_age",
               "TEU_VeI_T1DM","TEU_VeI_T2DM",
               "TEU_VeI_nonmet_meds","TEU_ethnicgrp"),
    mapper = FN_Diabetes,
    post_exclusion = FALSE,
    display_name = "Prevalent diabetes types at baseline",
    description = "Prevalent diabetes types at baseline from Eastwood2016 Algorithm 1 (Self-report only)"
  )
}

# Convert Estw_diab_prevalent into gestational diab (after exclusion)
Estw_gestational<-function(){
  list(
    name="Estw_gestational",
    source=c("Estw_diab_prevalent"),
    mapper=function(x){y<-factor(x,levels=c("Diabetes unlikely","Possible gestational diabetes"),
                                 labels = c("No","Yes"))},
    post_exclusion=TRUE, # has to be derived after exclusion criteria because we excluded T1D/T2D
    display_name="Gestational diabetes",
    description="Prevalent gestational diabetes at baseline from Eastwood2016."
  )
}


# Combine Estw_diab_prevalent and TEU_HES_diab_excl
TEU_diab_excl<-function(){
  list(
    name="TEU_diab_excl",
    source=c("Estw_diab_prevalent","TEU_HES_diab_excl"),
    mapper=function(data){
      data<-data%>%
        mutate(y=case_when(
          (Estw_diab_prevalent %in% c("Diabetes unlikely","Possible gestational diabetes") & TEU_HES_diab_excl=="Yes")|
            Estw_diab_prevalent %in% c("Possible type 1 diabetes","Possible type 2 diabetes") ~ "T1/T2D",
          Estw_diab_prevalent %in% "Diabetes unlikely" & TEU_HES_diab_excl=="No" ~ "Diabetes unlikely",
          Estw_diab_prevalent %in% "Possible gestational diabetes" & TEU_HES_diab_excl=="No" ~ "Possible gestational diabetes"
        ))
      
      return(data$y)
    },
    post_exclusion=FALSE,
    display_name='Types of prevalent diabetes at baseline',
    description='Types of prevalent diabetes at baseline derived from Eastwood2016 and HES'
  )
}

#####################
# T2DM event date (HES dx + Dth)
####################


# T2DM HES FU date
TEU_HES_T2DM_fudate<-function(record_level=FALSE){
  list(
    name='TEU_HES_T2DM_fudate',
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'coding87_ICD9_Diabetes.xlsx'),
      ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20220128.xlsx'),
      #OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
      condition = 'T2DM',
      return_label = 'followup_date',
      record_level = record_level),
    post_exclusion=FALSE,
    display_name='T2DM date of diagnoses at follow-up',
    description='T2DM diagnoses date identified from HES (ICD-10) data after baseline'
  )
}

# T2D Dth date (primary + secondary)
TEU_Dth_T2DM_dthdate <-function(record_level=TRUE,level="any"){
  list(
    name = 'TEU_Dth_T2DM_dthdate',
    source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
    mapper = function(data){
      mapping=read.xlsx_kdrive(file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20220128.xlsx'),col_types = c('text'))
      ICD10_codes<-mapping[which(mapping$Conditions=='T2DM'),]$Code
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level,level=level)(data)
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'T2DM death date',
    description = paste0('Death date caused by type 2 diab from Death Registry data (both primary + secondary causes)',
                         'The data used was ', if(record_level) {'record-level data from the UKB data portal.'} 
                         else {'summary data from the UKB data showcase'})
  )
}


# Incident T2D event date (Based on HES FU date + Dth date + PC date)

TEU_T2DM_eventdate<-function(){
  list(
    name = 'TEU_T2DM_eventdate',
    source = c('TEU_HES_T2DM_fudate','TEU_Dth_T2DM_dthdate','TEU_PC_T2D_date'),
    mapper = function(data){
      y<-pmin(data$TEU_HES_T2DM_fudate,data$TEU_Dth_T2DM_dthdate,data$TEU_PC_T2D_date,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Incident T2DM event date',
    description = 'Incident T2DM event date at follow-up based on HES, Death Registry, and Primary Care data'
  )
  
}

HESDth_T2D_eventdate<-function(){
  list(
    name = 'HESDth_T2D_eventdate',
    source = c('TEU_HES_T2DM_fudate','TEU_Dth_T2DM_dthdate'),
    mapper = function(data){
      y<-pmin(data$TEU_HES_T2DM_fudate,data$TEU_Dth_T2DM_dthdate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Incident T2DM event date',
    description = 'Incident T2DM event date at follow-up based on HES, Death Registry'
  )
  
}

HESDthGP_T2D_eventdate<-function(){
  list(
    name = 'HESDthGP_T2D_eventdate',
    source = c('TEU_HES_T2DM_fudate','TEU_Dth_T2DM_dthdate','PC_T2D_eventdate'),
    mapper = function(data){
      y<-pmin(data$TEU_HES_T2DM_fudate,data$TEU_Dth_T2DM_dthdate,data$PC_T2D_eventdate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Incident T2DM event date',
    description = 'Incident T2DM event date at follow-up based on HES, Death Registry, GP dx'
  )
}

#####################
# T2DM censoring date (Other Death date, Admin censoring date, lost to follow-up date)
####################

# Other cause dth date
# Note: This derivation was different from other "Not sth" Death because in this case, T2D death was picked up
# from both primary and secondary. Using the previous approach (e.g. TEU_Dth_NotMACE_dthdate) would count people with
# secondary deaths with T2D as "Not T2D" death, which is wrong!
TEU_Dth_NotT2DM_dthdate <-function(){
  list(
    name = 'TEU_Dth_NotT2DM_dthdate',
    source = c("ID","TEU_Dth_T2DM_dthdate"),
    mapper = function(data){
      
      # Create a list of IDs who died from T2D (primary or secondary cause)
      T2D_ID<-data%>%
        filter(!is.na(TEU_Dth_T2DM_dthdate))%>%
        pull(ID)
      
      # Read in record-level dth data and remove ppl who died from T2D
      deaths_all<-FN_Death_registry()%>%
        #rename(Dth_ICD10 = Dth_ICD10All)%>%
        filter(!ID %in% T2D_ID)
      
      y <- deaths_all[["dth_date"]][match(data$ID, deaths_all$ID)]
      return(y)
      
    },
    post_exclusion = FALSE,
    display_name = 'Non T2DM death date',
    description = 'Death date caused by non T2DM from Death Registry data'
  )
}


# Admin censoring date
Admin_HES_CensorDate<-function(record_level=TRUE){
  list(
    name = 'Admin_HES_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        HES <- read_yaml(file.path(config$data$portal$HES, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
        # deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml"))
        # datelist <- lapply(x, FUN = function(z) {min(FN_toDate(HES[[z]]), FN_toDate(deaths[[z]]))})
        # y <- do.call(c, datelist)
      }
      else {
        HES <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$HES %>%
          lapply(., FUN=FN_toDate)
      } 
      y <- dplyr::case_when(
        x=='England' ~ HES$England,
        x=='Scotland' ~ HES$Scotland,
        x=='Wales' ~ HES$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date from UKB HES',
    description = 'Censoring date according to origin of hospital data'
  )
}


# Admin censoring date (Death registry)
# https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates

Admin_Dth_CensorDate<-function(record_level=TRUE){
  list(
    name = 'Admin_Dth_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
      } else {
        deaths <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$Death %>%
          lapply(., FUN=FN_toDate)
      }
      
      y <- dplyr::case_when(
        x=='England' ~ deaths$England,
        x=='Scotland' ~ deaths$Scotland,
        x=='Wales' ~ deaths$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date for death registry data',
    description = 'Administrative censoring date for death registries data by country, using assessment centre as a proxy for country'
  )
}

# Admin censoring date
Admin_PC_CensorDate<-function(){
  list(
    name = 'Admin_PC_CensorDate',
    source = 'ID',
    mapper = function(x){
      # Read in admin censoring time of PC
      Admin_GP <- read_yaml(file.path(config$data$portal$primarycare, "censoring.yml"))%>%
        lapply(.,lapply,as.Date)
      
      # Read in GP censoring data
      gp_censoring<-readRDS(file.path(config$data$portal$primarycare,"gp_censoring.rds"))
      
      data<-as.data.frame(x);names(data)<-"ID"
      
      data<-left_join(data,gp_censoring,by=c("ID"="eid"))%>%
        # End of coverage takes the earliest of UKB web and UKB PC pdf
        mutate(EndPC_date=case_when(
          data_provider==1 ~ min(Admin_GP$Extract_start$`England Vision`,Admin_GP$UKBweb$`England Vision`),
          data_provider==2 ~ min(Admin_GP$Extract_start$Scotland,Admin_GP$UKBweb$Scotland),
          data_provider==3 ~ min(Admin_GP$Extract_start$`England TPP`,Admin_GP$UKBweb$`England TPP`),
          data_provider==4 ~ min(Admin_GP$Extract_start$Wales,Admin_GP$UKBweb$Wales)
        ),
        # Also incorporate deduction date
        Admin_GP_CensorDate=pmin(EndPC_date,deduct_date,na.rm = TRUE)
        )
      
      return(data$Admin_GP_CensorDate)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date from Primary Care data',
    description = 'Censoring date according to gp_registration, gp_clinical and gp_scripts'
  )
}

Admin_CensorDate_HESDth <- function(sources=c("HES", "Dth")){
  list(
    name = 'Admin_CensorDate',
    source = glue("Admin_{sources}_CensorDate"),
    mapper = function(data){
      y <- do.call(pmin, data[,glue("Admin_{sources}_CensorDate")])
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date',
    description = 'Administrative censoring date, taken as the minimum of death censoring and HES censoring)'
  )
}


# Lost to follow-up
BaC_LostFUDate<-function(){
  list(
    name = 'BaC_LostFUDate',
    source = 'BaC_DateLostFU.0.0',
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = 'Date lost to follow-up',
    description = 'Date lost to follow-up'
  )
}


# T2DM censoring date (Based on Death date by non T2DM + Admin censoring date + lost to follow-up)
HESDth_T2DM_censordate<-function(){
  list(
    name = 'HESDth_T2DM_censordate',
    source = c('TEU_Dth_NotT2DM_dthdate','Admin_CensorDate','BaC_LostFUDate'),
    mapper = function(data){
      y<-pmin(data$TEU_Dth_NotT2DM_dthdate,data$Admin_CensorDate,data$BaC_LostFUDate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'T2DM censoring date',
    description = 'Censoring date for T2DM outcome'
  )
}

T2D_censordate<-function(){
  list(
    name = 'T2D_censordate',
    source = c('HESDth_T2DM_censordate','Admin_PC_CensorDate'),
    mapper = function(data){
      y<-pmin(data$HESDth_T2DM_censordate,data$Admin_PC_CensorDate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'T2DM censoring date',
    description = 'Censoring date for T2DM outcome, incorporating Admin HES/Dth/GP censoring,
    deduction date, lost to follow-up date, Dth by other causes.'
  )
}


# Incident T2DM status (0=censored 1=event)
TEU_T2DM_status<-function(){
  list(
    name = 'TEU_T2DM_status',
    source = c('TEU_T2DM_censordate','TEU_T2DM_eventdate'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$TEU_T2DM_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(TEU_T2DM_eventdate) & TEU_T2DM_eventdate<=TEU_T2DM_censordate ~ 1,
          is.na(TEU_T2DM_eventdate) |(!is.na(TEU_T2DM_eventdate)&TEU_T2DM_eventdate>TEU_T2DM_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'Incident T2DM status',
    description = 'Incident status of T2DM (0=censored, 1=event)'
    
  )
}



# T2DM follow-up time
TEU_T2DM_time<-function(){
  list(
    name = 'TEU_T2DM_time',
    source = c('TEU_T2DM_status','TEU_T2DM_censordate','TEU_T2DM_eventdate','Rec_DateAssess'),
    mapper = function(data){
      
      data=data%>%
        mutate(time=case_when(
          TEU_T2DM_status==0 ~ as.numeric(difftime(TEU_T2DM_censordate, Rec_DateAssess, unit='days'))/365.25,
          TEU_T2DM_status==1 ~ as.numeric(difftime(TEU_T2DM_eventdate, Rec_DateAssess, unit='days'))/365.25))
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'T2DM follow up time (in years)',
    description = 'If censoring status=0, this fields returns time difference in years between censoring date and baseline date.
    If censoring status=1, this fields returns time to T2DM event.'
  )
}

# Indicator of who had GP record
TEU_PC_ind<-function(){
  list(
    name = 'TEU_PC_ind',
    source = c("ID"),
    mapper = function(x){
      # Read in GP censoring data
      gp_censoring<-readRDS(file.path(config$data$portal$primarycare,"gp_censoring.rds"))
      
      #data<-as.data.frame(x);names(data)<-"ID"
      
      #join<-left_join(data,gp_censoring,by=c("ID"="eid"))%>%
       # mutate(ind=ifelse(is.na(data_provider),FALSE,TRUE))
      
      # different and more concise way
      y <- gp_censoring[["data_provider"]][match(x, gp_censoring$eid)]
      return(y)
      
    },
    post_exclusion = FALSE,
    display_name = 'Availability of GP data',
    description = 'Whether participants have GP data available based on cleaned GP data'
  )
}

# Prevalent status of diabetes based on PC clinical event data
TEU_PC_diab_prev <-function(){
  list(
    name = 'TEU_PC_diab_prev',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      
      # first read in the mapping (shared by Eirini and Fiona)
      gp_readv2<-read.xlsx_kdrive(file.path(config$cleaning$primarycare,"Primary care codes DM diag.xlsx"),col_types = "text",sheet=1)
      
      gp_readv3<-read.xlsx_kdrive(file.path(config$cleaning$primarycare,"Primary care codes DM diag.xlsx"),col_types = "text",sheet=2)
      
      # Extract the diab codes
      Diab_readv2<-gp_readv2%>%
        filter(!is.na(`Prevalent diabetes`))%>%
        pull(read_code)%>%unique()
      
      Diab_readv3<-gp_readv3%>%filter(!is.na(`Prevalent diabetes`))%>%pull(read_code)%>%unique()
      
      
      y<-FN_PCevent_filtercodes(read2_codes = Diab_readv2,read3_codes=Diab_readv3,return_label = 'baseline')(data)
      
      return(y)
    },
    post_exclusion = FALSE, #To resolve memory issue
    display_name = 'Prevalent diabetes status by gp clinical data',
    description = 'Prevalent diabetes (gestational diabetes not included) status at baseline derived from gp_clinical'
  )
}

# Prevalent status of diabetes based on PC prescription event data
TEU_PCmeds_diab_prev <-function(){
  list(
    name = 'TEU_PCmeds_diab_prev',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      
      #GP medications (3 mappings)
      gp_meds_readv2 <- read_excel(file.path(config$cleaning$primarycare,"Primary care codes DM meds.xlsx"),col_types = "text",sheet=1)
      
      gp_meds_BNF <- read_excel(file.path(config$cleaning$primarycare,"Primary care codes DM meds.xlsx"),col_types = "text",sheet=2)
      
      gp_meds_DMD <- read_excel(file.path(config$cleaning$primarycare,"Primary care codes DM meds.xlsx"),col_types = "text",sheet=3)
      
      # Extract diabetes medication codes
      
      Diab_readv2<-gp_meds_readv2%>%filter(!is.na(Diabetes))%>%pull(`Read code`)%>%unique()
      Diab_BNF<-gp_meds_BNF%>%filter(!is.na(Diabetes))%>%pull(BNF_Presentation_Code)%>%unique()
      Diab_DMD<-gp_meds_DMD%>%filter(!is.na(Diabetes))%>%pull(concept_id)%>%unique()
      
      y<-FN_PCmeds_filtercodes(read2_codes = Diab_readv2,BNF_codes=Diab_BNF,DMD_codes = Diab_DMD,
                               return_label = 'baseline')(data)
      
      return(y)
    },
    post_exclusion = FALSE, #To resolve memory issue
    display_name = 'Prevalent diabetes status by gp scripts data',
    description = 'Prevalent diabetes status at baseline derived from gp_scripts'
  )
}

# Incident status of T2D based on PC clinical event data
TEU_PC_T2D_inc <-function(){
  list(
    name = 'TEU_PC_T2D_inc',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      
      # first read in the mapping (shared by Eirini and Fiona)
      gp_readv2<-read.xlsx_kdrive(file.path(config$cleaning$primarycare,"Primary care codes DM diag.xlsx"),col_types = "text",sheet=1)
      
      gp_readv3<-read.xlsx_kdrive(file.path(config$cleaning$primarycare,"Primary care codes DM diag.xlsx"),col_types = "text",sheet=2)
      
      # Extract the T2D codes
      T2D_readv2<-gp_readv2%>%filter(!is.na(T2D))%>%pull(read_code)%>%unique()
      
      T2D_readv3<-gp_readv3%>%filter(!is.na(T2D))%>%pull(read_code)%>%unique()
      
      
      y<-FN_PCevent_filtercodes(read2_codes = T2D_readv2,read3_codes=T2D_readv3,return_label = 'followup')(data)
      
      return(y)
    },
    post_exclusion = FALSE, 
    display_name = 'Incident T2D status by gp clinical data',
    description = 'Incident T2D status derived from gp_clinical'
  )
}

# Date of T2D dx based on PC clinical event data
PC_T2D_eventdate <-function(){
  list(
    name = 'PC_T2D_eventdate',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      
      # first read in the mapping (shared by Eirini and Fiona)
      gp_readv2<-read.xlsx_kdrive(file.path(config$cleaning$primarycare,"Primary care codes DM diag.xlsx"),col_types = "text",sheet=1)
      
      gp_readv3<-read.xlsx_kdrive(file.path(config$cleaning$primarycare,"Primary care codes DM diag.xlsx"),col_types = "text",sheet=2)
      
      # Extract the T2D codes
      T2D_readv2<-gp_readv2%>%filter(!is.na(T2D))%>%pull(read_code)%>%unique()
      
      T2D_readv3<-gp_readv3%>%filter(!is.na(T2D))%>%pull(read_code)%>%unique()
      
      
      y<-FN_PCevent_filtercodes(read2_codes = T2D_readv2,read3_codes=T2D_readv3,return_label = 'followup_date')(data)
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Date of incident T2D dx by gp clinical data',
    description = 'Date of incident T2D dx derived from gp_clinical'
  )
}

# Age of T2D dx based on PC clinical event data
TEU_PC_T2D_Age <-function(){
  list(
    name = 'TEU_PC_T2D_age',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      
      # first read in the mapping (shared by Eirini and Fiona)
      gp_readv2<-read.xlsx_kdrive(file.path(config$cleaning$primarycare,"Primary care codes DM diag.xlsx"),col_types = "text",sheet=1)
      
      gp_readv3<-read.xlsx_kdrive(file.path(config$cleaning$primarycare,"Primary care codes DM diag.xlsx"),col_types = "text",sheet=2)
      
      # Extract the T2D codes
      T2D_readv2<-gp_readv2%>%filter(!is.na(T2D))%>%pull(read_code)%>%unique()
      
      T2D_readv3<-gp_readv3%>%filter(!is.na(T2D))%>%pull(read_code)%>%unique()
      
      
      y<-FN_PCevent_filtercodes(read2_codes = T2D_readv2,read3_codes=T2D_readv3,return_label = 'duration')(data)
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Duration of T2D dx by gp clinical data',
    description = 'Duration of T2D dx derived from gp_clinical (prevalent/incident)'
  )
}

# Incident T2D status (0=censored 1=event) based on HES/Dth
HESDth_T2D_status<-function(){
  list(
    name = 'HESDth_T2D_status',
    source = c('T2D_censordate','HESDth_T2D_eventdate'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$T2D_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(HESDth_T2D_eventdate) & HESDth_T2D_eventdate<=T2D_censordate ~ 1,
          is.na(HESDth_T2D_eventdate) |(!is.na(HESDth_T2D_eventdate)&HESDth_T2D_eventdate>T2D_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'Incident T2DM status',
    description = 'Incident status of T2DM (0=censored, 1=event) based on HES/Dth data censored at end of GP'
    
  )
}


# T2D follow-up time based on HES/Dth
HESDth_T2D_time<-function(){
  list(
    name = 'HESDth_T2D_time',
    source = c('HESDth_T2D_status','T2D_censordate','HESDth_T2D_eventdate','Rec_DateAssess'),
    mapper = function(data){
      
      data=data%>%
        mutate(time=case_when(
          HESDth_T2D_status==0 ~ as.numeric(difftime(T2D_censordate, Rec_DateAssess, unit='days'))/365.25,
          HESDth_T2D_status==1 ~ as.numeric(difftime(HESDth_T2D_eventdate, Rec_DateAssess, unit='days'))/365.25))
      
      # Check if cases had negative FU
      if (data%>%filter(HESDth_T2D_status==1 & time<0)%>%nrow()>0){
        warning('Cases with negative follow-up: Need to double check!')
      }
        
      # For scenarios where individuals left GP before joining UKB, we truncate their FU time to 0
      data<-data%>%mutate(
          time=ifelse(time < 0, 0, time)
        )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'T2DM follow up time (in years)',
    description = 'If censoring status=0, this fields returns time difference in years between censoring date and baseline date.
    If censoring status=1, this fields returns time to T2DM event.'
  )
}

# Incident T2D status (0=censored 1=event) based on HES/Dth/GP
HESDthGP_T2D_status<-function(){
  list(
    name = 'HESDthGP_T2D_status',
    source = c('T2D_censordate','HESDthGP_T2D_eventdate'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$T2D_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(HESDthGP_T2D_eventdate) & HESDthGP_T2D_eventdate<=T2D_censordate ~ 1,
          is.na(HESDthGP_T2D_eventdate) |(!is.na(HESDthGP_T2D_eventdate)&HESDthGP_T2D_eventdate>T2D_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'Incident T2DM status',
    description = 'Incident status of T2DM (0=censored, 1=event) based on HES/Dth/GP data censored at end of GP'
    
  )
}


# T2D follow-up time based on HES/Dth/GP
HESDthGP_T2D_time<-function(){
  list(
    name = 'HESDthGP_T2D_time',
    source = c('HESDthGP_T2D_status','T2D_censordate','HESDthGP_T2D_eventdate','Rec_DateAssess'),
    mapper = function(data){
      
      data=data%>%
        mutate(time=case_when(
          HESDthGP_T2D_status==0 ~ as.numeric(difftime(T2D_censordate, Rec_DateAssess, unit='days'))/365.25,
          HESDthGP_T2D_status==1 ~ as.numeric(difftime(HESDthGP_T2D_eventdate, Rec_DateAssess, unit='days'))/365.25))
      
      # Check if cases had negative FU
      if (data%>%filter(HESDthGP_T2D_status==1 & time<0)%>%nrow()>0){
        warning('Cases with negative follow-up: Need to double check!')
      }
      
      # For scenarios where individuals left GP before joining UKB, we truncate their FU time to 0
      data<-data%>%mutate(
        time=ifelse(time < 0, 0, time)
      )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'T2DM follow up time (in years)',
    description = 'If censoring status=0, this fields returns time difference in years between censoring date and baseline date.
    If censoring status=1, this fields returns time to T2DM event.'
  )
}


# Self-report Parkinson's Disease
TEU_VeI_PD <- function(dx_codes = c(1262)) {
    list(
      name = "TEU_VeI_PD",
      source = c("ID", "Rec_DateAssess",
                 paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
                 paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
      mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                                 colname = "VeI_NonCancer",
                                 instance = 0,
                                 return_label = "dx",
                                 mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
      post_exclusion = FALSE,
      display_name = "Self-reported PD",
      description = "Whether Parkinson's Disease was reported in verbal interview at baseline"
  )
}

# PD derived using HES
TEU_HES_PD_prev<-function(record_level=TRUE){
  list(
    name='TEU_HES_PD_prev',
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD9_Mapping_20230210.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD10_Mapping_20230210.xlsx'),
                        #OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = c('PD'),
                        return_label = 'baseline',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name='Prevalent PD at baseline from HES',
    description="Parkinson's disease status identified from HES (ICD-9, ICD-10) data prior to or at baseline"
  )
}

# PD derived using GP data
TEU_PC_PD_prev <-function(){
  list(
    name = 'TEU_PC_PD_prev',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      
      # Read algorithm PD code list
      PD_file<-read.xlsx_kdrive(file.path(config$cleaning$HESvsGP_mapping,
                                          "PDDementia_codes_20230210.xlsx"),
                                col_types = "text",sheet=2)
      # Extract the PD diag codes
      PD_readv2<-PD_file%>%filter(PD=="1" & `Code Type`=="Read V2")%>%pull(Code)%>%unique()
      
      PD_readv3<-PD_file%>%filter(PD=="1" & `Code Type`=="Read CTV3")%>%pull(Code)%>%unique()
      
      y<-FN_PCevent_filtercodes(read2_codes = PD_readv2,read3_codes=PD_readv3,
                                return_label = 'baseline')(data)
      
      return(y)
    },
    post_exclusion = FALSE, #To resolve memory issue
    display_name = 'Prevalent PD dx by gp clinical data',
    description = "Prevalent Parkinson's disease diagnosis at baseline derived from gp_clinical"
  )
}

# Prevalent status of PD based on GP medication
TEU_PCmeds_PD_prev <-function(){
  list(
    name = 'TEU_PCmeds_PD_prev',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      # Read algorithm PD code list
      PD_file<-read.xlsx_kdrive(file.path(config$cleaning$HESvsGP_mapping,
                                          "PDDementia_codes_20230210.xlsx"),
                                col_types = "text",sheet=2)
      # Extract the PD diag codes
      PD_readv2<-PD_file%>%filter(PD=="1" & `Code Type`=="Read V2 & CTV3")%>%pull(Code)%>%unique()
      
      # UKB didn't provide BNF or dm+d codes
      y<-FN_PCmeds_filtercodes(read2_codes = PD_readv2,BNF_codes="",DMD_codes = "",
                               return_label = 'baseline')(data)
      
      return(y)
    },
    post_exclusion = FALSE, #To resolve memory issue
    display_name = 'Prevalent PD status by gp medication data',
    description = 'Prevalent PD medication users at baseline derived from gp_scripts'
  )
}

# PD HES FU date
TEU_HES_PD_fudate<-function(record_level=FALSE){
  list(
    name='TEU_HES_PD_fudate',
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD9_Mapping_20230210.xlsx'),
      ICD10_xlsx = file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD10_Mapping_20230210.xlsx'),
      #OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
      condition = 'PD',
      return_label = 'followup_date',
      record_level = record_level),
    post_exclusion=FALSE,
    display_name='PD date of diagnoses at follow-up',
    description='PD diagnoses date identified from HES (ICD-9 and 10) data after baseline'
  )
}

# PD Dth date (primary + secondary)
TEU_Dth_PD_dthdate <-function(record_level=TRUE,level="any"){
  list(
    name = 'TEU_Dth_PD_dthdate',
    source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
    mapper = function(data){
      mapping=read.xlsx_kdrive(file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD10_Mapping_20230210.xlsx'),col_types = c('text'))
      ICD10_codes<-mapping[which(mapping$Conditions=='PD'),]$Code
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level,level=level)(data)
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'PD death date',
    description = paste0('Death date caused by PD from Death Registry data (both primary + secondary causes)',
                         'The data used was ', if(record_level) {'record-level data from the UKB data portal.'} 
                         else {'summary data from the UKB data showcase'})
  )
}


TEU_Dth_NotPD_dthdate <-function(){
  list(
    name = 'TEU_Dth_NotPD_dthdate',
    source = c("ID","TEU_Dth_PD_dthdate"),
    mapper = function(data){
      
      # Create a list of IDs who died from PD (primary or secondary cause)
      PD_ID<-data%>%
        filter(!is.na(TEU_Dth_PD_dthdate))%>%
        pull(ID)
      
      # Read in record-level dth data and remove ppl who died from T2D
      deaths_all<-FN_Death_registry()%>%
        #rename(Dth_ICD10 = Dth_ICD10All)%>%
        filter(!ID %in% PD_ID)
      
      y <- deaths_all[["dth_date"]][match(data$ID, deaths_all$ID)]
      return(y)
      
    },
    post_exclusion = FALSE,
    display_name = 'Non PD death date',
    description = 'Death date caused by non PD from Death Registry data'
  )
}

HESDth_PD_censordate<-function(){
  list(
    name = 'HESDth_PD_censordate',
    source = c('TEU_Dth_NotPD_dthdate','Admin_CensorDate','BaC_LostFUDate'),
    mapper = function(data){
      y<-pmin(data$TEU_Dth_NotPD_dthdate,data$Admin_CensorDate,data$BaC_LostFUDate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'PD censoring date',
    description = 'Censoring date for PD outcome'
  )
}

PD_censordate<-function(){
  list(
    name = 'PD_censordate',
    source = c('HESDth_PD_censordate','Admin_PC_CensorDate'),
    mapper = function(data){
      y<-pmin(data$HESDth_PD_censordate,data$Admin_PC_CensorDate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'PD censoring date',
    description = 'Censoring date for PD outcome, incorporating Admin HES/Dth/GP censoring,
    deduction date, lost to follow-up date, Dth by other causes.'
  )
}


HESDth_PD_eventdate<-function(){
  list(
    name = 'HESDth_PD_eventdate',
    source = c('TEU_HES_PD_fudate','TEU_Dth_PD_dthdate'),
    mapper = function(data){
      y<-pmin(data$TEU_HES_PD_fudate,data$TEU_Dth_PD_dthdate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Incident PD event date',
    description = 'Incident PD event date at follow-up based on HES, Death Registry'
  )
  
}

# Date of PD dx based on PC clinical event data
PC_PD_eventdate <-function(){
  list(
    name = 'PC_PD_eventdate',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      
      # Read algorithm PD code list
      PD_file<-read.xlsx_kdrive(file.path(config$cleaning$HESvsGP_mapping,
                                          "PDDementia_codes_20230210.xlsx"),
                                col_types = "text",sheet=2)
      # Extract the PD diag codes
      PD_readv2<-PD_file%>%filter(PD=="1" & `Code Type`=="Read V2")%>%pull(Code)%>%unique()
      
      PD_readv3<-PD_file%>%filter(PD=="1" & `Code Type`=="Read CTV3")%>%pull(Code)%>%unique()
      
      y<-FN_PCevent_filtercodes(read2_codes = PD_readv2,read3_codes=PD_readv3,return_label = 'followup_date')(data)
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Date of incident PD dx by gp clinical data',
    description = 'Date of incident PD dx derived from gp_clinical'
  )
}

HESDthGP_PD_eventdate<-function(){
  list(
    name = 'HESDthGP_PD_eventdate',
    source = c('TEU_HES_PD_fudate','TEU_Dth_PD_dthdate','PC_PD_eventdate'),
    mapper = function(data){
      y<-pmin(data$TEU_HES_PD_fudate,data$TEU_Dth_PD_dthdate,data$PC_PD_eventdate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Incident PD event date',
    description = 'Incident PD event date at follow-up based on HES, Death Registry, GP dx'
  )
}

# Incident PD status (0=censored 1=event) based on HES/Dth
HESDth_PD_status<-function(){
  list(
    name = 'HESDth_PD_status',
    source = c('PD_censordate','HESDth_PD_eventdate'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$PD_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(HESDth_PD_eventdate) & HESDth_PD_eventdate<=PD_censordate ~ 1,
          is.na(HESDth_PD_eventdate) |(!is.na(HESDth_PD_eventdate)&HESDth_PD_eventdate>PD_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'Incident PD status',
    description = 'Incident status of PD (0=censored, 1=event) based on HES/Dth data censored at end of GP'
    
  )
}


# PD follow-up time based on HES/Dth
HESDth_PD_time<-function(){
  list(
    name = 'HESDth_PD_time',
    source = c('HESDth_PD_status','PD_censordate','HESDth_PD_eventdate','Rec_DateAssess'),
    mapper = function(data){
      
      data=data%>%
        mutate(time=case_when(
          HESDth_PD_status==0 ~ as.numeric(difftime(PD_censordate, Rec_DateAssess, unit='days'))/365.25,
          HESDth_PD_status==1 ~ as.numeric(difftime(HESDth_PD_eventdate, Rec_DateAssess, unit='days'))/365.25))
      
      # Check if cases had negative FU
      if (data%>%filter(HESDth_PD_status==1 & time<0)%>%nrow()>0){
        warning('Cases with negative follow-up: Need to double check!')
      }
      
      # For scenarios where individuals left GP before joining UKB, we truncate their FU time to 0
      data<-data%>%mutate(
        time=ifelse(time < 0, 0, time)
      )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'PD follow up time (in years)',
    description = 'If censoring status=0, this fields returns time difference in years between censoring date and baseline date.
    If censoring status=1, this fields returns time to PD event.'
  )
}

# Incident PD status (0=censored 1=event) based on HES/Dth/GP
HESDthGP_PD_status<-function(){
  list(
    name = 'HESDthGP_PD_status',
    source = c('PD_censordate','HESDthGP_PD_eventdate'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$PD_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(HESDthGP_PD_eventdate) & HESDthGP_PD_eventdate<=PD_censordate ~ 1,
          is.na(HESDthGP_PD_eventdate) |(!is.na(HESDthGP_PD_eventdate)&HESDthGP_PD_eventdate>PD_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'Incident PD status',
    description = 'Incident status of PD (0=censored, 1=event) based on HES/Dth/GP data censored at end of GP'
    
  )
}


# PD follow-up time based on HES/Dth/GP
HESDthGP_PD_time<-function(){
  list(
    name = 'HESDthGP_PD_time',
    source = c('HESDthGP_PD_status','PD_censordate','HESDthGP_PD_eventdate','Rec_DateAssess'),
    mapper = function(data){
      
      data=data%>%
        mutate(time=case_when(
          HESDthGP_PD_status==0 ~ as.numeric(difftime(PD_censordate, Rec_DateAssess, unit='days'))/365.25,
          HESDthGP_PD_status==1 ~ as.numeric(difftime(HESDthGP_PD_eventdate, Rec_DateAssess, unit='days'))/365.25))
      
      # Check if cases had negative FU
      if (data%>%filter(HESDthGP_PD_status==1 & time<0)%>%nrow()>0){
        warning('Cases with negative follow-up: Need to double check!')
      }
      
      # For scenarios where individuals left GP before joining UKB, we truncate their FU time to 0
      data<-data%>%mutate(
        time=ifelse(time < 0, 0, time)
      )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'PD follow up time (in years)',
    description = 'If censoring status=0, this fields returns time difference in years between censoring date and baseline date.
    If censoring status=1, this fields returns time to PD event.'
  )
}

# Self-report Dementia
TEU_VeI_Dem <- function(dx_codes = c(1263)) {
  list(
    name = "TEU_VeI_Dem",
    source = c("ID", "Rec_DateAssess",
               paste0("VeI_NonCancerCode.0.", seq(0, 33, by=1)),
               paste0("VeI_NonCancerYear.0.", seq(0, 33, by=1))),
    mapper = FN_VI_filtercodes(dx_codes = dx_codes,
                               colname = "VeI_NonCancer",
                               instance = 0,
                               return_label = "dx",
                               mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv")),
    post_exclusion = FALSE,
    display_name = "Self-reported dementia",
    description = "Whether dementia was reported in verbal interview at baseline"
  )
}

# Dementia derived using HES
TEU_HES_Dem_prev<-function(record_level=TRUE){
  list(
    name='TEU_HES_Dem_prev',
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD9_Mapping_20230210.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD10_Mapping_20230210.xlsx'),
                        #OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = c('Dementia'),
                        return_label = 'baseline',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name='Prevalent dementia at baseline from HES',
    description="Dementia status identified from HES (ICD-9, ICD-10) data prior to or at baseline"
  )
}

# Dementia derived using GP data
TEU_PC_Dem_prev <-function(){
  list(
    name = 'TEU_PC_Dem_prev',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      
      # Read algorithm code list
      Dem_file<-read.xlsx_kdrive(file.path(config$cleaning$HESvsGP_mapping,
                                          "PDDementia_codes_20230210.xlsx"),
                                col_types = "text",sheet=1)
      # Extract the diag codes
      Dem_readv2<-Dem_file%>%filter(Dementia=="1" & `Code Type`=="Read V2")%>%pull(Code)%>%unique()
      
      Dem_readv3<-Dem_file%>%filter(Dementia=="1" & `Code Type`=="Read CTV3")%>%pull(Code)%>%unique()
      
      y<-FN_PCevent_filtercodes(read2_codes = Dem_readv2,read3_codes=Dem_readv3,
                                return_label = 'baseline')(data)
      
      return(y)
    },
    post_exclusion = FALSE, #To resolve memory issue
    display_name = 'Prevalent dementia dx by gp clinical data',
    description = "Prevalent dementia diagnosis at baseline derived from gp_clinical"
  )
}

# Dementia HES FU date
TEU_HES_Dem_fudate<-function(record_level=FALSE){
  list(
    name='TEU_HES_Dem_fudate',
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(ICD9_xlsx = file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD9_Mapping_20230210.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD10_Mapping_20230210.xlsx'),
                        #OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = 'Dementia',
                        return_label = 'followup_date',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name='Dementia date of diagnoses at follow-up',
    description='Dementia diagnoses date identified from HES (ICD-9 and 10) data after baseline'
  )
}

# Dementia Dth date (primary + secondary)
TEU_Dth_Dem_dthdate <-function(record_level=TRUE,level="any"){
  list(
    name = 'TEU_Dth_Dem_dthdate',
    source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
    mapper = function(data){
      mapping=read.xlsx_kdrive(file.path(config$cleaning$HESvsGP_mapping,'PDDementia_ICD10_Mapping_20230210.xlsx'),col_types = c('text'))
      ICD10_codes<-mapping[which(mapping$Conditions=='Dementia'),]$Code
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level,level=level)(data)
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Dementia death date',
    description = paste0('Death date caused by Dementia from Death Registry data (both primary + secondary causes)',
                         'The data used was ', if(record_level) {'record-level data from the UKB data portal.'} 
                         else {'summary data from the UKB data showcase'})
  )
}


TEU_Dth_NotDem_dthdate <-function(){
  list(
    name = 'TEU_Dth_NotDem_dthdate',
    source = c("ID","TEU_Dth_Dem_dthdate"),
    mapper = function(data){
      
      # Create a list of IDs who died from dementia (primary or secondary cause)
      Dem_ID<-data%>%
        filter(!is.na(TEU_Dth_Dem_dthdate))%>%
        pull(ID)
      
      # Read in record-level dth data and remove ppl who died from Dementia
      deaths_all<-FN_Death_registry()%>%
        #rename(Dth_ICD10 = Dth_ICD10All)%>%
        filter(!ID %in% Dem_ID)
      
      y <- deaths_all[["dth_date"]][match(data$ID, deaths_all$ID)]
      return(y)
      
    },
    post_exclusion = FALSE,
    display_name = 'Non Dementia death date',
    description = 'Death date caused by non Dementia from Death Registry data'
  )
}

HESDth_Dem_censordate<-function(){
  list(
    name = 'HESDth_Dem_censordate',
    source = c('TEU_Dth_NotDem_dthdate','Admin_CensorDate','BaC_LostFUDate'),
    mapper = function(data){
      y<-pmin(data$TEU_Dth_NotDem_dthdate,data$Admin_CensorDate,data$BaC_LostFUDate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Dementia censoring date',
    description = 'Censoring date for Dementia outcome'
  )
}


HESDth_Dem_eventdate<-function(){
  list(
    name = 'HESDth_Dem_eventdate',
    source = c('TEU_HES_Dem_fudate','TEU_Dth_Dem_dthdate'),
    mapper = function(data){
      y<-pmin(data$TEU_HES_Dem_fudate,data$TEU_Dth_Dem_dthdate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Incident Dementia event date',
    description = 'Incident Dementia event date at follow-up based on HES, Death Registry'
  )
  
}

# Date of Dementia dx based on PC clinical event data
PC_Dem_eventdate <-function(){
  list(
    name = 'PC_Dem_eventdate',
    source = c('ID','Rec_DateAssess'),
    mapper = function(data){
      
      # Read algorithm Dementia code list
      Dem_file<-read.xlsx_kdrive(file.path(config$cleaning$HESvsGP_mapping,
                                          "PDDementia_codes_20230210.xlsx"),
                                col_types = "text",sheet=1)
      # Extract the Dementia diag codes
      Dem_readv2<-Dem_file%>%filter(Dementia=="1" & `Code Type`=="Read V2")%>%pull(Code)%>%unique()
      
      Dem_readv3<-Dem_file%>%filter(Dementia=="1" & `Code Type`=="Read CTV3")%>%pull(Code)%>%unique()
      
      y<-FN_PCevent_filtercodes(read2_codes = Dem_readv2,read3_codes=Dem_readv3,return_label = 'followup_date')(data)
      
      return(y)
    },
    post_exclusion = FALSE, 
    display_name = 'Date of incident dementia dx by gp clinical data',
    description = 'Date of incident dementia dx derived from gp_clinical'
  )
}

HESDthGP_Dem_eventdate<-function(){
  list(
    name = 'HESDthGP_Dem_eventdate',
    source = c('TEU_HES_Dem_fudate','TEU_Dth_Dem_dthdate','PC_Dem_eventdate'),
    mapper = function(data){
      y<-pmin(data$TEU_HES_Dem_fudate,data$TEU_Dth_Dem_dthdate,data$PC_Dem_eventdate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Incident dementia event date',
    description = 'Incident dementia event date at follow-up based on HES, Death Registry, GP dx'
  )
}

Dem_censordate<-function(){
  list(
    name = 'Dem_censordate',
    source = c('HESDth_Dem_censordate','Admin_PC_CensorDate'),
    mapper = function(data){
      y<-pmin(data$HESDth_Dem_censordate,data$Admin_PC_CensorDate,na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Dementia censoring date',
    description = 'Censoring date for dementia outcome, incorporating Admin HES/Dth/GP censoring,
    deduction date, lost to follow-up date, Dth by other causes.'
  )
}


# Incident Dementia status (0=censored 1=event) based on HES/Dth
HESDth_Dem_status<-function(){
  list(
    name = 'HESDth_Dem_status',
    source = c('Dem_censordate','HESDth_Dem_eventdate'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$Dem_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(HESDth_Dem_eventdate) & HESDth_Dem_eventdate<=Dem_censordate ~ 1,
          is.na(HESDth_Dem_eventdate) |(!is.na(HESDth_Dem_eventdate)&HESDth_Dem_eventdate>Dem_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'Incident dementia status',
    description = 'Incident status of dementia (0=censored, 1=event) based on HES/Dth data censored at end of GP'
    
  )
}


# Dementia follow-up time based on HES/Dth
HESDth_Dem_time<-function(){
  list(
    name = 'HESDth_Dem_time',
    source = c('HESDth_Dem_status','Dem_censordate','HESDth_Dem_eventdate','Rec_DateAssess'),
    mapper = function(data){
      
      data=data%>%
        mutate(time=case_when(
          HESDth_Dem_status==0 ~ as.numeric(difftime(Dem_censordate, Rec_DateAssess, unit='days'))/365.25,
          HESDth_Dem_status==1 ~ as.numeric(difftime(HESDth_Dem_eventdate, Rec_DateAssess, unit='days'))/365.25))
      
      # Check if cases had negative FU
      if (data%>%filter(HESDth_Dem_status==1 & time<0)%>%nrow()>0){
        warning('Cases with negative follow-up: Need to double check!')
      }
      
      # For scenarios where individuals left GP before joining UKB, we truncate their FU time to 0
      data<-data%>%mutate(
        time=ifelse(time < 0, 0, time)
      )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'Dementia follow up time (in years)',
    description = 'If censoring status=0, this fields returns time difference in years between censoring date and baseline date.
    If censoring status=1, this fields returns time to dementia event.'
  )
}

# Incident Dementia status (0=censored 1=event) based on HES/Dth/GP
HESDthGP_Dem_status<-function(){
  list(
    name = 'HESDthGP_Dem_status',
    source = c('Dem_censordate','HESDthGP_Dem_eventdate'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$Dem_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(HESDthGP_Dem_eventdate) & HESDthGP_Dem_eventdate<=Dem_censordate ~ 1,
          is.na(HESDthGP_Dem_eventdate) |(!is.na(HESDthGP_Dem_eventdate)&HESDthGP_Dem_eventdate>Dem_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'Incident dementia status',
    description = 'Incident status of dementia (0=censored, 1=event) based on HES/Dth/GP data censored at end of GP'
    
  )
}


# Dementia follow-up time based on HES/Dth/GP
HESDthGP_Dem_time<-function(){
  list(
    name = 'HESDthGP_Dem_time',
    source = c('HESDthGP_Dem_status','Dem_censordate','HESDthGP_Dem_eventdate','Rec_DateAssess'),
    mapper = function(data){
      
      data=data%>%
        mutate(time=case_when(
          HESDthGP_Dem_status==0 ~ as.numeric(difftime(Dem_censordate, Rec_DateAssess, unit='days'))/365.25,
          HESDthGP_Dem_status==1 ~ as.numeric(difftime(HESDthGP_Dem_eventdate, Rec_DateAssess, unit='days'))/365.25))
      
      # Check if cases had negative FU
      if (data%>%filter(HESDthGP_Dem_status==1 & time<0)%>%nrow()>0){
        warning('Cases with negative follow-up: Need to double check!')
      }
      
      # For scenarios where individuals left GP before joining UKB, we truncate their FU time to 0
      data<-data%>%mutate(
        time=ifelse(time < 0, 0, time)
      )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'Dementia follow up time (in years)',
    description = 'If censoring status=0, this fields returns time difference in years between censoring date and baseline date.
    If censoring status=1, this fields returns time to dementia event.'
  )
}

Dem_Depression<-function(){
  list(
    name="Dem_Depression",
    source=c("PsF_SeenPsychiat.0.0","PsF_SeenGP.0.0"),
    mapper=function(data){
      # self-report either yes then yes
      data<-data%>%
        mutate_all(~FN_toNA(values=c("Prefer not to answer","Do not know"))(.))%>%
        mutate(y=factor(case_when(
          is.na(PsF_SeenPsychiat.0.0) & is.na(PsF_SeenGP.0.0) ~ NA_character_,
          PsF_SeenPsychiat.0.0=="Yes" | PsF_SeenGP.0.0=="Yes" ~ "Yes",
          TRUE ~ "No"
        ),levels=c("No","Yes")))
      return(data$y)
    },
    post_exclusion=FALSE,
    display_name="Depression",
    description="Self-reported nerves, anxiety, tension or depression seen by psychiatrist or GP (Mukadam2022)"
  )
}


Dem_HearingLoss<-function(){
  list(
    name="Dem_HearingLoss",
    source=c("HMH_HearingProbs.0.0","HMH_HearingProbs_p.0.0","HMH_HearingAid.0.0"),
    mapper=function(data){
      # First merge the main and pilot fields together
      data<-data%>%
        mutate(HearingProbs=coalesce(HMH_HearingProbs.0.0,HMH_HearingProbs_p.0.0))%>%
        mutate_all(~FN_toNA(values=c("Prefer not to answer","Do not know"))(.))%>%
        mutate(y=factor(case_when(
          is.na(HearingProbs) & is.na(HMH_HearingAid.0.0) ~ NA_character_,
          HearingProbs%in%c("Yes","I am completely deaf") | HMH_HearingAid.0.0=="Yes" ~ "Yes",
          TRUE ~ "No" 
        ),levels=c("No","Yes")))
      return(data$y)
    },
    post_exclusion=FALSE,
    display_name="Hearing loss",
    description="Self-reported hearing impairment"
  )
}

Stevenson_HearingLoss<-function(){
  list(
    name="Stevenson_HearingLoss",
    source="HMH_HearingProbs.0.0",
    mapper=FN_labelfactor(recodeNA=c("Prefer not to answer","Do not know")),
    post_exclusion=FALSE,
    display_name="Hearing impairment",
    description="Hearing impairment"
  )
}

Dem_SocialIso_Mukadam2022<-function(){
  list(
    name="Dem_SocialIso",
    source=c("PsF_VisitFreq.0.0","PsF_VisitFreq_p.0.0"),
    mapper=function(data){
      data<-data%>%
        mutate(VisitFreq=coalesce(PsF_VisitFreq.0.0,PsF_VisitFreq_p.0.0))%>%
        mutate_all(~FN_toNA(values = c("Prefer not to answer","Do not know"))(.))%>%
        mutate(y=factor(case_when(
          is.na(VisitFreq)~NA_character_,
          VisitFreq=="Almost daily"~"Almost daily",
          TRUE ~ "Less than daily"
        ),levels=c("Almost daily","Less than daily")))
      return(data$y)
    },
    post_exclusion=FALSE,
    display_name="Social contact",
    description="Social contact (Mukadam2022)"
  )
}

Dem_LeisureAct<-function(){
  list(
    name="Dem_LeisureAct",
    source=paste0("PsF_LeisureAct.0.",0:4),
    mapper=function(data){
      data<-data%>%
        mutate(LeisureAct=coalesce(PsF_LeisureAct.0.0,
                                   PsF_LeisureAct.0.1,
                                   PsF_LeisureAct.0.2,
                                   PsF_LeisureAct.0.3,
                                   PsF_LeisureAct.0.4))%>%
        mutate(y=factor(case_when(LeisureAct=="Prefer not to answer" | is.na(LeisureAct)~NA_character_,
                           LeisureAct=="None of the above" ~ "Not engage in activities",
                           !is.na(LeisureAct) ~ "Engage in activities"),
                        levels=c("Engage in activities","Not engage in activities")))
      return(data$y)
    },
    post_exclusion=FALSE,
    display_name="Leisure/social activities",
    description="Whether participants engage in the specified leisure/social activities once a week or more often"
  )
}

Dem_SocialScore<-function(){
  list(
    name="Dem_SocialScore",
    source=c("HoH_HouseholdSize.0.0","PsF_VisitFreq.0.0","PsF_VisitFreq_p.0.0",
             "Dem_LeisureAct"),
    mapper=function(data){
      
      data<-data%>%
        # First categorise missing categories as NA
        mutate(size=FN_toNA(values=c(-1,-3))(HoH_HouseholdSize.0.0),
               VisitFreq=FN_toNA(values=c("Prefer not to answer","Do not know"))(coalesce(PsF_VisitFreq.0.0,PsF_VisitFreq_p.0.0)))%>%
        # Create point system
        mutate(size_point=ifelse(!is.na(size) & size>1,0,size),
               VisitFreq_point=case_when(
                 is.na(VisitFreq) ~ NA_real_,
                 VisitFreq %in% c("Almost daily","2-4 times a week","About once a week","About once a month")~0,
                 TRUE~1
               ),
               Leisure_point=as.numeric(Dem_LeisureAct)-1)%>%
        rowwise()%>%
        # Produce NA if all fields are NA
        mutate(sum=ifelse(is.na(size_point) & is.na(VisitFreq_point) & is.na(Leisure_point),NA,
                            sum(size_point,VisitFreq_point,Leisure_point,na.rm = TRUE)))
      
      return(data$sum)
      
    },
    post_exclusion=FALSE,
    display_name="Social contact score",
    description="Social contact. Range 0-3 (Stevenson2022)"
  )
}

Dem_SocialIso<-function(){
  list(
    name="Dem_SocialIso",
    source="Dem_SocialScore",
    mapper=function(x){
      y <-
        as.character(cut(x, breaks = c(0, 2, 4), right = FALSE))
      y <-
        factor(
          y,
          levels = c("[0,2)","[2,4)"),
          labels = c("No", "Yes")
        )
      return(y)
    },
    post_exclusion=FALSE,
    display_name="Social isolation",
    description="Social isolation. No (score <2) and Yes (score >=2) (Stevenson2022)"
  )
}

Dem_AirPollution<-function(){
  list(
    name="Dem_AirPollution",
    source="Air_PM25.0.0",
    mapper=function(x){
      y <-
        as.character(cut(x, breaks = c(0, 10,30), right = TRUE))
      #y[is.na(y)] <- "Unknown"
      y <-
        factor(
          y,
          levels = c("(0,10]", "(10,30]"),
          labels = c("Normal", "Above 10")
        )
      return(y)
    },
    post_exclusion=FALSE,
    display_name="Air pollution",
    description="Air pollution (Mukadam2022)"
  )
}

TEU_APOE_count <- function(alleles=c(1, 2, 3, 4)){
  listlist <- rep(list(list()), length(alleles))
  index <- 1
  for(i in alleles){
    listlist[[index]] <- list(
      name = paste0("TEU_APOE_e", i, "_count"),
      source = paste0("ID"),
      mapper = FN_JoinPRS(filepath=file.path("K:\\TEU\\2021\\APOEonDementia\\Git_Repo\\Data\\Derived\\gen_apoe.rds"),
                          colname=paste0("apoe", i)),
      post_exclusion=FALSE,
      display_name = paste0("APOE e", i, " count"),
      description = paste0("Number of ApoE e", i, " alleles")
    )
    index <- index + 1
  }
  return(listlist)
}

TEU_APOE_e4_carrier <- function() {
  list(
    name = "TEU_APOE_e4_carrier", 
    source = c("TEU_APOE_e4_count"), 
    mapper = function(x) {
      y <- x>0
      y <- factor(as.numeric(y), levels=c(0,1), labels=c("Not a carrier", "e4 carrier"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "ApoE e4 carrier",
    description = "Does this person have one or more copies of ApoE e4"
  )
}

# Incident Dementia status (0=censored 1=event) based on HES/Dth with full follow-up
TEU_Dem_status<-function(){
  list(
    name = 'TEU_Dem_status',
    source = c('HESDth_Dem_censordate','HESDth_Dem_eventdate'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$HESDth_Dem_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(HESDth_Dem_eventdate) & HESDth_Dem_eventdate<=HESDth_Dem_censordate ~ 1,
          is.na(HESDth_Dem_eventdate) |(!is.na(HESDth_Dem_eventdate)&HESDth_Dem_eventdate>HESDth_Dem_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'Incident dementia status',
    description = 'Incident status of dementia (0=censored, 1=event) based on HES/Dth data censored at end of HES/Dth'
    
  )
}


# Dementia follow-up time based on HES/Dth with full follow-up
TEU_Dem_time<-function(){
  list(
    name = 'TEU_Dem_time',
    source = c('TEU_Dem_status','HESDth_Dem_censordate','HESDth_Dem_eventdate','Rec_DateAssess'),
    mapper = function(data){
      
      data=data%>%
        mutate(time=case_when(
          TEU_Dem_status==0 ~ as.numeric(difftime(HESDth_Dem_censordate, Rec_DateAssess, unit='days'))/365.25,
          TEU_Dem_status==1 ~ as.numeric(difftime(HESDth_Dem_eventdate, Rec_DateAssess, unit='days'))/365.25))
      
      # Check if cases had negative FU
      if (data%>%filter(TEU_Dem_status==1 & time<0)%>%nrow()>0){
        warning('Cases with negative follow-up: Need to double check!')
      }
      
      # For scenarios where individuals left GP before joining UKB, we truncate their FU time to 0
      data<-data%>%mutate(
        time=ifelse(time < 0, 0, time)
      )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'Dementia follow up time (in years)',
    description = 'If censoring status=0, this fields returns time difference in years between censoring date and baseline date.
    If censoring status=1, this fields returns time to dementia event.'
  )
}
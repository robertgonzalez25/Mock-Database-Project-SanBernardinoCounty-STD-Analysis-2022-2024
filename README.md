Open the "SB STD CODE.R" file, apply the codes shown under the "***RUN***" text to manipulate your STD dataset on R Studio.
Your STD dataset is named as "finalr_practice(AutoRecovered).csv".
The STD dataset contains 1288 unique rows which could be manipulated using your "SB STD CODE.R".
Pick your disease (Chlamydia, Gonorrhea, or Syphilis) and discover unique counts, age-specific rates by age-groups, and incidence rates per year. 
Prior to attempting to analyze your STD data set, clean your table to make it easier to process.


Data Cleaning Formulas needing for input (original file: finalr_practice (AutoRecovered).csv)) 

stdreport <- finalr_practice_AutoRecovered_

stdreport$yrreported <- str_sub(stdreport$dtcreate, 1,4)

stdreport$region <- ifelse(stdreport$zipcd %in% c(92376),"Rialto",
ifelse(stdreport$zipcd %in% c(92405),"San Bernardino",
ifelse(stdreport$zipcd %in% c(92345),"Hesperia",
ifelse(stdreport$zipcd %in% c(92354),"Loma Linda",
ifelse(stdreport$zipcd %in% c(92336),"Fontana",
ifelse(stdreport$zipcd %in% c(91762),"Ontario","Unknown"))))))


stdreport$age_grp <- ifelse(stdreport$Age < 20, "<20", 
ifelse(stdreport$Age >= 20 & stdreport$Age <= 29,"20-29", 
ifelse(stdreport$Age >= 30 & stdreport$Age <= 39, "30-39",
ifelse(stdreport$Age >= 40 & stdreport$Age <= 49, "40-49",
ifelse(stdreport$Age >= 50 & stdreport$Age <= 59, "50-59",
ifelse(stdreport$Age >= 60 & stdreport$Age <= 69, "60-69", "unk"))))))



stdreport <- stdreport %>% 
filter(yrreported %in% (2022:2024)) %>% 
filter(status_report=="confirmed" |status_report=="probable")

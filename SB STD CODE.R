Mock Database Project: San Bernardino County STD Analysis (2022-2024) 


This data displays randomized numbers and categorical variables generated in Excel using "RANDBETWEEN" function - no authentic reports of any kind were used in this project


Concept #1: Ensure your packages are installed 

**RUN****3
some_packages <- c("rio", "here", "janitor", "lubridate", "flextable", "tidyverse", "skimr", "praise", 
                   "epikit", "gtsummary", "tidylog", "scales", "ggExtra", "gghighlight", "apyramid", "viridis", "tsibble", "readr", "gapminder", "readxl",
                   "data.table", "writexl", "gt", "stringr", "dplyr", "tidyr")
*RUN**
lapply(some_packages, library, character.only=TRUE)

***RUN****
install.packages (some_packages)

Concept #1(2): After you have converted your Excel data into a .csv file import the dataset into Rstudio



Concept #2: Rename your Data 
***RUN***
stdreport <- finalr_practice_AutoRecovered_


Concept #3: Create a new column that will just capture the year an STD was reported.
Name this column "yrreported"
Just focus on the year an STD was reported (2022,2023,2024) you dont need the month/day (as seen in the "dtcreate" column)

***RUN***
stdreport$yrreported <- str_sub(stdreport$dtcreate, 1,4)



Concept #4
Switch the new column "yrreported" from "character" -> "numberic"
***RUN***
stdreport$yrreported <- as.numeric(stdreport$yrreported)

Concept #5: Adding Regions. Associate regions to your dataset in order to analyze how many cases per region in the county
Concept #5(2): This hypothetical "San Bernardino County" only includes these cities
Cities we are including (Loma Linda, Hesperia San Bernardino, Rialto, Fontana, and Ontario)
***RUN***
stdreport$region <- ifelse(stdreport$zipcd %in% c(92376),"Rialto",
ifelse(stdreport$zipcd %in% c(92405),"San Bernardino",
ifelse(stdreport$zipcd %in% c(92345),"Hesperia",
ifelse(stdreport$zipcd %in% c(92354),"Loma Linda",
ifelse(stdreport$zipcd %in% c(92336),"Fontana",
ifelse(stdreport$zipcd %in% c(91762),"Ontario","Unknown"))))))



Concept #6: Categorize the age ranges into categorical variables. Create another column showing it 
(Age Groups: <20, 20-29, 30-39, 40-49, 50-59, 60-69)

***RUN***
stdreport$age_grp <- ifelse(stdreport$Age < 20, "<20", 
ifelse(stdreport$Age >= 20 & stdreport$Age <= 29,"20-29", 
ifelse(stdreport$Age >= 30 & stdreport$Age <= 39, "30-39",
ifelse(stdreport$Age >= 40 & stdreport$Age <= 49, "40-49",
ifelse(stdreport$Age >= 50 & stdreport$Age <= 59, "50-59",
ifelse(stdreport$Age >= 60 & stdreport$Age <= 69, "60-69", "unk"))))))


Concept #7: Clean your data. With infectious diseases, get rid of "suspect" cases - leave "probable" and "confirmed"
Concept #7 (2): There is only a handful of 2021 cases, filter these out in order to focus on 2022-2024 trends

***RUN***
stdreport <- stdreport %>% 
filter(yrreported %in% (2022:2024)) %>% 
filter(status_report=="confirmed" |status_report=="probable")

filter: removed 61 rows (5%), 1,226 rows remaining
filter: removed 8 rows (1%), 1,218 rows remaining

Concept #8 (1): For the purpose of this code, lets just focus on the disease "Chlamydia" 
Concept #8 (2): Chlamydia Counts by Age-Group & Year, San Bernardino County (2022-2024)
Concept #8(3) Filter all chlamydia counts from your STD report by year

2022
***RUN***
stdreport %>% 
filter(Disease == "Chlamydia") %>% 
filter(yrreported %in% (2022)) %>% 
group_by(Disease, age_grp) %>% 
summarise(count =n())

**RESULTS**
Disease   age_grp count
<chr>     <chr>   <int>
1 Chlamydia 20-29      76
2 Chlamydia 30-39      26
3 Chlamydia 40-49       8
4 Chlamydia 50-59       8
5 Chlamydia 60-69       1
6 Chlamydia <20         7
7 Chlamydia NA          5

2023
***RUN***
stdreport %>% 
filter(Disease == "Chlamydia") %>% 
filter(yrreported %in% (2023)) %>% 
group_by(Disease, age_grp) %>% 
summarise(count =n())

**RESULTS**
Disease   age_grp count
<chr>     <chr>   <int>
1 Chlamydia 20-29     208
2 Chlamydia 30-39      76
3 Chlamydia 40-49      14
4 Chlamydia 50-59      14
5 Chlamydia 60-69       7
6 Chlamydia <20         6
7 Chlamydia NA          9

2024
***RUN***
stdreport %>% 
filter(Disease == "Chlamydia") %>% 
filter(yrreported %in% (2024)) %>% 
group_by(Disease, age_grp) %>% 
summarise(count =n())

**RESULTS**
Disease   age_grp count
<chr>     <chr>   <int>
1 Chlamydia 20-29     110
2 Chlamydia 30-39      10
3 Chlamydia 40-49       5
4 Chlamydia 50-59       3
5 Chlamydia 60-69       2
6 Chlamydia <20         5
7 Chlamydia NA          5

If applied to a line graph, we would see trends
Chlamydia cases for 30-39 year olds increased from 2022 to 2023,
Chlamydia cases for 20-29 year olds increased from 2022 to 2023
Chlamydia cases for 60-69 year olds took a high spike from 2022 to 2023



Concept #9: Chlamydia Age-Specific Rates by Sex and Age-Group, San Bernardino County (2022-2024)
Look at the incidence rate of Chlamydia cases among both MALES and FEMALES - within EACH age-group 
See which age-group (among both MALES and FEMALES) has the highest rates
LETS BEGIN


Concept #9(2): Start by using the "2023 DMV census" (Theoritical). The population count per age-group/sex will be your denominator
The 2023 DMV population  census shows the following population count for each age group in San Bernardino County 
Females(>20): 200 | Males(>20): 300
Females(20-29): 4100 | Males(20-29): 3700
Females(30-39): 3400 | Males(30-39): 3100
Females(40-49): 2900 | Males(40-49): 2900
Females(50-59): 2100 | Males(50-59): 1700
Females(60-69): 1300 | Males(60-69): 1200
Females(70-79): 1000 | Males(70-79): 1000
Females(80-89): 450  | Males(80-89): 500
Females(90-99): 140  | Males(90-99): 100

Lets say the overall population of San Bernardino County, if you were to add all age groups up, would be
Total Population: 30,090

Concept #9 (2): Now, to get age-specific rates, see all chlamydia cases grouped by sex and age-group
You could use the code below to see all chlamydia cases among sex and age-groups per year
                          ****But to keep this code clean, lets just look into (2023)**


2023 Chlamydia Female Cases
***RUN***
stdreport %>%
filter(Disease == "Chlamydia") %>% 
filter(Gender == "Female") %>% 
filter(yrreported %in% (2023)) %>% 
group_by(Disease, age_grp) %>% 
summarise(count= n())

**RESULTS**
Disease   age_grp count
<chr>     <chr>   <int>
  1 Chlamydia 20-29   168
2 Chlamydia 30-39      50
3 Chlamydia 40-49       9
4 Chlamydia 50-59       7
5 Chlamydia 60-69       4
6 Chlamydia <20         3
7 Chlamydia NA          6

Concept #9 (3): Implement the Female DMV population as your denominator into STD Report 
Create a date frame called "female_pop" 

***RUN***
female_pop <- data.frame(
  age_grp = c("<20", "20-29", "30-39", "40-49", "50-59", "60-69"),
  female_population = c(200, 4100, 3400, 2900, 2100, 1300)
  )

Concept #9(4): Tell R to divide the counts of Chlamydia Cases for each Female age-group in 2023 by the corresponding female age-group generalpopulation
Concept #9(5): We are using the year "2023" to look for trends because it is the median year from 2022-2024
***RUN***
stdreport %>%
filter(Disease == "Chlamydia") %>% 
filter(Gender == "Female") %>% 
filter(yrreported %in% (2023)) %>% 
group_by(Disease, age_grp) %>% 
summarise(count= n(), .groups = "drop") %>% 
left_join(female_pop, by = "age_grp")


***RESULTS***
Disease   age_grp count female_population
<chr>     <chr>   <int>             <dbl>
  1 Chlamydia 20-29     168             4100
2 Chlamydia 30-39      50              3400
3 Chlamydia 40-49       9              2900
4 Chlamydia 50-59       7              2100
5 Chlamydia 60-69       4              1300
6 Chlamydia <20         3               200
7 Chlamydia NA          6                NA

Concept #9(6): Tell R to divide the count by the corresponding female population then multiply by 1000
mutate(rate_1000 = count / female_population) %>% 
mutate(multiply1000 = rate_1000 * 1000)

***RUN***
stdreport %>%
  filter(Disease == "Chlamydia") %>% 
  filter(Gender == "Female") %>% 
  filter(yrreported %in% (2023)) %>% 
  group_by(Disease, age_grp) %>% 
  summarise(count= n(), .groups = "drop") %>% 
  left_join(female_pop, by = "age_grp") %>% 
  mutate(rate_1000 = count / female_population) %>% 
  mutate(multiply1000 = rate_1000 * 1000)
**RESULTS**
Disease   age_grp count female_population rate_1000 multiply1000
<chr>     <chr>   <int>             <dbl>     <dbl>        <dbl>
  1 Chlamydia 20-29     168              4100   0.0410         41.0 
2 Chlamydia 30-39      50              3400   0.0147         14.7 
3 Chlamydia 40-49       9              2900   0.00310         3.10
4 Chlamydia 50-59       7              2100   0.00333         3.33
5 Chlamydia 60-69       4              1300   0.00308         3.08
6 Chlamydia <20         3               200   0.015          15   
7 Chlamydia NA          6                NA  NA              NA   

Concept #9(7): Repeat everything with the MALE Population

2023 Chlamydia Male Cases
***RUN***
stdreport %>% 
filter(Disease == "Chlamydia") %>% 
filter(Gender == "Male") %>% 
filter(yrreported %in% (2023)) %>% 
group_by(Disease, age_grp) %>% 
summarise(count =n())
**RESULTS**
Disease   age_grp count
<chr>     <chr>   <int>
  1 Chlamydia 20-29    40
2 Chlamydia 30-39      26
3 Chlamydia 40-49       5
4 Chlamydia 50-59       7
5 Chlamydia 60-69       3
6 Chlamydia <20         3
7 Chlamydia NA          3

***RUN***
male_pop <- data.frame(
  age_grp = c("<20", "20-29", "30-39", "40-49", "50-59", "60-69"),
  male_population = c(300, 3700, 3100, 2900, 1700, 1200)
)

***RUN***
stdreport %>% 
  filter(Disease == "Chlamydia") %>% 
  filter(Gender == "Male") %>% 
  filter(yrreported %in% (2023)) %>% 
  group_by(Disease, age_grp) %>% 
  summarise(count= n(), .groups = "drop") %>% 
  left_join(male_pop, by = "age_grp") %>% 
mutate(rate_1000 = count / male_population) %>% 
mutate(multiply1000 = rate_1000 * 1000)

**RESULTS**
Disease   age_grp count male_population rate_1000 multiply1000
<chr>     <chr>   <int>           <dbl>     <dbl>        <dbl>
  1 Chlamydia 20-29      40         3700   0.0108         10.8 
2 Chlamydia 30-39      26           3100   0.00839         8.39
3 Chlamydia 40-49       5           2900   0.00172         1.72
4 Chlamydia 50-59       7           1700   0.00412         4.12
5 Chlamydia 60-69       3           1200   0.0025          2.5 
6 Chlamydia <20         3           300    0.01            10   
7 Chlamydia NA          3           NA     NA              NA   


In 2023, Females, Ages (20-29), had the highest incidence rates of infection for Chlamydia
In 2023, Males, Ages (20-29), had the highest incidence rates of infection for Chlamydia 


Concept #10: Chlamydia Incidence Rates per Year in San Bernardino County. See how San Bernardino County is doing when dealing with Chlamydia in general
***RUN***
stdreport %>% 
  filter(Disease == "Chlamydia") %>% 
  group_by(Disease, yrreported) %>% 
  summarise(count= n())

**RESULTS**
Disease   yrreported count
<chr>          <dbl> <int>
  1 Chlamydia     2022   131
2 Chlamydia       2023   334
3 Chlamydia       2024   140

Concept #10(2): Create a new column showing the results of cases/the hypothetical San Bernardino County Population 

***RUN***
stdreport %>% 
  filter(Disease == "Chlamydia") %>% 
  group_by(Disease, yrreported) %>% 
  summarise(count= n()) %>% 
  mutate(chlamyratebyyr = count/30090) %>% 
**RESULTS**  
<chr>          <dbl> <int>          <dbl>
1 Chlamydia       2022   131        0.00435
2 Chlamydia       2023   334        0.0111 
3 Chlamydia       2024   140        0.00465

*If multiply by a standard 1000 (smaller population) you would get the rate for each year*
  
Chlamydia 2022: 4.35 per 1000 population or 43.5 per 10,000
Chlamydia 2023: 11.1 per 1000 population or 111 per 10,000
Chlamydia 2024: 4.65 per 1000 population  46.5 per 10,000



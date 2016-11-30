########################################################
#            Working Notes
#


# Drop out email addresses

# Consider reverse coded items - for calculating reliability
# the keys parameter can be used (psych package). If some
# of the items will be analysed on their own, may need to flag
# them in some way.
#
#
# D1_x vars have odd values (13, 14, etc). Need recoding.

# names(df)  # lists all the variable names with column numbers, so can reference by number.


# Get files into R
# Change path and file names as necessary.
folder <- file.path("C:","Users","jckenned","Documents","Consulting","Convergence Partners Survey","Data Analysis")
datafile1 <- "20161119_Womens_Careers_and_Aspirations.csv"
datafile2 <- "20161119_Womens_Careers_and_Aspirations_2.csv"
datafile3 <- "B4 and B5.csv"
library(dplyr)
library(readr) 
library(ggplot2)
library(psych)   # Creating scales and calculating reliability and other descriptives.
library(car)     # Used for the recode function.
library(lubridate)

# read_CSV in readr is better than R's built-in read.csv function; see Peng
# msdr.pdf p. 14.

# Nice info here on using the RStudio <Import Dataset> gui 
# for bringing in the csv file - creates syntax reflecting
# choices made. 
# https://support.rstudio.com/hc/en-us/articles/218611977-Importing-Data-with-RStudio

# Set up vector of variable names for use in the read_csv function.

varNames <- c("ResponseID", "ResponseSet", "Name", "ExtDataRef", "Email", 
              "IP_Add", "Status", "StartDate", "EndDate", "Finished", NA, 
              NA, NA, "Token", NA, "Marital", "Age", "WkHours", "JobStatus", 
              "JobStatTXT", "Deps0_5", "Deps5_10", "Deps11_18", "Deps19_30", 
              "DepsOther", NA, "JobTitle", "Seniority", "SeniorityTXT", 
              "Ethnic", "EthnicTXT", "Tenure", "Overseas", "Educ", "EducTXT", 
              "TotSal", "DirReps", "NinOrg", "OccGp", "Sector", "SectorTXT", 
              "NZRegion", "AKRegion", NA, "C1_1_ImpJob", "C1_2_ImpFam", 
              "C1_3_ImpLife", "C2_1_WLB", "C2_2_WLB", "C2_3_WLB", "C2_4_WLB", 
              "C2_5_WLB", "C3_Aspire", "C3_AspireTXT", "C4_AspChange", 
              "C4_AspChangeTXT", "C5_1_CarAdv", "C5_2_CarAdv", "C5_3_CarAdv", 
              "C5_4_CarAdv", "C5_5_CarAdv", "C5_6_CarAdv", "C5_7_CarAdv", 
              "C5_8_CarAdv", "C5_1_CarAdvTXT", "C6_PrimCareResp", 
              "C6_PrimCareRespTXT", "C7_PrimRespHHold", "C7_PrimRespHHoldTXT", 
              NA, "D1_1_Barrier", "D1_2_Barrier", "D1_5_Barrier", 
              "D1_6_Barrier", "D1_7_Barrier", "D1_7_BarrierTXT", "D1_8_Barrier",
              "D2_1_Conf", "D2_2_SE", "D2_3_PubSpk", "D2_4_IntPers", "D2_5_IQ", 
              "D2_6_PersEffic", "D2_7_PersEffect", "D2_8_Tech", "D3_1_Decn", 
              "D3_2_Grow", "D3_3_Probs", "D3_4_Chall", NA, "E1_1_CultFit", 
              "E1_2_CultFit", "E1_3_CultFit", "E1_4_CultFit", "E1_5_CultFit", 
              "E1_6_CultFit", "E1_7_CultFit", "E2_1_Network", "E2_2_Network", 
              "E3_1_Mentor", "E3_2_Mentor", "E3_3_Mentor", "E3_4_Mentor", 
              "E4_1_CarMgt", "E4_2_CarMgt", "E4_3_CarMgt", "E5_1_DevAsst", 
              "E5_2_DevAsst", "E5_3_DevAsst", "E5_4_DevAsst", "E5_5_DevAsst", 
              "E5_6_DevAsst", "E5_7_DevAsst", "E6_1_GeogMob", "E6_2_GeogMob", 
              "E6_3_GeogMob", "E6_4_GeogMob", "E6_5_GeogMob", "E6_6_GeogMob", 
              "E6_7_GeogMob", "E6_8_GeogMob", "E6_8_GeogMobTXT", "E7_Barriers", 
              NA, "F1_1_TrackRcd", "F2_1_OwnCareer", "F2_2_OwnCareer", 
              "F2_3_OwnCareer", "F2_4_OwnCareer", "F3_1_Relships", 
              "F3_2_Relships", "F3_3_Relships", "F3_4_Relships", "F4_1_Mentor", 
              "F4_2_Mentor", "F4_3_Mentor", "F4_4_Mentor", "F4_5_Mentor", 
              "F4_6_Mentor", "F4_7_Mentor", "F4_8_Mentor", "F5_1_DevAss", 
              "F5_2_DevAss", "F5_3_DevAss", "F5_4_DevAss", "F6_1_ProcJust", 
              "F6_2_ProcJust", "F6_3_ProcJust", "F6_4_ProcJust", 
              "F6_5_ProcJust", "F7_1_DistJust", "F7_2_DistJust", 
              "F7_3_DistJust", "F7_4_DistJust", "F7_5_DistJust", "F8_1_Empower",
              "F8_2_Empower", "F8_3_Empower", "F8_4_Empower", "F9_1_DevAss", 
              "F9_2_DevAss", "F9_3_DevAss", "F10_1_Turnover", "F10_2_Turnover", 
              "F10_3_Turnover", "F10_4_Turnover", "F11_1_JobSat", 
              "F11_2_JobSat", "F11_3_JobSat", "F11_4_JobSat", "F11_5_JobSat", 
              "F12_1_CareerSat", "F12_2_CareerSat", "F12_3_CareerSat", 
              "F12_4_CareerSat", "F12_5_CareerSat", "F13_1_LifeSat", 
              "F13_2_LifeSat", "F13_3_LifeSat", "F13_4_LifeSat", 
              "F13_5_LifeSat", NA, "F14a_1_CarSucc", "F14a_2_CarSucc", 
              "F14a_3_CarSucc", "F14b_CarSucc", "F14b_CarSuccTXT", NA, 
              "G1_AspST", "G2_AspLT", "G3_AspFactors", NA, NA, "H1_1_OrgFacil",
              "H1_2_OrgFacil", "H1_3_OrgFacil", "H1_4_OrgFacil", 
              "H1_5_OrgFacil", "H1_6_OrgFacil", "H1_7_OrgFacil", 
              "H1_8_OrgFacil", "H1_8_OrgFacilTXT", "H2_OrgDoMore", 
              "H3_OrgDoMoreTXT", "H4_1_Career_cfMen", "H4_2_Career_cfWom", 
              "H5_Recomm", "H6_RecommTXT", "H7_IncOrgPerf", "H8_IncOrgPerfTXT", 
              "H9_AddAnything", "H10_ParticInIv", "H11_GetCopy", "H12_Email", 
              NA, "Lat", "Long", "LocAcc", NA)

# I've specified column types for the Deps columns. Some people used 
# leading zeroes when entering number of dependents, and by default, 
# R treats these as characters. A leading zero in one datafile and not
# the other will result in different character formats, and the 
# bind function (for combining into one df) will fail.

data1 <- read_csv(file.path(folder, datafile1), 
                  col_names = varNames,
                  col_types = cols(StartDate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                   EndDate = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                   Deps0_5 = col_integer(), Deps5_10 = col_integer(), 
                                   Deps11_18 = col_integer(), Deps19_30 = col_integer(), 
                                   DepsOther = col_integer()), 
                  skip = 2)

data2 <- read_csv(file.path(folder, datafile2), 
                  col_names = varNames,
                  col_types = cols(StartDate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                   EndDate = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                   Deps0_5 = col_integer(), Deps5_10 = col_integer(), 
                                   Deps11_18 = col_integer(), Deps19_30 = col_integer(), 
                                   DepsOther = col_integer()), 
                  skip = 2)

data3 <- read_csv(file.path(folder, datafile3))
# data3 is the recoded B4 (Tenure) and B5 (Overseas) variables

# spec(data1) can be used to retrieve the class spec used on import.

# Now combine the datasets (.id parameter identifies which dataset each
# case comes from).

fulldata <- bind_rows(data1, data2, .id = "Source")

# identical(data3$ResponseID, fulldata$ResponseID) # Check that sets of 
# response IDs are same in data3 and fulldata.
fulldata <- full_join(fulldata, data3, by = "ResponseID")

# Used following when trying to find out which variables were 
# different classes in the two datasets. Equalitytest will have 
# FALSE for vars which differ.
#
# class1 <- sapply(data1, class)
# class2 <- sapply(data2, class)
# classasvector <- unlist(class1)
# classasvector2 <- unlist(class2)
# equalitytest <- classasvector == classasvector2

###### DATA CLEANING #######
# Select out D1_7, C5_8, E6_8, H1_8 (No info, relates to 'other' text comment)
# Select out all vars beginning with X (headings etc).
# Prob others can be dropped - ResponseSet etc.

responses <- fulldata %>% 
        select(-starts_with("X")) %>% 
        select(-ends_with("D1_7_Barrier")) %>% 
        select(-ends_with("C5_8_CarAdv")) %>% 
        select(-ends_with("E6_8_GeogMob")) %>% 
        select(-ends_with("H1_8_OrgFacil")) %>% 
        filter(ResponseID != "R_3GvvOBTj4XHfyeM") %>% 
        filter(ResponseID != "R_RKpGHXWPmVQx3Sp") %>% 
        filter(ResponseID != "R_2sbE1CxaFo6uCCG") %>% 
        mutate(TotDeps = Deps0_5 + Deps5_10 + Deps11_18 + Deps19_30 + 
                       DepsOther) %>% 
        mutate(MinsTaken = (interval(StartDate, EndDate)) / dminutes(1))


# Recode weird values in items - editing changes to Qualtrics survey
# means that some scales had gaps (e.g., 1-4, 6 instead of 1-5)
responses$C2_1_WLB <- car::recode(responses$C2_1_WLB,"13=1 ; 14=2 ; 15=3 ; 16=4 ; 17=5; else=NA")
responses$C2_2_WLB <- car::recode(responses$C2_2_WLB,"13=1 ; 14=2 ; 15=3 ; 16=4 ; 17=5; else=NA")
responses$C2_3_WLB <- car::recode(responses$C2_3_WLB,"13=1 ; 14=2 ; 15=3 ; 16=4 ; 17=5; else=NA")
responses$C2_4_WLB <- car::recode(responses$C2_4_WLB,"13=1 ; 14=2 ; 15=3 ; 16=4 ; 17=5; else=NA")
responses$C2_5_WLB <- car::recode(responses$C2_5_WLB,"13=1 ; 14=2 ; 15=3 ; 16=4 ; 17=5; else=NA")
responses$C3_Aspire <- car::recode(responses$C3_Aspire,"1=1 ; 2=2 ; 3=3 ; 4=4 ; 5=5 ; 6=6 ;
                                   8=7 ; 9=8 ; 7=9 ; 10=10 ; else=NA")
responses$C6_PrimCareResp <- car::recode(responses$C6_PrimCareResp, "1=1 ; 8=2 ; 9=3 ;10=4 ; 11=5 ; 12=6 ;
                                   13=7 ; 14=8 ; else=NA")
responses$D1_1_Barrier <- car::recode(responses$D1_1_Barrier,"13=1 ; 18=2 ; 14=3 ; 15=4 ; 16=5; else=NA")
responses$D1_2_Barrier <- car::recode(responses$D1_2_Barrier,"13=1 ; 18=2 ; 14=3 ; 15=4 ; 16=5; else=NA")
responses$D1_5_Barrier <- car::recode(responses$D1_5_Barrier,"13=1 ; 18=2 ; 14=3 ; 15=4 ; 16=5; else=NA")
responses$D1_6_Barrier <- car::recode(responses$D1_6_Barrier,"13=1 ; 18=2 ; 14=3 ; 15=4 ; 16=5; else=NA")
responses$D1_8_Barrier <- car::recode(responses$D1_8_Barrier,"13=1 ; 18=2 ; 14=3 ; 15=4 ; 16=5; else=NA")
responses$E4_1_CarMgt <- car::recode(responses$E4_1_CarMgt,"1=1 ; 2=2 ; 3=3 ; 4=4 ; 6=5; else=NA")
responses$E4_2_CarMgt <- car::recode(responses$E4_2_CarMgt,"1=1 ; 2=2 ; 3=3 ; 4=4 ; 6=5; else=NA")
responses$E4_3_CarMgt <- car::recode(responses$E4_3_CarMgt,"1=1 ; 2=2 ; 3=3 ; 4=4 ; 6=5; else=NA")
responses$F14a_1_CarSucc <- car::recode(responses$F14a_1_CarSucc,"1=1 ; 3=2 ; 4=3 ; 5=4 ; 6=5; 7=6; 8=7; else=NA")
responses$F14a_2_CarSucc <- car::recode(responses$F14a_2_CarSucc,"1=1 ; 3=2 ; 4=3 ; 5=4 ; 6=5; 7=6; 8=7; else=NA")
responses$F14a_3_CarSucc <- car::recode(responses$F14a_3_CarSucc,"1=1 ; 3=2 ; 4=3 ; 5=4 ; 6=5; 7=6; 8=7; else=NA")



#### Check missing responses in integer vars ###
numericvars <- responses[sapply(responses,is.numeric)]
# Sum number of missing responses in each row.
Missing <- apply(numericvars, 1, function(x) sum(is.na(x)))
allvars <- bind_cols(responses, as.data.frame(Missing))
# Note - bind_cols binds on position, not a key variable, but should
# be safe here.


# Create a numeric ID to help with creating scales later.
allvars$ID <- c(1:length(allvars$ResponseID))



# Following lines generate df containing only numeric varsfor Nazim; he
# uses PLS which doesn't like character variables:
WCA_Numeric <- allvars[sapply(allvars,is.numeric)]
write.csv(WCA_Numeric, file = "WCA_Numeric.csv")

# Following lines were to add Missing to numericvars for Nazim; he
# uses PLS which doesn't like character variables:
# WCA_Numeric <- bind_cols(numericvars, as.data.frame(Missing))
# write.csv(WCA_Numeric, file = "WCA_Numeric.csv")


###########  CREATING SCALES  #############

# Need to create scales for 5-pt and 7-pt scales separately or else
# recoding won't work.

keys.list_5 <- list(ID=c("ID"),
                  C2_WLB = c("-C2_1_WLB", "C2_2_WLB", "-C2_3_WLB", "-C2_4_WLB", 
                             "-C2_5_WLB"),
                  D1_PersCirc = c("D1_1_Barrier", "D1_2_Barrier", "D1_5_Barrier",
                                 "D1_6_Barrier", "D1_8_Barrier"),
                  D2_Abilities = c("D2_1_Conf", "D2_2_SE", "D2_3_PubSpk", 
                                   "D2_4_IntPers", "D2_5_IQ", "D2_6_PersEffic", 
                                   "D2_7_PersEffect", "D2_8_Tech"),
                  E1_CultFit = c("E1_1_CultFit", "E1_2_CultFit", "E1_3_CultFit",
                                 "E1_4_CultFit","E1_5_CultFit", "E1_6_CultFit",
                                 "E1_7_CultFit"),
                  E2_Network = c("E2_1_Network", "E2_2_Network"),
                  E3_Mentor = c("E3_1_Mentor", "E3_2_Mentor", "E3_3_Mentor",
                                "E3_4_Mentor"),
                  E4_CarMgt = c("E4_1_CarMgt", "E4_2_CarMgt", "E4_3_CarMgt"),
                  E5_DevAsst = c("E5_1_DevAsst", "E5_2_DevAsst", "E5_3_DevAsst",
                                 "E5_4_DevAsst", "E5_5_DevAsst", "E5_6_DevAsst",
                                 "E5_7_DevAsst"),
                  E6_GeoMob = c("E6_1_GeogMob", "E6_2_GeogMob", "E6_3_GeogMob"),
                  F2_MngCar = c("F2_1_OwnCareer", "F2_2_OwnCareer", 
                                "F2_3_OwnCareer", "F2_4_OwnCareer"),
                  F3_Relships = c("F3_1_Relships", "F3_2_Relships", 
                                  "F3_3_Relships", "F3_4_Relships"),
                  F4_Mentor = c("F4_1_Mentor", "F4_2_Mentor", "F4_3_Mentor", 
                                "F4_4_Mentor", "F4_5_Mentor", "F4_6_Mentor", 
                                "F4_7_Mentor", "F4_8_Mentor"),
                  F5_DevAss = c("F5_1_DevAss", "F5_2_DevAss", "F5_3_DevAss", 
                                "F5_4_DevAss"),
                  F6_ProcJust = c("F6_1_ProcJust", "-F6_2_ProcJust", 
                                  "-F6_3_ProcJust", "F6_4_ProcJust", 
                                  "F6_5_ProcJust"),
                  F7_DistJust = c("F7_1_DistJust", "F7_2_DistJust", "F7_3_DistJust", 
                                  "F7_4_DistJust", "F7_5_DistJust"),
                  F8_Empower = c("F8_1_Empower", "F8_2_Empower", "F8_3_Empower", 
                                 "F8_4_Empower"),
                  F9_DevAss = c("F9_1_DevAss", "F9_2_DevAss", "F9_3_DevAss"),
                  F10_Turnover = c("F10_1_Turnover", "F10_2_Turnover", 
                                   "F10_3_Turnover", "F10_4_Turnover"),
                  F11_JobSat = c("F11_1_JobSat", "F11_2_JobSat", "-F11_3_JobSat", 
                                 "F11_4_JobSat", "-F11_5_JobSat"),
                  F12_CareerSat =c("F12_1_CareerSat", "F12_2_CareerSat", 
                                   "F12_3_CareerSat", "F12_4_CareerSat", 
                                   "F12_5_CareerSat"),
                  F13_LifeSat = c("F13_1_LifeSat", "F13_2_LifeSat", "F13_3_LifeSat", 
                                  "F13_4_LifeSat", "F13_5_LifeSat"),
                  F14a_CarSucc = c("F14a_1_CarSucc", "F14a_2_CarSucc", "F14a_3_CarSucc"),
                  H1_OrgFacil =c("H1_1_OrgFacil", "H1_2_OrgFacil", "H1_3_OrgFacil", 
                                 "H1_4_OrgFacil", "H1_5_OrgFacil", "H1_6_OrgFacil", 
                                 "H1_7_OrgFacil")
                  )
itemsused_5 <- c("ID", "C2_1_WLB", "C2_2_WLB", "C2_3_WLB", "C2_4_WLB", "C2_5_WLB",
                 "D1_1_Barrier", "D1_2_Barrier", "D1_5_Barrier", 
                 "D1_6_Barrier", "D1_8_Barrier",
                 "D2_1_Conf", "D2_2_SE", "D2_3_PubSpk",
                 "D2_4_IntPers", "D2_5_IQ", "D2_6_PersEffic",
                 "D2_7_PersEffect", "D2_8_Tech",
                 "E1_1_CultFit", "E1_2_CultFit", "E1_3_CultFit", "E1_4_CultFit",
                 "E1_5_CultFit", "E1_6_CultFit", "E1_7_CultFit", "E2_1_Network", 
                 "E2_2_Network", "E3_1_Mentor", "E3_2_Mentor", "E3_3_Mentor", 
                 "E3_4_Mentor", "E4_1_CarMgt", "E4_2_CarMgt", "E4_3_CarMgt", 
                 "E5_1_DevAsst", "E5_2_DevAsst", "E5_3_DevAsst", "E5_4_DevAsst", 
                 "E5_5_DevAsst", "E5_6_DevAsst", "E5_7_DevAsst", 
                 "E6_1_GeogMob", "E6_2_GeogMob", "E6_3_GeogMob", 
                 "F2_1_OwnCareer", "F2_2_OwnCareer", "F2_3_OwnCareer", 
                 "F2_4_OwnCareer", "F3_1_Relships", "F3_2_Relships", 
                 "F3_3_Relships", "F3_4_Relships", "F4_1_Mentor", "F4_2_Mentor", 
                 "F4_3_Mentor", "F4_4_Mentor", "F4_5_Mentor", "F4_6_Mentor",
                 "F4_7_Mentor", "F4_8_Mentor", "F5_1_DevAss", "F5_2_DevAss", 
                 "F5_3_DevAss", "F5_4_DevAss", "F6_1_ProcJust", "F6_2_ProcJust", 
                 "F6_3_ProcJust", "F6_4_ProcJust", "F6_5_ProcJust", "F7_1_DistJust", 
                 "F7_2_DistJust", "F7_3_DistJust", "F7_4_DistJust", "F7_5_DistJust",
                 "F8_1_Empower", "F8_2_Empower", "F8_3_Empower", "F8_4_Empower",
                 "F9_1_DevAss", "F9_2_DevAss", "F9_3_DevAss",
                 "F10_1_Turnover", "F10_2_Turnover", "F10_3_Turnover", "F10_4_Turnover",
                 "F11_1_JobSat", "F11_2_JobSat", "F11_3_JobSat", "F11_4_JobSat", 
                 "F11_5_JobSat", "F12_1_CareerSat", "F12_2_CareerSat", 
                 "F12_3_CareerSat", "F12_4_CareerSat", "F12_5_CareerSat",
                 "F13_1_LifeSat", "F13_2_LifeSat", "F13_3_LifeSat", "F13_4_LifeSat", 
                 "F13_5_LifeSat", "F14a_1_CarSucc", "F14a_2_CarSucc", "F14a_3_CarSucc",
                 "H1_1_OrgFacil", "H1_2_OrgFacil", "H1_3_OrgFacil", "H1_4_OrgFacil", 
                 "H1_5_OrgFacil", "H1_6_OrgFacil", "H1_7_OrgFacil"
                 )
subset_allvars_5 <- allvars %>% dplyr::select(one_of(itemsused_5))
scores_5 <- scoreItems(keys.list_5, subset_allvars_5, impute="none", min=1, max=5, digits = 2)
# add the new scale scores to allvars using dplyr::left_join, matching by ID.
allvars <- dplyr::left_join(allvars, as.data.frame(scores_5$scores), by = "ID")

keys.list_7 <- list(ID=c("ID"), 
                    D3_CareerSE = c("D3_1_Decn", "D3_2_Grow", "D3_4_Chall"))
itemsused_7 <- c("ID", "D3_1_Decn", "D3_2_Grow", "D3_4_Chall")
subset_allvars_7 <- allvars %>% dplyr::select(one_of(itemsused_7))
scores_7 <- scoreItems(keys.list_7, subset_allvars_7, impute="none", min=1, max=5, digits = 2)
# add the new scale scores to allvars using dplyr::left_join, matching by ID.
allvars <- dplyr::left_join(allvars, as.data.frame(scores_7$scores), by = "ID")

############ Print off reliabilities etc. ####################
#
#      Can comment out this code when not required           #

# sink("Scale psychometrics.txt") # Send output to txt file in working directory
# scores_7$alpha
# scores_7$n.items
# scores_7$corrected
# scores_5$alpha
# scores_5$n.items
# scores_5$corrected
# sink()


### Create factor variables from ethnic, marital, etc ###
allvars$Marital_f <- as.factor(allvars$Marital)
levels(allvars$Marital_f) <- c("Married", "Single", "Other")
# A1 Marital status?
#  Married/ Partnered (1)
#  Single (2)
#  Other (3)


allvars$Ethnic_f <- as.factor(allvars$Ethnic)
levels(allvars$Ethnic_f) <- c("NZ European", "Maori", "Asian", "Pacific", "ME.LA.A", "Other")
# B3 Ethnic origin (main)?
#  (NZ) European (1)
#  Maori (2)
#  Asian (3)
#  Pacific peoples (4)
#  Middle Eastern/ Latin American/African (5)
#  Other (please specify) (6) ____________________

allvars$JobStatus_f <- as.factor(allvars$JobStatus)
levels(allvars$JobStatus_f) <- c("Permanent", "Temp.Contract", "Self-employed", "Other")
# A4 Job status?
#  Permanent (1)
#  Temporary/short contract (2)
#  Self-employed (3)
#  Other (please specify) (4) ____________________

allvars$Seniority_f <- as.factor(allvars$Seniority)
levels(allvars$Seniority_f) <- c("Director", "ExecMgt", "SnrMgt", "MidMgt", "1stLvlMgt", "Super", "Other", "None")
# B2 Seniority?
#  Directorship/board member (1)
#  Executive management (2)
#  Senior management (3)
#  Middle management (4)
#  First-level management (5)
#  Supervisory level (6)
#  Other (please specify) (7) ____________________
#  No management/supervision responsibility (8)

allvars$Age_f <- as.factor(allvars$Age)
levels(allvars$Age_f) <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")
# A2 What is your age group?
#  15 - 24 years (1)
#  25 - 34 years (2)
#  35 - 44 years (3)
#  45 - 54 years (4)
#  55 - 64 years (5)
#  65 years and over (6)

allvars$Educ_f <- as.factor(allvars$Educ)
levels(allvars$Educ_f) <- c("PhD", "Masters", "PG Dip", "Bachelor", "Dip/Cert", "High School", "Other")
# B6 What is your highest education qualification?
#  PhD (1)
#  Master's degree (2)
#  Postgraduate diploma/ certificate (3)
#  Bachelor's degree (4)
#  Diploma/certificate (5)
#  High school qualification(s) (6)
#  Other (please specify) (7) ____________________



allvars$C6_PrimCareResp_f <- as.factor(allvars$C6_PrimCareResp)
levels(allvars$C6_PrimCareResp_f) <- c("Spouse", "Other Family", "Paid Care", "Friends", "Other", "It Depends", "Equal Share", "Me")
# C6 Who is primarily responsible for caring roles? (please select one)
#  Spouse/partner (1)
#  Other family member (8)
#  Paid care/professional (e.g. childminder, nurse) (9)
#  Friend(s) (10)
#  Other (please specify) (11) ____________________
#  It depends (please comment further) (12) ____________________
#  Roles/tasks are shared equally (13)
#  Me (14)


allvars$C7_PrimRespHHold_f <- as.factor(allvars$C7_PrimRespHHold)
# Note: noone answered '4' so leave out 'Friends' level
levels(allvars$C7_PrimRespHHold_f) <- c("Spouse", "Other Family", "Paid Care", "Other", "It Depends", "Equal Share", "Me")
# C7 Who is primarily responsible for household tasks/chores? (please tick one)
#  Spouse/partner (1)
#  Other family member (2)
#  Paid care/professional (e.g. cleaner) (3)
#  Friend(s) (4)
#  Other (please specify) (5) ____________________
#  It depends (please comment further) (6) ____________________
#  Roles/tasks are shared equally (7)
#  Me (8)





allvars$TotSal_f <- as.factor(allvars$TotSal)
levels(allvars$TotSal_f) <- c("Under $30k", "$30k-$50k", "$50k-$80k", "$80k-$100k", "$100k-$250k", "$250k-$500k", "Over $500k")
# B7 What is your total salary (including all benefits)?
#  Under $30,000 (1)
#  $30,001 - $50,000 (2)
#  $50,001 - $80,000 (3)
#  $80,001 to $100,000 (4)
#  $100,001 to $250,000 (5)
#  $250,001 to $500,000 (6)
#  Over $500,000 (7)
# TotSal


# Following code is playing around wiht different forms of graphs

ggplot(allvars, aes(Missing)) + geom_histogram(binwidth = 1)

boxplot(allvars$C1_1_ImpJob,allvars$C1_2_ImpFam, allvars$C1_3_ImpLife, 
        names = c("Job", "Family", "Life"))


ggplot(allvars, aes(Marital, C1_1_ImpJob)) + 
        geom_boxplot(aes(group = factor(Marital)))

hist(allvars$E1_1_CultFit, col = "blue")

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(allvars, Marital == 1)$F14a_1_CarSucc, col = "green")
hist(subset(allvars, Marital == 2)$F14a_1_CarSucc, col = "green")

ggplot(allvars, aes(Marital, allvars$F11_JobSat)) + geom_boxplot(aes(group = factor(Marital)))
ggplot(allvars, aes(Marital, allvars$F13_LifeSat)) + geom_boxplot(aes(group = factor(Marital)))
ggplot(allvars, aes(Marital, allvars$F12_CareerSat)) + geom_boxplot(aes(group = factor(Marital)))
ggplot(allvars, aes(allvars$E1_CultFit, F12_CareerSat, colour = Marital)) +
        geom_point() + geom_smooth()

WLB_items <- dplyr::select(allvars, one_of(c("C2_1_WLB", "C2_2_WLB", "C2_3_WLB", "C2_4_WLB", "C2_5_WLB")))
corr.test(WLB_items, adjust = "none")


boxplot(allvars$C1_1_ImpJob,allvars$C1_2_ImpFam, allvars$C1_3_ImpLife, notch = F, 
        names = c("Job", "Family", "Life"))

ggplot(allvars, aes(Marital, Seniority)) + geom_boxplot(aes(group = factor(Marital)), notch = F)
ggplot(allvars, aes(Ethnic, Seniority)) + geom_boxplot(aes(group = factor(Ethnic)), notch = F)

ggplot(allvars, aes(Marital, C1_3_ImpLife)) + 
        geom_boxplot(aes(group = factor(Marital)), notch = F) + 
        scale_x_discrete(breaks=c("1", "2", "3"), labels=c("Married", "Single", "Other"))

tover <- allvars %>% select(one_of(c("E1_CultFit", "E2_Network", "E3_Mentor" , 
                                     "E4_CarMgt", "E5_DevAsst", "E6_GeoMob", "F10_Turnover")))
cout <- corr.test(tover)

tover2 <- allvars %>% select(one_of(c("F2_MngCar", "F3_Relships", "F4_Mentor" , 
                                      "F5_DevAss", "F6_ProcJust", "F7_DistJust", 
                                      "F8_Empower", "F9_DevAss", "F10_Turnover")))
cout2 <- corr.test(tover2)
kable(cout2$r, digits=2)
kable(cout2$p, digits=3)

table(allvars$NZRegion)
table(allvars$AKRegion)

plot(allvars$F10_Turnover, allvars$F9_DevAss)
ggplot(allvars, aes(F9_DevAss, F10_Turnover)) + geom_point() + geom_smooth()

allvars %>% filter(Marital %in% c(1, 2)) %>% 
        ggplot(aes(C1_1_ImpJob, fill = factor(TotDeps))) + 
        geom_histogram(binwidth = 10) +
        facet_wrap(~Marital_f, ncol = 1) +
        xlab("Importance of Job for Married and Single")

allvars %>% filter(Marital %in% c(1, 2)) %>% 
        ggplot(aes(C1_2_ImpFam, fill = factor(TotDeps))) + 
        geom_histogram(binwidth = 10) +
        facet_wrap(~Marital_f, ncol = 1) +
        xlab("Importance of Family for Married and Single")

allvars %>% filter(Marital %in% c(1, 2)) %>% 
        ggplot(aes(C1_3_ImpLife, fill = factor(TotDeps))) + 
        geom_histogram(binwidth = 10) +
        facet_wrap(~Marital_f, ncol = 1) +
        xlab("Importance of Life/Leisure for Married and Single")

allvars %>% filter(Marital_f %in% c("Married", "Single")) %>% 
        ggplot(aes(Seniority, group = Seniority_f)) + 
        geom_histogram(binwidth = 1) +
        facet_wrap(~Marital_f, ncol = 1) +
        xlab("Seniority")

ggplot(allvars, aes(F10_Turnover, fill = Seniority_f)) + 
        geom_histogram(binwidth = 1) +
        xlab("Turnover Intention")

## Following two graphs use geom_text to label the bars with counts.
## Could also do percent.

allvars %>% filter(!is.na(Age_f)) %>% 
        ggplot(aes(Age_f)) +
        geom_bar(stat = "count", fill = "steelblue") +
        xlab("Age Distribution") +
        ylab("Frequency") +
        geom_text(aes(label = ..count..), stat = "count", vjust=1.6, color="white", size=3.5)
        

allvars %>% filter(!is.na(Age_f)) %>% 
        ggplot(aes(Age_f)) +
        geom_bar(stat = "count", fill = "steelblue") +
        xlab("Age Distribution") +
        ylab("Frequency") +
        geom_text(aes(label = ..count..), stat= "count", vjust = -.5, size=5)

allvars %>% filter(!is.na(Ethnic_f)) %>% 
        ggplot(aes(Ethnic_f)) +
        geom_bar(stat = "count", fill = "steelblue") +
        xlab("Ethnicity") +
        ylab("Frequency") +
        geom_text(aes(label = ..count..), stat= "count", vjust = -.5, size=5)


# Create tables of percentages of demographics

MaritalSum <- allvars %>% filter(!is.na(Marital_f)) %>%
        group_by(Marital_f) %>% 
        summarise(n(), pct = round(100*(n()/sum(!is.na(allvars$Marital_f))),1))
knitr::kable(MaritalSum, col.names = c("Marital Status", "N", "Pct"))

EthnicSum <- allvars %>% filter(!is.na(Ethnic_f)) %>%
        group_by(Ethnic_f) %>% 
        summarise(n(), pct = round(100*(n()/sum(!is.na(allvars$Ethnic_f))),1))
knitr::kable(EthnicSum, col.names = c("Ethnicity", "N", "Pct"))

JobStatusSum <- allvars %>% filter(!is.na(JobStatus_f)) %>%
        group_by(JobStatus_f) %>% 
        summarise(n(), pct = round(100*(n()/sum(!is.na(allvars$JobStatus_f))),1))
knitr::kable(JobStatusSum, col.names = c("Job Status", "N", "Pct"))

SenioritySum <- allvars %>% filter(!is.na(Seniority_f)) %>%
        group_by(Seniority_f) %>% 
        summarise(n(), pct = round(100*(n()/sum(!is.na(allvars$Seniority_f))),1))
knitr::kable(SenioritySum, col.names = c("Job Seniority", "N", "Pct"))

AgeSum <- allvars %>% filter(!is.na(Age_f)) %>%
        group_by(Age_f) %>% 
        summarise(n(), pct = round(100*(n()/sum(!is.na(allvars$Age_f))),1))
knitr::kable(AgeSum, col.names = c("Age Range", "N", "Pct"))

EducSum <- allvars %>% filter(!is.na(Educ_f)) %>%
        group_by(Educ_f) %>% 
        summarise(n(), pct = round(100*(n()/sum(!is.na(allvars$Educ_f))),1))
knitr::kable(EducSum, col.names = c("Education", "N", "Pct"))

C6_PrimCareRespSum <- allvars %>% filter(!is.na(C6_PrimCareResp_f)) %>%
        group_by(C6_PrimCareResp_f) %>% 
        summarise(n(), pct = round(100*(n()/sum(!is.na(allvars$C6_PrimCareResp_f))),1))
knitr::kable(C6_PrimCareRespSum, col.names = c("Primary Carer", "N", "Pct"))

C7_PrimRespHHoldSum <- allvars %>% filter(!is.na(C7_PrimRespHHold_f)) %>%
        group_by(C7_PrimRespHHold_f) %>% 
        summarise(n(), pct = round(100*(n()/sum(!is.na(allvars$C7_PrimRespHHold_f))),1))
knitr::kable(C7_PrimRespHHoldSum, col.names = c("Primary Household", "N", "Pct"))

TotSalSum <- allvars %>% filter(!is.na(TotSal_f)) %>%
        group_by(TotSal_f) %>% 
        summarise(n(), pct = round(100*(n()/sum(!is.na(allvars$TotSal_f))),1))
knitr::kable(TotSalSum, col.names = c("Salary Range", "N", "Pct"))


# Work out distribution of response times

ShortTime <- allvars %>% 
        select(ResponseID, Source, Missing, MinsTaken) %>% 
        arrange(MinsTaken) %>% 
        filter(MinsTaken <= 30)

ggplot(ShortTime, aes(MinsTaken)) +
        geom_histogram()


fast <- ShortTime$ResponseID
testx <- allvars %>% filter(ResponseID %in% fast)
mean(testx$Age, na.rm = TRUE)
mean(allvars$Age, na.rm = TRUE)

glimpse(allvars %>% filter(ResponseID %in% fast))

ggplot(ShortTime, aes(MinsTaken, Missing)) +
        geom_point() +
        geom_smooth(method=lm)

## Correlations and Regressions done for Nazim (email 30 Nov)

sink(file = "Corrs_Reg_for_Nazim.txt")
# Correlations for Q4
tocorver1 <- allvars %>% select(one_of(c("F1_1_TrackRcd", "F2_MngCar", "F3_Relships", "F4_Mentor", "F5_DevAss", "F10_Turnover", "F11_JobSat", "F12_CareerSat", "F13_LifeSat")))
corout1 <- corr.test(tocorver1)
cat("\n\n")
print(corout1)


tocorver2 <- allvars %>% select(one_of(c("F6_ProcJust", "F7_DistJust", "F8_Empower", "F9_DevAss", "F10_Turnover", "F11_JobSat", "F12_CareerSat", "F13_LifeSat")))
corout2 <- corr.test(tocorver2)
print(corout2)


# Regressions for Q4 - DO THEY INCLUDE VIF, CHARTS AND MEAN?
nreg41 <- lm(F10_Turnover ~ F1_1_TrackRcd + F2_MngCar + F3_Relships + F4_Mentor + F5_DevAss, allvars)
nreg42 <- lm(F11_JobSat ~ F1_1_TrackRcd + F2_MngCar + F3_Relships + F4_Mentor + F5_DevAss, allvars)
nreg43 <- lm(F12_CareerSat ~ F1_1_TrackRcd + F2_MngCar + F3_Relships + F4_Mentor + F5_DevAss, allvars)
nreg44 <- lm(F13_LifeSat ~ F1_1_TrackRcd + F2_MngCar + F3_Relships + F4_Mentor + F5_DevAss, allvars)

nreg45 <- lm(F10_Turnover ~ F6_ProcJust + F7_DistJust + F8_Empower + F9_DevAss, allvars)
nreg46 <- lm(F11_JobSat ~ F6_ProcJust + F7_DistJust + F8_Empower + F9_DevAss, allvars)
nreg47 <- lm(F12_CareerSat ~ F6_ProcJust + F7_DistJust + F8_Empower + F9_DevAss, allvars)
nreg48 <- lm(F13_LifeSat ~ F6_ProcJust + F7_DistJust + F8_Empower + F9_DevAss, allvars)


# Correlations for Q5
tocorver3 <- allvars %>% select(one_of(c("D1_PersCirc", "D2_Abilities", "D3_CareerSE", "F10_Turnover", "F11_JobSat", "F12_CareerSat", "F13_LifeSat")))
corout3 <- corr.test(tocorver3)
print(corout3)


tocorver4 <- allvars %>% select(one_of(c("E1_CultFit", "E2_Network", "E3_Mentor", "E4_CarMgt", "E5_DevAsst", "E6_GeoMob", "F10_Turnover", "F11_JobSat", "F12_CareerSat", "F13_LifeSat")))
corout4 <- corr.test(tocorver4)
print(corout4)

# Regressions for Q5 - DO THEY INCLUDE VIF, CHARTS AND MEAN?
nreg51 <- lm(F10_Turnover ~ D1_PersCirc + D2_Abilities + D3_CareerSE, allvars)
nreg52 <- lm(F11_JobSat ~ D1_PersCirc + D2_Abilities + D3_CareerSE, allvars)
nreg53 <- lm(F12_CareerSat ~ D1_PersCirc + D2_Abilities + D3_CareerSE, allvars)
nreg54 <- lm(F13_LifeSat ~ D1_PersCirc + D2_Abilities + D3_CareerSE, allvars)

nreg55 <- lm(F10_Turnover ~ E1_CultFit + E2_Network + E3_Mentor + E4_CarMgt + E5_DevAsst + E6_GeoMob, allvars)
nreg56 <- lm(F11_JobSat ~ E1_CultFit + E2_Network + E3_Mentor + E4_CarMgt + E5_DevAsst + E6_GeoMob, allvars)
nreg57 <- lm(F12_CareerSat ~ E1_CultFit + E2_Network + E3_Mentor + E4_CarMgt + E5_DevAsst + E6_GeoMob, allvars)
nreg58 <- lm(F13_LifeSat ~ E1_CultFit + E2_Network + E3_Mentor + E4_CarMgt + E5_DevAsst + E6_GeoMob, allvars)


# Some more for Fishing
nreg61 <- lm(F10_Turnover ~ F11_JobSat + F12_CareerSat + F13_LifeSat, allvars)
nreg62 <- lm(F10_Turnover ~ F11_JobSat + F12_CareerSat + F13_LifeSat + F14a_CarSucc, allvars)

tocorver5 <- allvars %>% select(one_of(c("F10_Turnover", "F11_JobSat", "F12_CareerSat", "F13_LifeSat", "F14a_CarSucc")))
corout5 <- corr.test(tocorver5)
print(corout5)


list_reg <- list(nreg41, nreg42, nreg43, nreg44, nreg45, nreg46, nreg47, nreg48,
                 nreg51, nreg52, nreg53, nreg54, nreg55, nreg56, nreg57, nreg58,
                 nreg61, nreg62)

for(i in 1:length(list_reg)) {
        print(summary(list_reg[[i]]))
        cat("Following table gives VIF values for each variable:\n")
        print(vif(list_reg[[i]]))
        cat("Following tests if SQRT(vif) > 2:\n")
        print(sqrt(vif(list_reg[[i]])) > 2) # problem?
        cat("\n\n")
}

sink()



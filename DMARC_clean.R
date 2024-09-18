rm(list = ls())

# Package installation 
'
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("iClusterPlus")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("acs")
install.packages("lubridate")
'

library(iClusterPlus)
library(ggplot2)
library(tidyverse)
library(acs)
library(lubridate)


# Reading in data 
fd <- read.csv("data/DMARC_data.csv", stringsAsFactors = FALSE)
# Lower poverty level = lower score closer to 0
# AFN is a unique household identifier 
# "lnm" is last name 


# INITAIL TABLES  ---------------
'
table(fd$annual_income) # good
table(fd$fed_poverty_level) # good 
table(fd$dob) # good
table(fd$location) # good, what does "all" mean?
table(fd$afn) # good
table(fd$zip) # good (the "-" zips are still there but there are 0 of each now)
table(fd$lnm) # good
table(fd$served_date) # good
table(fd$education) # good
table(fd$income_source) # good
table(fd$homeless) # good
table(fd$housing) # good
table(fd$housing_type) # good
table(fd$family_type) # good
table(fd$ethnicity) # good
table(fd$race) # good
table(fd$gender) # good
table(fd$snap_household) # good
table(fd$service_name) # good
'

# CLEANNIG CODE ---------------

# Blank cleaning function 
blankclean <- function(data, columns_to_clean) {
  for (col in columns_to_clean) {
    data[[col]] <- as.factor(ifelse(data[[col]] %in% c("", " ", "Not Selected"), "Unknown", data[[col]]))
  }
  return(data)
}

# Columns to clean
columns_to_clean <- c("afn", "race", "education", "ethnicity", "gender", "zip", "location", 
                              "family_type", "housing_type", "housing", "homeless", 
                              "income_source")

# Clean the specified columns
fd <- blankclean(fd, columns_to_clean)


# Make poverty level and annual income numeric and clean
fd$fed_poverty_level = as.numeric(fd$fed_poverty_level)
fd$annual_income = as.numeric(fd$annual_income)
fd$fed_poverty_level[fd$fed_poverty_level %in% c("-143")] = 143
fd$annual_income[fd$annual_income %in% c("-42339")] = 42339


# ZIP cleaning
# 50047 50047-7767  
# 50111 50111-4924 
# 50266 50266-8181 
# 50325 50325-7781 
fd$zip[fd$zip == "50047-7767"] = "50047"
fd$zip[fd$zip == "50111-4924"] = "50111"
fd$zip[fd$zip == "50266-8181"] = "50266"
fd$zip[fd$zip == "50325-7781"] = "50325"

# Getting rid of blank afn entry 
fd = fd[-677797,]

# Clean lnm
# https://stackoverflow.com/questions/18374986/capitalizing-text-of-a-specific-column-in-rs-data-frame
# https://stackoverflow.com/questions/75084373/how-to-remove-rows-by-condition-in-r
# fd$lnm = as.character(fd$lnm)
# remove non-letters
# fd = fd[!(fd$lnm %in% c("", "-", "0", "1", "2", "3", "4")),]
# capitalize all letters
# fd$lnm = toupper(fd$lnm)
# fd$lnm = as.factor(fd$lnm)

# Household size 
fd <- fd %>%
  group_by(afn, served_date) %>%
  mutate(household_size = length(individual_id))

# Changing outlier (clearly mistakes) high annual incomes
#fd_sorted = fd[order(fd$annual_income, decreasing = TRUE),]

# There are a lot holy cow - individually checking each one - fixing ones that clearly are mis-inputs
#subsett = subset(fd, fd$annual_income >= 400000)
#View(subsett)
#fd_sorted
#weird = subset(fd, fd$afn == "IM-560863") # Gotta change poverty levels after this
#View(weird)

# Any index past 677797 (the row we removed missing an afn) needs to be 1 less
middle = median(fd$annual_income)

fd[c(170309,185032,199601),]$annual_income = 20676 # this changes the income values by index, then rerun weird to check
fd[c(190287,217172,238658,238659,262260,262261,281989,302635),]$annual_income = 16056
fd[c(261604,284776,298389),]$annual_income = 40000
fd[c(261605,284772,298390),]$annual_income = 40000
fd[403491,]$annual_income = 39000 
fd[407195,]$annual_income = 25428
fd[c(509920,509925,509926),]$annual_income = 45600
fd[c(509921,509922,509923,509924),]$annual_income = 45600
fd[c(811392,551099),]$annual_income = 0
fd[c(617799,625746),]$annual_income = 10000
fd[c(859236,1230841),]$annual_income = middle
fd[859237,]$annual_income = 9732
fd[c(881720,883511),]$annual_income = 48000
fd[c(1007696,1007697,1007698,1007699,1007700,1007701,1007702,1007703,1027114,1027115,1027116,1027117,1027118,1027119,1027120,1027121),]$annual_income = 36672
fd[c(1029706,1029707),]$annual_income = 50000
fd[c(1342357,1393535),]$annual_income = 26400
fd[c(1350730,1350733,1350735,1350736,1350737),]$annual_income = 34520
fd[c(128201,128202,128203,128204,197594,197595,197596,197597,364229,364230,364231,364232,407196,407197,407198,407199,498192,498193,498194,498195),]$annual_income = 45000
fd[c(177011,223073,261606,284777,298391),]$annual_income = 8775
fd[c(177010,223070,223071,223072,261601,261602,261603,284773,284774,284775,298386,298387,298388),]$annual_income = 0
fd[c(6862,30210,44088,65386),]$annual_income = 13600
fd[c(15631,31584),]$annual_income = 11500
fd[c(15931,15932,43468,43469,121853,121854),]$annual_income = 26500
fd[c(111404,111405,152918,152919,152920,152921,152922,158833,158834,158835,158836,158837,179208,179209,179210,179211),]$annual_income = 24000
fd[c(111406,152923,158838,179212),]$annual_income = 28760
fd[c(122005,146562,166228),]$annual_income = 12408
fd[c(140584,163259,185082,217526,227067,254132,266435,289039,311143),]$annual_income = 13560
fd[c(1091446,1091447,1126878,1126879,1194845,1194846,1222114,1222115),]$annual_income = 17000
fd[c(156364,177704),]$annual_income = 13700
fd[c(165379,194655,213708),]$annual_income = 23000
fd[c(188547,219886,357095,420470),]$annual_income = 15000
fd[c(189482,215923,277608,312756,338550),]$annual_income = 13500
fd[c(191265,200327,233964,248565),]$annual_income = 15000
fd[c(197722,197723,907999,908000,908001,908002,908003,965685,965686,965687,965688,965689,1230482,1230483,1295948,1295949,1295950,1295951,1295952),]$annual_income = 23000
fd[207116,]$annual_income = 12000
fd[c(213241,231462,286799),]$annual_income = 13344
fd[c(400707,400708,400709,400710,437689,437690,437691,437692,465173,465174,465175,465176,509135,509136,509137),]$annual_income = 30800
fd[c(375930,395449,414987,443775,457564,488425),]$annual_income = 15000
fd[c(538757,551013,602859,613167,625514,639863,649865,653349,655069,656565,680711,691416,709527,748794,755883,764325,778050),]$annual_income = 25000
fd[240183,]$annual_income = 15500
fd[302507,]$annual_income = 12960
fd[331848,]$annual_income = 22880
fd[341094,]$annual_income = 13400
fd[c(393525,398614,494389,494390,518224,709516,1368820,1446747,1393373,1393374,1427280),]$annual_income = middle
fd[c(243032,243033,488634),]$annual_income = 13000
fd[494391,]$annual_income = 24000
fd[498835,]$annual_income = 32805
fd[509138,]$annual_income = 30800
fd[556144,]$annual_income = 10000
fd[c(632096,637735,652583,693209,717603,717604,717605,717606,776496,776497,776498,776499),]$annual_income = 20000
fd[c(653614,735465),]$annual_income = 14688
fd[c(824859,840842,862617,894471,933574,989929,1037269,1059809,1083758,1099363),]$annual_income = 12200
fd[c(886878,886879,895176,895177),]$annual_income = 101000
fd[c(896528,916301,1089948),]$annual_income = 25000
fd[c(1038047,1038048,1038049),]$annual_income = 21000
fd[1079082,]$annual_income = 1919
fd[c(966313,1040418,1056896,1069390,1100828,1122034,1178342),]$annual_income = 25000
fd[1152504,]$annual_income = 65000


# Now, do the same with the corresponding poverty levels
#subsetttt = subset(fd, fed_poverty_level >= 750)
#View(subsetttt)
#weird = subset(fd, fd$afn == "77-5359754")
#View(weird)


# Changing outlier (clearly mistakes) poverty levels
fd[c(170309,185032,199601),]$fed_poverty_level = 172
fd[c(190287,217172,238658,238659,262260,262261,281989,302635),]$fed_poverty_level = 133
fd[c(261604,284776,298389,261605,284772,298390),]$fed_poverty_level = 119
fd[403491,]$fed_poverty_level = 231
fd[407195,]$fed_poverty_level = 87
fd[c(509920,509925,509926,509921,509922,509923,509924),]$fed_poverty_level = 117
fd[c(811392,551099),]$fed_poverty_level = 0
fd[c(617799,625746),]$fed_poverty_level = 83
fd[c(859236,1230841),]$fed_poverty_level = 89
fd[c(881720,883511),]$fed_poverty_level = 373
fd[c(1007696,1007697,1007698,1007699,1007700,1007701,1007702,1007703,1027114,1027115,1027116,1027117,1027118,1027119,1027120,1027121),]$fed_poverty_level = 72
fd[c(1029706,1029707),]$fed_poverty_level = 274
fd[c(1342357,1393535),]$fed_poverty_level = 134
fd[c(1350730,1350733,1350735,1350736,1350737),]$fed_poverty_level = 210
fd[c(128201,128202,128203,128204,197594,197595,197596,197597,364229,364230,364231,364232,407196,407197,407198,407199,498192,498193,498194,498195),]$fed_poverty_level = 183
fd[c(177011,223073,261606,284777,298391),]$fed_poverty_level = 55
fd[c(177010,223070,223071,223072,261601,261602,261603,284773,284774,284775,298386,298387,298388),]$fed_poverty_level = 0
fd[c(6862,30210,44088,65386),]$fed_poverty_level = 113
fd[c(15931,15932,43468,43469,121853,121854),]$fed_poverty_level = 164
fd[c(15631,31584),]$fed_poverty_level = 96
fd[c(111404,111405,152918,152919,152920,152921,152922,158833,158834,158835,158836,158837,179208,179209,179210,179211),]$fed_poverty_level = 82
fd[c(111406,152923,158838,179212),]$fed_poverty_level = 171
fd[c(122005,146562,166228),]$fed_poverty_level = 103
fd[c(140584,163259,185082,217526,227067,254132,266435,289039,311143),]$fed_poverty_level = 112
fd[c(1091446,1091447,1126878,1126879,1194845,1194846,1222114,1222115),]$fed_poverty_level = 46
fd[c(156364,177704),]$fed_poverty_level = 114
fd[c(165379,194655,213708),]$fed_poverty_level = 190
fd[c(188547,219886,357095,420470),]$fed_poverty_level = 74
fd[c(189482,215923,277608,312756,338550),]$fed_poverty_level = 112
fd[c(191265,200327,233964,248565),]$fed_poverty_level = 125
fd[c(197722,197723,907999,908000,908001,908002,908003,965685,965686,965687,965688,965689,1230482,1230483,1295948,1295949,1295950,1295951,1295952),]$fed_poverty_level = 133
fd[207116,]$fed_poverty_level = 99
fd[c(213241,231462,286799),]$fed_poverty_level = 110
fd[c(400707,400708,400709,400710,437689,437690,437691,437692,465173,465174,465175,465176,509135,509136,509137),]$fed_poverty_level = 126
fd[c(375930,395449,414987,443775,457564,488425),]$fed_poverty_level = 121
fd[c(538757,551013,602859,613167,625514,639863,649865,653349,655069,656565,680711,691416,709527,748794,755883,764325,778050),]$fed_poverty_level = 195
fd[240183,]$fed_poverty_level = 125
fd[302507,]$fed_poverty_level = 45
fd[331848,]$fed_poverty_level = 189
fd[341094,]$fed_poverty_level = 108
fd[c(393525,398614,494389,494390,518224,709516,1368820,1446747,1393373,1393374,1427280),]$fed_poverty_level = 89
fd[c(243032,243033,488634),]$fed_poverty_level = 81
fd[494391,]$fed_poverty_level = 74
fd[498835,]$fed_poverty_level = 263
fd[509138,]$fed_poverty_level = 126
fd[556144,]$fed_poverty_level = 81
fd[617793,]$fed_poverty_level = 300
fd[c(632096,637735,652583,693209,717603,717604,717605,717606,776496,776497,776498,776499),]$fed_poverty_level = 92
fd[c(653614,735465),]$fed_poverty_level = 118
fd[c(824859,840842,862617,894471,933574,989929,1037269,1059809,1083758,1099363),]$fed_poverty_level = 90
fd[c(886878,886879,895176,895177),]$fed_poverty_level = 312
fd[c(896528,916301,1089948),]$fed_poverty_level = 154
fd[c(1038047,1038048,1038049),]$fed_poverty_level = 82
fd[1079082,]$fed_poverty_level = 15
fd[c(966313,1040418,1056896,1069390,1100828,1122034,1178342),]$fed_poverty_level = 208
fd[1152504,]$fed_poverty_level = 355


# Update the weird dates before turning them into dates
fd$dob[fd$dob == "0001-01-01"] = "2001-01-01"
fd$dob[fd$dob == "0002-04-08"] = "2002-04-08"
fd$dob[fd$dob == "0003-01-01"] = "2003-01-01"
fd$dob[fd$dob == "0005-08-18"] = "2005-08-18"
fd$dob[fd$dob == "0006-05-06"] = "2006-05-06"
fd$dob[fd$dob == "0009-03-28"] = "2009-03-28"
fd$dob[fd$dob == "0023-10-06"] = "1923-10-06"
fd$dob[fd$dob == "0068-03-18"] = "1968-03-18"
fd$dob[fd$dob == "0075-02-14"] = "1975-02-14"
fd$dob[fd$dob == "0952-11-09"] = "1952-11-09"
fd$dob[fd$dob == "0980-10-22"] = "1980-10-22"
fd$dob[fd$dob == "0981-03-15"] = "1981-03-15"
fd$dob[fd$dob == "0986-02-19"] = "1986-02-19"
fd$dob[fd$dob == "0993-11-19"] = "1993-11-19"
fd$dob[fd$dob == "1014-11-28"] = "2014-11-28"
fd$dob[fd$dob == "1075-05-14"] = "1975-05-14"
fd$dob[fd$dob == "9921-11-25"] = "2021-11-25"
fd$dob[fd$dob == "9871-10-16"] = "1971-10-16"
fd$dob[fd$dob == "9809-03-27"] = "1989-03-27"
fd$dob[fd$dob == "2091-12-17"] = "1991-12-17"

# Change 0000-00-00 and 0012-00-00 dates into something that can parse

# Date 0012-00-00 is a current student - make it 2012
fd$dob[fd$dob == "0012-00-00"] = "2012-01-01"

# Changing 0000-00-00 dates
fd$dob[fd$dob == "0000-00-00"] = "1111-11-11"

fd = subset(fd, dob != "")

# Turn into dates
fd$dob = ymd(fd$dob)

# update served_date as well
fd$served_date = ymd(fd$served_date)

#calculate age and make column
fd$age = as.numeric(difftime(fd$served_date, fd$dob, units = "weeks")) / 52.25
fd$age = floor(fd$age)

# impute median age for big ages
fd$age[fd$age == 909] = median(fd$age)
fd$age[fd$age == 908] = median(fd$age)
fd$age[fd$age == 907] = median(fd$age)
fd$age[fd$age == 906] = median(fd$age)
fd$age[fd$age == 905] = median(fd$age)
fd$age[fd$age == 904] = median(fd$age)
fd$age[fd$age == -4] = median(fd$age)
fd$age[fd$age == -3] = median(fd$age)
fd$age[fd$age == -2] = median(fd$age)
fd$age[fd$age == -1] = median(fd$age)

# maybe try and find number of distinct visits (for each household) to the pantry 
# by using afn
fd = fd %>%
  group_by(afn) %>%
  mutate(household_visits = n_distinct(served_date))

# flagging if someone is a new customer
fd = arrange(fd, afn, served_date)
fd$first_visit = +(!duplicated(fd$afn))

# Make service_name, snap_household, first_visit a factor 
fd$service_name = as.factor(fd$service_name)
fd$snap_household = as.factor(fd$snap_household)
fd$first_visit = as.factor(fd$first_visit)

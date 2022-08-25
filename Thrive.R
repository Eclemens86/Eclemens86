install.packages("stringr")
library("stringr")
thrive = read.csv(file.choose(), header = T)
thriveWITHDEP = read.csv(file.choose(), header = T)
thriveWITHDEPnew = subset(thriveWITHDEP, NMR.Number > 9496)
# 6527 is last 2016 date
# 6526 is first 2017 date
# 15357 is the first date 
# This is a subset of everything starting from January 1, 2017
thriveWITHDEPnew[is.na(thriveWITHDEPnew) | thriveWITHDEPnew == "Inf"] = NA
attach(thriveWITHDEPnew)

thriveWITHDEPnewCaps = data.frame(lapply(thriveWITHDEPnew, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

attach(ThriveNoVG)
dim(ThriveNoVG)

# GETTING RID OF REDUNDANT NMR TYPES
ThriveNoVG$NMR.Type[ThriveNoVG$NMR.Type == "VENDOR"] <- "SCAR"
ThriveNoVG$NMR.Type[ThriveNoVG$NMR.Type == "METHODS"] <- "PROCESS"
ThriveNoVG$NMR.Type[ThriveNoVG$NMR.Type == "SAFETY"] <- "PROCESS"
ThriveNoVG$NMR.Type[ThriveNoVG$NMR.Type == "ENVIRO."] <- "PROCESS"
ThriveNoVG$NMR.Type[ThriveNoVG$NMR.Type == "PEOPLE"] <- "WORK"
ThriveNoVG$NMR.Type[ThriveNoVG$NMR.Type == "OPERATOR"] <- "WORK"
ThriveNoVG$NMR.Type[ThriveNoVG$NMR.Type == "BLANK"] <- "N/A"


# REMOVING VG and VGE parts (NEW DF)
VGParts = data.frame(dplyr::filter(thriveWITHDEPnewCaps, grepl('VG', Part.Number)))
dim(VGParts) #319
dim(thriveWITHDEPnewCaps) #6525

ThriveNoVG = thriveWITHDEPnewCaps[!(thriveWITHDEPnewCaps$NMR.Number %in% VGParts$NMR.Number),]
dim(ThriveNoVG) #6228
# ThriveNoVG2 = subset(ThriveNoVG, select = -c(X))


## DATA FRAMES
# These are specific instances or parts of the data set I am
# trying to isolate to complete further analysis

freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

thriveNewFlyer = data.frame(dplyr::filter(ThriveNoVG, grepl('NEW FLYER INDUSTRIES', Customer.Name)))
dim(thriveNewFlyer) #706

rev(freqfunc(ThriveNoVG$Customer.Name, 10))
rev(freqfunc(ThriveNoVG$NMR.Type, 4))
rev(freqfunc(ThriveNoVG$Part.Number, 10))
rev(freqfunc(ThriveNoVG$Department, 5))

#TECHNICAL COMPLAINTS
TC = data.frame(dplyr::filter(ThriveNoVG, grepl('TECHNICAL COMPLAINT', Problem.Issue)))
dim(TC) #1145
head(TC)
rev(freqfunc(TC$Customer.Name, 7))

# SCRATCH
Scratch = data.frame(dplyr::filter(ThriveNoVG, grepl('SCRATCH', Problem.Issue)))
dim(Scratch) # 266
head(Scratch)

# BURRS/DROSS
Burr = data.frame(dplyr::filter(ThriveNoVG, grepl('BURR', Problem.Issue)))
Dross = data.frame(dplyr::filter(ThriveNoVG, grepl('DROSS', Problem.Issue)))
BandD = rbind(Burr, Dross)

dim(BandD) # 213
head(BandD)

# LASER
Laser = data.frame(dplyr::filter(ThriveNoVG, grepl('LASER', Problem.Issue)))
dim(Laser) #409
head(Laser)

# SHIPPING
Ship = data.frame(dplyr::filter(ThriveNoVG, grepl('SHIP', Problem.Issue)))
dim(Ship) #331
head(Ship)

# MATERIAL
Material = data.frame(dplyr::filter(ThriveNoVG, grepl('MATERIAL', Problem.Issue)))
dim(Material) #756
head(Material)

# SUPPLIER
Supplier = data.frame(dplyr::filter(ThriveNoVG, grepl('SUPPLIER', Problem.Issue)))
dim(Supplier) #14
head(Supplier)

# RUST
Rust = data.frame(dplyr::filter(ThriveNoVG, grepl('RUST', Problem.Issue)))
dim(Rust) #40

#BENDING
BendParts = data.frame(dplyr::filter(ThriveNoVG, grepl('BEND', Problem.Issue)))
dim(BendParts) #316
BentParts = data.frame(dplyr::filter(ThriveNoVG, grepl('BENT', Problem.Issue)))
dim(BentParts) #210
FullBparts = rbind(BendParts, BentParts)
dim(FullBparts) #526

rev(freqfunc(FullBparts$Customer.Name, 5))
rev(freqfunc(FullBparts$Part.Description, 5))
rev(freqfunc(FullBparts$NMR.Type, 5))

#PANTING
PaintParts = data.frame(dplyr::filter(ThriveNoVG, grepl('PAINT', Problem.Issue)))
dim(PaintParts) #415

rev(freqfunc(PaintParts$Customer.Name, 5))
rev(freqfunc(PaintParts$NMR.Type, 4))

## GRAPHS
paretochart(ThriveNoVG$NMR.Type, main = " Common NMR Types")
paretochart(ThriveNoVG$Department, main = "Common Department Occurances")


## LOOPS
# Here I am parsing through our data to find instances of 
# specific key words being used in the NMR reports

# IDEA 1
keyword = "Bend"
count = 0
for (i in length(Problem.Issue)){
  r = strsplit(Problem.Issue[i], split = " ")
  for (word in r){
    if (keyword == word){
      count = count + 1
    }
  }
}
print(count)
r

tester = c("a", "b", "c", "b","b")
for (xx in length(tester)){
  for (word in tester){
    if (word == "b"){
      print("Yes")
      count = count + 1 
    }
  }
}
print(count)
tester


# IDEA 2
i = 1
while (i < 5){
  r = data.frame(c(strsplit(Problem.Issue[i], split = " ")))
  for (word in r){
    ifelse("Bent" == word, count + 1, next)
    i = i + 1
  }
}
print(count)


# TEST LOOP 
x = c("a","b", "c")
if ("b" %in% x){
  print("Test complete") 
}

## COLINEARITY CHECKS (NOT APLLICABLE DUE TO CATEGORICAL DATA)
# Colinearity is when 2 variables are so closely related, that they 
# actuality become a detriment to the variables ability to predict
# an outcome

## TIME SERIES (NOT APLLICABLE DUE TO CATEGORICAL DATA)
Time = 1:6525 
# This is the number of observations in our data set converted 
# into numeric intervals to be compared over time

## MODLES AND REGRESSION (NOT APLLICABLE DUE TO CATEGORICAL DATA)
# Here, I am trying to find correlation, and also predict certain 
# variables using other variables in our data set
m1 = lm(NMR.Type ~ Customer.Name)
summary(m1)
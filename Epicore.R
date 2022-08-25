epi = read.csv(file.choose(), header = T, encoding = "UTF-8")
head(epi)

epi[is.na(epi) | epi == "Inf"] = NA

epiTLACAPS = data.frame(lapply(epi$TLA.Part, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

attach(epi)
dim(epi)

freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

install.packages("qicharts")
library("qicharts")

install.packages("qicharts2")
library("qicharts2")


install.packages("ggplot2")
install.packages("qcc")

library("ggplot2")
library("qcc")

paretochart(epi$TLA.Part)

rev(freqfunc(epi$TLA.Part, 25))
rev(freqfunc(epi$Asm.Part, 10))

## NOW WE CAN TAKE THE AMOUNT OF NMRS FOR A SPECIFIC PART AND THEN
## DIVIDE THAT BY THE AMOUNT OF JOBS DONE WITH THAT PART. WE CAN
## ALSO DO THIS WITH SPECIFIC QUANTITIES AS WELL

# 6228 total NMRs / 32881 total jobs ran = (roughly) 18.94% NMR frequency

NF1 = data.frame(dplyr::filter(epi, grepl('NF522615', TLA.Part)))
# The NMR frequency for NF522615 was 14/542 meaning 2.58% of jobs ran had an NMR 
sum(NF1$Qty.Comp) #5420 total parts completed 
dim(NF1) # 542 total jobs ran

rev(freqfunc(NF1$Oper, 6)) # 950(542) (950 was the only inspection op ran on this part in every job)
# 14 TOTAL NMRs FOR NF522615
# MOST RECENT DATE AN NMR WAS ENTERED: 9/1/2021 
# FIRST YEAR THE JOB WAS RUN: 2020

NF2 = data.frame(dplyr::filter(epi, grepl('NF567582', TLA.Part)))
# The NMR frequency for NF567582 was 3/361 meaning 0.83% of jobs ran had an NMR 
sum(NF2$Qty.Comp) #5403 total parts completed 

rev(freqfunc(NF2$Oper, 6)) # 950(361)
# 3 TOTAL NMRs FOR NF567582
# MOST RECENT DATE AN NMR WAS ENTERED: 4/8/2019
# FIRST YEAR THE JOB WAS RUN: 2018

NF3 = data.frame(dplyr::filter(epi, grepl('NF568228', TLA.Part))) 
# The NMR frequency for NF568228 was 10/262 meaning 3.82% of jobs ran had an NMR 
sum(NF3$Qty.Comp) #2618 total parts completed 

rev(freqfunc(NF3$Oper, 6)) # 950(262)
# 10 TOTAL NMRs FOR NF568228
# MOST RECENT DATE AN NMR WAS ENTERED: 11/1/2021
# FIRST YEAR THE JOB WAS RUN: 2018

NF4 = data.frame(dplyr::filter(epi, grepl('NF696432', TLA.Part)))
# The NMR frequency for NF696432 was 0/239 meaning 0% of jobs ran had an NMR 
sum(NF4$Qty.Comp) #1750 total parts completed 

rev(freqfunc(NF4$Oper, 6)) # 950(238), 951(1)
# 0 TOTAL NMRs FOR NF696432
# MOST RECENT DATE AN NMR WAS ENTERED: NEVER
# FIRST YEAR THE JOB WAS RUN: 2017 (still running)

NF5 = data.frame(dplyr::filter(epi, grepl('NF522626', TLA.Part)))
# The NMR frequency for NF522626 was 16/234 meaning 6.84% of jobs ran had an NMR 
sum(NF5$Qty.Comp) #2356 total parts completed 

rev(freqfunc(NF5$Oper, 6)) # 950(234)
# 16 TOTAL NMRs FOR PART NF522626
# MOST RECENT DATE AN NMR WAS ENTERED: 3/4/2020
# FIRST YEAR THE JOB WAS RUN: 2018

NF6 = data.frame(dplyr::filter(epi, grepl('NF696428', TLA.Part)))
# The NMR frequency for NF696428 was 0/144 meaning 0% of jobs ran had an NMR 
sum(NF6$Qty.Comp) #718 total parts completed 

rev(freqfunc(NF6$Oper, 6)) # 950(143), 951(1)
# 0 TOTAL NMRs FOR NF696428
# MOST RECENT DATE AN NMR WAS ENTERED: NEVER
# FIRST YEAR THE JOB WAS RUN: 2018 (still running)

AC1 = data.frame(dplyr::filter(epi, grepl('AC257108', TLA.Part)))
# The NMR frequency for AC257108 was 9/301 meaning 2.99% of jobs ran had an NMR 
sum(AC1$Qty.Comp) #1225 total parts completed 

rev(freqfunc(AC1$Oper, 6)) # 950(262), 952(39)
# 9 TOTAL NMRs FOR AC257108
# MOST RECENT DATE AN NMR WAS ENTERED: 4/27/2022
# FIRST YEAR THE JOB WAS RUN: 2020

AC2 = data.frame(dplyr::filter(epi, grepl('AC185801', TLA.Part)))
# The NMR frequency for AC185801 was 0/221 meaning 0% of jobs ran had an NMR 
sum(AC2$Qty.Comp) #3906 total parts completed 

rev(freqfunc(AC2$Oper, 6)) # 949(16), 950(188), 951(12), 952(5)
# 0 TOTAL NMRs FOR AC185801
# MOST RECENT DATE AN NMR WAS ENTERED: NEVER
# FIRST YEAR THE JOB WAS RUN: 2019 (have not ran since 2020)

AC3 = data.frame(dplyr::filter(epi, grepl('AC189281', TLA.Part)))
# The NMR frequency for AC189281 was 2/158 meaning 1.27% of jobs ran had an NMR 
sum(AC3$Qty.Comp) #33181 total parts completed 

rev(freqfunc(AC3$Oper, 6)) # 949(133), 950(8), 951(11), 952(6)
# 2 TOTAL NMRs FOR AC189281
# MOST RECENT DATE AN NMR WAS ENTERED: 3/15/2022
# FIRST YEAR THE JOB WAS RUN: 2020

AC4 = data.frame(dplyr::filter(epi, grepl('AC959000', TLA.Part)))
# The NMR frequency for AC959000 was 3/152 meaning 1.97% of jobs ran had an NMR 
sum(AC4$Qty.Comp) #1511 total parts completed 

rev(freqfunc(AC4$Oper, 6)) # 950(151), 951(1)
# 3 TOTAL NMRs FOR AC959000
# MOST RECENT DATE AN NMR WAS ENTERED: 4/27/2022
# FIRST YEAR THE JOB WAS RUN: 2020




NF7 = data.frame(dplyr::filter(epi, grepl('NF572978', TLA.Part)))
# The NMR frequency for NF572978 was 0/130 meaning 0% of jobs ran had an NMR 
sum(NF7$Qty.Comp) #759 total parts completed 

rev(freqfunc(NF7$Oper, 6)) # 950(130)
# 0 TOTAL NMRs FOR NF572978
# MOST RECENT DATE AN NMR WAS ENTERED: NEVER
# FIRST YEAR THE JOB WAS RUN: 2018 (still running)

NF8 = data.frame(dplyr::filter(epi, grepl('NF793873', TLA.Part)))
# The NMR frequency for NF793873 was 3/128 meaning 2.34% of jobs ran had an NMR 
sum(NF8$Qty.Comp) #714 total parts completed 

rev(freqfunc(NF8$Oper, 6)) # 950(126), 951(2)
# 3 TOTAL NMRs FOR NF793873
# MOST RECENT DATE AN NMR WAS ENTERED: 1/7/2022
# FIRST YEAR THE JOB WAS RUN: 2019

SI1 = data.frame(dplyr::filter(epi, grepl('SI120804', TLA.Part)))
# The NMR frequency for SI120804 was 2/115 meaning 1.74% of jobs ran had an NMR 
sum(SI1$Qty.Comp) #34980 total parts completed 

rev(freqfunc(SI1$Oper, 6)) # 950(115)
# 2 TOTAL NMRs FOR SI120804
# MOST RECENT DATE AN NMR WAS ENTERED: 6/11/2021
# FIRST YEAR THE JOB WAS RUN: 2017

AC5 = data.frame(dplyr::filter(epi, grepl('AC405081', TLA.Part)))
# The NMR frequency for AC405081 was 12/101 meaning 11.88% of jobs ran had an NMR 
sum(AC5$Qty.Comp) #6730 total parts completed 

rev(freqfunc(AC5$Oper, 6)) # 949(74), 950(9), 951(17), 952(1)
# 12 TOTAL NMRs FOR AC405081
# MOST RECENT DATE AN NMR WAS ENTERED: 5/16/2022
# FIRST YEAR THE JOB WAS RUN: 2021

OS1 = data.frame(dplyr::filter(epi, grepl('OS0031', TLA.Part)))
# The NMR frequency for OS0031 was 5/91 meaning 5.49% of jobs ran had an NMR 
sum(OS1$Qty.Comp) #7919 total parts completed 

rev(freqfunc(OS1$Oper, 6)) # 950(23), 951(4), 952(1), 954(63)
# 5 TOTAL NMRs FOR OS0031
# MOST RECENT DATE AN NMR WAS ENTERED: 1/19/2021
# FIRST YEAR THE JOB WAS RUN: 2020







AC6 = data.frame(dplyr::filter(epi, grepl('AC627915', TLA.Part)))
AC6 = AC6[-c(61,62,63,64,65,66,67,68,69,70,71),]
# The NMR frequency for AC627915 was 8/102 meaning 7.84% of jobs ran had an NMR 
sum(AC6$Qty.Comp) #294 total parts completed 

rev(freqfunc(AC6$Oper, 6)) # 950(96), 952(6)
# 8 TOTAL NMRs FOR AC627915
# MOST RECENT DATE AN NMR WAS ENTERED: 2/10/2020
# FIRST YEAR THE JOB WAS RUN: 2019

AC7 = data.frame(dplyr::filter(epi, grepl('AC203950', TLA.Part)))
# The NMR frequency for AC203950 was 1/90 meaning 1.10% of jobs ran had an NMR 
sum(AC7$Qty.Comp) #1175 total parts completed 

rev(freqfunc(AC7$Oper, 6)) # 949(1), 950(41), 952(49)
# 1 TOTAL NMR FOR AC203950
# MOST RECENT DATE AN NMR WAS ENTERED: 11/11/2019
# FIRST YEAR THE JOB WAS RUN: 2019

AC8 = data.frame(dplyr::filter(epi, grepl('AC257107', TLA.Part)))
# The NMR frequency for AC257107 was 0/87 meaning 0% of jobs ran had an NMR 
sum(AC8$Qty.Comp) #394 total parts completed 

rev(freqfunc(AC8$Oper, 6)) # 951(87)
# 0 TOTAL NMRs FOR AC257107
# MOST RECENT DATE AN NMR WAS ENTERED: NEVER
# FIRST YEAR THE JOB WAS RUN: 2019

BXB1 = data.frame(dplyr::filter(epi, grepl('BXB51121', TLA.Part)))
# The NMR frequency for BXB51121 was 2/84 meaning 2.38% of jobs ran had an NMR 
sum(BXB1$Qty.Comp) #1008 total parts completed 

rev(freqfunc(BXB1$Oper, 6)) # 950(56), 951(28)
# 2 TOTAL NMRs FOR BXB51121
# MOST RECENT DATE AN NMR WAS ENTERED: 9/7/2021
# FIRST YEAR THE JOB WAS RUN: 2021

AC9 = data.frame(dplyr::filter(epi, grepl('AC627905', TLA.Part)))
# The NMR frequency for AC627905 was 3/81 meaning 3.70% of jobs ran had an NMR 
sum(AC9$Qty.Comp) #243 total parts completed 

rev(freqfunc(AC9$Oper, 6)) # 950(79), 952(2)
# 3 TOTAL NMRs FOR AC627905
# MOST RECENT DATE AN NMR WAS ENTERED: 2/19/2021
# FIRST YEAR THE JOB WAS RUN: 2020


EL1 = data.frame(dplyr::filter(epi, grepl('EL873601', TLA.Part)))
# The NMR frequency for EL873601was 3/81 meaning 3.70% of jobs ran had an NMR 
sum(EL1$Qty.Comp) #17435 total parts completed 

rev(freqfunc(EL1$Oper, 6)) # 950(79), 952(2)
# 3 TOTAL NMRs FOR EL873601 
# MOST RECENT DATE AN NMR WAS ENTERED: 2/19/2021
# FIRST YEAR THE JOB WAS RUN: 2020


EL2 = data.frame(dplyr::filter(epi, grepl('EL873604', TLA.Part)))
# The NMR frequency for EL873604 was 3/81 meaning 3.70% of jobs ran had an NMR 
sum(EL2$Qty.Comp) #243 total parts completed 

rev(freqfunc(EL2$Oper, 6)) # 950(79), 952(2)
# 3 TOTAL NMRs FOR EL873604 
# MOST RECENT DATE AN NMR WAS ENTERED: 2/19/2021
# FIRST YEAR THE JOB WAS RUN: 2020





## TEMPLATE
X = data.frame(dplyr::filter(epi, grepl('X', TLA.Part)))
# The NMR frequency for X was 0/0 meaning 0% of jobs ran had an NMR 

rev(freqfunc(NF5$Oper, 6)) 
# X was the only inspection type ran on this part out of its X jobs
# X TOTAL NMRs FOR PART X
# MOST RECENT DATE AN NMR WAS ENTERED WAS: XX/XX/XXXX

Top10 = rbind(NF1, NF2, NF3, NF4, AC1, NF5, NF6, AC2, AC3, AC4) 
paretochart(Top10$TLA.Part, title = "Pareto Chart of Top 10 TLA Part #'s")


Top15 = rbind(NF1, NF2, NF3, NF4, AC1, NF5, NF6, AC2, AC3, AC4, NF7, NF8, SI1, AC5, OS1) 
paretochart(Top15$TLA.Part, title = "Pareto Chart of Top 15 TLA Part #'s")


Top20 = rbind(NF1, NF2, NF3, NF4, AC1, NF5, NF6, AC2, AC3, AC4, NF7, NF8, SI1, AC5, OS1, AC6, AC7, AC8, BXB1, AC9) 
paretochart(Top20$TLA.Part, title = "Pareto Chart of Top 20 TLA Part #'s", xlab = "Part Number", ylab = "Job Frequency")

counts = c(33181, 5420, 5403, 3906, 2618, 2356, 1750, 1511, 1225, 718)
names = c("AC189281", "NF522615", "NF567582", "AC185801", "NF568228", "NF522626", "NF696432", 
          "AC959000", "AC257108", "NF696428")
b = barplot(counts, names.arg = names, xlab = "Part Number", ylab = "Quantity", col = "blue", main = "Total Part Quantity")

frame1 = data.frame(counts, names)
plot1 = ggplot(frame1, aes(names, counts)) + geom_bar(stat = "identity")+
geom_text(aes(label = signif(counts)), nudge_y = 1000)
plot1

x = sum(epi$TLA.Part == epi$Asm.Part)
y = sum(epi$TLA.Part != epi$Asm.Part)
Total = x + y
Total
# There are 28,593 instances of the TLA and ASM part number being the same in the data set
# There are also 4,288 instances of this not being the case as well 

mean(epi$Qty.Comp) #309.59
mean(epi$Required.Qty) #301.35
mean(epi$Disparity) #-8.24
# On average, we make around 8 more parts than needed for each job we run

## MODLES AND REGRESSION 
# Here, I am trying to find correlation, and also predict certain 
# variables using other variables in our data set
model1 = lm(Disparity ~ factor(Oper) + Asm)
summary(model1) 
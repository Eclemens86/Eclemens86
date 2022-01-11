#The followings are data frames for "Salary" "PPG" "FG%" "GP" "MPG"

#SALARY
curry.salary<-c(2913840,2508901,3958742,9887642,10629213,
         11370786,12112359,34682550,37457154,40231758)
westbrook.salary<- c(4017720,4090724,13668750,14693906,15719062,
             16744218,26540100,28530608,35654150,37303152)
paul.salary<- c(14490152,13167648,17779458,18668431,20068563,21468696,
        22868828,24268959,35654150,36099827)
durant.salary<- c(6053663,12480948,16669629,17832627,18995624,
          20158622,26540100,25000000,30000000,37199000)
harden.salary<- c(4304520,3706270,5820416,13701250,14728844,15756438,
          26540100,28299399,30421854,35811563)
wall.salary <- c(5144280,4451040,5915880,7459925,14746000,15851950,
        16957900,18063850,19169800,38199000)
james.salary<- c(14500000,12896159,17545000,19067500,20644400,
         22970500,30963450,33285709,35654150,37436858)
lowry.salary<- c(5750000,4628049,5750000,6210000,12000000,
         12000000,12000000,28703704,31000000,32809028)
griffin.salary<- c(5357280,4612820,7226892,16441500,17674613,
           18907725,20140838,32088932,31873932,
           34234964)
george.salary<- c(2238660,1936730,2574120,3282003,15925680,
          17120106,18314532,19508958,30560700,33005556)

salary <- data.frame(curry.salary, westbrook.salary, paul.salary, durant.salary, harden.salary, wall.salary, james.salary, lowry.salary, griffin.salary, george.salary)
SALARY<- as.data.frame(t(salary))


rownames(SALARY)<-c("Stephen Curry","Russell Westbrook","Chris Paul", "Kevin Durant","James Harden",
                    "John Wall","LeBron James", "Kyle Lowry","Blake Griffin","Paul George")
colnames(SALARY) = c("2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20")

SALARY

#_________________________________________________________________________________________________________________

#PPG (Points Per Game)
curry.ppg <- c(18.6, 14.7, 22.9, 24.0, 23.8, 30.1, 25.3, 26.4, 27.3, 20.8)
westbrook.ppg <- c(21.9, 23.6, 23.2, 21.8, 28.1, 23.5, 31.6, 25.4	, 22.9, 27.2)
paul.ppg <- c(15.9, 19.8, 16.9, 19.1, 19.1, 19.5, 18.1, 18.6, 15.6, 17.6)
durant.ppg <- c(27.7, 28.0, 28.1, 32.0, 25.4, 28.2, 25.1, 26.4, 26.0, 0)
harden.ppg <- c(12.2, 16.8, 25.9, 25.4, 27.4, 29.0, 29.1, 30.4, 36.1, 34.3)
wall.ppg <- c(16.4, 16.3, 18.5, 19.3, 17.6, 19.9, 23.1, 19.4, 20.7, 0.0)
james.ppg <- c(26.7, 27.1, 26.8, 27.1, 25.3, 25.3, 26.4, 27.5, 27.4, 25.3)
lowry.ppg <- c(13.5, 14.3, 11.6, 17.9, 17.8, 21.2, 22.4, 16.2, 14.2, 19.4)
griffin.ppg <- c(22.5, 20.7, 18.0, 24.1, 21.9, 21.4, 21.6, 21.4, 24.5, 15.5)
george.ppg <- c(7.8, 12.1, 17.4, 21.7, 8.8, 23.1, 23.7, 21.9, 28.0, 21.5)

ppg <- data.frame(curry.ppg, westbrook.ppg, paul.ppg, durant.ppg, harden.ppg, wall.ppg, james.ppg, lowry.ppg, griffin.ppg, george.ppg)
PPG <- as.data.frame(t(ppg))

rownames(PPG) <- c("Stephen Curry", "Russell Westbrook", "Chris Paul", "Kevin Durant", "James Harden",
                   "John Wall", "Lebron James", "Kyle Lowry", "Blake Griffin", "Paul George")
colnames(PPG) = c("2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20")

PPG

#_______________________________________________________________________________________________________________

#FG% (Field Goal Percentage)
curry.fg <- c(48.0,49.0,45.1,47.1,48.7,50.4,46.8,49.5,47.2,40.2)
westbrook.fg <- c(44.2,45.7,43.8,43.7,42.6,45.4,42.5,44.9,42.8,47.2)
paul.fg <- c(46.3,47.8,48.1,46.7,48.5,46.2,47.6,46.0,41.9,48.9)
durant.fg <- c(46.2,49.6,51.0,50.3,51.0,50.5,53.7,51.6,52.1,0)
harden.fg <- c(43.6,49.1,43.8,45.6,44.0,43.9,44.0,44.9,44.2,44.4)
wall.fg <- c(40.9,42.3,44.1,43.3,44.5,42.4,45.1,42.0,44.4,0)
james.fg <- c(51.0,53.1,56.5,56.7,48.8,52.0,54.8,54.2,51.0,49.3)
lowry.fg <- c(42.6,40.9,40.1,42.3,41.2,42.7,46.4,42.7,41.1,42.6)
griffin.fg <- c(50.6,54.9,53.8,52.8,50.2,49.9,49.3,43.8,46.2,35.2)
george.fg <- c(45.3,44.0,41.9,42.4,36.7,41.8,46.1,43.0,43.8,43.9)

fg <- data.frame(curry.fg, westbrook.fg, paul.fg, durant.fg, harden.fg, wall.fg, james.fg, lowry.fg, griffin.fg, george.fg)
FG <- as.data.frame(t(fg))

rownames(FG) <- c("Stephen Curry","Russell Westbrook","Chris Paul", "Kevin Durant","James Harden",
                    "John Wall","LeBron James", "Kyle Lowry","Blake Griffin","Paul George")
colnames(FG) <- c("2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20")

FG

#_____________________________________________________________________________________________________________

#GP (Games Played)
curry.gp <- c(48,25,76,77,80,79,79,51,69,5)
westbrook.gp <- c(55,68,80,45,67,80,81,80,73,57)
paul.gp <- c(59,61,67,61,82,74,61,58,58,70)
durant.gp <- c(51,68,79,80,27,72,62,68,78,0)
harden.gp <- c(55,64,76,73,81,82,81,72,78,68)
wall.gp <- c(42,67,49,81,79,77,78,41,32,0)
james.gp <- c(54,64,74,77,69,76,74,82,55,67)
lowry.gp <- c(55,48,66,79,70,77,60,78,65,58)
griffin.gp <- c(55,67,77,79,67,35,61,58,75,18)
george.gp <- c(34,66,79,79,6,81,75,79,77,48)

gp <- data.frame(curry.gp, westbrook.gp, paul.gp, durant.gp, harden.gp, wall.gp, james.gp, lowry.gp, griffin.gp, george.gp)
GP <- as.data.frame(t(gp))

rownames(GP)<-c("Stephen Curry","Russell Westbrook","Chris Paul", "Kevin Durant","James Harden",
                "John Wall","LeBron James", "Kyle Lowry","Blake Griffin","Paul George")
colnames(GP) = c("2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20")

GP

#____________________________________________________________________________________________________________

#MPG (Minutes Per Game)
curry.mpg <- c(33.6,28.1,38.2,36.5,32.7,34.2,33.4,32.0,33.8,27.9)
westbrook.mpg <- c(34.7,35.3,34.9,30.7,34.4,34.4,34.6,36.4,36.0,36.0)
paul.mpg <- c(36.0,36.3,33.4,35.0,34.8,32.7,31.5,31.8,32.0,31.5)
durant.mpg <- c(38.9,38.6,38.5,38.5,33.8,35.8,33.4,34.2,34.6, 0)
harden.mpg <- c(26.7,31.4,38.3,38.0,36.8,38.1,36.4,35.4,36.8,36.5)
wall.mpg <- c(37.8,36.2,32.7,36.3,35.9,36.2,36.4,34.4,34.5,0)
james.mpg <- c(38.8,37.5,37.9,37.7,36.1,35.6,37.8,36.9,35.2,34.6)
lowry.mpg <- c(34.2,32.1,29.7,36.2,34.5,37.0,38.4,32.2,34.1,36.2)
griffin.mpg <- c(37.9,36.2,32.5,35.8,35.2,33.4,34.0,33.85,35.0,28.5)
george.mpg <- c(20.7,29.7,37.6,36.2,15.2,34.8,35.9,36.6,36.9,29.6)

mpg <- data.frame(curry.mpg, westbrook.mpg, paul.mpg, durant.mpg, harden.mpg, wall.mpg, james.mpg, lowry.mpg, griffin.mpg, george.mpg)
MPG <- as.data.frame(t(mpg))

rownames(MPG)<-c("Stephen Curry","Russell Westbrook","Chris Paul", "Kevin Durant","James Harden",
                "John Wall","LeBron James", "Kyle Lowry","Blake Griffin","Paul George")
colnames(MPG) = c("2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20")

MPG

#______________________________________________________________________________________________________________
#______________________________________________________________________________________________________________
#Graph

install.packages("ggplot2")
library(ggplot2)

#_______________________________________________________________________________
#Select players
#_______________________________________________________________________________
#Player 1: Stephen Curry PPG
sc <- data.frame(curry.ppg, curry.fg, curry.salary)
rownames(sc) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
sc$ratio_salary_ppg <- format(round(curry.salary/curry.ppg,2), nsmall=2)
sc$ratio_salary_ppg <- as.numeric(sc$ratio_salary_ppg)
sc$ratio_salary_fg <- curry.fg
sc

#Stephen Curry's salary to ppg ratio over 10 years
ggplot(data=sc, aes(x = as.numeric(rownames(sc)), y = ratio_salary_ppg))+
  geom_point() + geom_smooth() + geom_text(aes(label=rownames(sc)),
  hjust=0.5, vjust=2) + xlim(2010, 2019)

#Correlation between Stephen Curry's points per game and salary
cor(curry.ppg,curry.salary)

#_______________________________________________________________________________
#Player 2: LeBron James PPG
lbj <- data.frame(james.ppg, james.fg, james.salary)
rownames(lbj) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
lbj$ratio_salary_ppg <- format(round(james.salary/james.ppg,2), nsmall=2)
lbj$ratio_salary_ppg <- as.numeric(lbj$ratio_salary_ppg)
lbj$ratio_salary_fg <- james.fg
lbj

#LeBron James's salary to ppg ratio over 10 years
ggplot(data=lbj, aes(x = as.numeric(rownames(lbj)), y = ratio_salary_ppg))+
  geom_point() + geom_smooth() + geom_text(aes(label=rownames(lbj)),
                                           hjust=0.5, vjust=2) + xlim(2010, 2019)

#Correlation between LeBron James's points per game and salary
cor(james.ppg,james.salary)

sd(james.ppg)
sd(curry.ppg)
#LBJ has negative correlation between ppg and salary. The reason is because his salary has been increasing over the years
#but his ppg is very stable with very small standard deviation (only 0.88).
#while curry's standard deviation for his ppg is 4.46, which is 5 times of lbj's
#_______________________________________________________________________________
#Player 1: Stephen Curry FG%
sc1 <- data.frame(curry.ppg, curry.fg, curry.salary)
rownames(sc1) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
sc1$ratio_salary_fg <- format(round(curry.salary/curry.fg,2), nsmall=2)
sc1$ratio_salary_fg <- as.numeric(sc$ratio_salary_fg)
sc1

#Stephen Curry's salary to ppg ratio over 10 years
ggplot(data=sc1, aes(x = as.numeric(rownames(sc1)), y = ratio_salary_fg))+
  geom_point() + geom_smooth() + geom_text(aes(label=rownames(sc1)),
                                           hjust=0.5, vjust=2) + xlim(2010, 2019)

#Correlation between Stephen Curry's field goal percentage and salary
cor(curry.fg,curry.salary)
#_______________________________________________________________________________
#Player 2: LeBron James FG%
lbj1 <- data.frame(james.ppg, james.fg, james.salary)
rownames(lbj1) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
lbj1$ratio_salary_fg <- format(round(james.salary/james.fg,2), nsmall=2)
lbj1$ratio_salary_fg <- as.numeric(jbj1$ratio_salary_fg)
lbj1

#LeBron James's salary to ppg ratio over 10 years
ggplot(data=lbj1, aes(x = as.numeric(rownames(lbj1)), y = ratio_salary_ppg))+
  geom_point() + geom_smooth() + geom_text(aes(label=rownames(lbj1)),
                                           hjust=0.5, vjust=2) + xlim(2010, 2019)

#Correlation between LeBron James's field goal percentage and salary
cor(james.fg,james.salary)

sd(james.fg)
sd(curry.fg)
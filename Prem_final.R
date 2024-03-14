# Evan Clemens

rm(list = ls())

library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(randomForest)
library(dplyr)
library(glmnet)
library(lubridate)
library(RColorBrewer)
library(reshape2)

# Reading in data 
prem <- read.csv("2020-2022.csv", stringsAsFactors = TRUE)

str(prem)
summary(prem)

# Changing the column names for Avg>2.5 and Avg<2.5 to get rid of "> and <"
colnames(prem)[27] <- "AvgOver2.5"
colnames(prem)[28] <- "AvgUnder2.5"

# Creating Y var(s)
# For random forests, Y = factor. For logistic regression Y = binary 
prem$over2.5_f <- ifelse((prem$FTHG + prem$FTAG) >= 3, "Yes", "No")
prem$htw_f <- ifelse(prem$FTHG > prem$FTAG, "W", "D/L")

prem$over2.5_fact <- as.factor(prem$over2.5_f)
prem$htw_fact <- as.factor(prem$htw_f)

##############################
# DATA PREPERATION -----------
##############################
RNGkind(sample.kind = "default")
set.seed(2291352)

train.index <- sample(x = 1:nrow(prem), size = ( 0.8 * nrow(prem)))

# Creating training data
train.df <- prem[train.index,]

# Creating testing data
test.df <- prem[-train.index,]

# Removing unneeded variables
train.df <- subset(train.df, select = -c(Date, FTHG, FTAG, FTR, over2.5_f, htw_f))

#######################################
# SINGLE BASE TREE over 2.5 -----------
#######################################
set.seed(172172172)
ctree <- rpart(over2.5_fact ~ ., data = train.df,  method = 'class')
rpart.plot(ctree)

##################################
# SINGLE BASE TREE htw -----------
##################################
set.seed(172172172)
ctree <- rpart(htw_fact ~ ., data = train.df,  method = 'class')
rpart.plot(ctree)

#######################################
# BASELINE FOREST over 2.5 ------------
#######################################
str(train.df)
na.exclude(prem)

myforestover <- randomForest(over2.5_fact ~ ., 
                        data = train.df,
                        ntree = 1000,
                        mtry = 4)
myforestover
plot(myforestover)

##################################
# BASELINE FOREST htw ------------
##################################
str(train.df)
na.exclude(prem)

myforesthtw <- randomForest(htw_fact ~ ., 
                         data = train.df,
                         ntree = 1000,
                         mtry = 4)
myforesthtw
plot(myforesthtw)
##########################################
# TUNING FOREST for over 2.5 -------------
##########################################

str(train.df)

# Making list of potential M values 
mtry <- c(1:25)

# Making room for our pairs of m and OOB error 
keeps <- data.frame(m = rep(NA, length(mtry)),
                   OOB_error_rate = rep(NA, length(mtry)))

# Looping over each element of mtry
for (idx in 1:length(mtry)) {
  print(paste0("Fitting m = ", mtry[idx]))
  tempforest <- randomForest(over2.5_fact ~ .,
                            data = train.df,
                            ntree = 1000,
                            mtry = mtry[idx])
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_error_rate"] <- mean( predict(tempforest) != train.df$over2.5_fact )
  
  
}

keeps

ggplot(data = keeps) + geom_line(aes(x = m, y = OOB_error_rate)) # Loop through 2-3 times 
# Loop 1 we see that an m of 6 is ideal of MINIMIZING OOB error
# Loop 2 we see that an m of 14 is ideal of MINIMIZING OOB error
# Loop 3 we see that an m of 3 is ideal of MINIMIZING OOB error


# New tree w/ m tuning 
final_forest_over <- randomForest(over2.5_fact ~ .,
                            data = train.df,
                            ntree = 1000,
                            mtry = 6,
                            importance = TRUE) # based on tuning of m   
final_forest_over
plot(final_forest_over)
# 6 was the lower error rate I got from the 3 loops so I went with 6.

#############################################
# TUNING FOREST for home team W -------------
#############################################
str(prem)

# Making list of potential M values 
mtry <- c(1:25)

# Making room for our pairs of m and OOB error 
keeps <- data.frame(m = rep(NA, length(mtry)),
                   OOB_error_rate = rep(NA, length(mtry)))

# Looping over each element of mtry
for (idx in 1:length(mtry)) {
  print(paste0("Fitting m = ", mtry[idx]))
  tempforest <- randomForest(htw_fact ~ .,
                            data = train.df,
                            ntree = 1000,
                            mtry = mtry[idx])
  keeps[idx, "m"] = mtry[idx]
  keeps[idx, "OOB_error_rate"] = mean( predict(tempforest) != train.df$htw_fact )
  
  
}

keeps

ggplot(data = keeps) + geom_line(aes(x = m, y = OOB_error_rate)) # loop through 2-3 times
# An m of 2 is ideal for MINIMIZING OOB error

# New tree w/ m tuning 
final_forest_htw <- randomForest(htw_fact ~ .,
                            data = train.df,
                            ntree = 1000,
                            mtry = 2,
                            importance = TRUE) # based on tuning of m   
final_forest_htw
plot(final_forest_htw)
###########################################
# RESULTS for over 2.5 --------------------
###########################################

# PREDICTION:
# ROC curve
pi_hat <- predict(final_forest_over, test.df, type = "prob")[, "Yes"]
rocCurve <- roc(response = test.df$over2.5_fact,
               predictor = pi_hat,
               levels = c("No", "Yes"))
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

# If we set the pi_star = 0.568, we are guaranteed a specificity of 0.714 and 
# a sensitivity of 0.853.

# That is, we predict under 2.5 goals 71.4% of the time when it actually goes under.
# We predict over 2.5 goals 85.3% of the time when it actually goes over

# AUC = 0.842. (.5 = random guesses, 1 = complete accuracy)
# This is higher than what I got with my tree

# Making a column of predicted values (Yes or No)
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold
test.df$forest_pred <- ifelse(pi_hat > pi_star, "Yes", "No")
test.df

# Variable Importance Plots
varImpPlot(final_forest_over, type = 1)

# Fitting a logistic regression with only the best variables from the 
# random forest with a Bernoulli random component since data is binary. 

# Creating a Bernoulli random variable 
prem$over2.5_fact_bin <- ifelse(prem$over2.5_fact == "Yes", 1, 0)

m1 <- glm(over2.5_fact_bin ~ HTAG + HTHG, data = prem, 
         family = binomial(link = "logit"))
m1
# AIC of m1 = 765.7

m2 <- glm(over2.5_fact_bin ~ HTAG + HTHG + AST, data = prem, 
         family = binomial(link = "logit"))
m2
# AIC of m1 = 749

m3 <- glm(over2.5_fact_bin ~ HTAG + HTHG + AST + HTR, data = prem, 
         family = binomial(link = "logit"))
m3
# AIC of m1 = 748.2

m4 <- glm(over2.5_fact_bin ~ HTAG + HTHG + AST + HTR + htw_fact, data = prem, 
         family = binomial(link = "logit"))
m4
# AIC of m1 = 718

m5 <- glm(over2.5_fact_bin ~ HTAG + HTHG + AST + HTR + htw_fact + HST, data = prem, 
         family = binomial(link = "logit"))
m5
# AIC of m1 = 700.7

m6 <- glm(over2.5_fact_bin ~ HTAG + HTHG + AST + HTR + htw_fact + HST + AvgD, data = prem, 
         family = binomial(link = "logit"))
m6
# AIC of m1 = 702.7
# AIC increases, that means we should stick with m5!

summary(m5)
# No Std. error over 5, no complete separation
coef(m5)
beta_hat <- coef(m5)

exp(beta_hat)
# Custom odds ratios
# Estimated odds of a game going over 2.5 goals when the away team records 5 SOT
# e^(-3.75905719 + 0.30287802(5)) = .106
# Estimated odds of a game going over 2.5 goals when the away team records 10 SOT
# e^(-3.75905719 + 0.30287802(10)) = .482

# Estimated odds of a game going over 2.5 goals when the home team records 10 SOT
# e^(-3.75905719 + 0.19287269(10)) = .161

confint(m5, level = 0.95)
# We are 95% confident that the odds of the game going over 2.5 goals change 
# by a factor between 1.23 and 1.49 for each shot on target the away team records.

# We are 95% confident that the odds of of the game going over 2.5 goals change 
# by a factor between 1.11 and 1.33 for each shot on target the home team records.

##############################################
# RESULTS for home team W --------------------
##############################################

# PREDICTION:
# ROC curve
pi_hat <- predict(final_forest_htw, test.df, type = "prob")[, "W"]
rocCurve <- roc(response = test.df$htw_fact,
               predictor = pi_hat,
               levels = c("D/L", "W"))
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

# If we set the pi_star = 0.391, we are guaranteed a specificity of 0.636 and 
# a sensitivity of 0.906.

# That is, we predict a draw or loss 63.6% of the time when the home team 
# actually does not get a result
# We predict a win 90.6% of the time when the home team actually wins.

# AUC = 0.829. (.5 = random guesses, 1 = complete accuracy)
# This is higher than what we got with the tree, so we prefer this model 
# over the tree.

# Making a column of predicted values (W or D/L)
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold
test.df$forest_pred <- ifelse(pi_hat > pi_star, "W", "D/L")
test.df

# Variable Importance Plots
varImpPlot(final_forest_htw, type = 1)

# Fitting a logistic regression with only the best variables from the 
# random forest. 

# Creating a Bernoulli random variable 
prem$htw_fact_bin <- ifelse(prem$htw_fact == "W", 1, 0)

m1 <- glm(htw_fact_bin ~ HTR + HST, data = prem, 
         family = binomial(link = "logit"))
m1
# AIC of m1 = 694.4

m2 <- glm(htw_fact_bin ~ HTR + HST + AST, data = prem, 
         family = binomial(link = "logit"))
m2
# AIC of m1 = 646.5

m3 <- glm(htw_fact_bin ~ HTR + HST + AST + HTHG, data = prem, 
         family = binomial(link = "logit"))
m3
# AIC of m1 = 647
# AIC increases, that means we should stick with m2!

summary(m2)
# No Std. error over 5 meaning no complete separation

beta_hat <- coef(m2)
beta_hat
exp(beta_hat)
# Custom odds ratios
# Estimated odds of a game ending in a home team win when the away team records 5 SOT
# e^(-2.417 - 0.338(5)) = .013

# Estimated odds of a game ending in a home team win  when the home team records 5 SOT
# e^(-2.417 + 0.368(5)) = .561

confint(m2, level = 0.95)
# We are 95% confident that the odds of of the game ending in a home team win change 
# by a factor between 1.32 and 1.59 for each shot on target the away team records.

# We are 95% confident that the odds of of the game ending in a home team win change 
# by a factor between .64 and .79 for each shot on target the home team records.

##############################
# CLUSTERING ----------------
##############################

# Further insight
ggplot(data = prem) + geom_histogram(aes(x = HST))
# HST varies between 0-15, what is driving that?

# Plot between home team (fact) and HST (num)
ggplot(data = prem) + geom_violin(aes(x = HomeTeam, y = HST)) +
  geom_jitter(aes(x = HomeTeam, y = HST, colour = HST), alpha = I(0.5)) + 
  scale_colour_gradientn(colours = c("#663300", "#FFFFCC")) + coord_flip()

# Plot between ref (fact) and HY (num)
ggplot(data = prem) + geom_violin(aes(x = Referee, y = HY)) +
  geom_jitter(aes(x = Referee, y = HY, colour = HY), alpha = I(0.5)) + 
  scale_colour_gradientn(colours = c("#663300", "#FFFFCC")) + coord_flip()

# Does home odds effect shots on target like we assume? 
ggplot(data = prem) + geom_point(aes(x = AvgH, y = HST, colour = AvgOver2.5)) +
  scale_colour_gradientn(colours = c("#663300", "#FFFFCC")) + 
  geom_text(aes(x = AvgH, y = HST, label = ifelse(HST < 4 & AvgH > 14, HomeTeam, ""))) +
  labs(caption = ("3 = Brentford, 5 = Burnley, 15 = Newcastle,
                  16 = Norwich, 21 = West Brom"))

summary(prem$HomeTeam)
# 3 = Brentford (13), 5 = Burnley (17,R), 15 = Newcastle (12,11), 
# 16 = Norwich (R), 21 = West Brom (R)

str(prem)
prem_sub <- subset(prem) 
prem_sub_X <- subset(prem, select = -c(1,2,3,4,7,10,11,29,30,31,32,33,34)) #remove categorical columns)

# Standardizing
prem_stand <- apply(prem_sub_X, 2, function(x){(x - mean(x))/sd(x)})

#k-means clustering
wss <- (nrow(prem_stand)-1)*sum(apply(prem_stand,2,var)) 
for (i in 2:21) {
  
  wss[i] <- sum(kmeans(prem_stand, centers=i)$withinss)
  
}

#Elbow method using a "scree plot":
ggplot() + geom_line(aes(1:21, wss)) +
  labs(x = "Number of Clusters", y = "Within Cluster Sums of Squares (WSS)")
#looks like 7 might be a good starting point since that's where the elbow starts

prem_kmeans <- kmeans(prem_stand, 7)

# Saving the cluster labs to the x matrix (pre-standardized)
prem_sub_X$km_cluster <- as.factor(prem_kmeans$cluster)

head(prem_sub_X)

prem_long <- melt(prem_sub_X, id.vars = c("km_cluster"))

head(prem_long)

ggplot(data = prem_long) + geom_boxplot(aes(x = km_cluster, y = value, fill = km_cluster)) +
  facet_wrap(~variable, scales = "free") + scale_fill_brewer(palette = "Dark2") + 
  ggtitle("K-means Clusters")

# Saving the clusters onto the original data
prem_sub$km_cluster = as.factor(prem_kmeans$cluster)

# Look at cluster 1
View(subset(prem_sub, km_cluster == 1))
# Nothing sticking out, very average all around. Teams in this cluster are ones 
# that before the game, the general public consensus is that they are on the same
# level (bad vs bad, good vs good)

# Look at cluster 2
View(subset(prem_sub, km_cluster == 2)) 
# Away wins, second highest home odds and second lowest away odds, only 13 instances
# of the home team taking anything from the game. Away team goals average at half 
# is almost 1 which is the highest of any cluster. Highest avg FTAG as well. 
# Higher home fouls than cluster 4 and movement for home red cards 
# (home teams getting frustrated?). 
# Bad memories in this cluster as 3 big Man U home defeats appear here. 

# Look at cluster 3
View(subset(prem_sub, km_cluster == 3)) 
# Highest full time home goals, lowest full time away goals. Highest home shots and 
# lowest away shots, same with ST. Lowest foul rates  for both teams 
# and highest corner rate for home teams. Lowest home odds and highest draw and 
# away odds by far (home team predicted to win). Lowest over odds and highest under
# odds, HOME team expected to win and win big. 
# A huge amount of Liverpool and Man City home wins by a lot in this cluster. 

# Look at cluster 4
View(subset(prem_sub, km_cluster == 4)) 
# Away wins as well. Away dominating defensively with lowest home shots and shots 
# on target. Equal amount of fouling. Most away corners by far of any cluster. 
# Highest average odds for home teams and draws. Mostly big six clubs winning, 
# second highest odds for the under and second lowest odds for the over meaning 
# AWAY team excepted to win and win big. 
# Man City known for killing teams and this cluster has a lot of Man City massive
# away wins. 

# Look at cluster 5
View(subset(prem_sub, km_cluster == 5)) 
# Highest fouls and yellows. Higher over odds and lower under odds. This means that
# it is two teams that are excepted to be in a close, low scoring, rough game. 
# Lots of good away wins from man U in this cluster.

# Look at cluster 6
View(subset(prem_sub, km_cluster == 6))
# Highest over 2.5 odds and lowest under odds. Not a lot of variation in H, D, or A
# odds that stick out, a pick em' perhaps? Lowest half time goals for both teams. 
# Odds back up final result of low scoring game. Lower average shots as well. 
# Man U vs their rivals Man City finishing 0-0 in this cluster. 

# Look at cluster 7
View(subset(prem_sub, km_cluster == 7)) 
# Lower average home odds, higher average away odds, the over and under are around the
# same as well. Man U's famous 9-0 win against Southampton in this cluster. 


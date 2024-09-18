# Evan Clemens

# INTRODUCTION ----
# PROBELMS:
# 1. Research question -> What are the the under reached location(s) in terms of
#                         seniors and food scarcity based on demographic and  
#                         economic data? We plan to use both the DMARC data
#                         and outside sources (potentially ACS and/or other
#                         areas) to get more a accurate look at the senior demographic 
#                         and paint a picture of the economic landscape 
#                         of the Des Moines metropolitan area to help answer
#                         this question.
# 2. Visualizations    -> Map of Iowa poverty levels (ACS/CPI data) by county or
#                         city, overlaid with locations of food pantries to show
#                         potential areas of need. 
# 3. Modeling goal     -> Clustering households to gain deeper insights into senior families in terms of 
#                         potential vulnerability. Also we could use time series principals to tell us
#                         what factors are influencing peoples general food pantry preferences

# 4/15
# Here is the plan guys :) 
# 1. See if a seed works to make process reproducible (seed is set, need to confirm if 
#    reproducible) if seed not working, sad :( , then use hierarchical clustering 

# 4/22
# SLIDE FEEDBACK
# 1. Give examples of what clustering is to people who might not know
# 2. Make a side mentioning model limitations (k-means meant for continuous data, not binary)

# CODE FEEDBACK 
# 1. Make more descriptive visualizations from clustering 

source("src/DMARC_clean.R")
head(fd)
summary(fd)
str(fd)

install.packages(c("fastDummies", "matrixStats", "reshape2", "gridExtra", 
                   "patchwork", "TSstudio", "RColorBrewer"))

library(fastDummies)
library(matrixStats)
library(reshape2)
library(gridExtra)
library(patchwork)
library(TSstudio)
library(RColorBrewer)


# Finding and eliminating NA's FOR fd
# Check for NA values in the entire data frame
sum(is.na(fd))
which(is.na(fd))
# Summarize the presence of NA values
na_matrix <- is.na(fd)
summary(na_matrix)
# If you want to know the total count of NA values in each column
col_na_counts <- colSums(na_matrix)
print(col_na_counts)
# If you want to know the total count of NA values in each row
row_na_counts <- rowSums(na_matrix)
print(row_na_counts)
# If you want to know the indices of rows and columns where NA values are located
indices <- which(na_matrix[1:1465405, 1:24], arr.ind = TRUE)
print(indices) # 151 instances of NA. DOB and Age columns.

# EXPLORATORY ANALYSIS AND VISUALIZATIONS-------
ggplot(data = fd) + geom_histogram(aes(x = fed_poverty_level))

# Plot between ethnicity (categorical) and fed_poverty_level (num)
ggplot(data = fd) + geom_violin(aes(x = ethnicity, y = fed_poverty_level)) +
  geom_jitter(aes(x = ethnicity, y = fed_poverty_level, colour = fed_poverty_level), 
              alpha = I(0.5)) + 
  scale_colour_gradientn(colours = c("#663300", "#FFFFCC"))

# CLUSTERING ----
# GROUP BY AFN (family level)
# Make this a new clustering only subset
# Senior citizen is 65 or older

# Subset to where anyone in the house is a senior, so we include other family members
# Could also show us factors that contribute to a high number of household visits
fd_afn = fd %>%
  group_by(afn) %>%
  summarise(female_prop = sum(gender == "Woman (girl)")/length(gender),
            male_prop = sum(gender == "Man (boy)")/length(gender),
            snap_Y_prop = sum(snap_household == "Y")/length(snap_household),
            income_min = min(annual_income),
            income_max = max(annual_income),
            income_mean = mean(annual_income),
            income_var = var(annual_income), # na.rm = TRUE if we want to remove NA's
            fedpov_min = min(fed_poverty_level),
            fedpov_max = max(fed_poverty_level),
            age_min = min(age),
            age_max = max(age),
            age_mean = mean(age),
            age_sr = sum(age >= 65)/length(age),
            grandfam = ifelse(any(age >= 65) & any(age <= 16), 1, 0),
            doughnutfam = ifelse(any(age >= 65) & any(age < 16) & !any(age >= 17 & age <= 64), 1, 0),
            edu_college = sum(education == "College 2 or 4 yr  Degreee" | education == "College Advanced Degree")/length(education),
            edu_in_school = sum(education == "Pre-K and younger (Currently)" | education == "K-8 (Currently)" | education == "9-12 (Currently)")/length(education),
            income_source_retired = sum(income_source == "Retired")/length(income_source),
            income_source_ss = sum(income_source == "Social Security")/length(income_source),
            mean_house = mean(household_size)
            
            ) %>%
  filter(age_max >= 65)

table(fd_afn$doughnutfam)
table(fd_afn$grandfam)

# K-means clustering
fd_afn_sub = subset(fd_afn)

fd_afn_subIV = subset(fd_afn_sub, income_var != "")
mean(fd_afn_subIV$income_var)

# Replacing NA's in income_var with the mean of the column 
fd_afn_sub$income_var <- ifelse(is.na(fd_afn_sub$income_var) == TRUE, 36610191, 
                                fd_afn_sub$income_var)

# remove categorical columns and original vars for dummies 
fd_afn_subX = subset(fd_afn_sub, select = -c(1)) 
# numbers in "select" arg are related to columns in data 

# Standardizing
fd_stand = fd_afn_subX %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale))

# Finding and eliminating NA's FOR fd_stand
sum(is.na(fd_stand))
which(is.na(fd_stand))
# Check for NA values in the entire data frame
na_matrix <- is.na(fd_stand)
# Summarize the presence of NA values
summary(na_matrix)
# If you want to know the total count of NA values in each column
col_na_counts <- colSums(fd_stand)
# Print the counts
print(col_na_counts)
# If you want to know the total count of NA values in each row
row_na_counts <- rowSums(fd_stand)
# Print the counts
print(row_na_counts)
# If you want to know the indices of rows and columns where NA values are located
indices <- which(na_matrix[1:62350, 1:19], arr.ind = TRUE)
# Print the indices
print(indices)


set.seed(861512)
#k-means clustering
sum_of_variances <- sum(colVars(as.matrix(fd_stand)))

wss <- (nrow(fd_stand) - 1) * sum_of_variances

for (i in 3:20) {
  wss[i] <- sum(kmeans(fd_stand, centers = i, iter.max = 30)$withinss)
}
# iter.max = num (default is 10), algorithm = "algorithm" (default is Hartigan-Wong)

# Elbow method using a "screen plot":
ggplot() + geom_line(aes(1:20, wss)) +
  labs(x = "Number of Clusters", y = "Within Cluster Sums of Squares (WSS)")
# looks like 6 might be a good starting point since that's where the elbow starts

fd_kmeans <- kmeans(fd_stand, 6)

# Saving the cluster labs to the x matrix (pre-standardized)
fd_afn_subX$km_cluster <- as.factor(fd_kmeans$cluster)
head(fd_afn_subX)

fd_afn_long <- melt(fd_afn_subX, id.vars = c("km_cluster"))
head(fd_afn_long)

fd_afn_long_sub <- subset(fd_afn_long, !(variable %in% c("income_max", "income_var", 
                                                         "grandfam", "doughnutfam", 
                                                         "edu_college")))


ggplot(data = fd_afn_long_sub) + geom_boxplot(aes(x = km_cluster, y = value, fill = km_cluster)) +
  facet_wrap(~variable, scales = "free") + 
  scale_fill_brewer(name = "Clusters", palette = "Dark2") + 
  ggtitle("Clustering of senior families ") +
  labs(x = "Cluster", y = "Value") 
  

# Saving the clusters onto the original data
fd_afn_sub$km_cluster = as.factor(fd_kmeans$cluster)


# CLUSTER DESCRIPTION ----
# Look at cluster 1 
View(subset(fd_afn_sub, km_cluster == 1)) 
# Cluster description: Older, conventional, 2-person wealthier families. 
# Gender: Equal male and female prop
# Age: Higher avg age, 0.75 senior prop
# Income: Very little snap activity,  Highest avg income and fed poverty level with most 
# of the high outliers
# Misc: (edu, source, house size): 2nd highest grandfam prop

# Look at cluster 2 
View(subset(fd_afn_sub, km_cluster == 2))
# Cluster description: Younger, conventional, 2-person poorer families with snap
# Gender: About 50/50 males and females 
# Age: 2nd lowest average age  
# Income: Lower avg income and lowest fed poverty level, about half have snap 
# Misc: (edu, source, house size):

# Look at cluster 3 
View(subset(fd_afn_sub, km_cluster == 3))
# Cluster description: Mostly single female seniors living off social security 
# Gender: Mostly females
# Age: Mostly seniors
# Income: No snap, lower mean income
# Misc: (edu, source, house size): Social security as their main source of income

# Look at cluster 4
View(subset(fd_afn_sub, km_cluster == 4))
# Cluster description: Mostly single male, poorer seniors 
# Gender: Mostly males
# Age: Mostly seniors
# Income: No snap, lowest mean income
# Misc: (edu, source, house size): 60-70% social security income

# Look at cluster 5 
View(subset(fd_afn_sub, km_cluster == 5))
# Cluster description: Mostly single female, wealthier, retired seniors  
# Gender: Mostly females
# Age: Mostly seniors, oldest age
# Income: No snap
# Misc: (edu, source, house size): Retired as main income source 

# Look at cluster 6 
View(subset(fd_afn_sub, km_cluster == 6)) 
# Cluster description: Bigger grand/doughnut families with snap and children
# lowest mean age and education still in school, higher mean household size, high snap %

# MERGE DATA -------
# Merge cluster data with original data.

fd_merge <- merge(fd,fd_afn_sub,by ="afn") 

fd_merge2 = merge(fd_afn,fd_afn_sub,by ="afn") 

# Next steps would be merging on individual unique visit data. 
# This would give us a more concise look at being able to analyze what 
# each cluster group is telling us in our efforts to define how vulnerability is 
# shown in the data.

# PLOTS ----
fd_afn_sub$grandfam = as.factor(fd_afn_sub$grandfam)
fd_afn_sub$doughnutfam = as.factor(fd_afn_sub$doughnutfam)


Grand_plot <- ggplot(data = fd_afn_sub) +
  geom_bar(aes(x = km_cluster, fill = grandfam), position = "fill") + 
  ggtitle("Proportion of Grand Families in Each Cluster") + 
  labs(x = "Cluster", y = "Proportion", 
  caption = "Grand family - A household that has members present that are 65 or (seniors) 
  older and also members who are 16 or younger (minors)") + theme_bw() + 
  scale_fill_brewer(name = "Grand \nFamily", labels = c("No","Yes"),
                    type = "qual", palette = "Greys") 

Doughnut_plot <- ggplot(data = fd_afn_sub) +
  geom_bar(aes(x = km_cluster, fill = doughnutfam), position = "fill") + 
  ggtitle("Proportion of Doughnut Families in Each Cluster") + 
  labs(x = "Cluster", y = "Proportion", 
       caption = "Doughnut family - A grand family that has no adults in the household, so only members that 
       are seniors and also minors") + 
  theme_bw() + 
  scale_fill_brewer(name = "Doughnut \nFamily", labels = c("No","Yes"), 
                    type = "qual", palette = "Greys") 

Grand_plot + Doughnut_plot

# TIME SERIES ----
# Time series plot of each cluster with the data being times visited by month by unique visit 
# length(unique(individual_id))

# Have cluster number recognized so filtering works 
# Only need one row per served date

fd_merge %>%
  mutate(week = floor_date(served_date, unit = "week")) %>% 
  group_by(km_cluster, week) %>%
  summarize(n_peeps = length(unique(individual_id))) %>%
  mutate(km_cluster = factor(km_cluster, levels = c("1","2","3","4","5","6"), 
                             labels = c(
          "Conventional 2-person wealthier families", 
          "Conventional 2-person poorer families",
          "Single female seniors with social security",
          "Single male, poorer seniors",
          "Single female, wealthier, retired seniors",
          "Grand families w/ snap and children"))) %>% 
  filter(n_peeps > 1) %>%
  ggplot() + geom_line(aes(x = week, y = n_peeps, colour = km_cluster)) +
  ggtitle("Unique Pantry Visits Over Time for each Cluster") + 
  labs(x = "Weeks", y = "Unique Visits") +
  facet_wrap(~km_cluster, scales = "free_y") +
  scale_colour_brewer(name = "Clusters", palette = "Dark2")

# Only cluster 6 
fd_merge %>%
  mutate(week = floor_date(served_date, unit = "week")) %>% 
  group_by(km_cluster, week) %>%
  summarize(n_peeps = length(unique(individual_id))) %>%
  mutate(km_cluster = factor(km_cluster, levels = c("1","2","3","4","5","6"), 
                             labels = c(
                               "Conventional 2-person wealthier families", 
                               "Conventional 2-person poorer families",
                               "Single female seniors with social security",
                               "Single male, poorer seniors",
                               "Single female, wealthier, retired seniors",
                               "Grand families w/ snap and children"))) %>% 
  filter(km_cluster == "Grand families w/ snap and children") %>%
  ggplot() + geom_line(aes(x = week, y = n_peeps), color = "#E6AC3F") +
  ggtitle("Unique Pantry Visits Over Time for Cluster 6") + 
  labs(x = "Weeks", y = "Unique Visits") +
  facet_wrap(~km_cluster, scales = "free_y") 
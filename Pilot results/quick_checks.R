setwd("/home/yulia/Documents/MIDS/W241/Group Project/Pilot results/")
library(pwr)
library(tidyverse)

pilot <- read.csv("W241 Final Project Survey - Control_July 17, 2018_07.21.csv",
                  skip = 3, header = FALSE, stringsAsFactors = FALSE)

Q5_rating <- read.csv("Pilot_Q5.csv")

headers <- read.csv("W241 Final Project Survey - Control_July 17, 2018_07.21.csv",
                  nrows = 1)

colnames(pilot) <- colnames(headers)

str(pilot)

hist(pilot$Duration..in.seconds.,
     breaks = seq(0,1200,30))

table(pilot$Q8) # correct is 2
table(pilot$Q6) # correct is 3
table(pilot$Q7) # correct is 3

pilot$Q8_correct <- pilot$Q8==2
pilot$Q6_correct <- pilot$Q6==3
pilot$Q7_correct <- pilot$Q7==3

table(pilot$Q8_correct, pilot$Q6_correct, pilot$Q7_correct)

pilot$n_correct_answers <- pilot$Q8_correct + pilot$Q6_correct + pilot$Q7_correct
table(pilot$n_correct_answers)

View(pilot[pilot$n_correct_answers==3,]$Q5)
View(pilot[pilot$n_correct_answers==2,]$Q5)

pilot <- merge(pilot, Q5_rating[,c("mTurkCode","Q5_rating")], by = "mTurkCode")
pilot$Duration_gt_60sec <- ifelse(pilot$Duration..in.seconds. < 60, 0, 1)

table(pilot$Q5_rating)
table(pilot$Q5_rating, pilot$n_correct_answers)
table(pilot$Q5_rating, pilot$Duration_gt_60sec)
summary(pilot$Duration..in.seconds.)
summary(pilot[pilot$Q5_rating>0,]$Duration..in.seconds.)
View(pilot[pilot$Duration_gt_60sec==0,])

pilot$pass <- (pilot$Duration..in.seconds. >= 60) & (pilot$Q5_rating > 0) & (pilot$n_correct_answers > 0)+0
table(pilot$pass)
table(pilot$Q4, pilot$pass)

par(mfrow=c(1,2))
hist(pilot$Q4, 
     breaks = seq(0,7,1)-0.5,
     ylim = c(0,20),
     col = "grey",
     xlab = "Rating",
     ylab = "Number of responses",
     main = "Ratings for the full dataset")
hist(pilot[pilot$pass==1,]$Q4, 
     breaks = seq(0,7,1)-0.5,
     ylim = c(0,20),
     col = "blue",
     xlab = "Rating",
     ylab = "Number of responses",
     main = "Ratings for the cleaned dataset")

summary(pilot$Q4)

pilot %>% 
  group_by(pass) %>% 
  summarize(mean(Q4, na.rm = TRUE))

t.test(Q4 ~ pass, data=pilot)

summary(pilot[pilot$pass==1,]$Q4)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   3.000   4.000   3.964   5.000   6.000 



# Parameters:

ate <- 1 # effect size (treatment 1 vs. control or treatment 2 vs. control)
sigma <- sd(pilot[pilot$pass==1,]$Q4, na.rm = TRUE) # standard deviation
alpha <- 0.05
beta <- 0.1 # 1-power

# Built-in formula
d = ate/sigma # assuming sigma is pooled standard deviation (equal variance)
pwr.t.test(d = d, sig.level = alpha, power = 1-beta, type = 'two.sample', alternative = 'greater')
# 34 responses in each bucket


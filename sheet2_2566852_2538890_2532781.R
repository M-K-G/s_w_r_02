###############
### Cleaning Data
###############

## Name: Kopf-Giammanco, Martin and Maike Puhl, and Jonathan Watkins
## Matriculation number: 2566852 (MKG), 2538890 (MP), 2532781 (JW)

# Please do the "Cleaning Data with R" exercise that was assigned in DataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands
# you learned there in the exercises below.
# This week, the exercise will be about getting data into a good enough shape to start analysing.
# Next week, there will be a tutorial on how to further work with this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory.
# done. It's in the subfolder data

# 2. Read in the data into a variable called "dat".
dat <- read.csv("data/digsym.csv")

# 3. Load the libraries languageR, stringr, dplyr and tidyr.
library(languageR)
library(stringr)
library(dplyr)
library(tidyr)

# 4. How many rows, how many columns does that data have?
dim(dat) # 3700 rows, 11 columns

# 5. Take a look at the structure of the data frame using "glimpse".
glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows.
head(dat, 20)
tail(dat, 20)

# 7. Is there any missing data in any of the columns?
# Yes, for example in columns named StimulDS1.CRESP, StimulDS1.RESP (factors) we empty strings, and
# e.g. in StimulDS1.RT there are a few "NA"s

# 8. Get rid of the row number column.

dat <- select(dat, -X)

# 9. Put the Sub_Age column second.
dat <- dat[,c(1,10,2,3,4,5,6,7,8,9)]

# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.

dat$ExperimentName <- (dat$ExperimentName = "dsc")

# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2",
# then assign data2 to dat and finally remove data2.

data2 <- dat[dat$List == "Trial:2", ]
dat <- data2
rm(data2)


# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".
dat <- separate(dat, Sub_Age, c("Subject", "Age"))

# 13. Make subject a factor.
dat$Subject <- as.factor(dat$Subject)

# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".

dat$File <- str_remove_all(dat$File, "[123456789_]")

# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e.,
# same number of characters, such that "1_right" should be replaced by "1_right0" etc).
# Note: I thought we're supposed to get rid of the leading digit underscore...?
dat$File <- as.factor(str_pad(dat$File, 8, "right", "0"))

# 16. Remove the column "List".

dat$List <- NULL

# 17. Change the data type of "Age" to integer.
dat$Age <- as.integer(dat$Age)

# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?

any(is.na(dat)) # there are no NAs in the data anymore

# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put
# in value 1, otherwise put 0.

dat$accuracy <- ifelse(dat$StimulDS1.RESP == dat$StimulDS1.CRESP, 1, 0)

# 20. How many wrong answers do we have in total?
length(dat$accuracy) - sum(dat$accuracy)
# 185 wrong answers in total

# 21. What's the percentage of wrong responses?
(length(dat$accuracy) - sum(dat$accuracy)) / length(dat$accuracy)
# percentage of wrong responses is 5.55


# 22. Create a subset "correctResponses" that only contains those data points where subjects
# responded correctly.
correctResponses <- subset(dat, accuracy == 1)

# 23. Create a boxplot of StimulDS1.RT - any outliers?
boxplot(dat$StimulDS1.RT)
# yes, there are outliers

# 24. Create a histogram of StimulDS1.RT with bins set to 50.
hist(dat$StimulDS1.RT, breaks = 50)

# 25. Describe the two plots - any tails? any suspiciously large values?
# There are outliers. The Max. of dat$StimulDS1.RT is at 13852 (milliseconds?)
# which is about ten times the amount for the 3rd quartile. Aside from that,
# there are plenty of values outside the IQR - as the circles beyond
# the whiskers in the boxplot indicate.
summary(dat$StimulDS1.RT)

# 26. View summary of correct_RT.
# no variable/dataset of that name? (correct Reaction Time?)
# Here is a summary of correct Response instead...
summary(dat$StimulDS1.CRESP)
summary(correctResponses)

# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named
# "cleaned".
cleaned <- filter(dat, StimulDS1.RT != max(StimulDS1.RT))
boxplot(cleaned$StimulDS1.RT)
hist(cleaned$StimulDS1.RT, breaks = 50)

## EXTRA Exercises:
##You can stop here for your submission of this week's assignment,
##but you are encouraged to try it now.
##All these exercises will be discussed and solved in the tutorial!

# 28. Dealing with the tail of the distribution: outlier removal
# Now we want to define a cutoff value for the StimulDS1.RT variable in the correctResp dataset.
# Values should not differ more than 2.5 standard deviations from the grand mean of this variable.
# This condition should be applied in a new variable called "correct_RT_2.5sd", which prints NA
# if an RT value is below/above the cutoff.
cutoff_dif <- sd(cleaned$StimulDS1.RT) * 2.5
mn_RT <- mean(cleaned$StimulDS1.RT)
cutoff_val <- (sd(cleaned$StimulDS1.RT) * 2.5) + mean(cleaned$StimulDS1.RT)

cleaned <- cleaned %>%
  mutate(correct_RT_2.5sd = ifelse(cleaned$StimulDS1.RT > (mn_RT+cutoff_dif), NA, StimulDS1.RT))

# 29. Take a look at the outlier observations.
# Any subjects who performed especially poorly?
any(is.na(cleaned$correct_RT_2.5sd))
# yes

# 30. How many RT outliers are there in total?
sum(is.na(cleaned$correct_RT_2.5sd))
# 82 subjects performed poorly

# 31. Plot a histogram and boxplot of the correct_RT_2.5sd column again - nice and clean eh?
hist(cleaned$correct_RT_2.5sd, breaks = 50)
boxplot(cleaned$correct_RT_2.5sd)

# 32. Next, we'd like to take a look at the average accuracy per subject.
# Using the "cast" function from the library "reshape", create a new data.frame which shows the
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".
library(reshape)

# Here is an attempt at grouping mean accuracy by Subject.
sub_avg <- cleaned %>%
  group_by(Subject) %>%
  summarise(avrg_accuracy = mean(accuracy))

# 33. Sort in ascending order or plot of the average accuracies per subject.
sub_avg <- sub_avg %>%
  arrange(avrg_accuracy)

hist(sub_avg$avrg_accuracy)
boxplot(sub_avg$avrg_accuracy)
summary(sub_avg)
min(sub_avg$avrg_accuracy)

# 34. Would you exclude any subjects, based on their avrg_accuracy performance?
# Yes, I would probably exclude one subject based on their poor performance, i.e.
# the observation with an average accuracy of 0.8555556.

# create vector with outlier-subjects:
subj_to_rm <- sub_avg %>%
  filter(avrg_accuracy < quantile(sub_avg$avrg_accuracy)[2]-IQR(sub_avg$avrg_accuracy)) %>%
  pull(Subject)

# remove observations that match subjects in outlier-subjects (only one):
cleaned_fin <- cleaned %>%
  filter(Subject != subj_to_rm)

# 35. Congrats! Your data are now ready for analysis. Please save the data frame you created
# into a new file called "digsym_clean.csv".
write.csv(cleaned_fin, "data/digsym_clean.csv")

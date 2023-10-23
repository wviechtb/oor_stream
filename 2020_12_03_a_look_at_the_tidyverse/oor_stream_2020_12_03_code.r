############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2020-12-03
#
# Topic(s):
# - a look at the tidyverse (dplyr, magrittr, tibble, etc.)
#
# last updated: 2020-12-10

############################################################################

# some potentially useful links:

# https://education.rstudio.com
# https://www.tidyverse.org
# https://psyr.djnavarro.net
# https://moderndive.com/
# https://r4ds.had.co.nz/
# https://www.datacamp.com/community/tutorials/pipe-r-tutorial

############################################################################

# install the tidyverse
#install.packages("tidyverse")

# load the tidyverse
library(tidyverse)

# set working directory (adjust to your own computer; in RStudio, go to the
# Session menu, 'Set Working Directory', and 'To Source File Location')
setwd("~/temp")

# read in data (using the 'base R' way)
dat <- read.delim("data_survey_edit.txt", na.strings="")

# look at first 6 rows of dat
head(dat)

# figure out the class of 'dat'
class(dat)

# note: 'dat' is a data frame

# read in data (using the 'tidyverse' way)
dat <- read_delim("data_survey_edit.txt", delim="\t")

# note: this uses readr (https://readr.tidyverse.org)

# look at first 6 rows of dat
head(dat)

# figure out the class of 'dat'
class(dat)

# note: 'dat' is a a tibble (https://tibble.tidyverse.org)

# basic idea of the difference between nested functions and piping
sqrt(log(5))
5 %>% log %>% sqrt

# note: the pipe operator comes from magrittr (https://magrittr.tidyverse.org)

# select a variable (note: this returns a tibble / data.frame)
dat %>% select(pss)

# pull out a variable (note: this returns a vector)
dat %>% pull(pss)

# could also do it this way, but this is unncessarily complex
dat %>% select(pss) %>% pull()

# pull out a variable and take the mean of it
dat %>% pull(pss) %>% mean(na.rm=TRUE)

# the 'base R' way of doing the same thing
mean(dat$pss, na.rm=TRUE)

# compute the mean of the male and female subjects
dat %>% filter(sex == "male")   %>% pull(pss) %>% mean(na.rm=TRUE)
dat %>% filter(sex == "female") %>% pull(pss) %>% mean(na.rm=TRUE)

# the 'base R' way of doing the same thing
mean(dat$pss[dat$sex == "male"],   na.rm=TRUE)
mean(dat$pss[dat$sex == "female"], na.rm=TRUE)

# another way using the subset() function
mean(subset(dat, sex == "male",   select = pss, drop = TRUE), na.rm=TRUE)
mean(subset(dat, sex == "female", select = pss, drop = TRUE), na.rm=TRUE)

# yet another way
by(dat$pss, dat$sex, mean, na.rm=TRUE)

# the tidyverse approach to the by() example
dat %>% group_by(sex) %>% summarise(meanpss = mean(pss, na.rm=TRUE))

# the result is a tibble where values are printed with lots of rounding;
# could pipe this further to as.data.frame() to turn it into a plain old
# data frame where these numbers are printed with less rounding
dat %>% group_by(sex) %>% summarise(meanpss = mean(pss, na.rm=TRUE)) %>% as.data.frame()

# if we just need the means directly, can pull them out again with pull()
dat %>% group_by(sex) %>% summarise(meanpss = mean(pss, na.rm=TRUE)) %>% pull(meanpss)

# to get rid of that message wrt to ungrouping and put each step on its own line
dat %>%
   group_by(sex) %>%
   summarise(meanpss = mean(pss, na.rm=TRUE), .groups="drop") %>%
   pull(meanpss)

# remove the 'pss' variable from dat
dat$pss <- NULL

# compute the sum of the 10 PSS variables (the 'base R' way)
#dat$pss <- dat$pss1 + dat$pss2 + ...
#dat$pss <- with(dat, pss1 + pss2 + ...)
dat$pss <- rowSums(dat[grep("pss", names(dat))])

# remove 'pss' variable from dat (the tidyverse way)
dat <- dat %>% select(-pss)

# compute the sum of the pss variables and add it to the tibble as a new variable
dat$pss <- dat %>% select(starts_with("pss")) %>% mutate(pss = rowSums(.)) %>% pull(pss)

# remove 'pss' variable again
dat <- dat %>% select(-pss)

# could also do it this way, but now we again have multiple nested functions
dat <- dat %>% mutate(pss = rowSums(select(.,starts_with("pss"))))

# make a copy of dat
tmp <- dat

# look at first 6 rows of tmp
head(tmp)

# reorder data frame by increasing age (the 'base R' way)
tmp <- tmp[order(tmp$age),]

# look at first 6 rows of tmp
head(tmp)

# make a copy of dat
tmp <- dat

# look at first 6 rows of tmp
head(tmp)

# reorder data frame by increasing age the tidyverse way
tmp <- arrange(tmp, age)

# look at first 6 rows of tmp
head(tmp)

# or with piping
tmp <- dat
tmp <- tmp %>% arrange(age)
head(tmp)

############################################################################

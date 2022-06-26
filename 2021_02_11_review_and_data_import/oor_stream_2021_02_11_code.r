############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-02-11
#
# Topic(s):
# - review of some basics from the Intro to R course
# - data import
#
# last updated: 2021-02-11

############################################################################

# set working directory (in RStudio, 'Session' menu, 'Set Working Directory',
# 'To Source File Location'; then copy-paste setwd() command below)
#setwd("...")

############################################################################

# read in plain text data

# read in csv data
dat <- read.csv("example_data_csv.csv", na.strings="", as.is=TRUE)
dat

# read in tab delimited data
dat <- read.table("example_data_tabdelim.txt", header=TRUE, sep="\t", na.strings="", as.is=TRUE)
dat

# could also use read.delim()
dat <- read.delim("example_data_tabdelim.txt", na.strings="", as.is=TRUE)
dat

############################################################################

# read in Excel data

#install.packages("readxl")
library(readxl)

# read in Excel data
dat <- read_excel("example_data_excel.xlsx")
dat

dat <- data.frame(dat)
dat

############################################################################

# read in SPSS, Stata, and SAS files

#install.packages("haven")
library(haven)

# read in SPSS data
dat <- read_spss("example_data_spss.sav")
dat

dat <- data.frame(dat)
dat

dat$var2 <- ifelse(dat$var2 == 1, "yes", "no")
dat

dat$var3[dat$var3 == ""] <- NA
dat

# read in Stata data
dat <- read_stata("example_data_stata.dta")
dat

dat <- data.frame(dat)
dat

dat$var3[dat$var3 == ""] <- NA
dat

############################################################################

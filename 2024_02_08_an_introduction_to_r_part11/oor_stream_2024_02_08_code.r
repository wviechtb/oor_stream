############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-02-08
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 11.6
#
# last updated: 2024-02-09

############################################################################

## 11.6 Generalized linear models

# copy the mtcars dataset to dat and inspect the dataset

dat <- mtcars
dat

# fit a linear regression model using lm() predicting mpg (the gas mileage in
# miles per gallon) from hp (horsepower) and 'am' (whether the car has an
# 0 = automatic or 1 = manual transmission)

res <- lm(mpg ~ hp + am, data=dat)
summary(res)

# fit the same model using glm()

res <- glm(mpg ~ wt + am, family=gaussian, data=dat)
summary(res)

# the results are the same, but fitting the model with lm() is more efficient
# and some additional statistics are reported (e.g., R^2) in the output

############################################################################

# read in the data from data_heart.dat

dat <- read.table("data_heart.dat", header=TRUE, sep="\t", as.is=TRUE)

# inspect the first 20 rows

head(dat, 20)

# dataset from: https://www.statlearning.com/resources-second-edition
# (with minor adjustments to make it a bit easier to work with)
#
# The dataset includes 303 patients and the following variables:
#
# id          - subject id
# age         - age (in years)
# sex         - sex (0 = female, 1 = male)
# chestpain   - chest pain type (typical, nontypical, nonanginal, asymptomatic)
# restbp      - resting blood pressure (in mm Hg on admission to the hospital)
# chol        - serum cholesterol (in mg/dl)
# fbs         - fasting blood sugar > 120 mg/dl (0 = false, 1 = true)
# restecg     - resting electrocardiographic result (0 = normal, 1 = some abnormality)
# maxhr       - maximum heart rate achieved (in bpm)
# exang       - exercise induced angina (0 = no, 1 = yes)
# oldpeak     - ST depression induced by exercise relative to rest
# slope       - slope of the peak exercise ST segment (1 = upsloping, 2 = flat, 3 = downsloping)
# ca          - number of major vessels colored by fluoroscopy (0-3)
# thal        - Thallium stress test result (normal, fixed, or reversable)
# ahd         - have angiographic heart disease or not (no, yes)
#
# The purpose of the dataset was to study whether the presence/absence of
# angiographic heart disease (variable 'ahd') can be predicted based on the
# other variables (and if so, how well). The specific meaning of all these
# variables is not that impotant -- we will just use this dataset for
# illustrating a logistic regression model.

# recode ahd into a 0/1 variable

dat$ahd <- ifelse(dat$ahd == "yes", 1, 0)

# fit a logistic regression model predicting ahd (but see below) from several
# other variables in the dataset

res <- glm(ahd ~ sex + age + chol, family=binomial, data=dat)
summary(res)

# the dependent variable here is a 0/1 variable, which is assumed to have a
# binomial distribution, which, as a special case, is actually a Bernoulli
# distribution (https://en.wikipedia.org/wiki/Bernoulli_distribution); the
# mean of such a distribution is p, where p is the probability of seeing a 1;
# the model says that m^{-1}(p) = beta0 + beta1*x1 + beta2*x2 + ..., but what
# is this function m() and its inverse m^{-1}()? for family=binomial, m^{-1}()
# is by default the 'logit' function, which is given by log(p/(1-p)), so the
# model says that the logit-transformed probability of a 1 is a linear
# function of one or multiple predictors (sidenote: p/(1-p) are the so-called
# 'odds' of seeing a 1 and hence log(p/(1-p)) are the so-called 'log odds');
# so here, the default 'link function' is the logit transformation

# we can see what the default link is for a particular 'family' under the
# following help file (and what other options are available)

help(family)

# to illustrate, say p=0.7, then the logit-transformed value is as follows

log(0.7 / (1 - 0.7))

# which can also be computed with qlogis() (i.e., eta = m^{-1}() = qlogis())

qlogis(0.7)

# note that qlogis() maps probabilities between 0 and 1 to the real line
# (i.e., to minus to plus infinity); for example:

qlogis(0)
qlogis(0.01)
qlogis(0.5)
qlogis(0.99)
qlogis(1)

# so based on the model, we can compute predicted log odds (of having
# angiographic heart disease); for example, for 56 year-old males with a
# cholesterol level of 260, the predicted log odds are as follows

predict(res, newdata=data.frame(sex=1, age=56, chol=260))

# in the notation explained in this section, this value is eta (or more
# precisely, eta with a hat on top of it, since it is a predicted value); if
# we want the (estimated/predicted) probability, we need to back-transform
# this, so we need to apply m() to this value, which we can do using the
# plogis() function (so then we are getting p = m(eta))

plogis(predict(res, newdata=data.frame(sex=1, age=56, chol=260)))

# so the model predicts that there is a 0.61 probability of ahd (since ahd is
# something we would like to avoid, it is common to call this probability also
# the 'risk' of having ahd)

# note: plogis() maps values between minus and plus infinity to 0 and 1)

plogis(-Inf)
plogis(-3)
plogis(0)
plogis(3)
plogis(Inf)

# so, by using the logit link (and its corresponding back-transformation), we
# are guaranteed that a predicted probability is always a value between 0 and
# 1 (which is good, since that is the range for probabilities)

# we can get the predicted probability (risk) directly with predict()

predict(res, newdata=data.frame(sex=1, age=56, chol=260), type="response")

# how can we interpret the estimated model coefficients?

summary(res)

# let's start with the intercept

coef(res)[[1]]

# the intercept corresponds to the estimated log odds of ahd when sex=0,
# age=0, and chol=0, which we can turn into the predicted risk with plogis()

plogis(coef(res)[[1]])

# but of course we shouldn't interpret this value because we are extrapolating
# beyond the range of our data (one could center 'age' and 'chol' at some more
# meaningful values, so that the intercept is also more sensible)

# now let's look at the coefficient for sex

coef(res)[[2]]

# this estimates how the log odds of ahd change for a one-unit increase in sex
# (i.e., the difference in log odds when sex = x + 1 versus when sex = x);
# since sex is a dummy variable, we are therefore getting the difference
# between the log odds for males (sex=1) versus females (sex=0)

# for example, the predicted log odds for males and females (when age=56 and
# chol=260 are held constant) are as follows

coef(res)[[1]] + coef(res)[[2]] * 1 + coef(res)[[3]] * 56 + coef(res)[[4]] * 260
coef(res)[[1]] + coef(res)[[2]] * 0 + coef(res)[[3]] * 56 + coef(res)[[4]] * 260

# and the difference between those two is the coefficient for sex

(coef(res)[[1]] + coef(res)[[2]] * 1 + coef(res)[[3]] * 56 + coef(res)[[4]] * 260) -
(coef(res)[[1]] + coef(res)[[2]] * 0 + coef(res)[[3]] * 56 + coef(res)[[4]] * 260)

coef(res)[[2]]

# note that it does not matter what the age and chol values above are (as long
# as we are holding these values constant for males and females)

# the predicted risks for males and females (with age=56 and chol=260) are

plogis(coef(res)[[1]] + coef(res)[[2]] * 1 + coef(res)[[3]] * 56 + coef(res)[[4]] * 260)
plogis(coef(res)[[1]] + coef(res)[[2]] * 0 + coef(res)[[3]] * 56 + coef(res)[[4]] * 260)

# remember that we can also use predict() to get these values

predict(res, newdata=data.frame(sex=1, age=56, chol=260), type="response")
predict(res, newdata=data.frame(sex=0, age=56, chol=260), type="response")

# so males are much more likely to have ahd compared to females in this
# dataset; we can also compute the difference between these risks

predict(res, newdata=data.frame(sex=1, age=56, chol=260), type="response") -
predict(res, newdata=data.frame(sex=0, age=56, chol=260), type="response")

# however, it is not common to report results in this way; unfortunately, when
# computing risk differences in this way, the age and chol values do matter

predict(res, newdata=data.frame(sex=1, age=46, chol=260), type="response") -
predict(res, newdata=data.frame(sex=0, age=46, chol=260), type="response")

# what is typically reported in logistic regression is not such risk
# differences, but the ratio of the odds (i.e., the odds ratio)

# consider a male with age=56 and chol=260; we can compute the predicted
# probability and turn this into the predicted odds

p1 <- predict(res, newdata=data.frame(sex=1, age=56, chol=260), type="response")
p1 / (1 - p1)

# and now let's do the same for a female

p0 <- predict(res, newdata=data.frame(sex=0, age=56, chol=260), type="response")
p0 / (1 - p0)

# the ratio of these two odds is the odds ratio

(p1 / (1 - p1)) / (p0 / (1 - p0))

# so the odds of ahd are more than 5 times larger for males compared to females

# it turns out that we can get this odds ratio if we simply exponentiate the
# coefficient for sex

exp(coef(res)[[2]])

# while it may not seem as natural to report results in this manner, the
# advantage is that the odds ratio stays constant if we change the values of
# the other variables (still holding them constant for males and females)

p1 <- predict(res, newdata=data.frame(sex=1, age=46, chol=260), type="response")
p0 <- predict(res, newdata=data.frame(sex=0, age=46, chol=260), type="response")
(p1 / (1 - p1)) / (p0 / (1 - p0))

# we can do the same thing with the coefficient for age

exp(coef(res)[[3]])

# so the odds of ahd are 1.06 times larger when age goes up by one year

# a one-year difference is not that much, but an additional 10 years might
# have a bigger impact; we can just multiply the coefficient for age by 10 and
# then exponentiate this value

exp(coef(res)[[3]] * 10)

# so the odds of ahd are 1.9 times larger when age goes up by 10 years

############################################################################

# we will skip the example given in this section for now (the "small,
# artificial example") and instead consider a different type of GLM, namely a
# Poisson regression model; this is often used when we have an outcome that is
# a count of something; then we might assume that this variable follows a
# Poisson distribution (https://en.wikipedia.org/wiki/Poisson_distribution)

# consider the following dataset

dat <- InsectSprays
dat

# the count variable indicates the number of inspects (caterpillars of the
# five-spotted hawkmoth; https://en.wikipedia.org/wiki/Manduca_quinquemaculata)
# found on agricultural plots treated with different types of insecticides

# fit a Poisson regression model using spray as a categorical predictor

res <- glm(count ~ spray, family=poisson, data=dat)
summary(res)

# the default link for family=poisson is the log link; so in this case, we are
# modeling the mean of the Poisson distribution (which is often denoted as
# lambda) as log(lambda) = beta0 + beta1*x1 + beta2*x2 + ...

# the estimated log-transformed mean for spray type A is just the intercept

coef(res)[[1]]

# hence, the estimated mean count for spray type A is

exp(coef(res)[[1]])

# the estimated log-transformed mean for spray type B is the intercept plus
# the coefficient for sprayB

coef(res)[[1]] + coef(res)[[2]]

# hence, the estimated mean count for spray type B is

exp(coef(res)[[1]] + coef(res)[[2]])

# we can again use the predict function to compute these predicted mean counts
# (for all spray types) as follows

predict(res, newdata=data.frame(spray=c("A","B","C","D","E","F")), type="response")

# fit the reduced model that assumes that the mean count does not depend on
# the spray type and then compare this model against the one above

res0 <- glm(count ~ 1, family=poisson, data=dat)
anova(res0, res, test="Chisq")

# note that the predicted mean counts above are the same as the mean counts
# for the different spray types as observed in our data

by(dat$count, dat$spray, mean)

# in a Poisson distribution, the mean is equal to the variance (see the
# Wikipedia link above); but if we compute the variances of the counts for the
# different spray types, we see that the variances are consistently above the
# means (except for type E); this is called 'overdispersion'

tab <- data.frame(mean     = by(dat$count, dat$spray, mean),
                  variance = by(dat$count, dat$spray, var))
tab$ratio <- tab$variance / tab$mean
tab

# this violates an assumption of the Poisson distribution; we can relax this
# assumption with the quasi-Poisson family, which allows the variance of the
# counts to differ from the means by a multiplicative factor

res2 <- glm(count ~ spray, family=quasipoisson, data=dat)
summary(res2)

# this factor (1.507713) is just the average of the ratios we saw above

mean(tab$ratio)

# note that the coefficients have not changed, since we still want to get the
# same predicted mean counts (which match the observed means); however, the
# standard errors are now larger, to reflect the increased uncertainty in the
# estimates due to the larger variances; in fact, the standard errors are
# increased by a factor that is equal to the square root of the overdispersion
# parameter

coef(summary(res2))[,"Std. Error"] / coef(summary(res))[,"Std. Error"]
sqrt(mean(tab$ratio))

############################################################################

# now let's turn to the example given in the manual (but rename x to age)

dat <- data.frame(age = c(20,35,45,55,70),
                  n   = rep(50,5),
                  y   = c(6,17,26,37,44))
dat

# this is a dataset given in aggregated form (so instead of 50 rows for each
# of the age groups, we just have a single row with the number of cases)

# fit a logistic regression model predicting the log odds of blindness based
# on age; here, the dependent variable is a matrix with two columns, giving
# the number of cases and non-cases

res1 <- glm(cbind(y,n-y) ~ age, family=binomial, data=dat)
summary(res1)

# for age values 20-70, compute the predicted probability of blindness

ages <- 20:70

pred1 <- predict(res1, newdata=data.frame(age=ages), type="response")
pred1

# we can take these probabilities and multiply them by 50 to get the predicted
# number of blind individuals within each age group

pred1 * 50

# turn this into a plot and add the predicted numbers to the plot

plot(y ~ age, data=dat, pch=21, bg="gray", cex=1.5, bty="l", ylim=c(0,50),
     xlab="Age", ylab="Number of Blind Individuals out of 50")

lines(ages, pred1*50, lwd=2)

# we could also treat the y values as Poisson distributed counts (note that
# this model only makes sense since there are exactly 50 individuals in each
# age group)

res2 <- glm(y ~ age, family=poisson, data=dat)
summary(res2)

# for age values 20-70, compute the predicted counts and add these as a line
# to the plot above

pred2 <- predict(res2, newdata=data.frame(age=ages), type="response")
pred2

lines(ages, pred2, lwd=2, col="red")

# instead of assuming that log(mean) is a function of age for the Poisson
# model, we could also assume that sqrt(mean) is a function of age; in other
# words, we could use a square root link

res3 <- glm(y ~ age, family=poisson(link=sqrt), data=dat)
summary(res3)

# again get the predicted counts and add these as a line to the plot

pred3 <- predict(res3, newdata=data.frame(age=ages), type="response")
pred3

lines(ages, pred3, type="l", lwd=2, col="blue")

legend("topleft", bty="n", lwd=2, col=c("black", "red", "blue"),
       legend=c("Binomial model", "Poisson model (log link)", "Poisson model (sqrt link)"))

# draw the points again so they are not overlapped by lines

points(y ~ age, data=dat, pch=21, bg="gray", cex=1.5)

############################################################################

# as we saw above, changing the link function changes the results; for the
# binomial model, this can lead to some interesting alternative model types

# logistic regression model (using a logit link)

res <- glm(cbind(y,n-y) ~ age, family=binomial, data=dat)
summary(res)

# estimated odds ratio (so the odds of blindness are 1.08 times greater when
# age goes up by one year)

exp(coef(res)[[2]])

# relative risk regression (using a log link)

res <- glm(cbind(y,n-y) ~ age, family=binomial(link=log), data=dat)
summary(res)

# note: when fitting such a model, we assume that log(p) is a function of one
# or multiple predictors; get the predicted log(p) values for age values 20-70

predict(res, newdata=data.frame(age=ages))

# but there is no guarantee here that when we back-transform these values
# (through exponentiation) that the predicted risks are between 0 and 1

predict(res, newdata=data.frame(age=ages), type="response")

# we get lucky that this is not a problem, but can fail in other cases

# when we fit such a model, then the exponentiated coefficient corresponds to
# the estimated risk ratio (so the risk of blindness is 1.03 times greater
# when age goes up by one year)

exp(coef(res)[[2]])

# absolute risk regression (using an identity link; for this to work, one has
# to give the link in quotes)

res <- glm(cbind(y,n-y) ~ age, family=binomial(link="identity"), data=dat)
summary(res)

# now we assume that p is directly a function of one or more predictors;
# again, this can fail if the model yields predicted probabilities below 0 or
# above 1 (luckily, this is again not a problem)

predict(res, newdata=data.frame(age=ages))

# here, the coefficient directly indicates how the risk changes for a one unit
# increase in age

coef(res)[[2]]

############################################################################

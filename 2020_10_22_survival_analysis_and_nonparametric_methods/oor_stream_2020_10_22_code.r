############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2020-10-22
#
# Topic(s):
# - survival analysis
# - non-parametric methods
#
# last updated: 2020-10-22

############################################################################

# restart the R session (Menu 'Session' - Restart R)

############################################################################

# survival analysis
# https://en.wikipedia.org/wiki/Survival_analysis

library(survival)

leukemia

help(leukemia)

# Kaplan-Meier estimator / plot
# https://en.wikipedia.org/wiki/Kaplan–Meier_estimator

with(leukemia, Surv(time, status))

res <- survfit(Surv(time, status) ~ x, data = leukemia)
summary(res)

# Kaplan-Meier plot with 95% CI

plot(res, lty = c("solid", "dotted"),
     xlab = "Month", ylab = "Survival Proportion")
legend("topright", inset=.02, c("Maintained","Nonmaintained"),
       lty = c("solid","dotted"))

# Kaplan-Meier plot with 95% CI

plot(res, col=c("blue","red"),
     xlab = "Month", ylab = "Survival Proportion")
lines(res, conf.int=TRUE, col=c("blue","red"), lty="dotted") # add confidence intervals
legend("topright", inset=.02, c("Maintained","Nonmaintained"),
       col = c("blue","red"), lwd=1, bg="white")

# log-rank test
# https://en.wikipedia.org/wiki/Logrank_test

survdiff(Surv(time, status) ~ x, data = leukemia)

# Cox proportional hazards model
# https://en.wikipedia.org/wiki/Proportional_hazards_model

res <- coxph(Surv(time, status) ~ x , data = leukemia)
summary(res)

# relevel x

leukemia$x
leukemia$x <- relevel(leukemia$x, ref="Nonmaintained")
leukemia$x

res <- coxph(Surv(time, status) ~ x, data = leukemia)
summary(res)

# predicted survivor function
# https://en.wikipedia.org/wiki/Survival_function

newdat <- data.frame(x=c("Nonmaintained","Maintained"))
pred <- summary(survfit(res, newdata = newdat))
pred

plot(pred$surv[,1] ~ pred$time, ylim=c(0,1), type="s", col="red",
     xlab = "Month", ylab = "Predicted Survival")
lines(pred$surv[,2] ~ pred$time, type="s", col="blue")
legend("topright", legend=c("Maintained","Nonmaintained"),
       lty="solid", col=c("blue","red"), inset=.02)

# testing the proportional hazards assumption

cox.zph(res)

############################################################################

# some non-parametric methods

rm(list=ls())

# read in data

load("data_survey_edit.rdata")

# Pearson correlation between age and stress

cor(dat$age, dat$pss, use="complete.obs")

# Spearman rank correlation coefficient between age and stress
# https://en.wikipedia.org/wiki/Spearman's_rank_correlation_coefficient

cor(dat$age, dat$pss, use="complete.obs", method="spearman")

cor.test(dat$age, dat$pss, method="spearman")

# t-test comparing the mean stress level of men versus women

t.test(pss ~ sex, data=dat, var.equal=TRUE)

# Wilcoxon rank-sum test / Mann–Whitney U test
# https://en.wikipedia.org/wiki/Mann–Whitney_U_test

wilcox.test(pss ~ sex, data=dat)

# note: the Mann–Whitney U test is NOT a test of a difference in medians (this
# is only true under the restrictive assumptions)

# an example where the medians are the same, but the test is significant

grp1 <- rep(c(-2, 0,  5), each = 20)
grp2 <- rep(c(-1, 0, 10), each = 20)
grp1
grp2

median(grp1)
median(grp2)

wilcox.test(grp1, grp2)

rm(grp1, grp2)

# one-way ANOVA comparing the mean stress level of the different marital status groups

res <- aov(pss ~ marital, data=dat)
summary(res)

# Kruskal-Wallis rank sum test
# https://en.wikipedia.org/wiki/Kruskal–Wallis_one-way_analysis_of_variance

kruskal.test(pss ~ marital, data=dat)

# follow-up test comparing two specific groups

tmp <- subset(dat, marital == "single" | marital == "divorced")
wilcox.test(pss ~ marital, data=tmp)

# can do this for all 28 (8*7/2) pairs (tedious!); may want to use
# a correction for multiple testing (such as the Bonferroni correction)

# Friedman rank sum test: friedman.test()
# https://en.wikipedia.org/wiki/Friedman_test

############################################################################

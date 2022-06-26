############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-07-15
#
# Topic(s):
# - meta-analysis (part 11: longitudinal models and network meta-analysis)
#
# last updated: 2021-07-16

############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

############################################################################

# copy data into 'dat' and examine data
dat <- dat.ishak2007
dat

# fit a random-effects model for each timepoint
rma(y1i, v1i, data=dat)
rma(y2i, v2i, data=dat)
rma(y3i, v3i, data=dat)
rma(y4i, v4i, data=dat)

# create long format dataset
dat.long <- reshape(dat, direction="long", idvar="study", v.names=c("yi","vi"),
                    varying=list(c(2,4,6,8), c(3,5,7,9)))
dat.long <- dat.long[order(dat.long$study, dat.long$time),]
rownames(dat.long) <- NULL
dat.long

# remove missing measurement occasions from dat.long
is.miss  <- is.na(dat.long$yi)
dat.long <- dat.long[!is.miss,]
rownames(dat.long) <- NULL
head(dat.long, 10)

# construct the full (block diagonal) V matrix with an AR(1) structure
rho.within <- .97 # value as used by Ishak et al. (2007)
V <- lapply(split(with(dat, cbind(v1i, v2i, v3i, v4i)), dat$study), diag)
V <- lapply(V, function(v) sqrt(v) %*% toeplitz(ARMAacf(ar=rho.within, lag.max=3)) %*% sqrt(v))
V <- bldiag(V)
V <- V[!is.miss,!is.miss] # remove missing measurement occasions from V
V[1:10,1:10]

# don't need the wide format data anymore
dat <- dat.long

# plot data
with(dat, interaction.plot(time, study, yi, type="b", pch=19, lty="solid", xaxt="n",
               legend=FALSE, xlab="Time Point", ylab="Mean Difference", bty="l"))
axis(side=1, at=1:4, lab=c("1 (3 months)", "2 (6 months)", "3 (12 months)", "4 (12+ months)"))

# multivariate model with an AR(1) structure for the true effects
res1 <- rma.mv(yi, V, mods = ~ factor(time) - 1, random = ~ time | study,
               struct = "AR", data = dat)
print(res1, digits=2)

# multivariate model with a heteroscedastic AR(1) structure for the true effects
res2 <- rma.mv(yi, V, mods = ~ factor(time) - 1, random = ~ time | study,
               struct = "HAR", data = dat)
print(res2, digits=2)

# LRT comparing the two different models
anova(res1, res2)

# the model that allows for different tau^2 values at the 4 measurement
# occasions (res2) does not fit significantly better than the model that
# assumes a single tau^2 value for all measurement occasions

# model with a CS (compound symmetry) structure (note: struct="CS" is actually the default)
res0 <- rma.mv(yi, V, mods = ~ factor(time) - 1, random = ~ factor(time) | study,
               struct = "CS", data = dat)
print(res0, digits=2)

# LRT comparing models res1 and res0
anova(res0, res1) # cannot do this
fitstats(res0, res1) # but we still can compare the information criteria

# the AIC, BIC, and AICc values are lower for res1; hence, the information
# criteria suggest that the model with the AR(1) structure is preferable
# (although the difference in the values is not that large)

# could even consider adding an observation level random effect to the model
dat$id <- 1:nrow(dat)
res3 <- rma.mv(yi, V, mods = ~ factor(time) - 1, random = list(~ time | study, ~ 1 | id),
               struct = "AR", data = dat)
print(res3, digits=2)

# LRT comparing models res1 and res3
anova(res1, res3)

# the addition of the observation level random effect does not lead to a
# significant improvement in the fit of the model

# but the measurement occasions are not equally spaced; so let's create a
# variable that reflects the exact time of the 4 measurement occasions (in
# months), assuming that the long-term follow-up occurred at 18 months
dat$months <- dat$time * 3
dat$months <- ifelse(dat$months == 12, 18, dat$months)
dat$months <- ifelse(dat$months ==  9, 12, dat$months)
dat

# model with CAR structure
res4 <- rma.mv(yi, V, mods = ~ factor(time) - 1, random = ~ months | study,
               struct = "CAR", data = dat)
print(res4, digits=2)

# the autocorrelation coefficient rho reflects the correlation for a lag of
# one month; based on this, we can compute the autocorrelation for longer (or
# shorter) lags (note: 18 - 3 = 15 is the longest lag in these data)
lags <- seq(1, 15, length=100)
plot(lags, res4$rho^lags, type="l", lwd=3)

# estimate/test the difference in the effect at the 3- and 6-month follow-ups
predict(res4, newmod=c(1,-1,0,0))
anova(res4, L=c(1,-1,0,0))

# model allowing the moderating effect of disease duration to differ at the 4 timepoints
res5 <- rma.mv(yi, V, mods = ~ factor(time) + factor(time):mdur - 1, random = ~ months | study,
               struct = "CAR", data = dat)
print(res5, digits=2)

# some relevant readings:
#
# Ishak, K. J., Platt, R. W., Joseph, L., Hanley, J. A., & Caro, J. J. (2007).
# Meta-analysis of longitudinal studies. Clinical Trials, 4(5), 525-539.
# https://doi.org/10.1177/1740774507083567
#
# Musekiwa, A., Manda, S. O., Mwambi, H. G., & Chen, D. G. (2016).
# Meta-analysis of effect sizes reported at multiple time points using general
# linear mixed model. PLOS ONE, 11(10), e0164898.
# https://doi.org/10.1371/journal.pone.0164898

############################################################################

# copy data into 'dat'
dat <- dat.bcg
dat

# calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat)
dat

# random-effects model
res <- rma(yi, vi, data=dat)
res

# restructure dataset into long format
dat <- to.long(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat)
dat <- dat[c(1:3,12:15)]
dat

# calculate log risks and corresponding sampling variances
dat <- escalc(measure="PLN", xi=out1, mi=out2, data=dat)
dat

# fit bivariate model with fixed study effects, a random group (i.e., treatment)
# effect, and fix the value of rho = 1/2
res <- rma.mv(yi, vi, mods = ~ factor(study) + group - 1,
              random = ~ group | study, rho=1/2, data=dat)
res

# note: results are the same as for the standard random-effects model
#
# why set rho=1/2?
#
# Var(theta_t - theta_c) = Var(theta_t) + Var(theta_c) - 2*rho*SD(theta_t)*SD(theta_c)
#                        = tau^2        + tau^2        - 2*rho*tau*tau
#                        = 2*tau^2 - 2*rho*tau^2
#                        = 2*(1-rho)*tau^2
#                        = tau^2 when rho = 1/2
#
# this way, tau^2 from the model reflects the amount of heterogeneity in the
# underlying treatment effects (i.e., the difference between the log risk in
# a treatment group and its corresponding control group)
#
# hence, the standard random-effects model can also be thought of as a
# multilevel/multivariate model with fixed study effects and a random
# group/treatment effect

# a model with random study effects
res <- rma.mv(yi, vi, mods = ~ group, random = list(~ 1 | study, ~ group | study), rho=1/2, data=dat)
res

# same as this
res <- rma.mv(yi, vi, mods = ~ group, random = ~ group | study, data=dat)
res
res <- rma.mv(yi, vi, mods = ~ group, random = ~ 1 | study/group, data=dat)
res

############################################################################

# copy data into 'dat'
dat <- dat.hasselblad1998
dat

# install the igraph package
#install.packages("igraph")

# load the igraph package
library(igraph)

# create network graph
pairs <- data.frame(do.call(rbind,
   sapply(split(dat$trt, dat$study), function(x) t(combn(x,2)))), stringsAsFactors=FALSE)
lvls <- c("no_contact", "self_help", "ind_counseling", "grp_counseling")
pairs$X1 <- factor(pairs$X1, levels=lvls)
pairs$X2 <- factor(pairs$X2, levels=lvls)
tab <- table(pairs[,1], pairs[,2])
tab # adjacency matrix
g <- graph_from_adjacency_matrix(tab, mode = "plus", weighted=TRUE, diag=FALSE)
vertex_attr(g, "name") <- c("No Contact", "Self-Help",
                            "Individual\nCounseling", "Group\nCounseling")
plot(g, edge.curved=FALSE, edge.width=E(g)$weight, layout=layout_on_grid,
     vertex.size=45, vertex.color="lightgray", vertex.label.color="black", vertex.label.font=2)

# calculate log odds for each study arm
dat <- escalc(measure="PLO", xi=xi, ni=ni, add=1/2, to="all", data=dat)
dat

# convert trt variable to factor with desired ordering of levels
dat$trt <- factor(dat$trt, levels=c("no_contact", "self_help", "ind_counseling", "grp_counseling"))

# add a space before each level (this makes the output a bit more legible)
levels(dat$trt) <- paste0(" ", levels(dat$trt))

# network meta-analysis using an arm-based model with fixed study effects
# by setting rho=1/2, tau^2 reflects the amount of heterogeneity for all treatment comparisons
res <- rma.mv(yi, vi, mods = ~ factor(study) + trt - 1,
              random = ~ trt | study, rho=1/2, data=dat, btt="trt")
res

# all pairwise odds ratios of interventions versus no contact
predict(res, newmods=cbind(matrix(0, nrow=3, ncol=24), diag(3)), transf=exp, digits=2)

# all pairwise odds ratios comparing interventions (ic vs sh, gc vs sh, and gc vs ic)
predict(res, newmods=cbind(matrix(0, nrow=3, ncol=24), rbind(c(-1,1,0), c(-1,0,1), c(0,-1,1))), transf=exp, digits=2)

# forest plot of ORs of interventions versus no contact
forest(c(0,res$beta[25:27]), sei=c(0,res$se[25:27]), psize=1, xlim=c(-3,4), digits=c(2,1), efac=2,
       slab=c("No Contact", "Self-Help", "Individual Counseling", "Group Counseling"),
       atransf=exp, at=log(c(.5, 1, 2, 4, 8)), xlab="Odds Ratio for Intervention vs. No Contact",
       header=c("Intervention", "Odds Ratio [95% CI]"))

# restructure dataset to a contrast-based format
dat <- to.wide(dat.hasselblad1998, study="study", grp="trt", ref="no_contact", grpvars=6:7)

# calculate log odds ratios for each treatment comparison
dat <- escalc(measure="OR", ai=xi.1, n1i=ni.1,
                            ci=xi.2, n2i=ni.2, add=1/2, to="all", data=dat)
dat

# calculate the variance-covariance matrix of the log odds ratios for multiarm studies
# see Gleser & Olkin (2009), equation (19.11), for the covariance equation
calc.v <- function(x) {
   v <- matrix(1/(x$xi.2[1] + 1/2) + 1/(x$ni.2[1] - x$xi.2[1] + 1/2), nrow=nrow(x), ncol=nrow(x))
   diag(v) <- x$vi
   v
}
V <- bldiag(lapply(split(dat, dat$study), calc.v))
V[1:5,1:5]

# add contrast matrix to dataset
dat <- contrmat(dat, grp1="trt.1", grp2="trt.2")
dat

# network meta-analysis using a contrast-based random-effects model
# by setting rho=1/2, tau^2 reflects the amount of heterogeneity for all treatment comparisons
res <- rma.mv(yi, V, mods = ~ self_help + ind_counseling + grp_counseling - 1,
              random = ~ comp | study, rho=1/2, data=dat)
res

# predicted odds ratios of interventions versus no contact
predict(res, newmods=diag(3), transf=exp, digits=2)

# predicted odds ratios of the three interventions against each other
predict(res, newmods=rbind(c(-1,1,0),c(-1,0,1),c(0,-1,1)), transf=exp, digits=2)

# can separate the QE-statistic into a test for heterogeneity (QE) and a test for inconsistency (QM)
inc <- rma.mv(yi, V, mods = ~ self_help + ind_counseling + grp_counseling +
                              (self_help + ind_counseling + grp_counseling):design - 1,
              data=dat, btt="design")
inc
inc$QE + inc$QM
res$QE

# direct evidence only for self-help versus no contact (note: fixing tau^2 to
# the value from the network meta-analysis)
dir <- rma(yi, vi, data=dat, subset=comp=="se-no", tau2=res$tau2)
dir

# indirect evidence only for self-help versus no contact (note: fixing tau^2
# to the value from the network meta-analysis)
ind <- rma.mv(yi, V, mods = ~ self_help + ind_counseling + grp_counseling - 1,
              random = ~ comp | study, rho=1/2, data=dat, subset=comp!="se-no", tau2=res$tau2)
ind

# combine the direct and indirect evidence assuming they are consistent with
# each other; note: this gives the same result as the network meta-analysis
rma(c(coef(dir), coef(ind)[1]), c(vcov(dir), vcov(ind)[1,1]), method="FE")

# the Q-test above is identical to testing the difference between the direct
# and indirect evidence; we can also test this directly as follows
rma(c(coef(dir), coef(ind)[1]), c(vcov(dir), vcov(ind)[1,1]), mods = c(0,1), method="FE")

# note: this approach (where we obtain the direct and indirect evidence and
# then combine it) always works when there are no 'multiarm' studies (i.e., no
# studies with more than two arms) or no multiarm studies have examined the
# contrast we are interested in -- which is the case in the example above (the
# two multiarm studies, 2 and 9, did not examine self-help vs. no contact)

# fit random inconsistency effects model (see Law et al., 2016)
inc <- rma.mv(yi, V, mods = ~ self_help + ind_counseling + grp_counseling - 1,
              random = list(~ comp | study, ~ comp | design), rho=1/2, phi=1/2, data=dat)
inc

# LRT comparing the random inconsistency effects model with the model assuming
# consistency (note: since the estimate of gamma^2 = 0, we get a p-value of 1)
anova(res, inc)

# some relevant readings:
#
# Salanti, G., Higgins, J. P. T., Ades, A. E., & Ioannidis, J. P. A. (2008).
# Evaluation of networks of randomized trials. Statistical Methods in Medical
# Research, 17(3), 279-301. https://doi.org/10.1177/0962280207080643
#
# Law, M., Jackson, D., Turner, R., Rhodes, K., & Viechtbauer, W. (2016). Two
# new methods to fit models for network meta-analysis with random
# inconsistency effects. BMC Medical Research Methodology, 16, 87.
# https://doi.org/10.1186/s12874-016-0184-5

############################################################################

# the same analysis as above but using the netmeta package

# install the netmeta package
#install.packages("netmeta")

# load the netmeta package
library(netmeta)

tmp <- dat.hasselblad1998
tmp <- pairwise(treat=trt, event=xi, n=ni, studlab=study, data=tmp, sm="OR", addincr=TRUE)
net <- netmeta(tmp, data=tmp, comb.fixed=FALSE)
net

# compare this to what we did earlier; note: netmeta() uses a different method
# for estimating tau^2, so in order to obtain the same results, we fix tau^2
# to the value that netmeta() estimated
res <- rma.mv(yi, V, mods = ~ self_help + ind_counseling + grp_counseling - 1,
              random = ~ comp | study, rho=1/2, data=dat, tau2=net$tau2)
predict(res, newmods=diag(3), transf=exp, digits=4)
predict(res, newmods=rbind(c(-1,1,0),c(-1,0,1),c(0,-1,1)), transf=exp)
inc <- rma.mv(yi, V, mods = ~ self_help + ind_counseling + grp_counseling +
                              (self_help + ind_counseling + grp_counseling):design - 1,
              data=dat, btt="design")
inc

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-08
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 6.3 - 6.6
#
# last updated: 2025-05-09

############################################################################

# load the rethinking and dagitty packages
library(rethinking)
library(dagitty)

############################################################################

### 6.3: Collider bias

## 6.3.1: Collider of false sorrow

# simulate data according to the age, marriage, and happiness example
dat <- sim_happiness(seed=1977, N_years=1000)

# examine the distribution of the variables
precis(dat)

# inspect the code for sim_happiness() to see how exactly the data are simulated
sim_happiness

# Figure 6.4: plot of age versus happiness with blue dots corresponding to
# married individuals
plot(happiness ~ age, data=dat, pch=21, col=ifelse(married==1, "#1e59ae", "black"),
     bg=ifelse(married==1, "#1e59ae", "white"), bty="l")
legend(16, 2.5, pch=21, col=c("black","#1e59ae"), pt.bg=c("white","#1e59ae"),
       legend=c("unmarried","married"), horiz=TRUE, xpd=TRUE, text.width=20, bty="n")

# make a copy of the data that only includes the adults (age >= 18)
dat2 <- dat[dat$age >= 18,]

# rescale the age variable so 0 corresponds to 18 and 1 corresponds to 65
dat2$A <- (dat2$age - 18) / (65 - 18)

# marriage status indicator (1 = not married, 2 = married)
dat2$mid <- dat2$married + 1

# define the model predicting happiness from marriage status and A (age)
model <- alist(happiness ~ dnorm(mu, sigma),
               mu <- a[mid] + bA*A,
               a[mid] ~ dnorm(0, 1),
               bA ~ dnorm(0, 2),
               sigma ~ dexp(1))

# fit the model and inspect the results
res <- quap(model, data=dat2)
precis(res, depth=2)

# we find a negative slope for A, which is not actually a reflection of how
# the data are simulated (where happiness is unrelated to age); this happens
# because marriage status is a collider

# define the model without marriage status as predictor
model <- alist(happiness ~ dnorm(mu, sigma),
               mu <- a + bA*A,
               a ~ dnorm(0, 1),
               bA ~ dnorm(0, 2),
               sigma ~ dexp(1))

# fit the model and inspect the results
res <- quap(model, data=dat2)
precis(res, depth=2)

# now we find a slope of =~ 0 for age

# note that there is no correlation between A (age) and happiness in the data
cor(dat2$A, dat2$happiness)

# but within each subgroup (unmarried and married), there is negative
# correlation between A (age) and happiness
cor(dat2$A[dat2$mid == 1], dat2$happiness[dat2$mid == 1])
cor(dat2$A[dat2$mid == 2], dat2$happiness[dat2$mid == 2])

## 6.3.2: The haunted DAG

# set values for the simulation
N <- 200  # number of grandparent-parent-child triads
b_GP <- 1 # direct effect of G on P
b_GC <- 0 # direct effect of G on C
b_PC <- 1 # direct effect of P on C
b_U  <- 2 # direct effect of U on P and C

# simulate the data
set.seed(1)
U <- 2*rbern(N, 0.5) - 1
G <- rnorm(N)
P <- rnorm(N, b_GP*G + b_U*U)
C <- rnorm(N, b_PC*P + b_GC*G + b_U*U)
dat <- data.frame(C=C, P=P, G=G, U=U)

# define and fit the model predicting C from P and G and inspect the results
res <- quap(alist(C ~ dnorm(mu, sigma),
                  mu <- a + b_PC*P + b_GC*G,
                  a ~ dnorm(0, 1),
                  c(b_PC,b_GC) ~ dnorm(0, 1),
                  sigma ~ dexp(1)), data=dat)
precis(res)

# note the negative slope for b_GC, when in fact the true slope is 0; again,
# this is a result of P being a collider

# Figure 6.5
sel <- P >= quantile(P, .45) & P <= quantile(P, .60)
plot(scale(C) ~ scale(G), pch=21, col=ifelse(U==1,"#1e59ae","black"),
     bg=ifelse(U==1,ifelse(sel,"#1e59ae","transparent"),ifelse(sel,"black","transparent")),
     xlab="gradparent education (G)", ylab="grandchild education (C)", bty="l")
legend(-2.8, 2, pch=21, col=c("#1e59ae","black"), pt.bg=c("#1e59ae","black"),
       text.col=c("#1e59ae","black"), title.col="gray50",
       title="parents in 45th-60th centile", bty="n",
       legend=c("good neighborhoods","bad neighborhoods"))

# define and fit the model predicting C from P, G, and U and inspect the results
res <- quap(alist(C ~ dnorm(mu, sigma),
                  mu <- a + b_PC*P + b_GC*G + b_U*U,
                  a ~ dnorm(0, 1),
                  c(b_PC,b_GC,b_U) ~ dnorm(0, 1),
                  sigma ~ dexp(1)), data=dat)
precis(res)

# when controlling for U, then we find a slope around 0 for b_GC

############################################################################

### 6.4: Confronting confounding

## 6.4.2: Two roads

# define and plot the DAG
dag <- dagitty("dag {
A [pos=\"-0.290,-1.324\"]
B [pos=\"-0.290,-0.644\"]
C [pos=\"0.815,-1.026\"]
U [latent,pos=\"-1.429,-1.026\"]
X [exposure,pos=\"-1.421,-0.130\"]
Y [outcome,pos=\"0.823,-0.130\"]
A -> C A -> U C -> B C -> Y U -> B U -> X X -> Y}")
plot(dag)

# check what variable(s) need to be adjusted for to obtain an unbiased
# estimate of the relationship between X and Y
adjustmentSets(dag)

# so controlling for either C or A would do

## 6.4.3: Backdoor waffles

# define and plot the DAG
dag <- dagitty("dag {
A [pos=\"-1.050,0.160\"]
D [outcome,pos=\"0.654,0.160\"]
M [pos=\"-0.185,-0.478\"]
S [pos=\"-1.050,-1.063\"]
W [exposure,pos=\"0.654,-1.063\"]
A -> D A -> M M -> D S -> A S -> M S -> W W -> D}")
plot(dag)

# show all paths and whether they are open or not
print(do.call(cbind, paths(dag)), quote=FALSE)

# which indirect paths between W and D are open/closed?
# W <- S -> A -> D         - open
# W <- S -> A -> M -> D    - open
# W <- S -> M -> D         - open
# W <- S -> M <- A -> D    - closed (because M is a collider for S and A)

# check what variable(s) need to be adjusted for
adjustmentSets(dag)

# check for the implied conditional independencies in the DAG
impliedConditionalIndependencies(dag)

############################################################################

### 6.6: Practice

# Exercise 6H2

# get the WaffleDivorce dataset and put it into 'dat' (see section 5.1 and the
# code from the session on 2025-02-27 for our previous coverage of these data)
dat <- get(data(WaffleDivorce))

# inspect the first 6 rows
head(dat)

# abbreviate the names of the variables as in the DAG
dat$D <- dat$Divorce
dat$M <- dat$Marriage
dat$A <- dat$MedianAgeMarriage
dat$S <- dat$South
dat$W <- dat$WaffleHouses

# we will check the conditional independencies given above in these data; we
# could use Bayesian models for this, but for simplicity let's just use
# standard OLS estimation

# check A _||_ W | S
summary(lm(A ~ W + S, data=dat))
summary(lm(W ~ A + S, data=dat))

# note: it does not matter whether A is the outcome and W the predictor or
# vice-versa; the t-statistic (i.e., the 'signal to noise ratio') is the same
# in both models

# check D _||_ S | A, M, W
summary(lm(D ~ S + A + M + W, data=dat))
summary(lm(S ~ D + A + M + W, data=dat))

# check M _||_ W | S
summary(lm(M ~ W + S, data=dat))
summary(lm(W ~ M + S, data=dat))

# so it seems that there is no evidence to suggest that the implied
# conditional independencies in our DAG are violated

############################################################################

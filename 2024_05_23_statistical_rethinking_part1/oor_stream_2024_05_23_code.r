############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-23
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 1.1 - 1.4
#
# last updated: 2024-05-24

############################################################################

# there really isn't any code for this session, except that we need to install
# some packages to be used further on in the book; note that installing some
# of these packages requires a C++ compiler; for those working under Windows,
# installing RTools (https://cran.r-project.org/bin/windows/Rtools/) should be
# sufficient; there is also some more info related to install RStan here:
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#configuring-c-toolchain

# install the rstan package
install.package("rstan")

# install some additional packages needed
install.packages(c("coda","mvtnorm","remotes","dagitty"))

# install the cmdstanr package
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# install CmdStan via the cmdstanr package
cmdstanr::install_cmdstan()

# install the rethinking package
remotes::install_github("rmcelreath/rethinking")

############################################################################

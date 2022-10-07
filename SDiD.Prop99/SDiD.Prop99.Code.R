devtools::install_github("synth-inference/synthdid")
library(synthdid)
library(rngtools)
library(future)
library(doFuture)
library(future.batchtools)
library(xtable)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

estimators = list(did=did_estimate,
                  sc=sc_estimate,
                  sdid=synthdid_estimate)
str(synthdid_estimate)

str(sc_estimate)

str(did_estimate)

data('california_prop99')
head(california_prop99)

setup = panel.matrices(california_prop99)
head(setup)

estimates = lapply(estimators, function(estimator) { estimator(setup$Y,
                                                               setup$N0, setup$T0) } )

head(estimates)

standard.errors = mapply(function(estimate, name) {
  set.seed(12345)
  if(name == 'mc') { mc_placebo_se(setup$Y, setup$N0, setup$T0) }
  else {             sqrt(vcov(estimate, method='placebo'))     }
}, estimates, names(estimators))

head(standard.errors)

california.table = rbind(unlist(estimates), unlist(standard.errors))
rownames(california.table) = c('estimate', 'standard error')
colnames(california.table) = toupper(names(estimators))
round(california.table, digits=1)

#Figure 1
synthdid_plot(estimates[1:3], facet.vertical=FALSE,
              control.name='control', treated.name='california',
              lambda.comparable=TRUE, se.method = 'none',
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7,
              diagram.alpha=1, onset.alpha=.7) +
  theme(legend.position=c(.26,.07), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank())

#Figure 2        
synthdid_units_plot(rev(estimates[1:3]), se.method='none') +
  theme(legend.background=element_blank(), legend.title = element_blank(),
        legend.direction='horizontal', legend.position=c(.17,.07),
        strip.background=element_blank(), strip.text.x = element_blank())

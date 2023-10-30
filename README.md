# wnai_bayesnet
Bayesian networks and causal inferences of complexity in the Western North American Indian dataset (WNAI), prepared into the data file df.csv

## Paper title
Scripts are for the manuscript titled 'Bayesian causal inference suggests institutions solve socioecological coordination problems in small-scale societies' by Hamilton MJ, RS Walker, and B Buchanan. 

## Use R package bnlearn
bnlearn (bnlearn.com) is an `R` package for learning the graphical structure of Bayesian networks, estimating their parameters and performing some useful inference. We use the boostrap feature for understanding causal structures in the WNAI data.

## Phase 1 structure learning example: bootstrap multiple network structures
```splus
library(bnlearn)
set.seed(1)
boot <- boot.strength(all, R = 10001, cpdag = FALSE,
                      algorithm = "tabu")
boot[boot$strength > 0.8 & boot$direction >= 0.5, ]
avg.boot <- averaged.network(boot, threshold = .8)
plot(boot)
avg.boot

strength.plot(avg.boot, boot, threshold=.8,
              layout = "dot",
              shape = "rectangle")
```
## In Phase 2 use R package brms
brms (paul-buerkner.github.io/brms/) is an `R` package that provides an interface to fit Bayesian generalized (non-)linear multivariate multilevel models using Stan. We use it to estimate the regression coefficients of the edges in our network while adjusting for spatial and linguistic autocorrelation using Gaussian processes.

## Phase 2 parameter learning example: bayesian path model with brms
```splus
library(brms)
ritualmod <- bf(ritual ~ 1  + war + Subsistence + pop.density +Agriculture + (1|phyla) + gp(Lat, Long))
warmod <- bf(war ~ 1  + Politics + (1|phyla) + gp(Lat, Long))
propmod <- bf(property ~ 1  + econ_dist + Politics + (1|phyla) + gp(Lat, Long))
politicsmod <- bf(Politics ~ 1  + pop.density + (1|phyla) + gp(Lat, Long))
sharemod <- bf(econ_dist ~ 1  + marriage + pop.density + (1|phyla) + gp(Lat, Long))
marriagemod <- bf(marriage ~ 1  + Subsistence + (1|phyla) + gp(Lat, Long))
snmod <- bf(supernatural ~ 1  + Agriculture + (1|phyla) + gp(Lat, Long))
dolmod <- bf(div_of_labor ~ 1  + material_culture + Subsistence + Agriculture + (1|phyla) + gp(Lat, Long))
techmod <- bf(technology ~ 1 + Agriculture + material_culture + (1|phyla) + gp(Lat, Long)) 

mv <- mvbf( techmod + sharemod + marriagemod + ritualmod + snmod + dolmod + propmod + politicsmod + warmod,
            rescor=FALSE )
m <- brm(data = join_df, family = gaussian(link = "identity"),
      mv, prior = set_prior("normal(0,.5)", class = "b"),
      iter =  2e3, chains = 2, cores = 3, #save_all_pars = TRUE,
      control = list(adapt_delta = .999, max_treedepth = 20),
      seed = 14, backend = "cmdstanr")
prior_summary(m)
m
pl <- plot(m, N = 4, ask = FALSE)
posterior_summary(m)
bayes_R2(m)
conditional_effects(m, points=T)
ranef(m)
```

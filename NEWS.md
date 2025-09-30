# RBaM 1.1.1

* New functionality: `runModel()`, to perform a single run of any model available in RBaM.
* New functionality: inference functions, to compute standard likelihoods, priors and posteriors.
* New functionality: Markov Chain Monte Carlo (MCMC) samplers, in particular an adaptive Metropolis sampler `MCMC_AM()` largely inspired by [Haario et al. (2001)](https://doi.org/10.2307/3318737), and a one-at-a-time Metropolis sampler `MCMC_OAAT()`.
* New functionality: `SPD_estimate()` to estimate a stage-period-discharge (SPD) BaRatin model.
* New functionality: 1-D hydrodynamical model 'MAGE_ZQV' available in the list of RBaM models.
* Bug fix: function `runExe()`, used by main function `BaM()`, was not handling the user-requested workspace properly.
* Miscellaneous minor changes.

# RBaM 1.0.1

* Miscellaneous changes following CRAN review.

# RBaM 1.0.0

* Added a `NEWS.md` file to track changes to the package.

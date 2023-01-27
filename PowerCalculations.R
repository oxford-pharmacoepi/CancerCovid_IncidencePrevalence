# Power analysis and sample size calculations for Cancer/Covid study

install.packages("powerSurvEpi")
library(powerSurvEpi)

install.packages("WebPower")
library(WebPower)

install.packages("pwr")
library(pwr)

# sample size calculation for incidence and prevalence:

# incidence
# base rate and change after lockdown come from the supplementary material from the Coma et al 2020 paper 
# on average they observed incidence rates of 72.4 for all cancer combined before lockdown 
# compared to 54.6 after lockdown)


exp0 <- 72.4/100000 
slope <- exp(1) # this is estimated - not sure about this

wp.poisson(exp0=1.0005, exp1=0.36, alpha=0.05, power=0.80, alternative ="two.sided", family="Bernoulli")



# prevalence
# effect size measure for proportions where the expected prevalence of a cancer in the general population is 
x <- sqrt(0.0875) # this is roughly the point prevalence of breast cancer observed just before lockdown based on my preliminary analyses
y <- sqrt(0.075) # this is roughly the point prevalence of breast cancer observed just after lockdown based on my preliminary analyses


h <- (2*asin(x)-(2*asin(y))) # this comes from here as a test for two proportions 
# https://med.und.edu/research/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf

effect_s <- print(h)

# get estimated sample size given power
# sample size required = 7489
pwr.2p.test(h=(effect_s), sig.level=0.05, power=0.80, alternative="two.sided")


# get post-hoc power given the sample size observed and the preliminary results observed
# power = 0.878
pwr.2p.test(h=(effect_s), n=9319, sig.level=0.05, alternative="two.sided")





# Calculating sample size for survival analysis:
# see here: https://cran.r-project.org/web/packages/powerSurvEpi/powerSurvEpi.pdf

# vary the theta value to examine smaller hazard ratio. "Small, medium, and large HRs comparing 2
# groups would be approximately 1.3, 1.9, and 2.8, respectively." (from https://acsjournals.onlinelibrary.wiley.com/doi/pdf/10.1002/cncr.29924


Dsupersupersmall <- numDEpi.default(power = 0.8,
                                    theta = 1.1,
                                    p = 0.33,
                                    rho2 = 0.5^2,
                                    alpha = 0.05)

Dsupersmall <- numDEpi.default(power = 0.8,
                               theta = 1.2,
                               p = 0.33,
                               rho2 = 0.5^2,
                               alpha = 0.05)

Dsmall <- numDEpi.default(power = 0.8,
                          theta = 1.3,
                          p = 0.33,
                          rho2 = 0.5^2,
                          alpha = 0.05)

Dmed <- numDEpi.default(power = 0.8,
                        theta = 1.9,
                        p = 0.33,
                        rho2 = 0.5^2,
                        alpha = 0.05)

Dlarge <- numDEpi.default(power = 0.8,
                          theta = 2.8,
                          p = 0.33,
                          rho2 = 0.5^2,
                          alpha = 0.05)

# proportion of subjects died of the disease of interest.
psi10 <- 0.10
psi5 <- 0.05
psi1 <- 0.01

# total number of subjects required to achieve the desired power

# different effect sizes, 1% death rate
ceiling(Dsupersupersmall / psi1)
ceiling(Dsupersmall / psi1)
ceiling(Dsmall / psi1)
ceiling(Dmed / psi1)
ceiling(Dlarge / psi1)


# different effect sizes, 5% death rate
ceiling(Dsupersupersmall / psi5)
ceiling(Dsupersmall / psi5)
ceiling(Dsmall / psi5)
ceiling(Dmed / psi5)
ceiling(Dlarge / psi5)


# different effect sizes, 10% death rate
ceiling(Dsupersupersmall / psi10)
ceiling(Dsupersmall / psi10)
ceiling(Dsmall / psi10)
ceiling(Dmed / psi10)
ceiling(Dlarge / psi10)




# In my erap protocol i write: 
# Objective 6 will be a survival analysis comparing death due to cancer   before 
# and after lockdown up to the latest of follow-up. With a type 1 error of 0.05 
# and a power of 0.8, between 900 – 68,800 patients would be required (per cancer) 
# to identify a hazard ratios ranging from small (1.3) to large (2.8) for a 
# covariate with correlation to the outcome of 0.5 and an anticipated death rate 
# ranging between 1% to 10%. The number of patients identified in CPRD (GOLD) 
# (detailed above) gives sufficient sample size to address this objective for 
# breast cancer and prostate cancer, assuming small, medium or large effects, 
# and anticipate death rates between 5-10%; colorectal cancer and lung, assuming 
# medium or large effects, and anticipate death rates between 5-10%.

# For chi square. see here: 
# https://med.und.edu/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf

pwr.chisq.test(w = 0.3, df = 3, sig.level = 0.5, power = 0.8)

# w=effect size, df=degrees of freedom, sig.level=significant level, power=power of test

# In my erap protocol I write: 
# Objective 6 will statistically compare frequencies of GP visits, measurements 
# and procedures in diagnosed patients before and after lockdown. With a type 1
# error of 0.05 and a power of 0.8, 33 patients would be required to identify a 
# medium effect size of 0.3 between time periods. The number of patients 
# identified in CPRD (GOLD) (detailed above) gives sufficient sample size to 
# address this objective.


# Power analysis and sample size calculations for Cancer/Covid study

install.packages("powerSurvEpi")
library(powerSurvEpi)

install.packages("WebPower")
library(WebPower)

install.packages("pwr")
library(pwr)

# =============== sample size calculation for objective 1 and 4 ============== #

# characterising counts of measurements and procedures etc. before and after lockdown
# simple sample size calculation of proportions, assuming there are small differences before and after lockdown
# assuming small effect size of 0.2

pwr.2p.test(h=0.2, sig.level=0.05, power=.80, alternative="two.sided")

# Difference of proportion power calculation for binomial distribution (arcsine transformation) 

# h = 0.2
# n = 392.443
# sig.level = 0.05
# power = 0.8
# alternative = two.sided



# ============ sample size calculation for incidence ========================= #

# incidence based on Coma et al (2020)
# base rate and change after lockdown come from the supplementary material from the Coma et al 2020 paper 
# on average they observed incidence rates of 72.4 for all cancer combined before lockdown 
# compared to 54.6 after lockdown)
# In the erap protocol we write: "For objective 2 it is possible to derive a possible 
# effect size measure from previous literature. For example, Coma et al. (2020) report 
# the monthly average incidence of all cancers before lockdown as 72.4 per 100,000 person years, 
# compared to 54.6 after lockdown. Using these estimates, a sample of 56.78 individuals 
# would be required to replicate these findings." 

exp0 <- exp(72.4/100000)  # Expect the base rate (intercept) for cancer is 72.4 (per 100,000), so exp0 = exp(72.4/100000) = 1.000724
slope <- exp(-1.02) # Expect the relative increase of the event rate (slope) to be -1.02, so exp1 = exp(-1.02) = 0.36. Note that this is estimated

wp.poisson(exp0=exp0, exp1=slope, alpha=0.05, power=0.80, alternative ="two.sided", family="Bernoulli")

#Power for Poisson regression

#n power alpha     exp0      exp1    beta0 beta1
#56.88938   0.8  0.05 1.000724 0.3605949 0.000724 -1.02


# ============ sample size calculation for prevalence ======================== #

# For prevalence there are currently no published rates of cancers before and after lockdown. 
# As such we have calculated post-hoc power based on our preliminary analyses. As an 
# example, prevalence of breast cancer was reduced by ~0.125% immediately following lockdown. 
# Using this estimate in post-hoc power analyses yields power of 88% to identify changes 
# in prevalence as large as observed based on our sample size of 9319 patients with breast cancer.

# calculate effect size based on two prevalence rates
x <- sqrt(0.0875) # this is roughly the point prevalence of breast cancer observed just before lockdown based on my preliminary analyses
y <- sqrt(0.075) # this is roughly the point prevalence of breast cancer observed just after lockdown based on my preliminary analyses


effect_s <- (2*asin(x)-(2*asin(y))) # this comes from here as a test for two proportions 
# https://med.und.edu/research/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf

print(effect_s)

# get estimated sample size given power
pwr.2p.test(h=(effect_s), sig.level=0.05, power=0.80, alternative="two.sided")

#Difference of proportion power calculation for binomial distribution (arcsine transformation) 

#h = 0.04578309
#n = 7489.043
#sig.level = 0.05
#power = 0.8
#alternative = two.sided




# ============ post-hoc power calculation for prevalence ===================== #

# get post-hoc power given the sample size observed and the preliminary results observed

pwr.2p.test(h=(effect_s), n=9319, sig.level=0.05, alternative="two.sided")

# power = 0.878



# ============= Estimate of sample size in denominator based on prior simulation study
# Hawley et al (2019)

3000*(3.5/0.12)



# ============== Calculating sample size for survival analysis =============== #

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


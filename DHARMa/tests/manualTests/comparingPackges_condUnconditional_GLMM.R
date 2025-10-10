# Comparing if conditional and unconditional simulations have comparable sds across packages/models expected to give similar results (same data, model fit)
# The variance of conditional simulations should be smaller than the variance of unconditional simulations
#library(DHARMa)
# Cosmina Werneke & Melina Leite

devtools::load_all() # latest DHARMa version 0.4.8a!
library(mgcv)
library(glmmTMB)
library(lme4)
library(tidyverse)
library(ggeffects)

# create data (Poisson, because this is required for GLMMadaptive)
# increasing RE variance to see more difference between conditional and unconditional
set.seed(123)
dat = createData(100,randomEffectVariance = 2 )

# FIT MODELS
# mgcv
mgam = mgcv::gam(observedResponse ~ Environment1 + s(group, bs = "re"), data = dat, family = poisson())

# lme4
mglmer = lme4::glmer(observedResponse ~ Environment1 + (1|group), data = dat, family = poisson())

# glmmTMB
mglmmTMB = glmmTMB::glmmTMB(observedResponse ~ Environment1 + (1|group), data = dat, family = poisson())

# spaMM
mspaMM = spaMM::HLfit(observedResponse ~ Environment1 + (1|group), data = dat, family = poisson())

# GLMMadaptive
mGLMMadaptive = GLMMadaptive::mixed_model(fixed = observedResponse ~ Environment1, random = ~ 1 |group, data = dat, family = poisson())


# SIMULATIONS - taking SDs to have smaller variation
gam_cond = apply(getSimulations(mgam, nsim = 100), 2, sd)

glmer_cond = apply(getSimulations(mglmer, simulateREs = "conditional", nsim = 100), 2, sd)
glmer_uncond = apply(getSimulations(mglmer, simulateREs = "unconditional", nsim = 100), 2, sd)

glmmTMB_cond = apply(getSimulations(mglmmTMB, simulateREs = "conditional", nsim = 100), 2, sd)
glmmTMB_uncond = apply(getSimulations(mglmmTMB, simulateREs = "unconditional", nsim = 100), 2, sd)

spaMM_cond = apply(getSimulations(mspaMM, simulateREs = "conditional", nsim = 100), 2, sd)
spaMM_uncond = apply(getSimulations(mspaMM, simulateREs = "unconditional", nsim = 100), 2, sd)

GLMMadaptive_cond = apply(getSimulations(mGLMMadaptive, simulateREs = "conditional", nsim = 100), 2, sd)
GLMMadaptive_uncond = apply(getSimulations(mGLMMadaptive, simulateREs = "unconditional", nsim = 100), 2, sd)


varis <- data.frame(gam_cond,glmer_cond, glmmTMB_cond, spaMM_cond, GLMMadaptive_cond,
                    glmer_uncond, glmmTMB_uncond, spaMM_uncond, GLMMadaptive_uncond) %>%
  pivot_longer(1:9, names_to = "model", values_to = "sim_sd") %>%
  separate("model", c("model", "simulation")) %>%
  mutate(model = fct_relevel(model, "glmer", "glmmTMB", "spaMM", "GLMMadaptive", "gam"))

ggplot(varis, aes(x=model, y=sim_sd, col=simulation)) +
  geom_boxplot() + scale_y_log10()

# just to have a look:
mod <- glm(sim_sd ~ model*simulation, data=varis, family = "Gamma")
summary(mod)
mod |>
  ggpredict(terms=c("model", "simulation")) |> plot()

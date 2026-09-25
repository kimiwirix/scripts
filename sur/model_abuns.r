# setwd("~/lab/exp/2026/today7/")
library(tidyverse)
library(broom.mixed)
library(lme4)

#' Model abundances of bacteria at single timepoint considering
#' design choices.

################# Functions ###########
parboot_glmer <- function(large_model, small_model,
                          nsim = 1000){
  
  # observed LRT
  obs_lrt <- 2 * (as.numeric(logLik(large_model)) - as.numeric(logLik(small_model)))
  
  # simulate responses from the NULL model (random intercept only)
  sims <- simulate(small_model, nsim = nsim)
  
  sim_lrt <- rep(NA_real_, nsim)
  for (i in seq_len(nsim)) {
    m1 <- try(refit(large_model, sims[[i]]), silent = TRUE)
    m0 <- try(refit(small_model, sims[[i]]), silent = TRUE)
    if (!inherits(m1, "try-error") && !inherits(m0, "try-error")) {
      sim_lrt[i] <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m0)))
    }
  }
  
  # how many fits worked
  cat(sum(!is.na(sim_lrt)), "fits worked\n")
  
  # bootstrap p-value
  sim_ok <- sim_lrt[!is.na(sim_lrt)]
  p_boot <- (sum(sim_ok >= obs_lrt) + 1) / (length(sim_ok) + 1)
  
  return(p_boot)
}

#################################################

#' Read data
Tab <- read_tsv("data/counts_all.tsv")
Meta <- read_tsv("data/meta_all.tsv")
Strain_map <- read_tsv("CC_dbs/strains_in_comsints.tsv")

#' Calculate depth per sample, required for normalization
Meta <- Tab %>%
  summarise(across(-Strain, sum)) %>%
  pivot_longer(everything(), names_to = "label_final", values_to = "depth") %>%
  right_join(Meta, by = "label_final")

#' Clean metadata columns, rename label_final to sample.
#' Recode time to -1, 0, 1.
#' Make batch categorical.
Meta <- Meta %>%
  select(-label, -timepoint) %>%
  rename(sample = label_final) %>%
  mutate(temp = replace_when(temp, temp == 30 ~ -1,
                             temp == 37 ~ 0,
                             temp == 42 ~ 1)) %>%
  mutate(batch = paste0("batch", batch))

#' Create master data tibble
Dat <- Tab %>%
  pivot_longer(-Strain,
               names_to = "sample",
               values_to = "count") %>%
  full_join(Meta, by = "sample")

#' Clean strains out of expected communities
Dat %>%
  left_join(Strain_map %>% select(community, Strain = strain, presence),
            by = c("Strain", "community")) %>%
  filter(presence == 0) %>%
  arrange(desc(count)) %>%
  print(n = 20)
#' We observe that there is a handful of samples
#' with more than .5% contamination.
#' NOTE: we are going to **ignore** this contamination for the moment
Dat <- Dat %>%
  left_join(Strain_map %>% select(community, Strain = strain, presence),
            by = c("Strain", "community")) %>%
  filter(presence == 1) %>%
  select(-presence) # remove unnecessary variable

#' Add strain specific and observation-level variables
Dat <- Dat %>%
  mutate(st_com = paste0(Strain, "_", community)) %>%
  mutate(st_batch = paste0(Strain, "_", batch)) %>%
  mutate(obs = paste0("o", 1:length(count)))

#' Calculate frequency of species in the inocula.
#' Not clear if it will be needed but better to precalculate
Inoc_freqs <- Dat %>%
  filter(hrs == 0) %>%
  mutate(freq_t0 = count / depth) %>%
  select(Strain, community, freq_t0)

#' Model abundances in one timepoint. We define the timepoint
#' below and automate following steps
timepoint_hrs <- 9

#' Select data from selected timepoint and append
#' inocula frequencies
dat <- Dat %>%
  filter(hrs == timepoint_hrs) %>%
  left_join(Inoc_freqs, by = c("Strain", "community"))

#' Check number of st_levels and number of observations per level
#' We have 6-9 obs per level as expected. Variation is because
#' a few syncoms had an extra rep sequenced
dat %>% count(st_com) %>% summary()

#' Fit main models. Considering combinations of terms
#' for strain-specific temperature slope (1 + temp | Strain),
#' and strain changes between communities (1+temp|stcom) effects.
m1.multi <- glmer(count ~ log(depth) + temp +
                    (1 + temp | st_com) +
                     (1 | st_batch) + (1 | obs),
                   data = dat,
                   family = poisson(link = "log"),
                  control = glmerControl(optimizer = "bobyqa"))
m2.multi <- glmer(count ~ log(depth) + temp +
                    (1 + temp | Strain) +
                    (1 + temp | st_com) +
                    (1 | st_batch) + (1 | obs),
                  data = dat,
                  family = poisson(link = "log"),
                  control = glmerControl(optimizer = "bobyqa"))
m3.multi <- glmer(count ~ log(depth) + temp +
                    (1 + temp | Strain) +
                    (1 | st_batch) + (1 | obs),
                  data = dat,
                  family = poisson(link = "log"),
                  control = glmerControl(optimizer = "bobyqa"))

#' All models converge easily. Compare with AIC and BIC.
#' The second model which contains both effects is the preferred model
#' by both criteria. **Move forward with m2.multi**.
AIC(m1.multi, m2.multi, m3.multi)
BIC(m1.multi, m2.multi, m3.multi)

#' Check estimates, no issues, no zeroes.
VarCorr(m2.multi)
getME(m2.multi, "theta")

#' We also check for overdispersion in the selected model. The
#' (1|obs) term should handle this, but good to check.
#' DHARMa test confirms no overdispersion. Residuals are not
#' gaussian.
m2.multi.simres <- DHARMa::simulateResiduals(m2.multi)
plot(m2.multi.simres)
DHARMa::testDispersion(m2.multi.simres)

#' Now we move to test the two temperature effects
#' we fit a model with none of the terms (m0), and
#' a model with only one of them (a,b)
m0.multi <- glmer(count ~ log(depth) + temp +
                    (1 | st_com) +
                    (1 | st_batch) + (1 | obs),
                  data = dat,
                  family = poisson(link = "log"),
                  control = glmerControl(optimizer = "bobyqa"))
m0a.multi <- glmer(count ~ log(depth) + temp +
                     (1 + temp | Strain) +
                     (1 | st_com) +
                     (1 | st_batch) + (1 | obs),
                   data = dat,
                   family = poisson(link = "log"),
                   control = glmerControl(optimizer = "bobyqa"))
m0b.multi <- glmer(count ~ log(depth) + temp +
                     (1 + temp | st_com) +
                     (1 | st_com) +
                     (1 | st_batch) + (1 | obs),
                   data = dat,
                   family = poisson(link = "log"),
                   control = glmerControl(optimizer = "bobyqa"))

#' m0a was singular, which would be an issue for the m1 models, but
#' not here. LRT test and nonparametric bootstrap still valid.
#' Checking estimates, it is the batch variable the responsible
#' for the singularity, which has no effect on the biological variables
VarCorr(m0a.multi)
getME(m0a.multi, "theta") 

#' Test for the importance of the temperature effects.
#' First we test the effect of the strain by community specific temperature effect
anova(m2.multi, m0a.multi)

#' Second for the strain specific (averaged across communities) effect
anova(m2.multi, m0b.multi)

#' Then for the two terms together
anova(m2.multi, m0.multi)

#' All test are significant, consistent with AIC/BIC.
#' Parametric bootstrap and boundary corrected p-values can be done.
#' First is a bit slow, will ignore for now. It will likely give the
#' same results. Boundary corrected p-value can be calculated as well,
#' though given significance it wont change the results.
# parboot_glmer(m2.multi, m0.multi, nsim = 200)
chisq <- anova(m2.multi, m0.multi)$Chisq[2]
0.5 * pchisq(chisq, 1, lower.tail = FALSE) + 0.5 * pchisq(chisq, 2, lower.tail = FALSE)

#' Last sanity checks
m2.multi@optinfo$conv$opt   # should be 0
isSingular(m2.multi)        # should be FALSE
isSingular(m0.multi)
isSingular(m0a.multi)
isSingular(m0b.multi)

#' Now we use broom.mixed to get the random effects, identify significant
#' ones and plot with ggplot (better than lattice)

Ranefs <- tidy(m2.multi, effects = "ran_vals",
               conf.level = 0.95,
               conf.int = TRUE)
Ranefs
  
#' Lets look at the strain average effects (that is across all communities)
Ranefs %>%
  filter(group == "Strain") %>%
  filter(term == "temp") %>%
  arrange(desc(estimate))

#' From the results CH447, CH90, CH23 have a temperature dependent shift which
#' is more positive than the average strain. Though from the plot it
#' seems like CH3 is highly non-monotonic with respect to temperature. Need
#' to add quadratic terms
p1 <- dat %>%
  filter(Strain %in% c("CH447", "CH90", "CH23")) %>%
  ggplot(aes(x = temp, y = count/depth,
             group = interaction(Strain,batch,community))) +
  facet_wrap(. ~ Strain) +
  geom_point() +
  geom_line() +
  ggtitle(label = paste("Strains with positive temp|Strain effect at time", timepoint_hrs)) +
  theme_classic()
p1
ggsave("strain_pos_slopes.png", width = 8, height = 4)

#' From the results "CH154a", "CH29", "CH161d", "CH99b" have a temperature
#' dependent shift which is more negative than the average strain, and they
#' all tend to go extinct at higher temperatures
p1 <- dat %>%
  filter(Strain %in% c("CH154a", "CH29", "CH161d", "CH99b")) %>%
  ggplot(aes(x = temp, y = count/depth,
             group = interaction(Strain,batch,community))) +
  facet_wrap(. ~ Strain) +
  geom_point() +
  geom_line() +
  ggtitle(label = paste("Strains with negative temp|Strain effect at time", timepoint_hrs)) +
  theme_classic()
p1
ggsave("strain_neg_slopes.png", width = 8, height = 4)




#' Now we look at the strains that have community specific differences
#' (interactions) in their response to temperature. I will display only
#' significant results. There are 33 significant strain-community specific
#' temperature behaviors
Res <- Ranefs %>%
  filter(group == "st_com") %>%
  filter(term == "temp") %>%
  filter((conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0)) %>%
  arrange(desc(estimate)) %>%
  print(n = 100)


#' Plotting all strains with at least one significant difference and
#' color coding significant effects
Res <- Res %>%
  separate(level, into = c("Strain", "community"), sep = "_") %>%
  mutate(slope = "up") %>%
  mutate(slope = replace(slope, conf.high < 0, "down"))
Res

p1 <- dat %>%
  filter(Strain %in% unique(Res$Strain)) %>%
  left_join(Res %>%
              select(Strain, community, slope),
            by = c("Strain", "community")) %>%
  mutate(slope = replace_na(slope, "none")) %>%
  # filter(Strain == "CH23") %>% print(n = 100)
  ggplot(aes(x = temp, y = count/depth,
             group = interaction(Strain,batch,community),
             colour = slope)) +
  facet_wrap(. ~ Strain, scales = "free_y") +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("red","black","blue")) +
  ggtitle(label = paste("Strains with significant community specific effects at time", timepoint_hrs)) +
  theme_classic()
p1
ggsave("st_com_slopes.png", width = 8, height = 4)

#' Up and down indicate **changes to the slope** so we see that 
#' cases where a strain grows more at low temperature lead to
#' a more negative slope, so color red, and viceversa. Generally makes
#' sensem though CH23 still problmeatic (and interesting as it is
#' systematically non-monotonic).


#### Not used

#' Started with ideas of model one strain at a time, but abandoned for
#' multi model. Much better.
#' 
#' #' Now model 1 strain
#' strain_list <- c("CH111")
#' syncom_list <- Strain_map$community[ Strain_map$strain == "CH111" & Strain_map$presence == 1]
#' dat <- Dat %>% filter(Strain %in% strain_list) %>%
#'   filter(community %in% syncom_list) %>%
#'   mutate(obs = paste0("o", 1:length(count)))
#' dat
#' 
#' 
#' m1.single <- glmer(count ~ log(depth) + freq_t0 + temp +
#'                      (1 + temp|community) +
#'                      (1|batch) + (1|obs),
#'                    data = dat,
#'                    family = poisson(link = "log"))
#' summary(m1.single)
#' 
#' # bm1.single <- brms::brm(count ~ log(depth) + freq_t0 + temp +
#' #                           (1 + temp|community) +
#' #                           (1|batch) + (1|obs),
#' #                         data = dat,
#' #                         family = poisson(link = "log"),
#' #                         cores = 4,
#' #                         chains = 4)
#' # summary(bm1.single)
#' 
#' #' Test overall effect via parametric bootstrap
#' m0.single <- glmer(count ~ log(depth) + freq_t0 + temp +
#'                      (1 | community) +
#'                      (1|batch) + (1|obs),
#'                    data = dat,
#'                    family = poisson(link = "log"))
#' 
#' summary(m0.single)
#' 
#' anova(m1.single, m0.single)
#' parboot_glmer(m1.single, m0.single, nsim = 50)
#' 
#' #' Simulate residuals
#' m1.single.simres <- DHARMa::simulateResiduals(m1.single)
#' plot(m1.single.simres)
#' DHARMa::testDispersion(m1.single.simres)
#' 
#' 
#' dat %>%
#'   ggplot(aes(x = temp, y = count / depth,
#'              group = interaction(community, batch, sep = "_", drop = TRUE))) +
#'   geom_point() +
#'   geom_line()
#' 
#' 

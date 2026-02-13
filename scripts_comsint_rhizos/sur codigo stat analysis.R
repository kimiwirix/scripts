#sur process data for statistical analysis

library(tidyverse)
library(readODS)

Tab <- read_csv("C:/Users/natal/Documents/LIIGH/freq.csv") %>%
  rename(strain = 1)

Meta <- read_csv("C:/Users/natal/Documents/LIIGH/meta.csv") %>%
  rename(id = 1)

NS1_comsints<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS1.ods", sheet="syncoms")%>%
  as.data.frame()

NS2_comsints<-read_ods("C:/Users/natal/Documents/LIIGH/data/data_comsint_rhizos/data_NS2.ods", sheet="syncoms")%>%
  as.data.frame()

syncoms<-NS2_comsints%>%
  left_join(NS1_comsints, by = "strain")


meta <- Meta %>%
#  filter(community %in% c("R1", "R2")) %>%
  left_join(syncoms %>%
              select(strain, R1,R2) %>%
              pivot_longer(-strain, names_to = "community", values_to = "presence") %>%
              pivot_wider(names_from = "strain", values_from = "presence"),
            by = "community")

meta %>% 
  print(n = 100)


meta <- meta %>%
  pivot_longer(-c("id", "community", "hrs", "techrep", "exp", "temp", "color_comsint", "community_temp"),
               names_to = "strain", values_to = "added") %>%
  mutate(added = replace_na(added, 0))

meta %>% 
  print(n = 100)



strains <- Tab$strain
strains <- strains %>%
  str_replace("NS_042g_27F", "ST00042") %>%
  str_replace("NS_164C_27F", "ST00164") %>%
  str_replace("NS_110C_1_27F", "ST00110") 
strains


Tab$strain <- strains


tab <- Tab %>%
  pivot_longer(-strain, names_to = "id", values_to = "count") 
tab



Dat <- meta %>%
  left_join(tab %>%
              group_by(id) %>%
              summarize(depth = sum(count)),
            by = "id") %>%
  left_join(tab, by = c("id", "strain"))
Dat



Dat <- Dat %>%
  left_join(Dat %>%
              filter(hrs == 0) %>%
              group_by(strain, community, exp) %>%
              summarise(i_freq = sum(count) / sum(depth),
                        .groups = "drop"),
            by = c("strain", "community", "exp"))
Dat



Dat %>% 
  ggplot(aes(x = added == 1, y = count)) +
  facet_wrap(~ strain, scales = "free_y") +
  geom_point(position = position_jitter(width = 0.1, height = 0)) +
  theme_classic()


library(lme4)
Dat

dat <- Dat %>%
  mutate(b_com = paste0(community, "_", strain)) %>%
  mutate(b_com = replace(b_com, added == 0, NA)) %>%
  
  mutate(b_rep = paste0(exp, "_", strain)) %>%
  mutate(b_rep = replace(b_rep, added == 0, NA)) %>%
  
  mutate(b_temp = paste0(temp, "_", strain)) %>%
  mutate(b_temp = replace(b_temp, added == 0, NA)) %>%
  
  mutate(b_obs = as.character(1:n()))
dat



m1 <- glmer(count ~ log(depth) + i_freq + (1|b_com) + 
              (1|b_rep) + (1|b_temp) + (1|b_obs), 
            data = dat %>%
              filter(hrs == 24), 
            family = poisson(link = log) )
summary(m1)


AIC(m1)
BIC(m1)


b_com <- ranef(m1, condVar = TRUE, whichel = "b_com", postVar = TRUE)
res <- tibble(effect = row.names(b_com$b_com),
              est = b_com$b_com[,1],
              postVar = attr(b_com$b_com, "postVar")[,,]) %>%
  mutate(lower = qnorm(p = 0.025, mean = est, sd = sqrt(postVar)),
         upper = qnorm(p = 0.975, mean = est, sd = sqrt(postVar)),
         pval = 2 * pnorm(q = -abs(est), mean = 0, sd = sqrt(postVar))) %>%
  mutate(qval = p.adjust(pval)) %>%
  arrange(est) %>%
  mutate(effect = factor(effect, levels = effect))
res


res %>%
  ggplot(aes(x = est, y = effect)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  theme_classic()



b_temp <- ranef(m1, condVar = TRUE, whichel = "b_temp", postVar = TRUE)
res <- tibble(effect = row.names(b_temp$b_temp),
              est = b_temp$b_temp[,1],
              postVar = attr(b_temp$b_temp, "postVar")[,,]) %>%
  mutate(lower = qnorm(p = 0.025, mean = est, sd = sqrt(postVar)),
         upper = qnorm(p = 0.975, mean = est, sd = sqrt(postVar)),
         pval = 2 * pnorm(q = -abs(est), mean = 0, sd = sqrt(postVar))) %>%
  mutate(qval = p.adjust(pval)) %>%
  arrange(est) %>%
  mutate(effect = factor(effect, levels = effect))
res


res %>%
  ggplot(aes(x = est, y = effect)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  theme_classic()


dat %>%
  filter(strain == "ST00046") %>%
  filter(hrs %in% c(24)) %>%
  ggplot(aes(col = factor(temp))) +
  facet_wrap(~community) +
  geom_segment(aes(x = 0, y = i_freq, xend = temp, yend = count / depth, linetype = exp )) +
  theme_classic() 

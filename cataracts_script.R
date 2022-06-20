### Cataracts Project
### STAA556

#-- Load libraries
library(readxl)
library(tidyverse)
library(kableExtra)
library(DescTools)
library(ggsci)
library(lme4)
library(sjPlot)
library(emmeans)
library(broom.mixed)

# -- Read in the data
cats <- read_excel("GRSD.cataract.xlsx", sheet = "Sheet1")

# remove spaces from column and value names
names(cats) <- str_replace_all(names(cats), " ", "_")
cats <- cats %>%
  mutate(CoatColor = str_replace_all(coat_color, " ", "_"))
str(cats)

# turn categorical vars into factor
cats <- cats %>%
  rename(Age = `age_(days)`,
         Weight = weight,
         Animal = animal) %>%
  mutate(Sex = as.factor(sex),
         CoatColor = as.factor(CoatColor),
         Family = as.factor(family), # should this stay a factor? Yes?
         BCS = as.ordered(BCS),
         Treatment = relevel(as.factor(groups), ref = "Unirradiated"),
         MyeloidLeukemia = as.factor(Myeloid_Leukemia),
         HarderianTumor = as.factor(Harderian_Tumor),
         PreTLymphoma = as.factor(PreT_Lymphoma),
         Score = as.ordered(Cataract_Score)) # ordinal cat; leave as numeric?

# select vars, add binary conversion for score
cats <- cats %>%
  select(c(Animal, Sex, Weight, CoatColor, Family, BCS, Age, Treatment,
           MyeloidLeukemia, HarderianTumor, PreTLymphoma, Score)) %>%
  mutate(Cataracts = ifelse(Score < 2, 0, 1))
str(cats)

#-- Review each predictor/covariate

# individual
length(unique(cats$Animal))

# sex
cats %>% count(Sex)
# even enough split between gender

# weight
summary(cats$Weight)
hist(cats$Weight, br = 20)
# distribution for weight looks symmetric

# coat color - probably irrelevant?
cats %>% count(CoatColor)
cats %>% group_by(CoatColor) %>% count(Sex)
cats %>% group_by(Family) %>% count(CoatColor)

# family !random effect of interest!
length(unique(cats$Family))
fam_counts <- cats %>% group_by(Family) %>% count()
summary(fam_counts$n)
hist(fam_counts$n, br = 15) # large variation in family size

# BCS - body condition score
cats %>% count(BCS)

# age
range(cats$Age)
hist(cats$Age, br = 20)
ggplot(cats, aes(x = Age)) +
  geom_histogram(binwidth = 10) +
  facet_grid(vars(Treatment))

# group / groups
cats %>% count(Treatment)
# sample sizes are pretty unequal; higher mortality in the irradiated groups?

# -- look for relationships between treatment group and other vars
# group and age
cats %>% group_by(Treatment) %>% summarise(mean_age = mean(Age),
                                           median_age = median(Age),
                                           sd_age = sd(Age))

# cancer covariates
cats %>% count(MyeloidLeukemia)
cats %>% count(HarderianTumor)
cats %>% count(PreTLymphoma)

#-- Review Response
cats %>% count(Score)
cats %>% group_by(Sex) %>% count(Score)
cats %>% group_by(Family) %>% summarise(mean_fam_score = mean(as.numeric(Score)))
cats %>% group_by(Treatment) %>% summarise(mean_grp_score = mean(as.numeric(Score)))

cats %>% group_by(Score) %>% count(MyeloidLeukemia)
cats %>% group_by(Score) %>% count(HarderianTumor)
cats %>% group_by(Score) %>% count(PreTLymphoma)

#-- Exploratory Plots - add group sample sizes to these plots
# Score by age
ggplot(cats, aes(x = Score, y = Age, fill = Score)) + geom_boxplot()

# Score by age and sex
# Point plot with jitter
ggplot(cats, aes(x = Score, y = Age, color = Sex)) + geom_jitter(width = 0.2)
# Faceted boxplots
ggplot(cats, aes(x = Score, y = Age, fill = Sex)) + geom_boxplot() + facet_wrap(vars(Sex))
ggplot(cats, aes(x = Sex, y = Age, fill = Score)) + geom_boxplot() + facet_wrap(vars(Score))

# Score by treatment group
# distribution looks similar across groups
ggplot(cats, aes(x = as.numeric(Score))) + geom_histogram(bins = 4) + facet_grid(vars(Treatment))

# Score by age and treatment group
ggplot(cats, aes(x = Score, y = Age, color = Treatment)) + geom_jitter(width = 0.2)
ggplot(cats, aes(x = Score, y = Age, fill = Treatment)) + geom_boxplot() + facet_wrap(vars(Treatment))
ggplot(cats, aes(x = Treatment, y = Age, fill = Score)) + geom_boxplot() + facet_wrap(vars(Score))
# unirradiated mice tend to be older in all groups except score = 2;
# especially note score = 4 ; cataracts due to age?

# Make some plots with the binomial response!

#-- Summary table ideas

# Plot of average score of sex-group-family
# treatment on x, score on y, line for each family
# facet or color by sex
grsex_score <- cats %>%
  group_by(Sex, Treatment, Family) %>%
  summarize(mean_score = mean(Cataracts))


ggplot(grsex_score, aes(x = Treatment, y = mean_score, color = Sex)) +
  geom_line(aes(group = interaction(Family, Sex))) +
  scale_color_startrek() +
  scale_x_discrete(expand = c(0, .1)) +
  theme_light() +
  labs(y = "mean score",
       title = "Catarats by Family, Sex, Treatment Group")

ggplot(grsex_score, aes(x = Treatment, y = mean_score, color = Sex)) +
  geom_line(aes(group = Family)) + facet_grid(vars(Sex)) +
  scale_color_startrek() +
  scale_x_discrete(expand = c(0, .1)) +
  theme_light() +
  labs(y = "mean score",
       title = "Cataracts by Family, Sex, Treatment Group")

ggplot(grsex_score, aes(x = Treatment, y = mean_score, color = Sex)) +
  geom_line(aes(group = Family)) + facet_wrap(vars(Sex)) +
  scale_color_startrek() +
  scale_x_discrete(expand = c(0, .1)) +
  theme_light() +
  labs(y = "mean score",
       title = "Cataracts by Family, Sex, Treatment Group")

# barplot of proportion of each group with cataracts
sex_trt <- cats %>%
  group_by(Treatment, Sex, Cataracts) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Treatment, Sex) %>%
  mutate(prop = round(n/sum(n), digits = 2))
cats_grp <- sex_trt %>%
  filter(Cataracts == 1)
ggplot(cats_grp, aes(x = Sex, y = prop, fill = Treatment)) +
  geom_col(position = "dodge") +
  scale_fill_startrek() +
  theme_light() +
  ggtitle("Sample Proportion with Cataracts by Sex and Treatment Group")
logits <- sex_trt %>%
  group_by(Treatment, Sex) %>%
  summarise(odds_ratio = prop[Cataracts == 1]/prop[Cataracts == 0])
ggplot(logits, aes(x = Sex, y = odds_ratio, fill = Treatment)) +
  geom_col(position = "dodge") +
  scale_fill_startrek() +
  theme_light() +
  labs(y = "Odds Ratio",
       title = "Empirical Odds Ratios of Cataracts by Sex, Treatment")

# table with total counts and proportions by group, with aggregates
trt <- cats %>%
  group_by(Treatment, Cataracts) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Treatment) %>%
  mutate(prop = round(n/sum(n), digits = 2),
         Sex = "Combined") %>%
  relocate(Sex, .after = Treatment)
sex <- cats %>%
  group_by(Sex, Cataracts) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Sex) %>%
  mutate(prop = round(n/sum(n), digits = 2),
         Treatment = "Combined") %>%
  relocate(Treatment)

sum_tab <- sex_trt %>%
  pivot_wider(names_from = Sex, values_from = c(n, prop)) %>%
  mutate(Cataracts = ifelse(Cataracts == 0, "No", "Yes"),
         Treatment = rep(" ", 1)) %>%
  relocate(prop_F, .after = n_F)
n_C <- c(438, 58, 214, 63, 281, 115)
prop_C <- c(0.88, 0.12, 0.77, 0.23, 0.71, 0.29)

sum_tab <- sum_tab %>%
  ungroup() %>%
  mutate(n_C = n_C, prop_C = prop_C) %>%
  add_row(Treatment = " ", Cataracts = "$\\sum$", n_F = 258, prop_F = NA,
          n_M = 238, prop_M = NA, n_C = 496, prop_C = NA, .after = 2) %>%
  add_row(Treatment = " ", Cataracts = "$\\sum$", n_F = 145, prop_F = NA,
          n_M = 132, prop_M = NA, n_C = 277, prop_C = NA, .after = 5) %>%
  add_row(Treatment = " ", Cataracts = "$\\sum$", n_F = 205, prop_F = NA,
          n_M = 191, prop_M = NA, n_C = 396, prop_C = NA, .after = 8) %>%
  add_row(Treatment = " ", Cataracts = "$\\sum$", n_F = 608, prop_F = NA,
          n_M = 561, prop_M = NA, n_C = 1169, prop_C = NA, .after = 9)

options(knitr.kable.NA = '')
kbl(sum_tab,
    caption = "Mice Counts and Percentages by Sex, Treatment Group, Cataracts",
    col.names = c("Treatment", "Cataracts", "n", "prop", "n", "prop", "n", "prop")) %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_classic_2(full_width = F) %>%
  add_header_above(c(" " = 2, "Female" = 2, "Male" = 2, "$\\sum$" = 2)) %>%
  pack_rows(index = c("Control" = 3, "Gamma" = 3, "HZE" = 3, "$\\sum$" = 1))

# score by group, family - too long
fam_scores <- cats %>% group_by(Family, Treatment) %>%
  summarise(mean_score = mean(as.numeric(Score))) %>%
  pivot_wider(names_from = Treatment, values_from = mean_score) %>%
  kbl(caption = "Mean Cataract Score by Group within Family", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Cambria")

# counts of score by group
grs_ct <- cats %>% group_by(Score) %>%
  count(Treatment) %>%
  pivot_wider(names_from = Score, values_from = n) %>%
  group_by(Treatment) %>%
  mutate(Total = sum(c(`1`, `2`, `3`, `4`))) %>%
  kbl(caption = "Counts of Score by Treatment") %>%
  add_header_above(c( " " = 1, "Cataract Score" = 4, " " = 1)) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 14)

# plot of score by group faceted by family
ggplot(cats, aes(x = Treatment, y = Score, color = Treatment)) + geom_jitter(width = 0.2) +
  facet_wrap(vars(Family)) +
  scale_color_startrek() +
  scale_x_discrete(labels = c("Gam", "HZE", "none")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Treatment Group", y = "Cataract Score", color = "Group",
       title = "Cataract Score by Treatment, faceted by Family")

# save eda plots for presentation
png(filename = "eda.png", units = "in", width = 12, height = 7, res = 300)
l1 <- ggplot(grsex_score, aes(x = Treatment, y = mean_score, color = Sex)) +
  geom_line(aes(group = Family)) + facet_grid(vars(Sex)) +
  scale_color_startrek() +
  scale_x_discrete(expand = c(0, .2)) +
  theme_light() +
  labs(y = "mean score",
       title = "Cataract Score \nby Family, Sex, Treatment Group")
b1 <- ggplot(cats_grp, aes(x = Sex, y = prop, fill = Treatment)) +
  geom_col(position = "dodge") +
  scale_fill_startrek() +
  theme_light() +
  ggtitle("Cataract Sample Proportion \nby Sex, Treatment Group")
grid.arrange(b1, l1, ncol = 2)
dev.off()

# -- Predictor Correlation

# Numeric Variable Correlations
cor(cats$Weight, cats$Age)

# Categorical Variable Correlations
catcors <- cats %>%
  summarise(sex_coat = Lambda(Sex, CoatColor, direction = "symmetric"),
            sex_fam = Lambda(Sex, Family, direction = "symmetric"),
            sex_BCS = Lambda(Sex, BCS, direction = "symmetric"),
            sex_group = Lambda(Sex, Treatment, direction = "symmetric"),
            sex_leuk = Lambda(Sex, MyeloidLeukemia, direction = "symmetric"),
            sex_hard = Lambda(Sex, HarderianTumor, direction = "symmetric"),
            sex_lymph = Lambda(Sex, PreTLymphoma, direction = "symmetric"),
            sex_score = Lambda(Sex, Score, direction = "symmetric"),
            coat_fam = Lambda(CoatColor, Family, direction = "symmetric"),
            coat_BCS = Lambda(CoatColor, BCS, direction = "symmetric"),
            coat_group = Lambda(CoatColor, Treatment, direction = "symmetric"),
            coat_leuk = Lambda(CoatColor, MyeloidLeukemia, direction = "symmetric"),
            coat_hard = Lambda(CoatColor, HarderianTumor, direction = "symmetric"),
            coat_lymph = Lambda(CoatColor, PreTLymphoma, direction = "symmetric"),
            coat_score = Lambda(CoatColor, Score, direction = "symmetric"),
            fam_BCS = Lambda(Family, BCS, direction = "symmetric"),
            fam_group = Lambda(Family, Treatment, direction = "symmetric"),
            fam_leuk = Lambda(Family, MyeloidLeukemia, direction = "symmetric"),
            fam_hard = Lambda(Family, HarderianTumor, direction = "symmetric"),
            fam_lymph = Lambda(Family, PreTLymphoma, direction = "symmetric"),
            fam_score = Lambda(Family, Score, direction = "symmetric"),
            group_leuk = Lambda(Treatment, MyeloidLeukemia, direction = "symmetric"),
            group_hard = Lambda(Treatment, HarderianTumor, direction = "symmetric"),
            group_lymph = Lambda(Treatment, PreTLymphoma, direction = "symmetric"),
            group_score = Lambda(Treatment, Score, direction = "symmetric"),
            score_leuk = Lambda(Score, MyeloidLeukemia, direction = "symmetric"),
            score_hard = Lambda(Score, HarderianTumor, direction = "symmetric"),
            score_lymph = Lambda(Score, PreTLymphoma, direction = "symmetric"))
catcors <- as.data.frame(t(round(catcors, digits = 3)))
colnames(catcors) <- "Cor"
catcors
# coat color and family appear to have a correlation, probably due to genetics

# Categorical v. Numeric Variable Correlations
# adjust p-values by total number of comparisons = .05/8 = .00625

# Weight
summary(aov(Weight ~ Sex, data = cats))
kruskal.test(Weight ~ Sex, data = cats)

summary(aov(Weight ~ CoatColor, data = cats))
kruskal.test(Weight ~ CoatColor, data = cats)

summary(aov(Weight ~ Family, data = cats))
kruskal.test(Weight ~ Family, data = cats)

summary(aov(Weight ~ BCS, data = cats))
kruskal.test(Weight ~ BCS, data = cats)

summary(aov(Weight ~ Treatment, data = cats))
kruskal.test(Weight ~ Treatment, data = cats)

summary(aov(Weight ~ MyeloidLeukemia, data = cats))
kruskal.test(Weight ~ MyeloidLeukemia, data = cats)
summary(aov(Weight ~ HarderianTumor, data = cats))
kruskal.test(Weight ~ HarderianTumor, data = cats)
summary(aov(Weight ~ PreTLymphoma, data = cats))
kruskal.test(Weight ~ PreTLymphoma, data = cats)

# Age
summary(aov(Age ~ Sex, data = cats))
kruskal.test(Age ~ Sex, data = cats)

summary(aov(Age ~ CoatColor, data = cats))
kruskal.test(Age ~ CoatColor, data = cats)

summary(aov(Age ~ Family, data = cats))
kruskal.test(Age ~ Family, data = cats)

summary(aov(Age ~ BCS, data = cats))
kruskal.test(Age ~ BCS, data = cats)

summary(aov(Age ~ Treatment, data = cats))
kruskal.test(Age ~ Treatment, data = cats)

summary(aov(Age ~ MyeloidLeukemia, data = cats))
kruskal.test(Age ~ MyeloidLeukemia, data = cats)
summary(aov(Age ~ HarderianTumor, data = cats))
kruskal.test(Age ~ HarderianTumor, data = cats)
summary(aov(Age ~ PreTLymphoma, data = cats))
kruskal.test(Age ~ PreTLymphoma, data = cats)


#-- Logistic Regression

# fit initial model, look at model diagnostics
mod0 <- glmer(Cataracts ~ Treatment + (1|Family), data = cats, family = binomial,
              control = glmerControl(optimizer = "bobyqa"))
summary(mod0)
plot_model(mod0, sort.est = TRUE, show.values = TRUE,
           color = "Dark2", vline.color = "darkorchid3",
           width = 0.1, title = "Model 0: Cataracts Odds Ratios by Treatment Group")
tab_model(mod0, show.re.var = TRUE,
          pred.labels = c("Unirradiated", "Gamma", "HZE"),
          dv.labels = "Model 0 Effects of Treatment on Cataracts")

# what about a model with a random slope for family?
mod1 <- glmer(Cataracts ~ Treatment + (Treatment|Family),
              data = cats, family = binomial,
              control = glmerControl(optimizer = "bobyqa"))
summary(mod1)
plot_model(mod1, sort.est = TRUE, show.values = TRUE,
           color = "Dark2", vline.color = "darkorchid3",
           width = 0.1, title = "Model 1: Cataracts Odds Ratios by Treatment Group")
tab_model(mod1, show.re.var = TRUE,
          pred.labels = c("Unirradiated", "Gamma", "HZE"),
          dv.labels = "Model 1 Effects of Treatment on Cataracts")

modfull <- glmer(Cataracts ~ Treatment + scale(Age) + scale(Weight) +
                   Sex + MyeloidLeukemia + BCS + CoatColor +
                   HarderianTumor + PreTLymphoma + (1|Family),
                 data = cats, family = binomial,
                 control = glmerControl(optimizer = "bobyqa"))
summary(modfull)
# Of covariates, only Sex looks significant


# Final model
# add exploratory plot showing differences by sex
summary(modsex)
mod <- glmer(Cataracts ~ Treatment*Sex + (1|Family), data = cats, family = binomial)
summary(mod)
fixmod <- glm(Cataracts ~ Treatment*Sex, data = cats, family = binomial)
summary(fixmod)

AIC(mod0, modfull, modsex, mod, fixmod)

# Extract estimates for fixed and random effects
coefs <- data.frame(coef(mod)$Family) %>%
  rename(Intercept = X.Intercept.,
         Gamma_F = TreatmentGamma,
         HZE_F = TreatmentHZE,
         M = SexM,
         Gamma_M = TreatmentGamma.SexM,
         HZE_M = TreatmentHZE.SexM) %>%
  rownames_to_column("Family")
pred_dat <- cats %>%
  select(c(Treatment, Sex, Family, Cataracts)) %>%
  left_join(coefs, by = "Family") %>%
  mutate(Fits = fitted(mod))
pred_dat <- pred_dat %>%
  mutate(FixEff = ifelse(Treatment == "Unirradiated" & Sex == "F", 0,
                         ifelse(Treatment == "Gamma" & Sex == "F", Gamma_F,
                                ifelse(Treatment == "HZE" & Sex == "F", HZE_F,
                                       ifelse(Treatment == "Unirradiated" & Sex == "M", M,
                                              ifelse(Treatment == "Gamma" & Sex == "M",Gamma_M, HZE_M))))))
# think about how to plot this!
ggplot(pred_dat, aes(x = Treatment, y = Fits, group = Family)) +
  geom_point() +
  geom_abline(aes(intercept = Intercept,
                  slope = FixEff*Cataracts))

# -- Post-Hoc Fixed Effect Analysis
cats_emms <- emmeans(mod, ~ Treatment | Sex, infer = TRUE, type = "response")
cats_emms
pairs(cats_emms, reverse = TRUE)

# save lineplot of probs for presentation
png(filename = "est_probs_plot.png", units = "in", width = 6, height = 6, res = 300)
emmip(cats_emms, Treatment ~ Sex) + theme_light() +
  ggtitle("Estimated Marginal Probabilities of \nCataracts by Sex, Treatment Group") + scale_color_startrek()
dev.off()


# -- Bayesian Logistic Regression
# note: when moving to rmarkdown, make sure to specify/compile model in separate chunks!
library(coda)
library(rjags)
library(R2jags)

# Specify the model
cat("model{
  for(i in 1:N){
    CAT[i] ~ dbern(p[i])     # Bernoulli-distributed response
    logit(p[i]) <- b0 + b1*Gamma[i] + b2*HZE[i] + b3*Male[i] +
    b4*Male[i]*Gamma[i] + b5*Male[i]*HZE[i] + a[Family[i]]   # likelihood function
  }
  for(j in 1:nFam){
    a[j] ~ dnorm(0, tau)
  }
  b0 ~ dnorm(0.0, 1.0E-3)   # vaguely informative priors
  b1 ~ dnorm(0.0, 1.0E-3)
  b2 ~ dnorm(0.0, 1.0E-3)
  b3 ~ dnorm(0.0, 1.0E-3)
  b4 ~ dnorm(0.0, 1.0E-3)
  b5 ~ dnorm(0.0, 1.0E-3)
  tau ~ dgamma(1.0E-3,1.0E-3)
  sigma2 <- 1/tau     # convert precision 'tau' to variance 'sigma2'
}", file = "cat.jag")

# Prepare the data for JAGS
# break Treatment into dummy variables for each group
treatment <- model.matrix(~ Treatment - 1, cats)
sex <- model.matrix(~Sex -1, cats)
colnames(treatment) <- c("Unirradiated", "Gamma", "HZE")
cats <- data.frame(cats, treatment, sex)

# format relevant data as a list
data <- list(CAT = cats$Cataracts, Gamma = cats$Gamma, HZE = cats$HZE, Male = cats$SexM,
             Family = cats$Family, nFam = length(unique(cats$Family)), N = nrow(cats))

# Setup
nIter <- 60000
nChains <- 3
nThin <- 1
BurnIn <- 10000
nAdapt <- 1000
ests <- summary(mod)$coef[,1] # pull starting values from frequentist model
var <- as.numeric(as.data.frame(VarCorr(mod))$vcov)
inits <- list(list("tau" = var+0.2, "b0" = ests[1]+5, "b1" = ests[2]+5, "b2" = ests[3]+5,
                   "b3" = ests[4]+2, "b4" = ests[5]+2, "b5" = ests[6]+2),
              list("tau" = var-0.2, "b0" = ests[1]-0.5, "b1" = ests[2]-0.5, "b2" = ests[3]-0.5,
                   "b3" = ests[4]-0.2, "b4" = ests[5]-0.2, "b5" = ests[6]-0.2),
              list("tau" = var, "b0" = ests[1], "b1" = ests[2], "b2" = ests[3],
                   "b3" = ests[4], "b4" = ests[5], "b5" = ests[6]))

# -- Compile and run the model
params <- c("b0", "b1", "b2", "b3", "b4", "b5", "sigma2")
set.seed(556)
model.fit <- jags(data = data,
                  inits = inits,
                  parameters.to.save = params,
                  model.file = "cat.jag",
                  n.chains = nChains,
                  n.iter = nIter,
                  n.burnin = BurnIn,
                  n.thin = nThin)

# Model Diagnostics, Plots
library(superdiag)
library(mcmcplots)
library(ggmcmc)

mcmc.model <- as.mcmc(model.fit)
summary(mcmc.model)
plot(mcmc.model)
gelman.diag(mcmc.model)
gelman.plot(mcmc.model)

traplot(mcmc.model, parms = c("b0", "b1", "b2", "b3", "b4", "b5", "sigma2"))
denplot(mcmc.model, parms = params)
acf(mcmc.model[[3]][,1])
acf(mcmc.model[[3]][,2])
acf(mcmc.model[[3]][,3])
acf(mcmc.model[[3]][,4])
acf(mcmc.model[[3]][,5])
acf(mcmc.model[[3]][,6])
acf(mcmc.model[[3]][,8])


caterplot(mcmc.model, parms = c("b0", "b1", "b2", "b3", "b4", "b5", "sigma2")) # plot of cred intervals
ggmcmc.model <- ggs(mcmc.model)
ggs_density(ggmcmc.model)
hpds <- HPDinterval(mcmc.model[[3]])

# Convert posterior distributions to odd ratios
posts <- mcmc.model[[3]][,-7]
phpds <- HPDinterval(posts)
posts <- data.frame(posts)

# Create table of posterior estimates
means <- apply(posts, 2, mean)
medians <- apply(posts, 2, median)
mode_fun <- function(x) {
  ux <- round(unique(x), digits = 3)
  return(ux[which.max(tabulate(match(x, ux)))])
}
modes <- apply(round(posts, 4), 2, mode_fun)
sds <- apply(posts, 2, sd)
bayes_tab <- round(data.frame(c(ests, var), means, medians, modes, sds, phpds), digits = 3)
rownames(bayes_tab) <- c("$\\hat{\\beta_0}$", "$\\hat{\\beta_1}$", "$\\hat{\\beta_2}$",
                         "$\\hat{\\beta_3}$", "$\\hat{\\beta_4}$", "$\\hat{\\beta_5}$", "$\\hat{\\sigma^2}$")

kbl(bayes_tab,
    caption = "Final Model  Statistics", row.names = TRUE,
    col.names = c("GLMM Est", "MCMC Mean", "MCMC Median", "MCMC Mode", "MCMC SD", "HPD Lower", "HPD Upper")) %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_classic_2(full_width = F, html_font = "Cambria")




# Posterior density plots with HPD intervals

ggplot(posts, aes(x = b0)) +
  geom_density(color = "cyan4", fill = "aquamarine4", alpha = 0.5) +
  geom_vline(aes(xintercept = bayes_tab[1,5]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[1,6]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[1,2]), color = "darkorange3") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_light() +
  labs(x = "Probability",
       title = "Female Control: Posterior Density with 95% HPD Interval")

ggplot(posts, aes(x = b1)) +
  geom_density(color = "cyan4", fill = "aquamarine4", alpha = 0.5) +
  geom_vline(aes(xintercept = bayes_tab[2,5]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[2,6]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[2,2]), color = "darkorange3") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_light() +
  labs(x = "Probability",
       title = "Female Gamma: Posterior Density with 95% HPD Interval")

ggplot(posts, aes(x = b2)) +
  geom_density(color = "cyan4", fill = "aquamarine4", alpha = 0.5) +
  geom_vline(aes(xintercept = bayes_tab[3,5]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[3,6]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[3,2]), color = "darkorange3") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_light() +
  labs(x = "Probability",
       title = "Female HZE: Posterior Density with 95% HPD Interval")

ggplot(posts, aes(x = b3)) +
  geom_density(color = "cyan4", fill = "aquamarine4", alpha = 0.5) +
  geom_vline(aes(xintercept = bayes_tab[4,5]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[4,6]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[4,2]), color = "darkorange3") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_light() +
  labs(x = "Probability",
       title = "Male Control: Posterior Density with 95% HPD Interval")

ggplot(posts, aes(x = b4)) +
  geom_density(color = "cyan4", fill = "aquamarine4", alpha = 0.5) +
  geom_vline(aes(xintercept = bayes_tab[5,5]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[5,6]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[5,2]), color = "darkorange3") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_light() +
  labs(x = "Probability",
       title = "Male Gamma: Posterior Density with 95% HPD Interval")

ggplot(posts, aes(x = b5)) +
  geom_density(color = "cyan4", fill = "aquamarine4", alpha = 0.5) +
  geom_vline(aes(xintercept = bayes_tab[6,5]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[6,6]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[6,2]), color = "darkorange3") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_light() +
  labs(x = "Probability",
       title = "Male HZE: Posterior Density with 95% HPD Interval")


ggplot(posts, aes(x = sigma2)) +
  geom_density(color = "cyan4", fill = "aquamarine4", alpha = 0.5) +
  geom_vline(aes(xintercept = bayes_tab[7,6]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[7,7]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[7,3]), color = "darkorange3") + # calculate mode and replot
  scale_x_continuous(limits = c(0, 1.0), expand = c(0, 0)) +
  theme_light() +
  labs(x = "Probability",
       title = "Random Family Variance: Posterior Density with 95% HPD Interval")

# parameterize the random effect using the mode = 1.41 and the sd = 0.31:
# Family ~ Gamma(shape = mode^2/sd^2, rate = mode/sd^2)

# Final model contains fixed effects for Treatment, Sex, an interaction term, and a random effect for Family
plot_model(mod, sort.est = TRUE, show.values = TRUE, type = "int", pred.type = "re",
           color = "Dark2", show.p = TRUE,
           width = 0.5, title = "Final Model: Cataracts Odds Ratios by Sex, Treatment Group")
tab_model(mod, show.re.var = TRUE,
          pred.labels = c("Control", "Gamma", "HZE",
                          "Control(Male)", "Gamma(Male)", "HZE(Male)"),
          dv.labels = "Final Model Effects of Treatment on Cataracts")

# Extract estimates for fixed and random effects
coefs <- data.frame(coef(mod)$Family)
coefs <- coefs %>%
  rename(Intercept = X.Intercept., B1 = TreatmentGamma, B2 = TreatmentHZE,
         B3 = SexM, B4 = TreatmentGamma.SexM, B5 = TreatmentHZE.SexM) %>%
  rownames_to_column(var = "Family") %>%
  mutate(Family = as.factor(Family))
preds <- cats %>%
  select(c(Animal, Family, Cataracts, Gamma, HZE, SexM)) %>%
  rename(Sex = SexM) %>%
  left_join(coefs, by = "Family") %>%
  mutate(LogOdds = Intercept + B1*Gamma + B2*HZE + B3*Sex +
           B4*Gamma*Sex + B5*HZE*Sex)
preds <- preds %>%
  mutate(Odds = exp(LogOdds))
preds <- preds %>%
  mutate(Probs = Odds / (1 + Odds)) # fitted values!(

cats <- cats %>%
  mutate(Probs = fitted(mod))

ggplot(cats, aes(x = Probs, y = Cataracts)) +
  geom_point()


# -- Post-Hoc Fixed Effect Analysis
cats_emms <- emmeans(mod, ~ Treatment | Sex, infer = TRUE, type = "response")
cats_emms
pairs(cats_emms, reverse = TRUE)

# save lineplot of probs for presentation
png(filename = "est_probs_plot.png", units = "in", width = 6, height = 6, res = 300)
emmip(cats_emms, Treatment ~ Sex) + theme_light() +
  ggtitle("Estimated Marginal Probabilities of \nCataracts by Sex, Treatment Group") + scale_color_startrek()
dev.off()

p_var <- exp(var)/(1+exp(var))
sigs <- bayes_tab[7,]
p_sigs <- exp(sigs)/(1+exp(sigs))
psig <- as.numeric(p_sigs[4])
hpdl <- as.numeric(p_sigs[6])
hpdu <- as.numeric(p_sigs[7])
REs <- augment(ranef(mod,condVar = TRUE), ci.level = 0.95) %>%
  select(c(level, estimate, lb, ub)) %>%
  rename(Family = level) %>%
  mutate(Prob = exp(estimate)/(1+exp(estimate)),
         Lower = exp(lb)/(1+exp(lb)),
         Upper = exp(ub)/(1+exp(ub)))
colors <- c("Family Effect" = "#5C88DAFF", "GLMM Est" = "#CC0C00FF", "Bayes Mode" = "#84BD00FF",
            "HPD Interval" = "#FFCD00FF")
re_plot <- ggplot(REs, aes(x = Prob, y = Family, xmin = Lower, xmax = Upper)) +
  geom_errorbarh(aes(height = 0, color = "Family Effect")) +
  geom_point(aes(color = "Family Effect")) +
  geom_vline(aes(xintercept = p_var, color = "GLMM Est"), lwd = 1, lty = 2) +
  geom_vline(aes(xintercept = psig, color = "Bayes Mode"), lwd = 1, lty = 2) +
  geom_vline(aes(xintercept = hpdl, color = "HPD Interval"), lwd = 1, lty = 4) +
  geom_vline(aes(xintercept = hpdu, color = "HPD Interval"), lwd = 1, lty = 4) +
  theme_light() +
  scale_color_manual(values = colors) +
  labs(color = "", title = "Random Effect by Family")

png(filename = "re_plot.png", units = "in", width = 8, height = 6, res = 300)
re_plot
dev.off()

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
  theme_minimal() +
  ggtitle("Sample Proportion with Cataracts by Sex and Treatment Group")

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
modsex <- glmer(Cataracts ~ 0 + Treatment + Sex + (1|Family), data = cats, family = binomial)
mod <- glmer(Cataracts ~ Treatment*Sex + (1|Family), data = cats,
             family = binomial, glmerControl(optimizer = "Nelder_Mead"))

mod <- glmer(Cataracts ~ 0 + Treatment*Sex + (1|Family), data = cats, family = binomial)
summary(modsex)
summary(mod)

plot_model(mod, sort.est = TRUE, show.values = TRUE, type = "int", pred.type = c("fe", "re"),
           color = "Dark2", vline.color = "darkorchid3",
           width = 0.1, title = "Final Model: Cataracts Odds Ratios by Sex, Treatment Group")

tab_model(mod, show.re.var = TRUE,
          pred.labels = c("Control(Female)", "Gamma(Female)", "HZE(Female)",
                          "Control(Male)", "Gamma(Male)", "HZE(Male)"),
          dv.labels = "Final Model Effects of Treatment on Cataracts")


# -- Bayesian Logistic Regression
# note: when moving to rmarkdown, make sure to specify/compile model in separate chunks!
library(coda)
library(rjags)
library(R2jags)

# Specify the model
cat("model{
  for(i in 1:N){
    CAT[i] ~ dbern(p[i])     # Bernoulli-distributed response
    logit(p[i])<- b0*Unirradiated[i] + b1*Gamma[i] + b2*HZE[i] + b3*Male[i] +
    b4*Male[i]*Gamma[i] + b5*Male[i]*HZE[i] + a[Family[i]]   # likelihood function
  }
  for(j in 1:nFam){
    a[j] ~ dnorm(0, tau)
  }
  b0 ~ dnorm(0.0, 1.0E-4)   # vaguely informative priors
  b1 ~ dnorm(0.0, 1.0E-4)
  b2 ~ dnorm(0.0, 1.0E-4)
  b3 ~ dnorm(0.0, 1.0E-4)
  b4 ~ dnorm(0.0, 1.0E-4)
  b5 ~ dnorm(0.0, 1.0E-4)
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
data <- list(CAT = cats$Cataracts, Unirradiated = cats$Unirradiated, Gamma = cats$Gamma, HZE = cats$HZE, Male = cats$SexM,
             Family = cats$Family, nFam = length(unique(cats$Family)), N = nrow(cats))

# Setup
nIter <- 60000
nChains <- 3
nThin <- 1
BurnIn <- 10000
nAdapt <- 1000
ests <- summary(mod)$coef[,1] # pull starting values from frequentist model
var <- as.numeric(as.data.frame(VarCorr(mod))$vcov)
inits <- list(list("tau" = var+0.2, "b0" = ests[1]+0.5, "b1" = ests[2]+0.5, "b2" = ests[3]+0.5,
                   "b3" = ests[4]+0.2, "b4" = ests[5]+0.2, "b5" = ests[6]+0.2),
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

traplot(mcmc.model, parms = params)
denplot(mcmc.model, parms = params)

caterplot(mcmc.model, parms = c("b0", "b1", "b2", "b3", "b4", "b5", "sigma2")) # plot of cred intervals
ggmcmc.model <- ggs(mcmc.model)
ggs_density(ggmcmc.model)
hpds <- HPDinterval(mcmc.model[[3]])

# Convert posterior distributions to odd ratios
ors <- exp(mcmc.model[[3]][,-7])
or_hpds <- round(HPDinterval(ors), 3)
posts <- data.frame(ors)

# Create table of posterior estimates
means <- apply(posts, 2, mean)
medians <- apply(posts, 2, median)
mode_fun <- function(x) {
  ux <- round(unique(x), digits = 3)
  return(ux[which.max(tabulate(match(x, ux)))])
}
modes <- apply(round(posts, 4), 2, mode_fun)
sds <- apply(posts, 2, sd)
bayes_tab <- round(data.frame(means, medians, modes, sds, or_hpds), digits = 3)
rownames(bayes_tab) <- c("b0", "b1", "b2", "b3", "b4", "b5", "$\\sigma^2$")
kbl(bayes_tab,
    caption = "Bayes Model: Posterior Distribution Statistics",
    col.names = c("Mean", "Median", "Mode", "SD", "HPD Lower", "HPD Upper")) %>%
  kable_styling(latex_options = "hold_position") %>%
  kable_classic_2(full_width = F, html_font = "Cambria")

# Posterior density plots with HPD intervals
ggplot(posts, aes(x = sigma2)) +
  geom_density(color = "cornflowerblue", fill = "cornflowerblue", alpha = 0.5) +
  geom_vline(aes(xintercept = bayes_tab[7,5]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[7,6]), color = "darkorchid", linetype = "dashed") +
  geom_vline(aes(xintercept = bayes_tab[7,3]), color = "red") + # calculate mode and replot
  scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
  theme_minimal() +
  labs(title = "Posterior Density of Random Family Effect with 95% HPD Interval")

# parameterize the random effect using the mode = 1.41 and the sd = 0.31:
# Family ~ Gamma(shape = mode^2/sd^2, rate = mode/sd^2)






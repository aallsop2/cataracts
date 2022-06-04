### Cataracts Project
### STAA556

#-- Load libraries
library(readxl)
library(tidyverse)
library(kableExtra)
library(DescTools)
library(ggsci)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(influence.ME)

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
# spike ~ 800 days old, otherwise fairly even

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
ggplot(cats, aes(x = Score, y = Age)) + geom_boxplot()

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
mod0 <- glmer(cataracts ~ groups + (1|fam), data = cats, family = binomial,
              control = glmerControl(optimizer = "bobyqa"))
summary(mod0)
isSingular(mod0)
tidy(mod0, effects = c("ran_pars", "fixed"),
     component = c("cond"),
     conf.int = TRUE, data = cats)
mod0_diag <- augment(mod0)
str(mod0_diag)
plot(mod0, pch = 20, col = "black", lty = "dotted",
     main = "Base Model : Residuals by Fitted Values")
# why do the diagnostics look so strange?
# explore influence.ME package to review glmm model diagnostics?

# -- Nonparametric Bootstrap

# -- MC simulation

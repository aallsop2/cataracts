library(readxl)
library(tidyverse)
library(kableExtra)

# -- Read in the data
cats <- read_excel("GRSD.cataract.xlsx", sheet = "Sheet1")
head(cats)
str(cats)

#-- Data cleaning

# remove spaces from column and value names
names(cats) <- str_replace_all(names(cats), " ", "_")
cats <- cats %>%
  mutate(coat_color = str_replace_all(coat_color, " ", "_"))
str(cats)

# turn categorical vars into factor
cats <- cats %>%
  rename(age = `age_(days)`) %>%
  mutate(sex = as.factor(sex),
         coat_color = as.factor(coat_color),
         fam = as.factor(family), # should this stay a factor? Yes?
         BCS = as.factor(BCS),
         group = as.factor(group),
         groups = as.factor(groups),
         myeloid_leuk = as.factor(Myeloid_Leukemia),
         harderian_tumor = as.factor(Harderian_Tumor),
         preT_lymph = as.factor(PreT_Lymphoma),
         cat_score = as.ordered(Cataract_Score)) # ordinal cat; leave as numeric?

#-- Review each predictor/covariate

# individual
length(unique(cats$animal))

# sex
cats %>% count(sex)
# even enough split between gender

# weight
summary(cats$weight)
hist(cats$weight, br = 20)
# distribution for weight looks symmetric

# coat color - probably irrelevant?
cats %>% count(coat_color)
cats %>% group_by(coat_color) %>% count(sex)
cats %>% group_by(family) %>% count(coat_color)

# family !random effect of interest!
length(unique(cats$fam))
fam_counts <- cats %>% group_by(fam) %>% count()
summary(fam_counts$n)
hist(fam_counts$n, br = 15) # large variation in family size; watch for this!

# BCS - body condition score
cats %>% count(BCS)
# BCS looks correlated with weight; maybe don't use both in the model
summary(aov(weight ~ BCS, data = cats))
kruskal.test(weight ~ BCS, data = cats)

# age
range(cats$age)
hist(cats$age, br = 20)
# spike ~ 800 days old, otherwise fairly even

# group / groups
cats %>% count(group)
cats %>% count(groups)
# sample sizes are pretty unequal; higher mortality in the irradiated groups?

# cancer covariates
cats %>% count(myeloid_leuk)
cats %>% count(harderian_tumor)
cats %>% count(preT_lymph)

#-- Review Response
cats %>% count(cat_score)
cats %>% group_by(sex) %>% count(cat_score)
cats %>% group_by(fam) %>% summarise(mean_fam_score = mean(as.numeric(cat_score)))
cats %>% group_by(groups) %>% summarise(mean_grp_score = mean(as.numeric(cat_score)))

cats %>% group_by(cat_score) %>% count(myeloid_leuk)
cats %>% group_by(cat_score) %>% count(harderian_tumor)
cats %>% group_by(cat_score) %>% count(preT_lymph)

#-- Exploratory Plots - add group sample sizes to these plots
# Score by age
ggplot(cats, aes(x = cat_score, y = age)) + geom_boxplot()

# Score by age and sex
# Point plot with jitter
ggplot(cats, aes(x = cat_score, y = age, color = sex)) + geom_jitter(width = 0.2)
# Faceted boxplots
ggplot(cats, aes(x = cat_score, y = age, fill = sex)) + geom_boxplot() + facet_wrap(vars(sex))
ggplot(cats, aes(x = sex, y = age, fill = cat_score)) + geom_boxplot() + facet_wrap(vars(cat_score))

# Score by treatment group
# distribution looks similar across groups
ggplot(cats, aes(x = as.numeric(cat_score))) + geom_histogram(bins = 4) + facet_grid(vars(groups))

# Score by age and treatment group
ggplot(cats, aes(x = cat_score, y = age, color = groups)) + geom_jitter(width = 0.2)
ggplot(cats, aes(x = cat_score, y = age, fill = groups)) + geom_boxplot() + facet_wrap(vars(groups))
ggplot(cats, aes(x = groups, y = age, fill = cat_score)) + geom_boxplot() + facet_wrap(vars(cat_score))
# unirradiated mice tend to be older in all groups except score = 2;
# especially note score = 4 ; cataracts due to age?

#-- Summary table ideas

# score by group, family - too long
fam_scores <- cats %>% group_by(family, groups) %>%
  summarise(mean_score = mean(as.numeric(cat_score))) %>%
  pivot_wider(names_from = groups, values_from = mean_score) %>%
  kbl(caption = "Mean Cataract Score by Group within Family", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Cambria")




#-- Analysis


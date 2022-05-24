library(readxl)
library(tidyverse)
library(lmerTest)
library(broom.mixed)

cataracts <- read_excel("C:/Users/allso/Documents/Grad School/STAA 556 Statistical Consulting/GRSD.cataract.xlsx")
cataracts$groups <- as.factor(cataracts$groups)
cataracts$cataract_score <- cataracts$`Cataract Score`
cataracts$coat_color <- cataracts$`coat color`
cataracts$age <- cataracts$`age (days)`
cataracts$myeloid_leukemia <- cataracts$`Myeloid Leukemia`
cataracts$harderian_tumor <- cataracts$`Harderian Tumor`
cataracts$pret_lymphoma <- cataracts$`PreT Lymphoma`

nrow(cataracts %>% 
       filter(groups == "HZE"))
nrow(cataracts %>% 
       filter(groups == "Gamma"))
nrow(cataracts %>% 
       filter(groups == "Unirradiated"))

# boxplot of cataract score by group -------

ggplot(data = cataracts, aes(x = groups, y = cataract_score)) +
  geom_boxplot()


# naive model/full model ----------

full_mod <- lmer(cataract_score ~ sex + weight + BCS +
                   age + groups + myeloid_leukemia + harderian_tumor +
                   pret_lymphoma + (1|family) + (1|coat_color), data = cataracts)
summary(full_mod)


# check model assumptions ------------

diagd <- augment(full_mod)
str(daigd)
ggplot(diagd, aes(sample = .resid)) + stat_qq() + facet_grid(~groups)
ggplot(diagd, aes(sample = .resid)) + stat_qq() + facet_grid(~coat_color)
ggplot(diagd, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0)

#These do not look good. Need to play around with different models. Maybe do a transformation?













library(readxl)
library(tidyverse)
library(lmerTest)
library(broom.mixed)

cats <- read_excel("C:/Users/allso/Documents/Grad School/STAA 556 Statistical Consulting/GRSD.cataract.xlsx")
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
qqnorm(resid(full_mod))

# check model assumptions ------------

diagd <- augment(full_mod)
str(diagd)
ggplot(diagd, aes(sample = .resid)) + stat_qq() + facet_grid(~groups)
ggplot(diagd, aes(sample = .resid)) + stat_qq() + facet_grid(~coat_color)
ggplot(diagd, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0)

#These do not look good. Need to play around with different models. Maybe do a transformation?


# create column of 0s and 1s for response
cataracts <- cataracts %>%
  mutate(cataract_status = ifelse(cataract_score >= 2, 1, 0))

# fit binary response model ------
binary_mod <- glmer(cataract_status ~ sex + weight + BCS +
                     age + groups + myeloid_leukemia + harderian_tumor +
                     pret_lymphoma + coat_color + (1|family),
                    family = binomial, data = cataracts)


diagd <- augment(binary_mod)
str(diagd)
ggplot(diagd, aes(sample = .resid)) + stat_qq() + facet_grid(~groups)
ggplot(diagd, aes(sample = .resid)) + stat_qq() + facet_grid(~coat_color)
ggplot(diagd, aes(sample = .resid)) + stat_qq()
ggplot(diagd, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0)

qqnorm(resid(binary_mod))

binary_mod2 <- glmer(cataract_status ~ sex + weight + BCS +
                      groups + myeloid_leukemia + harderian_tumor +
                      pret_lymphoma + coat_color + (1|family),
                    family = binomial, data = cataracts)
summary(binary_mod2)
qqnorm(resid(binary_mod2))


binary_mod3 <- glmer(cataract_status ~ sex + weight +
                       groups + (1|family),
                     family = binomial, data = cataracts)
summary(binary_mod3)
qqnorm(resid(binary_mod3), main = "mod 3")

sub_cat <-cataracts[,c(3,5,7)]
pairs(sub_cat)
plot(cataracts$family, cataracts$weight)










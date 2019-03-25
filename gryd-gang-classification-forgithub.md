---
title: "GRYD Gang Classification: Logistic Regression"
author: "Ben Shulman"
date: "12/15/2018"
output:
  html_document:
    keep_md: true
    toc: true
editor_options: 
  chunk_output_type: console
---



This is an analysis of three groups' likelihoods to classify crimes as gang-related. These results are still in preparation, so the analyses shown here use synthetic data, and do not reflect any real-world patterns.

I first model how the number of gang-characteristics relates to each rating group's likelihood of labeling a crime as gang-related.
I also use these models to calculate each rating group's decision cut-points -- the number of characteristics for which their odds of classification pass the 50:50 mark.
I then model how each characteristic independently relates to each rater's classifications. 


```r
# libraries
library(tidyverse)
library(emmeans)
library(reghelper)
library(interactions)
library(broom)
library(viridis)
library(cowplot)
library(kableExtra)

# plot theme
theme_set(theme_classic() +
            theme(axis.title.y = element_text(angle = 0,
                                              vjust = 0.5),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  strip.text.x = element_text(size = rel(1.2))
                  ))

# read in gang classification data
gcdat <- read_csv("/Users/Ben/Dropbox/PhD/Research/gryd/gang-classification/for github/gryd-gang-classification-synthdat.csv",
                  na = "NA",
                  col_names = c(
                    "motivated",
                    "area",
                    "affiliated",
                    "recent.activity",
                    "retaliation.risk",
                    "group1_Gang_Flag",
                    "group2_Gang_Flag",
                    "group3_Gang_Flag",
                    "Global_Gang_Flag",
                    "sum.characteristics"
                  ),
                  skip = 1)


# reshape to long form to make grouping analyses easier
# now each row appears 3 times, once for each rating group
gcdatl <- gcdat %>% 
  gather(rater, gang,
         matches("group")) %>%
  mutate(rater = as.factor(rater))

# kable style
print_kable <- function(table, ...) {
  kable(table, ...) %>% 
    kable_styling(full_width = FALSE)
}
```

# Modeling Characteristic Count

How does the likelihood of gang classification change with the number of gang characteristics? For each rater, I fit a logistic regression predicting their gang classification from the number of gang characteristics.


```r
# null model without rater
sum.model.0 <- glm(
  gang ~ sum.characteristics,
  family = binomial,
  data = gcdatl
)

summary(sum.model.0)
```

```
## 
## Call:
## glm(formula = gang ~ sum.characteristics, family = binomial, 
##     data = gcdatl)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.5809  -1.2763   0.9038   1.0816   1.2745  
## 
## Coefficients:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -0.22544    0.05461  -4.129 3.65e-05 ***
## sum.characteristics  0.22746    0.02012  11.303  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 7466.7  on 5481  degrees of freedom
## Residual deviance: 7336.2  on 5480  degrees of freedom
##   (206 observations deleted due to missingness)
## AIC: 7340.2
## 
## Number of Fisher Scoring iterations: 4
```

```r
# rater main effects
sum.model.1 <- glm(
  gang ~ sum.characteristics + rater,
  family = binomial,
  data = gcdatl
)

summary(sum.model.1)
```

```
## 
## Call:
## glm(formula = gang ~ sum.characteristics + rater, family = binomial, 
##     data = gcdatl)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8182  -1.1187   0.7400   0.9064   1.6638  
## 
## Coefficients:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            0.19922    0.06810   2.925  0.00344 ** 
## sum.characteristics    0.23899    0.02116  11.295  < 2e-16 ***
## ratergroup2_Gang_Flag  0.04614    0.07304   0.632  0.52757    
## ratergroup3_Gang_Flag -1.29495    0.06944 -18.648  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 7466.7  on 5481  degrees of freedom
## Residual deviance: 6838.2  on 5478  degrees of freedom
##   (206 observations deleted due to missingness)
## AIC: 6846.2
## 
## Number of Fisher Scoring iterations: 4
```

```r
# rater interaction
sum.model.2 <- glm(
  gang ~ sum.characteristics * rater,
  family = binomial,
  data = gcdatl
)

anova(sum.model.0,
      sum.model.1,
      sum.model.2,
      test = "Chisq") %>% tidy()
```

```
## # A tibble: 3 x 5
##   Resid..Df Resid..Dev    df Deviance    p.value
##       <dbl>      <dbl> <dbl>    <dbl>      <dbl>
## 1      5480      7336.    NA     NA   NA        
## 2      5478      6838.     2    498.   7.18e-109
## 3      5476      6751.     2     86.9  1.37e- 19
```

```r
summary(sum.model.2)
```

```
## 
## Call:
## glm(formula = gang ~ sum.characteristics * rater, family = binomial, 
##     data = gcdatl)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1208  -1.0336   0.5954   0.8548   1.6188  
## 
## Coefficients:
##                                           Estimate Std. Error z value
## (Intercept)                               -0.34812    0.09428  -3.692
## sum.characteristics                        0.49712    0.03851  12.910
## ratergroup2_Gang_Flag                      1.17573    0.14585   8.061
## ratergroup3_Gang_Flag                     -0.64773    0.13587  -4.767
## sum.characteristics:ratergroup2_Gang_Flag -0.49934    0.05497  -9.084
## sum.characteristics:ratergroup3_Gang_Flag -0.29857    0.05183  -5.761
##                                           Pr(>|z|)    
## (Intercept)                               0.000222 ***
## sum.characteristics                        < 2e-16 ***
## ratergroup2_Gang_Flag                     7.56e-16 ***
## ratergroup3_Gang_Flag                     1.87e-06 ***
## sum.characteristics:ratergroup2_Gang_Flag  < 2e-16 ***
## sum.characteristics:ratergroup3_Gang_Flag 8.36e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 7466.7  on 5481  degrees of freedom
## Residual deviance: 6751.3  on 5476  degrees of freedom
##   (206 observations deleted due to missingness)
## AIC: 6763.3
## 
## Number of Fisher Scoring iterations: 4
```

## Followup: simple slopes and region of significance


```r
# simple slopes
simple_slopes(sum.model.2) %>% 
  filter(rater != "sstest") %>% 
  mutate(ci.low = `Test Estimate` - qnorm(.975) * `Std. Error`,
         ci.high = `Test Estimate` + qnorm(.975) * `Std. Error`) %>% 
  print_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> sum.characteristics </th>
   <th style="text-align:left;"> rater </th>
   <th style="text-align:right;"> Test Estimate </th>
   <th style="text-align:right;"> Std. Error </th>
   <th style="text-align:right;"> t value </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> Pr(&gt;|t|) </th>
   <th style="text-align:right;"> ci.low </th>
   <th style="text-align:right;"> ci.high </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> sstest </td>
   <td style="text-align:left;"> group1_Gang_Flag </td>
   <td style="text-align:right;"> 0.4971234 </td>
   <td style="text-align:right;"> 0.0385057 </td>
   <td style="text-align:right;"> 12.9103941 </td>
   <td style="text-align:right;"> 5476 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 0.4216536 </td>
   <td style="text-align:right;"> 0.5725931 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sstest </td>
   <td style="text-align:left;"> group2_Gang_Flag </td>
   <td style="text-align:right;"> -0.0022198 </td>
   <td style="text-align:right;"> 0.0392290 </td>
   <td style="text-align:right;"> -0.0565845 </td>
   <td style="text-align:right;"> 5476 </td>
   <td style="text-align:right;"> 0.9548762 </td>
   <td style="text-align:right;"> -0.0791072 </td>
   <td style="text-align:right;"> 0.0746677 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sstest </td>
   <td style="text-align:left;"> group3_Gang_Flag </td>
   <td style="text-align:right;"> 0.1985523 </td>
   <td style="text-align:right;"> 0.0346891 </td>
   <td style="text-align:right;"> 5.7237685 </td>
   <td style="text-align:right;"> 5476 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 0.1305629 </td>
   <td style="text-align:right;"> 0.2665416 </td>
  </tr>
</tbody>
</table>

```r
# the johnson_neyman function requires dummy codes not factor input
# manual dummies with either group one or group three as the reference
gcdummy <- gcdatl %>% 
  mutate(one_two = ifelse(rater == "group2_Gang_Flag",
                          1,
                          0),
         one_three = ifelse(rater == "group3_Gang_Flag",
                           1,
                           0),
         three_one = ifelse(rater == "group1_Gang_Flag",
                          1,
                          0),
         three_two = one_two)

# checking dummy codes
distinct(gcdummy, rater, one_two, one_three, three_one, three_two) %>% print_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> rater </th>
   <th style="text-align:right;"> one_two </th>
   <th style="text-align:right;"> one_three </th>
   <th style="text-align:right;"> three_one </th>
   <th style="text-align:right;"> three_two </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group3_Gang_Flag </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

```r
# group one as reference
sum.model.3 <- glm(
  gang ~ sum.characteristics * one_two + sum.characteristics * one_three,
  family = binomial,
  data = gcdummy
)

summary(sum.model.3)
```

```
## 
## Call:
## glm(formula = gang ~ sum.characteristics * one_two + sum.characteristics * 
##     one_three, family = binomial, data = gcdummy)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1208  -1.0336   0.5954   0.8548   1.6188  
## 
## Coefficients:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -0.34812    0.09428  -3.692 0.000222 ***
## sum.characteristics            0.49712    0.03851  12.910  < 2e-16 ***
## one_two                        1.17573    0.14585   8.061 7.56e-16 ***
## one_three                     -0.64773    0.13587  -4.767 1.87e-06 ***
## sum.characteristics:one_two   -0.49934    0.05497  -9.084  < 2e-16 ***
## sum.characteristics:one_three -0.29857    0.05183  -5.761 8.36e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 7466.7  on 5481  degrees of freedom
## Residual deviance: 6751.3  on 5476  degrees of freedom
##   (206 observations deleted due to missingness)
## AIC: 6763.3
## 
## Number of Fisher Scoring iterations: 4
```

```r
# sig except for 2.29 - 2.79
interactions::johnson_neyman(
  sum.model.3,
  pred = one_two,
  modx = sum.characteristics,
  plot = FALSE
)
```

```
## JOHNSON-NEYMAN INTERVAL 
## 
## When sum.characteristics is OUTSIDE the interval [2.06, 2.66], the
## slope of one_two is p < .05.
## 
## Note: The range of observed values of sum.characteristics is [0.00,
## 5.00]
```

```r
# sig through entire range
interactions::johnson_neyman(
  sum.model.3,
  pred = one_three,
  modx = sum.characteristics,
  plot = FALSE
)
```

```
## JOHNSON-NEYMAN INTERVAL 
## 
## When sum.characteristics is OUTSIDE the interval [-4.51, -0.98], the
## slope of one_three is p < .05.
## 
## Note: The range of observed values of sum.characteristics is [0.00,
## 5.00]
```

```r
# model 4 ciw as reference-------------------------
sum.model.4 <- glm(
  gang ~ sum.characteristics * three_one + sum.characteristics * three_two,
  family = binomial,
  data = gcdummy
)

summary(sum.model.4)
```

```
## 
## Call:
## glm(formula = gang ~ sum.characteristics * three_one + sum.characteristics * 
##     three_two, family = binomial, data = gcdummy)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1208  -1.0336   0.5954   0.8548   1.6188  
## 
## Coefficients:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   -0.99586    0.09784 -10.178  < 2e-16 ***
## sum.characteristics            0.19855    0.03469   5.724 1.04e-08 ***
## three_one                      0.64773    0.13587   4.767 1.87e-06 ***
## three_two                      1.82346    0.14818  12.306  < 2e-16 ***
## sum.characteristics:three_one  0.29857    0.05183   5.761 8.36e-09 ***
## sum.characteristics:three_two -0.20077    0.05237  -3.834 0.000126 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 7466.7  on 5481  degrees of freedom
## Residual deviance: 6751.3  on 5476  degrees of freedom
##   (206 observations deleted due to missingness)
## AIC: 6763.3
## 
## Number of Fisher Scoring iterations: 4
```

```r
# sig through entire range
interactions::johnson_neyman(
  sum.model.4,
  pred = three_one,
  modx = sum.characteristics,
  plot = FALSE
)
```

```
## JOHNSON-NEYMAN INTERVAL 
## 
## When sum.characteristics is OUTSIDE the interval [-4.51, -0.98], the
## slope of three_one is p < .05.
## 
## Note: The range of observed values of sum.characteristics is [0.00,
## 5.00]
```

```r
# sig through entire range
interactions::johnson_neyman(
  sum.model.4,
  pred = three_two,
  modx = sum.characteristics,
  plot = FALSE
)
```

```
## JOHNSON-NEYMAN INTERVAL 
## 
## When sum.characteristics is OUTSIDE the interval [6.78, 16.06], the
## slope of three_two is p < .05.
## 
## Note: The range of observed values of sum.characteristics is [0.00,
## 5.00]
```

# Table: predicted likelihood of classification for 0-5 factors, by group


```r
# predicted values accross range
# extra points so later the plot will be smooth
new_dat <- tibble(rater = unique(gcdatl$rater)) %>% 
  merge(tibble(
    sum.characteristics = seq(0, 5, by = 0.1)
    )) %>% 
  as_tibble()

sum.pred <- sum.model.2 %>% 
  augment(newdata = new_dat) %>% 
  select(rater,
         sum.characteristics,
         .fitted,
         .se.fit) %>%
  distinct() %>% 
  # calculate CIs
  mutate(ci.low = .fitted - qnorm(.975) * .se.fit,
         ci.high = .fitted + qnorm(.975) * .se.fit) %>%
  # convert to odds and probabilities
  mutate_at(vars(.fitted, ci.low, ci.high),
            funs(odds = exp(.),
                 prob = exp(.) / (1 + exp(.)
                                  )))

sum.contrasts <- emmeans(sum.model.2,
        ~ rater * sum.characteristics,
        at = list(sum.characteristics = 0:5)) %>% 
  contrast("pairwise", simple = "rater")

print_kable(sum.contrasts)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> contrast </th>
   <th style="text-align:right;"> sum.characteristics </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> z.ratio </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group2_Gang_Flag </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.1757263 </td>
   <td style="text-align:right;"> 0.1458502 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> -8.061188 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.6477350 </td>
   <td style="text-align:right;"> 0.1358734 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 4.767195 </td>
   <td style="text-align:right;"> 0.0000056 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.8234612 </td>
   <td style="text-align:right;"> 0.1481775 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 12.305926 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group2_Gang_Flag </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -0.6763831 </td>
   <td style="text-align:right;"> 0.1026474 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> -6.589382 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.9463061 </td>
   <td style="text-align:right;"> 0.0956873 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 9.889564 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1.6226892 </td>
   <td style="text-align:right;"> 0.1053330 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 15.405331 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group2_Gang_Flag </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> -0.1770400 </td>
   <td style="text-align:right;"> 0.0764454 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> -2.315902 </td>
   <td style="text-align:right;"> 0.0536225 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.2448772 </td>
   <td style="text-align:right;"> 0.0722675 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 17.225965 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.4219172 </td>
   <td style="text-align:right;"> 0.0756174 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 18.804102 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group2_Gang_Flag </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.3223031 </td>
   <td style="text-align:right;"> 0.0848204 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 3.799831 </td>
   <td style="text-align:right;"> 0.0004254 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.5434483 </td>
   <td style="text-align:right;"> 0.0816159 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 18.911129 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.2211452 </td>
   <td style="text-align:right;"> 0.0763246 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 15.999373 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group2_Gang_Flag </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0.8216462 </td>
   <td style="text-align:right;"> 0.1207820 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 6.802719 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.8420194 </td>
   <td style="text-align:right;"> 0.1160679 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 15.870193 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.0203732 </td>
   <td style="text-align:right;"> 0.1068521 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 9.549393 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group2_Gang_Flag </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1.3209894 </td>
   <td style="text-align:right;"> 0.1674076 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 7.890857 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2.1405905 </td>
   <td style="text-align:right;"> 0.1601699 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 13.364502 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag - group3_Gang_Flag </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0.8196011 </td>
   <td style="text-align:right;"> 0.1499794 </td>
   <td style="text-align:right;"> Inf </td>
   <td style="text-align:right;"> 5.464759 </td>
   <td style="text-align:right;"> 0.0000001 </td>
  </tr>
</tbody>
</table>

```r
# sum means
sum.pred %>% 
  filter(sum.characteristics %in% 0:5) %>% 
  select(rater, sum.characteristics,
         .fitted, .se.fit,
         .fitted_prob) %>% 
  print_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> rater </th>
   <th style="text-align:right;"> sum.characteristics </th>
   <th style="text-align:right;"> .fitted </th>
   <th style="text-align:right;"> .se.fit </th>
   <th style="text-align:right;"> .fitted_prob </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -0.3481204 </td>
   <td style="text-align:right;"> 0.0942797 </td>
   <td style="text-align:right;"> 0.4138383 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.8276059 </td>
   <td style="text-align:right;"> 0.1112818 </td>
   <td style="text-align:right;"> 0.6958485 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group3_Gang_Flag </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -0.9958554 </td>
   <td style="text-align:right;"> 0.0978413 </td>
   <td style="text-align:right;"> 0.2697571 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.1490030 </td>
   <td style="text-align:right;"> 0.0655650 </td>
   <td style="text-align:right;"> 0.5371820 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.8253861 </td>
   <td style="text-align:right;"> 0.0789793 </td>
   <td style="text-align:right;"> 0.6953785 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group3_Gang_Flag </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -0.7973031 </td>
   <td style="text-align:right;"> 0.0696944 </td>
   <td style="text-align:right;"> 0.3106027 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.6461263 </td>
   <td style="text-align:right;"> 0.0517131 </td>
   <td style="text-align:right;"> 0.6561370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.8231663 </td>
   <td style="text-align:right;"> 0.0562996 </td>
   <td style="text-align:right;"> 0.6949081 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group3_Gang_Flag </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> -0.5987509 </td>
   <td style="text-align:right;"> 0.0504811 </td>
   <td style="text-align:right;"> 0.3546295 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.1432497 </td>
   <td style="text-align:right;"> 0.0633648 </td>
   <td style="text-align:right;"> 0.7582758 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.8209466 </td>
   <td style="text-align:right;"> 0.0563861 </td>
   <td style="text-align:right;"> 0.6944372 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group3_Gang_Flag </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> -0.4001986 </td>
   <td style="text-align:right;"> 0.0514397 </td>
   <td style="text-align:right;"> 0.4012646 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.6403731 </td>
   <td style="text-align:right;"> 0.0912213 </td>
   <td style="text-align:right;"> 0.8375857 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0.8187268 </td>
   <td style="text-align:right;"> 0.0791642 </td>
   <td style="text-align:right;"> 0.6939660 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group3_Gang_Flag </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> -0.2016463 </td>
   <td style="text-align:right;"> 0.0717664 </td>
   <td style="text-align:right;"> 0.4497585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group1_Gang_Flag </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2.1374964 </td>
   <td style="text-align:right;"> 0.1248717 </td>
   <td style="text-align:right;"> 0.8944946 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2_Gang_Flag </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0.8165071 </td>
   <td style="text-align:right;"> 0.1115005 </td>
   <td style="text-align:right;"> 0.6934944 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group3_Gang_Flag </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> -0.0030941 </td>
   <td style="text-align:right;"> 0.1003068 </td>
   <td style="text-align:right;"> 0.4992265 </td>
  </tr>
</tbody>
</table>

### Plot


```r
plot_data <- sum.pred %>% 
  select(-.se.fit) %>% 
  gather(key, value, -rater, -sum.characteristics) %>% 
  mutate(key = ifelse(str_detect(key, "_"),
                      key,
                      paste(key,
                            "log",
                            sep = "_")
                      )) %>% 
  extract(key,
         into = c("key", "log_prob"),
         regex = "(.*)_(.*)"
  ) %>% 
  spread(key, value) %>% 
  filter(log_prob != "odds")

hline_dat <- tribble(
  ~log_prob, ~yint,
  "log", 0,
  "prob", 0.5
)

log_prob_labels <- c(
  "log" = "Log-Odds",
  "prob" = "Probability"
)

sum.plot <- ggplot(plot_data,
                   aes(x = sum.characteristics,
                       y = .fitted,
                       ymin = ci.low,
                       ymax = ci.high,
                       fill = rater)) +
  geom_line(aes(color = rater)) +
  geom_ribbon(alpha = .3) +
  labs(y = "Likelihood\nof Gang Classification",
       x = "Number of Indicator Factors",
       color = "Group",
       fill = "Group") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  # theme(legend.position = c(-0.2, 0.1)) +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  geom_hline(data = hline_dat,
    aes(yintercept = yint),
    linetype = 2,
    alpha = 0.7) +
  facet_wrap(vars(log_prob),
             nrow = 2,
             scales = "free_y",
             labeller = as_labeller(log_prob_labels)
             ) +
  annotate(geom = "rect",
           xmin = 2.06,
           xmax = 2.66,
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2
  )

sum.plot
```

![](gryd-gang-classification-forgithub_files/figure-html/sum-plots-1.png)<!-- -->

```r
ggsave("/Users/Ben/Dropbox/PhD/Research/gryd/gang-classification/for github/sum_plot-github.pdf",
          plot = sum.plot,
          scale = 0.7)
```


Lines indicate the estimated log odds (upper panel) and probabilities (lower panel) of gang classification, by the number of characteristics, for each of the three groups. Error bars show 95% confidence intervals.
The shaded rectangle indicates the range of the X-axis for which the effect for groups 1 and 2 were not significantly different.

## Decision Criteria

### Cut Points

For each of the 3 raters, at which point does their likelihood of classifying cross the 50:50 mark?

At the 50:50 mark, the log odds of gang classification are 0.

For the logistic regression,

logit = intercept + x * slope.

With some algebra:

0 = intercept + x * slope

x * slope = - intercept

x = - intercept / slope.


```r
sum.params <- sum.model.2 %>% 
  tidy() %>% 
  select(term, estimate) %>% 
  spread(term, estimate) %>% 
  rename(
     "intercept" = "(Intercept)",
     "slope" = "sum.characteristics",
     "group2_int" = "ratergroup2_Gang_Flag",
     "group3_int" = "ratergroup3_Gang_Flag",
     "group2_slope" = "sum.characteristics:ratergroup2_Gang_Flag",
     "group3_slope" = "sum.characteristics:ratergroup3_Gang_Flag"
  )

decision.crit <-
  # simple slopes and intercepts
  with(sum.params,
       tribble(
         ~rater, ~intercept, ~slope,
         "group1", intercept, slope,
         "group2", intercept + group2_int, slope + group2_slope,
         "group3", intercept + group3_int, slope + group3_slope
         )
       )%>% 
  # calculate decision point
  mutate(decision.point = - intercept / slope)

print_kable(decision.crit)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> rater </th>
   <th style="text-align:right;"> intercept </th>
   <th style="text-align:right;"> slope </th>
   <th style="text-align:right;"> decision.point </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> group1 </td>
   <td style="text-align:right;"> -0.3481204 </td>
   <td style="text-align:right;"> 0.4971234 </td>
   <td style="text-align:right;"> 0.7002696 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group2 </td>
   <td style="text-align:right;"> 0.8276059 </td>
   <td style="text-align:right;"> -0.0022198 </td>
   <td style="text-align:right;"> 372.8368614 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> group3 </td>
   <td style="text-align:right;"> -0.9958554 </td>
   <td style="text-align:right;"> 0.1985523 </td>
   <td style="text-align:right;"> 5.0155831 </td>
  </tr>
</tbody>
</table>

# Modeling Characteristics Independently

How does the likelihood of gang classification change with of the different gang characteristics? For each rater, I fit a logistic regression predicting their gang classification from the different gang characteristics.


```r
# ordering characteristics for output
the_order <- c(
  "intercept",
  "area",
  "affiliated",
  "recent.activity",
  "retaliation.risk",
  "motivated"
)

ind.model.0 <- glm(gang ~ 
                     area +
                     affiliated +
                     recent.activity +
                     retaliation.risk +
                     motivated,
                   family = binomial,
                   data = gcdatl)

ind.model.1 <- glm(gang ~ 
                     area +
                     affiliated +
                     recent.activity +
                     retaliation.risk +
                     motivated +
                     rater,
                   family = binomial,
                   data = gcdatl)

anova(ind.model.0,
      ind.model.1,
      test = "LRT")
```

```
## Analysis of Deviance Table
## 
## Model 1: gang ~ area + affiliated + recent.activity + retaliation.risk + 
##     motivated
## Model 2: gang ~ area + affiliated + recent.activity + retaliation.risk + 
##     motivated + rater
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1      5476     7310.2                          
## 2      5474     6807.3  2   502.94 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
ind.models <- tibble(
  var = the_order[-1]
) %>% 
  mutate(
    m1 = list(ind.model.1),
    m2_formula = paste0(
      "gang ~ area + affiliated + recent.activity + retaliation.risk + motivated + ",
    var,
    "*rater"
    ),
    
    m2 = map(m2_formula,
             ~ glm(formula = formula(.x),
                  family = binomial,
                  data = gcdatl
                  )),
    lrt = map2(m1, m2,
               ~ anova(.x,
                       .y,
                       test = "Chisq")),
    tidy_lrt = map(lrt,
                   ~ tidy(.x))
           )
  
ind.models %>% 
  select(var, tidy_lrt) %>% 
  unnest(tidy_lrt)
```

```
## # A tibble: 10 x 6
##    var              Resid..Df Resid..Dev    df Deviance   p.value
##    <chr>                <dbl>      <dbl> <dbl>    <dbl>     <dbl>
##  1 area                  5474      6807.    NA     NA   NA       
##  2 area                  5472      6790.     2     16.9  2.12e- 4
##  3 affiliated            5474      6807.    NA     NA   NA       
##  4 affiliated            5472      6756.     2     51.2  7.58e-12
##  5 recent.activity       5474      6807.    NA     NA   NA       
##  6 recent.activity       5472      6786.     2     21.0  2.70e- 5
##  7 retaliation.risk      5474      6807.    NA     NA   NA       
##  8 retaliation.risk      5472      6764.     2     43.3  4.05e-10
##  9 motivated             5474      6807.    NA     NA   NA       
## 10 motivated             5472      6728.     2     79.2  6.19e-18
```

```r
gcdatl <- mutate(gcdatl,
                 rater_r = relevel(rater, ref = 2)
                 )
ind.model.2 <- glm(gang ~   
                     rater*area +
                     rater*affiliated +
                     rater*recent.activity +
                     rater*retaliation.risk +
                     rater*motivated,
                   family = binomial,
                   data = gcdatl
)

summary(ind.model.2)
```

```
## 
## Call:
## glm(formula = gang ~ rater * area + rater * affiliated + rater * 
##     recent.activity + rater * retaliation.risk + rater * motivated, 
##     family = binomial, data = gcdatl)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.5350  -1.0537   0.5667   0.8898   1.6308  
## 
## Coefficients:
##                                        Estimate Std. Error z value
## (Intercept)                            -0.06191    0.10594  -0.584
## ratergroup2_Gang_Flag                   1.09820    0.16946   6.481
## ratergroup3_Gang_Flag                  -0.96066    0.15294  -6.281
## area                                   -0.06937    0.12280  -0.565
## affiliated                              0.78224    0.11704   6.683
## recent.activity                         1.35519    0.47196   2.871
## retaliation.risk                        0.20484    0.12236   1.674
## motivated                               0.89178    0.12348   7.222
## ratergroup2_Gang_Flag:area             -0.24460    0.18393  -1.330
## ratergroup3_Gang_Flag:area              0.27902    0.17077   1.634
## ratergroup2_Gang_Flag:affiliated       -0.53343    0.16576  -3.218
## ratergroup3_Gang_Flag:affiliated       -0.47453    0.15837  -2.996
## ratergroup2_Gang_Flag:recent.activity  -0.83909    0.55056  -1.524
## ratergroup3_Gang_Flag:recent.activity  -1.58926    0.52669  -3.017
## ratergroup2_Gang_Flag:retaliation.risk -0.35871    0.17758  -2.020
## ratergroup3_Gang_Flag:retaliation.risk -0.03804    0.16958  -0.224
## ratergroup2_Gang_Flag:motivated        -0.90865    0.17435  -5.212
## ratergroup3_Gang_Flag:motivated        -0.68463    0.16703  -4.099
##                                        Pr(>|z|)    
## (Intercept)                             0.55895    
## ratergroup2_Gang_Flag                  9.13e-11 ***
## ratergroup3_Gang_Flag                  3.35e-10 ***
## area                                    0.57213    
## affiliated                             2.34e-11 ***
## recent.activity                         0.00409 ** 
## retaliation.risk                        0.09410 .  
## motivated                              5.12e-13 ***
## ratergroup2_Gang_Flag:area              0.18355    
## ratergroup3_Gang_Flag:area              0.10229    
## ratergroup2_Gang_Flag:affiliated        0.00129 ** 
## ratergroup3_Gang_Flag:affiliated        0.00273 ** 
## ratergroup2_Gang_Flag:recent.activity   0.12750    
## ratergroup3_Gang_Flag:recent.activity   0.00255 ** 
## ratergroup2_Gang_Flag:retaliation.risk  0.04339 *  
## ratergroup3_Gang_Flag:retaliation.risk  0.82249    
## ratergroup2_Gang_Flag:motivated        1.87e-07 ***
## ratergroup3_Gang_Flag:motivated        4.15e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 7466.7  on 5481  degrees of freedom
## Residual deviance: 6681.6  on 5464  degrees of freedom
##   (206 observations deleted due to missingness)
## AIC: 6717.6
## 
## Number of Fisher Scoring iterations: 5
```

```r
simple_slopes(ind.model.2)
```

```
##               rater     area Test Estimate Std. Error t value   df
## 1  group1_Gang_Flag   sstest       -0.0694     0.1228 -0.5649 5464
## 2  group2_Gang_Flag   sstest       -0.3140     0.1369 -2.2931 5464
## 3  group3_Gang_Flag   sstest        0.2096     0.1187  1.7666 5464
## 4a           sstest 0.325772        1.0185     0.1376  7.4013 5464
## 4b           sstest 0.325772       -0.8698     0.1254 -6.9337 5464
## 5a           sstest 0.755564        0.9134     0.1305  7.0010 5464
## 5b           sstest 0.755564       -0.7498     0.1229 -6.1005 5464
## 6a           sstest 1.185355        0.8083     0.1661  4.8649 5464
## 6b           sstest 1.185355       -0.6299     0.1589 -3.9638 5464
##     Pr(>|t|) Sig.
## 1    0.57213     
## 2    0.02184    *
## 3    0.07729    .
## 4a 1.348e-13  ***
## 4b 4.101e-12  ***
## 5a 2.541e-12  ***
## 5b 1.057e-09  ***
## 6a 1.145e-06  ***
## 6b 7.376e-05  ***
```

```r
# for each of the 3 raters
# regress logit of gang classification onto each of the gang characteristics
ind.models <- gcdatl %>%
  nest(-rater) %>%
  mutate(
    fit = map(data, ~ glm(gang ~ motivated +
                            area +
                            affiliated +
                            recent.activity +
                            retaliation.risk,
                          family = binomial,
                          data = .x)),  # S3 list-col
    tidied = map(fit, tidy)
  )

# extract model summaries
ind.fit <- ind.models %>%
  unnest(tidied) %>%
  mutate(term = recode(term,
                       "(Intercept)" = "intercept")) %>% 
  arrange(rater, match(term, the_order))
ind.fit
```

```
## # A tibble: 18 x 6
##    rater            term             estimate std.error statistic  p.value
##    <fct>            <chr>               <dbl>     <dbl>     <dbl>    <dbl>
##  1 group1_Gang_Flag intercept         -0.0619     0.106    -0.584 5.59e- 1
##  2 group1_Gang_Flag area              -0.0694     0.123    -0.565 5.72e- 1
##  3 group1_Gang_Flag affiliated         0.782      0.117     6.68  2.34e-11
##  4 group1_Gang_Flag recent.activity    1.36       0.472     2.87  4.09e- 3
##  5 group1_Gang_Flag retaliation.risk   0.205      0.122     1.67  9.41e- 2
##  6 group1_Gang_Flag motivated          0.892      0.123     7.22  5.12e-13
##  7 group2_Gang_Flag intercept          1.04       0.132     7.84  4.68e-15
##  8 group2_Gang_Flag area              -0.314      0.137    -2.29  2.18e- 2
##  9 group2_Gang_Flag affiliated         0.249      0.117     2.12  3.40e- 2
## 10 group2_Gang_Flag recent.activity    0.516      0.284     1.82  6.87e- 2
## 11 group2_Gang_Flag retaliation.risk  -0.154      0.129    -1.20  2.32e- 1
## 12 group2_Gang_Flag motivated         -0.0169     0.123    -0.137 8.91e- 1
## 13 group3_Gang_Flag intercept         -1.02       0.110    -9.27  1.85e-20
## 14 group3_Gang_Flag area               0.210      0.119     1.77  7.73e- 2
## 15 group3_Gang_Flag affiliated         0.308      0.107     2.88  3.92e- 3
## 16 group3_Gang_Flag recent.activity   -0.234      0.234    -1.00  3.17e- 1
## 17 group3_Gang_Flag retaliation.risk   0.167      0.117     1.42  1.55e- 1
## 18 group3_Gang_Flag motivated          0.207      0.112     1.84  6.55e- 2
```

```r
# extract predictions and calculate CIs and predicted probabilities
# need to feed prediction terms to augment function
#   because the CIW ratings don't have an observation with
#   exactly one of each characteristic present

# data for predictions
new.pred.data <- rbind(matrix(0, 1, 5),
                           diag(5)) %>% as_tibble()
names(new.pred.data) <- c("motivated",
                          "area",
                          "affiliated",
                          "recent.activity",
                          "retaliation.risk")

# make predictions
ind.pred <- ind.models %>%
  mutate(predicted = map(fit, ~ augment(x = .x, newdata = new.pred.data))) %>%
  unnest(predicted) %>%
  # calculate CIs
  mutate(ci.low = .fitted - qnorm(.975) * .se.fit,
         ci.high = .fitted + qnorm(.975) * .se.fit) %>%
  # convert to odds and probabilities
  mutate_at(vars(.fitted, ci.low, ci.high),
            funs(odds = exp(.),
                 prob = exp(.) / (1 + exp(.)
                 ))) %>%
  # column with flag for intercept rows
  mutate(intercept = 1 - rowSums(select(., motivated,
                                        area,
                                        affiliated,
                                        recent.activity,
                                        retaliation.risk))) %>%
  # gather predictions by term
  gather(key = term, value = present,
         intercept,
         motivated,
         area,
         affiliated,
         recent.activity,
         retaliation.risk) %>%
  # keep only rows where one term is present
  filter(present == 1) %>%
  # drop the present flag
  select(-present) %>%
  # factor term to control order of levels in plot
  mutate(term = factor(term, levels = rev(the_order)))
```

### Odds ratio plot

Points indicate the estimated odds-ratio of gang classification for crimes with each characteristic, over and above the effect of the others. Error bars show 95% confidence intervals.


```r
# odds ratio plot
ind.or.plot <- ggplot(filter(ind.pred, # no OR for intercept
                          term != "intercept"),
                   aes(x = term,
                       y = .fitted_odds,
                       ymin = ci.low_odds,
                       ymax = ci.high_odds,
                       color = rater)) +
  geom_point(size = 2.5, position = position_dodge(0.5)) +
  geom_errorbar(size = 1.3, position = position_dodge(0.5)) +
  labs(y = "Odds Ratio\nof Gang Classification",
       x = "Gang Characteristics",
       color = "Rater",
       fill = "Rater") +
  coord_flip() +
  scale_color_viridis(discrete=TRUE) + scale_fill_viridis(discrete=TRUE) +
  guides(fill = guide_legend(reverse = TRUE),
         color = guide_legend(reverse = TRUE))
ind.or.plot
```

![](gryd-gang-classification-forgithub_files/figure-html/odds-plot-1.png)<!-- -->

### Probabilities plot

Points indicate the estimated probability of gang classification for crimes with each characteristic, over and above the effect of the others. Error bars show 95% confidence intervals.


```r
# probabilities plot
ind.pr.plot <- ggplot(ind.pred,
                   aes(x = term,
                       y = .fitted_prob,
                       ymin = ci.low_prob,
                       ymax = ci.high_prob,
                       color = rater)) +
  geom_point(size = 2.5, position = position_dodge(0.5)) +
  geom_errorbar(size = 1.3, position = position_dodge(0.5)) +
  # optionaly, add line connecting points
  # stat_summary(fun.y=sum, geom="line",
  #              aes(group = rater),
  #              position = position_dodge(0.5)) +
  labs(y = "Probability\nof Gang Classification",
       x = "Gang Characteristics",
       color = "Rater",
       fill = "Rater") +
  coord_flip() +
  scale_color_viridis(discrete=TRUE) + scale_fill_viridis(discrete=TRUE) +
    guides(fill = guide_legend(reverse = TRUE),
         color = guide_legend(reverse = TRUE))
ind.pr.plot
```

![](gryd-gang-classification-forgithub_files/figure-html/prop-plot-1.png)<!-- -->

## ----knit-options, include=FALSE-----------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

# version notes
# v0 fit models
# v1 adding CIs
# v2 reordering characteristics per Jeff's suggestion
#   I'd like to present the factors in the following order: none (intercept), area, affiliated, recent, retaliation, motivated. (Right now it is: none, motivated, area, affiliated, recent, retaliation).
# v2 modeling the interaction of the labeling group explicitly
# v2 modeling simple slopes rather than subsetting
# unlabeling the rating groups for github

## ----setup---------------------------------------------------------------
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

## ----sum-----------------------------------------------------------------
# null model without rater
sum.model.0 <- glm(
  gang ~ sum.characteristics,
  family = binomial,
  data = gcdatl
)

summary(sum.model.0)

# rater main effects
sum.model.1 <- glm(
  gang ~ sum.characteristics + rater,
  family = binomial,
  data = gcdatl
)

summary(sum.model.1)

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

summary(sum.model.2)

## ----sum-followup--------------------------------------------------------
# simple slopes
simple_slopes(sum.model.2) %>% 
  filter(rater != "sstest") %>% 
  mutate(ci.low = `Test Estimate` - qnorm(.975) * `Std. Error`,
         ci.high = `Test Estimate` + qnorm(.975) * `Std. Error`) %>% 
  print_kable()

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

# group one as reference
sum.model.3 <- glm(
  gang ~ sum.characteristics * one_two + sum.characteristics * one_three,
  family = binomial,
  data = gcdummy
)

summary(sum.model.3)

# sig except for 2.29 - 2.79
interactions::johnson_neyman(
  sum.model.3,
  pred = one_two,
  modx = sum.characteristics,
  plot = FALSE
)

# sig through entire range
interactions::johnson_neyman(
  sum.model.3,
  pred = one_three,
  modx = sum.characteristics,
  plot = FALSE
)

# model 4 ciw as reference-------------------------
sum.model.4 <- glm(
  gang ~ sum.characteristics * three_one + sum.characteristics * three_two,
  family = binomial,
  data = gcdummy
)

summary(sum.model.4)

# sig through entire range
interactions::johnson_neyman(
  sum.model.4,
  pred = three_one,
  modx = sum.characteristics,
  plot = FALSE
)

# sig through entire range
interactions::johnson_neyman(
  sum.model.4,
  pred = three_two,
  modx = sum.characteristics,
  plot = FALSE
)

## ------------------------------------------------------------------------
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

# sum means
sum.pred %>% 
  filter(sum.characteristics %in% 0:5) %>% 
  select(rater, sum.characteristics,
         .fitted, .se.fit,
         .fitted_prob) %>% 
  print_kable()

## ----sum-plots-----------------------------------------------------------
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

ggsave("/Users/Ben/Dropbox/PhD/Research/gryd/gang-classification/for github/sum_plot-github.pdf",
          plot = sum.plot,
          scale = 0.7)

## ----decision-point------------------------------------------------------
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

## ----indiv---------------------------------------------------------------
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

simple_slopes(ind.model.2)

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

## ----odds-plot-----------------------------------------------------------
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

## ----prop-plot-----------------------------------------------------------
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


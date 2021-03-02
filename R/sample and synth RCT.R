library(tidyverse)


# A rough population with a series of covariates --------------------------

N <- 200

sample <- tibble(id = 1:N,
                 sex = sample(c(1, 0), N, replace = TRUE),
                 age = sample(18:80, N, replace = TRUE, prob = dnorm(seq(18, 80, by = 1), mean = 45, sd = 15)),
                 cont1 = rpois(N, 15),
                 cont2 = log(abs(rnorm(N, 4, 1))),
                 cont3 = rnorm(N, 15, 3),
                 disc1 = sample(c(1, 0), N, replace = TRUE, prob = c(0.2, 0.8)),
                 disc2 = sample(c(1, 0), N, replace = TRUE, prob = c(0.35, 0.65))
)


# perfect RCT -------------------------------------------------------------
## Randomised selection - no induced imbalances in assignment ##
# covariates age, cont2, cont3, disc1 and disc2 have causal effect on outcome
# exposure causes a 2-unit decrease in outcome, calculated from potential outcome
# with added variance from a mean effect

trial_pop <- sample %>% 
  rowwise() %>% 
  mutate(potential_outcome = 0.03*age + 0.2*cont2 + 0.5*cont3 + 0.6*disc1 + 0.4*disc2 + rnorm(1, sd = 30) + 40,
         exposed = sample(c(1, 0), 1),
         outcome = potential_outcome - 8*exposed + rnorm(1, sd = 3)*exposed)

# Baseline variables (Table 1)
trial_pop %>% 
  ungroup() %>% 
  mutate(across(sex:disc2, .fns = list(p = ~ t.test(.x ~ exposed)$p.value), .names = "{.col}_{.fn}"),
         exposed = factor(exposed, levels = c(0, 1), labels = c("control", "exposed"))) %>% 
  group_by(exposed) %>% 
  summarise(sex_mean = sum(sex)/n(),
            across(age:cont3, .fns = list(mean = mean, sd = sd), .names = "{.col}_{.fn}"),
            across(disc1:disc2, .fns = list(mean = ~ sum(.x)/n())),
            across(ends_with("_p"), ~unique(.x))) %>% 
  pivot_longer(-exposed, names_to = c("variable", ".value"), names_sep = "_") %>% 
  pivot_wider(names_from = c(exposed), values_from = c(mean, sd), names_glue = "{exposed}_{.value}") %>% 
  select(variable, control_mean, control_sd, exposed_mean, exposed_sd, p)


## Testing for differences ------------------------------------------------

trial_pop %>% 
  ggplot(aes(outcome, fill = factor(exposed))) +
  stat_density(alpha = 0.5, position = "identity")

# mean difference

trial_pop %>% 
  group_by(exposed) %>% 
  summarise(mean = mean(outcome), sd = sd(outcome), se = sd/sqrt(n())) %T>%
  print() %>% 
  summarise(diff_mean = diff(mean)) %>% 
  pull(diff_mean) %>% 
  sprintf("This represents a mean difference of %f", .)

# Uncontrolled, simple t-test for difference in means

t.test(outcome ~ exposed, data = trial_pop)

# lm controlling for all covariates

lm(outcome ~ exposed + age + sex + cont1 + cont2 + cont3 + disc1 + disc2, data = trial_pop) %T>%
  {print(summary(.))} %>% 
  {cat("Confidence Intervals:\n")
    confint(.)}


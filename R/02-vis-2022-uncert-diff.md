Study 1: Mixed Effects Modeling
================

## 1. Import Packages

``` r
library(ggplot2)
library(statsr)
library(lme4)
library(sjPlot)
library(dplyr)
library(brmstools)
theme_set(theme_sjplot())
```

## 2. Load Data

``` r
df <- readr::read_csv("../data/belief_data_prolific_all_exclude.csv")

# refactor and categorize
df$vis_condition <- factor(df$vis_condition, c("uncertainty","scatter","hop"))
levels(df$vis_condition ) <- c("scatter","uncertainty","hop")
```

``` r
nrow(df)
```

    ## [1] 2913

``` r
# number of uncertainty_difference missing
sum(is.na(df$uncertainty_difference))
```

    ## [1] 0

## 3. Frequentist Mixed Effects Modeling (`lme4`)

``` r
# first model is normal response
m1 = lmer(uncertainty_difference ~ vis_condition + (1|user_token), df)

# second model is lognormal response
#m2 = glmer(diffBeliefAbs ~ vis_condition +  (1|user_token), df, family = gaussian(link = "log"))
```

``` r
a <- plot_model(m1,show.values = TRUE, vline.color = "grey", value.offset = .4, value.size = 3, type="est", show.intercept = TRUE ) +
  scale_y_continuous(breaks=seq(-.75,0.75,.25)) +
  theme(axis.text.y = element_text(size = 8),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = "Uncertainty Difference", title = "") +
  ylim(-0.2, 0.2)

a
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## 4a. Absolute Belief Difference

### Bayesian Mixed Effects

Let’s examine the first regression to estimate the effect on the
Uncertainty Difference (`uncertainty_difference`). We’ll use the same
functional form as model `m`.

``` r
library(brms)

# assume normal response variable
bm <- brms::brm(uncertainty_difference ~ vis_condition + (1|user_token), data = df, backend = "cmdstanr", cores = parallel::detectCores() -1)

save(bm, file = "../models/fit_baseline_diff_uncertainty.rda")
```

``` r
load("../models/fit_baseline_diff_uncertainty.rda")
```

First let’s look at metadata around the model.

``` r
coef_bm <- coefplot(bm)
coef_bm
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
coef_m_df <- a$data %>% rename(Parameter = term) %>% mutate(Parameter = as.character(Parameter))

coef_bm_df <- coef_bm$data
coef_bm_df$Parameter[coef_bm_df$Parameter=="Intercept"] <- "(Intercept)"

joined_models <- inner_join(coef_bm_df, coef_m_df, by = "Parameter")
```

Notice that the coefficients are very similar to Frequentist:

``` r
joined_models %>%
  rename(Bayesian_Estimate = Estimate, Freq_Estimate = estimate) %>%
  select(Parameter, Bayesian_Estimate, Freq_Estimate) %>%
  mutate(abs_diff = round(abs(Bayesian_Estimate - Freq_Estimate),3)) %>%
  knitr::kable()
```

| Parameter                 | Bayesian\_Estimate | Freq\_Estimate | abs\_diff |
|:--------------------------|-------------------:|---------------:|----------:|
| (Intercept)               |         -0.0080190 |     -0.0079357 |         0 |
| vis\_conditionhop         |          0.0105158 |      0.0107641 |         0 |
| vis\_conditionuncertainty |         -0.0513417 |     -0.0517034 |         0 |

We see the same for the coefficients standard errors (though they mean
slightly different things):

``` r
joined_models %>%
  rename(Bayesian_Error = Est.Error, Freq_Error = std.error) %>%
  select(Parameter, Bayesian_Error, Freq_Error) %>%
  mutate(abs_diff_error = round(abs(Bayesian_Error - Freq_Error),3)) %>%
  knitr::kable()
```

| Parameter                 | Bayesian\_Error | Freq\_Error | abs\_diff\_error |
|:--------------------------|----------------:|------------:|-----------------:|
| (Intercept)               |       0.0159581 |   0.0161723 |                0 |
| vis\_conditionhop         |       0.0225645 |   0.0229048 |                0 |
| vis\_conditionuncertainty |       0.0231204 |   0.0232196 |                0 |

### Model convergence / posterior predictive check

The convergence stats also look good - Rhat’s are at 1 and we have
“fuzzy caterpillars”.

``` r
plot(bm)
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

But remember - convergence doesn’t mean great fit. Let’s evaluate
overfitting with Posterior Predictive Checks. We’ll do 10 draws and
compare to actual.

``` r
pp_check(bm)
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Modify response (likelihood) to t-distribution

Let’s try instead a distribution likelihood because of the
overdispersion (fat tails) we observe in the data.

``` r
bm2 <- brms::brm(uncertainty_difference ~ vis_condition + (1|user_token), data = df, family = student(link = "identity", link_sigma = "log", link_nu = "logm1"), backend = "cmdstanr", cores = parallel::detectCores() - 1)

save(bm2, file = "../models/fit_baseline_diff_uncertainty2.rda")
```

``` r
load("../models/fit_baseline_diff_uncertainty2.rda")
```

### What are model priors?

``` r
bm2$prior
```

    ##                 prior     class                     coef      group resp dpar
    ##                (flat)         b                                              
    ##                (flat)         b         vis_conditionhop                     
    ##                (flat)         b vis_conditionuncertainty                     
    ##  student_t(3, 0, 2.5) Intercept                                              
    ##         gamma(2, 0.1)        nu                                              
    ##  student_t(3, 0, 2.5)        sd                                              
    ##  student_t(3, 0, 2.5)        sd                          user_token          
    ##  student_t(3, 0, 2.5)        sd                Intercept user_token          
    ##  student_t(3, 0, 2.5)     sigma                                              
    ##  nlpar bound       source
    ##                   default
    ##              (vectorized)
    ##              (vectorized)
    ##                   default
    ##                   default
    ##                   default
    ##              (vectorized)
    ##              (vectorized)
    ##                   default

### What are the coefficients?

``` r
bm2
```

    ##  Family: student 
    ##   Links: mu = identity; sigma = identity; nu = identity 
    ## Formula: uncertainty_difference ~ vis_condition + (1 | user_token) 
    ##    Data: df (Number of observations: 2913) 
    ##   Draws: 4 chains, each with iter = 1000; warmup = 0; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~user_token (Number of levels: 261) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.08      0.01     0.06     0.10 1.00      928     2004
    ## 
    ## Population-Level Effects: 
    ##                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                    0.00      0.01    -0.02     0.02 1.00     2115
    ## vis_conditionuncertainty    -0.05      0.02    -0.08    -0.02 1.00     2359
    ## vis_conditionhop             0.00      0.02    -0.03     0.03 1.00     2145
    ##                          Tail_ESS
    ## Intercept                    2949
    ## vis_conditionuncertainty     2947
    ## vis_conditionhop             2831
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.17      0.01     0.16     0.18 1.00     3194     3179
    ## nu        1.86      0.09     1.69     2.05 1.00     3587     3264
    ## 
    ## Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
coef_bm2 <- coefplot(bm2)
coef_bm2
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### Model Comparison

First, let’s use leave-one-out (loo) cross-validation. It will also
provide estimate to determine point leverage (aka outliers).

``` r
looNormal <- loo(bm, save_psis = TRUE)
print(looNormal)
```

    ## 
    ## Computed from 4000 by 2913 log-likelihood matrix
    ## 
    ##          Estimate    SE
    ## elpd_loo  -1367.8  77.6
    ## p_loo       120.4   6.5
    ## looic      2735.5 155.2
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## Pareto k diagnostic values:
    ##                          Count Pct.    Min. n_eff
    ## (-Inf, 0.5]   (good)     2912  100.0%  778       
    ##  (0.5, 0.7]   (ok)          1    0.0%  1903      
    ##    (0.7, 1]   (bad)         0    0.0%  <NA>      
    ##    (1, Inf)   (very bad)    0    0.0%  <NA>      
    ## 
    ## All Pareto k estimates are ok (k < 0.7).
    ## See help('pareto-k-diagnostic') for details.

``` r
looNormal <- loo(bm, save_psis = TRUE)
print(looNormal)
```

    ## 
    ## Computed from 4000 by 2913 log-likelihood matrix
    ## 
    ##          Estimate    SE
    ## elpd_loo  -1367.8  77.6
    ## p_loo       120.4   6.5
    ## looic      2735.5 155.2
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## Pareto k diagnostic values:
    ##                          Count Pct.    Min. n_eff
    ## (-Inf, 0.5]   (good)     2912  100.0%  778       
    ##  (0.5, 0.7]   (ok)          1    0.0%  1903      
    ##    (0.7, 1]   (bad)         0    0.0%  <NA>      
    ##    (1, Inf)   (very bad)    0    0.0%  <NA>      
    ## 
    ## All Pareto k estimates are ok (k < 0.7).
    ## See help('pareto-k-diagnostic') for details.

``` r
looT <- loo(bm2, save_psis = TRUE)
print(looT)
```

    ## 
    ## Computed from 4000 by 2913 log-likelihood matrix
    ## 
    ##          Estimate    SE
    ## elpd_loo   -784.0  66.2
    ## p_loo       165.5   2.5
    ## looic      1568.0 132.3
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## All Pareto k estimates are good (k < 0.5).
    ## See help('pareto-k-diagnostic') for details.

When comparing two fitted models, we can estimate the difference in
their expected predictive accuracy by the difference in elpd-dloo or
elpd-dwaic.

``` r
loo_compare(looNormal, looT)
```

    ##     elpd_diff se_diff
    ## bm2    0.0       0.0 
    ## bm  -583.8      44.7

WAIC criterion

``` r
waicNormal = waic(bm)
waicT = waic(bm2)
loo_compare(waicNormal, waicT)
```

    ##     elpd_diff se_diff
    ## bm2    0.0       0.0 
    ## bm  -583.7      44.7

As a last step, let’s do a posterior predictive check:

``` r
pp_check(bm2) + xlim(-3,3)
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

### Compare Coefficients

As a final check, let’s compare the coefficients for the normal Bayesian
mixed effects model and the t Distribution Bayesian mixed effects model.

``` r
coef_bm_df <- coef_bm$data
coef_bm2_df <- coef_bm2$data
coef_bm_df$Parameter[coef_bm_df$Parameter=="Intercept"] <- "(Intercept)"
coef_bm2_df$Parameter[coef_bm2_df$Parameter=="Intercept"] <- "(Intercept)"

joined_models <- inner_join(coef_bm_df, coef_bm2_df, by = "Parameter")
```

Let’s examine the coefficient differences with the different
likelihoods.

``` r
un_coef <- joined_models %>%
  rename(Normal_Estimate = Estimate.x, TDist_Estimate = Estimate.y) %>%
  select(Parameter, Normal_Estimate, TDist_Estimate) 

un_error <- joined_models %>%
  rename(Normal_low = `2.5%ile.x`, Normal_high = `97.5%ile.x`,TDist_low = `2.5%ile.y`, TDist_high = `97.5%ile.y`) %>%
  select(Parameter, Normal_low, Normal_high, TDist_low, TDist_high) 

var_order <- c("(Intercept)","vis_conditionuncertainty","vis_conditionhop")

inner_join(un_coef,un_error,by="Parameter") %>%
  tidyr::pivot_longer(-Parameter) %>%
  tidyr::separate(name, c("Model","Estimate"), sep = "_") %>%
  tidyr::pivot_wider(names_from = c("Estimate")) %>%
  mutate(Parameter = factor(Parameter, levels = rev(var_order))) %>%
  mutate(Model = factor(Model, levels = c("Normal", "TDist"))) %>%
  ggplot(aes(x = Parameter, color = Model)) +
  geom_hline(yintercept = 0, alpha = 0.4) +
  geom_point(aes(y = Estimate),  position=position_dodge(.9)) +
  geom_errorbar(aes(ymin = low, ymax = high),  position=position_dodge(.9)) +
  theme(legend.position = c(0.2,0.85),) +
  labs(title = "Uncertainty Difference", subtitle = "Vary by Response Distribution") +
  scale_color_manual(values = c("TDist" = "red",
                                "Normal"="black")) +  
  coord_flip()
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

We see the same for the coefficients standard errors:

``` r
joined_models %>%
  rename(Normal_Error = Est.Error.x, Lognormal_Error = Est.Error.y) %>%
  select(Parameter, Normal_Error, Lognormal_Error) %>%
  mutate(Diff_Error = round(Normal_Error - Lognormal_Error,3)) %>%
  knitr::kable()
```

| Parameter                 | Normal\_Error | Lognormal\_Error | Diff\_Error |
|:--------------------------|--------------:|-----------------:|------------:|
| (Intercept)               |     0.0159581 |        0.0113134 |       0.005 |
| vis\_conditionhop         |     0.0225645 |        0.0166427 |       0.006 |
| vis\_conditionuncertainty |     0.0231204 |        0.0159625 |       0.007 |

## Candidate models

For model selection, we will consider additional models.

-   `uncertainty_difference ~ vis_condition + (1|user_token) + true_correlation * vis_condition`

``` r
# https://discourse.mc-stan.org/t/smooth-spline-modeling-with-brm/6364
bm3 <- brms::brm(uncertainty_difference ~ vis_condition + (1|user_token) + true_correlation * vis_condition, data = df, family = student(link = "identity", link_sigma = "log", link_nu = "logm1"),, backend = "cmdstanr", cores = parallel::detectCores() - 1)

save(bm3, file = "../models/fit_baseline_diff_uncertainty3.rda")
```

-   `uncertainty_difference ~ vis_condition + (1|user_token) + pre_belief_distance * vis_condition`

``` r
bm4 <- brms::brm(uncertainty_difference ~ vis_condition + (1|user_token) + pre_belief_distance * vis_condition, data = df, family = student(link = "identity", link_sigma = "log", link_nu = "logm1"), backend = "cmdstanr", cores = parallel::detectCores() - 1)

save(bm4, file = "../models/fit_baseline_diff_uncertainty4.rda")
```

-   `uncertainty_difference ~ vis_condition + (1|user_token) + pre_belief_distance * vis_condition + true_correlation * vis_condition`

``` r
bm5 <- brms::brm(uncertainty_difference ~ vis_condition + (1|user_token) + pre_belief_distance * vis_condition + true_correlation * vis_condition, data = df, family = student(link = "identity", link_sigma = "log", link_nu = "logm1"),, backend = "cmdstanr", cores = parallel::detectCores() - 1)

save(bm5, file = "../models/fit_baseline_diff_uncertainty5.rda")
```

-   `uncertainty_difference ~ vis_condition + (1|user_token) + pre_attitude * vis_condition`

``` r
bm6 <- brms::brm(uncertainty_difference ~ vis_condition + (1|user_token) + pre_attitude_strength * vis_condition, data = df, family = student(link = "identity", link_sigma = "log", link_nu = "logm1"),, backend = "cmdstanr", cores = parallel::detectCores() - 1)

save(bm6, file = "../models/fit_baseline_diff_uncertainty6.rda")
```

``` r
load("../models/fit_baseline_diff_uncertainty3.rda")
load("../models/fit_baseline_diff_uncertainty4.rda")
load("../models/fit_baseline_diff_uncertainty5.rda")
load("../models/fit_baseline_diff_uncertainty6.rda")
```

``` r
waic3 = waic(bm3)
waic4 = waic(bm4)
waic5 = waic(bm5)
waic6 = waic(bm6)
```

``` r
loo_compare(waicNormal, waicT, waic3, waic4, waic5, waic6)
```

    ##     elpd_diff se_diff
    ## bm5    0.0       0.0 
    ## bm6   -0.1       5.1 
    ## bm4   -3.3       2.8 
    ## bm3   -3.9       3.4 
    ## bm2   -4.2       4.3 
    ## bm  -587.9      44.5

``` r
pp_check(bm6) + xlim(-3,3)
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
bm5
```

    ##  Family: student 
    ##   Links: mu = identity; sigma = identity; nu = identity 
    ## Formula: uncertainty_difference ~ vis_condition + (1 | user_token) + pre_belief_distance * vis_condition + true_correlation * vis_condition 
    ##    Data: df (Number of observations: 2913) 
    ##   Draws: 4 chains, each with iter = 1000; warmup = 0; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~user_token (Number of levels: 261) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.08      0.01     0.06     0.10 1.00     1072     2047
    ## 
    ## Population-Level Effects: 
    ##                                              Estimate Est.Error l-95% CI
    ## Intercept                                       -0.00      0.01    -0.03
    ## vis_conditionuncertainty                        -0.04      0.02    -0.08
    ## vis_conditionhop                                 0.01      0.02    -0.03
    ## pre_belief_distance                              0.03      0.01     0.00
    ## true_correlation                                -0.06      0.02    -0.11
    ## vis_conditionuncertainty:pre_belief_distance    -0.04      0.02    -0.08
    ## vis_conditionhop:pre_belief_distance            -0.07      0.02    -0.11
    ## vis_conditionuncertainty:true_correlation        0.06      0.03    -0.00
    ## vis_conditionhop:true_correlation                0.06      0.03    -0.00
    ##                                              u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept                                        0.02 1.00     1445     1952
    ## vis_conditionuncertainty                        -0.01 1.00     1551     2337
    ## vis_conditionhop                                 0.04 1.00     1534     2169
    ## pre_belief_distance                              0.06 1.00     1602     2230
    ## true_correlation                                -0.02 1.00     1614     2546
    ## vis_conditionuncertainty:pre_belief_distance     0.00 1.00     1722     2649
    ## vis_conditionhop:pre_belief_distance            -0.03 1.00     1807     2673
    ## vis_conditionuncertainty:true_correlation        0.12 1.00     1860     2566
    ## vis_conditionhop:true_correlation                0.13 1.00     1951     2310
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.17      0.01     0.16     0.18 1.00     2488     2538
    ## nu        1.86      0.10     1.69     2.06 1.00     3009     2873
    ## 
    ## Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
coef_bm5 <- coefplot(bm5)
coef_bm5
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
bm6
```

    ##  Family: student 
    ##   Links: mu = identity; sigma = identity; nu = identity 
    ## Formula: uncertainty_difference ~ vis_condition + (1 | user_token) + pre_attitude_strength * vis_condition 
    ##    Data: df (Number of observations: 2913) 
    ##   Draws: 4 chains, each with iter = 1000; warmup = 0; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~user_token (Number of levels: 261) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.08      0.01     0.07     0.10 1.00      870     1933
    ## 
    ## Population-Level Effects: 
    ##                                                Estimate Est.Error l-95% CI
    ## Intercept                                         -0.03      0.02    -0.07
    ## vis_conditionuncertainty                          -0.04      0.03    -0.10
    ## vis_conditionhop                                  -0.00      0.03    -0.06
    ## pre_attitude_strength                              0.01      0.01    -0.00
    ## vis_conditionuncertainty:pre_attitude_strength    -0.00      0.01    -0.02
    ## vis_conditionhop:pre_attitude_strength             0.00      0.01    -0.01
    ##                                                u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept                                          0.01 1.00     2447     2659
    ## vis_conditionuncertainty                           0.02 1.00     2344     2187
    ## vis_conditionhop                                   0.05 1.00     2423     2930
    ## pre_attitude_strength                              0.02 1.00     2786     2831
    ## vis_conditionuncertainty:pre_attitude_strength     0.01 1.00     2778     2726
    ## vis_conditionhop:pre_attitude_strength             0.02 1.00     2799     3104
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.17      0.01     0.16     0.18 1.00     3244     2756
    ## nu        1.85      0.10     1.68     2.04 1.00     4165     2934
    ## 
    ## Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
coef_bm6 <- coefplot(bm6)
coef_bm6
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

## Posterior Predictives

``` r
library(bayesplot)

# bm2

mcmc_areas(
  bm2,
  pars = c("b_vis_conditionuncertainty","b_vis_conditionhop"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
# bm5

mcmc_areas(
  bm5,
  pars = c("b_vis_conditionuncertainty","b_vis_conditionhop"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

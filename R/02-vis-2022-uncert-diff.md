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
#file_url = "https://drive.google.com/file/d/1QKkAmK5VXkm8CVP_QjjBUsfuMw5t3x5W/view?usp=sharing"
file_output = "../data/vis2022/model_data.csv"
#download.file(file_url,file_output)
df <- readr::read_csv("../data/vis2022/belief_data_prolific_all_exclude.csv")

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

save(bm, file = "../models/2022/fit_baseline_diff_uncertainty.rda")
```

``` r
load("../models/2022/fit_baseline_diff_uncertainty.rda")
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

save(bm2, file = "../models/2022/fit_baseline_diff_uncertainty2.rda")
```

``` r
load("../models/2022/fit_baseline_diff_uncertainty2.rda")
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
coef_bm2 <- coefplot(bm2)
coef_bm2
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

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

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

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

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

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

save(bm3, file = "../models/2022/fit_baseline_diff_uncertainty3.rda")
```

-   `uncertainty_difference ~ vis_condition + (1|user_token) + pre_belief_distance * vis_condition`

``` r
bm4 <- brms::brm(uncertainty_difference ~ vis_condition + (1|user_token) + pre_belief_distance * vis_condition, data = df, family = student(link = "identity", link_sigma = "log", link_nu = "logm1"), backend = "cmdstanr", cores = parallel::detectCores() - 1)

save(bm4, file = "../models/2022/fit_baseline_diff_uncertainty4.rda")
```

-   `uncertainty_difference ~ vis_condition + (1|user_token) + pre_belief_distance * vis_condition + true_correlation * vis_condition`

``` r
bm5 <- brms::brm(uncertainty_difference ~ vis_condition + (1|user_token) + pre_belief_distance * vis_condition + true_correlation * vis_condition, data = df, family = student(link = "identity", link_sigma = "log", link_nu = "logm1"),, backend = "cmdstanr", cores = parallel::detectCores() - 1)

save(bm5, file = "../models/2022/fit_baseline_diff_uncertainty5.rda")
```

-   `uncertainty_difference ~ vis_condition + (1|user_token) + pre_attitude * vis_condition`

``` r
bm6 <- brms::brm(uncertainty_difference ~ vis_condition + (1|user_token) + pre_attitude * vis_condition, data = df, family = student(link = "identity", link_sigma = "log", link_nu = "logm1"),, backend = "cmdstanr", cores = parallel::detectCores() - 1)

save(bm6, file = "../models/2022/fit_baseline_diff_uncertainty6.rda")
```

``` r
load("../models/2022/fit_baseline_diff_uncertainty3.rda")
load("../models/2022/fit_baseline_diff_uncertainty4.rda")
load("../models/2022/fit_baseline_diff_uncertainty5.rda")
load("../models/2022/fit_baseline_diff_uncertainty6.rda")
```

``` r
waic3 = waic(bm3)
waic4 = waic(bm4)
waic5 = waic(bm5)
waic6 = waic(bm6)
```

``` r
loo_compare(waicNormal, waicT, waic3, waic4, waic5)
```

    ##     elpd_diff se_diff
    ## bm5    0.0       0.0 
    ## bm4   -3.3       2.8 
    ## bm3   -3.9       3.4 
    ## bm2   -4.2       4.3 
    ## bm  -587.9      44.5

``` r
pp_check(bm6) + xlim(-3,3)
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
coef_bm5 <- coefplot(bm5)
coef_bm5
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
coef_bm6 <- coefplot(bm6)
coef_bm6
```

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

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

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

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

![](02-vis-2022-uncert-diff_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

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
df <- readr::read_csv("../data/vis2022/belief_data_prolific_all_2022_03_25.csv")

# refactor and categorize
df$vis_condition <- factor(df$vis_condition, c("uncertainty","scatter","hop"))
levels(df$vis_condition ) <- c("uncertainty","scatter","hop")

df$diffBeliefAbs <- abs(df$post_belief-df$pre_belief)
```

``` r
nrow(df)
```

    ## [1] 2532

``` r
# number of diffBeliefAbs missing
sum(is.na(df$diffBeliefAbs))
```

    ## [1] 0

## 3. Frequentist Mixed Effects Modeling (`lme4`)

``` r
# Absolute Belief Distance
# first model is normal response
m1 = lmer(diffBeliefAbs ~ vis_condition +  (1|user_token), df)

# second model is lognormal response
#m2 = glmer(diffBeliefAbs ~ vis_condition +  (1|user_token), df, family = gaussian(link = "log"))
```

``` r
a <- plot_model(m1,show.values = TRUE, vline.color = "grey", value.offset = .4, value.size = 3, type="est", show.intercept = TRUE ) +
  scale_y_continuous(breaks=seq(-.75,0.75,.25)) +
  theme(axis.text.y = element_text(size = 8),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = "Absolute Belief Difference", title = "") +
  ylim(-0.25, 0.9)

a
```

![](01-vis-2022-abs-belief_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## 4a. Absolute Belief Difference

### Bayesian Mixed Effects

Let’s examine the first regression to estimate the effect on the
absolute belief change (`diffBeliefAbs`). We’ll use the same functional
form as model `m`.

``` r
library(brms)

# assume normal response variable
bm <- brms::brm(diffBeliefAbs ~ vis_condition + (1|user_token), data = df)

save(bm, file = "../models/2022/fit_baseline_abs_belief.rda")
```

``` r
load("../models/2022/fit_baseline_abs_belief.rda")
```

First let’s look at metadata around the model.

``` r
coef_bm <- coefplot(bm)
coef_bm
```

![](01-vis-2022-abs-belief_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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

| Parameter             | Bayesian\_Estimate | Freq\_Estimate | abs\_diff |
|:----------------------|-------------------:|---------------:|----------:|
| (Intercept)           |          0.4178505 |      0.4186077 |     0.001 |
| vis\_conditionhop     |         -0.0177092 |     -0.0180369 |     0.000 |
| vis\_conditionscatter |         -0.0101661 |     -0.0104712 |     0.000 |

We see the same for the coefficients standard errors (though they mean
slightly different things):

``` r
joined_models %>%
  rename(Bayesian_Error = Est.Error, Freq_Error = std.error) %>%
  select(Parameter, Bayesian_Error, Freq_Error) %>%
  mutate(abs_diff_error = round(abs(Bayesian_Error - Freq_Error),3)) %>%
  knitr::kable()
```

| Parameter             | Bayesian\_Error | Freq\_Error | abs\_diff\_error |
|:----------------------|----------------:|------------:|-----------------:|
| (Intercept)           |       0.0206086 |   0.0202500 |                0 |
| vis\_conditionhop     |       0.0299879 |   0.0296429 |                0 |
| vis\_conditionscatter |       0.0305282 |   0.0307871 |                0 |

### Model convergence / posterior predictive check

The convergence stats also look good - Rhat’s are at 1 and we have
“fuzzy caterpillars”.

``` r
plot(bm)
```

![](01-vis-2022-abs-belief_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

But remember - convergence doesn’t mean great fit. Let’s evaluate
overfitting with Posterior Predictive Checks. We’ll do 10 draws and
compare to actual.

``` r
pp_check(bm)
```

![](01-vis-2022-abs-belief_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Modify response (likelihood) to lognormal

Let’s try instead a lognormal likelihood (specifically the
`hurdle_lognormal` because we have a handful of cases where
diffBeliefAbs equals zero (see [brms
comment](https://discourse.mc-stan.org/t/convergence-fails-for-every-truncated-gaussian-model/10040/2)))..

``` r
df$diffBeliefAbsAdjusted <- ifelse(df$diffBeliefAbs==0,0.01,df$diffBeliefAbs)

bm2 <- brms::brm(diffBeliefAbsAdjusted ~ vis_condition + (1|user_token), data = df, family = hurdle_lognormal(link = "identity", link_sigma = "log"))

save(bm2, file = "../models/2022/fit_baseline_abs_belief2.rda")
```

``` r
load("../models/2022/fit_baseline_abs_belief2.rda")
```

### What are model priors?

``` r
bm2$prior
```

    ##                    prior     class                 coef      group resp dpar
    ##                   (flat)         b                                          
    ##                   (flat)         b     vis_conditionhop                     
    ##                   (flat)         b vis_conditionscatter                     
    ##               beta(1, 1)        hu                                          
    ##  student_t(3, -1.3, 2.5) Intercept                                          
    ##     student_t(3, 0, 2.5)        sd                                          
    ##     student_t(3, 0, 2.5)        sd                      user_token          
    ##     student_t(3, 0, 2.5)        sd            Intercept user_token          
    ##     student_t(3, 0, 2.5)     sigma                                          
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

![](01-vis-2022-abs-belief_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### Model Comparison

First, let’s use leave-one-out (loo) cross-validation. It will also
provide estimate to determine point leverage (aka outliers).

``` r
looNormal <- loo(bm, save_psis = TRUE)
print(looNormal)
```

    ## 
    ## Computed from 4000 by 2532 log-likelihood matrix
    ## 
    ##          Estimate   SE
    ## elpd_loo  -1250.3 48.4
    ## p_loo       132.3  5.0
    ## looic      2500.6 96.7
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## All Pareto k estimates are good (k < 0.5).
    ## See help('pareto-k-diagnostic') for details.

``` r
looNormal <- loo(bm, save_psis = TRUE)
print(looNormal)
```

    ## 
    ## Computed from 4000 by 2532 log-likelihood matrix
    ## 
    ##          Estimate   SE
    ## elpd_loo  -1250.3 48.4
    ## p_loo       132.3  5.0
    ## looic      2500.6 96.7
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## All Pareto k estimates are good (k < 0.5).
    ## See help('pareto-k-diagnostic') for details.

``` r
looLog <- loo(bm2, save_psis = TRUE)
print(looLog)
```

    ## 
    ## Computed from 4000 by 2532 log-likelihood matrix
    ## 
    ##          Estimate    SE
    ## elpd_loo   -485.7  60.9
    ## p_loo       112.7   4.5
    ## looic       971.3 121.8
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## Pareto k diagnostic values:
    ##                          Count Pct.    Min. n_eff
    ## (-Inf, 0.5]   (good)     2530  99.9%   1150      
    ##  (0.5, 0.7]   (ok)          2   0.1%   309       
    ##    (0.7, 1]   (bad)         0   0.0%   <NA>      
    ##    (1, Inf)   (very bad)    0   0.0%   <NA>      
    ## 
    ## All Pareto k estimates are ok (k < 0.7).
    ## See help('pareto-k-diagnostic') for details.

When comparing two fitted models, we can estimate the difference in
their expected predictive accuracy by the difference in elpd-dloo or
elpd-dwaic.

``` r
loo_compare(looNormal, looLog)
```

    ##     elpd_diff se_diff
    ## bm2    0.0       0.0 
    ## bm  -764.6      55.1

WAIC criterion

``` r
waicNormal = waic(bm)
waicLog = waic(bm2)
loo_compare(waicNormal, waicLog)
```

    ##     elpd_diff se_diff
    ## bm2    0.0       0.0 
    ## bm  -764.6      55.1

As a last step, let’s do a posterior predictive check:

``` r
pp_check(bm2) + xlim(-1,3)
```

![](01-vis-2022-abs-belief_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Compare Coefficients

As a final check, let’s compare the coefficients for the normal Bayesian
mixed effects model and the (hurdle) Lognormal Bayesian mixed effects
model.

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
  rename(Normal_Estimate = Estimate.x, Lognormal_Estimate = Estimate.y) %>%
  select(Parameter, Normal_Estimate, Lognormal_Estimate) 

un_error <- joined_models %>%
  rename(Normal_low = `2.5%ile.x`, Normal_high = `97.5%ile.x`,Lognormal_low = `2.5%ile.y`, Lognormal_high = `97.5%ile.y`) %>%
  select(Parameter, Normal_low, Normal_high, Lognormal_low, Lognormal_high) 

var_order <- c("(Intercept)","vis_conditionscatter","vis_conditionhop")

inner_join(un_coef,un_error,by="Parameter") %>%
  tidyr::pivot_longer(-Parameter) %>%
  tidyr::separate(name, c("Model","Estimate"), sep = "_") %>%
  tidyr::pivot_wider(names_from = c("Estimate")) %>%
  mutate(Parameter = factor(Parameter, levels = rev(var_order))) %>%
  mutate(Model = factor(Model, levels = c("Normal", "Lognormal"))) %>%
  ggplot(aes(x = Parameter, color = Model)) +
  geom_hline(yintercept = 0, alpha = 0.4) +
  geom_point(aes(y = Estimate),  position=position_dodge(.9)) +
  geom_errorbar(aes(ymin = low, ymax = high),  position=position_dodge(.9)) +
  theme(legend.position = c(0.2,0.2),) +
  labs(title = "Abs Belief Difference", subtitle = "Vary by Response Distribution") +
  scale_color_manual(values = c("Lognormal" = "red",
                                "Normal"="black")) +  
  coord_flip()
```

![](01-vis-2022-abs-belief_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

We see the same for the coefficients standard errors:

``` r
joined_models %>%
  rename(Normal_Error = Est.Error.x, Lognormal_Error = Est.Error.y) %>%
  select(Parameter, Normal_Error, Lognormal_Error) %>%
  mutate(Diff_Error = round(Normal_Error - Lognormal_Error,3)) %>%
  knitr::kable()
```

| Parameter             | Normal\_Error | Lognormal\_Error | Diff\_Error |
|:----------------------|--------------:|-----------------:|------------:|
| (Intercept)           |     0.0206086 |        0.0640709 |      -0.043 |
| vis\_conditionhop     |     0.0299879 |        0.0957013 |      -0.066 |
| vis\_conditionscatter |     0.0305282 |        0.0982067 |      -0.068 |

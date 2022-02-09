Study 3: Mixed Effects Modeling
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
df <- read.csv(file="../data/vis2020/data_exclude.csv")

# refactor and categorize
df$visGroup <- factor(df$visGroup, c("line","band","hop"))
levels(df$visGroup ) <- c("Line","Cone","HOPs")
df$nDataShown <- factor(df$nDataShown)

df <- rename(df, 
             sampleUncertainty = uncertaintyShown,
             visTreatment = visGroup)

df <- within(df, visTreatment <- relevel(visTreatment, ref = 1))
```

## 3. Exploratory Graphs

``` r
g1 <- df %>%
  rename(Congruency = congruency) %>%
  ggplot(aes(x=preBeliefDistance,fill=Congruency)) + 
  geom_density(alpha=0.5) +
  annotate("text", x = 1.4, y = 2.8, label = "Incongruent", size = 2.5) +
  annotate("text", x = 0.75, y = 3.7, label = "Congruent", size = 2.5) +
  theme(legend.position = "none") + 
  labs(x = " ", y = " ", subtitle = "Pre-Belief Distance") 

g2 <- df %>%
  ggplot(aes(x=sampleUncertainty,fill=nDataShown)) + 
  geom_density(alpha=0.5) +
  annotate("text", x = 1.3, y = 2, label = "Data Shown\n(n = 10)", size = 2.5) +
  annotate("text", x = 0.75, y = 3.7, label = "Data Shown\n(n = 100)", size = 2.5) +
  theme(legend.position = "none") +
  labs(x = " ", y = " ", subtitle = "Sample Uncertainty")

cowplot::plot_grid(g1, g2,
                   label_x = -0.2,
                   ncol = 2)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## 4. Frequentist Mixed Effects Modeling (`lme4`)

``` r
# Absolute Belief Distance
m = lmer(diffBeliefAbs ~ visTreatment * preBeliefDistance + visTreatment * sampleUncertainty +  sampleUncertainty * preBeliefDistance  +  (1|usertoken) + (1|vars),df)
# Uncertainty Difference
m1 = lmer(diffUncertainty ~ visTreatment * preBeliefDistance + visTreatment * sampleUncertainty +  sampleUncertainty * preBeliefDistance  + (1|usertoken) + (1|vars),df)
```

``` r
a <- plot_model(m,show.values = TRUE, vline.color = "grey", value.offset = .4, value.size = 3, type="est", show.intercept = TRUE ) +
  scale_y_continuous(breaks=seq(-.75,0.75,.25)) +
  theme(axis.text.y = element_text(size = 8),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = "Absolute Belief Difference", title = "") +
  ylim(-0.25, 0.9)

b <- plot_model(m1, vline.color = "grey",show.values = TRUE, value.offset = .4, value.size = 3, show.intercept = TRUE) +
  ylim(-.3,.3) +
  theme(axis.text.y=element_blank(),
        plot.subtitle=element_text(size=11), plot.title = element_text(size = 1)) +
  labs(subtitle = "Uncertainty Difference", title = "")

# final plots
library(cowplot)

plot_grid(a,
  b,
  label_x = -0.2,
  ncol = 2,
  rel_widths = c(4.6, 2.4)) 
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## 5a. Absolute Belief Difference

### Bayesian Mixed Effects

For the Vis 2020 paper, we did not run a Bayesian mixed effects model.

Let’s examine the first regression to estimate the effect on the
absolute belief change (`diffBeliefAbs`). We’ll use the same functional
form as model `m`.

``` r
library(brms)

bm <- brms::brm(diffBeliefAbs ~ visTreatment * preBeliefDistance + visTreatment * sampleUncertainty +  sampleUncertainty * preBeliefDistance + (1|usertoken) + (1|vars), data = df)
```

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppParallel/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/opt/R/arm64/include   -fPIC  -falign-functions=64 -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1
    ## 
    ## SAMPLING FOR MODEL '59195a97df8e8d442eb5755719ebddfc' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000255 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.55 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 9.67148 seconds (Warm-up)
    ## Chain 1:                5.15899 seconds (Sampling)
    ## Chain 1:                14.8305 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '59195a97df8e8d442eb5755719ebddfc' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.000158 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.58 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 9.63505 seconds (Warm-up)
    ## Chain 2:                5.05123 seconds (Sampling)
    ## Chain 2:                14.6863 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '59195a97df8e8d442eb5755719ebddfc' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000157 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.57 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 9.64559 seconds (Warm-up)
    ## Chain 3:                4.96968 seconds (Sampling)
    ## Chain 3:                14.6153 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '59195a97df8e8d442eb5755719ebddfc' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000172 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.72 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 9.99218 seconds (Warm-up)
    ## Chain 4:                5.06903 seconds (Sampling)
    ## Chain 4:                15.0612 seconds (Total)
    ## Chain 4:

First let’s look at metadata around the model.

``` r
coefplot(bm)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
coef_m_df <- a$data %>% rename(Parameter = term) %>% mutate(Parameter = as.character(Parameter))

coef_bm <- coefplot(bm)

coef_bm_df <- coef_bm$data
coef_bm_df$Parameter[coef_bm_df$Parameter=="Intercept"] <- "(Intercept)"

joined_models <- inner_join(coef_bm_df, coef_m_df, by = "Parameter")
```

Notice that the coefficients are very similar to Frequentist:

``` r
joined_models %>%
  rename(Bayesian_Estimate = Estimate, Freq_Estimate = estimate) %>%
  select(Bayesian_Estimate, Freq_Estimate) %>%
  mutate(abs_diff = round(abs(Bayesian_Estimate - Freq_Estimate),3)) %>%
  knitr::kable()
```

| Bayesian\_Estimate | Freq\_Estimate | abs\_diff |
|-------------------:|---------------:|----------:|
|          0.1382383 |      0.1340957 |     0.004 |
|          0.7193759 |      0.7252758 |     0.006 |
|         -0.0170234 |     -0.0210087 |     0.004 |
|         -0.0250486 |     -0.0226014 |     0.002 |
|         -0.0046405 |     -0.0034115 |     0.001 |
|         -0.1171013 |     -0.1171506 |     0.000 |
|          0.0248408 |      0.0244427 |     0.000 |
|          0.0951004 |      0.0951972 |     0.000 |
|         -0.1125716 |     -0.1128555 |     0.000 |
|         -0.0702860 |     -0.0699720 |     0.000 |

We see the same for the coefficients standard errors (though they mean
slightly different things):

``` r
joined_models %>%
  rename(Bayesian_Error = Est.Error, Freq_Error = std.error) %>%
  select(Bayesian_Error, Freq_Error) %>%
  mutate(abs_diff_error = round(abs(Bayesian_Error - Freq_Error),3)) %>%
  knitr::kable()
```

| Bayesian\_Error | Freq\_Error | abs\_diff\_error |
|----------------:|------------:|-----------------:|
|       0.0399810 |   0.0364427 |            0.004 |
|       0.0506123 |   0.0420362 |            0.009 |
|       0.0506271 |   0.0439540 |            0.007 |
|       0.0387089 |   0.0364888 |            0.002 |
|       0.0391010 |   0.0390879 |            0.000 |
|       0.0357425 |   0.0354924 |            0.000 |
|       0.0313559 |   0.0313056 |            0.000 |
|       0.0395057 |   0.0396635 |            0.000 |
|       0.0360918 |   0.0357082 |            0.000 |
|       0.0316552 |   0.0320763 |            0.000 |

### Model convergence / posterior predictive check

The convergence stats also look good - Rhat’s are at 1 and we have
“fuzzy catepillars”.

``` r
plot(bm)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

But remember - convergence doesn’t mean great fit. Let’s evaluate
overfitting with Posterior Predictive Checks. We’ll do 10 draws and
compare to actual.

``` r
pp_check(bm)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

There looks like some misspecification.

### Modify response (likelihood) to lognormal

Let’s try instead a lognormal likelihood.

### What are model priors?

``` r
bm2$prior
```

    ##                  prior     class                                coef     group
    ##                 (flat)         b                                              
    ##                 (flat)         b                   preBeliefDistance          
    ##                 (flat)         b preBeliefDistance:sampleUncertainty          
    ##                 (flat)         b                   sampleUncertainty          
    ##                 (flat)         b                    visTreatmentCone          
    ##                 (flat)         b  visTreatmentCone:preBeliefDistance          
    ##                 (flat)         b  visTreatmentCone:sampleUncertainty          
    ##                 (flat)         b                    visTreatmentHOPs          
    ##                 (flat)         b  visTreatmentHOPs:preBeliefDistance          
    ##                 (flat)         b  visTreatmentHOPs:sampleUncertainty          
    ##  student_t(3, -1, 2.5) Intercept                                              
    ##   student_t(3, 0, 2.5)        sd                                              
    ##   student_t(3, 0, 2.5)        sd                                     usertoken
    ##   student_t(3, 0, 2.5)        sd                           Intercept usertoken
    ##   student_t(3, 0, 2.5)        sd                                          vars
    ##   student_t(3, 0, 2.5)        sd                           Intercept      vars
    ##   student_t(3, 0, 2.5)     sigma                                              
    ##  resp dpar nlpar bound       source
    ##                             default
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                             default
    ##                             default
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                             default

### What are the coefficients?

``` r
coefplot(bm2)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Model Comparison

First, let’s use leave-one-out (loo) cross-validation. It will also
provide estimate to determine point leverage (aka outliers).

``` r
looNormal <- loo(bm, save_psis = TRUE)
print(looNormal)
```

    ## 
    ## Computed from 4000 by 4260 log-likelihood matrix
    ## 
    ##          Estimate    SE
    ## elpd_loo  -1826.5  76.4
    ## p_loo       218.8   7.7
    ## looic      3652.9 152.7
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
    ## Computed from 4000 by 4260 log-likelihood matrix
    ## 
    ##          Estimate    SE
    ## elpd_loo  -1826.5  76.4
    ## p_loo       218.8   7.7
    ## looic      3652.9 152.7
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
    ## Computed from 4000 by 4260 log-likelihood matrix
    ## 
    ##          Estimate    SE
    ## elpd_loo  -1466.4  81.1
    ## p_loo       203.8   7.1
    ## looic      2932.9 162.2
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## Pareto k diagnostic values:
    ##                          Count Pct.    Min. n_eff
    ## (-Inf, 0.5]   (good)     4259  100.0%  580       
    ##  (0.5, 0.7]   (ok)          1    0.0%  1730      
    ##    (0.7, 1]   (bad)         0    0.0%  <NA>      
    ##    (1, Inf)   (very bad)    0    0.0%  <NA>      
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
    ## bm  -360.0      78.4

WAIC criterion

``` r
waicNormal = waic(bm)
waicLog = waic(bm2)
loo_compare(waicNormal, waicLog)
```

    ##     elpd_diff se_diff
    ## bm2    0.0       0.0 
    ## bm  -359.8      78.4

As a last step, let’s do a posterior predictive check:

``` r
pp_check(bm2) + xlim(-1,3)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Better – but we’re still overfitting. It appears to be bimodal, maybe
even “tri”-modal.

This may be a solution to do a Bayesian mixture for lognormal. [Chapter
20 of “An Introduction to Bayesian Data Analysis for Cognitive
Science”](https://vasishth.github.io/bayescogsci/book/a-mixture-model-of-the-speed-accuracy-trade-off-the-fast-guess-model-account.html)

## 5b. Uncertainty Difference

### Bayesian Mixed Effects

Let’s now examine uncertainty difference (`diffUncertainty`). We’ll use
the same functional form as model `m`.

``` r
bm_u <- brms::brm(diffUncertainty ~ visTreatment * preBeliefDistance + visTreatment * sampleUncertainty +  sampleUncertainty * preBeliefDistance + (1|usertoken) + (1|vars), data = df)
```

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Users/rhymenoceros/Desktop/correlation-belief-vaccine/renv/library/R-4.1/aarch64-apple-darwin20/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/unsupported"  -I"/Users/rhymenoceros/Desktop/correlation-belief-vaccine/renv/library/R-4.1/aarch64-apple-darwin20/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/"  -I"/Users/rhymenoceros/Desktop/correlation-belief-vaccine/renv/library/R-4.1/aarch64-apple-darwin20/RcppParallel/include/"  -I"/Users/rhymenoceros/Desktop/correlation-belief-vaccine/renv/library/R-4.1/aarch64-apple-darwin20/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/opt/R/arm64/include   -fPIC  -falign-functions=64 -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1
    ## 
    ## SAMPLING FOR MODEL '60d941588c6f3464bfcb0ac83aa444df' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0.000289 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.89 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 11.1987 seconds (Warm-up)
    ## Chain 1:                5.19646 seconds (Sampling)
    ## Chain 1:                16.3951 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '60d941588c6f3464bfcb0ac83aa444df' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0.00016 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.6 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 11.2981 seconds (Warm-up)
    ## Chain 2:                5.2345 seconds (Sampling)
    ## Chain 2:                16.5326 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '60d941588c6f3464bfcb0ac83aa444df' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0.000162 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 1.62 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 11.2082 seconds (Warm-up)
    ## Chain 3:                5.21268 seconds (Sampling)
    ## Chain 3:                16.4209 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '60d941588c6f3464bfcb0ac83aa444df' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.000156 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 1.56 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 11.5681 seconds (Warm-up)
    ## Chain 4:                5.19867 seconds (Sampling)
    ## Chain 4:                16.7668 seconds (Total)
    ## Chain 4:

``` r
#save(bm_u, file = "../models/bm_u.rda")
```

First let’s look at metadata around the model.

``` r
coefplot(bm_u)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
coef_mu_df <- b$data %>% rename(Parameter = term) %>% mutate(Parameter = as.character(Parameter))

coef_bm_u <- coefplot(bm_u)

coef_bm_u_df <- coef_bm_u$data
coef_bm_u_df$Parameter[coef_bm_u_df$Parameter=="Intercept"] <- "(Intercept)"

joined_models <- inner_join(coef_bm_u_df, coef_mu_df, by = "Parameter")
```

Notice that the coefficients are very similar to Frequentist:

``` r
joined_models %>%
  rename(Bayesian_Estimate = Estimate, Freq_Estimate = estimate) %>%
  select(Bayesian_Estimate, Freq_Estimate) %>%
  mutate(abs_diff = round(abs(Bayesian_Estimate - Freq_Estimate),3)) %>%
  knitr::kable()
```

| Bayesian\_Estimate | Freq\_Estimate | abs\_diff |
|-------------------:|---------------:|----------:|
|         -0.0891671 |     -0.0875698 |     0.002 |
|          0.0131538 |      0.0120174 |     0.001 |
|          0.1018924 |      0.1013096 |     0.001 |
|          0.1293593 |      0.1285109 |     0.001 |
|         -0.1862188 |     -0.1863117 |     0.000 |
|          0.0672140 |      0.0676643 |     0.000 |
|          0.2009528 |      0.2007491 |     0.000 |
|         -0.0679236 |     -0.0686954 |     0.001 |
|          0.0043500 |      0.0055774 |     0.001 |
|          0.1301434 |      0.1298018 |     0.000 |

We see the same for the coefficients standard errors (though they mean
slightly different things):

``` r
joined_models %>%
  rename(Bayesian_Error = Est.Error, Freq_Error = std.error) %>%
  select(Bayesian_Error, Freq_Error) %>%
  mutate(abs_diff_error = round(abs(Bayesian_Error - Freq_Error),3)) %>%
  knitr::kable()
```

| Bayesian\_Error | Freq\_Error | abs\_diff\_error |
|----------------:|------------:|-----------------:|
|       0.0578988 |   0.0564353 |            0.001 |
|       0.0689174 |   0.0673025 |            0.002 |
|       0.0673181 |   0.0650201 |            0.002 |
|       0.0549677 |   0.0537443 |            0.001 |
|       0.0445922 |   0.0447271 |            0.000 |
|       0.0475922 |   0.0475906 |            0.000 |
|       0.0430368 |   0.0419628 |            0.001 |
|       0.0459529 |   0.0453695 |            0.001 |
|       0.0484136 |   0.0479265 |            0.000 |
|       0.0445491 |   0.0430000 |            0.002 |

### Model convergence / posterior predictive check

The convergence stats also look good - Rhat’s are at 1 and we have
“fuzzy catepillars”.

``` r
plot(bm_u)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-24-3.png)<!-- -->

But remember - convergence doesn’t mean great fit. Let’s evaluate
overfitting with Posterior Predictive Checks. We’ll do 10 draws and
compare to actual.

``` r
pp_check(bm_u)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

There looks like some misspecification but at least we’re

### Modify response (likelihood) to lognormal

Let’s try instead a student t distribution

### What are model priors?

``` r
bm2_u$prior
```

    ##                 prior     class                                coef     group
    ##                (flat)         b                                              
    ##                (flat)         b                   preBeliefDistance          
    ##                (flat)         b preBeliefDistance:sampleUncertainty          
    ##                (flat)         b                   sampleUncertainty          
    ##                (flat)         b                    visTreatmentCone          
    ##                (flat)         b  visTreatmentCone:preBeliefDistance          
    ##                (flat)         b  visTreatmentCone:sampleUncertainty          
    ##                (flat)         b                    visTreatmentHOPs          
    ##                (flat)         b  visTreatmentHOPs:preBeliefDistance          
    ##                (flat)         b  visTreatmentHOPs:sampleUncertainty          
    ##  student_t(3, 0, 2.5) Intercept                                              
    ##         gamma(2, 0.1)        nu                                              
    ##  student_t(3, 0, 2.5)        sd                                              
    ##  student_t(3, 0, 2.5)        sd                                     usertoken
    ##  student_t(3, 0, 2.5)        sd                           Intercept usertoken
    ##  student_t(3, 0, 2.5)        sd                                          vars
    ##  student_t(3, 0, 2.5)        sd                           Intercept      vars
    ##  student_t(3, 0, 2.5)     sigma                                              
    ##  resp dpar nlpar bound       source
    ##                             default
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                             default
    ##                             default
    ##                             default
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                        (vectorized)
    ##                             default

### What are the coefficients?

``` r
coefplot(bm2_u)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

### Model Comparison

First, let’s use leave-one-out (loo) cross-validation. It will also
provide estimate to determine point leverage (aka outliers).

``` r
looNormal_u <- loo(bm_u, save_psis = TRUE)
print(looNormal_u)
```

    ## 
    ## Computed from 4000 by 4267 log-likelihood matrix
    ## 
    ##          Estimate    SE
    ## elpd_loo  -3052.0  68.4
    ## p_loo       133.6   4.3
    ## looic      6104.1 136.9
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## All Pareto k estimates are good (k < 0.5).
    ## See help('pareto-k-diagnostic') for details.

``` r
looT <- loo(bm2_u, save_psis = TRUE)
print(looT)
```

    ## 
    ## Computed from 4000 by 4267 log-likelihood matrix
    ## 
    ##          Estimate    SE
    ## elpd_loo  -2690.7  72.1
    ## p_loo       153.3   1.7
    ## looic      5381.4 144.3
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## All Pareto k estimates are good (k < 0.5).
    ## See help('pareto-k-diagnostic') for details.

When comparing two fitted models, we can estimate the difference in
their expected predictive accuracy by the difference in elpd-dloo or
elpd-dwaic.

``` r
loo_compare(looNormal_u, looT)
```

    ##       elpd_diff se_diff
    ## bm2_u    0.0       0.0 
    ## bm_u  -361.4      31.0

WAIC criterion

``` r
waicNormal_u = waic(bm_u)
waicT = waic(bm2_u)
loo_compare(waicNormal_u, waicT)
```

    ##       elpd_diff se_diff
    ## bm2_u    0.0       0.0 
    ## bm_u  -361.3      31.0

As a last step, let’s do a posterior predictive check:

``` r
pp_check(bm2_u) + xlim(-3,3)
```

![](03-vis-2020-mixedeffects_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

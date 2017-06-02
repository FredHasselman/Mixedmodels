# Generalized Mixed Models
Fred Hasselman  
02/06/2017  





# **Introduction to the GMM family**

Many different terms are used to describe some form of the hierarchical *Generalized Mixed Model* family of statistical models, or *GeMMs*  for short (pronounce like the precious stones, *gems*). 

## What's in a name?

* _Generalized_ refers to the fact that we can use any distribution from [the exponential family](https://en.wikipedia.org/wiki/Exponential_family#Table_of_distributions) as a model for our response variable(s), because we know the [functions that link](https://en.wikipedia.org/wiki/Hierarchical_generalized_linear_model#Models_with_different_distributions_and_link_functions) the parameters of those distributions. 
* _Mixed_ refers to possibility to have different types of effects in the model, *fixed* and *random* effects.
    + Random effect structures can be used to model dependencies in the data ([heteroscedasticity](http://www.dummies.com/education/economics/econometrics/how-to-distinguish-between-homoskedastic-and-heteroskedastic-disturbances/)) that affects the eficiency and accuracy of OLS estimators.
    + Random effect structures can be *simple*, *autoregressive*, hierarchically *nested*, *cross-classified*, of the *multiple membership* kind or any combination of those. 
* _GeMMs_ can be *linear* or *nonlinear* models, and to estimate model parameters (OLS cannot be used) different methods are available; *Maximum Likelihood* estimators, *MCMC* and *Bayesian* methods. There are also extensions based on *vector autoregression (VAR)* and *LASSO*.


### When to use GeMMs? {-}

**Always**  

### Why? {-}

The *General Linear Model*  (the classical regression models and ANOVA variations) are just special cases of GeMMs in which the following assumptions hold (cf. Field):

* _Linear additivity_ of effects
* _Normality_ something or other
* _Homogeneity_ of variance
* _Independence_ of observations

For real world data, especially in the social and life sciences, at least one of those assumptions will be violated. So the answer to the *Why?* question is: **Because we need more realistic statistical models!**.

## `R` packages

We'll use the `lme4` package and some additional tools.

On thing you will notice is that `lme4` no longer provides p-values for fixed effects, here's what the authors have to say about it:

>  One of the most frequently asked questions about 'lme4' is "how do I calculate p-values for estimated parameters?" Previous versions of `lme4` provided the `mcmcsamp` function, which efficiently generated a Markov chain Monte Carlo sample from the posterior distribution of the parameters, assuming flat (scaled likelihood) priors. Due to difficulty in constructing a version of 'mcmcsamp' that was reliable even in cases where the estimated random effect variances were near zero (e.g. <https://stat.ethz.ch/pipermail/r-sig-mixed-models/2009q4/003115.html> `mcmcsamp` has been withdrawn (or more precisely, not updated to work with `lme4` versions greater than 1.0.0).




```r
install.packages(c("lme4","lmerTest","pbkrtest","nlme","sjPlot"), dependencies = TRUE, repos = "https://cloud.r-project.org")
```


We'll also use some standard packages for plotting data and model results.

```r
install.packages(c("lattice","latticeExtra","ggplot2","gridExtra","scales"), dependencies = TRUE, repos = "https://cloud.r-project.org")
```


These libraries are not essential, but generally will enhance you `R` experience.

```r
install.packages(c("plyr","tidyverse","rio"), dependencies = TRUE, repos = "https://cloud.r-project.org")
```


## Online sources

Here is a (non-exhaustive) list of great sources:

* [lme4: Mixed-effects modeling with R (Bates)](http://lme4.r-forge.r-project.org/lMMwR/lrgprt.pdf)
    + [lme4 vignette](https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf)
    + [Mixed models in R using `lme4` and `nlme`](https://socserv.socsci.mcmaster.ca/jfox/Books/Companion/appendix/Appendix-Mixed-Models.pdf)
    + [Writing up *lmer* results](https://web.stanford.edu/class/psych253/section/section_8/lmer_examples.html)
    + [Getting started with mixed effect models in r (Knowles, R-bloggers)](https://www.r-bloggers.com/getting-started-with-mixed-effect-models-in-r/)
    + [General guide to mixed models in r](http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html)
* [Center for multilevel modeling](http://www.bristol.ac.uk/cmm/)
    + From the creators of MLwiN
* [Multilevel Analysis (Hox)](https://stats.idre.ucla.edu/other/examples/ma-hox/)
    + Code examples from book in `HLM`, `SAS`, `Stata`, `R`
* [Introduction to Multilevel Modeling (Kreft & de Leeuw)](https://stats.idre.ucla.edu/other/examples/imm/)
    + Code examples from book in `HLM`, `R`, `SPSS`, `Mplus`, `SAS`, `Stata`
* [Applied Longitudinal Data Analysis (Singer & Willett)](https://stats.idre.ucla.edu/other/examples/alda/)
    + Code examples from book in `R`, `SPSS`, `Mplus`, `SAS`, `Stata`
* [Using lme/lmer to fit 2 and 3 level longitudinal models (Rpsychologist)](http://rpsychologist.com/r-guide-longitudinal-lme-lmer)
    + Excellent site with many examples
* [Inferential Methods for Linear Mixed Models](http://www.maths.bath.ac.uk/~jjf23/mixchange/index.html)
    + Includes examples for multivariate mutilevel modeling
* [R to MLwiN](https://www.jstatsoft.org/article/view/v072i10)
* [Power Analysis for Random Effects Models (Westfall)](http://jakewestfall.org/publications/JWK_AnnRev.pdf)
    + Provides `R`, `SPSS` and `SAS` code
    + Also see the [online tool](https://jakewestfall.shinyapps.io/two_factor_power/) and [additional topics supplement](http://jakewestfall.org/publications/JWK_AnnRev_Appendix.pdf)
* [Stan. Bayesian inference for multilevel GLMMs and more](http://mc-stan.org/interfaces/)
* [Generalized Additive Mixed Models (GAMM) for for modeling timeseries data](http://www.sfs.uni-tuebingen.de/~jvanrij/Tutorial/GAMM.html)
    + [Examples](http://www.sfs.uni-tuebingen.de/~hbaayen/publications/supplementCave.pdf)
* [Multilevel Analysis (Snijder & Boskers)](https://stats.idre.ucla.edu/other/examples/ma-snijders/)
    + Code examples from book in `HLM`, `Mplus`, `SAS`, `Stata`


**Random Effect Models**

# Classical linear regression  {.tabset .tabset-fade .tabset-pills}  

OLS regression versus the variance components model.

## Assignment {-}
 
Let's repeat the example form the slides. Import the data from [Github](https://github.com/FredHasselman/Mixedmodels/tree/master/Cologne/AssignmentData), or run the code below and load it directly into R^[Importing SPSS files into R can sometimes lead to strange behaviour, because SPSS assigns both *variable* and *value* labels, which most software packages cannot handle. If you can't get rid of the labels, use `rio::export(df,"filename.ext")` and set `.ext` to `.csv` or `.xlsx`, then reload the file using `df <- rio::import("filename.ext")`].


```r
library(rio)
df <- rio::import("https://github.com/FredHasselman/Mixedmodels/raw/master/Cologne/AssignmentData/IntroductionData.sav")

# Change some variables to nominal factors
df$student.f <- factor(df$student)
df$school.f  <- factor(df$school, levels = 1:4, labels = paste("school",1:4))
df$style.f   <- factor(df$style, levels = c(-.5,.5), labels = c("informal style","formal style"))

# To mimic the slides, we analyse relative to school 4
df$school.f <- relevel(df$school.f, ref = "school 4")
```

### First, look at the data. 

    + Plot language ability `x` (at the start of the year) against `y` (at the end of the year) and mark each school with a shape or colour. 
    + If you are new to `R`, just look at the solution and copy and run the code. Plot the predicted values.

2. Using the basic `R` stats functions we can fit a regression model for the entire sample: $Y_{i} = \beta_{0} + \beta_{1} X_{i} + \varepsilon_{i},\ \text{with}\ i = 1,2,\ldots,20$.
    + Use the function `lm`. 
    + If you are unfamiliar with the function, look at the manual entry by typing `?lm` in the console.
    + Plot the predicted values.

### Now, fit the model with an intercept for each school. This means adding the `school.f` variable. The use of factors in `R` is very convenient, you do not have to create dummy variables, `R` will do this for you. So we are looking for a model of the type: $Y_{i} = \beta_{0j} + \beta_{1} X_{ij} + \varepsilon_{ij},\ \text{with}\ i = 1,2,\ldots,20\ \text{and}\ j = 1,2,\ldots,4$. 
    + Plot the predicted values.

### To fit the model with an intercept for each teaching style, we simply add the factor `style.f` variable. So we are looking for a model of the type: $Y_{ij} = \beta_{0k} + \beta_{1} X_{ik} + \varepsilon_{ik},\ \text{with}\ i = 1,2,\ldots,20\ \text{and}\ k = 1,2$. 
    + On the slides, the model was fitted to the grand-mean centered version of `x`. We will get back to centering data, the variable is in the dataset as `xc`. You could also calculate the variable yoursef using the function `scale`:

```r
df$xc <- scale(df$x, scale = FALSE)
```
    + Plot the predicted values.

5. To fit the model with both `school` and `teaching style` effects and just 1 random error term, we need dummy variables. They are already in the dataset. We should get a model of the type: $Y_{i} = \beta_{0} + \beta_{1} X_{i} + \beta_{2}\ Style_{i} + \beta_{3}\ d1_{i} + \beta_{4}\ d2_{i} +  \varepsilon_{i},\ \text{with}\ i = 1,2,\ldots,20$.
    + Plot the predicted values.

6. Compare the models using the function `anova()`.


## Solution {-}

### Plot the data

```r
# Using Lattice
library(lattice)
xyplot(y ~ x, groups = school.f, data = df, auto.key = TRUE)
```

![](ML_intro_files/figure-html/q1-1.png)<!-- -->

```r

# Using ggplot2
library(ggplot2)
ggplot(df, aes(x=x,y=y,group=school.f)) +
  geom_point(aes(colour=style.f,shape=school.f), size=5) +
  theme_bw() 
```

![](ML_intro_files/figure-html/q1-2.png)<!-- -->

### Fit the global model.

```r
# Fit the model and display a summary of the results
fit1 <- lm(y ~ x, data=df)
summary(fit1)
```

```

Call:
lm(formula = y ~ x, data = df)

Residuals:
    Min      1Q  Median      3Q     Max 
-9.1517 -2.8425 -0.6517  3.6893  6.8483 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   1.3887     2.9016   0.479    0.638    
x             4.1272     0.5254   7.855 3.17e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 4.371 on 18 degrees of freedom
Multiple R-squared:  0.7742,	Adjusted R-squared:  0.7616 
F-statistic: 61.71 on 1 and 18 DF,  p-value: 3.175e-07
```

```r

# Plot fit 1
df$pred1 <- predict(fit1)
ggplot(df, aes(x=x,y=pred1, colour = school.f, shape=school.f)) +
  geom_point(aes(y=y)) +
  geom_line(aes(group=school.f)) +
  ggtitle("Model 1", subtitle = fit1$call) +
  theme_bw()
```

![](ML_intro_files/figure-html/q2-1.png)<!-- -->

### Fit the model with intercepts for schools.

```r
# Fit the model and display a summary of the results
fit2 <- lm(y ~ x + school.f, data=df)
summary(fit2)
```

```

Call:
lm(formula = y ~ x + school.f, data = df)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.0000 -0.5531  0.4750  1.1438  1.7625 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)       16.7375     2.1667   7.725 1.32e-06 ***
x                  2.4375     0.2911   8.373 4.89e-07 ***
school.fschool 1 -13.5750     1.3225 -10.265 3.54e-08 ***
school.fschool 2  -8.3125     1.4368  -5.786 3.59e-05 ***
school.fschool 3  -4.3625     1.0814  -4.034  0.00108 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.647 on 15 degrees of freedom
Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9662 
F-statistic: 136.6 on 4 and 15 DF,  p-value: 1.317e-11
```

```r

# Plot fit 2
df$pred2 <- predict(fit2)
ggplot(df, aes(x=x,y=pred2, colour = school.f, shape=school.f)) +
  geom_point(aes(y=y)) +
  geom_line(aes(group=school.f)) +
  ggtitle("Model 2", subtitle = fit2$call) +
  theme_bw()
```

![](ML_intro_files/figure-html/q3-1.png)<!-- -->

### Fit the model with intercepts for teaching style.

```r
# Fit the model and display a summary of the results
fit3 <- lm(y ~ xc + style.f, data=df)
summary(fit3)
```

```

Call:
lm(formula = y ~ xc + style.f, data = df)

Residuals:
    Min      1Q  Median      3Q     Max 
-6.8561 -2.1189  0.4936  2.2097  6.2686 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)          20.6062     1.1975  17.208 3.44e-12 ***
xc                    4.0623     0.4556   8.917 8.08e-08 ***
style.fformal style   4.4875     1.6948   2.648   0.0169 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 3.784 on 17 degrees of freedom
Multiple R-squared:  0.8401,	Adjusted R-squared:  0.8213 
F-statistic: 44.66 on 2 and 17 DF,  p-value: 1.707e-07
```

```r

# Plot fit 3
df$pred3 <- predict(fit3)
ggplot(df, aes(x=xc,y=pred3,colour=style.f,shape=school.f)) +
  geom_point(aes(y=y)) +
  geom_line(aes()) +
  ggtitle("Model 3", subtitle = fit3$call) +
  theme_bw()
```

![](ML_intro_files/figure-html/q4-1.png)<!-- -->

### Fit the model with an interaction between school and teaching style.

```r
# Fit the model and display a summary of the results
fit4 <- lm(y ~ xc + style.f + d1 + d2, data=df)
summary(fit4)
```

```

Call:
lm(formula = y ~ xc + style.f + d1 + d2, data = df)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.0000 -0.5531  0.4750  1.1438  1.7625 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)          20.4438     0.5215  39.198  < 2e-16 ***
xc                    2.4375     0.2911   8.373 4.89e-07 ***
style.fformal style   4.8125     0.7387   6.515 9.78e-06 ***
d1                    4.6063     0.5829   7.902 1.00e-06 ***
d2                    4.1562     0.7184   5.786 3.59e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.647 on 15 degrees of freedom
Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9662 
F-statistic: 136.6 on 4 and 15 DF,  p-value: 1.317e-11
```

```r

# Plot fit 4
df$pred4 <- predict(fit4)
ggplot(df, aes(x=xc,y=pred4,shape=style.f,colour=school.f)) +
  geom_point(aes(y=y)) +
  geom_line() +
  ggtitle("Model 4", subtitle = fit4$call) +
  theme_bw()
```

![](ML_intro_files/figure-html/q5-1.png)<!-- -->

### Compare model fit

```r
# Compare the models
anova(fit1,fit2, fit3, fit4)
```

```
Analysis of Variance Table

Model 1: y ~ x
Model 2: y ~ x + school.f
Model 3: y ~ xc + style.f
Model 4: y ~ xc + style.f + d1 + d2
  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1     18 343.83                                  
2     15  40.68  3    303.16 37.266 3.421e-07 ***
3     17 243.43 -2   -202.76 37.386 1.486e-06 ***
4     15  40.67  2    202.76 37.386 1.486e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


# Variance Components  {.tabset .tabset-fade .tabset-pills}  

OLS regression versus the variance components model.

## Assignment 


### The empty multilevel model

* Fit a model that considers the variation in the intercepts of the schools as a random variation around the grand mean. 
     + Schools are Level 2: We assume the school intercepts can be considered to be drawn from a normal distribution with $\mu = 0$ and unknown $\sigma_{u}^2$. Parameter $\sigma_{u}^2$ will be estimated from the data. 
     + Student scores will be considered to vary randomly around the mean of their respective schools. We will assume students in each school vary around the school mean in the same way, a normal distribution with $\mu = 0$ and unknown $\sigma_{e}^2$.
     + Use the function `lmer()` to model the data. It looks similar to `lm()` but you need to specify the random effect structure. Look at the manual, or if you can't get it to work, at the solutions.
     + Add teaching style as a factor.
     + Compare the the single level model to the variance components model


## Solution

 Variance components

```r
library(lme4)
library(lmerTest)

fit5 <- lmer(y ~ xc + (1|school.f/student.f) ,data = df)
summary(fit5)
```

```
Linear mixed model fit by REML 
t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
Formula: y ~ xc + (1 | school.f/student.f)
   Data: df

REML criterion at convergence: 87.7

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-1.1903 -0.4498  0.1506  0.4296  0.8199 

Random effects:
 Groups             Name        Variance Std.Dev.
 student.f:school.f (Intercept)  1.534   1.239   
 school.f           (Intercept) 31.524   5.615   
 Residual                        1.264   1.124   
Number of obs: 20, groups:  student.f:school.f, 19; school.f, 4

Fixed effects:
            Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)  22.8265     2.8332  2.9080   8.057  0.00447 ** 
xc            2.5433     0.2947 14.8480   8.629 3.62e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
   (Intr)
xc -0.005
```


```r
# Add teaching style
fit6 <- lmer(y ~ xc + style.f + (1|school.f/student.f) ,data = df)
summary(fit6)
```

```
Linear mixed model fit by REML 
t-tests use  Satterthwaite approximations to degrees of freedom ['lmerMod']
Formula: y ~ xc + style.f + (1 | school.f/student.f)
   Data: df

REML criterion at convergence: 81.7

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-1.2021 -0.4341  0.1621  0.4484  0.8050 

Random effects:
 Groups             Name        Variance Std.Dev.
 student.f:school.f (Intercept)  1.482   1.217   
 school.f           (Intercept) 36.561   6.047   
 Residual                        1.309   1.144   
Number of obs: 20, groups:  student.f:school.f, 19; school.f, 4

Fixed effects:
                    Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)          20.4529     4.3082  1.9360   4.747   0.0443 *  
xc                    2.5294     0.2948 14.7440   8.580 4.09e-07 ***
style.fformal style   4.7521     6.0950  1.9390   0.780   0.5194    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Correlation of Fixed Effects:
            (Intr) xc    
xc           0.007       
styl.ffrmls -0.707 -0.014
```




## Random intercepts {.tabset .tabset-fade .tabset-pills}  

Random intercepts at for schools.

### Assignment {-}



### Solution {-}


```r
# Use 

```



## Random intercepts & slopes {.tabset .tabset-fade .tabset-pills}  

Random intercepts & slopes

### Assignment {-}


### Code {-}


```r
# Use 

```

### Solution


## Cross-level interactions {.tabset .tabset-fade .tabset-pills}  

Random intercepts & slopes

### Assignment {-}


### Code {-}


```r
# Use 

```

### Solution {-}




# **Best practices**

## Centering predictors {.tabset .tabset-fade .tabset-pills}  

To center... or not?

### Assignment {-}


### Code {-}


```r
# Use 

```

### Solution {-}


## Modeling strategies {.tabset .tabset-fade .tabset-pills}  

Start small ... or start full?

### Assignment {-}


### Code {-}


```r
# Use 

```

### Solution {-}




## Testing random effects {.tabset .tabset-fade .tabset-pills}  

Start small ... or start full?

### Assignment {-}


### Code {-}


```r
# Use 

```

### Solution {-}




# **Multilevel model for change**

## Time as a predictor {.tabset .tabset-fade .tabset-pills}  

Random intercepts & slopes

### Assignment {-}


### Code {-} 


```r
# Use 

```

### Solution {-}


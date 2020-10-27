
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R package `ers`

[![](https://img.shields.io/badge/devel%20version-1.0.0-blue.svg)](https://github.com/umich-biostatistics/ers)
[![](https://img.shields.io/github/languages/code-size/umich-biostatistics/ers.svg)](https://github.com/umich-biostatistics/ers)

## Overview

Estimate environmental risk scores from a number of potentially
correlated risk factors using adaptive elastic net (AENET) regression.
ENET regression does variable selection and can select multiple non-zero
collinear variables without overfitting. AENET is an adaptive version of
elastic net (ENET) that satisfies the asymptotic normality assumption
that allows us to conduct statistical inference any hypothesis testing
by providing large sample standard errors and p-values. Adaptive weights
shink smaller coefficients to zero, whereas larger coefficients are
penalized less. The technical details of the model implemented here are
described in “Associations of cumulative exposure to heavy metal
mixtures with obesity and its comorbidities among U.S. adults in NHANES
2003–2014” (2018) In press at Environment International
<doi:10.1016/j.envint.2018.09.035>.

## Installation

You should have `R` at least version 3.5.0 installed before trying to
install this package.

If the devtools package is not yet installed, install it first:

``` r
install.packages('devtools')
```

Then run:

``` r
# install AEenrich from Github:
devtools::install_github('umich-biostatistics/ers') 
library(ers)
```

## Example usage

For documentation pages:

``` r
?ers
```

### Quick example:

``` r
names(metal)
#>  [1] "log_LBXBPB"  "log_LBXBCD"  "log_LBXTHG"  "log_URXUAS"  "log_URXUAB" 
#>  [6] "log_URXUDMA" "log_URXUMMA" "log_URXUBA"  "log_URXUCD"  "log_URXUCO" 
#> [11] "log_URXUCS"  "log_URXUMO"  "log_URXUHG"  "log_URXUPB"  "log_URXUSB" 
#> [16] "log_URXUTL"  "log_URXUTU"  "log_URXUUR"
head(metal)
#>    log_LBXBPB  log_LBXBCD log_LBXTHG log_URXUAS log_URXUAB log_URXUDMA
#> 1  0.27875360 -0.39794001  0.6232493  1.4742163  1.3765770  0.30103000
#> 2 -0.22184875  0.00000000 -0.8538720 -0.3979400 -0.5228787  0.07918125
#> 3 -0.09691001 -0.09691001 -0.3979400  0.6720979  0.0000000  0.47712125
#> 4 -0.04575749 -0.39794001  0.8388491  0.9444827  0.7923917  0.47712125
#> 5  0.30103000 -0.52287874  0.2787536  1.0334238 -0.2218488  0.69897000
#> 6  0.68124124 -0.52287874 -0.1549020  0.2552725 -0.5228787  0.07918125
#>   log_URXUMMA  log_URXUBA log_URXUCD  log_URXUCO log_URXUCS log_URXUMO
#> 1  -0.2218488  0.38738983 -0.6989700 -0.53760200 0.59988307   1.813581
#> 2  -0.2218488  0.39967372 -0.6020600 -0.34678749 0.06069784   1.527630
#> 3  -0.2218488 -0.07058107 -0.4814861 -0.05551733 0.68663627   1.593064
#> 4  -0.2218488  0.02118930 -1.2218487 -0.23657201 0.28330123   1.164353
#> 5   0.0000000  0.44870632 -0.2676062 -0.49485002 0.79448805   1.884229
#> 6  -0.2218488 -0.65757732 -1.0969100 -0.65757732 0.26481782   1.363612
#>    log_URXUHG  log_URXUPB log_URXUSB log_URXUTL log_URXUTU log_URXUUR
#> 1 -0.14874165 -0.23657201  -1.301030 -0.8538720 -1.0000000  -2.397940
#> 2 -1.00000000 -0.63827216  -1.301030 -1.3979400 -1.6989700  -2.397940
#> 3 -1.00000000 -0.38721614  -1.221849 -1.0000000 -1.3979400  -2.397940
#> 4 -0.48148606 -0.63827216  -1.301030 -1.2218487 -1.0969100  -2.397940
#> 5 -0.04095861  0.08990511  -1.301030 -0.5086383 -0.8860566  -1.920819
#> 6 -1.00000000 -0.33724217  -1.301030 -1.0457575 -1.6989700  -2.397940
names(Y)
#> NULL
head(Y)
#>      log_BMXWAIST
#> [1,]     2.033424
#> [2,]     1.879096
#> [3,]     1.992995
#> [4,]     1.845098
#> [5,]     2.031004
#> [6,]     2.032619
names(covs)
#>  [1] "PERMTH_INT"  "PERMTH_EXM"  "URXUHG"      "URDUHGLC"    "smk"        
#>  [6] "pa"          "CVD"         "sbp"         "dbp"         "HTN"        
#> [11] "dm"          "stroke"      "all_death"   "cvd_death"   "dm_death"   
#> [16] "cycle03"     "cycle05"     "male"        "mexican"     "otherhis"   
#> [21] "white"       "black"       "otherrace"   "educ1"       "educ2"      
#> [26] "educ3"       "educ4"       "educ5"       "never_smk"   "former_smk" 
#> [31] "current_smk" "non_pa"      "mod_pa"      "vig_pa"
head(covs)
#> # A tibble: 6 x 34
#>   PERMTH_INT PERMTH_EXM URXUHG URDUHGLC   smk    pa   CVD   sbp   dbp   HTN
#>        <dbl>      <dbl>  <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1         88         88   0.71        0     1     0     0  127.  78.7     0
#> 2        103        102   0.1         1     1     2     0  111.  73.3     0
#> 3         85         84   0.1         1     0     0     0  200   93.3     1
#> 4         86         84   0.33        0     0     2     0  128   73.3     0
#> 5         88         88   0.91        0     0     1     0  128   61.3     0
#> 6         95         94   0.1         1     1     0     0  168   93.3     1
#> # ... with 24 more variables: dm <dbl>, stroke <dbl>, all_death <dbl>,
#> #   cvd_death <dbl>, dm_death <dbl>, cycle03 <dbl>, cycle05 <dbl>, male <dbl>,
#> #   mexican <dbl>, otherhis <dbl>, white <dbl>, black <dbl>, otherrace <dbl>,
#> #   educ1 <dbl>, educ2 <dbl>, educ3 <dbl>, educ4 <dbl>, educ5 <dbl>,
#> #   never_smk <dbl>, former_smk <dbl>, current_smk <dbl>, non_pa <dbl>,
#> #   mod_pa <dbl>, vig_pa <dbl>
```

``` r
set.seed(7794)
fit = ers(x = metal, y = as.numeric(Y), covar = covs,
          control = list(lambda2.start = seq(0.001, 0.5, by = 0.01),
                         lambda2.adapt = seq(0.001, 0.5, by = 0.01)))
```

### Current Suggested Citation

Wang, Xin, Bhramar Mukherjee, and Sung Kyun Park. “Associations of
cumulative exposure to heavy metal mixtures with obesity and its
comorbidities among US adults in NHANES 2003–2014.” Environment
international 121 (2018): 683-694.
[doi:10.1016/j.envint.2018.09.035](https://doi.org/10.1016/j.envint.2018.09.035).

Famale Labor Participation in STEM Areas
================
2024-05-27

This repository provides an analysis of female labor participation in
STEM areas for the New Dimensions program.

New Dimensions is a comprehensive business intelligence program for
women from disadvantaged backgrounds in Ecuador. Our goal is to open
pathways for Ecuadorian women in STEM through data education and
employability skills. This analysis provides context and background for
the need for the program.

The inaugural cohort included 55 women and received \$25,000 in funding
from the US Embassy in Ecuador in 2023. We are now seeking support for
the upcoming second cohort scheduled to launch in 2025. Our goal is to
secure funding for the next four cohorts (i.e. 2025 through 2028).

Elaborated by: Alonso Quijano [X](https://x.com/aquijanoruiz),
[Github](https://github.com/aquijanoruiz)  
Source: Ecuador 2023 National Labor Survey
[ENEMDU](https://www.ecuadorencifras.gob.ec/enemdu-anual/)

## Data preprocessing

``` r
# -------------------------------------------------- #
# (1) Loading the data
# -------------------------------------------------- #

# Load libraries
library(haven)
library(labelled)
library(tidyverse)
library(survey)

# Load data
# Set temporary directory
td <- tempdir()

# Unzip the data file
unzip(zipfile = "ENEMDU_ANUAL_2023.zip", exdir = td)

# Read the SPSS file
enemdu2023 <- read_spss(file.path(td, "BDDenemdu_personas_2023_anual.sav"))
enemdu2023_labels <- var_label(enemdu2023)
```

``` r
# -------------------------------------------------- #
# (2) Preprocessing and Data Manipulation
# -------------------------------------------------- #

# Define economically active population (PEA)
enemdu2023 <- enemdu2023 %>% mutate(pea = ifelse(between(condact, 1, 8), 1, 0))

# Define adequate employment (people earning the minimum salary or higher)
enemdu2023 <- enemdu2023 %>% mutate(emp_adeq = ifelse(condact == 1, 1, 0))

# Define IESS contribution (1 if contributes to IESS, 0 otherwise)
enemdu2023 <- enemdu2023 %>% 
  mutate(iess = ifelse(between(p05a, 1, 4) | between(p05b, 1, 4), 1, 0))

# Define STEM field (1 if in STEM, 0 otherwise)
# We exclude people in health sciences to avoid overrepresentation of women in nursing and midwifery
enemdu2023$stem <- ifelse(substr(enemdu2023$p41, 1,2) %in% c(21, 25), 1, 0)

# Remove non responses from income variable
enemdu2023 <-  enemdu2023 %>% mutate(
  income = case_when(ingrl == -1 ~ 0, ingrl == 999999 ~ NA, TRUE ~ ingrl))
```

## Analysis w/o survey design

``` r
# -------------------------------------------------- #
# (3) Employment Classification
# -------------------------------------------------- #

# Men
enemdu2023 %>% filter(p02 == 1, pea == 1) %>% 
  group_by(condact) %>% summarize(n = n()) %>% 
  mutate(prob = prop.table(n))
```

    ## # A tibble: 8 × 3
    ##   condact                                                  n    prob
    ##   <dbl+lbl>                                            <int>   <dbl>
    ## 1 1 [Empleo Adecuado/Pleno]                            44099 0.454  
    ## 2 2 [Subempleo por insuficiencia de tiempo de trabajo] 16759 0.173  
    ## 3 3 [Subempleo por insuficiencia de ingresos]           2114 0.0218 
    ## 4 4 [Otro empleo no pleno]                             24941 0.257  
    ## 5 5 [Empleo no remunerado]                              4871 0.0502 
    ## 6 6 [Empleo no clasificado]                              299 0.00308
    ## 7 7 [Desempleo abierto]                                 3566 0.0367 
    ## 8 8 [Desempleo oculto]                                   409 0.00421

``` r
# Women
enemdu2023 %>% filter(p02 == 2, pea == 1) %>% 
  count(condact) %>% mutate(prob = prop.table(n))
```

    ## # A tibble: 8 × 3
    ##   condact                                                  n    prob
    ##   <dbl+lbl>                                            <int>   <dbl>
    ## 1 1 [Empleo Adecuado/Pleno]                            26528 0.344  
    ## 2 2 [Subempleo por insuficiencia de tiempo de trabajo] 12702 0.165  
    ## 3 3 [Subempleo por insuficiencia de ingresos]            964 0.0125 
    ## 4 4 [Otro empleo no pleno]                             21921 0.285  
    ## 5 5 [Empleo no remunerado]                             10516 0.137  
    ## 6 6 [Empleo no clasificado]                              216 0.00280
    ## 7 7 [Desempleo abierto]                                 3696 0.0480 
    ## 8 8 [Desempleo oculto]                                   495 0.00643

``` r
# Indigenous Women
enemdu2023 %>% filter(p02 == 2, pea == 1, p15 == 1) %>% 
  count(condact) %>% mutate(prob = prop.table(n))
```

    ## # A tibble: 8 × 3
    ##   condact                                                  n     prob
    ##   <dbl+lbl>                                            <int>    <dbl>
    ## 1 1 [Empleo Adecuado/Pleno]                              820 0.119   
    ## 2 2 [Subempleo por insuficiencia de tiempo de trabajo]   993 0.144   
    ## 3 3 [Subempleo por insuficiencia de ingresos]             88 0.0127  
    ## 4 4 [Otro empleo no pleno]                              2115 0.306   
    ## 5 5 [Empleo no remunerado]                              2746 0.397   
    ## 6 6 [Empleo no clasificado]                                4 0.000579
    ## 7 7 [Desempleo abierto]                                  123 0.0178  
    ## 8 8 [Desempleo oculto]                                    24 0.00347

``` r
# -------------------------------------------------- #
# (4) Occupation Type
# -------------------------------------------------- #

# Men
enemdu2023 %>% filter(p02 == 1, empleo == 1) %>% 
  count(grupo1) %>% mutate(prob = prop.table(n))
```

    ## # A tibble: 10 × 3
    ##    grupo1                                                        n    prob
    ##    <dbl+lbl>                                                 <int>   <dbl>
    ##  1  1 [Personal direct./admin. pública y empresas]            1416 0.0152 
    ##  2  2 [Profesionales científicos e intelectuales]             7635 0.0820 
    ##  3  3 [Técnicos y profesionales de nivel medio]               5592 0.0601 
    ##  4  4 [Empleados de oficina]                                  2631 0.0283 
    ##  5  5 [Trabajad. de los servicios y comerciantes]            14531 0.156  
    ##  6  6 [Trabajad. calificados agropecuarios y pesqueros]      18926 0.203  
    ##  7  7 [Oficiales operarios y artesanos]                      14435 0.155  
    ##  8  8 [Operadores de instalac. máquinas y montad.]           10250 0.110  
    ##  9  9 [Trabajadores no calificados, ocupaciones elementales] 17278 0.186  
    ## 10 10 [Fuerzas Armadas]                                        389 0.00418

``` r
# Women
enemdu2023 %>% filter(p02 == 2, empleo == 1) %>%
  count(grupo1) %>% mutate(prob = prop.table(n))
```

    ## # A tibble: 10 × 3
    ##    grupo1                                                        n     prob
    ##    <dbl+lbl>                                                 <int>    <dbl>
    ##  1  1 [Personal direct./admin. pública y empresas]            1054 0.0145  
    ##  2  2 [Profesionales científicos e intelectuales]             9521 0.131   
    ##  3  3 [Técnicos y profesionales de nivel medio]               4792 0.0658  
    ##  4  4 [Empleados de oficina]                                  3042 0.0418  
    ##  5  5 [Trabajad. de los servicios y comerciantes]            22078 0.303   
    ##  6  6 [Trabajad. calificados agropecuarios y pesqueros]       7679 0.105   
    ##  7  7 [Oficiales operarios y artesanos]                       4283 0.0588  
    ##  8  8 [Operadores de instalac. máquinas y montad.]             715 0.00982 
    ##  9  9 [Trabajadores no calificados, ocupaciones elementales] 19671 0.270   
    ## 10 10 [Fuerzas Armadas]                                         12 0.000165

``` r
# Indigenous women
enemdu2023 %>% filter(p02 == 2, empleo == 1, p15 == 1) %>%
  count(grupo1) %>% mutate(prob = prop.table(n))
```

    ## # A tibble: 9 × 3
    ##   grupo1                                                       n    prob
    ##   <dbl+lbl>                                                <int>   <dbl>
    ## 1 1 [Personal direct./admin. pública y empresas]              11 0.00163
    ## 2 2 [Profesionales científicos e intelectuales]              215 0.0318 
    ## 3 3 [Técnicos y profesionales de nivel medio]                 82 0.0121 
    ## 4 4 [Empleados de oficina]                                    51 0.00754
    ## 5 5 [Trabajad. de los servicios y comerciantes]              934 0.138  
    ## 6 6 [Trabajad. calificados agropecuarios y pesqueros]       1623 0.240  
    ## 7 7 [Oficiales operarios y artesanos]                        242 0.0358 
    ## 8 8 [Operadores de instalac. máquinas y montad.]              21 0.00310
    ## 9 9 [Trabajadores no calificados, ocupaciones elementales]  3587 0.530

``` r
# -------------------------------------------------- #
# (5) Income
# -------------------------------------------------- #

# Average income by gender (STEM only)
enemdu2023 %>% filter(stem == 1) %>% group_by(as_factor(p02)) %>% 
  summarize(avg_income = mean(income, na.rm = TRUE))
```

    ## # A tibble: 2 × 2
    ##   `as_factor(p02)` avg_income
    ##   <fct>                 <dbl>
    ## 1 " Hombre"             1174.
    ## 2 " Mujer"               958.

``` r
# Median income by gender (STEM only)
enemdu2023 %>% filter(stem == 1) %>% group_by(as_factor(p02)) %>% 
  summarize(avg_income = median(income, na.rm = TRUE))
```

    ## # A tibble: 2 × 2
    ##   `as_factor(p02)` avg_income
    ##   <fct>                 <dbl>
    ## 1 " Hombre"              1000
    ## 2 " Mujer"                850

## Analysis w/ survey design

``` r
# -------------------------------------------------- #
# (6) Survey Design
# -------------------------------------------------- #

# Create survey design object
enemdu2023_svydesign <- svydesign(data = enemdu2023,
                                  ids = ~ upm, # Primary Sampling Unit
                                  strata = ~ estrato, # Strata
                                  weights = ~ fexp, # Weights
                                  nest = TRUE) # Multistage Sampling

# -------------------------------------------------- #
# (7) Analysis with Survey Design
# -------------------------------------------------- #

# Employment classification of economically active women
svymean(x = ~ as_factor(condact),
        design = subset(enemdu2023_svydesign, pea == 1 & p02 == 2), 
        na.rm = TRUE)
```

    ##                                                                         mean
    ## as_factor(condact)Menores de 15 años                               0.0000000
    ## as_factor(condact)Empleo Adecuado/Pleno                            0.2908540
    ## as_factor(condact)Subempleo por insuficiencia de tiempo de trabajo 0.1653917
    ## as_factor(condact)Subempleo por insuficiencia de ingresos          0.0106955
    ## as_factor(condact)Otro empleo no pleno                             0.3218140
    ## as_factor(condact)Empleo no remunerado                             0.1615230
    ## as_factor(condact)Empleo no clasificado                            0.0023804
    ## as_factor(condact)Desempleo abierto                                0.0427669
    ## as_factor(condact)Desempleo oculto                                 0.0045745
    ## as_factor(condact)Población Económicamente Inactiva                0.0000000
    ##                                                                        SE
    ## as_factor(condact)Menores de 15 años                               0.0000
    ## as_factor(condact)Empleo Adecuado/Pleno                            0.0057
    ## as_factor(condact)Subempleo por insuficiencia de tiempo de trabajo 0.0037
    ## as_factor(condact)Subempleo por insuficiencia de ingresos          0.0008
    ## as_factor(condact)Otro empleo no pleno                             0.0046
    ## as_factor(condact)Empleo no remunerado                             0.0081
    ## as_factor(condact)Empleo no clasificado                            0.0003
    ## as_factor(condact)Desempleo abierto                                0.0015
    ## as_factor(condact)Desempleo oculto                                 0.0004
    ## as_factor(condact)Población Económicamente Inactiva                0.0000

``` r
# Percentage of women who contribute to IESS
svymean(x = ~ iess,
        design = subset(enemdu2023_svydesign, pea == 1 & p02 == 2), 
        na.rm = TRUE)
```

    ##         mean     SE
    ## iess 0.32627 0.0062

``` r
# Gender classification of individuals in STEM fields
svymean(x = ~ as_factor(p02),
        design = subset(enemdu2023_svydesign, stem == 1), 
        na.rm = TRUE)
```

    ##                          mean     SE
    ## as_factor(p02) Hombre 0.81378 0.0132
    ## as_factor(p02) Mujer  0.18622 0.0132

``` r
# Educational attainment of adult women
svymean(x = ~ as_factor(nnivins),
        design = subset(enemdu2023_svydesign, pea == 1 & p02 == 2 & p03 >= 18), 
        na.rm = TRUE)
```

    ##                                                     mean     SE
    ## as_factor(nnivins)Ninguno                      0.0459698 0.0046
    ## as_factor(nnivins)Centro de Alfabetización     0.0041008 0.0007
    ## as_factor(nnivins)Educacion Básica             0.3729145 0.0065
    ## as_factor(nnivins)Educacion Media/Bachillerato 0.3132415 0.0047
    ## as_factor(nnivins)Superior                     0.2637734 0.0059

``` r
# Average income by gender
svyby(~ income, ~ as_factor(p02), 
      design = enemdu2023_svydesign, 
      svymean, na.rm = TRUE)
```

    ##         as_factor(p02)   income       se
    ##  Hombre         Hombre 495.6362 5.071176
    ##  Mujer           Mujer 419.7013 5.069835

``` r
(419.7013 - 495.6362) / 495.6362
```

    ## [1] -0.1532069

``` r
# Average income by gender (STEM only)
svyby(~ income, ~ as_factor(p02), 
      design = subset(enemdu2023_svydesign, stem == 1), 
      svymean, na.rm = TRUE)
```

    ##         as_factor(p02)    income       se
    ##  Hombre         Hombre 1206.6674 29.14844
    ##  Mujer           Mujer  970.5498 45.50951

``` r
(970.5498 - 1206.6674) / 1206.6674
```

    ## [1] -0.1956775

``` r
# Average income by gender (non-STEM only)
svyby(~ income, ~ as_factor(p02), 
      design = subset(enemdu2023_svydesign, stem == 0), 
      svymean, na.rm = TRUE)
```

    ##         as_factor(p02)   income       se
    ##  Hombre         Hombre 479.6023 4.801907
    ##  Mujer           Mujer 415.4917 4.995044

``` r
(415.4917 - 479.6023) / 479.6023
```

    ## [1] -0.1336745

``` r
# Median income of women (STEM only)
women_income <- enemdu2023 %>% filter(p02 == 2, stem == 1) %>% select(income) %>% pull()
women_weights <- enemdu2023 %>% filter(p02 == 2, stem == 1) %>% select(fexp) %>% pull()
median(rep(women_income, times=women_weights), na.rm = TRUE)
```

    ## [1] 820

``` r
# Median income of women (STEM only)
men_income <- enemdu2023 %>% filter(p02 == 1, stem == 1) %>% select(income) %>% pull()
men_weights <- enemdu2023 %>% filter(p02 == 1, stem == 1) %>% select(fexp) %>% pull()
median(rep(men_income, times=men_weights), na.rm = TRUE)
```

    ## [1] 1070

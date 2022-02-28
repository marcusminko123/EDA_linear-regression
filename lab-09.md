Lab 09 - Grading the professor, Pt. 1
================
Marcus Minko
02-28-2022

### Load packages and data

``` r
library(tidyverse) 
library(broom)
library(openintro)
```

### View Data Distribrution

``` r
evals %>% 
  ggplot(aes(x = score)) + geom_histogram (bindwidth = .25)
```

    ## Warning: Ignoring unknown parameters: bindwidth

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lab-09_files/figure-gfm/score-distribution-1.png)<!-- -->

``` r
evals %>% 
  summarize (mean(score), median(score), min(score), max(score))
```

    ## # A tibble: 1 × 4
    ##   `mean(score)` `median(score)` `min(score)` `max(score)`
    ##           <dbl>           <dbl>        <dbl>        <dbl>
    ## 1          4.17             4.3          2.3            5

``` r
# score distribution is somewhat left-skewed; in general, students give pretty scores
```

Remove this text, and add your answer for Exercise 1 here. Add code
chunks as needed. Don’t forget to label your code chunk. Do not use
spaces in code chunk labels.

### Exercise 2

…

Add exercise headings as needed.

## 

For Exercise 12, relevel() function can be helpful!

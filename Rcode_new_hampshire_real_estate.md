Rcode_new_hampshire_dataset
================
2026-03-07

<h2>

Loading packages
</h2>

``` r
library('tidyverse')
```

    ## Warning: package 'tidyverse' was built under R version 4.5.2

    ## Warning: package 'tidyr' was built under R version 4.5.2

    ## Warning: package 'readr' was built under R version 4.5.2

    ## Warning: package 'purrr' was built under R version 4.5.2

    ## Warning: package 'dplyr' was built under R version 4.5.2

    ## Warning: package 'stringr' was built under R version 4.5.2

    ## Warning: package 'forcats' was built under R version 4.5.2

    ## Warning: package 'lubridate' was built under R version 4.5.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.0     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library('ggplot2')
library('patchwork')
```

    ## Warning: package 'patchwork' was built under R version 4.5.2

<h2>

Loading data
</h2>

``` r
df <- read.csv('new_hampshire_real_estate_2026.csv')
```

<h1>

Finding quartiles for data for cleaing
</h1>

<h2>

Converting columns to integers
</h2>

``` r
df$garage <- as.integer(df$garage)
df$stories <- as.integer(df$stories)
```

<h3>

Garage
</h3>

``` r
q3_garage <- quantile(df$garage, c(0.75),na.rm=TRUE)
q1_garage <- quantile(df$garage, c(0.25),na.rm=TRUE)
iqr_garage <- q3_garage - q1_garage
lower_range_garage <- q1_garage - 3 * (iqr_garage)
upper_range_garage <- q3_garage + 3 * (iqr_garage)
```

<h3>

stories
</h3>

``` r
q3_stories <- quantile(df$stories, c(0.75),na.rm=TRUE)
q1_stories <- quantile(df$stories, c(0.25),na.rm=TRUE)
iqr_stories <- q3_stories - q1_stories
lower_range_stories <- q1_stories - 1.5 * (iqr_stories)
upper_range_stories <- q3_stories + 1.5 * (iqr_stories)
```

<h3>

baths_full_calc
</h3>

``` r
q3_baths <- quantile(df$baths_full_calc, c(0.75),na.rm=TRUE)
q1_baths <- quantile(df$baths_full_calc, c(0.25),na.rm=TRUE)
iqr_baths <- q3_baths - q1_baths
lower_range_baths <- q1_baths - 3 * (iqr_baths)
upper_range_baths <- q3_baths + 3 * (iqr_baths)
```

<h3>

beds
</h3>

``` r
q3_beds <- quantile(df$beds, c(0.75),na.rm=TRUE)
q1_beds <- quantile(df$beds, c(0.25),na.rm=TRUE)
iqr_beds <- q3_beds - q1_beds
lower_range_beds <- q1_beds - 1.5 * (iqr_beds)
upper_range_beds <- q3_beds + 1.5 * (iqr_beds)
```

<h3>

sqft
</h3>

``` r
q3_sqft <- quantile(df$sqft, c(0.75),na.rm=TRUE)
q1_sqft <- quantile(df$sqft, c(0.25),na.rm=TRUE)
iqr_sqft <- q3_sqft - q1_sqft
lower_range_sqft <- q1_sqft - 1.5 * (iqr_sqft)
upper_range_sqft <- q3_sqft + 1.5 * (iqr_sqft)
```

<h3>

listprice
</h3>

``` r
q3_listprice <- quantile(df$listPrice, c(0.75),na.rm=TRUE)
q1_listprice <- quantile(df$listPrice, c(0.25),na.rm=TRUE)
iqr_listprice <- q3_listprice - q1_listprice
lower_range_listprice <- q1_listprice - 1.5 * (iqr_listprice)
upper_range_listprice <- q3_listprice + 1.5 * (iqr_listprice)
```

<h2>

All ranges for the variables
</h2>

``` r
print(paste('range for garage is ', lower_range_garage,'-',upper_range_garage))
```

    ## [1] "range for garage is  -2 - 5"

``` r
print(paste('range for stories is ', lower_range_stories,'-',upper_range_stories))
```

    ## [1] "range for stories is  -0.5 - 3.5"

``` r
print(paste('range for baths is ', lower_range_baths,'-',upper_range_baths))
```

    ## [1] "range for baths is  -2 - 5"

``` r
print(paste('range for beds is ', lower_range_beds,'-',upper_range_beds))
```

    ## [1] "range for beds is  -1 - 7"

``` r
print(paste('range for sqft is ', lower_range_sqft,'-',upper_range_sqft))
```

    ## [1] "range for sqft is  -667.375 - 4483.625"

``` r
print(paste('range for listprice is ', lower_range_listprice,'-',upper_range_listprice))
```

    ## [1] "range for listprice is  -447350 - 1412250"

<h1>

Creating graphs to find correlation between variables and listing price
</h1>

<br>

<h2>

Importing cleaned dataset
</h2>

``` r
df_clean <- read.csv('new_hampshire_real_estate_2026_cleaned.csv')
```

<h2>

creating graphs
</h2>

<br>

<h3>

Finding relationships with scatter plots
</h3>

``` r
plot1 <-ggplot(df_clean)+ geom_point(mapping=aes(x=listPrice, y=sqft)) + geom_smooth(mapping=aes(x=listPrice, y=sqft))
plot2 <- ggplot(df_clean, aes(x=listPrice, y=stories)) + geom_point() + geom_smooth()
plot3 <- ggplot(df_clean, aes(x=listPrice, y=beds)) + geom_point() + geom_smooth()
plot4 <- ggplot(df_clean, aes(x=listPrice, y=baths_full_calc)) + geom_point() + geom_smooth()
plot5 <- ggplot(df_clean, aes(x=listPrice, y=garage)) + geom_point() + geom_smooth()
(plot1 + plot2 + plot3) / (plot4 + plot5)
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](Rcode_new_hampshire_real_estate_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

<h3>

Finding correlation with box plots
</h3>

``` r
plot22 <-ggplot(df_clean)+ geom_boxplot(mapping=aes(x=factor(stories), y=listPrice))
plot33 <-ggplot(df_clean)+ geom_boxplot(mapping=aes(x=factor(garage), y=listPrice))
plot44 <-ggplot(df_clean)+ geom_boxplot(mapping=aes(x=factor(beds), y=listPrice))
plot55 <-ggplot(df_clean)+ geom_boxplot(mapping=aes(x=factor(baths_full_calc), y=listPrice))
(plot22 + plot33) / (plot44 + plot55)
```

![](Rcode_new_hampshire_real_estate_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
<h1>

Calculation Correlation
<h1>

``` r
cor1 <-cor(df_clean$listPrice,df_clean$sqft)
cor2 <-cor(df_clean$listPrice,df_clean$stories)
cor3 <-cor(df_clean$listPrice,df_clean$beds)
cor4 <-cor(df_clean$listPrice,df_clean$baths_full_calc)
cor5 <-cor(df_clean$listPrice,df_clean$garage)
print(paste('sqft: ',cor1, ' stories: ', cor2, ' beds: ', cor3, ' baths_full_calc: ', cor4, ' garages: ', cor5))
```

    ## [1] "sqft:  0.601811749162882  stories:  0.160966482111504  beds:  0.327584319344065  baths_full_calc:  0.473429429914525  garages:  0.427958153127197"

<h1>

seeing if house type correlates with listing price
</h1>

``` r
type_plot1 <- ggplot(df_clean, aes(x=type,y=listPrice))+
  geom_boxplot()
type_plot2 <- ggplot(df_clean, aes(x=type,y=listPrice))+
  stat_summary(fun = mean, geom = "bar")
type_plot1 + type_plot2
```

![](Rcode_new_hampshire_real_estate_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

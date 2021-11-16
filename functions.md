writing\_functions
================

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.463928266  0.854615262  1.398073667  0.784291163 -0.602246560
    ##  [6]  0.553962269 -2.247078584  0.831243552  0.791137349  0.738028594
    ## [11]  0.584756237  1.046063241 -0.562012485 -0.986597282  1.185859979
    ## [16]  0.066439211  0.519664208 -1.172088197 -1.126437540 -1.768667667
    ## [21] -1.351791549  0.087140562  0.617126062  0.009666644 -0.715076404

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
   
}

z_scores(x = x_vec)
```

    ##  [1]  0.463928266  0.854615262  1.398073667  0.784291163 -0.602246560
    ##  [6]  0.553962269 -2.247078584  0.831243552  0.791137349  0.738028594
    ## [11]  0.584756237  1.046063241 -0.562012485 -0.986597282  1.185859979
    ## [16]  0.066439211  0.519664208 -1.172088197 -1.126437540 -1.768667667
    ## [21] -1.351791549  0.087140562  0.617126062  0.009666644 -0.715076404

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  0.177281958  0.703542082 -0.570512756  0.131405933 -0.172280944
    ##  [6]  0.020999543 -0.236772913 -1.405923284  0.009126516  0.044984288
    ## [11] -0.145667870 -1.222171659  0.371270381 -0.135906733 -1.947797599
    ## [16]  1.531947755  1.194904413 -0.665549131 -0.988169902 -1.124127598
    ## [21] -0.063614543 -0.827065791  0.412970846 -1.260394104  2.067392245
    ## [26]  0.231256186 -0.615973703  0.091424641  0.284880732  0.716413134
    ## [31] -0.427612236  1.258287104  0.737110579 -0.709527912 -0.213064245
    ## [36]  3.502054729 -0.420415293 -0.824764205  0.623367905 -0.133308549

let’s try again

``` r
z_scores = function(x){
  
  if(!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if(length(x) <3){
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(c("my", "name", "is", "jeff")): x needs to be numeric

``` r
z_scores(mycars)
```

    ## Error in z_scores(mycars): 找不到对象'mycars'

``` r
z_scores(y_vec)
```

    ##  [1]  0.177281958  0.703542082 -0.570512756  0.131405933 -0.172280944
    ##  [6]  0.020999543 -0.236772913 -1.405923284  0.009126516  0.044984288
    ## [11] -0.145667870 -1.222171659  0.371270381 -0.135906733 -1.947797599
    ## [16]  1.531947755  1.194904413 -0.665549131 -0.988169902 -1.124127598
    ## [21] -0.063614543 -0.827065791  0.412970846 -1.260394104  2.067392245
    ## [26]  0.231256186 -0.615973703  0.091424641  0.284880732  0.716413134
    ## [31] -0.427612236  1.258287104  0.737110579 -0.709527912 -0.213064245
    ## [36]  3.502054729 -0.420415293 -0.824764205  0.623367905 -0.133308549

# Multiple outputs

``` r
mean_and_sd = function(x){
  
  if(!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if(length(x) < 3){
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
 
  return(output_df)
}

mean_and_sd(y_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.1 0.328

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.21  4.05

## different sample sizzes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
    
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.79  2.71

lets’ write a function that simulate data, compute the mean and sd

``` r
sim_mean_sd = function(n, mu = 3, sigma = 4){
  
  # do checks on inputs
  
  sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
}

sim_mean_sd(30, 40, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  39.6  3.51

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.33  3.42

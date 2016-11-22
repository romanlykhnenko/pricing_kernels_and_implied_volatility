
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **locLinBW** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : locLinBW

Published in : pricing_kernels_and_implied_volatility

Description : 'Selects bandwidth for multivariate local linear kernel regression using
cross-validation'

Keywords : 'kernel regression, bandwidth, cross-validation, multistarting, local linear,
least-squares, kernel, regression, multivariate, risk neutral density, numerical optimization'

Author : Roman Lykhnenko

Submitted : Roman Lykhnenko

Datafile : C_2012.csv

Input: 
- C_2012.csv: Call prices 2012

Output: 
- locLinBW.RData: 'Bandwidths used for estimation of risk neutral density based on local linear
kernel regression'

```


### R Code:
```r
# install and load packages
libraries = c("dplyr", "MASS", "gridExtra", "ggplot2", "Matrix", "parallel", "caTools", 
              "np", "RColorBrewer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# set working directory
setwd("/home/rama/Masterarbeit/pricing_kernels_and_implied_volatility")


bandwidthMonKfoldCVyears = list()

for (year in 2012){
  
  # specify maturity (data to use)
  label = "vdax2m"
  
  # path to the file with option data
  pathToData = paste("locLinBW/C_", as.character(year), label, ".csv", sep = "")
  
  # read option data
  C_2012 = read.csv(pathToData, sep = ",", header = TRUE)
  C_2012 = na.omit(C_2012)
  
  # add ttm in days
  C_2012$ttmDays = (C_2012$TTM) * 365
  
  # moneyness(Strike/S_0)
  C_2012$moneyness = C_2012$EXERCISE_PRICE/C_2012$DAX
  
  # option price
  C = C_2012[, "settlement"]
  
  # discount factor
  D = exp(-(C_2012$euriborRate) * C_2012$TTM)
  
  # scale by discount factor
  C = C/D
  
  # scale by forward price
  C = C/C_2012$DAX
  
  optionDataRegression = cbind(C, C_2012[, c("TTM", "moneyness", "VDAX")])
  optionDataCV0        = optionDataRegression[seq(1, nrow(optionDataRegression), 8), ]
  
  bwCVnp = npregbw(xdat     = optionDataCV0[, c("TTM", "moneyness", "VDAX")],
                   ydat     = optionDataCV0$C,
                   regtype  = "ll",
                   ftol     = 5, 
                   tol      = 5, 
                   bwmethod = "cv.ls")
  
  bandwidthMonKfoldCVyears[[as.character(year)]] = list("mon"   = bwCVnp$bw[2],
                                                        "vola"  = bwCVnp$bw[3],
                                                        "matur" = bwCVnp$bw[1])
  
}

# save as RData
save(bandwidthMonKfoldCVyears, 
     file = paste0("locLinBW/locLinBW", label , ".RData"))



```

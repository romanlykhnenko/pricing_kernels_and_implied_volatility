
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **epk3VolaIntervalsVDAX2m** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : epk3VolaIntervalsVDAX2m

Published in : pricing_kernels_and_implied_volatility

Description : 'Estimates and plots (yearly) empirical pricing kernels (EPK), risk neutral densities
(RND) and physical densities (PD) of DAX 30 index return conditional on time to maturity 2 months
and different levels of VDAX-NEW-Subindex 2 (20 equally spaced numbers from 5% to 95% quantile of
VDAX-NEW-Subindex 2 in a given year). Local linear kernel regression is used for estimation of the
conditional risk neutral density and local constant kernel regression is used for estimation of
conditional physical density. EPK is obtained as a ratio of RND and PD. The panels on the
right-hand side of figures depict EPK, RND, PD conditional on 20%, 50% and 80% quantiles of
VDAX-NEW-Subindex 2 with 95% confidence intervals. Colors from red to blue correspond to increasing
values of volatility within each interval. All results are shown on a continuously compounded
2-months period returns scale.'

Keywords : 'pricing kernel, risk neutral density, physical density, kernel regression, volatility,
dax, vdax, kernel, regression, risk, risk aversion'

See also : 'epk3VolaIntervalsVDAX, epk3VolaIntervalsVDAX1m, epk3VolaIntervalsVDAX3m, locLinBW,
termStructurePK'

Author : Roman Lykhnenko

Submitted : Roman Lykhnenko

Datafile : C_2012vdax2m.csv, timeSeriesDaxVdax2m.csv, locLinBWvdax2m.RData

Input: 
- timeSeriesDaxVdax2m.csv: Time series of VDAX-NEW-Subindex 2 and DAX 30 index
- C_2012vdax2m.csv: Call prices 2012
- locLinBWvdax2m.RData: 'Bandwidths used for estimation of RND based on local linear kernel
regression'

Output: 
- listRndPDpkVDAX2m2012.RData: 'Estimated conditional pricing kernels, risk neutral and physical
densities'
- epk3VolaIntervalsVDAX2m_RND2012.png: Plot of estimated conditional risk neutral densities
- epk3VolaIntervalsVDAX2m_PD2012.png: Plot of estimated conditional physical densities
- epk3VolaIntervalsVDAX2m_PK2012.png: Plot of estimated conditional pricing kernels

Example : 'The estimated conditional pricing kernels, risk neutral and physical densities for year
2012. For more details see Description.'

```

![Picture1](epk3VolaIntervalsVDAX2m_PD2012.png)

![Picture2](epk3VolaIntervalsVDAX2m_PK2012.png)

![Picture3](epk3VolaIntervalsVDAX2m_RND2012.png)


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

load("epk3VolaIntervalsVDAX2m/locLinBWvdax2m.RData")

timeToMaturity         = 2/12
horizonPhysicalDensity = 300

for (trading_year in 2012) {
  
  year = trading_year
  
  # path to Data 
  pathToData = paste("epk3VolaIntervalsVDAX2m/C_", as.character(year), "vdax2m", 
                     ".csv", sep = "")
  
  # read option data
  C_2012 = read.csv(pathToData, sep = ",", header = TRUE)
  C_2012 = na.omit(C_2012)
  
  # load Dax and VDax data
  bothIndexesDax      = read.csv("epk3VolaIntervalsVDAX2m/timeSeriesDaxVdax2m.csv")
  bothIndexesDax      = bothIndexesDax[, c("Date", "DAX", "VDAX")]
  bothIndexesDax$Date = as.Date(as.character(bothIndexesDax$Date), "%Y-%m-%d")
  
  bothIndexesDax = bothIndexesDax[bothIndexesDax$Date >= as.Date("2000-01-01"), ]
  bothIndexesDax = bothIndexesDax[bothIndexesDax$Date <= as.Date("2013-07-31"), ]
  
  
  # for selection of quantiles
  bothIndexesDaxQuant = bothIndexesDax[bothIndexesDax$Date <= as.Date(paste(as.character(year), 
                                                                            "-12-31", sep = "")), ]
  bothIndexesDaxQuant = bothIndexesDaxQuant[bothIndexesDaxQuant$Date >= as.Date(paste(as.character(year - 
                                                                                                     1), "-12-31", sep = "")), ]
  
  ttmFraction = timeToMaturity
  
  specifyQuant = c(0.05, 0.95)
  
  lowVolaIntUp    = quantile(bothIndexesDaxQuant$VDAX, 0.35)
  mediumVolaIntUp = quantile(bothIndexesDaxQuant$VDAX, 0.65)
  highVolaIntUp   = quantile(bothIndexesDaxQuant$VDAX, 0.95)
  lowVolaIntDown  = quantile(bothIndexesDaxQuant$VDAX, 0.05)
  
  
  
  vola_levels = as.character(seq(from = as.numeric(quantile(bothIndexesDaxQuant$VDAX, 
                                                            specifyQuant))[1], 
                                 to = as.numeric(quantile(bothIndexesDaxQuant$VDAX, specifyQuant))[2], 
                                 length.out = 20))
  
  listRndPDpk = list()
  
  for (vola_item in vola_levels) {
    print(vola_item)
    VDAX_level = as.numeric(vola_item)
    
    # add ttm in days
    C_2012$ttmDays = (C_2012$TTM) * 365
    
    # moneyness(Strike/S_0)
    C_2012$moneyness = C_2012$EXERCISE_PRICE/C_2012$DAX
    
    moneyness_est = seq(0.75, 1.5, length.out = 100)
    
    # option price
    C = C_2012[, "settlement"]
    
    # discount factor
    D = exp(-(C_2012$euriborRate) * C_2012$TTM)
    
    # scale by discount factor
    C = C/D
    
    # scale by forward price
    C = C/C_2012$DAX
    
    
    # subset option data used for estimation
    optionData      = cbind(C, C_2012[, c("TTM", "moneyness", "VDAX", "Date")])
    optionData$Date = as.Date(as.character(optionData$Date))
    
    
    # bandwidth specification as used for estimtion of the RND
    hTTM_RND  = bandwidthMonKfoldCVyears[[as.character(year)]]$matur
    hMon_RND  = bandwidthMonKfoldCVyears[[as.character(year)]]$mon
    hVDAX_RND = bandwidthMonKfoldCVyears[[as.character(year)]]$vola
    
    
    
    # Function performing local linear kernel regression
    locLinRegression = function(moneynessValue, tauValue, vdaxValue, data = optionData, 
                                h.TTM, h.moneyness, h.VDAX, output = "C") {
      
      # u_i, independent variables
      regressors = data[, c("TTM", "moneyness", "VDAX")]
      
      # response(dependent) variable of regression equation
      DependentVar = data[, output]
      
      n = nrow(regressors)
      
      Kernel = rep(0, n)
      
      # 3dim kernel function
      for (j in 1:n) {
        
        Kernel[j] = (dnorm((regressors$TTM[j] - tauValue)/h.TTM, mean = 0, 
                           sd = 1, log = FALSE)/h.TTM) * (dnorm((regressors$VDAX[j] - vdaxValue)/h.VDAX, 
                                                                mean = 0, sd = 1, log = FALSE)/h.VDAX) * (dnorm((regressors$moneyness[j] - 
                                                                                                                   moneynessValue)/h.moneyness, mean = 0, sd = 1, log = FALSE)/h.moneyness)
      }
      
      Kernel_matrix = Diagonal(n = length(Kernel), Kernel)
      
      regressors_minus_u = cbind(regressors$TTM - tauValue, regressors$VDAX - 
                                   vdaxValue, regressors$moneyness - moneynessValue)
      
      Omega = cbind(1, regressors_minus_u)
      
      invertedExpression = tryCatch(solve(t(Omega) %*% Kernel_matrix %*% Omega), 
                                    error = function(c) NA)
      
      if (is.element(NA, as.vector(invertedExpression))) {
        FirstOrderDerMon = NA
        constTerm        = NA
      } else {
        estCoef          = invertedExpression %*% t(Omega) %*% Kernel_matrix %*% DependentVar
        FirstOrderDerMon = estCoef[4]
        constTerm        = estCoef[1]
      }
      
      out = list(firstOrderDerivativeMoneyness = FirstOrderDerMon, constTerm = constTerm)
      
      out
      
    }
    
    
    # 1dim RND by numerical differentiation of obtained 1st order derivative
    
    coefficientsAll = mclapply(moneyness_est, locLinRegression, tauValue = ttmFraction, 
                               vdaxValue = VDAX_level, h.TTM = hTTM_RND, h.moneyness = hMon_RND, h.VDAX = hVDAX_RND)
    
    regression1DerMon = sapply(coefficientsAll, FUN = function(listItem) {
      listItem[["firstOrderDerivativeMoneyness"]]
    })
    
    # increment moneynes to compute derivtive
    delta          = 0.005
    moneyness_est0 = moneyness_est + delta
    
    
    # compute regression at the changed moneyness
    coefficientsAll0 = mclapply(moneyness_est0, locLinRegression, tauValue = ttmFraction, 
                                vdaxValue = VDAX_level, h.TTM = hTTM_RND, h.moneyness = hMon_RND, h.VDAX = hVDAX_RND)
    
    regression1DerMon0 = sapply(coefficientsAll0, FUN = function(listItem) {
      listItem[["firstOrderDerivativeMoneyness"]]
    })
    
    RND_given_ttm_final = (regression1DerMon0 - regression1DerMon)/delta
    
    RND_given_ttm = abs(RND_given_ttm_final * moneyness_est)
    excess_return = log(moneyness_est)
    
    # plot(excess_return, abs(RND_given_ttm), type='p') trapz(excess_return,
    # abs(RND_given_ttm))
    
    # calculation of the conditional variance
    
    optionDataForResidualsIndex = seq(1, nrow(optionData), 280)
    optionDataForResiduals      = optionData[optionDataForResidualsIndex, ]
    
    
    replicateOptionDataForResiduals = lapply(1:nrow(optionDataForResiduals), FUN = function(index) optionDataForResiduals)
    
    # function to perform loc lin regression used for calculation of conditional
    # variance
    locLinRegressionForRes = function(moneynessValue, tauValue, vdaxValue, data = optionData, 
                                      output = "C") {
      
      # u_i, independent variables
      regressors = data[, c("TTM", "moneyness", "VDAX")]
      
      # response(dependent) variable of regression equation
      DependentVar = data[, output]
      
      # bandwidth specification
      h.TTM       = sd(regressors$TTM) * nrow(regressors)^(-1/(4 + 3))
      h.moneyness = sd(regressors$moneyness) * nrow(regressors)^(-1/(4 + 3))
      h.VDAX      = sd(regressors$VDAX) * nrow(regressors)^(-1/(4 + 3))
      
      n = nrow(regressors)
      
      Kernel = rep(0, n)
      
      # 3dim kernel function
      for (j in 1:n) {
        
        Kernel[j] = (dnorm((regressors$TTM[j] - tauValue)/h.TTM, mean = 0, 
                           sd = 1, log = FALSE)/h.TTM) * (dnorm((regressors$VDAX[j] - vdaxValue)/h.VDAX, 
                                                                mean = 0, sd = 1, log = FALSE)/h.VDAX) * (dnorm((regressors$moneyness[j] - 
                                                                                                                   moneynessValue)/h.moneyness, mean = 0, sd = 1, log = FALSE)/h.moneyness)
      }
      
      Kernel_matrix = Diagonal(n = length(Kernel), Kernel)
      
      regressors_minus_u = cbind(regressors$TTM - tauValue, regressors$VDAX - 
                                   vdaxValue, regressors$moneyness - moneynessValue)
      
      Omega = cbind(1, regressors_minus_u)
      
      invertedExpression = tryCatch(solve(t(Omega) %*% Kernel_matrix %*% Omega), 
                                    error = function(c) NA)
      
      if (is.element(NA, as.vector(invertedExpression))) {
        FirstOrderDerMon = NA
        constTerm        = NA
      } else {
        estCoef = invertedExpression %*% t(Omega) %*% Kernel_matrix %*% DependentVar
        FirstOrderDerMon = estCoef[4]
        constTerm        = estCoef[1]
      }
      
      out = list(firstOrderDerivativeMoneyness = FirstOrderDerMon, constTerm = constTerm)
      
      out
      
    }
    
    
    # preparation to compute residuals
    forResiduals = mcmapply(locLinRegressionForRes, optionDataForResiduals$moneyness, 
                            optionDataForResiduals$TTM, optionDataForResiduals$VDAX, data = replicateOptionDataForResiduals)
    
    estimOptionPrice = unlist(forResiduals["constTerm", ])
    
    optionDataForResiduals$estimOptionPrice = estimOptionPrice
    
    # squared residuals
    optionDataForResiduals$squaredResid = (optionDataForResiduals$C - optionDataForResiduals$estimOptionPrice)^2
    
    
    # local linear regresson with squared residual as a dependent variable
    coefficientsResidualsReg = mclapply(moneyness_est, locLinRegressionForRes, 
                                        tauValue = ttmFraction, vdaxValue = VDAX_level, data = optionDataForResiduals, 
                                        output = "squaredResid")
    
    conditionalVariance = sapply(coefficientsResidualsReg, FUN = function(listItem) {
      listItem[["constTerm"]]
    })
    
    
    kernelConstant = 0.2115711
    
    
    # bandwidth specification for estimtion of the joint density, ROT
    hTTMjointDen = sd(optionData$TTM) * nrow(optionData)^(-1/(4 + 3)) * (4/5)^(1/(3 + 
                                                                                    4))
    hMonjointDen = sd(optionData$moneyness) * nrow(optionData)^(-1/(4 + 3)) * (4/5)^(1/(3 + 
                                                                                          4))
    hVDAXjointDen = sd(optionData$VDAX) * nrow(optionData)^(-1/(4 + 3)) * (4/5)^(1/(3 + 
                                                                                      4))
    
    # joint density of moneyness, tau, vola
    joinDensityRegressors = function(pointMon, pointTTM, pointVDAX, hMon, hTTM, 
                                     hVDAX, data = optionData) {
      
      jointDens = mean((1/hMon) * (1/hTTM) * (1/hVDAX) * dnorm((data$moneyness - 
                                                                  pointMon)/hMon, 
                                                               mean = 0, sd = 1, log = FALSE) * dnorm((data$TTM - 
                                                                                                         pointTTM)/hTTM, mean = 0, sd = 1, log = FALSE) * dnorm((data$VDAX - 
                                                                                                                                                                   pointVDAX)/hVDAX, mean = 0, sd = 1, log = FALSE))
      
      jointDens
    }
    
    jointDenValues = unlist(mclapply(moneyness_est, joinDensityRegressors, pointTTM = ttmFraction, 
                                     pointVDAX = VDAX_level, hMon = hMonjointDen, 
                                     hTTM = hTTMjointDen, hVDAX = hVDAXjointDen, 
                                     data = optionData))
    
    sigmaSquared = (moneyness_est^2) * (1/(3 * sqrt(pi))^3) * kernelConstant * 
      abs(conditionalVariance) / jointDenValues
    factorDistribution = nrow(optionData) * ((hMon_RND)^4) * (hMon_RND * hTTM_RND * 
                                                                hVDAX_RND)
    rndVariance = abs(sigmaSquared/factorDistribution)
    
    
    rndLocLinWithCI = data.frame(excess_return, 
                                 RND_given_ttm, 
                                 rndLocLinUp   = RND_given_ttm + qnorm(0.95, 0, 1) * sqrt(rndVariance), 
                                 rndLocLinDown = RND_given_ttm - qnorm(0.95, 0, 1) * sqrt(rndVariance)) 
    
    colnames(rndLocLinWithCI) = c("market_return", "RNDlocLin", "rndLocLinUp", 
                                  "rndLocLinDown") 
    
    
    # Local const estimation of PD 
    
    # used in contemporaneous approach for calculation of physical density
    dateHistDensity = as.Date(paste(as.character(year), "-12-31", sep = "")) 
    
    # physical density estimation (using contemporaneous method) 
    
    # maturity for which historical density to be calculated
    tauDays = round(ttmFraction * 365, 0)
    
    # how many observations from past to use for estimation of hist. density
    horizon = horizonPhysicalDensity
    
    # take only those rows that are earlier than dateHistDensity
    bothIndexesDax = bothIndexesDax[bothIndexesDax$Date <= dateHistDensity, ]
    
    # supposed to contain returns of the index observed over each maturity
    returnsDaxVdax           = bothIndexesDax[1:horizon, c("DAX", "VDAX")]
    colnames(returnsDaxVdax) = c("DaxReturns", "VDAXlevel")
    
    lengthBothIndexes = length(bothIndexesDax[, 1])
    
    for (i in 1:horizon) {
      returnsDaxVdax[i, 1] = log(bothIndexesDax$DAX[lengthBothIndexes - i]/(bothIndexesDax$DAX[lengthBothIndexes - 
                                                                                                 i - tauDays]))
      returnsDaxVdax[i, 2] = bothIndexesDax$VDAX[lengthBothIndexes - i]
    }
    
    
    # Local const regression(NW) for esimation of conditional physical density
    
    # specify bandwidth
    bwC = npcdensbw(xdat = returnsDaxVdax$VDAXlevel, ydat = returnsDaxVdax$DaxReturns, 
                    bwmethod = "normal-reference")
    
    hDaxReturnsJoint = 1.5 * bwC$ybw
    hVDAXJoint       = bwC$xbw
    hVDAXmarginal    = bwC$xbw
    
    # conditional kernel density estimation
    conditDensity = function(pointReturns, pointVDAX, hDaxReturnsJoint, hVDAXJoint, 
                             hVDAXmarginal, returnsDaxVdax) {
      
      jointDens = mean((1/hDaxReturnsJoint) * (1/hVDAXJoint) * dnorm((returnsDaxVdax[, 
                                                                                     1] - pointReturns)/hDaxReturnsJoint, mean = 0, sd = 1, log = FALSE) * 
                         dnorm((returnsDaxVdax[, 2] - pointVDAX)/hVDAXJoint, mean = 0, sd = 1, 
                               log = FALSE))
      
      marginalDenVDAX = mean((1/hVDAXmarginal) * dnorm((returnsDaxVdax[, 2] - 
                                                          pointVDAX)/hVDAXmarginal, mean = 0, sd = 1, log = FALSE))
      
      outCondDensity = jointDens/marginalDenVDAX
      
      nFactor = nrow(returnsDaxVdax)
      c1      = hDaxReturnsJoint
      c2      = hVDAXJoint
      confInt = ((1/(4 * 3.14)) * outCondDensity/(c1 * c2 * marginalDenVDAX))/nFactor
      
      out = list(conditionalDensity = outCondDensity, confidenceInterval = confInt)
      
    }
    
    # range of log returns (to calculate values of the density at these points)
    eurostoxx_range = excess_return  # from file with RND estimation
    
    
    eurostoxx_hist_densityValues = lapply(eurostoxx_range, conditDensity, 
                                          pointVDAX = VDAX_level, 
                                          hDaxReturnsJoint = hDaxReturnsJoint, 
                                          hVDAXJoint = hVDAXJoint, 
                                          hVDAXmarginal = hVDAXmarginal, 
                                          returnsDaxVdax = returnsDaxVdax)
    
    # get density values, and variance separately
    
    densityValues = sapply(eurostoxx_hist_densityValues, FUN = function(listItem) {
      listItem[[1]]
    })
    
    variancePD = sapply(eurostoxx_hist_densityValues, FUN = function(listItem) {
      listItem[[2]]
    })
    
    # data frame with density values and CI
    dataPDlocConstWithCI = data.frame(returns = eurostoxx_range, PhysicalDensityLocalConstValue = densityValues, 
                                      pdLocalConstUpperBound = densityValues + qnorm(0.95, 0, 1) * sqrt(variancePD), 
                                      pdLocalConstLowerBound = densityValues - qnorm(0.95, 0, 1) * sqrt(variancePD))
    
    
    # density integrates to 1 trapz(eurostoxx_range, densityValues)
    # plot(eurostoxx_range, densityValues)
    
    # EPK
    
    PK_value = (rndLocLinWithCI$RNDlocLin)/(dataPDlocConstWithCI$PhysicalDensityLocalConstValue)
    
    varianceEPK = (1/(dataPDlocConstWithCI$PhysicalDensityLocalConstValue))^2 * rndVariance + 
      ((1/(dataPDlocConstWithCI$PhysicalDensityLocalConstValue))^4) * ((rndLocLinWithCI$RNDlocLin)^2) * 
      variancePD
    
    rndPDpk = data.frame(returns                        = eurostoxx_range, 
                         PricingKernel                  = PK_value, 
                         pkUpperBound                   = PK_value + qnorm(0.95, 0, 1) * sqrt(varianceEPK),
                         pkLowerBound                   = PK_value - qnorm(0.95, 0, 1) * sqrt(varianceEPK), 
                         PhysicalDensityLocalConstValue = densityValues, 
                         pdLocalConstUpperBound         = densityValues + qnorm(0.95, 0, 1) * sqrt(variancePD), 
                         pdLocalConstLowerBound         = densityValues - qnorm(0.95, 0, 1) * sqrt(variancePD), 
                         RND_given_ttm, 
                         rndLocLinUp                    = RND_given_ttm + qnorm(0.95, 0, 1) * sqrt(rndVariance), 
                         rndLocLinDown                  = RND_given_ttm - qnorm(0.95, 0, 1) * sqrt(rndVariance))
    
    listRndPDpk[[vola_item]] = rndPDpk
  }
  
  # save as RData
  save(vola_levels, 
       listRndPDpk, 
       year, 
       lowVolaIntUp, 
       mediumVolaIntUp, 
       highVolaIntUp, 
       file = paste0("epk3VolaIntervalsVDAX2m/listRndPDpkVDAX2m", year, ".RData"))
  
  # quantiles of VDAX used for CI 
  quantile_20 = quantile(bothIndexesDaxQuant$VDAX, 0.2)
  quantile_50 = quantile(bothIndexesDaxQuant$VDAX, 0.5)
  quantile_80 = quantile(bothIndexesDaxQuant$VDAX, 0.8)
  
  #
  # definition of funtions for plotting of PKs, PDs, RNDs
  #
  
  plotPK = function(yearItem){
    
    # load precomputed objects
    load(paste0("epk3VolaIntervalsVDAX2m/listRndPDpkVDAX2m", as.character(yearItem), ".RData"))
    
    # add aditional label to every plot
    remarkPlot = "VDAX2m"
    
    # prepare for plotting
    
    for(itemN in (1:length(vola_levels))){
      listRndPDpk[[vola_levels[itemN]]]$VDAXlevel = vola_levels[itemN]
    }
    
    
    # bind all elements of the list in one data frame
    dataAllVDAXlevels = do.call("rbind", listRndPDpk) 
    
    
    # Plotting together with CI 
    
    # small volatility interval
    pkAllsmallVola = ggplot(data = dataAllVDAXlevels %>% filter(VDAXlevel <= as.numeric(lowVolaIntUp)), 
                            aes(x = returns, y = PricingKernel, colour = VDAXlevel)) + 
      geom_line() + 
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 5)) + 
      theme_bw() + 
      theme(legend.position = "none") + 
      ggtitle(paste0("low volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntDown), 2), 
                                         nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)), 
                     ")")) +
      xlab("") + 
      ylab("PK") + 
      scale_colour_brewer(palette = "PuOr")
    
    
    pkAllsmallVolaCI = ggplot(data = dataAllVDAXlevels %>%
                                filter(VDAXlevel ==
                                         as.numeric(vola_levels)[as.numeric(vola_levels)
                                                                 >= as.numeric(quantile_20)
                                                                 ][1]),
                              aes(x = returns)) +
      geom_line(aes(y = PricingKernel), size = 0.5, colour = "black") +
      geom_ribbon(aes(ymin = pkLowerBound, ymax = pkUpperBound), alpha = 0.5) +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 5)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("low volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntDown), 
                                               2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)), 
                     ")")) +
      xlab("") +
      ylab("PK")
    
    
    # medium volatility interval
    pkAllmediumVola = ggplot(data = dataAllVDAXlevels %>% 
                               filter(VDAXlevel > as.numeric(lowVolaIntUp),
                                      VDAXlevel <= as.numeric(mediumVolaIntUp)), 
                             aes(x = returns, y = PricingKernel, colour = VDAXlevel)) +
      geom_line() +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 5)) +
      theme_bw() +
      theme(legend.position="none") +
      ggtitle(paste0("medium volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)), 
                     ")")) +
      xlab("") +
      ylab("PK") +
      scale_colour_brewer(palette = "PuOr")
    
    # take first VDAX level that is not less than 50% quantile
    pkAllmediumVolaCI = ggplot(data = dataAllVDAXlevels %>%
                                 filter(VDAXlevel ==
                                          as.numeric(vola_levels)[as.numeric(vola_levels)
                                                                  >= as.numeric(quantile_50)
                                                                  ][1]),
                               aes(x = returns))+
      geom_line(aes(y = PricingKernel), size = 0.5, colour = "black") +
      geom_ribbon(aes(ymin = pkLowerBound, ymax = pkUpperBound), alpha = 0.5) +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 5)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("medium volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("PK")
    
    # high volatility
    pkAllhighVola = ggplot(data = dataAllVDAXlevels %>%
                             filter(VDAXlevel > as.numeric(mediumVolaIntUp)),
                           aes(x = returns, y = PricingKernel, colour = VDAXlevel)) +
      geom_line() +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 5)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("high volatility (between ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(highVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("PK") +
      scale_colour_brewer(palette="PuOr")
    
    
    pkAllhighVolaCI = ggplot(data = dataAllVDAXlevels %>%
                               filter(VDAXlevel ==
                                        as.numeric(vola_levels)[as.numeric(vola_levels)
                                                                >= as.numeric(quantile_80)
                                                                ][1]),
                             aes(x = returns)) +
      geom_line(aes(y = PricingKernel), size = 0.5, colour = "black") +
      geom_ribbon(aes(ymin = pkLowerBound, ymax = pkUpperBound), alpha = 0.5) +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 5) ) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("high volatility (between ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(highVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("PK")
    
    # allocate plots on the one page
    out = grid.arrange(pkAllsmallVola,
                       pkAllsmallVolaCI,
                       pkAllmediumVola,
                       pkAllmediumVolaCI,
                       pkAllhighVola,
                       pkAllhighVolaCI,
                       ncol = 2) 
    
    
    # specify name of the plot to be saved
    plotName = paste("epk3VolaIntervalsVDAX2m/",
                     "epk3VolaIntervalsVDAX2m",
                     "_",
                     "PK",
                     as.character(year),
                     ".png", sep = "")
    
    ggsave(plotName, out, width = 9, height = 12)
    
    
  }
  
  
  plotPD = function(yearItem){
    
    # load precomputed objects
    load(paste0("epk3VolaIntervalsVDAX2m/listRndPDpkVDAX2m", as.character(yearItem), ".RData"))
    
    # add aditional label to every plot
    remarkPlot = "VDAX2m"
    
    # prepare for plotting
    
    for(itemN in (1:length(vola_levels))){
      listRndPDpk[[vola_levels[itemN]]]$VDAXlevel = vola_levels[itemN]
    }
    
    # bind all elements of the list in one data frame
    dataAllVDAXlevels = do.call("rbind", listRndPDpk) 
    
    # Plotting together with CI
    
    # small volatility interval 
    pdAllsmallVola = ggplot(data = dataAllVDAXlevels %>%
                              filter(VDAXlevel <= as.numeric(lowVolaIntUp)),
                            aes(x = returns, 
                                y = PhysicalDensityLocalConstValue, 
                                colour = VDAXlevel)) +
      geom_line() +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("low volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntDown), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)), ")")) +
      xlab("") +
      ylab("PD") +
      scale_colour_brewer(palette = "PuOr")
    
    pdAllsmallVolaCI = ggplot(data = dataAllVDAXlevels %>%
                                filter(VDAXlevel ==
                                         as.numeric(vola_levels)[as.numeric(vola_levels)
                                                                 >= as.numeric(quantile_20)
                                                                 ][1]),
                              aes(x = returns)) +
      geom_line(aes(y = PhysicalDensityLocalConstValue), size = 0.5, colour="black") +
      geom_ribbon(aes(ymin = pdLocalConstLowerBound, ymax = pdLocalConstUpperBound), alpha=0.5) +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("low volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntDown), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("PD")
    
    
    # medium volatility interval
    pdAllmediumVola = ggplot(data = dataAllVDAXlevels %>% 
                               filter(VDAXlevel > as.numeric(lowVolaIntUp),
                                      VDAXlevel <= as.numeric(mediumVolaIntUp)), 
                             aes(x = returns, 
                                 y = PhysicalDensityLocalConstValue, 
                                 colour = VDAXlevel)) +
      geom_line() +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim=c(0, 10)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("medium volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("PD") +
      scale_colour_brewer(palette="PuOr")
    
    # take first VDAX level that is not less than 50% quantile
    pdAllmediumVolaCI = ggplot(data=dataAllVDAXlevels %>%
                                 filter(VDAXlevel ==
                                          as.numeric(vola_levels)[as.numeric(vola_levels)
                                                                  >= as.numeric(quantile_50)
                                                                  ][1]),
                               aes(x = returns)) +
      geom_line(aes(y = PhysicalDensityLocalConstValue), size = 0.5, colour = "black") +
      geom_ribbon(aes(ymin = pdLocalConstLowerBound, ymax = pdLocalConstUpperBound), alpha = 0.5) +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("medium volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("PD")
    
    # high volatility
    pdAllhighVola = ggplot(data=dataAllVDAXlevels %>%
                             filter(VDAXlevel > as.numeric(mediumVolaIntUp)),
                           aes(x = returns, y = PhysicalDensityLocalConstValue, colour = VDAXlevel )) +
      geom_line() +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10) ) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("high volatility (between ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(highVolaIntUp), 2), nsmall = 2)), 
                     ")")) +
      xlab("") +
      ylab("PD") +
      scale_colour_brewer(palette = "PuOr")
    
    
    pdAllhighVolaCI = ggplot(data = dataAllVDAXlevels %>%
                               filter(VDAXlevel ==
                                        as.numeric(vola_levels)[as.numeric(vola_levels)
                                                                >= as.numeric(quantile_80)
                                                                ][1]),
                             aes(x = returns))+
      geom_line(aes(y = PhysicalDensityLocalConstValue), size = 0.5, colour = "black") +
      geom_ribbon(aes(ymin = pdLocalConstLowerBound, ymax = pdLocalConstUpperBound), alpha = 0.5) +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 8) ) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("high volatility (between ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(highVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("PD")
    
    # allocate plots on the one page
    out = grid.arrange(pdAllsmallVola,
                       pdAllsmallVolaCI,
                       pdAllmediumVola,
                       pdAllmediumVolaCI,
                       pdAllhighVola,
                       pdAllhighVolaCI,
                       ncol = 2) 
    
    
    # specify name of the plot to be saved
    plotName = paste("epk3VolaIntervalsVDAX2m/",
                     "epk3VolaIntervalsVDAX2m",
                     "_PD",
                     as.character(year),
                     ".png", sep = "")
    
    ggsave(plotName, out, width = 9, height = 12)
    
  }
  
  plotRND = function(yearItem){
    
    # load precomputed objects
    load(paste0("epk3VolaIntervalsVDAX2m/listRndPDpkVDAX2m", as.character(yearItem), ".RData"))
    
    # add aditional label to every plot
    remarkPlot = "VDAX2m"
    
    # prepare for plotting
    
    for(itemN in (1:length(vola_levels))){
      listRndPDpk[[vola_levels[itemN]]]$VDAXlevel = vola_levels[itemN]
    }
    
    # bind all elements of the list in one data frame
    dataAllVDAXlevels = do.call("rbind", listRndPDpk) 
    
    # Plotting together with CI 
    
    # small volatility interval 
    pdAllsmallVola = ggplot(data = dataAllVDAXlevels %>%
                              filter(VDAXlevel <= as.numeric(lowVolaIntUp)),
                            aes(x = returns, y = RND_given_ttm, colour = VDAXlevel)) +
      geom_line() +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("low volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntDown), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("RND") +
      scale_colour_brewer(palette = "PuOr")
    
    pdAllsmallVolaCI = ggplot(data = dataAllVDAXlevels %>%
                                filter(VDAXlevel ==
                                         as.numeric(vola_levels)[as.numeric(vola_levels)
                                                                 >= as.numeric(quantile_20)
                                                                 ][1]),
                              aes(x = returns)) +
      geom_line(aes(y = RND_given_ttm), size = 0.5, colour = "black") +
      geom_ribbon(aes(ymin = rndLocLinDown, ymax = rndLocLinUp), alpha = 0.5) +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10) ) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("low volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntDown), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("RND")
    
    
    # medium volatility interval
    pdAllmediumVola = ggplot(data = dataAllVDAXlevels %>% 
                               filter(VDAXlevel > as.numeric(lowVolaIntUp),
                                      VDAXlevel <= as.numeric(mediumVolaIntUp)), 
                             aes(x = returns, y = RND_given_ttm, colour = VDAXlevel )) +
      geom_line() +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("medium volatility (between ", 
                     as.character(format(round(as.numeric(lowVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     ")" )) +
      xlab("") +
      ylab("RND") +
      scale_colour_brewer(palette = "PuOr")
    
    # take first VDAX level that is not less than 50% quantile
    pdAllmediumVolaCI = ggplot(data = dataAllVDAXlevels %>%
                                 filter(VDAXlevel ==
                                          as.numeric(vola_levels)[as.numeric(vola_levels)
                                                                  >= as.numeric(quantile_50)
                                                                  ][1]),
                               aes(x = returns)) +
      geom_line(aes(y = RND_given_ttm), size = 0.5, colour = "black") +
      geom_ribbon(aes(ymin = rndLocLinDown, ymax = rndLocLinUp), alpha = 0.5) +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10)) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("medium volatility (between ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     ")" ))+
      xlab("") +
      ylab("RND")
    
    # high volatility
    pdAllhighVola = ggplot(data = dataAllVDAXlevels %>% filter(VDAXlevel > as.numeric(mediumVolaIntUp)), 
                           aes(x = returns, y = RND_given_ttm, colour = VDAXlevel)) + 
      geom_line() + 
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10)) +
      theme_bw() + 
      theme(legend.position = "none") + 
      ggtitle(paste0("high volatility (between ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(highVolaIntUp), 2), nsmall = 2)),
                     ")")) + 
      xlab("") + 
      ylab("RND") + 
      scale_colour_brewer(palette = "PuOr")
    
    
    pdAllhighVolaCI = ggplot(data = dataAllVDAXlevels %>%
                               filter(VDAXlevel ==
                                        as.numeric(vola_levels)[as.numeric(vola_levels)
                                                                >= as.numeric(quantile_80)
                                                                ][1]),
                             aes(x = returns)) +
      geom_line(aes(y = RND_given_ttm), size = 0.5, colour = "black") +
      geom_ribbon(aes(ymin = rndLocLinDown, ymax = rndLocLinUp), alpha = 0.5) +
      coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 10) ) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(paste0("high volatility (between ", 
                     as.character(format(round(as.numeric(mediumVolaIntUp), 2), nsmall = 2)),
                     " and ", 
                     as.character(format(round(as.numeric(highVolaIntUp), 2), nsmall = 2)),
                     ")")) +
      xlab("") +
      ylab("RND")
    
    # allocate plots on the one page
    out = grid.arrange(pdAllsmallVola,
                       pdAllsmallVolaCI,
                       pdAllmediumVola,
                       pdAllmediumVolaCI,
                       pdAllhighVola,
                       pdAllhighVolaCI,
                       ncol = 2) 
    
    
    # specify name of the plot to be saved
    plotName = paste("epk3VolaIntervalsVDAX2m/",
                     "epk3VolaIntervalsVDAX2m",
                     "_",
                     "RND",
                     as.character(year),
                     ".png", sep = "")
    
    ggsave(plotName, out, width = 9, height = 12)
    
    
  }
  
  plotPK(year)
  plotRND(year)
  plotPD(year)
  
}



```

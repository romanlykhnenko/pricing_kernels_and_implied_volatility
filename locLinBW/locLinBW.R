# install and load packages
libraries = c("dplyr", "MASS", "gridExtra", "ggplot2", "Matrix", "parallel", "caTools", 
              "np", "RColorBrewer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# set working directory
setwd("/home/rama/Masterarbeit/masterThesisRomanLykhnenko")


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



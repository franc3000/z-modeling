# Winsorisation is the transformation of statistics by limiting extreme values 
# in the statistical data to reduce the effect of possibly spurious outliers.
winsorize <- function(x) {
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

this.dir = '~/Documents/Code/modeling/'
setwd(this.dir)

# import the data
x = read.csv('data/income_tav_ratio.csv')
head(x)

# raw ratio
hist(x$ratio, breaks=100, main='Ratio: Income / TotalAssessedValue', xlab='Ratio')

# winsorized ratio
x$ratio.winsor = winsorize(x$ratio)
hist(x$ratio.winsor, col='gray', breaks=100, 
     main='Winsorized Ratio: Income / TotalAssessedValue', xlab='Ratio')

# 0 < ratio < 95th pctile
q95 = quantile(x$ratio, .95)
hist(x[x$ratio.winsor < q95 & x$ratio.winsor > 0,]$ratio, breaks=60, col='gray', 
     main='0 < Ratio < 95th Pctile', xlab='Ratio')

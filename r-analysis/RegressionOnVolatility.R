# functions are available in source file
factorGroupFileName="VolatilityFunctions.R"
source(factorGroupFileName)

#
# The resolved data frame may be created by joining the resolved factors to a price file
#
# Source and destination file names
priceFileName="dataframes/WIKI_BLK.csv"
resolvedFactorsFileName="dataframes/factors.clean.csv"

# PCA groups
pricesDF = read.csv(priceFileName)

# Market risk factors
resolvedFactorsDF = read.csv(resolvedFactorsFileName)

keyColumnName="valueDate"
priceColumnName="closePrice"

resolvedDataFrame= merge(pricesDF[,c(keyColumnName,priceColumnName)],resolvedFactorsDF,by=keyColumnName)

#
# The number of samples and the h-day window
#
numSamples=250
hDays=10

start=nrow(resolvedDataFrame)-numSamples
end=nrow(resolvedDataFrame)
workDF = resolvedDataFrame[start:end,]

hDayDF=hDayVolatility(hDays,workDF)

lm.cmc = lm(closePrice~., data=hDayDF)
anova(lm.cmc)
lm.cmc.step=step(lm.cmc)

#
# Optional plot
#
actualCol=3
predictedCol=2

plotXAxis=as.vector(substr(workDF[,keyColumnName],1,10))
plot(hDayDF$closePrice, xaxt="n", type="l", col=actualCol, ylab="10-day Price Volatility", xlab="Date", main="BlackRock. (BLK)" )
xAxisPoints = seq(1,length(plotXAxis), length.out = 5)
axis(1, at=xAxisPoints, labels = plotXAxis[xAxisPoints])
legend(x=1, y=6.5, legend=c("Actual","Predicted"),lty=c(1,1),col=c(actualCol,predictedCol)) # Customize according to output
lines(predict(lm.cmc.step,hDayDF), type="l" , col=predictedCol)


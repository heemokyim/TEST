load("NatalRiskData.rData")
getwd()
sdata
train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]








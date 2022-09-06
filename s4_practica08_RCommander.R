
data(cancer, package="survival")
with(cancer, Hist(age, scale="frequency", breaks="Sturges", col="darkgray"))
library(abind, pos=19)
library(e1071, pos=20)
numSummary(cancer[,"age", drop=FALSE], statistics=c("mean", "sd", "IQR", 
  "quantiles"), quantiles=c(0,.25,.5,.75,1))
Boxplot( ~ age, data=cancer, id=list(method="y"))


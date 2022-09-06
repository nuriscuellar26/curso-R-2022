
Demo <- readSPSS("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/demo.sav", 
  rownames=FALSE, stringsAsFactors=TRUE, tolower=FALSE)
library(abind, pos=17)
local({
  .Table <- xtabs(~marital+inccat, data=Demo)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})





stripchart(income ~ marital, vertical=TRUE, method="stack", ylab="income", data=Demo)


library(e1071, pos=18)
numSummary(Demo[,"income", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles"), 
  quantiles=c(0,.25,.5,.75,1))
Boxplot( ~ income, data=Demo, id=list(method="y"))


with(Demo, plotMeans(income, marital, error.bars="se", connect=TRUE))



t.test(income~marital, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Demo)





estaturas <- read.table("estaturas.dat")

cor(estaturas[,c("V1","V5")], use="complete")


numSummary(estaturas[,"V2", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles"), 
  quantiles=c(0,.25,.5,.75,1))


scatterplot(V1~V5, regLine=FALSE, smooth=FALSE, boxplots=FALSE, data=estaturas)


RegModel.1 <- lm(V1~V5, data=estaturas)
summary(RegModel.1)


Anova(RegModel.1, type="II")


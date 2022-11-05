
costes <- 
  read.table("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/costes.dat", 
  header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
library(lattice, pos=17)
editDataset(costes)


costes <- read.table("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/costes.dat", header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", 
  strip.white=TRUE)
xyplot(Costes ~ Unidades, type="p", pch=16, auto.key=list(border=TRUE), 
  par.settings=simpleTheme(pch=16), scales=list(x=list(relation='same'), 
  y=list(relation='same')), data=costes)

RegModel.1 <- lm(Costes~Unidades, data=costes)
summary(RegModel.1)


LinearModel.2 <- lm(Costes ~ Unidades-1, data=costes)
summary(LinearModel.2)
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(LinearModel.2)
par(oldpar)
costes<- within(costes, {
  fitted.LinearModel.2 <- fitted(LinearModel.2)
  residuals.LinearModel.2 <- residuals(LinearModel.2)
  rstudent.LinearModel.2 <- rstudent(LinearModel.2)
  hatvalues.LinearModel.2 <- hatvalues(LinearModel.2)
  cooks.distance.LinearModel.2 <- cooks.distance(LinearModel.2) 
})
preciocasas <- 
  read.table("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/preciocasas.txt",
   header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", 
  strip.white=TRUE)
scatterplotMatrix(~X0.000000000e.000+X1.000000000e.000+X1.300000000e.001+X1.540000000e.002+X2.000000000e.000+X2.213330000e.005+X3.000000000e.000,
   regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), data=preciocasas)
editDataset(preciocasas)
scatterplotMatrix(~Precio+X1+X2+X3+X4+X5+X6, regLine=FALSE, smooth=FALSE, 
  diagonal=list(method="density"), data=preciocasas)
RegModel.3 <- lm(Precio~X1+X2+X3+X4+X5+X6, data=preciocasas)
summary(RegModel.3)

library(MASS, pos=18)
stepwise(RegModel.3, direction='backward/forward', criterion='BIC')
vif(RegModel.3)
round(cov2cor(vcov(RegModel.3)), 3) # Correlations of parameter estimates


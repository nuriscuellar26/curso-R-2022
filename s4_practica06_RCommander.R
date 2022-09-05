
consumo <- 
  read.table("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/Consumo.txt",
   header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", 
  strip.white=TRUE)
summary(consumo)
with(consumo, Barplot(PC, xlab="PC", ylab="Frequency", label.bars=TRUE))
library(colorspace, pos=18)
with(consumo, piechart(PC, xlab="", ylab="", main="PC", col=rainbow_hcl(3), 
  scale="percent"))


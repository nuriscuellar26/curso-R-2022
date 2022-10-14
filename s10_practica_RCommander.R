
empleados <- 
  readXL("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/empleados.xls", 
  rownames=FALSE, header=TRUE, na="", sheet="Respuestas", stringsAsFactors=TRUE)
save("empleados", 
  file="C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/empleados.R.Data")


alpha<-0.05
varianza<-7.5

n <- nrow(empleados)
media <- mean(empleados$Altura)
cuantil<- qnorm(1-alpha/2)

lim_inferior<-media -cuantil * sqrt(varianza) / sqrt(n)
lim_inferior

im_superior<- media + cuantil * sqrt(varianza) / sqrt(n)
lim_superior

with(empleados, (t.test(Altura, alternative='two.sided', mu=0.0, conf.level=.90)))
local({
  .Table <- xtabs(~ Sexo , data= empleados )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.95, correct=FALSE)
})

Tapply(Peso ~ Sexo, var, na.action=na.omit, data=empleados) # variances by group
var.test(Peso ~ Sexo, alternative='two.sided', conf.level=.95, data=empleados)
t.test(Peso~Sexo, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=empleados)
s
upuesto6 <- 
  read.table("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/supuesto6.txt",
   header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

with(supuesto6, (t.test(Antes, Después, alternative='two.sided', conf.level=.90, 
  paired=TRUE)))

empleados <- within(empleados, {
  coche_rec <- Recode(Coche, '"No"="2No"; "Sí"="1Sí";', as.factor=TRUE)
})

library(abind, pos=17)
local({  .Table <- xtabs(~Sexo+coche_rec, data=empleados)
  cat("\nPercentage table:\n")
  print(rowPercents(.Table))
  prop.test(.Table, alternative='two.sided', conf.level=.85, correct=FALSE)
})


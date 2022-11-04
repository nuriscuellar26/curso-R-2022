
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



universidad <- 
  read.table("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/universidad.txt", 
             header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
with(universidad, (t.test(C.I., alternative='two.sided', mu=0.0, conf.level=.95)))

Tapply(C.I. ~ Grupo, var, na.action=na.omit, data=universidad) # variances by group
var.test(C.I. ~ Grupo, alternative='two.sided', conf.level=.98, data=universidad)
t.test(C.I.~Grupo, alternative='two.sided', conf.level=.98, var.equal=TRUE, data=universidad)
local({
  .Table <- xtabs(~ Grupo , data= universidad )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.90, correct=FALSE)
})
universidad <- within(universidad, {
  Grupo_rec <- Recode(Grupo, '"A"="2A"; "B"="1B"', as.factor=TRUE)
})
local({
  .Table <- xtabs(~ Grupo_rec , data= universidad )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.90, correct=FALSE)
})
library(abind, pos=18)
local({  .Table <- xtabs(~Grupo+Estadistica, data=universidad)
cat("\nPercentage table:\n")
print(rowPercents(.Table))
prop.test(.Table, alternative='two.sided', conf.level=.93, correct=FALSE)
})
tcardiaca <- 
  read.table("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/tcardiaca.txt",
             header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
with(tcardiaca, (t.test(TCM, TCT, alternative='two.sided', conf.level=.99, paired=TRUE)))
empresa <- 
  readXL("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/empresa.xls",
         rownames=FALSE, header=TRUE, na="", sheet="Respuestas", stringsAsFactors=TRUE)
empresa <- within(empresa, {
  Producto_rec <- Recode(Producto, '"Sí"="1Sí"; "No"="2No"', as.factor=TRUE)
})
local({  .Table <- xtabs(~Sexo+Producto_rec, data=empresa)
cat("\nPercentage table:\n")
print(rowPercents(.Table))
prop.test(.Table, alternative='two.sided', conf.level=.99, correct=FALSE)
})
moscas <- 
  read.table("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/moscas.txt",
             header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
with(moscas, (t.test(Longitud, alternative='two.sided', mu=0.0, conf.level=.99)))
salario <- 
  readXL("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/salario.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Hoja1", stringsAsFactors=TRUE)
with(salario, (t.test(Salario, alternative='two.sided', mu=0.0, conf.level=.89)))
Tapply(Salario ~ Ciudad, var, na.action=na.omit, data=salario) # variances by group
var.test(Salario ~ Ciudad, alternative='two.sided', conf.level=.95, data=salario)

t.test(Salario~Ciudad, alternative='two.sided', conf.level=.90, var.equal=FALSE, 
       data=salario)
local({
  .Table <- xtabs(~ Ciudad , data= salario )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.90, correct=FALSE)
})
manzanas <- 
  readXL("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/manzanas.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Hoja1", stringsAsFactors=TRUE)
with(manzanas, (t.test(Antes, Después, alternative='two.sided', conf.level=.98, paired=TRUE)))
propuesto <- 
  readXL("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/propuesto3-1.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Hoja1", stringsAsFactors=TRUE)
local({
  .Table <- xtabs(~ Azules , data= propuesto )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.95, correct=FALSE)
})
propuesto <- within(propuesto, {
  Azules_rec <- Recode(Azules, '"Sí"="1Sí"; "No"="2No"', as.factor=TRUE)
})
local({
  .Table <- xtabs(~ Azules_rec , data= propuesto )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.95, correct=FALSE)
})
propuesto4 <- 
  readXL("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/propuesto4-1.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Hoja1", stringsAsFactors=TRUE)
propuesto4 <- within(propuesto4, {
  resultado_rec <- Recode(resultado, '"Sí"="1Sí"; "No"="2No"', as.factor=TRUE)
})
local({  .Table <- xtabs(~medida+resultado_rec, data=propuesto4)
cat("\nPercentage table:\n")
print(rowPercents(.Table))
prop.test(.Table, alternative='two.sided', conf.level=.95, correct=FALSE)
})
propuesto4 <- within(propuesto4, {
  media_rec <- Recode(medida, '"menosde20"="1menosde20"; "masde40"="2masde40";', 
                      as.factor=TRUE)
})
local({  .Table <- xtabs(~media_rec+resultado_rec, data=propuesto4)
cat("\nPercentage table:\n")
print(rowPercents(.Table))
prop.test(.Table, alternative='two.sided', conf.level=.95, correct=FALSE)
})


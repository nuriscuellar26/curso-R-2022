
Datos01 <- read.table("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/datos01.txt", header=TRUE, stringsAsFactors=TRUE, sep="", 
  na.strings="NA", dec=".", strip.white=TRUE)
contam_mex <- readXL("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/contaminacion_mexico.xls", rownames=FALSE, header=TRUE, 
  na="", sheet="contaminacion_mexico", stringsAsFactors=TRUE)
Demo <- readSPSS("C:/Users/abbyc/Desktop/Ciclo II 2022/Análisis estadístico con R/curso-R-2022/demo.sav", rownames=FALSE, stringsAsFactors=TRUE, tolower=FALSE)


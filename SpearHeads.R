#1
spear <-data.frame(spearheads)
print(spear)

#2
print(names(spear))
names(spear) <- c("id", "Materiales", "Contexto","Loop","Remache","Conservación","Fecha","Longitud_max",
                       "Longitud_encaje","Ancho_max","Ancho_encaje","Losoc","Ancho_max_encaje","Peso")
names(spear)[names(spear) == "Materiales"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
#He cambiado los nombres con la función names(data.frame), pero para esta práctica es necesario cambiar los
#nombres con otras función, para la cual he escrito dos ejemplos y no todas las variables.

#3
spear$Contexto=factor(spear$Contexto, levels = c(1,2,3), labels=c("S/C","Habitacional","Funerario"))
spear$Conservación=factor(spear$Conservación, levels = c(1,2,3,4), labels=c("Excelente","Bueno","Regular","Malo"))
spear$Remache=factor(spear$Remache, levels = c(1,2), labels=c("Sí","No"))
spear$Materiales=factor(spear$Materiales, levels = c(1,2), labels=c("Bronce","Hierro"))

View(spear)

#4
freq.mat=table(spear$Materiales)
View(freq.mamtt)
freq.cont=table(spear$Contexto)
View(freq.cont)
freq.cons=table(spear$Conservación)
View(freq.cons)

#5
cross.contmat=table(spear$Materiales,spear$Contexto)
View(cross.contcons)
cross.consmat=table(spear$Conservación, spear$Materiales)
View(cross.consmat)

#6
prop.mat=prop.table(freq.mat)
View(prop.mat)
prop.mat <- data.frame(prop.mat)
prop.mat$Porcentaje <- prop.mat$Freq * 100
prop.mat

prop.cont=prop.table(freq.cont)
View(prop.cont)
prop.cont <- data.frame(prop.cont)
prop.cont$Porcentaje <- prop.cont$Freq * 100
prop.cont

prop.cons=prop.table(freq.cons)
View(prop.cons)
prop.cons <- data.frame(prop.cons)
prop.cons$Porcentaje <- prop.cons$Freq * 100
prop.cons

#7
prop.cross.matcont=round(prop.table(cross.contmat)*100,0)
View(prop.cross.matcont)
prop.cross.matcons=round(prop.table(cross.consmat)*100,0)
View(prop.cross.matcons)


#8
bar.cons=barplot(table(spear$Conservación))
bar.cons=barplot(table(spear$Contexto))

#9
barh.mater=barplot(table(spear$Materiales), horiz=TRUE,
                   main = "Materiales",
                   xlab = "Frecuencia",
                   ylab = "Materiales",
                   col = "brown")
barh.remache=barplot(table(spear$Remache), horiz = TRUE,
                     main = "Remache",
                     xlab = "Frecuencia",
                     ylab = "Remache",
                     col = "brown")

#10
bar.cond = barplot(cross.consmat, width = 0.85, ylim = c(0, sum(cross.consmat[,1])*1.1),
                   main = "Estado de conservación vs. Materiales",
                   ylab = "Frecuencia",
                   legend=T)

#11
labs <- paste("(",freq.cons,")\n", names(freq.cons), sep ="")
pie(freq.cons, labels = labs, main="Conservación recuento", col=gray.colors(length(levels(factor(names(freq.cons)))), start = 0.3, end = 0.9))

#12
windows(width = 15, height = 15)
var_continuas = spear[sapply(spear, is.numeric)]
hist.variables = hist(unlist(var_continuas), 
                               main = "Probabilidad de variables continuas",
                               xlab = "Valor", 
                               prob = TRUE,
                      col = "brown")

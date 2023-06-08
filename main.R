################################################################################
#
# AJUSTE DE LAS RESPUESTAS DEL TEST MINIST A UN MODELO DE TEORÍA DE RESPUESTA 
# AL ÍTEM.
#
# Autor: Borja Artiñano Arizmendi
# Fecha: 30/05/2023
# 
################################################################################

# Cargar librerías a emplear

library(openxlsx)
library(stringr)
library(lavaan)
library(psych)
library(tidyverse)
library(mirt)
library(difR)
library(plink)
library(PerFit)
library(catR)

# Borrar elementos de la última sesión.
rm(list=ls())

####
# 1. LECTURA DE DATOS Y PREPARACIÓN DE DATOS
####

datos_l = readRDS("datalist.RDS")

# Vamos a trabajar con los cuadernillos 1 y 2 del test, con las respuestas de 
# los participantes estadounidenses y canadienses.

datos = datos_l[[3]]  # Datos de los particpantes estadounidenses y canadienses.
names(datos) = tolower(names(datos)) # Pasamos a lowercase los nombres de las variables.

table(datos$idcntry, datos$idbook) # sujetos por país y cuadernillo

otras_var = str_subset(names(datos),"^m0",negate=TRUE) # variables que NO son los ítems
nombre_items = str_subset(names(datos),"^m0") # los ítems
nombre_items_rec = str_subset(nombre_items,"_r$") # los ítems recodificados

# Valores perdidos

datos$r_validas = rowSums(!is.na(datos[, nombre_items_rec])) 
table(datos$r_validas) # Frecuencias de respuestas válidas

apply(datos[1:6, ], 1, function (x) sum(is.na(x))) # Número de datos perdidos para
                                                   # los 6 primeros sujetos

# Hay muchos valores perdidos por sujeto. Esto es porque actualmente 
# las respuestas al primer y segundo cuadernillo están en la misma base, pero cada 
# sujeto solo ha respondido a uno de ellos.

datos = datos[datos$r_validas != 0, ] # Eliminar sujetos que no responden a nada
                                      # al no tener información sobre ellos.

# Cargar fichero con las características de los ítems

test = read.xlsx("T11_G8_ItemInformation.xlsx") 

# Crear una variable con los ítems recodificados

test$Item.ID_rec = paste(test$Item.ID, "_r", sep = "") 

# Seleccionamos el cuadernillo 1, compuesto por los bloques M01 y M02

test_M01_M02 = test[which(test$Block == "M01" | test$Block == "M02"), ]
test_M01_M02$Item.ID_rec[which(!test_M01_M02$Item.ID_rec %in% names(datos))] # Comprobar que solo se han guardado los ítems correctos

# Frecuencias de respuestas:
    # 8 -> ítem no alcanzado
    # 9 -> omisión

response.frequencies(datos[datos$idbook == "1", test_M01_M02$Item.ID_rec])

# Recodificar las omisiones y los ítems no alcanzados.
# Se van a tratar los ítems no alcanzados como valor perdido, y las omisiones como fallo.

datos[, nombre_items_rec][datos[, nombre_items_rec] == 9] = 0
datos[, nombre_items_rec][datos[, nombre_items_rec] == 8] = NA

# Seleccionar personas que han respondido al primer cuadernillo.

x1 = datos[datos$idbook == "1", ]

####
# 2. ANÁLISIS PSICOMÉTRICO CLÁSICO
####

cuadernillo1 = test_M01_M02$Item.ID_rec # Nombre de los ítems del cuadernillo 1
key1 = test_M01_M02$Key # opción correcta 
respuesta_r_1 = x1[, cuadernillo1] # respuestas de los sujetos al cuadernillo 1

# Mostrar alfa de Cronbach y parámetros de los ítems según la TCT.

analisis_clasico = psych::alpha(respuesta_r_1, keys = key1); analisis_clasico

####
# 3. ANÁLISIS DE LA UNIDIMENSIONALIDAD
####

# Análisis Paralelo
analisis_p = fa.parallel.poly(respuesta_r_1, 
                 fa = "pc",
                 sim = F,
                 correct = F)

analisis_p$ncomp # Número de componentes: 2
analisis_p$pc.values # Autovalores empíricos
analisis_p$pc.sim # Autovalores simulados

analisis_p$pc.values[1] / analisis_p$pc.values[2] # ratio entre el primer y segundo autovalor

# Análisis Factorial Confirmatorio de 1 factor
modelo = paste("F1 =~ NA*", 
                paste(cuadernillo1,collapse="+"),
               "\nF1 ~~ 1*F1",sep="") # se fija la varianza del factor a 1.

cfa_1 = lavaan(data = respuesta_r_1,
               model = modelo,
               ordered = cuadernillo1,)

summary(cfa_1, 
        fit.measures = T,
        standardized = T,
        rsq = T)

# Índices de ajuste para el AFC:

# p de chi cuadrado robusto == 0
# RMSEA robust == 0.037
# CFI robust == 0.96
# TLI robust == 0.96

# Pesos:
sort(inspect(object = cfa_1, what = "std")$lambda) # Hay dos ítems con peso < 0.3

# Proporción de varianza explicada:
sum(inspect(cfa_1, what = "rsquare")) / length(cuadernillo1) # 0.34

# Evaluación de los residuos:
residuos_cfa1 = resid(cfa_1)$cov
residuos_cfa1[upper.tri(residuos_cfa1)] = NA
inds = which(!is.na(residuos_cfa1), arr.ind =TRUE);inds
Tablaresiduos  = as.data.frame(list(var1    = rownames(residuos_cfa1)[inds[,1]],
                                     var2    = colnames(residuos_cfa1)[inds[,2]],
                                     resid   = round(residuos_cfa1[inds],2)))

print(Tablaresiduos[abs(Tablaresiduos[, 3]) > 0.1,]) # Parejas con residuos mayores 
                                                     # de 0.1 en valor absoluto
####
# 4. ESTIMACIÓN DEL MODELO TRI
####

# Se prueban dos modelos TRI, primero general, y a continuación otro más simple.
# Finalmente, se decide que el ajuste del modelo general es significativamente mejor.

####################################################################################
#
# Modelo 1:
# Modelamos los ítems de respuesta seleccionada con el ML3P, los de respuesta construida
# con el ML2P (porque no esperamos aciertos por pseudoazar), y los politómicos según el 
# modelo de Respuesta Graduada.
#
###################################################################################

# Definir tipo de ítem:

modelo_TRI1 = rep("3PL", ncol(respuesta_r_1))
modelo_TRI1[which(test_M01_M02$ItemType == "CR")] = "2PL"
modelo_TRI1[which(test_M01_M02$Maximum.Points == 2)] = "graded"

# Información de cada ítem:

data.frame(i = 1:nrow(test_M01_M02),
           f = round(response.frequencies(respuesta_r_1), 3),
           modelo = modelo_TRI1,
           key = test_M01_M02$Key) 

# Distribución a priori para la estimación de g/c:

mod1 = "F1 = 1-26
PRIOR = (1-2,9-12,15-18,22,24, g, norm,-1.099, 1)"

# Estimación de los parámetros:
TRI_fit1 = mirt(data = respuesta_r_1,
              model = mod1, 
              itemtype = modelo_TRI1,
              SE = T)

parametros = coef(TRI_fit1, simplify = T, IRTpars = T)$items 
parametros = apply(parametros, 2, function(x) round(x, 2)) # Parámetros de los ítems

################################################################################
#
# Modelo 2: Similar al anterior, pero los ítems de respuesta seleccionada se 
# modelan con el ML2P.
#
################################################################################

# Definir tipo de ítem:

modelo_TRI2 = rep("2PL", ncol(respuesta_r_1))
modelo_TRI2[which(test_M01_M02$Maximum.Points == 2)] = "graded"

# Información de cada ítem organizada según el modelo a aplicar:

data.frame(i = 1:nrow(test_M01_M02),
           f = round(response.frequencies(respuesta_r_1), 3),
           modelo = modelo_TRI2,
           key = test_M01_M02$Key) 

# Estimación de los parámetros:

TRI_fit2 = mirt(data = respuesta_r_1,
                model = 1, 
                itemtype = modelo_TRI2,
                SE = T)

coef(TRI_fit2, simplify = T, IRTpars = T)$items # Parámetros de los ítems

# Comparación de modelos:
anova(TRI_fit2, TRI_fit1) # Obtenemos que el desajuste aumenta significativamente 
                          # al emplear el modelo más simple. Por ello, mantenemos
                          # el M3PL para los ítems de respuesta seleccionada.
                        
# Para estudiar los parámetros de un ítem, vamos a representarlo gráficamente.

# Curva Característica de Respuesta del ítem 4.
itemplot(TRI_fit1, 4, type = "trace")

# Curva Característica Operante del ítem 4.
itemplot(TRI_fit1, 4, type = "threshold")

####
# 5. ESTUDIO DE LA INDEPENDENCIA LOCAL
####

# Se emplean los estadísticos chi cuadrado de Chen y Thissen, la versión 
# estandarizada, y la transformación en V de Cramer.

LDX2 = residuals(TRI_fit1, type = "LD",df.p = TRUE, table = FALSE)

# Extraer grados de libertad:
gl = LDX2$df.p
gl[upper.tri(gl)] = NA

# Extraer niveles de significación:
p = t(LDX2$df.p)
p[upper.tri(p)] = NA

# Obtener niveles de significación ajustados:
adjusted_p = p.adjust(as.numeric(p)[!is.na(as.numeric(p))], method = "BH")

# Extraer el estadístico de Chen y Thissen:
chi = abs(LDX2$LD)
chi[upper.tri(chi)] = NA

# Extraer la V de Cramer:
Vcramer = t(LDX2$LD)
Vcramer[upper.tri(Vcramer)] = NA

# Exraer el estadístico estandarizado y sus índices
SDLX2 = (chi-gl)/sqrt(2*gl)
inds = which(!is.na(SDLX2), arr.ind =TRUE)


# Organizar todo en una tabla:

dependencia_local = as.data.frame(list(item1 = rownames(SDLX2)[inds[,1]],
                      item2    = colnames(SDLX2)[inds[,2]],
                      chi     = round(chi[inds],2),
                      gl      = round(gl[inds],0),
                      p       = round(p[inds],5),
                      adjusted_p = round(adjusted_p,5),
                      SDLX2   = round(SDLX2[inds],2),
                      Vcramer = round(Vcramer[inds],2)))


dependencia_local = dependencia_local[order(dependencia_local$SDLX2,decreasing =TRUE),]

# Número de parejas con dependencia local según X2 estandarizado de Chen y Thiseen:
sum(dependencia_local$SDLX2 >= 10) # 6
dependencia_local[dependencia_local$SDLX2 >= 10, ]

# Según V de Cramer (punto de corte = 0.2):
sum(dependencia_local$Vcramer >= 0.2) # 2 
dependencia_local[dependencia_local$Vcramer >= 0.2, ]

# Según valor p ajustado (nivel de confianza 0.95):
sum(dependencia_local$adjusted.p < 0.05) # 0

####
# 6. ESTUDIO DEL AJUSTE ENTRE CCIs
####

# Número de personas con ítems no alcanzados:
sum(apply(respuesta_r_1, 1, is.na)) # 529

# Recodificar ítems no alcanzados como fallos:
datos_copia = datos
datos_copia[, nombre_items_rec][is.na(datos_copia[, nombre_items_rec])] = 0

x1_copia = datos_copia[datos_copia$idbook == "1", ]
respuesta_r_1_copia = x1_copia[, cuadernillo1]

# Comprobar que no hay valores perdidos en la copia:
sum(apply(respuesta_r_1_copia, 1, is.na)) # 0

# Crear modelo TRI sin valores perdidos:
TRI_fit1_copia = mirt(data = respuesta_r_1_copia,
                 model = mod1, 
                 itemtype = modelo_TRI1,
                 SE = T)

# X2 de Orlando y Thissen 
CCI_X2 = itemfit(TRI_fit1_copia, fit_stats = "S_X2")
CCI_X2$adjusted.p = p.adjust(CCI_X2$p.S_X2, method = "BH")
CCI_X2[order(CCI_X2$adjusted.p),][1:10,] # mostrar 10 ítems con mayor desajuste

# Gráfico del ítem con mayor desajuste
itemfit(TRI_fit1_copia, fit_stats = c("S_X2"), empirical.plot = 8, empirical.CI = 0.95)

####
# 7. ESTUDIO DEL AJUSTE GLOBAL
####

M2(TRI_fit1, calcNull = T, CI = 0.9, na.rm = T)

####
# 8. ESTIMAR NIVEL DE RASGO por MAP
####

theta_se = fscores(TRI_fit1, 
                method = "MAP",
                full.scores = T,
                full.scores.SE = T)

ks.test(x = theta_se[,1], y = "pnorm") # La distribución no es normal p = 0.033
describe(theta_se[, 1]) # Descriptivos: curtosis < 7 y asimetría < 2.

# Gráfico
hist(theta_se[, 1], density = 20, breaks = seq(-3, 3, 0.1), prob = T,
     xlab = expression(theta), ylab = "", xlim = c(-3, 3), main =
         "", yaxt = "n", col.main = "blue")

lines(density(theta_se[, 1]), lwd = 2, col = "brown") # Densidad de la función
curve(dnorm(x, mean=mean(theta_se[,1]), sd=sd(theta_se[,1])), col="darkblue", lwd=2, add=TRUE) # Función de densidad normal

# Aunque la distribución de las puntuaciones no sea estrictamente normal, no parece 
# la violación de la normalidad sea grave.

# Correlación entre theta y las puntuaciones directas
punt_directa = x1[, cuadernillo1]
punt_directa_suma = rowSums(punt_directa)
cor(theta_se[, 1], punt_directa_suma, use = "complete.obs") # 0.98

# Gráfico de correlación entre la puntuación diecta y theta
plot(theta_se[, 1], punt_directa_suma,
     xlab = expression(theta),
     ylab = "Puntuación Directa",
     cex = 0.5,
     col = "blue")
abline(lm(punt_directa_suma ~ theta_se[, 1]), col = "darkgreen", lwd = 2)

####
# 9. INFORMACIÓN DEL TEST
####

# Gráfico de información y SE básico
plot(TRI_fit1, type="infoSE",theta_lim=c(-4,4))

# Gráfico de fiabilidad básico
plot(TRI_fit1, type="rxx",theta_lim=c(-4,4))

# Gráfico de la información y el error típico estético:
# Información y error típico para theta entre (-4,4)
z  = seq(-4,4,.1)
Info_test = testinfo(x=TRI_fit1, Theta=z); 
SE = 1/sqrt(Info_test) 

# Incluirlo todo en un dataframe
info = as.data.frame(list(z=z,
                           Info=Info_test,
                           SE = SE, 
                           rxx = 1/(1+SE^2)))

round(info,3) # Tabla con la información y el error típico

op = par(mar = c(5,5,5,5)) # Cambiar márgenes del gráfico

# Pintar curva de información:
Infoplot= plot(z,Info_test,
                xlim=c(min(z),max(z)), ylim=c(0,26),
                axes=FALSE,
                xlab = expression(theta),
                ylab="",
                type="l",col="darkblue",lwd=2, 
                las=1,
                main="Información y error típico",
                col.main = "blue")

axis(side = 1, at=seq(-4,4,0.5),labels=seq(-4,4,0.5)) # Añadir eje X
axis(side = 2, at=seq(0,25,5),labels=seq(0,25,5),las=1) # Añadir eje Y
mtext(side =2, line=2.5,expression(paste("I(",theta,")",sep="")),las=1) # Título eje Y
par(new = T) # Indica que el próximo gráfico se superponga al actual

col=rep("green", length(z)) # La fiabilidad por encima de 0.8 se marca en verde. 
                            # Valor por defecto.
rxx8 = sqrt((1-0.8)/0.8) # Error típico para fiabilidad = 0.8
col[SE > rxx8] = "orange" # La fiabilidad por debajo de 0.8 se marca en naranja
rxx7 = sqrt((1-0.7)/0.7) # Error típico para fiabilidad = 0.7
col[SE > rxx7] = "red" # La fiabilidad por debajo de 0.7 se marca en rojo

# Añadir curva con el SE, por colores según la fiabilidad:
with(Infoplot, plot(z, SE, ylim=c(0,1),
                    pch=16, axes=F, 
                    col=col,
                    xlab=NA, 
                    ylab=NA, 
                    cex=0.5,
                    type = "p"))

axis(side = 4, at=seq(0,1,.1),labels=seq(0,1,.1),las=1) # Eje Y2
box() # Incluir el gráfico en una caja
mtext(side = 4, line=2.5, expression(paste("SE(",theta,")",sep="")),las=1) # Título eje Y2

# Fiabilida mayor de 0.8 en:
range(z[which(SE < sqrt((1-0.8)/0.8))]) # (-1.2, 2.5)

# Fiabilidad empírica de las puntuaciones
empirical_rxx(theta_se)

####
# 10. ANÁLISIS DEL FUNCIONAMIENTO DIFERENCIAL
####

# Dicotomizar ítems politómicos para estudiar su DIF
datos_dico = x1[, cuadernillo1]
datos_dico[datos_dico==2] = 1 # Los valores 2 pasan a ser 1.
punt_directa = apply(datos_dico, 1, sum) # Calcular puntuación directa
grupo = x1$idcntry # Variable que señala el grupo de pertenencia.
J = length(cuadernillo1) # Número de ítems

# Descriptivos y contraste de las diferencias en puntuación directa
datos_dico[is.na(datos_dico)] = 0 # se tratan las omisiones como fallos para poder facilitar los contrastes estadísticos
describeBy(punt_directa, group = grupo, mat = T) # Descriptivos.
effsize::cohen.d(punt_directa,as.factor(grupo)) # Diferencia = 0.15
t.test(punt_directa~grupo) # p = 0.0015

###
# 10. 1 Test de Razón de Verosimilitudes
###
# Paso 1: usar todo el test como anclaje
test_anclaje = cuadernillo1

grupo[grupo == "United States"] = 1 # Establecer estadounidenses como grupo de referencia.
grupo[grupo == "Canada"] = 2 # Establecer canadienses como grupo de focal.
datos_dico = datos_dico[order(grupo), ] # Organizar respuestas por grupo
grupo = grupo[order(grupo)] # Organizar participantes por grupo

# Calcular modelo en el que todos los ítems son invariantes, y estimar media y varianza
# para cada grupo.
m1 = multipleGroup(datos_dico, model = 1, group = grupo, 
                   invariance = c("free_means", "free_var", test_anclaje))

# Parámetros del grupo 1
par1.g1 = coef(object = extract.group(m1,1),
            simplify = TRUE,
            IRTpars = TRUE)$items[,c("a","b")]

# Parámetros del grupo 2
par1.g2 = coef(object = extract.group(m1,2),
            simplify = TRUE,
            IRTpars = TRUE)$items[,c("a","b")]

# Medias y varianzas de los grupos
media1.var1.g1 = coef(object = extract.group(m1,1))$GroupPars; media1.var1.g1
media1.var1.g2 = coef(object = extract.group(m1,2))$GroupPars; media1.var1.g2

# Se ve que la media del grupo canadiense es ligeramente superior.

# Calcular DIF para cada ítem
m1_libre = DIF(MGmodel = m1, 
            which.par = c('a1', 'd','d1','d2'), 
            scheme = 'drop',
            p.adjust="BH"); m1_libre


# Paso 2: Evaluar DIF con test de anclaje formado por los 5 ítems más discriminativos sin DIF
test_anclaje = par1.g1[which(m1_libre$adj_pvals>.05),] # Seleccionar ítems sin DIF en el primer paso.
mayor_a = order(test_anclaje[,1],decreasing=TRUE)[1:5] # Seleccionar los más discriminativos.

test_anclaje  = rownames(test_anclaje)[mayor_a] # Asignar ítems al test de anclaje.
index_anclaje = which((names(datos_dico) %in% test_anclaje)) # IDs de los ítems de anclaje en la BD
index_sospechosos = which(!(names(datos_dico) %in% test_anclaje)) # IDs de los ítems libres en la BD

# Calcular modelo en el que solo es invariante el nuevo test de anclaje.
m2 = multipleGroup(datos_dico, model = 1, group = grupo, 
                   invariance = c("free_means", "free_var", test_anclaje))

# Parámetros del grupo 1
par2.g1 = coef(object = extract.group(m2,1),
              simplify = TRUE,
              IRTpars = TRUE)$items[,c("a","b")]

# Parámetros del grupo 2
par2.g2 = coef(object = extract.group(m2,2),
              simplify = TRUE,
              IRTpars = TRUE)$items[,c("a","b")]; par2.g2

# Medias y varianzas de ambos grupos
media2.var2.g1 = coef(object = extract.group(m2,1))$GroupPars; media2.var2.g1
media2.var2.g2 = coef(object = extract.group(m2,2))$GroupPars; media2.var2.g2

# Organizar parámetros para ambos grupos en una tabla.
par2 = as.data.frame(cbind(par2.g1,par2.g2))
names(par2) = c("a.g1","b.g1","a.g2","b.g2")

# Comprobar DIF en los ítems sospechosos
m2_igual = DIF(MGmodel = m2, 
               which.par = c('a1', 'd'), 
               scheme = 'add',
               p.adjust="BH",
               items2test = index_sospechosos)

# Comprobar DIF en los ítems de anclaje
m2_libre = DIF(MGmodel = m2,
               which.par = c('a1', 'd'), 
               scheme = 'drop',
               p.adjust="BH",
               items2test = index_anclaje)

# Organizar información:
m2_conjunto = rbind(m2_igual, m2_libre) # Unir ítems sospechosos y de anclaje
m2_conjunto$adj_pvals = p.adjust(m2_conjunto$p,"BH") # Ajustar p. El ajuste de la
                                                # función DIF ya no vale, porque
                                                # se hacen más comparaciones.

itemsDIF = rownames(m2_conjunto)[m2_conjunto$adj_pvals<= .05] # Ítems con DIF
items_NoDIF = rownames(m2_conjunto)[m2_conjunto$adj_pvals > .05] # Ítems sin DIF

# Tabla final
TablaDIF.LRT = as.data.frame(list(chi = round(m2_conjunto$X2,3),
                                   df  = round(m2_conjunto$df,0),
                                   p = round(m2_conjunto$adj_pvals,3),
                                   tipo = ifelse(m2_conjunto$adj_pvals > .05,"NO DIF","DIF")),
                              row.names = names(datos_dico)); TablaDIF.LRT

# Modelo final, en el que seleccionamos como anclaje los ítems que no han tenido
# DIF en el paso 2.
m_final = multipleGroup(data = datos_dico,
                        model = 1,
                        group = grupo,
                        invariance = c('free_means', 'free_var', items_NoDIF))

# Representación gráfica de los ítems con DIF. 1: estadounidenses
                                             # 2: canadienses
plot(x=m_final,type="trace",
     which.items=which(TablaDIF.LRT$tipo=="DIF"),
     facet_items=TRUE)

# Medidas de tamaño del efecto
LRT.TE =  empirical_ES(m_final, DIF=TRUE,ref.group = 1)

# Seleccionar SIDS, UIDS y ESSD, y medias del ítem por grupo
selec = c("SIDS","UIDS","ESSD","mean.ES.foc", "mean.ES.ref")
TablaDIF.LRT[,selec] = round(LRT.TE[,selec],3) # Añadir a la tabla anterior

# Clasificar según NO DIF, DIF unidireccional (udif) y DIF no unidireccional (nudif)
TablaDIF.LRT$tipo = ifelse(TablaDIF.LRT$tipo =="NO DIF", "NO DIF",
                            ifelse((abs(TablaDIF.LRT$SIDS)==TablaDIF.LRT$UIDS), "uDIF",
                            "nuDIF"))
# Tamaño del efecto: 
    # " " -> Inexistente
    # "A" -> pequeño 
    # "B" -> mediano 
    # "C" -> grande
TablaDIF.LRT$size = ifelse(abs(TablaDIF.LRT$ESSD)  < .2," ",
                            ifelse(abs(TablaDIF.LRT$ESSD) < .5, "A",
                            ifelse(abs(TablaDIF.LRT$ESSD) < .8,"B", "C")))

# ¿Qué grupo es favorecido? F -> Focal (Canadá), R -> Referencia (EEUU)
TablaDIF.LRT$favor = ifelse(TablaDIF.LRT$mean.ES.foc == TablaDIF.LRT$mean.ES.ref, " ",
                             ifelse(TablaDIF.LRT$mean.ES.foc > TablaDIF.LRT$mean.ES.ref, "F", "R"))

# Podemos ver el tipo de DIF, el tamaño del efecto, y a quién favorece
TablaDIF.LRT[order(abs(TablaDIF.LRT$ESSD),decreasing="TRUE"),] 

###
# 10.2 SIBTEST
###

# Tratar ítems no alcanzados como fallos:
datos_dico[is.na(datos_dico)] = 0

# Calcular sibtest:
sibtest = difSIBTEST(datos_dico, grupo, "2", "udif", purify = T, nrIter = 20, 
                     p.adjust.method = "BH")
plot(sibtest) # Ítems con DIF (puede haber efecto de cancelación)
sibtest$DIFitems

# Calcular csibtest
csibtest = difSIBTEST(datos_dico, grupo, "2", "nudif", purify = T, nrIter = 20, p.adjust.method = "BH")
plot(csibtest) # Ítems con DIF (elimina el efecto de cancelación)
csibtest$DIFitems

# Organizar información
TablaSIBTEST = as.data.frame(round(cbind(cbind(sibtest$Beta,
                                               sibtest$X2,
                                               sibtest$df,
                                               sibtest$adjusted.p,
                                               csibtest$Beta,
                                               csibtest$X2,
                                               csibtest$df,
                                               csibtest$adjusted.p)),3))
# Cambiar nombres a la tabla
names(TablaSIBTEST) = c("beta1","X2_1","df_1_","adjusted.p1","beta2","X2_2","df_2","adjusted.p2")

# Distinguir entre: no DIF, uDIF y nuDIF
TablaSIBTEST[,"tipo"] = ifelse((TablaSIBTEST$adjusted.p1 > .05 & 
                                  TablaSIBTEST$adjusted.p2 > .05), "no DIF",
                                  ifelse((abs(TablaSIBTEST$beta1)==TablaSIBTEST$beta2),
                                  "uDIF", "nuDIF"))

# Si la beta calculada es > 0.1, se debe revisar el ítem
TablaSIBTEST$size = ifelse(TablaSIBTEST[,"tipo"] == "no DIF", " ",
                            ifelse((TablaSIBTEST$beta2 >= .1), "Revisar", "Ignorar"))
# ¿A quién favorece el DIF?
TablaSIBTEST$favor  = ifelse(TablaSIBTEST$tipo == "no DIF","",
                       ifelse(sign(TablaSIBTEST$beta1)==1,"R","F"))
TablaSIBTEST

# Impacto de los ítems con DIF en el test final
empirical_ES(m_final, DIF=FALSE,ref.group = 1,plot=T) # incluye el gráfico
empirical_ES(m_final, DIF=FALSE,ref.group = 1,plot=F) # no incluye el gráfico

# ïtems con DIF según SIBTEST
items_DIF_SIBTEST = names(datos_dico)[which(TablaSIBTEST$adjusted.p1<.05 | TablaSIBTEST$adjusted.p2<.05)]

# Ver si el DIF produce DTF
SIBTEST(dat= datos_dico, 
        group = grupo,
        suspect_set = which(names(datos_dico) %in% items_DIF_SIBTEST))

# Como CSIBTEST tiene una p = 0.054, mantenemos la H0 de que no existe DTF.        

####
# 11. DETECCIÓN DE PATRONES ABERRANTES
####

# Trabajamos con datos dicotomizados, ya que las funciones que permiten emplear 
# datos politómicos exigen que todos los ítems tengan las mismas categorías.

# Lz*
lzstar = lzstar(matrix = datos_dico, Ability.PModel = "WL")
scores_lzstar = lzstar$PFscores

set.seed(123)
PerFit:::plot.PerFit(lzstar, Nreps=10000, Blvl=.05,Breps=1000) # cuttoff recomendado de -1.71. 
                                                    # Porcentaje de patrones aberrantes de 4.74%. 

PerFit:::plot.PerFit(lzstar, UDlvl=-1.64) # Con el criterio teórico: 5.23% de patrones aberantes

# Ht
Ht = Ht(datos_dico)
scores_ht = Ht$PFscores   
set.seed(123)
PerFit:::plot.PerFit(Ht, Nreps=10000, Blvl=.05,Breps=1000) # cuttoff de 0.212, 
                                        # con un 6.15 % de patrones aberrantes.

PerFit:::plot.PerFit(Ht, UDlvl=.22) # El criterio teórico detecta un 7.01 % de patrones aberrantes.

# U3
U3 = PerFit::U3(datos_dico)
scores_U3 = U3$PFscores
set.seed(123)
PerFit:::plot.PerFit(U3, Nreps=10000, Breps=1000,Blvl=.05) #cuttoff de 0.345, 
                                          # con un 5.61 % de patrones aberrantes
PerFit:::plot.PerFit(U3, UDlvl=.25) # Criterio teórico: 21.73 % de patrones aberrantes

# Correlación entre los 3 estadísticos:
cor(data.frame(scores_lzstar$PFscores, scores_ht$PFscores, scores_U3$PFscores), 
    use = "pairwise.complete.obs")

####
# 12. EQUIPARACIÓN CON EL SEGUNDO CUADERNILLO
####

# Vamos a equiparar las puntuaciones obtenidas en el segundo cuadernillo a la 
# métrica del primer cuadernillo. Podemos hacer esto gracias al bloque M02, que funciona como test
# de anclaje.

# Seleccionamos el cuadernillo 2, compuesto por los bloques M02 y M03
test_M02_M03 = test[which(test$Block == "M02" | test$Block == "M03"), ]
cuadernillo2 = test_M02_M03$Item.ID_rec # Nombres de los ítems
x2 = datos[datos$idbook == "2", ] # Personas que responden al segundo cuadernillo.
respuesta_r_2 = x2[, cuadernillo2] # Respuestas al segundo cuadernillo.

datos_dico1 = datos_dico # Respuestas dico del primer cuaderno.
datos_dico2 = respuesta_r_2 # Copiar respuestas al segundo cuaderno a nueva variable.
datos_dico2[datos_dico2 == 2] = 1 # Respuestas dico del segundo cuaderno.

# Fiabilidad de cada cuadernillo
psych::alpha(datos_dico1)$total$raw_alpha # Cuadernillo 1: 0.84
psych::alpha(datos_dico2)$total$raw_alpha # Cuadernillo 2: 0.86

# Calibración de parámetros en cada cuadernillo con datos dicotómicos
tipoitem1 = test_M01_M02$ItemType # tipo de ítem 1er cuaderno.
model1 = rep("3PL",ncol(datos_dico1)) 
model1[which(tipoitem1 == "CR")]="2PL" # Los ítems de respuesta construida se 
                                       # modelan con el 2PL, el resto con el 3PL.

tipoitem2 = test_M02_M03$ItemType # tipo de ítem 2do cuaderno.
model2 = rep("3PL",ncol(datos_dico2))
model2[which(tipoitem2=="CR")]="2PL" # Los ítems de respuesta construida se 
                                     # modelan con el 2PL, el resto con el 3PL.

# Se establecen distribuciones a priori para el parámetro c/g, para evitar 
# valores extremos.

ncol(datos_dico1) # Número de ítems 1er cuaderno.
which(tipoitem1=="MC") # Índices de ítems a modelar con 3PL.

mod1 = "F1 = 1-26
PRIOR = (1, 2, 9-12,15-18,22,24, g, norm, -1.099, 1.0)" # Distribución a priori para c.

# Se repite el proceso para el segundo cuadernillo:

ncol(datos_dico2) 
which(tipoitem2=="MC") 

mod2 = "F1 = 1-32
PRIOR = (1,4-7,11,13,16-17,20,25,28-31, g, norm, -1.099, 1.0)"

# Ajustar datos al modelo TRI:
resY = mirt(datos_dico1, model = mod1, 
            itemtype = model1,
            technical=list(NCYCLES=50000)) # Cuaderno 1 o Y

resX  = mirt(data = datos_dico2, 
              model = mod2, 
              itemtype = model2,
              technical=list(NCYCLES=50000)) # Cuaderno 2 o X

parY = coef(resY,simplify=TRUE,IRTpars=TRUE)$items[,c("a","b","g")] # Parámetros Y
parX = coef(resX,simplify=TRUE,IRTpars=TRUE)$items[,c("a","b","g")] # Parámetros X

# Cambiamos los nombres de los objetos creados y los unimos en una lista

colnames(parY) = c("a","b","c")
colnames(parX) = c("a","b","c")

par = list(parY,parX[1:26,]) # Se seleccionan solo los primeros 26 ítems de X,
                             # para que ambos test tengan la misma longitud

# Estos parámetros se deben transformar a la clase "irt.pars", para realziar la 
# equiparación. Para ello, antes es necesario preparar varios argumentos:

# Número de categorías (2) en cada ítem.
catY = rep(2,ncol(datos_dico1)) # Cuaderno 1          
catX = rep(2,ncol(datos_dico2)) # Cuaderno 2          
cat  = list(catY,catX[1:nrow(parY)]) # unir en una lista 

# Especificar mdoelo asociado al ítem. Se emplea "drm" -> dichotomous response.
modY  = as.poly.mod(nrow(parY),"drm") # Cuaderno 1
modX  = as.poly.mod(nrow(parY),"drm") # Cuaderno 2
mod  = list(modY,modX) # Unir en una lista

# Posiciones de los ítems de anclaje:
anclaje =intersect(names(datos_dico1),names(datos_dico2)[1:nrow(parY)]) # Ítems de anclaje
posi1 = match(anclaje,names(datos_dico1)) # En el Cuadernillo 1
posi2 = match(anclaje,names(datos_dico2)) # En el cuadernillo 2
common = matrix(c(posi1,posi2),length(anclaje),2) # Unir en una lista  

# Transformar parámetros a la clase "irt.pars". 
EQUIP = as.irt.pars(x = par,             
                     cat = cat,             
                     poly.mod = mod,             
                     common = common,          
                     grp.names =  c("Y" , "X"))   

# Calcular constantes de equiparación
res = plink(x = EQUIP, base.grp = 1)
summary(res) # Constantes obtenidas por cada método.

# Equiparación por el método de Haebara
haebara = plink(x=EQUIP,
                 rescale="HB",
                 base.grp=1); 

# Parámetros equiparados
params_Y =link.pars(haebara)$Y # Parámetros del cuadernillo 1
params_X =link.pars(haebara)$X # Parámetros del cuadernillo 2

# Equiparación de puntuaciones verdaderas
pesos = as.weight(81, quadrature = T, mu = 0, sd = 1)
eq = plink:::equate(x=haebara,
                    method=c("TSE"),
                    base.grp=2, # Puntuaciones a transformar    
                    weights1=weight, # pesos del grupo X
                    D=1)

eq$theta[which(abs(eq$theta) > 4)] = NA # Eliminar valores extremos

# Equiparación de puntuaciones
names(eq)[2:3] = c("Puntuación en X", "Puntuación en Y")
eq[, 2:3] # Puntuaciones equiparadas

# Representación gráfica de las CCT
plot(eq[,c(1,2)],type="l",
     ylim=c(0,32),
     xlab=expression(theta),
     ylab="Puntuación Esperada",
     main= "Curva Característica del Test",
     col.main="blue") #X
lines(eq[,c(1,3)],type="l",col="red") #Y
legend(x=-2,y=30,legend=c("test X","test Y"),
       col=c("black", "red"), lty=1:2, cex=0.8)

####
# 13. CONSTRUCCIÓN DE UN TEST ADAPTATIVO INFORMATIZADO
####

nombres = c(names(datos_dico1), names(datos_dico2))
nombres = nombres[!duplicated(nombres)] # Nombres de los ítems que formarán el banco

datos_dico_12 = dplyr::bind_rows(datos_dico1, datos_dico2) # Respuestas en los dos cuadernillos
naniar::vis_miss(datos_dico_12)

# Calibración concurrente con los dos cuadernillos
modelo_completo = mirt(datos_dico_12, 1)
params = coef(modelo_completo, IRTpars = T, simplify = T)$items

# Especificaciones del TAI
# Puesta en marcha
inicio = list(theta = round(runif(1, -0.5, 0.5), 2), startSelect = "bOpt", randomesque = 5)
test = list(method = "ML", itemSelect = "bOpt")
parada = list(rule = "length", thr = 10)
final = list(method = "ML")

# Simulación del TAI
set.seed(123)
z = rnorm(1000, 0, 1) # Thetas simuladas
I = length(z)
respuestas_simuladas = genPattern(z, params) # Simular 

# TAI: se simulan las respuestas de unos sujetos simulados a un TAI. Esto permite 
# anticipar las propiedades del TAI en la población objetivo.
respondents = simulateRespondents(thetas = z,
                                        itemBank = params,
                                        start = inicio,
                                        test = test,
                                        stop = parada,
                                        final = final,
                                        rmax = 0.5,
                                        Mrmax = "restricted",
                                        responsesMatrix = respuestas_simuladas,
                                        save.output = T,
                                        output = c("", "catR", "txt"))

plot(respondents) # Gráficos informativos
print.catResult(respondents) # Propiedades del TAI
describe(respondents$final.values.df) # Thetas simuladas y estimadas

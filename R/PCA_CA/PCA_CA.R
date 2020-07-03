##Ayudantia PCA-CA-NMDS

##Los analisis de ordenacion de datos, son particularmente adaptados para
##analizar datos de comunidades ecologicas naturales, las cuales generalmente
##estan estructuradas en gradientes.

##El objetivo de los metodos de ordenacion, es representar la informacion a lo largo
##de un numero reducido de ejes ortogonales, construidos en una forma tal que representan
##de manera decreciente, las principales tendencias de la data.

##OJO!!! En este script exploramos 3 metodos (PCA, CA, NMDS) los cuales son descriptivos
##Es decir, no hay test estadisticos que nos permitan evaluar la significancia de las 
##estructuras detectadas.

##Metodos evaluados::

##PCA=Es el principal metodo que utiliza "eignvector". Trabaja con datos brutos y cuantitativos
##conserva las distancias euclideanas entre los sitios

##CA=Trabaja con datos de frecuencia, que son dimencionalmente homogeneos y no-negativos
##Conserva las distancias Chi2 entre filas o columnas. Se utiliza principalmente en ecologia
##para analizar tablas de datos de especies

##NMDS= A diferencia de los metodos anteriores no se basa en "eignvector". Este metodo trata de
##representar el set de los objetos a lo largo de un numero predeterminado de ejes mientras conserva
##las relaciones de orden entre ellos. Puede utilizar cualquier tipo de datos.


#####Ejercicio a realizar lo encuentran con toda la explicacion en Pag 117--Numerical Ecology with R--PCA

##La data a utilizar forma parte de una tesis doctoral, 
##que intenta usar las especies de peces para caracterizar
##zonas ecologicas a lo largo de los rios europeos. En la Tesis
##se encuentra que las comunidades de peces son buenos indicadores
##biologicos para caracterizar los cuerpos de agua.

##Esta base de datos consta de 3 matrices. La primera matriz, contiene
##informacion de abundancia para 27 especies de peces "DoubsSpe.csv". 

##La segunda matriz
##contiene 11 variables ambientales "DoubsEnv.csv".


##Y la tercera matriz contiene las coordenadas geograficas de los 30 sitios
##evaluados "DoubsSpa.csv". Esta base de datos no sera utilizada.

##Necesitamos los siguientes paquetes
library(ade4)
library(vegan)
library(gclus)
library(ape)

############################################################################################

#####Analisis de Componentes Principales (PCA)

##Es un analisis exclusivo para variables cuantitativas. Conserva las distancias Euclidianas y
##la relacion que detecta es lineal. Por lo tanto, no es adecuado para el analisis de abundancia
##de especies.

##OJO!!! Podemos realizar PCA en datos unimodales (abundancia de espcies) despues de hacer una
##pre-transformacion (Numerical Ecology with R pagina-128)

##1.- Fijar directorio de trabajo


##2.-Cargar la base de datos

env= read.csv("Camaras_GS_PCA.csv", row.names=1)
spe= read.csv("Species_bloque_GS.csv", row.names=1)

env= read.csv2("D:/PROJECTS/Gran Sabana 2018/Gran Sabana_junto/PCA/Camaras_GS.csv",row.names=1)
spe= read.csv2("Species_bloque_GS.csv", row.names=1)

##3.-Remover la fila que no tiene presencia de especies

spe= spe[-8,]
env= env[-8,]

##4.-Revisar las bases de datos env

summary(env)

##5.-Realizar un PCA sobre una matriz de correlacion.

##Para hacer PCA usamos el comando rda() del paquete vegan
##el nombre de la funcion se refiere al "analisis de redundancia"
##que se explica mas adelante.

##Utilizamos la base de datos "env" de variables ambientales
##Y con el analisis podemos contestar 
##(1) Como estan las variables correlacionadas (variables ambientales)
##(2)que podemos decir de la ordenacion de los sitios (rios)

env.pca=rda(env, scale=T)

##Poner scale = T, hace que las distintas variables se estandarizen

env.pca
summary(env.pca)  #scaling=2
summary(env.pca, scaling=1)

##Cuando no especificamos "scaling" (no confundir con scale)
##significa que los scores que obtenemos estan es scaling =2

##Es importante entender esto para cuando interpretamos los biplot

##Scaling=1, Las distancias de los objetos (rios) son aproximaciones de sus 
##distancias euclidianas en un espacio multidimencional. Los angulos entre
##los vectores no se pueden interpretar (variables ambientales)

##Scaling=2, Las distancias de los objetos (rios) NO son aproximaciones de sus 
##distancias euclidianas en un espacio multidimencional. Los angulos entre
##los vectores en el biplot representan su correlacion (variables ambientales)

##Por lo tanto, si nos interesa el analisis entre objetos (rios) elegimos
## scaling 1, si el interes principal es la relacion entre las variables 
##elegimos scaling 2.

##6.-Examinar y graficar los resultados del PCA

##7.-Eigenvalues

(ev=env.pca$CA$eig)

##8.-Aplicar el criterio de Kaiser-Guttman para seleccionar los ejes

ev[ev>mean(ev)]


##9.-Graficar la relacion

evplot = function(ev) {
  # Plot eigenvalues and % of variation for each axis
  op = par(mfrow=c(1,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  
}

evplot(ev)

##10.- biplots para PCA: scaling 1 y 2

##Usando biplot

par(mfrow=c(1,2))
biplot(env.pca, scaling=1, main="PCA-scaling 1")
biplot(env.pca, main="PCA-scaling 2")

##11.-Interpretacion de los biplots

##Primero: la proporcion de la varianza explicada en los primeros
##dos ejes es del 75%. Este alto valor hace que nuestra interpretacion
##de la data sea confiable

##Dos:En el biplot de Scaling 1, se muestra un gradiente que va de 
##izquierda a derecha, comenzando con un grupo formado por los sitios
##1-10, los cuales tienen los valores mas altos para alt, pen y los menores
##valores para deb, das y dur.

##El segundo grupo de sitios 11-16 tiene valores altos de oxy y bajos de nit
##Y un tercer grupo 17-22, muestra valores intermediosen casi todas las
##variables evaluadas, ya que no estan dispersos sobre los ejes.

##pho y amm como dbo muestran su maximo en sitios 23-25.
##Se ve un gradiente de sitios entre aquellos ricos en oxigeno a eutroficados


##Tres: En el biplot de Scaling 2, se muestra que las variables estan
##organizadas en grupos. La parte izq inferior del biplot muestra que alt y pen
##son muy altas y positivamente corralacionadas. Y estas variables estan negativamente
##correlacionadas con variables como distancia de la fuente (das).
##El contenido de oxigeno esta positivamente correlacionado con pendiente y altitud
##pero negativamente con pho y amm y dbo.

##La parte derecha del diagrama muestra variables asociadas con la parte baja del rio
##(eutroficacion).
##Ph y nit tienen flechas ortogonales una correlacion cercana a 0.
##El biplot scaling 2 es mas informativo que ver un cor(env)


##EXTRA##12.-Analisis de cluster para confirmar los grupos

par(mfrow=c(1,1))
env.clust=hclust(dist(scale(env)), "ward")

#Ward's minimum variance method aims at finding compact, spherical clusters

##Cortar dendrograma en 4 grupos

gr=cutree(env.clust, k=4)
grl=levels(factor(gr))

##Sacamos los score para sitios con scaling 1

sit.scl=scores(env.pca, display="wa", scaling=1)

#Realizamos el grafico

p=plot(env.pca, display="wa", scaling=1, type="n",
       main="PCA correlation-cluster")
abline(v=0, lty="dotted")
abline(h=0, lty="dotted")
for(i in 1:length(grl)){
  points(sit.scl[gr==i,], pch=(14+i),cex=2, col=i+1)
}
text(sit.scl, row.names(env), cex=.7, pos=3)

##13.-Como hacer PCA para datos de especies (Abundancia)
##Pagina 128 Numerical Ecology with R

##Utilizar una pre-transformacion Hellinger

spe.trans=decostand(spe, "hellinger")

##Realizar PCA

spe.trans.pca=rda(spe.trans)

##No se pone  scale = T porque la unidad de abundancia es unica

##ver resultados
spe.trans.pca
summary(spe.trans.pca)  #scaling=2
summary(spe.trans.pca, scaling=1)

##14.-Eigenvalues

(ev=spe.trans.pca$CA$eig)

##15.-Aplicar el criterio de Kaiser-Guttman para seleccionar los ejes

ev[ev>mean(ev)]


##16.-Graficar la relacion

evplot = function(ev) {
  # Plot eigenvalues and % of variation for each axis
  op = par(mfrow=c(1,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  
}

evplot(ev)

##17.- biplots para PCA: scaling 1 y 2

##Usando biplot

par(mfrow=c(1,2))
biplot(spe.trans.pca, scaling=1, main="PCA-scaling 1")
biplot(spe.trans.pca, main="PCA-scaling 2")

##Las especies no forman grupos tan claramente como las
##variables ambientales. Sin embargo, podemos ver como las especies
##se reemplazan a lo largo de la secuencia de sitios.

##Para comparar, pueden repetir el PCA con los datos originales de spe
##sin la transformacion. Y contestar que ordenacion muestra un mejor
##gradiente de especies a lo largo del curso del rio?

##18.-Evaluar la relacion entre las variables originales (especies)
##y cada PC, calculamos los coeficientes de determinacion R2

spe.trans.pca$CA

##Cada rda que hemos hecho tiene mucha mas informacion,
##despues hay "u" son los sitios del PCA, 
##"v" que son las variables (especies), 
##"u.eig" es lo mismo escalado por los eigenvalues y esto es lo que
##resulta de la descomposicion de la matriz en sus valores singulares.
##Si descompongo la matriz A=sumatoria U V.

##"rank"=cuantas variables tengo
##tot.chi=varianza total (inercia)
##xbar=Valores centrados (por los promedios de las variables)

##Ahora si queremos saber cuanta varianza de cada variable esta involucrada
##dentro de cada componente
##tenemos que seguir la formula R2(xvariable)=a2ij*lambdai/S2j. 

##a2ij

av1=spe.trans.pca$CA$v[,1]**2
av1

##landai

landa1=spe.trans.pca$CA$eig[1]
landa1
##a2ij*landai

mult=av1*landa1
mult

##S2j

var1=apply(spe.trans, 2, var)
var1

##R2(xvariable)=a2ij*landai/S2j

##Hay que hacer un paso mas
mult[1]/var1[1]
mult[2]/var1[2]
mult[1:28]/var1[1:28]

##Se repite para todas las especies y el PC2
##a2ij

av1=spe.trans.pca$CA$v[,2]**2
av1

##landai

landa1=spe.trans.pca$CA$eig[2]
landa1
##a2ij*landai

mult=av1*landa1
mult

##S2j

var1=apply(spe.trans, 2, var)
var1

##R2(xvariable)=a2ij*landai/S2j

##Hay que hacer un paso mas
mult[1]/var1[1]
mult[2]/var1[2]
mult[1:28]/var1[1:28]

##Este valor deberia dar entre 0 y 1 o entre 0 y 100



######################################################################


##Analisis de correspondencia (CA)

#Es la herramienta favorita para evaluar datos de presencia-ausencia de especies

#Com?nmente para medir la dependencia entre descriptores (variables o especies)
#se usan coeficientes como los de correlaci?n de Pearson (Legendre 1998).
#Sin embargo, debido a que las datas de abundancia de especies, 
#tienden a presentar un alto n?mero de ceros, es incorrecto hacer
#este tipo de an?lisis(PCA). La raz?n principal es que los ceros son usados
#como cualquier otra variable cuantitativa y para el caso de abundancia de especies
#la presencia de muchos doble-cero, sin ning?n significado biol?gico,
#distorsiona significativamente la dispersi?n de la elipse de los sitios
#con respecto al eje de las especies (Legendre 1998).

#El problema de los doble-cero en ecolog?a aparece por la naturaleza
#de las especies, las cuales se conoce
#que tiene distribuciones unimodales a lo largo de los gradientes ambientales (Whittaker, 1967).
#Es decir, si una especie est? presente en dos sitios, esto indica que hay 
#similitud entre los sitios, pero si las especies est?n ausentes es ambos sitios
#esto puede ser producto de varias razones como: distribuci?n
#unimodal, nicho optimo, patrones de dispersi?n, eventos hist?ricos o 
#variaci?n por muestreo estoc?stico, etc.

#Por lo tanto, para estos casos es mejor interpretar al cero como 
#"falta de informaci?n". Esta es la principal raz?n del porque realizar
#un an?lisis de CA en vez de un PCA en estos casos.
#El an?lisis de CA utiliza la distancia ??2 en vez de la distancia euclidiana,
#excluyendo los doble-cero, siendo mejor realizar un CA en el estudio de asociaci?n de especies (Legendre 1998).

##Los datos utilizados para realizar CA, tienen que ser tipo frecuencia
#homogeneos y no-negativos, tal es el caso de numero de especies o
#datos de presencia-ausencia.

##19.- Realizamos CA

spe.ca <- cca(spe) # parece una funciona vieja
spe.ca <- cca(spe, env, scannf = TRUE, nf = 2) #nueva pero no fundion luego
spe.ca          #No usamos scale porque toda misma unidad
summary(spe.ca) #Si no especifica utiliza scaling=2
summary(spe.ca, scaling=1)

##CA scaling 1= las filas (sitios) son los centroides de las columnas (especies)
##es apropiado cuando el interes es ver la ordenacion de los objetos (sitios).
##Interpretacion, (1) los sitios cercanos es probable que sean similares en su
##frecuencia relativa de especies. (2) Cualquier sitio cercano a una especie es probable
##que contenga una alta contribucion de dicha especie

##CA scaling 2= las columnas (especies) son los centroides de las filas (sitios).
##es apropiado cuando el interes es ver la ordenacion de las especies.
##Interpretacion, (1) especies cercanas tienden a tener frecuencias relativas similares
##entre los objetos(sitios), (2) una especie cercana a un sitio es altamente probable que
##se encuentre en ese sitio o tenga una frecuencia mayor que en otros sitios.

##20.-Eigenvalues

(ev=spe.ca$CA$eig)


##20.-Aplicar el criterio de Kaiser-Guttman para seleccionar los ejes

ev[ev>mean(ev)]


##21.-Graficar la relacion

evplot = function(ev) {
  # Plot eigenvalues and % of variation for each axis
  op = par(mfrow=c(1,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  
}

evplot(ev)

##El primer CA es extremadamente dominante

##22.- biplots para CA: scaling 1 y 2

##Usando biplot

par(mfrow=c(1,2))
plot(spe.ca, scaling=1, main="CA abundancia peces-scaling 1")
plot(spe.ca, main="CA abundancia peces-scaling 2")

##Se puede hacer el analisis de cluster aca tambien

##Primer eje posee los sitios (19-30) de la parte baja del rio
##Hay un claro contraste que explica la relevancia de el CA1
##Muchas especies relacionadas con los sitios 19-30, indicando
##que son mas abundantes rio abajo
##Y muchas de ellas estan ausentes en la parte alta del rio
##El segundo eje contrasta 10 sitios rio arriba con los intermedios
##cada grupo asociado a especies caracteristicas

##Scaling 2=pequeno grupo de especies esta distribuido entre los sitios
##OMB, CHA, BLA se encuentran en grupos intermedios (11-18)
##mientras que VAI, TRU, LOC se encuentran en una porcion mayor del
##rio (1-18)

###########################################################

##Finalmente podemos realizar NMDS

##Hay situaciones donde la prioridad del an?lisis no es preservar
##las distancias, sino que representar los objetos en un peque?o y
##espec?fico n?mero de dimensiones (2 o 3), es decir, 
##preservar las relaciones entre los objetos. Para estos
##casos es que usamos el NMDS.

##Este an?lisis, a diferencia del PCA, no est? limitado por el uso 
##de matrices de distancia Euclidiana. De hecho, puede hacer la ordenaci?n
##de los objetos desde cualquier matriz de distancia. Para el presente
##ejercicio realice un NMDS, a partir de una matriz de distancia de Bray-Curtis
##Cabe destacar que la distancia de Bray-Curtis, al igual que lo hace la distancia Chi2,
##no toma en cuenta los ceros en la data.

##Por otra parte, contrariamente a los an?lisis de PCA y CA, el NMDS
##no maximiza la variabilidad asociada
##a los ejes individuales de ordenaci?n.
##Es decir, los ejes del NMDS son arbitrarios, por lo tanto, los
##gr?ficos pueden ser arbitrariamente rotados, centrados o invertidos.
##De esta manera, del NMDS no
##obtenemos un an?lisis cuantitativo sino que podemos simplemente
##observar la relaci?n de cercan?a
##entre especies y sitios.

##Este an?lisis permite una ordenaci?n en la cual las distancias 
##entre todos los pares de especies y
##sitios, est?n separados en relaci?n a que esto corresponda 
##con su mayor "disimilitud" en la
##composici?n de especies.

##NMDS abundancia de especies- matriz de distancia Bray-Curtis

spe.nmds=metaMDS(spe, distance="bray")
spe.nmds

spe.nmds$stress

par(mfrow=c(1,1))
plot(spe.nmds, type="t", main=paste("NMDS/Bray-Stress=", round(spe.nmds$stress,3)))

par(mfrow=c(1,2))
stressplot(spe.nmds, main="Shepard plot")
gof=goodness(spe.nmds)
plot(spe.nmds, type="t", main="Goodness of fit")
points(spe.nmds, display="sites", cex=gof*2)

##Los resultados obtenidos del an?lisis de NMDS, nos da un espacio de 2
##dimensiones, con un valor de stress de 0.074.

##Una de las formas de evaluar si el an?lisis funciona
##es analizar el diagrama de Shepard.
##Esta grafica nos muestra la ordenaci?n de las distancias
##contra las disimilitudes. El grado en el cual las distancias
##corresponden con el orden de las disimilitudes,
##es determinado por una regresi?n monotonica.
##Esta l?nea de regresi?n tiene que lucir como una escalera
##en ascenso, en el cual los "fitted values" representan las 
##distancias hipot?ticas que est?n perfectamente
##correlacionadas con los valores de disimilitud.
##Para el caso de nuestro an?lisis vemos que nos
##muestra un buen ajuste de los datos.


##El siguiente paso es calcular la medida de "stress" del an?lisis,
##esta medida nos da el valor de "mal ajuste" (Badness-of-fit)
##y es calculada por el stress de Kruskal.
##Este valor tiene la propiedad de que
##decrece en funci?n de que el arreglo entre las distancias y disimilitudes
##mejora. Por lo tanto, el objetivo
##es encontrar la ordenaci?n con el menor valor de stress posible

##Para el caso de nuestro an?lisis, la
##medida de stress es 0.074 lo que seg?n Kruskal
##es ecol?gicamente interpretable y ?til

#Stress    Calidad del NMDS
#<0.05     Excelente: No hay probabilidad de mala
#          interpretaci?n
#0.05-0.10 Bueno: Peque?a probabilidad de obtener falsas
#          inferencias
#0.10-0.20 Mediano: Utilizable, pero algunas distancias
#          podr?an ser mal entendidas
#>0.20     Pobre: La ordenaci?n puede ser peligrosa de
#          interpretar.

#Finalmente, al comparar los tres m?todos de ordenaci?n PCA, CA, NMDS,
#se observa que el PCA es el
#que tiene el peor rendimiento.
#Nuestra segunda opci?n, el an?lisis de CA, 
#responde bien a la naturaleza de la data
#y genera una ordenaci?n similar a la encontrada con el an?lisis de NMDS.
#Sin embargo, el an?lisis de CA
#todav?a presenta solapamiento en alguna de las especies. 
#Mientras que el an?lisis de NMDS resulto muy
#?til y significativo en el an?lisis de la data, 
#presentando el ?nico problema de que no se puede realizar
#un an?lisis cuantitativo, sin embargo, 
#para efectos de reducci?n de dimensionalidad y ordenaci?n, logro
#separar todos los grupos.
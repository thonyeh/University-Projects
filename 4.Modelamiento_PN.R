gc()
rm(list=ls())
dev.off()

# install.packages('bit64')
##LIBRERIAS
library(data.table)
library(openxlsx)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(Hmisc)
library(cluster)
library(factoextra)
library(plyr)

#NOTACION CIENTIFICA

options(scipen=999)

`%notin%` <- Negate(`%in%`)

##LECTURA DE DATOS

setwd("D:/Proyectos/Banco_Comercio/LAFT/archivos_trabajo_PN")

#### GRAFICOS DE TENDENCIA DE TRANSACCIONES DE PN

TMOVIMIENTOS_AGR_PN=fread("TMOVIMIENTOS_AGR_PN_V2.TXT")

# TMOVIMIENTOS_AGR_PN[CPERSONA==3423743,]

#### DATA AGRUPADA POR CLIENTE PARA EL MODELAMIENTO

colnames(TMOVIMIENTOS_AGR_PN)

# DESCRIPTIVO=TMOVIMIENTOS_AGR_PN%>%
#   dplyr::group_by(PERIODO)%>%
#   dplyr::arrange(PERIODO)%>%
#   dplyr::summarise(IMPORTE_DEB_USD_SUM=sum(IMPORTE_DEB_USD,na.rm = T),
#                    CANTIDAD_DEB_USD_SUM=sum(CANTIDAD_DEB_USD,na.rm = T))


TMOVIMIENTOS_AGR_PN_1=unique(TMOVIMIENTOS_AGR_PN[,c(1:30)])

TMOVIMIENTOS_AGR_PN_1=TMOVIMIENTOS_AGR_PN_1%>%
  dplyr::arrange(CPERSONA,PERIODO)%>%
  dplyr::group_by(CPERSONA)%>%
  dplyr::slice(dplyr::n())

TMOVIMIENTOS_AGR_PN_1=as.data.table(TMOVIMIENTOS_AGR_PN_1)
# TMOVIMIENTOS_AGR_PN_1[CPERSONA==3423743,]

TMOVIMIENTOS_AGR_PN_1=unique(TMOVIMIENTOS_AGR_PN_1[,c(1:3,5:30)])

# length(unique(TMOVIMIENTOS_AGR_PN_1$CPERSONA))

TMOVIMIENTOS_AGR_PN_2=as.data.table(TMOVIMIENTOS_AGR_PN%>%
  dplyr::group_by(CPERSONA)%>%
  dplyr::summarise(MEDIAN_IMPORTE_CRE_SOL_USD=median(IMPORTE_CRE_SOL_USD,na.rm = T),
                   MEDIAN_IMPORTE_DEB_SOL_USD=median(IMPORTE_DEB_SOL_USD,na.rm = T),
                   MEDIAN_IMPORTE_CRE_SOL=median(IMPORTE_CRE_SOL,na.rm = T),
                   MEDIAN_IMPORTE_CRE_USD=median(IMPORTE_CRE_USD,na.rm = T),
                   MEDIAN_IMPORTE_DEB_SOL=median(IMPORTE_DEB_SOL,na.rm = T),
                   MEDIAN_IMPORTE_DEB_USD=median(IMPORTE_DEB_USD,na.rm = T),
                   
                   MEAN_CANTIDAD_CRE_SOL=mean(CANTIDAD_CRE_SOL,na.rm = T),
                   MEAN_CANTIDAD_CRE_USD=mean(CANTIDAD_CRE_USD,na.rm = T),
                   MEAN_CANTIDAD_DEB_SOL=mean(CANTIDAD_DEB_SOL,na.rm = T),
                   MEAN_CANTIDAD_DEB_USD=mean(CANTIDAD_DEB_USD,na.rm = T),
                   
                   MEAN_IMPORTE_CRE_SOL_USD=mean(IMPORTE_CRE_SOL_USD,na.rm = T),
                   MEAN_IMPORTE_DEB_SOL_USD=mean(IMPORTE_DEB_SOL_USD,na.rm = T),
                   MEAN_IMPORTE_CRE_SOL=mean(IMPORTE_CRE_SOL,na.rm = T),
                   MEAN_IMPORTE_CRE_USD=mean(IMPORTE_CRE_USD,na.rm = T),
                   MEAN_IMPORTE_DEB_SOL=mean(IMPORTE_DEB_SOL,na.rm = T),
                   MEAN_IMPORTE_DEB_USD=mean(IMPORTE_DEB_USD,na.rm = T)))

setkey(TMOVIMIENTOS_AGR_PN_1,"CPERSONA")
setkey(TMOVIMIENTOS_AGR_PN_2,"CPERSONA")

TMOVIMIENTOS_AGR_PN=TMOVIMIENTOS_AGR_PN_2[TMOVIMIENTOS_AGR_PN_1]

rm(TMOVIMIENTOS_AGR_PN_1,TMOVIMIENTOS_AGR_PN_2)

TMOVIMIENTOS_AGR_PN[is.na(TMOVIMIENTOS_AGR_PN)] <- NA

##### VARIABLE DEPENDIENTE : CLUSTER CON VARIABLES TRANSACCIONALES

#### USANDO LA MEDIA
# 
# colnames(TMOVIMIENTOS_AGR_PN)
# 
# df_cluster_med=TMOVIMIENTOS_AGR_PN[,c(1,12:17)]
# 
# #---------- CATEGORIZAR VARIABLES  -------------#
# 
# ### Variables
# 
# variables=data.table(colnames(df_cluster_med))
# 
# df_cluster_med[is.na(df_cluster_med)] <- 0
# 
# 
# df_cluster_med_cat = df_cluster_med[ ,.(CPERSONA = CPERSONA,
#                                         MEDIA_IMPORTE_CRE_SOL_USD_CAT=as.integer(cut2(MEAN_IMPORTE_CRE_SOL_USD, g=10)),
#                                         MEDIA_IMPORTE_DEB_SOL_USD_CAT=as.integer(cut2(MEAN_IMPORTE_DEB_SOL_USD, g=10)),
#                                         MEDIA_IMPORTE_CRE_SOL_CAT=as.integer(cut2(MEAN_IMPORTE_CRE_SOL, g=10)),
#                                         MEDIA_IMPORTE_CRE_USD_CAT=as.integer(cut2(MEAN_IMPORTE_CRE_USD, g=10)),
#                                         MEDIA_IMPORTE_DEB_SOL_CAT=as.integer(cut2(MEAN_IMPORTE_DEB_SOL, g=10)),
#                                         MEDIA_IMPORTE_DEB_USD_CAT=as.integer(cut2(MEAN_IMPORTE_DEB_USD, g=10)))]
# 
# 
# table(df_cluster_med_cat$MEDIA_IMPORTE_CRE_SOL_USD_CAT,useNA = "ifany") # si
# table(df_cluster_med_cat$MEDIA_IMPORTE_DEB_SOL_USD_CAT,useNA = "ifany") # si
# table(df_cluster_med_cat$MEDIA_IMPORTE_CRE_SOL_CAT,useNA = "ifany") # si
# table(df_cluster_med_cat$MEDIA_IMPORTE_CRE_USD_CAT,useNA = "ifany") # no
# table(df_cluster_med_cat$MEDIA_IMPORTE_DEB_SOL_CAT,useNA = "ifany") # si
# table(df_cluster_med_cat$MEDIA_IMPORTE_DEB_USD_CAT,useNA = "ifany") # no
# 
# #---------- PERFILAMIENTO - CLUSTERS
# colnames(df_cluster_med_cat)
# 
# # Numero preliminar de clusters optimo
# 
# # Usando la funcion kmeans
# set.seed(123)
# wss<-numeric()
# for(h in 1:10){
#   b<-kmeans(na.omit(df_cluster_med_cat)[,c(2:4,6)],h)
#   wss[h]<-b$tot.withinss
# }
# plot(1:10,wss, type="b",col="red", 
#      pch=20 ,xlab = "Clusters", main= "Numero de Clusters - Criterio WSS")
# abline(v = 5, col = "blue", lty=3)
# 
# #######################################
# #   CLARA  cluster 
# #######################################
# 
# # CLARA: Clustering Large Applications
# # CLARA es un m?todo de particion usado para tratar con data 
# # muy grandes (miles de observaciones) con la finalidad de reducir 
# # el tiempo de cómputo en el problema de almacenamiento RAM.
# 
# #---------------------------------------------------
# # Calcular CLARA
# set.seed(123)
# system.time(clara.res <- clara(df_cluster_med_cat[,c(2:4,6)],5, samples = 100, pamLike = TRUE))
# 
# print(clara.res)
# 
# # Medoides de cada  cluster
# clara.res$medoids
# summary(clara.res)
# # Número de cluster para cada observacion 
# table(clara.res$cluster)
# prop.table(table(clara.res$cluster))*100
# 
# # colnames(df_cluster_med)
# 
# # Promedios de cada cluster 
# aggregate(TMOVIMIENTOS_AGR_PN[,c(2:7)], by=list(cluster=clara.res$cluster), mean,na.rm = TRUE)
# 
# #--------------------------------------------------
# # Visualizacion de los resultados
# # par(mfcol = c(1,2))
# fviz_cluster(clara.res)
# 
# # Visualizacion de los resultados - otra forma
# # fviz_cluster(clara.res,
# #              ellipse.type = "t", # Concentration ellipse
# #              geom = "point", pointsize = 1,
# #              ggtheme = theme_classic()
# # )
# 
# # otra forma de ver la silueta
# fviz_silhouette(clara.res, palette = "jco",  ggtheme = theme_classic())
# # par(mfcol = c(1,1))
# 
# # grafico de silueta
# plot(silhouette(clara.res),  col = 2:3, main = "Silhouette plot")

#### USANDO LA MEDIANA

colnames(TMOVIMIENTOS_AGR_PN)

df_cluster_med=TMOVIMIENTOS_AGR_PN[,c(1:17)]

#---------- CATEGORIZAR VARIABLES  -------------#

### Variables

variables=data.table(colnames(df_cluster_med))

df_cluster_med[is.na(df_cluster_med)] <- 0
df_cluster_med_cat = df_cluster_med[ ,.(CPERSONA = CPERSONA,
                                        MEDIAN_IMPORTE_CRE_SOL_USD_CAT=as.integer(cut2(MEDIAN_IMPORTE_CRE_SOL_USD, g=10)),
                                        MEDIAN_IMPORTE_DEB_SOL_USD_CAT=as.integer(cut2(MEDIAN_IMPORTE_DEB_SOL_USD, g=10)),
                                        MEDIAN_IMPORTE_CRE_SOL_CAT=as.integer(cut2(MEDIAN_IMPORTE_CRE_SOL, g=10)),
                                        #MEDIAN_IMPORTE_CRE_USD_CAT=as.integer(cut2(MEDIAN_IMPORTE_CRE_USD, g=10)),
                                        MEDIAN_IMPORTE_DEB_SOL_CAT=as.integer(cut2(MEDIAN_IMPORTE_DEB_SOL, g=10)),
                                        #MEDIAN_IMPORTE_DEB_USD_CAT=as.integer(cut2(MEDIAN_IMPORTE_DEB_USD, g=10)),
                                        
                                        MEAN_CANTIDAD_CRE_SOL_CAT=as.integer(cut2(MEAN_CANTIDAD_CRE_SOL, g=10)),
                                        #MEAN_CANTIDAD_CRE_USD_CAT=as.integer(cut2(MEAN_CANTIDAD_CRE_USD, g=10)),
                                        MEAN_CANTIDAD_DEB_SOL_CAT=as.integer(cut2(MEAN_CANTIDAD_DEB_SOL, g=10)))]

table(df_cluster_med_cat$MEDIAN_IMPORTE_CRE_SOL_USD_CAT,useNA = "ifany") # si
table(df_cluster_med_cat$MEDIAN_IMPORTE_DEB_SOL_USD_CAT,useNA = "ifany") # si
table(df_cluster_med_cat$MEDIAN_IMPORTE_CRE_SOL_CAT,useNA = "ifany") # si
# table(df_cluster_med_cat$MEDIAN_IMPORTE_CRE_USD_CAT,useNA = "ifany") # no
table(df_cluster_med_cat$MEDIAN_IMPORTE_DEB_SOL_CAT,useNA = "ifany") # si
# table(df_cluster_med_cat$MEDIAN_IMPORTE_DEB_USD_CAT,useNA = "ifany") # no
# 
table(df_cluster_med_cat$MEAN_CANTIDAD_CRE_SOL_CAT,useNA = "ifany") # si
# table(df_cluster_med_cat$MEAN_CANTIDAD_CRE_USD_CAT,useNA = "ifany") # no
table(df_cluster_med_cat$MEAN_CANTIDAD_DEB_SOL_CAT,useNA = "ifany") # si
# table(df_cluster_med_cat$MEAN_CANTIDAD_DEB_USD_CAT,useNA = "ifany") # no


#---------- PERFILAMIENTO - CLUSTERS

# Iteraciones para la mejor combinacion
df_cluster_med_cat <- as.data.table(df_cluster_med_cat)

variables = colnames(as.data.frame(df_cluster_med_cat)[,c(2:length(colnames(df_cluster_med_cat)))])
n=length(variables)

all <- data.frame(ldply(1:n, function(x)t(combn(variables, x))))
all$n_cols <- n- apply(all, 1, function(x) sum(is.na(x)))

prop_table_result=data.frame()
average_silhouette=data.frame()


for (i in 22:dim(all)[1]){
  cols = as.character(all[i,c(1:as.integer(all[i,n+1]))])
  #print(cols)
  set.seed(123)
  system.time(clara.res <- clara(df_cluster_med_cat[,..cols],5, samples = 100, pamLike = TRUE))
  prop_table_result = rbind(prop_table_result,prop.table(table(clara.res$cluster))*100  )
  colnames(prop_table_result)<-c("G1","G2","G3","G4","G5")
  average_silhouette <- rbind(average_silhouette,c(as.numeric(as.character(as.factor(summary(silhouette(clara.res))[1]$si.summary)[4]))) )  
}


tablas=list("combinacion_variables"=all
            ,"proporciones combinaciones"=prop_table_result
            ,"silueta combinaciones"=average_silhouette)

# write.xlsx(tablas,"tablas_result_cluster.xlsx")

rm(i,n,prop_table_result,tablas,variables,average_silhouette,clara.res,cols,all)

# Seleccion de la mejor iteracion (guiandome del excel que exportamos)

set.seed(123)
wss<-numeric()
for(h in 1:10){
  b<-kmeans(na.omit(df_cluster_med_cat)[,c('MEDIAN_IMPORTE_CRE_SOL_USD_CAT','MEDIAN_IMPORTE_DEB_SOL_USD_CAT','MEDIAN_IMPORTE_CRE_SOL_CAT','MEDIAN_IMPORTE_DEB_SOL_CAT')],h)
  wss[h]<-b$tot.withinss
}
plot(1:10,wss, type="b",col="red", 
     pch=20 ,xlab = "Clusters", main= "Numero de Clusters - Criterio WSS")
abline(v = 5, col = "blue", lty=3)

set.seed(123)
system.time(clara.res <- clara(df_cluster_med_cat[,c('MEDIAN_IMPORTE_CRE_SOL_USD_CAT','MEDIAN_IMPORTE_DEB_SOL_USD_CAT','MEDIAN_IMPORTE_CRE_SOL_CAT','MEDIAN_IMPORTE_DEB_SOL_CAT')],5, samples = 100, pamLike = TRUE))

print(clara.res)
prop.table(table(clara.res$cluster))*100
aggregate(TMOVIMIENTOS_AGR_PN[,c(2:11)], by=list(cluster=clara.res$cluster), mean,na.rm = TRUE)
fviz_cluster(clara.res)

# otra forma de ver la silueta
fviz_silhouette(clara.res, palette = "jco",  ggtheme = theme_classic())
# par(mfcol = c(1,1))

# grafico de silueta
# plot(silhouette(clara.res),  col = 2:3, main = "Silhouette plot")

# # volver factor cluster
clara.res$cluster <- as.factor(clara.res$cluster)

# # Junta el archivo de datos con la columna de cluster
TMOVIMIENTOS_AGR_PN = cbind(TMOVIMIENTOS_AGR_PN, cluster = clara.res$cluster)

rm(b,h,wss)

################## VARIABLES CATEGORICAS

# colnames(TMOVIMIENTOS_AGR_PN[,c(20,21,23,25,29,30,32,33,34,35,38,39,40,42,43,44,45)])

# FLAG_CAER

table(TMOVIMIENTOS_AGR_PN$FLAG_CAER,useNA = "ifany")


# FLAG_PERSONA_NEG

table(TMOVIMIENTOS_AGR_PN$FLAG_PERSONA_NEG,useNA = "ifany")


# CTIPOCAER

table(TMOVIMIENTOS_AGR_PN$CTIPOCAER,useNA = "ifany")


# CPROVINCIA - Tabla_TPERSONADIRECCIONES

table(TMOVIMIENTOS_AGR_PN$CPROVINCIA,useNA = "ifany")

# CCIUDAD - Tabla_TPERSONADIRECCIONES

table(TMOVIMIENTOS_AGR_PN$CCIUDAD,useNA = "ifany")

# CPROVINCIA_NACIMIENTO - Tabla_TNATURALINFORMACIONADICIONAL

table(TMOVIMIENTOS_AGR_PN$CPROVINCIA_NACIMIENTO,useNA = "ifany")


# CCIUDAD_NACIMIENTO - Tabla_TNATURALINFORMACIONADICIONAL

table(TMOVIMIENTOS_AGR_PN$CCIUDAD_NACIMIENTO,useNA = "ifany")


# CPROFESION - Tabla_TNATURALINFORMACIONADICIONAL

table(TMOVIMIENTOS_AGR_PN$CPROFESION,useNA = "ifany")

# CNIVELEDUCACION - Tabla_TNATURALINFORMACIONADICIONAL

table(TMOVIMIENTOS_AGR_PN$CNIVELEDUCACION,useNA = "ifany")

# CFUENTEINGRESO - Tabla_TNATURALINFORMACIONADICIONAL

table(TMOVIMIENTOS_AGR_PN$CFUENTEINGRESO,useNA = "ifany")

# GENERO - Tabla_TNATURALINFORMACIONBASICA

table(TMOVIMIENTOS_AGR_PN$GENERO,useNA = "ifany")

# CESTADOCIVIL - Tabla_TNATURALINFORMACIONBASICA

table(TMOVIMIENTOS_AGR_PN$CESTADOCIVIL,useNA = "ifany")

# CRESIDENCIA - Tabla_TPERSONA

table(TMOVIMIENTOS_AGR_PN$CRESIDENCIA,useNA = "ifany")

# CACTIVIDAD - Tabla_TPERSONA

table(TMOVIMIENTOS_AGR_PN$CACTIVIDAD,useNA = "ifany")

# CTIPOBANCA - Tabla_TPERSONA

table(TMOVIMIENTOS_AGR_PN$CTIPOBANCA,useNA = "ifany")


# CTIPOSEGMENTO - Tabla_TPERSONA

table(TMOVIMIENTOS_AGR_PN$CTIPOSEGMENTO,useNA = "ifany")


################# LEVANTAMOS LAS DIMENSIONES

TCPROFESION=fread("TCPROFESION.TXT")
TCACTIVIDAD_PN=fread("TCACTIVIDAD_PN.TXT", encoding = 'UTF-8')
TCTIPOBANCA=fread("TCTIPOBANCA.TXT")
TCTIPOSEGMENTO=fread("TCTIPOSEGMENTO.TXT")
TPROVINCIAS=fread("TPROVINCIAS.TXT")


### CRUCE DE DIMENSIONES CON TABLA PRINCIPAL


### CPROFESION
setkey(TCPROFESION,"CPROFESION")
setkey(TMOVIMIENTOS_AGR_PN,"CPROFESION")

TMOVIMIENTOS_AGR_PN=TCPROFESION[,1:2][TMOVIMIENTOS_AGR_PN]

rm(TCPROFESION)

setnames(TMOVIMIENTOS_AGR_PN,"DESCRIPCION","DESC_PROFESION")

table(TMOVIMIENTOS_AGR_PN$CPROFESION,useNA = "ifany") ### 49 % MISSING
dim(TMOVIMIENTOS_AGR_PN[CPROFESION==0,])[1]/dim(TMOVIMIENTOS_AGR_PN)[1]
table(TMOVIMIENTOS_AGR_PN$DESC_PROFESION,useNA = "ifany")
dim(table(TMOVIMIENTOS_AGR_PN$DESC_PROFESION,useNA = "ifany")) # 125 categorias
TMOVIMIENTOS_AGR_PN[is.na(DESC_PROFESION),DESC_PROFESION:='SIN.INFORMACION']


### CACTIVIDAD
setkey(TCACTIVIDAD_PN,"CACTIVIDAD")
setkey(TMOVIMIENTOS_AGR_PN,"CACTIVIDAD")

TMOVIMIENTOS_AGR_PN=TCACTIVIDAD_PN[TMOVIMIENTOS_AGR_PN]

rm(TCACTIVIDAD_PN)

setnames(TMOVIMIENTOS_AGR_PN,"DESCRIPCION","DESC_ACTIVIDAD")

table(TMOVIMIENTOS_AGR_PN$CACTIVIDAD,useNA = "ifany") ### 3 % MISSING
dim(TMOVIMIENTOS_AGR_PN[is.na(CACTIVIDAD),])[1]/dim(TMOVIMIENTOS_AGR_PN)[1]
table(TMOVIMIENTOS_AGR_PN$DESC_ACTIVIDAD,useNA = "ifany")
dim(table(TMOVIMIENTOS_AGR_PN$DESC_ACTIVIDAD,useNA = "ifany")) # 61 categorias
TMOVIMIENTOS_AGR_PN[is.na(DESC_ACTIVIDAD),DESC_ACTIVIDAD:='SIN.INFORMACION']

### CTIPOBANCA
setkey(TCTIPOBANCA,"CTIPOBANCA")
setkey(TMOVIMIENTOS_AGR_PN,"CTIPOBANCA")

TMOVIMIENTOS_AGR_PN=TCTIPOBANCA[,1:2][TMOVIMIENTOS_AGR_PN]

rm(TCTIPOBANCA)

setnames(TMOVIMIENTOS_AGR_PN,"DESCRIPCION","DESC_TIPOBANCA")

table(TMOVIMIENTOS_AGR_PN$CTIPOBANCA,useNA = "ifany") ### 0 % MISSING
dim(TMOVIMIENTOS_AGR_PN[is.na(CTIPOBANCA),])[1]/dim(TMOVIMIENTOS_AGR_PN)[1]
table(TMOVIMIENTOS_AGR_PN$DESC_TIPOBANCA,useNA = "ifany")
dim(table(TMOVIMIENTOS_AGR_PN$DESC_TIPOBANCA,useNA = "ifany")) # 8 categorias
TMOVIMIENTOS_AGR_PN[is.na(DESC_ACTIVIDAD),DESC_ACTIVIDAD:='SIN.INFORMACION']


### CTIPOSEGMENTO
setkey(TCTIPOSEGMENTO,"CTIPOBANCA","CTIPOSEGMENTO")
setkey(TMOVIMIENTOS_AGR_PN,"CTIPOBANCA","CTIPOSEGMENTO")

TMOVIMIENTOS_AGR_PN=TCTIPOSEGMENTO[TMOVIMIENTOS_AGR_PN]

rm(TCTIPOSEGMENTO)

setnames(TMOVIMIENTOS_AGR_PN,"DESCRIPCION","DESC_TIPOSEGMENTO")

table(TMOVIMIENTOS_AGR_PN$CTIPOSEGMENTO,useNA = "ifany") ### 1 % MISSING
dim(TMOVIMIENTOS_AGR_PN[is.na(DESC_TIPOSEGMENTO),])[1]/dim(TMOVIMIENTOS_AGR_PN)[1]
table(TMOVIMIENTOS_AGR_PN$DESC_TIPOSEGMENTO,useNA = "ifany")
dim(table(TMOVIMIENTOS_AGR_PN$DESC_TIPOSEGMENTO,useNA = "ifany")) # 19 categorias
TMOVIMIENTOS_AGR_PN[is.na(DESC_TIPOSEGMENTO),DESC_TIPOSEGMENTO:='SIN.INFORMACION']


### CPROVINCIA
setkey(TPROVINCIAS,"CPROVINCIA")
setkey(TMOVIMIENTOS_AGR_PN,"CPROVINCIA")

TMOVIMIENTOS_AGR_PN=TPROVINCIAS[TMOVIMIENTOS_AGR_PN]

rm(TPROVINCIAS)

table(TMOVIMIENTOS_AGR_PN$CPROVINCIA,useNA = "ifany") ### 28 % MISSING
dim(TMOVIMIENTOS_AGR_PN[is.na(CPROVINCIA),])[1]/dim(TMOVIMIENTOS_AGR_PN)[1]
table(TMOVIMIENTOS_AGR_PN$DEPARTAMENTO,useNA = "ifany")
dim(table(TMOVIMIENTOS_AGR_PN$DEPARTAMENTO,useNA = "ifany")) # 26 categorias
TMOVIMIENTOS_AGR_PN[is.na(DEPARTAMENTO),DEPARTAMENTO:='SIN.INFORMACION']


### CREACION DE MACRO - REGIONES


TMOVIMIENTOS_AGR_PN[,MACRO_REGION:=ifelse(is.na(DEPARTAMENTO) | DEPARTAMENTO=='SIN.INFORMACION','SIN INFORMACION',
                                         ifelse(DEPARTAMENTO %in% c("LA LIBERTAD","LAMBAYEQUE", "PIURA", "TUMBES"), 'NORTE',
                                         ifelse(DEPARTAMENTO %in% c("ANCASH","HUANUCO", "JUNIN", "PASCO"), 'NOR-CENTRO',
                                         ifelse(DEPARTAMENTO %in% c("CALLAO","LIMA"), 'LIMA-CALLAO',
                                         ifelse(DEPARTAMENTO %in% c("AYACUCHO","HUANCAVELICA","ICA"), 'SUR_1',
                                         ifelse(DEPARTAMENTO %in% c("AMAZONAS","CAJAMARCA","SAN MARTIN"), 'NOR-ORIENTE',
                                         ifelse(DEPARTAMENTO %in% c("LORETO","UCAYALI"), 'SELVA',
                                         ifelse(DEPARTAMENTO %in% c("APURIMAC","CUSCO","MADRE DE DIOS"), 'SUR_2',
                                         ifelse(DEPARTAMENTO %in% c("AREQUIPA","MOQUEGUA", "PUNO", "TACNA"), 'SUR-ANDINA',NA)))))))))]

table(TMOVIMIENTOS_AGR_PN$MACRO_REGION,useNA = "ifany")
table(TMOVIMIENTOS_AGR_PN$DEPARTAMENTO,useNA = "ifany")

##### EDAD

summary(TMOVIMIENTOS_AGR_PN$EDAD)

# TMOVIMIENTOS_AGR_PN%>%
#   dplyr::group_by(cluster)%>%
#   dplyr::summarise(registros=n(),
#                    MEDIAN_IMPORTE_CRE_SOL_USD=mean(MEDIAN_IMPORTE_CRE_SOL_USD,na.rm = T))%>%
#   dplyr::mutate(part=registros/sum(registros))


#################### ARBOL


TMOVIMIENTOS_AGR_PN[is.na(EDAD),EDAD:=0]

TMOVIMIENTOS_AGR_PN[is.na(MEDIAN_IMPORTE_CRE_SOL_USD),MEDIAN_IMPORTE_CRE_SOL_USD:=0]
TMOVIMIENTOS_AGR_PN[is.na(MEDIAN_IMPORTE_DEB_SOL_USD),MEDIAN_IMPORTE_DEB_SOL_USD:=0]
TMOVIMIENTOS_AGR_PN[is.na(MEDIAN_IMPORTE_CRE_SOL),MEDIAN_IMPORTE_CRE_SOL:=0]
TMOVIMIENTOS_AGR_PN[is.na(MEDIAN_IMPORTE_CRE_USD),MEDIAN_IMPORTE_CRE_USD:=0]
TMOVIMIENTOS_AGR_PN[is.na(MEDIAN_IMPORTE_DEB_SOL),MEDIAN_IMPORTE_DEB_SOL:=0]
TMOVIMIENTOS_AGR_PN[is.na(MEDIAN_IMPORTE_DEB_USD),MEDIAN_IMPORTE_DEB_USD:=0]

TMOVIMIENTOS_AGR_PN[is.na(MEAN_CANTIDAD_CRE_SOL),MEAN_CANTIDAD_CRE_SOL:=0]
TMOVIMIENTOS_AGR_PN[is.na(MEAN_CANTIDAD_CRE_USD),MEAN_CANTIDAD_CRE_USD:=0]
TMOVIMIENTOS_AGR_PN[is.na(MEAN_CANTIDAD_DEB_SOL),MEAN_CANTIDAD_DEB_SOL:=0]
TMOVIMIENTOS_AGR_PN[is.na(MEAN_CANTIDAD_DEB_USD),MEAN_CANTIDAD_DEB_USD:=0]

############ ELECCION DE LAS VARIABÑES CATS + NUMERICAS QUE INGRESARAN AL ARBOL

DATAMODEL_PN=TMOVIMIENTOS_AGR_PN[,c("CPERSONA","FLAG_CAER","FLAG_PERSONA_NEG","MACRO_REGION",
                                    "DESC_TIPOBANCA","CNIVELEDUCACION","CFUENTEINGRESO",
                                    "MEDIAN_IMPORTE_CRE_SOL_USD","MEDIAN_IMPORTE_DEB_SOL_USD",
                                    "MEDIAN_IMPORTE_CRE_SOL","MEDIAN_IMPORTE_CRE_USD",
                                    "MEDIAN_IMPORTE_DEB_SOL","MEDIAN_IMPORTE_DEB_USD",
                                    "MEAN_CANTIDAD_CRE_SOL","MEAN_CANTIDAD_CRE_USD",
                                    "MEAN_CANTIDAD_DEB_SOL","MEAN_CANTIDAD_DEB_USD",
                                    "EDAD","cluster")]


set.seed(5)
index <- sample(x = 1:nrow(DATAMODEL_PN), size = nrow(DATAMODEL_PN)*0.7)
train.db <- DATAMODEL_PN[index,]
test.db <- DATAMODEL_PN[-index,]

prop.table(table(TMOVIMIENTOS_AGR_PN$cluster,useNA = "ifany"))
prop.table(table(train.db$cluster,useNA = "ifany"))
prop.table(table(test.db$cluster,useNA = "ifany"))

library(rpart)
arbol <- rpart(cluster ~ ., 
               data = train.db[,2:19],
               control = rpart.control(minsplit = 2, 
                                       maxdepth = 4, 
                                       cp = 0.01))
library(rpart.plot)
rpart.plot(arbol)

preds <- predict(arbol, newdata = test.db,type = "class")

library(caret)
confusionMatrix(data = preds,
                reference = test.db$cluster)










 ### Curso de Revisiones Sistemáticas y Meta-análisis ###
                    ## Javier Mariani ##



# Paquetes ----------------------------------------------------------------
install.packages("meta")
install.packages("robvis")
install.packages("rio")
library(meta)
library(robvis)
library(rio)

# Evaluación de calidad ----

sesgo <- import("Datos/sesgo.xlsx")
bias <- rob_summary(sesgo, tool="ROB2", weighted = FALSE, overall=TRUE)
bias

bias1 <- rob_traffic_light(sesgo, tool="ROB2",psize = 10)
bias1

# Meta-análisis de datas binomiales ----
aas_elwood <- import("Datos/aas_elwood.xlsx")
muerte <- metabin(muerte_exp, n_e, muerte_control, n_c, data=aas_elwood, sm="RR", studlab = nombre)
forest(muerte)

forest(muerte, common=FALSE, col.square = "black", col.diamond = "red") #Con algunas modificaciones.

aas_elwood$tipo <- ifelse(aas_elwood$nombre=="AMIS (1980)", "Peor", "Mejor")
muerte <- metabin(muerte_exp, n_e, muerte_control, n_c, data=aas_elwood, sm="RR", subgroup = tipo)
forest(muerte, common=FALSE, col.square = "black", col.diamond = "red")

 # Realice un MA utilizando la base de datos "bb_iam.xlsx" o el evento muerte de la base de datos "aas_elwood.xlsx".

# Meta-análisis de datas continuos ----
sildenafil <- import("Datos/sildenafil_gab.xlsx")
psp <- metacont(n_interv_med_hemodinam,psp_interv,ds_psp_interv,n_control_med_hemodinam, psp_control,ds_psp_control, data=sildenafil, studlab = paste0(Author," (",Year,")"))
forest(psp)

psp <- metacont(n_interv_med_hemodinam,psp_interv,ds_psp_interv,n_control_med_hemodinam, psp_control,ds_psp_control, data=sildenafil, studlab = paste0(Author," (",Year,")"), sm = "SMD", common=FALSE)
forest(psp, allstudies=FALSE)

#Subgrupos
sildenafil$nuevos <- ifelse(sildenafil$Year>=2015,1,0)
psp_subgrupo <- metacont(n_interv_med_hemodinam,psp_interv,ds_psp_interv,n_control_med_hemodinam, psp_control,ds_psp_control, data=sildenafil, studlab = paste0(Author," (",Year,")"), subgroup = nuevos)
forest(psp_subgrupo,layout="RevMan5")

###Modifique las figuras (cambie solo un modelo de análisis, modifique los colores de los cuadrados y diamantes,
# modifique el color de la letra de los subgrupos)

## Realice un análisis de la calidad de vida (qol) usando SMD.

# Heterogeneidad ----
# Estratificación
vo2 <- metacont(n_interv_medidos, VO2_interv, ds_VO2_interv, n_control_medidos, VO2_control, ds_VO2_control, data=sildenafil, studlab = Author, common = FALSE)
forest(vo2, allstudies = FALSE) ##Vemos que el MA tiene elevada heterogeneidad. 

# Vammos a repetir el MA estraficando por la dosis de sildenafil
vo2 <- metacont(n_interv_medidos, VO2_interv, ds_VO2_interv, n_control_medidos, VO2_control, ds_VO2_control, data=sildenafil, studlab = Author, common = FALSE,subgroup=dosis_diaria>=150)
forest(vo2, allstudies = FALSE) ##La heterogeneidad dentro del subgrupo se reduce.

# Meta-regresión
vo2 <- metacont(n_interv_medidos, VO2_interv, ds_VO2_interv, n_control_medidos, VO2_control, ds_VO2_control, data=sildenafil, studlab = Author, common = FALSE,subset = !is.na(sildenafil$VO2_interv))
mr_vo2 <- metareg(vo2, ~dosis_diaria) ##El coeficiente de regresión de "dosis_diaria" es significativo.
mr_vo2
bubble(mr_vo2, ylim=c(min(vo2$TE), max(vo2$TE)))
bubble(mr_vo2, ylim=c(min(vo2$TE), max(vo2$TE)), bg = factor(paste0(mr_vo2$dataAuthor, mr_vo2$data$Year)),col = factor(paste0(mr_vo2$dataAuthor, mr_vo2$data$Year)))
bubble(mr_vo2, ylim=c(min(vo2$TE), max(vo2$TE)), bg = factor(paste0(mr_vo2$dataAuthor, mr_vo2$data$Year)),col = factor(paste0(mr_vo2$dataAuthor, mr_vo2$data$Year)), studlab = TRUE)

##Exploración por estudio
metainf(vo2)
forest(metainf(vo2))

#Sesgo de publicación ----
funnel(metabin(muerte_interv, n_interv, muerte_plac,n_plac, data=sildenafil, studlab = Author, incr = 0.5,method.incr = "only0",allstudies = TRUE))
metabias(metabin(muerte_interv, n_interv, muerte_plac,n_plac, data=sildenafil, studlab = Author, incr = 0.5,method.incr = "only0",allstudies = TRUE))

funnel(muerte)
metabias(muerte, k.min=5)

##Trim and fill
trimfill(muerte)
summary(trimfill(muerte))
funnel(trimfill(muerte))

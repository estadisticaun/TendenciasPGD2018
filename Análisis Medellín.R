#################################################################################
# ANÁLISIS DE RESULTADOS DE LOS EXAMENES SABER PRO
#################################################################################

# LIBRERÍAS REQUERIDAS
library(readxl)  # version 1.0.0
library(tidyverse)
library(ggrepel)
library(plotly)
library(DT)          # version 0.4
library(highcharter) # version 0.5.0.9999


# IMPORTAR DATOS SABER PRO 2017

SBPRO_2017_GEN <- read_excel("Datos/SBPRO-2017-GEN.xlsx")

# SELECCIONAR VARIABLES DE INTERÉS

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% select(c(INST_COD_INSTITUCION:GRUPOREFERENCIA, 
                                              MOD_RAZONA_CUANTITAT_PUNT, MOD_RAZONA_CUANTITATIVO_PNAL, MOD_RAZONA_CUANTITATIVO_PGREF,
                                              MOD_LECTURA_CRITICA_PUNT, MOD_LECTURA_CRITICA_PNAL, MOD_LECTURA_CRITICA_PGREF,
                                              MOD_COMPETEN_CIUDADA_PUNT, MOD_COMPETEN_CIUDADA_PNAL, MOD_COMPETEN_CIUDADA_PGREF,
                                              MOD_INGLES_PUNT, MOD_INGLES_PNAL, MOD_INGLES_PGREF, MOD_COMUNI_ESCRITA_PUNT,
                                              MOD_COMUNI_ESCRITA_PNAL, MOD_COMUNI_ESCRITA_PGREF, PUNT_GLOBAL, PERCENTIL_GLOBAL))

# CREAR VARIABLES DE INTERÉS

# Crear Variable Unal

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(Unal =  case_when(.$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920) ~ "UN",
                           TRUE ~ "Resto IES"))


SBPRO_2017_GEN %>% group_by(Unal) %>% summarise(Mediana = median(PUNT_GLOBAL, na.rm = TRUE))
  
median(SBPRO_2017_GEN$PUNT_GLOBAL, na.rm = TRUE)

# Crear Sedes Unal

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(Sedes =  case_when(.$INST_COD_INSTITUCION == 1101 ~ "UN-Bogota",
                            .$INST_COD_INSTITUCION == 1102 ~ "UN-Medellin",
                            .$INST_COD_INSTITUCION == 1103 ~ "UN-Manizales",
                            .$INST_COD_INSTITUCION == 1104 ~ "UN-Palmira",
                            .$INST_COD_INSTITUCION == 1124 ~ "UN-Orinoquia",
                            .$INST_COD_INSTITUCION == 1125 ~ "UN-Amazonia",
                            .$INST_COD_INSTITUCION == 1126 ~ "UN-Caribe",
                            .$INST_COD_INSTITUCION == 9920 ~ "UN-Tumaco",
                            TRUE ~ "Resto IES"))


# Crear G12

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(G12 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104)  ~ "U. Nacional",
                          TRUE ~ "Resto IES"))

# Crear G16 (sedes Unal)
SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(G15 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION == 1101  ~ "UN-Bogota",
                          .$INST_COD_INSTITUCION == 1102  ~ "UN-Medellin",
                          .$INST_COD_INSTITUCION == 1103  ~ "UN-Manizales",
                          .$INST_COD_INSTITUCION == 1104  ~ "UN-Palmira",
                          TRUE ~ "Resto IES"))


# Crear Facultades Unal

Facultades <- read_excel("Datos/Facultades.xlsx")

SBPRO_2017_GEN <- left_join(SBPRO_2017_GEN, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


# Filtrar datos de la sede Medellín

Medellín <- SBPRO_2017_GEN %>% filter(SEDE == "Medellín")
Medellín$FACULTAD <- as.factor(Medellín$FACULTAD)


SBPRO_2017_GEN %>% group_by(Sedes) %>% summarise(Mediana = median(PUNT_GLOBAL))

# Puntaje Global

ggplot(data = Medellín, aes(y = PUNT_GLOBAL, x = fct_reorder(FACULTAD, PUNT_GLOBAL, fun = median, .desc =TRUE))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 185, col = "#2ca25f", size = 1, linetype="dotdash") +
  geom_hline(yintercept = 178, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Puntaje Promedios Prueba Saber PRO 2017 por Facultades Sede Medellín\n")+
  ylab("Puntaje Promedio Global Saber PRO 2017\n")+
  xlab("\nFacultades") +
  theme(axis.text.y = element_text(colour = "blue", size = 13, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", angle = 0),
        axis.title = element_text(face="bold", color="black", size=16),
        plot.title = element_text(hjust = 0.5, face="bold", color="black", size=16))
  


# Razonamiento Cuantitativo

SBPRO_2017_GEN %>% group_by(Sedes) %>% summarise(Mediana = median(MOD_RAZONA_CUANTITAT_PUNT))

ggplot(data = Medellín, aes(y = MOD_RAZONA_CUANTITAT_PUNT, x = fct_reorder(FACULTAD, MOD_RAZONA_CUANTITAT_PUNT, fun = median, .desc =TRUE))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 191, col = "#2ca25f", size = 1.5, linetype="dotdash") +
  geom_hline(yintercept = 191, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Puntaje Promedios prueba Razonamiento Cuantitativo examen Saber PRO 2017 \n por facultades Sede Medellín\n")+
  ylab("Puntaje Prueba\n")+
  xlab("\nFacultades") +
  theme(axis.text.y = element_text(colour = "blue", size = 13, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", angle = 0),
        axis.title = element_text(face="bold", color="black", size=16),
        plot.title = element_text(hjust = 0.5, face="bold", color="black", size=16))


# Lectura Crítica

SBPRO_2017_GEN %>% group_by(Sedes) %>% summarise(Mediana = median(MOD_LECTURA_CRITICA_PUNT))

ggplot(data = Medellín, aes(y = MOD_LECTURA_CRITICA_PUNT, x = fct_reorder(FACULTAD, MOD_LECTURA_CRITICA_PUNT, fun = median, .desc =TRUE))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 193, col = "#2ca25f", size = 1, linetype="dotdash") +
  geom_hline(yintercept = 186, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Puntaje Promedios prueba Lectura Crítica examen Saber PRO 2017 \n por facultades Sede Medellín\n")+
  ylab("Puntaje Prueba\n")+
  xlab("\nFacultades") +
  theme(axis.text.y = element_text(colour = "blue", size = 13, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", angle = 0),
        axis.title = element_text(face="bold", color="black", size=16),
        plot.title = element_text(hjust = 0.5, face="bold", color="black", size=16))

# Competencias ciudadanas

SBPRO_2017_GEN %>% group_by(Sedes) %>% summarise(Mediana = median(MOD_COMPETEN_CIUDADA_PUNT))

ggplot(data = Medellín, aes(y = MOD_COMPETEN_CIUDADA_PUNT, x = fct_reorder(FACULTAD, MOD_COMPETEN_CIUDADA_PUNT, fun = median, .desc =TRUE))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 184, col = "#2ca25f", size = 1, linetype="dotdash") +
  geom_hline(yintercept = 175, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Puntaje Promedios prueba Competencias Ciudadanas examen Saber PRO 2017 \n por facultades Sede Medellín\n")+
  ylab("Puntaje Prueba\n")+
  xlab("\nFacultades") +
  theme(axis.text.y = element_text(colour = "blue", size = 13, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", angle = 0),
        axis.title = element_text(face="bold", color="black", size=16),
        plot.title = element_text(hjust = 0.5, face="bold", color="black", size=16))


# Inglés

SBPRO_2017_GEN %>% group_by(Sedes) %>% summarise(Mediana = median(MOD_INGLES_PUNT))

ggplot(data = Medellín, aes(y = MOD_INGLES_PUNT, x = fct_reorder(FACULTAD, MOD_INGLES_PUNT, fun = median, .desc =TRUE))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 191, col = "#2ca25f", size = 1, linetype="dotdash") +
  geom_hline(yintercept = 184, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Puntaje Promedios prueba Inglés examen Saber PRO 2017 \n por facultades Sede Medellín\n")+
  ylab("Puntaje Prueba\n")+
  xlab("\nFacultades") +
  theme(axis.text.y = element_text(colour = "blue", size = 13, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", angle = 0),
        axis.title = element_text(face="bold", color="black", size=16),
        plot.title = element_text(hjust = 0.5, face="bold", color="black", size=16))



# Comunicación Escrita

# se  eliminan resultados faltantes de la prueba de comunicación escrita

Escrita <- Medellín %>% filter(!is.na(MOD_COMUNI_ESCRITA_PUNT))

SBPRO_2017_GEN %>% group_by(Sedes) %>% summarise(Mediana = median(MOD_COMUNI_ESCRITA_PUNT, na.rm = TRUE))


ggplot(data = Escrita, aes(y = MOD_COMUNI_ESCRITA_PUNT, x = fct_reorder(FACULTAD, MOD_COMUNI_ESCRITA_PUNT, fun = median, .desc =TRUE))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 169, col = "#2ca25f", size = 1, linetype="dotdash") +
  geom_hline(yintercept = 161, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Puntaje Promedios prueba Comunicación Escrita Saber PRO 2017 \n por facultades Sede Medellín\n")+
  ylab("Puntaje Promedio Global Saber PRO 2017\n")+
  xlab("\nFacultades") +
  theme(axis.text.y = element_text(colour = "blue", size = 13, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", angle = 0),
        axis.title = element_text(face="bold", color="black", size=16),
        plot.title = element_text(hjust = 0.5, face="bold", color="black", size=16))


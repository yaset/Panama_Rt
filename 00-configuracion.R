## 00-configuracion.R ####

# 1. Crea subdirectorios del proyecto
# 2. Instala los paquetes R requeridos
# 3. Carga funciones requeridas y tokens


# Quitar el comentario de la linea siguiente si desea
# forzar actualizacion de todos los paquetes (DEMORADO!!!)
# update.packages(ask = FALSE, dependencies = c('Suggests'))


## Crear subdirectorios si no existen ####

if (!dir.exists("data"))      {dir.create("data")}
if (!dir.exists("salidas"))   {dir.create("salidas")}
if (!dir.exists("figures"))  {dir.create("figures")}
if (!dir.exists("shapes"))    {dir.create("shapes")}
if (!dir.exists("functions")) {dir.create("functions")}


## Cargar las funciones y tokens ####

funciones <- list.files(path = "funciones", full.names = TRUE)
sapply(funciones, source)

tokens <- list.files(path = "tokens", full.names = TRUE)
sapply(tokens, source)



## Instala paquetes cran

paquetes <- c("xlsx",
              "readr",
              "ggplot2",
              "dplyr",
              "incidence",
              "earlyR",
              "distcrete",
              "cowplot",
              "janitor",
              "skimr",
              "ggsci",
              "gridExtra",
              "R0",
              "remotes")


install.packages(paquetes)

## Instala paquetes de Git Hub

paquetes_git <- c("annecori/EpiEstim",
                  "reconhub/projections")

remotes::install_github(paquetes_git)

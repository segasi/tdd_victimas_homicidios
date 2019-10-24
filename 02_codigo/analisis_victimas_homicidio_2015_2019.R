### Cargar paquetes, definir setup general y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----
# Fuente: SNSP

victimas <- 
  read_excel("../../../../../10 recursos/datos/snsp/victimas/Estatal-V°ctimas - septiembre 2019.xlsx") %>% 
  clean_names()

### Tidyear datos ----
victimas <- 
  victimas %>% 
  gather(enero:diciembre, 
         key = "mes",
         value = "num_victimas")

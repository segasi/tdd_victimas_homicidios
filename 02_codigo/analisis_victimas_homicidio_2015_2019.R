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
         value = "numero")

### Generar diversas variables ----
victimas <- 
  victimas %>% 
  # Generar variables
  mutate(mes_num = case_when(mes == "enero" ~ 1,
                             mes == "febrero" ~ 2,
                             mes == "marzo" ~ 3,
                             mes == "abril" ~ 4,
                             mes == "mayo" ~ 5,
                             mes == "junio" ~ 6,
                             mes == "julio" ~ 7,
                             mes == "agosto" ~ 8,
                             mes == "septiembre" ~ 9,
                             mes == "octubre" ~ 10,
                             mes == "noviembre" ~ 11,
                             mes == "diciembre" ~ 12), 
         fecha = make_date(ano, mes_num),
         admin = ifelse(fecha < as.Date("2018-12-01"), "Peña Nieto", "AMLO")) %>% 
  # Reordenar variables
  select(fecha, año = ano, mes, mes_num, everything())

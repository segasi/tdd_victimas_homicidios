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


### Gráficas del número mensual de víctimas de homicidio doloso, 2015-2019 ----

# Versión 1 - Rango eje y automático

victimas %>% 
  # Filtrar datos
  filter(fecha < as_date("2019-10-01"),
         subtipo_de_delito == "Homicidio doloso") %>% 
  # Calcular número mensual de vícitimas
  group_by(fecha) %>% 
  summarise(victimas_x_mes = sum(numero), 
            admin = last(admin)) %>% 
  ungroup() %>% 
  # Graficar
  ggplot(aes(x = fecha,
             y = victimas_x_mes)) +
  annotate(geom = "rect",  xmin = as_date("2018-12-01"), xmax = as_date("2019-09-01"), ymin = -Inf, ymax = Inf, fill = "#af272f", alpha = 0.3) +
  annotate(geom = "text", x = as_date("2018-06-15"), y = 1200, label = "Peña Nieto", fontface = "bold", color = "grey40", hjust = 0.5, size = 8, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_date("2019-04-20"), y = 1200, label = "AMLO", fontface = "bold", color = "white", size = 8, family = "Trebuchet MS Bold") +
  # Incluir tramo que une última observación de Peña Nieto con la primera de AMLO
  geom_segment(x = as_date("2018-11-01"), xend = as_date("2018-12-01"), y = 2728, yend = 2892, color = "grey30", size = 2, lineend = "round") +
  # Gráfica con datos mensuales
  geom_line(aes(color = admin), size = 2) +
  # Línea de regresión lineal por administración
  geom_smooth(aes(color = admin), method = "lm", se = F, size = 1, linetype = 2) +
  scale_x_date(breaks = seq(from = as_date("2015-01-01"), length = 11, by = "6 month"), date_labels = "%b %Y") +
  scale_y_continuous(labels = comma, breaks = seq(0, 3000, 250)) +
  scale_color_manual(values = c("white", "grey30")) +
  labs(title = str_wrap(str_to_upper("número mensual de víctimas de homicidio doloso, 2015-2019"), width = 70),
       subtitle = "Datos a septiembre de 2019",
       x = "\n",
       y = "Víctimas por mes\n",
       caption = "@segasi / Fuente: SNSP") +
  tema +
  theme(legend.position = "none") +
  ggsave("03_graficas/numero_mensual_victimas_homicidio_doloso.png", width = 14, height = 10, dpi = 200)


# Versión 2 - Rango eje y de 0 a 3500 ----

victimas %>% 
  # Filtrar datos
  filter(fecha < as_date("2019-10-01"),
         subtipo_de_delito == "Homicidio doloso") %>% 
  # Calcular número mensual de vícitimas
  group_by(fecha) %>% 
  summarise(victimas_x_mes = sum(numero), 
            admin = last(admin)) %>% 
  ungroup() %>% 
  # Graficar
  ggplot(aes(x = fecha,
             y = victimas_x_mes)) +
  annotate(geom = "rect",  xmin = as_date("2018-12-01"), xmax = as_date("2019-09-01"), ymin = -Inf, ymax = Inf, fill = "#af272f", alpha = 0.3) +
  annotate(geom = "text", x = as_date("2018-06-15"), y = 1200, label = "Peña Nieto", fontface = "bold", color = "grey40", hjust = 0.5, size = 8, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_date("2019-04-20"), y = 1200, label = "AMLO", fontface = "bold", color = "white", size = 8, family = "Trebuchet MS Bold") +
  # Incluir tramo que une última observación de Peña Nieto con la primera de AMLO
  geom_segment(x = as_date("2018-11-01"), xend = as_date("2018-12-01"), y = 2728, yend = 2892, color = "grey30", size = 2, lineend = "round") +
  # Gráfica con datos mensuales
  geom_line(aes(color = admin), size = 2) +
  # Línea de regresión lineal por administración
  geom_smooth(aes(color = admin), method = "lm", se = F, size = 1, linetype = 2) +
  scale_x_date(breaks = seq(from = as_date("2015-01-01"), length = 11, by = "6 month"), date_labels = "%b %Y") +
  scale_y_continuous(labels = comma, breaks = seq(0, 3500, 250), limits = c(0, 3500)) +
  scale_color_manual(values = c("white", "grey30")) +
  labs(title = str_wrap(str_to_upper("número mensual de víctimas de homicidio doloso, 2015-2019"), width = 70),
       subtitle = "Datos a septiembre de 2019",
       x = "\n",
       y = "Víctimas por mes\n",
       caption = "@segasi / Fuente: SNSP") +
  tema +
  theme(legend.position = "none") +
  ggsave("03_graficas/numero_mensual_victimas_homicidio_doloso_rango_y_0_3500.png", width = 14, height = 10, dpi = 200)

# Versión 3 - Rango eje y de 500 a 3500 ----

victimas %>% 
  # Filtrar datos
  filter(fecha < as_date("2019-10-01"),
         subtipo_de_delito == "Homicidio doloso") %>% 
  # Calcular número mensual de vícitimas
  group_by(fecha) %>% 
  summarise(victimas_x_mes = sum(numero), 
            admin = last(admin)) %>% 
  ungroup() %>% 
  # Graficar
  ggplot(aes(x = fecha,
             y = victimas_x_mes)) +
  annotate(geom = "rect",  xmin = as_date("2018-12-01"), xmax = as_date("2019-09-01"), ymin = -Inf, ymax = Inf, fill = "#af272f", alpha = 0.3) +
  annotate(geom = "text", x = as_date("2018-06-15"), y = 1200, label = "Peña Nieto", fontface = "bold", color = "grey40", hjust = 0.5, size = 8, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_date("2019-04-20"), y = 1200, label = "AMLO", fontface = "bold", color = "white", size = 8, family = "Trebuchet MS Bold") +
  # Incluir tramo que une última observación de Peña Nieto con la primera de AMLO
  geom_segment(x = as_date("2018-11-01"), xend = as_date("2018-12-01"), y = 2728, yend = 2892, color = "grey30", size = 2, lineend = "round") +
  # Gráfica con datos mensuales
  geom_line(aes(color = admin), size = 2) +
  # Línea de regresión lineal por administración
  geom_smooth(aes(color = admin), method = "lm", se = F, size = 1, linetype = 2) +
  scale_x_date(breaks = seq(from = as_date("2015-01-01"), length = 11, by = "6 month"), date_labels = "%b %Y") +
  scale_y_continuous(labels = comma, breaks = seq(0, 3500, 250), limits = c(500, 3500)) +
  scale_color_manual(values = c("white", "grey30")) +
  labs(title = str_wrap(str_to_upper("número mensual de víctimas de homicidio doloso, 2015-2019"), width = 70),
       subtitle = "Datos a septiembre de 2019",
       x = "\n",
       y = "Víctimas por mes\n",
       caption = "@segasi / Fuente: SNSP") +
  tema +
  theme(legend.position = "none") +
  ggsave("03_graficas/numero_mensual_victimas_homicidio_doloso_rango_y_500_3500.png", width = 14, height = 10, dpi = 200)
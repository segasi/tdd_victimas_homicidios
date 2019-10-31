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



### Gráfica del número y tendencia de víctimas mensuales de homicidio doloso, 2015-2019 ----

# Generar vector con fechas
fechas <- 
  victimas %>% 
  filter(fecha < as_date("2019-10-01")) %>% 
  distinct(fecha) %>% 
  arrange(fecha) %>% 
  pull()

# Generar vector con los datos a desagregar
datos <- 
  victimas %>% 
  filter(fecha < as_date("2019-10-01")) %>% 
  group_by(fecha, subtipo_de_delito) %>% 
  summarise(victimas_x_mes = sum(numero)) %>% 
  ungroup() %>%
  filter(subtipo_de_delito == "Homicidio doloso") %>%
  select(victimas_x_mes) %>% 
  pull()

# Transformar datos en objeto ts
datos_ts <- ts(datos, start = 2015, frequency = 12)

# Separar con método X11
separacion_x11 <- datos_ts %>% seas(x11 = "") 
tendencia_x11 <- trendcycle(separacion_x11)
ajuste_estacional_x11 <- seasadj(separacion_x11)

# Juntar en un tibble la tendencia y el ajuste estacional
datos_x11 <- 
  tibble(fecha = fechas, 
         tendencia_x11 = trendcycle(separacion_x11),
         ajuste_estacional_x11 = seasadj(separacion_x11))

# Separar con método STL
datos_stl <- 
  datos_ts %>% 
  stl(t.window = 13, s.window = 7, robust = F) %>% 
  sweep::sw_tidy_decomp()


# Juntar los resultados obtenidos con ambos métodos
datos_todos <- bind_cols(datos_stl, datos_x11)

# Procesar datos para graficar
datos_para_grafica <- 
  datos_todos %>% 
  gather(key = "variable",
         value = "valor",
         -c(index, fecha)) %>% 
  mutate(variable = case_when(variable == "observed" ~ "Valores observados",
                              variable == "tendencia_x11" ~ "Tendencia-cíclo | Método X11",
                              variable == "trend" ~ "Tendencia-cíclo | Método STL",
                              TRUE ~ variable))

# Graficar
datos_para_grafica %>%
  filter(variable %in% c("Valores observados", "Tendencia-cíclo | Método X11", "Tendencia-cíclo | Método STL")) %>%  
  mutate(variable  = fct_relevel(variable, "Valores observados", "Tendencia-cíclo | Método X11", "Tendencia-cíclo | Método STL")) %>% 
  arrange(desc(variable)) %>% 
  ggplot(aes(x = fecha, y = valor, color = variable)) +
  annotate(geom = "rect", 
           xmin = as_date("2018-12-01"), 
           xmax = as_date("2019-09-01"),
           ymin = -Inf, ymax = Inf, fill = "#af272f", alpha = 0.3) +
  annotate(geom = "text", x = as_date("2018-06-15"), y = min(datos_para_grafica$valor[datos_para_grafica$variable == "Valores observados"] - 10), label = "Peña Nieto", fontface = "bold", color = "grey40", hjust = 0.5, size = 8, family = "Trebuchet MS Bold") +
  annotate(geom = "text", x = as_date("2019-04-20"), y = min(datos_para_grafica$valor[datos_para_grafica$variable == "Valores observados"] - 10), label = "AMLO", fontface = "bold", color = "white", size = 8, family = "Trebuchet MS Bold") +
  geom_line(size = 2, alpha = 0.8) +
  # Recuadro primer punto de inflexión
  annotate(geom = "rect", 
           xmin = as_date("2017-10-15"), 
           xmax = as_date("2018-01-15"),
           ymin = 2550, ymax = 2740, 
           fill = "transparent", 
           color = "grey30",
           size = 1) +
  # Texto primer punto de inflexión
  annotate(geom = "text", x = as_date("2017-11-23"), y = 2400, label = "Primer punto\nde inflexión", fontface = "bold", color = "grey40", hjust = 0.5, size = 4, family = "Trebuchet MS Bold") +
  # Línea guía primer punto de inflexión
  annotate(geom = "segment", x = as_date("2017-11-27"), xend = as_date("2017-11-27"), y = 2470, yend = 2530, color = "grey30") +
  # Recuadro segundo punto de inflexión
  annotate(geom = "rect", 
           xmin = as_date("2018-09-15"), 
           xmax = as_date("2018-11-15"),
           ymin = 2830, ymax = 2930, 
           fill = "transparent", 
           color = "grey30",
           size = 1) +
  # Texto segundo punto de inflexión
  annotate(geom = "text", x = as_date("2018-10-10"), y = 2650, label = "Segundo punto\nde inflexión", fontface = "bold", color = "grey40", hjust = 0.5, size = 4, family = "Trebuchet MS Bold") +
  # Línea guía segundos puntos de inflexión
  annotate(geom = "segment", x = as_date("2018-10-15"), xend = as_date("2018-10-15"), y = 2710, yend = 2820, color = "grey30") +
  scale_x_date(breaks = seq(from = as_date("2015-01-01"), length = 11, by = "6 month"), date_labels = "%b %Y") +
  scale_y_continuous(labels = comma, breaks = seq(0, 3000, 250)) +
  scale_color_manual(values = c("grey70", "salmon", "steelblue")) +
  labs(title = str_wrap(str_to_upper("Víctimas mensuales de homicidio doloso, 2015-2019"), width = 70),
       subtitle = "Datos a septiembre de 2019",
       x = "\n",
       y = "Número de víctimas\n",
       color = NULL, 
       caption = "@segasi / Fuente: SNSP\n") +
  tema +
  theme(plot.title = element_text(size = 30),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.background = element_rect(color = "transparent", fill = "#ffffff30"),
        legend.position = c(0.155, 0.86), 
        legend.direction = "vertical") +
  ggsave("03_graficas/numero_mensual_y_tendencia_victimas_homicidio_doloso.png", width = 16, height = 10, dpi = 200)

# Tabla con los datos observados, las dos tendencias y sus respectivos cambios mensuales
datos_todos %>% 
  select(index, fecha, observed, trend, tendencia_x11) %>% 
  mutate(cambio_trend = trend - lag(trend),
         cambio_tendencia_x11 = as.numeric(tendencia_x11) - lag(as.numeric(tendencia_x11))) %>% 
  select(index, fecha, observed, trend, cambio_trend, tendencia_x11, cambio_tendencia_x11) %>% 
  print(n = Inf)






### Gráfica del número acumulado mensualmente de víctimas de homicidio doloso, por año, 2015-2019 ----
victimas %>% 
  filter(fecha < as_date("2019-10-01"),
         subtipo_de_delito == "Homicidio doloso") %>% 
  group_by(fecha) %>% 
  summarise(victimas_x_mes = sum(numero), 
            mes = str_to_title(last(mes)),
            año = mean(año), 
            admin = last(admin), 
            mes_num = mean(mes_num)) %>% 
  ungroup() %>% 
  arrange(fecha) %>% 
  group_by(año) %>% 
  mutate(victimas_acumuladas = cumsum(victimas_x_mes)) %>% 
  ungroup() %>% 
  select(fecha, año, mes, mes_num, victimas_x_mes, victimas_acumuladas, admin) %>% 
  mutate(mes = fct_relevel(mes, "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
         etiqueta_año = ifelse(año != 2019 & mes == "Diciembre", año, ""),
         etiqueta_amlo = ifelse(año == 2019 & mes == "Septiembre", año, "")) %>% 
  ggplot(aes(x = mes_num, y = victimas_acumuladas, group = año)) +
  geom_line(aes(color = admin), size = 2, alpha = 1) +
  geom_point(aes(color = admin), size = 1.6, alpha = 1) +
  geom_text(aes(label = etiqueta_año), family = "Trebuchet MS Bold", hjust = -0.2, size = 6, color = "grey40") +
  geom_text(aes(label = etiqueta_amlo), family = "Trebuchet MS Bold", vjust = -0.4, size = 6, color = "grey40") +
  scale_y_continuous(labels = comma, breaks = seq(0, 40000, 2500)) +
  scale_x_continuous(limits = c(1, 12.2), breaks = 1:12, labels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")) +
  scale_color_manual(values = c("#af272f", "grey70")) +
  labs(title = str_wrap(str_to_upper("Número acumulado mensualmente de víctimas de homicidio doloso, por año, 2015-2019"), width = 60),
       subtitle = "Datos a septiembre de 2019",
       x = "\n",
       y = "Víctimas acumuladas\n",
       color = NULL, 
       caption = "@segasi / Fuente: SNSP\n") +
  tema +
  theme(legend.text = element_text(size = 15),
        legend.background = element_rect(color = "#d3d3d380", fill = "#d3d3d330"),
        legend.position = c(0.85, -0.12), 
        legend.direction = "horizontal") +
  ggsave("03_graficas/numero_acumulado_mensual_victimas_homicidio_doloso_por_año.png", width = 14.5, height = 10, dpi = 200)


victimas %>% 
  filter(subtipo_de_delito == "Homicidio doloso") %>% 
  group_by(entidad, fecha) %>% 
  summarise(victimas_x_mes = sum(numero), 
            admin = last(admin)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fecha,
             y = victimas_x_mes,
             color = admin)) +
  geom_line() +
  geom_smooth(se = F, color = "black") +
  facet_wrap(~ entidad, ncol = 8)

victimas %>% 
  filter(subtipo_de_delito == "Homicidio doloso",
         fecha < as_date("2019-10-01")) %>% 
  group_by(fecha) %>% 
  summarise(victimas_x_mes = sum(numero), 
            admin = last(admin)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fecha,
             y = victimas_x_mes,
             color = admin)) +
  geom_line() +
  geom_smooth(method = "lm", se = F) 


victimas %>% 
  filter(subtipo_de_delito == "Homicidio doloso",
         fecha < as_date("2019-10-01")) %>% 
  group_by(fecha) %>% 
  summarise(victimas_x_mes = sum(numero), 
            admin = last(admin)) %>% 
  ungroup() %>% 
  # Promedio móvil de tres meses
  mutate(media_movil = rollmean(victimas_x_mes, 
                                k = 3, 
                                fill = NA,
                                align = "right")) %>% 
  ggplot(aes(x = fecha,
             y = victimas_x_mes)) +
  geom_line() +
  # geom_line(aes(x = fecha,
  #               y = media_movil), 
  #           color = "grey40") +
  geom_smooth(se = F) +
  geom_smooth(method = "lm", aes(color = admin, se = F))
  

victimas %>% 
  filter(subtipo_de_delito == "Homicidio doloso") %>% 
  group_by(entidad, fecha) %>% 
  summarise(victimas_x_mes = sum(numero), 
            admin = last(admin)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fecha,
             y = victimas_x_mes,
             color = admin)) +
  geom_point() +
  geom_smooth(se = T, color = "black")

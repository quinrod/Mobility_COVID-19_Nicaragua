## Combining 

regulares  <-  c(34,22,22,33,36,56,29,47,70,65,46,23,29,48,55,47,56,53,50,52,46)
prevención <-  c(16, 8,10, 0,13,18,26,27,25,30,26,14,13,23,13,15,20,12, 6,10,11)
State      <-  as.character(c("Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua","Nicaragua"))
Fecha      <- as.Date(c("2020-03-18","2020-03-25","2020-04-01","2020-04-08","2020-04-15","2020-04-22","2020-04-29","2020-05-06","2020-05-13","2020-05-20","2020-05-27","2020-06-03","2020-06-10","2020-06-17","2020-06-24","2020-07-01","2020-07-08","2020-07-15","2020-07-22","2020-07-29","2020-08-05"))
protestas <- data.frame(State,Fecha,regulares,prevención) %>%
             mutate(todas = regulares + prevención)

nue <- new[["data"]] 

movilidad_cases <- left_join(nue, movilidad_nica_final, by = c("State","Fecha")) %>%
                   left_join(.,protestas,by = c("State","Fecha")) %>%
  group_by(State, Actividad) %>%
  mutate(movilidad = ifelse(Days==1, 0, mov_desde_lineabase - last(mov_desde_lineabase))) 
                            
## tiendas_y_ocio. paradas_de_transporte, supermercados_y_farmacias, parques, locales_de_trabajo, zonas_residenciales

# A. Fallecidos y movilidad

selected_dep <- c('Nicaragua')
selected_act <- c('Tiendas y ocio')

# Create graphs
movilidad_cases %>%
  filter(State %in% selected_dep, Actividad %in% selected_act) %>%
  group_by(State) %>%
  mutate(mobility = rollmean(movilidad, k = 7, fill = NA)) %>%
  ungroup() %>%
  rename(., Días = Days) %>%
  ggplot(x=Días) + 
  labs(title = "Relación de nuevos casos y movilidad en Managua", 
       caption = "Fuente: Google movilidad y Observatorio Ciudadano COVID-19") +
  geom_bar(aes(Días, mov), stat = "identity", size=.1, color="red", alpha=.4) +
  geom_bar(aes(Días, mobility), stat = "identity", size=.1, color="black", alpha=.4) +
  xlab("Días desde 1er caso") +
  scale_y_continuous(name = "Nuevos casos (media móvil 7 días)", sec.axis = sec_axis(~.*1, name = "Movilidad ocio (media móvil 7 días)",
                                                                            breaks = seq(-40,0, by = 20)),
                     labels = comma) +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, vjust = 0.9, size = 8)) 

# B. Fallecidos y actividades de prevención

selected_dep <- c('Nicaragua')
selected_act <- c('Tiendas y ocio')

cases <- movilidad_cases %>%
  filter(State %in% selected_dep, Actividad %in% selected_act) %>%
  group_by(State) %>%
  mutate(mobility = rollmean(movilidad, k = 7, fill = NA)) %>%
  ungroup() %>%
  rename(., Días = Days)

deaths <- ggplot(cases, aes(x = Fecha, y = mov)) + geom_line(color = "red")
riots  <- ggplot(cases, aes(x = Fecha, y = prevención)) + geom_bar(color = "black", stat = "identity", size=1, alpha=.4)

ggarrange(deaths, riots)

calc_fudge_axis = function(y1, y2) {
  
  ylim1 <- range(y1, na.rm = TRUE)
  ylim2 <- range(y2, na.rm = TRUE)
  
  mult <- (ylim1[2] - ylim1[1]) / (ylim2[2] - ylim2[1])
  miny1 <- ylim1[1]
  miny2 <- ylim2[1]
  
  cast_to_y1 = function(x) {
    (mult * (x - miny2)) + miny1
  }
  
  yf <- cast_to_y1(y2)
  
  labelsyf <- pretty(y2)
  return(
    list(
      yf = yf,
      labels = labelsyf,
      breaks = cast_to_y1(labelsyf),
      zero = cast_to_y1(0)
    ))
}

rescaledVals <- calc_fudge_axis(cases$mov, y2 = cases$prevención)

# check if the ranges are equal
range(rescaledVals$yf, na.rm = TRUE) == range(cases$mov, na.rm = TRUE)

cases <- cases %>% mutate(prevenciónScaled = rescaledVals$yf)

pFinal <- ggplot(cases) + 
  geom_line(aes(x = Fecha, y = mov),color = "red", size = 1) + 
  geom_bar(aes(x = Fecha, y = prevenciónScaled), color = "black", stat = "identity", size=1) + 
  labs(title = "Relación de fallecidos y visitas prevención en Nicaragua", 
       caption = "Fuente: Sismología Social y Observatorio Ciudadano COVID-19") +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, vjust = 0.9, size = 8)) +
  scale_y_continuous(name = "Nuevos casos (media móvil 7 días)",
    sec.axis = dup_axis(breaks = rescaledVals$breaks, labels = paste0(rescaledVals$labels * 1), name = "No. de actividades de prevención")
  )

# Compare final plot wiht sales plot
ggarrange(pFinal, deaths)

# C.

# Create graphs
movilidad_cases %>%
  filter(State %in% selected_dep, Actividad %in% selected_act) %>%
  group_by(State) %>%
  mutate(mobility = rollmean(movilidad, k = 7, fill = NA)) %>%
  ungroup() %>%
  rename(., Días = Days) %>%
  ggplot(x=Fecha) + 
  labs(title = "Relación de nuevos muertes y actividades de prevención públicas en Nicaragua", 
       caption = "Fuente: Sismología Social y Observatorio Ciudadano COVID-19") +
  geom_path(aes(Fecha, mov), stat = "identity", size=1, color="red", alpha=.4) +
  geom_bar(aes(Fecha, prevención), stat = "identity", size=1, color="black", alpha=.4) +
  xlab("Días desde 1er caso") +
  scale_y_continuous(name = "No. de actividades de prevención",limits=c(0, 30),
                     sec.axis = sec_axis(~.*1, name = prevención,
                                breaks = seq(0,50, by = 10)),
                     labels = comma) +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, vjust = 0.9, size = 8)) 





 
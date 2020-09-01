## Combining 

nue <- new[["data"]] 

movilidad_cases <- left_join(nue, movilidad_nica_final, by = c("State","Fecha")) %>%
  group_by(State, Actividad) %>%
  mutate(movilidad = ifelse(Days==1, 0, mov_desde_lineabase - last(mov_desde_lineabase))) 
                            
## tiendas_y_ocio. paradas_de_transporte, supermercados_y_farmacias, parques, locales_de_trabajo, zonas_residenciales

selected_dep <- c('Managua')
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
 
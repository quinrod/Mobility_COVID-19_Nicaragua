library(data.table)
library(zoo)
library(RCurl)

# extract data
movilidad_global <- getURL("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=cc1386e74c2395f8")
movilidad_global <- read.csv(text = movilidad_global)

# filter Nicaragua
movilidad_nica <- movilidad_global %>% 
  filter (country_region == 'Nicaragua') %>%
  select (sub_region_1, metro_area, date:residential_percent_change_from_baseline) %>%
  mutate (sub_region_1 = ifelse(metro_area == 'Managua Metropolitan Area', 'Managua Área Metropolitana', as.character(sub_region_1)),
          sub_region_1 = ifelse(sub_region_1 == "", 'Nicaragua', as.character(sub_region_1))) %>%
  select (-metro_area) %>%
  rename(., State = sub_region_1, 
         tiendas_y_ocio = retail_and_recreation_percent_change_from_baseline,
         paradas_de_transporte = transit_stations_percent_change_from_baseline,
         supermercados_y_farmacias = grocery_and_pharmacy_percent_change_from_baseline,
         parques = parks_percent_change_from_baseline,
         locales_de_trabajo = workplaces_percent_change_from_baseline,
         zonas_residenciales = residential_percent_change_from_baseline)

# reshape
movilidad_nica_final <- movilidad_nica %>%
  reshape2::melt(.) %>%
  set_colnames(c('State', 'Fecha', 'Actividad', 'mov_desde_lineabase')) %>%
  mutate(Fecha = as.Date(Fecha,"%Y-%m-%d"),
         dias = as.numeric(Fecha),
         State = ifelse(State == 'Leon', 'León',as.character(State)),
         State = ifelse(State == 'Esteli', 'Estelí',as.character(State)),
         State = ifelse(State == 'South Caribbean Coast Autonomous Region', 'RACCS',as.character(State)),
         Actividad = ifelse(Actividad == 'tiendas_y_ocio', 'Tiendas y ocio',as.character(Actividad)),
         Actividad = ifelse(Actividad == 'paradas_de_transporte', 'Paradas de bus',as.character(Actividad)),
         Actividad = ifelse(Actividad == 'supermercados_y_farmacias', 'Supermercados y farmacias',as.character(Actividad)),
         Actividad = ifelse(Actividad == 'parques', 'Parques',as.character(Actividad)),
         Actividad = ifelse(Actividad == 'locales_de_trabajo', 'Locales de trabajo',as.character(Actividad)),
         Actividad = ifelse(Actividad == 'zonas_residenciales', 'Zonas residenciales',as.character(Actividad))
         )

# create filter
selected_dep <- c('Nicaragua')
selected_act <- c('Tiendas y ocio','Paradas de bus',
                  'Supermercado y farmacias') #'Parques','Locales de trabajo','zonas_residenciales'

## paradas_de_transporte, supermercados_y_farmacias, parques, locales_de_trabajo, zonas_residenciales
dev.off()

# Create graphs
mov_nica <- movilidad_nica_final %>%
  filter(State %in% selected_dep, Actividad %in% selected_act) %>%
  group_by(State) %>%
  mutate(mov = rollmean(mov_desde_lineabase, k = 7, fill = NA)) %>%
  ungroup() %>%
  ggplot(aes(Fecha, mov, col = Actividad)) +
  geom_line(show.legen=TRUE) + 
  ylab("Media móvil de 1 mes desde línea de base") +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  labs(title = "Tendencia de movilidad en Nicaragua", 
       caption = "Fuente: Google movilidad") +
  scale_x_date(labels = date_format("%b %Y")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 3),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 10))

figures <- "/Users/quinrod/projects/GitHub/Mobility-COVID-19_Nicaragua/figures/"
ggsave(paste(figures,'movilidad.png'), 
       device = "png", 
       width = 16,
       height = 12,
       units = 'in')

#+transition_reveal(Fecha) 

# save as a GIF
animate(mov_nica, fps = 10, width = 750, height = 450)
anim_save("/Users/quinrod/projects/GitHub/Mobility-COVID-19_Nicaragua/figures/movilidad.gif")

mov_nica <- movilidad_nica_final %>%
  filter(State %in% selected_dep, Actividad %in% selected_act) %>%
  group_by(State) %>%
  mutate(mov = rollmean(mov_desde_lineabase, k = 4, fill = NA)) %>%
  ungroup() %>%
  ggplot(aes(Fecha, mov, col = Actividad)) +
  geom_point(shape = 21, aes(fill = mov), size = 5, stroke = 1) + 
  geom_line() + 
  ylab("Media móvil de 1 mes desde línea de base") +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  labs(title = "Tendencia de movilidad en Nicaragua", 
       caption = "Source: Google movilidad") 


  
  
  
  
  
  
  

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
  mutate (sub_region_1 = ifelse(metro_area == 'Managua Metropolitan Area', 'Managua', as.character(sub_region_1)),
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
  set_colnames(c('State', 'fecha', 'actividad', 'mov_desde_lineabase')) %>%
  mutate(fecha = as.Date(fecha,"%Y-%m-%d"),
         dias = as.numeric(fecha))

# create filter
selected_dep <- c('Nicaragua')
selected_act <- c('tiendas_y_ocio','paradas_de_transporte',
                  'supermercados_y_farmacias',' parques',
                  'locales_de_trabajo','zonas_residenciales')

## paradas_de_transporte, supermercados_y_farmacias, parques, locales_de_trabajo, zonas_residenciales
dev.off()
# Create graphs
mov_nica <- movilidad_nica_final %>%
  filter(State %in% selected_dep, actividad %in% selected_act) %>%
  group_by(State) %>%
  mutate(mov = rollmean(mov_desde_lineabase, k = 4, fill = NA)) %>%
  ungroup() %>%
  ggplot(aes(fecha, mov, col = actividad)) +
  geom_line(show.legen=TRUE) + 
  ylab("Media móvil de 1 mes desde línea de base") +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  labs(title = "Tendencia de movilidad en Nicaragua", 
       caption = "Fuente: Google movilidad") 

figures <- "/Users/quinrod/projects/GitHub/Mobility-COVID-19_Nicaragua/figures/"
ggsave(paste(figures,'movilidad.png'), 
       device = "png", 
       width = 16,
       height = 12,
       units = 'in')

#+transition_reveal(fecha) 

# save as a GIF
animate(mov_nica, fps = 10, width = 750, height = 450)
anim_save("/Users/quinrod/projects/GitHub/Mobility-COVID-19_Nicaragua/figures/movilidad.gif")

mov_nica <- movilidad_nica_final %>%
  filter(State %in% selected_dep, actividad %in% selected_act) %>%
  group_by(State) %>%
  mutate(mov = rollmean(mov_desde_lineabase, k = 4, fill = NA)) %>%
  ungroup() %>%
  ggplot(aes(fecha, mov, col = actividad)) +
  geom_point(shape = 21, aes(fill = mov), size = 5, stroke = 1) + 
  geom_line() + 
  ylab("Media móvil de 1 mes desde línea de base") +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  labs(title = "Tendencia de movilidad en Nicaragua", 
       caption = "Source: Google movilidad") 


  
  
  
  
  
  
  

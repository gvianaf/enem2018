
# o arquivo é gigante, melhor aumentar o limite de memória
memory.limit(24576)

# lê os dddos. vai demorar
enem_2018 <- data.table::fread(input='./microdados_enem2018/DADOS/microdados_enem_2018.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

# tabela de participantes que 
# não têm acesso à internet Q025 == A
# nâo têm pc em casa Q024 == A
sem_int_pc <- enem_2018[Q025 == "A" & Q024 == "A", 
                        .N,
                        SG_UF_RESIDENCIA][]

# tabela com o total de inscritos
inscritos <- enem_2018[, .N, SG_UF_RESIDENCIA]
names(inscritos) <- c("SG_UF_RESIDENCIA", "Total")

# junta, calcula o percentual e mantém apenas UF e percentual
percentual_sem <- merge(sem_int_pc, inscritos)
percentual_sem$percentual <- percentual_sem$N / percentual_sem$Total
percentual_sem$N <- NULL
percentual_sem$Total <- NULL

# para não termos que fazer novamente
# prefiro exportar os resultados
rio::export(percentual_sem, "prop_sem_internet_pc.xlsx")

# Mapa shape & ggplot -----------------------------------------------------

library(tidyverse)
library(rgdal)
library(ggplot2)

percentual_sem <- rio::import("prop_sem_internet_pc.xlsx")

shape <- readOGR(dsn = "mapa-br\\.",
                 layer = "BR_UF_2019",
                 encoding = "UTF-8",
                 verbose = FALSE)

# fortify
shape_df <- broom::tidy(shape, region = "SIGLA_UF")

# merge
names(percentual_sem) <- c("id", "percentual")   # renomeia para terem o mesmo nome
shape_df <- merge(shape_df, percentual_sem)

# basic plot
graf <- shape_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = percentual),
               color = alpha("white", 0.6), size = 0.2) + 
  scale_fill_viridis_c() +
  coord_map()

graf

# plot viridis
library(showtext)
font_add("fira", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/FiraSans-Regular.ttf")
font_add("fira-bold", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/FiraSans-Bold.ttf")
showtext_auto()

theme_set(theme_void(base_family = "fira"))
theme_update(plot.caption = element_text(hjust = 1),
             plot.margin = margin(0,0,1,0, "cm"),
             plot.title = element_text(family = "fira-bold"))

# pontos centrais, para botar o label
centroide <- cbind.data.frame(data.frame(rgeos::gCentroid(shape, byid = TRUE),
                                         id = shape@data$SIGLA_UF))

graf <- shape_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = percentual),
               color = alpha("white", 0.6), size = 0.2) + 
  geom_text(data = centroide,
            aes(x = x, y = y, label = id),
            family = "fira", 
            size = 1.5, 
            color = "slategrey") +
  scale_fill_viridis_c(breaks = seq(0, 1, by = .1), 
                       direction = -1,
                       name = "Percentual",
                       labels = function(x) scales::percent(x, accuracy = 1L)) +
  coord_map() +
  labs(title = "Percentual de estudantes que participaram do ENEM 2018 e declararam não ter, conjuntamente,\nnem acesso à internet nem computador em casa",
       subtitle = "Nos estados do Norte, pelo menos 36% dos estudantes têm muitas dificuldades de conexão",
       caption = "Fonte: INEP em http://inep.gov.br/microdados\nhttps://github.com/gvianaf")

graf
ggsave("enem2018_internet_pc_v.pdf", width = 13, height = 8, device = cairo_pdf)
pdftools::pdf_convert("enem2018_internet_pc_v.pdf", format = "png", dpi = 350)

# plot magma
graf <- shape_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = percentual),
               color = alpha("white", 0.6), size = 0.2) + 
  geom_text(data = centroide,
            aes(x = x, y = y, label = id),
            family = "fira", 
            size = 1.5, 
            color = "slategrey") +
  scale_fill_viridis_c(option = "magma", 
                       direction = -1,
                       breaks = seq(0, 1, by = .1),
                       name = "Percentual",
                       labels = function(x) scales::percent(x, accuracy = 1L)) +
  coord_map() +
  labs(title = "Percentual de estudantes que participaram do ENEM 2018 e declararam não ter, conjuntamente,\nnem acesso à internet nem computador em casa",
       subtitle = "Nos estados do Norte, pelo menos 36% dos estudantes têm muitas dificuldades de conexão",
       caption = "Fonte: INEP em http://inep.gov.br/microdados\nhttps://github.com/gvianaf")

graf
ggsave("enem2018_internet_pc_m.pdf", width = 13, height = 8, device = cairo_pdf)
pdftools::pdf_convert("enem2018_internet_pc_m.pdf", format = "png", dpi = 350)

showtext_auto(FALSE)

# Mapa leaflet ------------------------------------------------------------

library(tidyverse)
library(sf)
library(leaflet)

percentual_sem <- rio::import("prop_sem_internet_pc.xlsx")

shape <- read_sf("mapa-br/BR_UF_2019.shp") %>% 
  # we want WSG84 latitude/longitute to use with leaflet
  st_transform(4326)   # https://community.rstudio.com/t/projection-problems-with-leaflet/27747/2

# merge
names(percentual_sem) <- c("SIGLA_UF", "Percentual")   # renomeia para terem o mesmo nome
shape <- merge(shape, percentual_sem)

Encoding(shape@data$NM_UF) <- "UTF-8"

# plot

mypalette <- colorNumeric(palette="viridis", NULL, reverse = TRUE)

mapa <- shape %>% 
  leaflet() %>% 
  addTiles() %>% 
  setView(lat = -15, lng = -47, zoom = 4) %>% 
  addPolygons(fillColor = ~mypalette(Percentual),
              stroke = FALSE,
              fillOpacity = 0.7,
              smoothFactor = 0.3,
              label = ~paste0(shape$NM_UF, ": ", scales::percent(shape$Percentual, accuracy = 1L))) %>% 
  addLegend("bottomright",
            pal = mypalette,
            values = ~Percentual*100,
            labFormat = labelFormat(suffix = "%"), 
            title = "Percentual")
mapa

# salva widget
htmlwidgets::saveWidget(mapa, "mapa_leaflet.html")

# Mapa hexgrid com geojson ------------------------------------------------
# não gostei...
# 
# library(geogrid)
# library(sf)
# library(tmap)
# 
# shape <- st_read("./mapa-br/BR_UF_2019.geojson") %>% 
#   st_set_crs(27700)
# 
# shape %>% 
#   tm_shape() +
#   tm_polygons() +
#   tm_text("SIGLA_UF")
# 
# par(mfrow = c(2, 2), mar = c(0, 0, 2, 0))
# for (i in 1:4) {
#   new_cells <- calculate_grid(shape = shape, grid_type = "hexagonal", seed = i)
#   plot(new_cells, main = paste("Seed", i, sep = " "))
# }
# 
# new_cells_hex <- calculate_grid(shape = shape, grid_type = "hexagonal", seed = 2)
# resulthex <- assign_polygons(shape, new_cells_hex)
# 
# resulthex %>% 
#   tm_shape() +
#   tm_polygons() +
#   tm_text("SIGLA_UF")
# 
# # merge data
# names(percentual_sem) <- c("SIGLA_UF", "Percentual")
# resulthex <- merge(resulthex, percentual_sem)
# 
# resulthex %>% 
#   tm_shape() +
#   tm_polygons("Percentual", palette = "viridis") +
#   tm_text("SIGLA_UF")

rm(list = ls())
library(openxlsx)
library(sf)
library(tidyverse)
library(leaflet)
library(viridis)
library(rmapshaper)
library(sf)
library(units)
library(scales)

read_sf("./communes.json") -> geo

read.xlsx("./deploiement.xlsx", sheet = 15) -> communes
names(communes) <- communes[2,]
communes <- communes[-(1:2),]
communes %>% 
    select(`Code commune`, `Nom commune`, `Logements`) %>% 
    mutate(Logements = round(as.numeric(Logements))) -> communes

read.xlsx("./deploiement.xlsx", sheet = 17) -> couverture
names(couverture) <- couverture[2,]
couverture <- couverture[-(1:2),]
couverture %>% 
    select(`Code commune`, `Champ`, `T4 2018`) %>% 
    filter(Champ != "Catégorie") %>% 
    select(`Code commune`, `T4 2018`) %>% 
    rename(Raccordables = `T4 2018`) -> couverture
couverture$Raccordables = as.numeric(couverture$Raccordables)

deploiement <- full_join(communes, couverture) 
rm(communes, couverture)

for (i in 1:nrow(deploiement)){
    if (deploiement$Raccordables[i] > deploiement$Logements[i]) {
        deploiement$Raccordables[i] <- deploiement$Logements[i]
    }
}

deploiement$Pourcentage <- deploiement$Raccordables/deploiement$Logements

geo$`Nom commune` <- geo$name
geo$area <- set_units(st_area(geo$geometry), km^2)

read.csv("./pop2.csv", 
         stringsAsFactors = FALSE,
         encoding = "UTF8") -> pop
pop <- pop[,-1]
colnames(pop) <- c("Code commune", "nom", "population")
pop %>% 
    group_by(`Code commune`) %>% 
    summarise(Population = round(sum(population))) -> pop

merged <- left_join(geo, deploiement, by = "Nom commune") 
merged <- merged %>% select(name, Pourcentage, area, geometry, `Code commune`)
merged <- left_join(merged, pop, by = "Code commune") 
merged %>% mutate(density = as.vector(Population / area)) %>% 
    select(name, Pourcentage, `Code commune`, density, geometry) -> merged

palftth <- colorNumeric("viridis", NULL)
paldens <- colorQuantile("viridis", NULL, n = 5)

# css_fix <- "div.info.legend.leaflet-control br {clear: both;}"

rm(deploiement, geo)

leaflet(merged, options = leafletOptions(preferCanvas = TRUE)) %>% 
    addTiles() %>% 
    addPolygons(stroke = FALSE, 
                smoothFactor = 0.2, 
                fillColor = ~palftth(Pourcentage),
                fillOpacity = 0.6,
                label = ~paste0(name,": ", round(Pourcentage * 100, 2)),
                group = "FTTH") %>% 
    addLegend("bottomright", 
              pal = palftth, 
              values = ~round(Pourcentage * 100, 2),
              title = "Part des bâtiments<br> raccordables FTTH, 2019",
              labFormat = labelFormat(suffix = "%"),
              opacity = 0.7,
              group = "FTTH") %>% 
    addPolygons(stroke = FALSE, 
            smoothFactor = 0.2, 
            fillColor = ~paldens(density),
            fillOpacity = 0.6,
            label = ~paste0(name,": ", round(density, 2)),
            group = "Densité de population") %>% 
    addLegend("bottomright", 
              pal = paldens, 
              values = ~round(density, 2),
              title = "Quantiles de densité<br> de population, 2016",
              opacity = 0.7,
              group = "Densité de population") %>% 
    addLayersControl(baseGroups = c("FTTH", "Densité de population"),
                     options = layersControlOptions(collapsed = FALSE))

merged2 <- left_join(merged, pop, by = "Code commune")

ggplot(merged2) +
    geom_point(aes(x = log(density), 
                   y = Pourcentage,
                   size = Population),
               shape = 1,
               alpha = 0.5,
               col = "mediumseagreen") +
    geom_smooth(aes(x = log(density), 
                    y = Pourcentage,
                    weight = Population),
                method = "lm",
                se = FALSE,
                col = "grey20") +
    scale_y_continuous(limits = c(0, 1),
                       labels = c("0%", "25%", "50%", "75%", "100%")) +
    scale_x_continuous(labels = comma) +
    scale_size_continuous(labels = comma, 
                          limits = c(0, 471941),
                          breaks = seq(from = 5000, 
                                       to = 350000, 
                                       by = 75000),
                          range = c(3, 18)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5, vjust = 30),
          plot.caption = element_text(size = 18, face = 3, vjust = -20),
          axis.text.x = element_text(size = 20, colour = "black"),
          axis.text.y = element_text(size = 20, colour = "black"),
          axis.title.x = element_text(size = 25, colour = "black", vjust = -5),
          axis.title.y = element_text(size = 25, colour = "black", vjust = 10),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "grey", size = 1),
          panel.grid.major.y = element_line(color = "grey", size = 1),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 22),
          legend.key = element_rect(fill = "white"),
          plot.margin = unit(c(3, 5, 3, 5), "cm")) +
    labs(title = "Densité de population et taux de bâtiments raccordables FttH",
         caption = "Données: ARCEP et INSEE") +
    xlab("Densité de population (échelle logarithmique)") +
    ylab("Taux de bâtiments raccordables par commune") -> m

png("merged2.png",
    height = 1000,
    width = 1600)
m
dev.off()

#library(widgetframe)
#frameWidget(m)
#https://github.com/rstudio/leaflet/issues/615

#library(htmlwidgets)
#saveWidget(m, file = "m.html", selfcontained = FALSE)

#pdf("./test.pdf")
#ggplot() +
#    geom_sf(data = merged, color = NA, aes(fill = Pourcentage))
#dev.off()

# https://stackoverflow.com/questions/34439928/embedding-an-r-htmlwidget-into-existing-webpage

#https://stackoverflow.com/questions/35386124/embedding-on-github

# simplify original communes with mapshaper
# C:\Users\jules>curl -f -o communes.zip --url "https://wambachers-osm.website/boundaries/exportBoundaries?cliVersion=1.0&cliKey=6f4b0380-1ef1-4cdf-ae75-0ce88e32e15a&exportAreas=land&from_AL=8&to_AL=8&exportFormat=json&union=false&selected=1403916"
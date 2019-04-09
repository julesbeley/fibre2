rm(list = ls())
library(openxlsx)
library(sf)
library(tidyverse)
library(leaflet)
library(viridis)
library(rmapshaper)

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

merged <- left_join(geo, deploiement, by = "Nom commune") 

merged <- merged %>% select(name, Pourcentage, geometry, `Code commune`)

pal <- colorNumeric("viridis", NULL)

leaflet(merged, options = leafletOptions(preferCanvas = TRUE)) %>% 
    addTiles() %>% 
    addPolygons(stroke = FALSE, 
                smoothFactor = 0.8, 
                fillColor = ~pal(Pourcentage),
                fillOpacity = 0.6)

#pdf("./test.pdf")
#ggplot() +
#    geom_sf(data = merged, color = NA, aes(fill = Pourcentage))
#dev.off()

# simplify original communes with mapshaper
# C:\Users\jules>curl -f -o communes.zip --url "https://wambachers-osm.website/boundaries/exportBoundaries?cliVersion=1.0&cliKey=6f4b0380-1ef1-4cdf-ae75-0ce88e32e15a&exportAreas=land&from_AL=8&to_AL=8&exportFormat=json&union=false&selected=1403916"
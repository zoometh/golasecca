# quakes1 <- quakes[1:10,]
library(leaflet)

f.leafIcons <- function(sit.per){
  leafIcons <- icons(
    # leafIcons
    iconUrl = ifelse(sit.per$contexte.site %in% c('funeraire'),
                     paste0(getwd(),"/img/s_funeraire.png"),
                     ifelse(sit.per$contexte.site %in% c('indetermine','non_rens'),
                            paste0(getwd(),"/img/s_indet.png"),
                            ifelse(sit.per$contexte.site %in% c('depot','sanctuaire','fluvial'),
                                   paste0(getwd(),"/img/s_depot.png"),
                                   ifelse(sit.per$contexte.site %in% c('habitat'),
                                          paste0(getwd(),"/img/s_habitat.png"), paste0(getwd(),"/img/s_autres.png"))))
    ),
    iconWidth = 15, iconHeight = 15
    # iconAnchorX = 22, iconAnchorY = 94
    # shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
    # shadowWidth = 50, shadowHeight = 64,
    # shadowAnchorX = 4, shadowAnchorY = 62
  )
  return(leafIcons)
}

# leaflet(data = quakes1) %>% addTiles() %>%
#   addMarkers(~long, ~lat, icon = leafIcons)

f.lflt.per <- function(chm.doss,tab,per){
  # create leaflet obj for the chef de tribu and sect Me
  # add the image
  # chm.doss <- "C:/Users/supernova/Dropbox/My PC (supernova-pc)/Documents/golasecca" ; per <- "GIC_IIAB"
  tab.per <- tab[tab[,per] > 0,]
  sit.per <- tab.per[, c("x", "y", "num","sites", "objets", "contexte.site")]
  sit.per <- sit.per[complete.cases(sit.per), ]
  sit.per <- sit.per[!duplicated(sit.per), ]
  sit.per <- na.omit(sit.per)
  
  leafIcons <- f.leafIcons(sit.per) # cr ions Lf
  # leaflet(width = "50%") %>% 
  #   setView(lng = sit.per$x, lat = sit.per$y) %>%
  # aPeriod <- leaflet(data = sit.per,width = "75%") %>%
  #   addTiles() %>%
  #   addMarkers(~x, ~y, icon = leafIcons, popup=~sites)
  paste0(sit.per$num,". ",sit.per$sites,"<br>",sit.per$contexte.site)
  aPeriod <- leaflet(data = sit.per,width = "75%") %>%
    addTiles() %>%
    addMarkers(~x, ~y,
               icon = leafIcons,
               popup=paste0(sit.per$num,". ",sit.per$sites,"<br>",sit.per$contexte.site),
               # popup=~sites,
               label=~num,
               labelOptions = labelOptions(noHide = T,
                                           direction = "center",
                                           textOnly = TRUE,
                                           style = list(
                                             # "color" = "red",
                                             # "font-family" = "serif",
                                             "font-style" = "bold",
                                             # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                             "font-size" = "8px"
                                             # "border-color" = "rgba(0,0,0,0.5)"
                                           )))
  htmlwidgets::saveWidget(aPeriod, file=paste0(chm.doss,"/img/site_",per,".html"))
  return(paste0(chm.doss,"/img/site_",per,".html"))
}
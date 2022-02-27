# 1. Maps Elements #############################################################
## 1.1 Color Platte ============================================================
gridpal <- colorFactor(palette = c("blue","red"),
                       domain = c(50,200),
                       na.color = "#808080")

pal <- colorFactor(palette = c("darkblue",
                               "red",
                               "gray47",
                               "red4",
                               "skyblue3",
                               "darkgreen"),
                   domain = c("Full_Description",
                              "Stratigraphic_Formations",
                              "Stratigraphic_Groups",
                              "Lithology",
                              "No_Description",
                              "Partly_Log_Description"),
                   na.color = "#808080")
## 1.2 Read-in shp files =======================================================
Read_Shapefile <- function(shp_path) {
  read_shp <- reactive({
    req(shp_path)
    infiles <- shp_path()$datapath # get the location of files
    dir <- unique(dirname(infiles)) # get the directory
    outfiles <- file.path(dir, shp_path()$name) # create new path name
    name <- strsplit(shp_path()$name[1], "\\.")[[1]][1] # strip name 
    purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files
    x <- try(read_sf(file.path(dir, paste0(name, ".shp"))))# try to read-in shapefile
    if(class(x)=="try-error") NULL else x # return Null or SF object
  })
  return(read_shp)
}    
# 2. Maps Combinations #########################################################
## 2.1 General =================================================================
wellsmap=function(CS_model_system,geogrid,transforms_shp,viewdif){
  leaflet(CS_model_system) %>%
    addMapPane("basesview", zIndex = 1) %>%
    addMapPane("geoview", zIndex = 2) %>%   
    addMapPane("wellsview", zIndex = 3) %>%  
    setView(lng=as.numeric(viewdif[1]),lat=as.numeric(viewdif[2]),zoom=as.numeric(viewdif[3])) %>% 
    addProviderTiles(providers$CartoDB.Positron #,
                     #options = pathOptions(pane = "basesview")
                     ) %>%
    addPolygons(data=geogrid,
                group = "geology",
                color= "black",
                fill=T,
                fillColor = ~gridpal(resoid),
                weight = 1,
                opacity = 0.4,
                dashArray='5,10',
                smoothFactor = 1#,
                #options = pathOptions(pane = "basesview")
                ) %>%
    addLegend("topright",
              data=geogrid,
              #pal = gridpal,
              values = ~resoid,
              opacity = 0.9,
              colors =c("blue","red"),
              labels = c("1:50,000 m","1:200,000 m"),
              title = "Geology maps resolution") %>%
    leafem::addLogo(
      img="https://csexamp.s3.us-east-2.amazonaws.com/GSI_logo.jpg",
      alpha = 1,
      src ="remote",
      position ="topleft",
      offset.x = 45,
      offset.y = 146,
      width = 30,
      height = 30
    ) %>%
    addCircleMarkers(CS_model_system,
                     lat=~Latitude,lng=~Longitude,label=~name,
                     radius =~0.0025*max_depth + 2.75, #~ifelse(LEVEL_DES == "Lithology", 4, 5),
                     color = ~pal(LEVEL_DES),
                     stroke = FALSE, fillOpacity = 0.99,
                     group = "wells" #,
                     #options = pathOptions(pane = "wellsview")
                     ) %>%
    addLegend("topright",
              pal = pal,
              values = ~LEVEL_DES,
              opacity = 0.9,
              labels = "LEVEL_DES ",
              title = "Well log - Interpretation dgree") %>%
    addLayersControl(overlayGroups = c("wells"),options =
                       layersControlOptions(collapsed=T)) %>% 
    addPolylines(data=transforms_shp,
                 color = "gray",
                 weight = 1,
                 opacity = 0.5,
                 smoothFactor = 0#,
                 #options = pathOptions(pane = "geoview")
                 ) %>% 
    addPmToolbar(
      toolbarOptions = pmToolbarOptions(drawMarker = T, position = "topleft"),
      drawOptions = pmDrawOptions(snappable = T,
                                  allowSelfIntersection = T,
                                  snapDistance = 5,
                                  tooltips = T, cursorMarker = T,
                                  finishOn =  'dblclick',
                                  hintlineStyle = list(color = "#3388ff", dashArray = "5,5"),
                                  templineStyle = list(color = "#ff0000")
      ),
      editOptions = pmEditOptions(preventMarkerRemoval = TRUE, draggable = FALSE),
      cutOptions = pmCutOptions(snappable = FALSE, allowSelfIntersection = FALSE)
    ) %>% 
    addMeasure(
      position = "topleft",
      primaryLengthUnit = "kilometers",
      secondaryLengthUnit = NULL,
      primaryAreaUnit = "sqmeters",
      secondaryAreaUnit = NULL,
      activeColor = "#ABE67E",
      completedColor = "#C8F2BE",
      popupOptions = list(className = "leaflet-measure-resultpopup", autoPanPadding = c(10,
                                                                                        10)),
      captureZIndex = 10000,
      localization = "en",
      decPoint = ".",
      thousandsSep = ","
    ) %>% 
    addLayersControl(overlayGroups = c('draw'), options =
                       layersControlOptions(collapsed=F))
}
## 2.2 National ================================================================
wellsmap_n=function(CS_model_system,geogrid,transforms_shp,viewdif,localtiles_dfs){
  leaflet(CS_model_system) %>%
    addMapPane("basesview", zIndex = 1) %>%
    addMapPane("geoview", zIndex = 2) %>%   
    addMapPane("wellsview", zIndex = 3) %>%  
    setView(lng=as.numeric(viewdif[1]),lat=as.numeric(viewdif[2]),zoom=as.numeric(viewdif[3])) %>% 
    addTiles(as.character(localtiles_dfs$pth),
             attribution = as.character(localtiles_dfs$att)#,
             #options = pathOptions(pane = "basesview")
             ) %>% 
    htmlwidgets::onRender("function(el, x) {
                 L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
    addPolygons(data=geogrid,
                group = "geology",
                color= "black",
                fill=T,
                fillColor = ~gridpal(resoid),
                weight = 1,
                opacity = 0.7,
                dashArray='5,10',
                smoothFactor = 1#,
                #options = pathOptions(pane = "basesview")
                ) %>%
    addLegend("topright",
              data=geogrid,
              #pal = gridpal,
              values = ~resoid,
              opacity = 0.9,
              colors =c("blue","red"),
              labels = c("1:50,000 m","1:200,000 m"),
              title = "Geology maps resolution") %>%
    leafem::addLogo(
      img="https://csexamp.s3.us-east-2.amazonaws.com/GSI_logo.jpg",
      alpha = 1,
      src ="remote",
      position ="topleft",
      offset.x = 45,
      offset.y = 146,
      width = 30,
      height = 30
    ) %>%
    addCircleMarkers(CS_model_system,
                     lat=~Latitude,
                     lng=~Longitude,
                     label=~name,
                     labelOptions=labelOptions(textsize = "30px",
                                               maxWidth = 3000,
                                               maxHeight = 3000,
                                               zIndexOffset=Inf),
                     radius =~0.0025*max_depth + 2.75, #~ifelse(LEVEL_DES == "Lithology", 4, 5),
                     color = ~pal(LEVEL_DES),
                     stroke = FALSE, fillOpacity = 0.99,
                     group = "wells"#,
                    # options = pathOptions(pane = "wellsview")
                    ) %>%
    addLegend("topright",
              pal = pal,
              values = ~LEVEL_DES,
              opacity = 0.9,
              labels = "LEVEL_DES ",
              title = "Well log - Interpretation dgree") %>%
    addLayersControl(overlayGroups = c("wells"),options =
                       layersControlOptions(collapsed=T)) %>% 
    addPolylines(data=transforms_shp,
                 color = "gray",
                 weight = 1,
                 opacity = 0.5,
                 smoothFactor = 0#,
                 #options = pathOptions(pane = "geoview")#
                 ) %>% 
    addPmToolbar(
      toolbarOptions = pmToolbarOptions(drawMarker = T, position = "topleft"),
      drawOptions = pmDrawOptions(snappable = T,
                                  allowSelfIntersection = T,
                                  snapDistance = 5,
                                  tooltips = T, cursorMarker = T,
                                  finishOn =  'dblclick',
                                  hintlineStyle = list(color = "#3388ff", dashArray = "5,5"),
                                  templineStyle = list(color = "#ff0000")
      ),
      editOptions = pmEditOptions(preventMarkerRemoval = TRUE, draggable = FALSE),
      cutOptions = pmCutOptions(snappable = FALSE, allowSelfIntersection = FALSE)) %>% 
    addMeasure(
      position = "topleft",
      primaryLengthUnit = "kilometers",
      secondaryLengthUnit = NULL,
      primaryAreaUnit = "sqmeters",
      secondaryAreaUnit = NULL,
      activeColor = "#ABE67E",
      completedColor = "#C8F2BE",
      popupOptions = list(className = "leaflet-measure-resultpopup", autoPanPadding = c(10,
                                                                                        10)),
      captureZIndex = 10000,
      localization = "en",
      decPoint = ".",
      thousandsSep = ","
    ) %>% 
    addLayersControl(overlayGroups = c('draw'), options =
                       layersControlOptions(collapsed=F))
}
# 3. Additional Layers #########################################################

add_element=function(main_map,ad_lyr,type){
  if (type=="Digital Terrain Model (contours)") {
    uni=st_union(ad_lyr)
    cord=st_coordinates(st_centroid(uni))
    add_map=main_map %>% 
      clearGroup(group="usrelement") %>% 
      setView(lng=cord[1],lat=cord[2],zoom = 13) %>% 
      addPolylines(data=ad_lyr,
                   color= "black",
                   smoothFactor = 1,
                   group="usrelement")
  }
  return(add_map)
 } 

# 4. 2D Geo Model ##############################################################
## 4.1 Simple 2D Geo map =======================================================  
updt_geo2d_map = function(geo2d_map,horizons_db_i,geomdl,obs_points,horizon_unit){
  
  edited_palette=c("#aeafb0","#a2cffc","#95b9de","#467bb3","#1adb9b","#1EDC66","#1EDC21","#0FA411",
                   "#ACE409","#DDE409","#F5C907","#F59B07","#F56507","#DC5C47","#D82D12",
                   "#D81254","#174182","#23539e","#722261","#260C21","#260C21","#260C21","#260C21",
                   "#260C21","#260C21","#260C21","#260C21")
  
  cs_pal <- colorNumeric(edited_palette, horizons_db_i$Elevation,na.color = "transparent")
  
  Sys.sleep(10)
  # Set base proxy map
  proxy_geo2d_map = geo2d_map %>% 
    clearGroup(group="geomodel") %>% 
    addPolylines(data=geomdl$cont,
                 fill = FALSE,
                 color="black",
                 weight = 1,
                 opacity = 0.9,
                 smoothFactor = 0,
                 group="geomodel")  %>%
    addCircleMarkers(
      data=horizons_db_i,
      label =~paste0("Id=",as.character(ID)," ;x=",as.character(round(Distance,0))," ;z=",as.character(round(Elevation,0))),
      labelOptions=labelOptions(style = list(
        "color" = "blue",
        "font-family" = "serif",
        "font-style" = "italic",
        "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
        "font-size" = "15px",
        "border-color" = "rgba(0,0,0,0.5)"
      ),
                                maxWidth = 3000,
                                maxHeight = 3000,
                                zIndexOffset=Inf),
      fillOpacity = 0.7,
      color = ~cs_pal(Elevation),
      stroke = FALSE,
      radius =3,
      group="cs_pnts"
    )  %>%
    addRasterImage(geomdl$rst,
                   color = cs_pal,
                   opacity = 0.2,
                   group="geomodel") %>%
    leaflet::addLegend(pal = cs_pal, values = horizons_db_i$Elevation,position ="topright",
                       title = paste0(horizon_unit," [m amsl]"),
                       group="geomodel")

  if(!is.null(obs_points)==T & nrow(obs_points)>0){
    #obs_points_st=st_as_sf(as_tibble(obs_pnt_df_grp), coords = c("lon", "lat"), crs = 4326,remove=F)
    proxy_geo2d_map = proxy_geo2d_map %>%
      addCircleMarkers(
        label =~paste0("name=",as.character(name)," ;z=",as.character(round(elv,0))),
        data=obs_points,
        #lat=~lat,lng=~lon,
        fillOpacity = 1,
        color = ~cs_pal(elv),
        stroke = FALSE,
        radius =6,
        group="geomodel"
      )
  }
  return(proxy_geo2d_map)
}

## 4.2 Extra layers 4 Geo map ==================================================

extra4geo2d = function(geo2d_map, horizons_db_i,geomdl,obs_points,horizon_unit){
  above_rst=geomdl$above_rst
  if (minValue(above_rst)<0){
    pa_rng = seq(minValue(above_rst),0, length.out=5)
    pb_rng = seq(1,maxValue(above_rst), length.out=5)
    ab_rng = c(pa_rng,pb_rng)
    ab_pal = pb_pal = colorNumeric("RdBu",ab_rng, na.color = "transparent")
  } else{
    ab_rng = seq(minValue(above_rst),maxValue(above_rst), length.out=10)
    ab_pal = colorNumeric("Blues", ab_rng, na.color = "transparent")
  }
  proxy_geo2d_map=updt_geo2d_map(geo2d_map,
                                 horizons_db_i,
                                 geomdl,
                                 obs_points,
                                 horizon_unit)  %>%
    clearGroup(group="geo_upper") %>% 
    addRasterImage(above_rst,
                   color = ab_pal,
                   opacity = 0.8,
                   group="geo_upper") %>%
    addPolylines(data=geomdl$zero_cont,
                 fill = FALSE,
                 label = ~level,
                 labelOptions=labelOptions(style = list("color" = "red")),
                 color="red",
                 weight = 2,
                 opacity = 0.9,
                 smoothFactor = 0,
                 group="geo_upper") %>%
    leaflet::addLegend(pal = ab_pal,
                       values = ab_rng,
                       title = "Upper Layer [m amsl]",
                       position = "topright",
                       group="geo_upper") %>% 
    addLayersControl(position = "topright", overlayGroups = c("geo_upper"))
return(proxy_geo2d_map)  
}







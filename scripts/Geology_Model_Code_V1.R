# CSMS Moac Parameters ##############################
Background_path="G:/Geohydrology"
basemap_pth=paste0("data/Background_layers/BaseMaps/")
library(ipdw)
library(htmltools)
library(kriging)
library(phylin)
library(raster)
library(leaflet)
# Unit Test ####################
tictoc::tic()
# # Unit = Top Senon
horizons_db_i=read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/horizons_db_i.csv")
surface_unit_st=st_read("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/surface_unit_st.shp")
obs_points_u=read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/obs_points_u.csv")
unit_bounds_st=st_read(paste0(Background_path,"/Apps/External_Data/Geology_Model_Moac_Elements/5-Senon_Update_polys.shp")) %>% st_transform(.,crs = 4326)
geology_blocks_st=sf::st_read(paste0(Background_path,"/Apps/External_Data/Geology_Model_Moac_Elements/Active_F_EastMt.shp")) %>% st_transform(.,crs = 4326)
source("G:/Temporary_Scripts/idw_try.R")

line2horizon=function(horizons_db_i,surface_unit_i,obs_points_u,unit_bounds_st,geology_blocks_st){
  
}

# Export
saveWidget(horizon_prod_lst$horizon_map, file=paste0("Q:/Projects/Open/Models/Esat_Mt/data/ANA/Horizons/JUDEA_LOWER/",Horizon_v,'_3.html'),selfcontained =T )
writeRaster(horizon_fix_lst$horizon_fix, paste0("Q:/Projects/Open/Models/Esat_Mt/data/ANA/Horizons/JUDEA_LOWER/",Horizon_v,"_3.tif"),overwrite=T)
# Check raster
chack_C=plot(horizon_fix_lst$chack, 
             breaks = c(round(horizon_fix_lst$chack@data@min,0),5,50, 100, round(horizon_fix_lst$chack@data@max,0)), 
             col = rainbow(6))

line2horizon=function(horizons_db_i,surface_unit_i,obs_points_u,unit_bounds_st,geology_blocks_st){
  
}



# 1. Build Horizon ##############################################################################
line2horizon=function(work_zone,unit_bound,Horizon_v,group_v,useoutcrop,outcrop_r){
## 1.1 Get Raw data =============================================================================
# Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
work_zone=st_transform(work_zone,crs = 4326)  
Active_F=sf::st_read("Q:/Projects/Open/Models/Esat_Mt/data/ANA/GDB/Active_F_EastMt.shp")%>%
  st_transform(.,crs = "+proj=longlat +datum=WGS84") %>%
  subset(.,,c("geometry")) %>% st_zm(.)


subzones= work_zone  %>%  st_intersection(.,unit_bound) %>% subset(.,,c("ID","geometry")) %>% 
  st_zm(.)
colnames(subzones)=c("Id_zone","geometry")

dtm=raster("G:/Layers/Geohydrology/Geohydrology/Apps/CS_Model_V01/data/DEMs/DTM.tif")

# Wells and Springs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ob_wells=read_excel("Q:/Projects/Open/Models/Esat_Mt/data/ANA/GDB/ob_wells_EastMt_V1.xlsx",sheet="ob_wells") %>%  
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326,remove=F) 
obs_pnt_zn=st_join(ob_wells, subzones, join = st_intersects,  left = TRUE, largest = T) %>%
  dplyr::filter(.,!is.na(Id_zone))

obs_pnt_df=cbind(as.data.table(st_coordinates(obs_pnt_zn)),obs_pnt_zn$name,obs_pnt_zn$elv,obs_pnt_zn$Id_zone,obs_pnt_zn$groups)
colnames(obs_pnt_df)=c("lon","lat","name","elv","Id_zone","groups")  

# Meta data file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
folderPath="Q:/Projects/Open/Models/Esat_Mt/data/ANA/CSs/CS_csv"
folder_df=list.files(path=folderPath, pattern = ".csv") %>% as.data.frame() %>% dplyr::rename(.,"filename"=".") %>%
  mutate(filepath=paste0(folderPath,"/",filename))
folder_df$Id=as.numeric(seq(1,nrow(folder_df)))

cs_pnt_lst=list()
for (i in 1 :nrow(folder_df)){
  cs_pnt_lst[[i]]=read.csv(folder_df$filepath[i])
}
### 1.1.2 Build main CSs sf ---------------------------------------------------------------------
CS_horizon=as.data.frame(Reduce(rbind,cs_pnt_lst)) 
CS_horizon_st=st_as_sf(CS_horizon, coords = c("Longitude", "Latitude"), crs = 4326,remove=F) 

## 1.2 Build Horizons============================================================================
# Filter to Specific Horizons
table(CS_horizon_st$Horizon)
CS_horizon_unt=CS_horizon_st %>% dplyr::filter(Horizon==Horizon_v)
obs_pnt_df_grp= dplyr::filter(obs_pnt_df,groups==group_v)
horizon_bord_st=work_zone
horizon_bord_sp=sf::as_Spatial(horizon_bord_st)

CS_horizon_fltr=CS_horizon_unt
CS_horizon_zone=st_join(st_as_sf(CS_horizon_fltr), subzones, join = st_intersects,  left = TRUE, largest = T)
CS_horizon_zone=dplyr::rename(CS_horizon_zone,"lon"="Longitude","lat"="Latitude","elv"="Elevation" )

# Build Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
raw_grid=raster(extent(horizon_bord_sp), resolution = c(0.003,0.003), crs = proj4string(horizon_bord_sp)) %>%
  raster::extend(., c(1,1))
tictoc::tic()
raw_grid_pl=st_join(st_as_sf(rasterToPolygons(raw_grid)), subzones, join = st_intersects,  left = TRUE, largest = T) %>% filter(.,!is.na(Id_zone))
tictoc::toc()
#st_write(st_as_sf(raw_grid_pl),"Q:/Projects/Open/Models/Esat_Mt/data/ANA/GDB/raw_grid_pl_EastMt_V5.shp")
#raw_grid_pl=st_read("Q:/Projects/Open/Models/Esat_Mt/data/ANA/GDB/raw_grid_pl_EastMt_V2.shp", crs = 4326)
raw_grid_sp=sf::as_Spatial(raw_grid_pl)

horizon_grid=raster::intersect(raw_grid_sp, horizon_bord_sp)

### 1.2.1 Run IDW trow Zones --------------------------------------------------------------------
horizon_lst=list()
act_zones=as.data.table(t(table(horizon_grid@data$Id_zone)))
tictoc::tic()
for (i in 1:NROW(act_zones)){
  print(i)
  Id_i=act_zones$V2[i]
  # split 2 zone ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Grid
  coords_poly_st=st_centroid(st_as_sf(horizon_grid)) %>% subset(.,,c("Id_zone","geometry")) %>% dplyr::filter(.,Id_zone==Id_i)
  coords_poly=st_coordinates(coords_poly_st);colnames(coords_poly)=c("lon","lat")
  
  # Get CS Elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  values_measure_cs=st_drop_geometry(subset(CS_horizon_zone,,c("Id_zone","elv"))) %>% dplyr::filter(.,Id_zone==Id_i) %>% as_tibble(.)
  coords_measure_zone=CS_horizon_zone %>% dplyr::filter(.,Id_zone==Id_i)
  coords_measure_cs=st_coordinates(coords_measure_zone) %>% as.data.table(.) ;colnames(coords_measure_cs)=c("lon","lat")
  
  wlsprng=F
  if(isTRUE(wlsprng)){
    obs_pnt_df_fltr=dplyr::filter(obs_pnt_df_grp,Id_zone==i) %>%  as.data.frame(.)
    if(nrow(obs_pnt_df_fltr)>0){
      # Get Wells and springs elements
      values_measure=as.numeric(rbind(values_measure_cs,obs_pnt_df_fltr$elv)$elv) 
      coords_measure=as.matrix(rbind(coords_measure_cs,subset(obs_pnt_df_fltr,,c("lon","lat"))))  
    }
  }  else{
    values_measure=as.numeric(values_measure_cs$elv) 
    coords_measure=as.matrix(coords_measure_cs) 
  }
  
  if(useoutcrop=="Boundary"){
    # Get Outcrops Elements
    outcrop_bound=st_as_sf(gBoundary(horizon_bord_sp,byid=T))
    st_length(outcrop_bound)
    numOfPoints=as.numeric(round(st_length(outcrop_bound),digits = 0))/100
    bu_points=sp::spsample(sf::as_Spatial(outcrop_bound), n = numOfPoints, type = "regular")
    bu_points_sdf=SpatialPointsDataFrame(bu_points, data.frame(x=1:length(bu_points)))
    
    bu_elv_sp=raster::extract(dtm,bu_points_sdf)
    bu_points_sdf@data$elv=bu_elv_sp
    bu_elv_st=st_as_sf(bu_points_sdf) %>% dplyr::filter(!is.na(elv))
    bu_elv_df=st_coordinates(bu_elv_st) %>% as.data.table(.)
    colnames(bu_elv_df)=c("lon","lat")
    
    values_measure_df=rbind(data.frame(elv=values_measure),st_drop_geometry(subset(bu_elv_st,,c("elv"))))
    values_measure=as.numeric(values_measure_df$elv) 
    coords_measure=as.matrix(rbind(as.data.frame(coords_measure),subset(bu_elv_df,,c("lon","lat"))))
  } else if (useoutcrop=="points") {
    # Test elements - Start
    bu_st=st_read("Q:/Projects/Open/Models/Esat_Mt/data/ANA/GDB/outcrops_EastMt_V2.shp")
    geology_50=sf::st_read("G:/Layers/Geohydrology/Geohydrology/Apps/CS_Model_V01/data/Background_layers/BaseMaps/geology_50_V6.shp")
    geology_200=sf::st_read("G:/Layers/Geohydrology/Geohydrology/Apps/CS_Model_V01/data/Background_layers/BaseMaps/geology_200_V4.shp")
    bu_geo_st_50=st_join(bu_st, geology_50, join = st_intersects, suffix = c(".lon", ".lat"), left = TRUE, largest = T)
    bu_geo_st_200=st_join(bu_st, geology_200, join = st_intersects, suffix = c(".lon", ".lat"), left = TRUE, largest = T)
    outcrops2horizons_units=read.csv("Q:/Projects/Open/Models/Esat_Mt/data/ANA/outcrops2horizons_units.csv")
    ou_horizon_st_50 = left_join(bu_geo_st_50,dplyr::filter(outcrops2horizons_units,Source==50)) %>% subset(.,,c("Horizon","geometry")) %>%
      dplyr::filter(.,Horizon==Horizon_v)
    ou_horizon_st_200 = left_join(bu_geo_st_200,dplyr::filter(outcrops2horizons_units,Source==200)) %>% subset(.,,c("Horizon","geometry")) %>%
      dplyr::filter(.,Horizon==Horizon_v)
    ou_horizon_st=bind_rows(ou_horizon_st_50,ou_horizon_st_200) %>% st_difference(.)
    
    # Test elements - End
    ou_horizon_sdf=SpatialPointsDataFrame(sf::as_Spatial(ou_horizon_st), data.frame(x=1:nrow(ou_horizon_st)))
    ou_elv_sp=raster::extract(dtm,ou_horizon_sdf)
    ou_horizon_sdf@data$elv=ou_elv_sp
    ou_elv_st=st_as_sf(ou_horizon_sdf) %>% dplyr::filter(!is.na(elv))
    ou_elv_df=st_coordinates(ou_elv_st) %>% as.data.table(.)
    colnames(ou_elv_df)=c("lon","lat")

    values_measure_df=rbind(data.frame(elv=values_measure),st_drop_geometry(subset(ou_elv_st,,c("elv"))))
    values_measure=as.numeric(values_measure_df$elv) 
    coords_measure=as.matrix(rbind(as.data.frame(coords_measure),subset(ou_elv_df,,c("lon","lat"))))
  }
  else{
    values_measure=values_measure
    coords_measure=coords_measure
  }
  
  # Run IDW ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  source("G:/Layers/Geohydrology/Temporary_Scripts/idw_try.R")
  horizon_int = phylin::idw(values=values_measure,
                            coords=coords_measure,
                            grid=coords_poly,
                            method = "Shepard",
                            p = 2,
                            R = 0.3,
                            N = 12)
  
  
  horizon_coord=cbind(coords_poly,horizon_int);colnames(horizon_coord)=c("lon","lat","elv")
  horizon_sf = st_as_sf(horizon_coord, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84",remove=F)
  horizon_df=as.data.frame(horizon_sf)
  horizon_lst[[i]]=horizon_df
}
tictoc::toc()
horizon_df=Reduce(rbind,horizon_lst)
horizon_rs=raster::rasterize(horizon_df[,1:2], raw_grid, horizon_df[,3], fun=mean)

# Smooze
horizon_rs_sm <- focal(horizon_rs, w= matrix(1,3,3), mean)
horizon_cont=rasterToContour(horizon_rs_sm)
## 1.3 QA =========================================================================================
edited_palette=c("#aeafb0","#a2cffc","#95b9de","#467bb3","#1adb9b","#1EDC66","#1EDC21","#0FA411",
                 "#ACE409","#DDE409","#F5C907","#F59B07","#F56507","#DC5C47","#D82D12",
                 "#D81254","#174182","#23539e","#722261","#260C21","#260C21","#260C21","#260C21",
                 "#260C21","#260C21","#260C21","#260C21")

cs_pal <- colorNumeric(edited_palette, CS_horizon_fltr$elv,na.color = "transparent")


horizon_map=leaflet() %>%
  setView(lng=35.35,lat=32.36,zoom=9.9) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%# 7.9
  #addTiles() %>%
  addPolylines(data=horizon_cont,
               fill = FALSE,
               color="white",
               weight = 1,
               opacity = 0.9,
               smoothFactor = 0) %>%
  addCircleMarkers(
    label =~paste0("Id=",as.character(cs)," ;x=",as.character(Distance)," ;z=",as.character(round(Elevation,0))),
    data=CS_horizon_fltr,
    #lat=~Latitude,lng=~Longitude,
    fillOpacity = 0.6,
    color = ~cs_pal(Elevation),
    stroke = FALSE,
    radius =1.5
  ) %>%
  addRasterImage(horizon_rs_sm,
                 color = cs_pal,
                 opacity = 0.3) %>%
  addPolylines(data=Active_F,
               fill = FALSE,
               color="black",
               weight = 1,
               opacity = 0.5,
               smoothFactor = 0) %>%
  leaflet::addLegend(pal = cs_pal, values =CS_horizon_fltr$Elevation,position ="topleft",
                     title = paste0(Horizon_v," [m amsl]"))

if(isTRUE(wlsprng)){
  obs_pnt_st_grp=st_as_sf(as_tibble(obs_pnt_df_grp), coords = c("lon", "lat"), crs = 4326,remove=F)
  horizon_map = horizon_map %>% 
    addCircleMarkers(
      label =~paste0("name=",as.character(name)," ;z=",as.character(round(elv,0))),
      data=obs_pnt_st_grp, 
      #lat=~lat,lng=~lon,
      fillOpacity = 1,
      color = ~cs_pal(elv),
      stroke = FALSE,
      radius =6
    )
}
#horizon_map


## 1.4 Topgrafy cut off ==============================================================================
bo_st=st_union(raw_grid_pl)
dtm_unit=raster::crop(dtm,as_Spatial(bo_st)) %>% raster::mask(.,as_Spatial(bo_st)) %>% 
  raster::resample(.,horizon_rs_sm)

s <- stack(horizon_rs_sm, dtm_unit)
rc <- function(layer,DTM) {ifelse(layer>=DTM,DTM,layer)} # layer-DTM>10
horizon_rs_cf <- overlay(s, fun=rc)


#plot(horizon_rs_cf)

## 1.5 Outcrops match ================================================================================
if(!is.null(outcrop_r)){
  ou_r=raster::resample(outcrop_r,horizon_rs_cf)
  horizon_rs_uc=horizon_rs_cf
  horizon_rs_uc[!is.na(ou_r)] <- NA
  horizon_rs=merge(ou_r,horizon_rs_uc)
} else {
  horizon_rs=horizon_rs_cf
}

horizon_prod_lst=list("horizon_map"=horizon_map,"horizon_rs_sm"=horizon_rs_sm,"horizon_rs_cf"=horizon_rs)
return(horizon_prod_lst)
}

# 2. Fix Horizon ################################################################################
fixbyupper=function(horizon,upperhorizon){
  horizon_rsmp=resample(horizon,upperhorizon)
  upperhorizon_rsmp=resample(upperhorizon,horizon_rsmp)
  hor_stack=stack(horizon_rsmp,upperhorizon_rsmp)
  # Fix
  difix=function(horizon_rsmp,upperhorizon_rsmp){
    ifelse(upperhorizon_rsmp-horizon_rsmp<0,upperhorizon_rsmp,horizon_rsmp)
  }
  horizon_fix_u=overlay(hor_stack,fun=difix,unstack=T)
  # Replace
  horizon_rsmp_uc=horizon_rsmp
  horizon_rsmp_uc[!is.na(horizon_fix_u)] <- NA
  horizon_fix=merge(horizon_fix_u,horizon_rsmp_uc)
  chack=upperhorizon-horizon_fix
  hozfix_lst=list("horizon_fix"=horizon_fix,"chack"=chack)
  return(hozfix_lst)
}

























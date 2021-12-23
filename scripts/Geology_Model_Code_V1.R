# CSMS Moac Parameters ##############################
Background_path="G:/Geohydrology"
basemap_pth=paste0("data/Background_layers/BaseMaps/")
library(ipdw)
library(htmltools)
library(kriging)
library(phylin)
library(raster)
library(leaflet)
library(automap)   # Automatized approach to Kriging
library(ranger)    # Random Forests
library(neuralnet) # Neural Networks
library(kernlab)   # Support Vector Machine 

# Unit Test ####################
tictoc::tic()
# # Unit = Top Senon
horizons_db_i=read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/horizons_db_i.csv")
surface_unit_st=st_read("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/surface_unit_st.shp")
country="Israel"
grid_reso=0.01
obs_points_u=read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/obs_points_u.csv")
unit_bounds_st=st_read(paste0(Background_path,"/Apps/External_Data/Geology_Model_Moac_Elements/5-Senon_Update_polys.shp")) %>% st_transform(.,crs = 4326)
geology_blocks_st=sf::st_read(paste0(Background_path,"/Apps/External_Data/Geology_Model_Moac_Elements/Active_F_EastMt.shp")) %>% st_transform(.,crs = 4326)
source("G:/Temporary_Scripts/idw_try.R")

line2horizon=function(horizons_db_i,surface_unit_i,country,obs_points_u,unit_bounds_st,geology_blocks_st){
  
}

# Export
saveWidget(horizon_prod_lst$horizon_map, file=paste0("Q:/Projects/Open/Models/Esat_Mt/data/ANA/Horizons/JUDEA_LOWER/",Horizon_v,'_3.html'),selfcontained =T )
writeRaster(horizon_fix_lst$horizon_fix, paste0("Q:/Projects/Open/Models/Esat_Mt/data/ANA/Horizons/JUDEA_LOWER/",Horizon_v,"_3.tif"),overwrite=T)
# Check raster
chack_C=plot(horizon_fix_lst$chack, 
             breaks = c(round(horizon_fix_lst$chack@data@min,0),5,50, 100, round(horizon_fix_lst$chack@data@max,0)), 
             col = rainbow(6))

line2horizon=function(horizons_db_i,surface_unit_i,country,grid_reso,obs_points_u,unit_bounds_st,geology_blocks_st){
## 1.1. Set Zones ==============================================================
  ### 1.1.1 General Boundary ---------------------------------------------------
  message("1.1.1 General Boundary")
  horizons_db_pnt= st_as_sf(horizons_db_i, coords = c("Longitude", "Latitude"), crs =4326,remove=F) %>% 
    dplyr::filter(!is.na(Elevation))
  if(!is.null(unit_bounds_st)==T){
    message("Set general boundary by extranl layer")
    work_zone=st_transform(unit_bounds_st,crs = 4326) %>% st_make_valid(.) %>%  st_union(.) %>% 
      st_cast(.,to="POLYGON")
  } else {.
    message("Set eneral Boundary by CS DB Extent")
    work_zone =  st_buffer(subset(horizons_db_pnt,,c("geometry")),dist=0.05) %>% st_union(.) %>% 
      nngeo::st_remove_holes(.) %>% 
      st_simplify(dTolerance = 0.01)
  }
  ## 1.1.2 Sub Boundary --------------------------------------------------------
  if(!is.null(geology_blocks_st)==T){
    message("1.1.2 Sub Boundary")
    work_zone = st_intersection(work_zone,subset(geology_blocks_st,,c(geometry))) 
  }

## 1.2 Set Elevations ==========================================================     
  message("1.2 Set Elevations")
  ### 1.2.1 Surface Elevation --------------------------------------------------
  dem_pth = "data/DEMs"
  if(country=="Israel"){
    DTM_rst=raster::crop(raster(paste0(dem_pth,"/DTM.tif")),work_zone)
  } else if (exists("DTM_rst",where=additional_layers_lst)==T) {
    DTM_rst=raster::crop(additional_layers_lst$DTM_rst,work_zone)
    names(DTM_rst)="DTM"
  } else if (country=="Indefinite" & exists("DTM_rst",where=additional_layers_lst)==F){
    DTM_rst=raster::crop(raster(paste0(dem_pth,"/DTM_30m.tif")),work_zone)
    names(DTM_rst)="DTM"
  }
  
  ### 1.2.2 Subsurface Elevations ----------------------------------------------
  
  ## 1.3 Build Grid ============================================================
  raw_grid=raster(extent(as_Spatial(work_zone)), resolution = c(grid_reso,grid_reso),
                  crs = proj4string(as_Spatial(work_zone))) %>%
    raster::extend(., c(1,1))
  grid_sf=st_as_sf(rasterToPolygons(raw_grid))
  raw_grid_pl=st_intersection(grid_sf,work_zone)
  grid_df=st_centroid(grid_sf) %>% st_coordinates(.) %>% 
    as_tibble()
  colnames(grid_df)=c("lon","lat")
  %>% colnames(.,)
  
  
  
   # Calculating the distance between every sample location and all other
  # sampling locations. 
  # This is data that machine learning algorithms can work with.
  sample_dm <- st_distance(horizons_db_pnt, horizons_db_pnt) %>% 
    as_tibble() %>% 
    mutate(across(everything(), as.double))
  
  # Next we calculate the distances between every sampling location and all
  # points of the prediction grid (where we want to interpolate later on).
  # This can take a couple of seconds.
  grid_dm <- st_distance(raw_grid_pl, horizons_db_pnt) %>% 
    as_tibble() %>% 
    mutate(across(everything(), as.double))
  
  
  # Let's set up two custom functions, so that we can transparently see what
  # is happening.
  # Scale a value (or, as we shall see, vector) to a range from 0 to 1
  normalize <- function(x, bottom, top){
    (x - bottom) / (top - bottom)
  }
  
  # backtransform the normalized value into an interpretable/meaningful number
  denormalize <- function(x, bottom, top){
    (top - bottom) * x + bottom
  }
  
  # Rather than just normalizing each vector of distances individually,
  # we make an informed decision about the distances that can possibly occur
  # within both the training data and the grid dataset. 
  # This makes sure distances are weighted the same everywhere.
  bottom_distance <- 0
  top_distance <- max(grid_dm)
  
  # normalizing both training and grid distances
  sample_dnorm <- map_dfc(
    sample_dm, 
    normalize, 
    bottom = bottom_distance, 
    top = top_distance
  )
  
  grid_dnorm <- map_dfc(
    grid_dm, 
    normalize, 
    bottom = bottom_distance, 
    top = top_distance
  )
  
  # The actual z-values need to be normalized as well, but here
  # we can derive max and min values directly from the data vector itself
  z=horizons_db_pnt$Elevation
  sample_znorm <- normalize(
    z, top = max(z), bottom = min(z)
  )
  
  # Before we start the actual work let's write a quick autoplot function so that
  # we can optically inspect our results.
  
  autoplot_spi <- function(myresult){
    ggplot(
      data = myresult,
      mapping = aes(x = x, y = y, fill = z)
    ) +
      facet_wrap(~model) +
      geom_tile() +
      scale_fill_viridis_c(option = "C") +
      theme_bw()
  }
 
  xyz=horizons_db_pnt %>%  st_coordinates(.) %>% 
    as_tibble(.) %>% 
    mutate(Z=horizons_db_pnt$Elevation) %>% 
    dplyr::distinct(.,X,Y,.keep_all = T)
  
  z=as.numeric(xyz$Z)
  x=as.numeric(xyz$X)
  y=as.numeric(xyz$Y)
  
  tictoc::tic()
  kriged=kriging::kriging(x, y, z, polygons=NULL, pixels=300,lags=3)
  tictoc::toc()
  krig_df=kriged[["map"]] %>%
    transmute(z=pred,
              lon=x,
              lat=y,
              id_krig = row_number()) %>% as.data.table(.,key="id_krig")
  

  # fit Interpolation Values to Standart Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  closest=RANN::nn2(subset(krig_df,,c("lon","lat")),
                    subset(grid_df,,c("lon","lat")),
                    k = 1, searchtype = "radius", radius = 0.01)[["nn.idx"]]
  
  Grid_idx=cbind(grid_df,closest)%>%dplyr::rename(.,"id_krig"="closest") %>% as.data.table(.,key="id_krig")
  intP_df=setDT(left_join(Grid_idx,subset(krig_df,,c("id_krig","z")),by="id_krig"))
  
  aa=subset(intP_df,,c("lon","lat","z")) 
  colnames(aa)=c("x","y","z")
bb=rasterFromXYZ(aa,res=grid_reso,crs=4326) 

r2 <- crop(bb, extent(as_Spatial(work_zone)))
r3 <- mask(r2, as_Spatial(work_zone))


plot(r3)
    
    subset(horizons_db_pnt,,c(geometry,Elevation)) %>%
    dplyr::rename(z=Elevation) %>% 
    

  sample_sf=
  
  
  
  raw_grid_pl = raw_grid_pl %>%  st_transform(.,crs=6991)
  res_krige <- autoKrige(               # kriging on autopilot
    formula = z~1,                      # ordinary kriging shall do
    input_data = as_Spatial(sample_sf), # converting to spatial points from {sp}
    new_data = as_Spatial(raw_grid_pl)
  ) %>% 
    .$krige_output %>% 
    as_tibble() %>% 
    select(coords.x1, coords.x2, var1.pred) %>% 
    setNames(c("x", "y", "z")) %>% 
    mutate(model = "Kriging")
  
  
  
horizons_db_pnt$Elevation  
  
  
  
  plot(raw_grid_pl) 
}

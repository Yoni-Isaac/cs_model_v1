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
uninclod="GG'"
surface_unit_st=st_read("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/surface_unit_st.shp")
country="Israel"
grid_reso=0.01
obs_points_u=read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/obs_points_u.csv")
unit_bounds_st=st_read(paste0(Background_path,"/Apps/External_Data/Geology_Model_Moac_Elements/5-Senon_Update_polys.shp")) %>% st_transform(.,crs = 4326)
geology_blocks_st=sf::st_read(paste0(Background_path,"/Apps/External_Data/Geology_Model_Moac_Elements/Active_F_EastMt.shp")) %>% st_transform(.,crs = 4326)
algorithm_s="Kriging"
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

line2horizon=function(horizons_db_i,uninclod,surface_unit_i,
                      country,grid_reso,obs_points_u,unit_bounds_st,geology_blocks_st,algorithm_s){
  
  # 1. Get Core DB #############################################################
  message("1. Get Core DB")
  horizons_db_pnt= st_as_sf(horizons_db_i, coords = c("Longitude", "Latitude"), crs =4326,remove=F) %>% 
    dplyr::filter(!is.na(Elevation)) %>% dplyr::filter(ID %notin% uninclod)
  
  # 2. Set Zones ###############################################################
  ## 2.1 General Boundary ======================================================
  message("2.1 General Boundary")
  
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
  ## 2.2 Sub Boundary ==========================================================
  if(!is.null(geology_blocks_st)==T){
    message("1.2.2 Sub Boundary")
    work_zone = st_intersection(work_zone,subset(geology_blocks_st,,c(geometry))) 
  }
  
  # 3. Set Elevations ##########################################################     
  message("1.2 Set Elevations")
  ## 3.1 Surface Elevation =====================================================
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
  
  ## 1.4 Interpolate ===========================================================
  if (algorithm=="Kriging"){
    ### 1.4.1 Kriging ----------------------------------------------------------
    message(paste0("Interpolate By: ", algorithm_s))
    
    # Build and check clean DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xyz=horizons_db_pnt %>%  st_coordinates(.) %>% 
      as_tibble(.) %>% 
      mutate(Z=horizons_db_pnt$Elevation) %>% 
      dplyr::distinct(.,X,Y,.keep_all = T)
    
    sing_ratio=round(100*(nrow(horizons_db_pnt)-nrow(xyz))/nrow(horizons_db_pnt),0)
    if(sing_ratio>10){
      warning(paste0("You lost over thene ",sing_ratio,"% of the points as a result of singularity." ) )
    } else if (sing_ratio>50 | nrow(xyz)<10){
      stop("There are not enough points to perform the interpolation.")
    }
    
    # Run interpolation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tictoc::tic()
    kriged=kriging::kriging(as.numeric(xyz$X), as.numeric(xyz$Y), as.numeric(xyz$Z), polygons=NULL, pixels=300,lags=3)
    krig_df=kriged[["map"]] %>%
      transmute(z=pred,
                lon=x,
                lat=y,
                id_krig = row_number()) %>% as.data.table(.,key="id_krig")
    
    
    # fit Interpolation Values to the Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    closest=RANN::nn2(subset(krig_df,,c("lon","lat")),
                      subset(grid_df,,c("lon","lat")),
                      k = 1, searchtype = "radius", radius = 0.01)[["nn.idx"]]
    
    Grid_idx=cbind(grid_df,closest)%>%dplyr::rename(.,"id_krig"="closest") %>% as.data.table(.,key="id_krig")
    int_df=setDT(left_join(Grid_idx,subset(krig_df,,c("id_krig","z")),by="id_krig")) %>% 
      subset(.,,c("lon","lat","z")) 
    colnames(int_df)=c("x","y","z")
    int_rst=rasterFromXYZ(int_df,res=grid_reso,crs=4326) 
    
    int_crp <- crop(int_rst, extent(as_Spatial(work_zone)))
    int <- mask(int_crp, as_Spatial(work_zone))
    tictoc::toc()
  } else if
  (algorithm_s %notin% c("Kriging", "IDW") )
  {
    ### 1.4.1 Machine learning algorithm - Prepossessing -----------------------
    grid_df <- horizons_db_pnt[, c("Longitude", "Latitude")]
    
    
    
    # Cacl Distance Matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## after: https://swilke-geoscience.net/post/spatial_ml/
    sample_dm <- st_distance(horizons_db_pnt, horizons_db_pnt) %>% 
      as_tibble() %>% 
      mutate(across(everything(), as.double))
    grid_dm <- st_distance(grid_df, horizons_db_pnt) %>% 
      as_tibble() %>% 
      mutate(across(everything(), as.double))
    
    # Build normalize functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    normalize <- function(x, bottom, top){ # Scale a vector  to a range from 0 to 1
      (x - bottom) / (top - bottom)
    }
    
    denormalize <- function(x, bottom, top){ # backtransform the normalized value into an interpretable/meaningful number
      (top - bottom) * x + bottom
    }
    bottom_distance <- 0
    top_distance <- max(grid_dm)
    
    # normalizing both training and grid distances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    # derive max and min values directly from the data vector itself ~~~~~~~~~~~
    z=horizons_db_pnt$Elevation
    sample_znorm <- normalize(
      z, top = max(z), bottom = min(z)
    ) 
    
    if (algorithm_s == "Neural Networks"){
      ### 1.4.3 Neural Networks ------------------------------------------------
      message(paste0("Interpolate By: ", algorithm_s))
      
      # Sample normalized points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      nn_input <- cbind(
        z = sample_znorm,
        sample_dnorm
      )
      
      # Train the model by specifying its size and presenting the input.
      tictoc::tic()
      mod_nn <- neuralnet(
        z~., # column z is dependent the variable, all other columns independent
        data = nn_input,
        hidden = c(
          200, 100, 50, 150
        ) # Four layers of several (arbitrarily chosen) neurons
      )
      tictoc::toc()
      
      # Predict elevations for our dem-grid
      predictions_nn <- predict(mod_nn, grid_dnorm)
      
      # Wrapping up the results
      res_nn <- cbind(
        grid_df,
        z = denormalize(
          predictions_nn, top = max(horizons_db_pnt$Elevation), bottom = min(horizons_db_pnt$Elevation)
        )
      ) %>% 
        mutate(model = "Neural Network")
      
     aa=st_as_sf(res_nn, coords = c("Longitude", "Latitude"), crs =4326,remove=F)
     plot(aa) 
      
      
      }
    
  }
}
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   




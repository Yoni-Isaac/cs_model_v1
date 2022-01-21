# CSMS Moac Parameters ##############################
Background_path="G:/Geohydrology"
basemap_pth=paste0("data/Background_layers/BaseMaps/")
Type_of_runing="t"
`%notin%` <<- Negate(`%in%`)
# source("G:/Temporary_Scripts/idw_try.R")
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

# # Unit = Top Senon
if(Type_of_runing=="u_t"){
  
  horizons_db_i=read.csv("G:/Geohydrology/Models/EastMt/CS_lines/judea_all_units_V1.csv")
  notincluded=NULL#"GG'"
  surface_unit_st=raster("G:/Geohydrology/Models/EastMt/DTM_EstMt_smooth.tif")
  country="Israel"
  grid_reso=0.00001*1000 # Convert resolution to dd
  obs_points_u=read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/obs_points_u_judeaUP.csv")
  obs_inclod=F
  unit_bounds_st=st_read("G:/Geohydrology/Models/EastMt/UGRID_SHAPE/4-judeaUP_polys.shp") %>% st_transform(.,crs = 4326) 
  geology_blocks_st=sf::st_read(paste0(Background_path,"/Apps/External_Data/Geology_Model_Moac_Elements/Active_F_EastMt.shp")) %>% st_transform(.,crs = 4326)
  #
  # algorithm_s="Kriging"
  # ap_lst=list(kriging_mdl="spherical", kriging_pxl=300, kriging_lags=3)
  # algorithm_s="Random Forests",
  # ap_lst=list(rf_normalize=T, trees_n=1000,mtry=100),
  # algorithm_s="Neural Networks",
  # ap_lst=list(layers_rng=c(10,200), layers_n=2)#,
  algorithm_s="Support Vector Machine"
  ap_lst=list(svm_typ="eps-bsvr", kernel= "polydot", svmc_v=25)
  upper_layer=raster("G:/Geohydrology/Apps/CS_Model_V02/data/DEMs/north_eastren_eocene_Base.tif")
  rst_cutter=T
  dtm_not2cut=F
  
  
  tictoc::tic()
  geomodel=line2horizon (
    horizons_db_i=read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/Fixed_CS-A2Z_V2.csv"),
    notincluded=NULL,#"GG'"
    surface_unit_st=st_read("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/surface_unit_st.shp"),
    country="Israel",
    grid_reso=0.00001*1000, # Convert resolution to dd,
    obs_points_u=NULL,#read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/obs_points_u.csv"),
    obs_inclod=F,
    unit_bounds_st=st_read(paste0(Background_path,"/Apps/External_Data/Geology_Model_Moac_Elements/5-Senon_Update_polys.shp")) %>% st_transform(.,crs = 4326) ,
    geology_blocks_st=sf::st_read(paste0(Background_path,"/Apps/External_Data/Geology_Model_Moac_Elements/Active_F_EastMt.shp")) %>% st_transform(.,crs = 4326) ,
    #
    # algorithm_s="Kriging",
    # ap_lst=list(kriging_mdl="spherical", kriging_pxl=300, kriging_lags=3),
    algorithm_s="Random Forests",
    ap_lst=list(rf_normalize=T, trees_n=1000,mtry=100),
    # algorithm_s="Neural Networks",
    # ap_lst=list(layers_rng=c(10,200), layers_n=2)#,
    # algorithm_s="Support Vector Machine",
    # ap_lst=list(svm_typ="eps-bsvr", kernel= "polydot", svmc_v=25),
    upper_layer,
    rst_cutter=F,
    dtm_not2cut=F
  )
  tictoc::toc()
  
  # Check raster
  plot(geomodel)
  write.csv(st_drop_geometry(geomodel_lst$int_pnt),"G:/Geohydrology/RB/eaocen_base_svm.csv")
}

# FUNC ################
line2horizon = function(horizons_db_i,notincluded,surface_unit_st,
                      country,grid_reso,obs_points_u,obs_inclod,unit_bounds_st,
                      geology_blocks_st,algorithm_s,ap_lst,
                      upper_layer,rst_cutter,dtm_not2cut){
  
  # 1. Get Core DB #############################################################
  message("1. Get Core DB")
  horizons_db_pnt= st_as_sf(horizons_db_i, coords = c("Longitude", "Latitude"), crs =4326,remove=F) %>% 
    dplyr::filter(!is.na(Elevation)) %>% dplyr::filter(ID %notin% notincluded)
  
  # 2. Set Zones ###############################################################
  ## 2.1 General Boundary ======================================================
  message("2.1 General Boundary")
  
  if(!is.null(unit_bounds_st)==T){
    message("Set general boundary by extranl layer")
    sf::sf_use_s2(FALSE)
    work_zone=st_transform(unit_bounds_st,crs = 4326) %>% st_make_valid(.) %>%  st_union(.) %>% 
      st_cast(.,to="POLYGON")
  } else {
    message("Set Boundary by CS DB Extent")
    work_zone = st_as_sfc(st_bbox(horizons_db_pnt))
    # work_zone = st_buffer(subset(horizons_db_pnt[1:nrow(horizons_db_pnt),],,c("geometry")),dist=0.05) %>% st_union(.) %>% 
    #   nngeo::st_remove_holes(.) %>% 
    #   st_simplify(dTolerance = 0.01)
  }
  ## 2.2 Sub Boundary ==========================================================
  if(!is.null(geology_blocks_st)==T){
    message("2.2 Sub Boundary")
    #work_zone = st_intersection(work_zone,subset(geology_blocks_st,,c(geometry))) 
  }
  
  # 3. Set Elevations ##########################################################     
  message("3. Set Elevations")
  ## 3.1 Surface Elevation =====================================================
  dem_pth = "data/DEMs"
  work_zone_sp=sf::as_Spatial(st_zm(work_zone, drop = TRUE, what = "ZM"))
  if(country=="Israel"){
    DTM_rst=raster::crop(raster(paste0(dem_pth,"/DTM.tif")),work_zone_sp)
  } else if (exists("DTM_rst",where=additional_layers_lst)==T) {
    DTM_rst=raster::crop(additional_layers_lst$DTM_rst,work_zone_sp)
    names(DTM_rst)="DTM"
  } else if (country=="Indefinite" & exists("DTM_rst",where=additional_layers_lst)==F){
    DTM_rst=raster::crop(raster(paste0(dem_pth,"/DTM_30m.tif")),work_zone_sp)
    names(DTM_rst)="DTM"
  }
  
  # 3.2 Subsurface Elevations ==================================================
  message("3.2 Subsurface Elevations")
  if (!is.null(obs_points_u)==T){
    obs_points4int = obs_points_u %>% 
      mutate(Distance=0,
             Elevation=targ_dpt,
             Horizon=horizons_db_pnt$Horizon[1],
             method="observer",
             ID=as.character(well_id),
             Range=0) 
    obs_points4int_st = st_as_sf(obs_points4int, coords = c("Longitude", "Latitude"), crs =4326,remove=F) %>% 
      subset(.,,names(horizons_db_pnt))  %>% 
      st_intersection(.,work_zone)
    if(obs_inclod==T){
      horizons_db_pnt=bind_rows(horizons_db_pnt,obs_points4int_st)
    }
 
  }
  # 4. Build Grid ##############################################################
  message("4. Build Grid")
  raw_grid=raster(extent(work_zone_sp), resolution = c(grid_reso,grid_reso),
                  crs = proj4string(work_zone_sp)) %>%
    raster::extend(., c(1,1))
  grid_sf=st_as_sf(rasterToPolygons(raw_grid))
  raw_grid_pl=st_intersection(grid_sf,work_zone)
  grid_df=st_centroid(grid_sf) %>% st_coordinates(.) %>% 
    as_tibble()
  colnames(grid_df)=c("lon","lat")
  
  # 5. Interpolate #############################################################
  message("5. Interpolate")
  ## 5.1 Classical algorithm ===================================================
  if (algorithm_s=="Kriging"){
    ### 5.1.1 Kriging ----------------------------------------------------------
    message(paste0("Interpolate By: ", algorithm_s))
    
    # Build and check clean DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    
    # Run Interpolation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tictoc::tic()
    kriged=kriging::kriging(as.numeric(xyz$X), as.numeric(xyz$Y), as.numeric(xyz$Z),
                            polygons=list(data.frame(grid_df$lon, grid_df$lat)),
                            model=ap_lst$kriging_mdl,
                            pixels=ap_lst$kriging_pxl,
                            lags=ap_lst$kriging_lags)
    krig_df=kriged[["map"]] %>%
      transmute(z=pred,
                lon=x,
                lat=y,
                id_krig = row_number()) %>% as.data.table(.,key="id_krig")
    
    tictoc::toc()
    # fit Interpolation Values to the Grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    closest=RANN::nn2(subset(krig_df,,c("lon","lat")),
                      subset(grid_df,,c("lon","lat")),
                      k = 1, searchtype = "radius", radius = 0.01)[["nn.idx"]]
    
    Grid_idx=cbind(grid_df,closest)%>%dplyr::rename(.,"id_krig"="closest") %>% as.data.table(.,key="id_krig")
    int_df=setDT(left_join(Grid_idx,subset(krig_df,,c("id_krig","z")),by="id_krig")) %>% 
      subset(.,,c("lon","lat","z")) 
    
    int <-df2rst(int_df,grid_reso,work_zone_sp)
    int_krg<-int
    
  } else if
  (algorithm_s %notin% c("Kriging", "IDW") )
  {
    ## 5.2 Machine learning algorithms =========================================
    ## after: https://swilke-geoscience.net/post/spatial_ml/
    
    sample_sf  <- st_as_sf(
      horizons_db_pnt, coords = c("Longitude", "Latitude")
    )
    grid_sf <- st_as_sf(
      grid_df, coords = c("lon", "lat")
    ) %>% st_set_crs(4326)
    
    # Cacl Distance Matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sample_dm <- st_distance(sample_sf, sample_sf) %>% 
      as_tibble() %>% 
      mutate(across(everything(), as.double))
    grid_dm <- st_distance(grid_sf, sample_sf) %>% 
      as_tibble() %>% 
      mutate(across(everything(), as.double))
    
    # normalizing both training and grid distances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    bottom_distance <- 0
    top_distance <- max(grid_dm)
    
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
    
    # Sample normalized points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    n_input <- cbind(
      z = sample_znorm,
      sample_dnorm
    )
    
    if (algorithm_s == "Neural Networks"){
      ### 5.2.1 Neural Networks ------------------------------------------------
      message(paste0("Interpolate By: ", algorithm_s))
      
      # Prep UI inputs
      st=ap_lst$layers_rng[1]
      ed=ap_lst$layers_rng[2]
      hidden_c=seq(st,ed,(ed-st)/ap_lst$layers_n)
      
      
      # Train the model by specifying its size and presenting the input.
      tictoc::tic()
      mod_nn <- neuralnet(
        z~., # column z is dependent the variable, all other columns independent
        data = n_input,
        hidden = hidden_c # Four layers of several (arbitrarily chosen) neurons
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
      )
      
      int <-df2rst(int_df=res_nn,grid_reso,work_zone_sp)
      int_nn=int
    }
    
    if (algorithm_s == "Support Vector Machine"){
      ### 5.2.2 Support Vector Machine -----------------------------------------
      message(paste0("Interpolate By: ", algorithm_s))
      mod_svm <- ksvm(
        z~., # column z is dependent the variable, all other columns independent
        data = n_input,
        type = ap_lst$svm_typ,
        kernel =ap_lst$kernel,
        C = ap_lst$svmc_v # A parameter to penalize overfitting
      )
      predictions_svm <- predict(mod_svm, grid_dnorm)
      
      res_svm <- cbind(
        grid_df,
        z = denormalize(
          predictions_svm, top = max(horizons_db_pnt$Elevation), bottom = min(horizons_db_pnt$Elevation)
        )
      )
      
      int <-df2rst(int_df=res_svm,grid_reso,work_zone_sp)
      int_svm=int
      
    }
    
    if (algorithm_s == "Random Forests"){
      ### 5.2.3 Random Forests -------------------------------------------------
      message(paste0("Interpolate By: ", algorithm_s))
      
      rf_input <- cbind(
        z = horizons_db_pnt$Elevation,
        sample_dm
      )
      if(ap_lst$rf_normalize==T){
        mod_rf_norm <- ranger(z~., data = n_input, num.trees = ap_lst$trees_n, mtry = ap_lst$mtry)
        predictions_rfnorm <- predict(mod_rf_norm, grid_dnorm) %>% .$prediction
        res_rf <- cbind(
          grid_df,
          z = denormalize(
            predictions_rfnorm, top = max(horizons_db_pnt$Elevation), bottom = min(horizons_db_pnt$Elevation)
          )
        )
      } else {
        mod_rf <- ranger(z~., data = rf_input, num.trees = ap_lst$trees_n, mtry = ap_lst$mtry)
        predictions_rf <- predict(mod_rf, grid_dm) %>% .$prediction
        res_rf <- cbind(
          grid_df,
          z = predictions_rf
        )
      }
      int <-df2rst(int_df=res_rf,grid_reso,work_zone_sp)
      int_rf=int
    }
    
  }
  # 6.Post Processing ##########################################################
  ## 6.1 Increase Resolution ===================================================
  ss <- raster(resolution=c(grid_reso*0.1,grid_reso*0.1), crs=proj4string(int), ext=extent(int)) 
  int4export <- resample(int, ss)
  
  ## 6.2 Cut with uppers =======================================================
  ### 6.2.1 Upper Layer --------------------------------------------------------
  if(rst_cutter==T & !is.null(upper_layer)==T){
    upper_layer_rs <- resample(upper_layer, int4export)
    s <- stack(int4export, upper_layer_rs)
    rc <- function(int4export,upper_layer_rs) {ifelse(int4export>=upper_layer_rs,upper_layer_rs,int4export)} 
    overlay_rs <- overlay(s, fun=rc)
    out_rs=raster::mask(int4export,upper_layer_rs,inverse=T)
    int4export=raster::mosaic(overlay_rs,out_rs,fun=mean)
  }
  ### 6.2.2 Topography ---------------------------------------------------------
  if(dtm_not2cut==F){
    DTM_rst_rs <- resample(DTM_rst, int4export)
    s <- stack(int4export, DTM_rst_rs)
    rc <- function(int4export,DTM_rst_rs) {ifelse(int4export>=DTM_rst_rs,DTM_rst_rs,int4export)}
    overlay_rs <- overlay(s, fun=rc)
    out_rs=raster::mask(int4export,DTM_rst_rs,inverse=T)
    int4export=raster::mosaic(overlay_rs,out_rs,fun=mean)
  }

  # 7. Export Elements #########################################################
  message("7. Export Elements")
  message("Geology Model was Successfully Completed")
  return(int4export)
}
# Sub Functions ################################################################
# Build normalize functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
normalize <- function(x, bottom, top){ # Scale a vector  to a range from 0 to 1
  (x - bottom) / (top - bottom)
}

denormalize <- function(x, bottom, top){ # backtransform the normalized value into an interpretable/meaningful number
  (top - bottom) * x + bottom
}
# Convert Interpolated Grid raster ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df2rst=function(int_df,grid_reso,work_zone_sp){
  colnames(int_df)=c("x","y","z")
  int_rst=rasterFromXYZ(int_df,res=grid_reso,crs=4326) 
  
  int_crp <- crop(int_rst, extent(work_zone_sp))
  int <- mask(int_crp, work_zone_sp)
  return(int)
}






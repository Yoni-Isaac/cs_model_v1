# source("G:/Temporary_Scripts/idw_try.R")
library(ipdw)
library(htmltools)
library(kriging)
library(phylin)
library(terra)
library(raster)
library(leaflet)
library(automap)   # Automatized approach to Kriging
library(ranger)    # Random Forests
library(neuralnet) # Neural Networks
library(kernlab)   # Support Vector Machine 
Type_of_runing="t"
`%notin%` <<- Negate(`%in%`)


# Unit Test ####################################################################

# # Unit = Top Senon
if(Type_of_runing=="u_t"){
  # CSMS Moac Parameters - Ran as Script =======================================
  Background_path="G:/Geohydrology"
  basemap_pth=paste0("data/Background_layers/BaseMaps/")
  horizons_db_i=read.csv("G:/Geohydrology/Models/SorekRefaem/data/ANA/GDB/Tebular/Fixed_CS-SS'-2022-06-30.csv")
  notincluded=NULL#"GG'"
  surface_unit_st=st_read("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/surface_unit_KefarShaul_st.shp")
  country="Israel"
  grid_reso=0.01
  obs_points_u=read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/obs_points_u_KefarShaul_st.csv")
  obs_inclod=F
  unit_bounds_st=st_read("G:/Geohydrology/Models/SorekRefaem/data/ANA/GDB/Features/UnitsBounds/KefarShaul_V3.shp") %>% st_transform(.,crs = 4326) 
  geology_blocks_st=sf::st_read("G:/Geohydrology/Models/SorekRefaem/data/ANA/GDB/Features/suboundray_V4_bof.shp") %>% st_transform(.,crs = 4326)
  #
  # algorithm_s="Kriging"
  # ap_lst=list(kriging_mdl="spherical", kriging_pxl=300, kriging_lags=3)
  # algorithm_s="Random Forests",
  # ap_lst=list(rf_normalize=T, trees_n=1000,mtry=100),
  # algorithm_s="Neural Networks",
  # ap_lst=list(layers_rng=c(10,200), layers_n=2)#,
  algorithm_s="Support Vector Machine"
  ap_lst=list(svm_typ="eps-bsvr", kernel= "polydot", svmc_v=25)
  upper_layer=NULL# raster("G:/Geohydrology/Apps/CS_Model_V02/data/DEMs/north_eastren_alluvium_Base.tif")
  rst_cutter=5
  dtm_not2cut=F
  
  # CSMS Moac Parameters - Ran as Function =====================================
  tictoc::tic()
  geomodel=line2horizon (
    horizons_db_i=read.csv("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/Fixed_CS-A2Z_V2.csv"),
    notincluded=NULL,#"GG'"
    surface_unit_st=NULL,# st_read("G:/Geohydrology/Apps/External_Data/Geology_Model_Moac_Elements/surface_unit_st.shp"),
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
    rst_cutter=5,
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
      st_cast(.,to="POLYGON") %>% st_sf(.)
  } else {
    message("Set Boundary by CS DB Extent")
    work_zone = st_as_sfc(st_bbox(horizons_db_pnt))
    # work_zone = st_buffer(subset(horizons_db_pnt[1:nrow(horizons_db_pnt),],,c("geometry")),dist=0.05) %>% st_union(.) %>% 
    #   nngeo::st_remove_holes(.) %>% 
    #   st_simplify(dTolerance = 0.01)
  }
  
  ## 2.2 Sub Boundaries ========================================================
  if(!is.null(geology_blocks_st)==T){
    message("2.2 Sub Boundaries")
    geology_blocks_st=rownames_to_column(geology_blocks_st,var="geoblock_id") 
    work_zone= st_intersection(geology_blocks_st,work_zone)
    plot(work_zone)
  } else {
    work_zone$geoblock_id=1
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
  
  ## 3.2 Subsurface Elevations ==================================================
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
  
  # Edit 02072022 S################
  # 4. Layers preparing ########################################################
  message("4. Layers preparing")
  ## 4.1 Geo blocks (Sub-boundaries) ===========================================
  ### 4.1.1 Split DB to blocks -------------------------------------------------
  if(!is.null(geology_blocks_st)==T){
    message("4.1 Geo blocks (Sub-boundaries)")
    horizons_db_pnt=st_join(horizons_db_pnt,subset(geology_blocks_st,,c("geoblock_id")),st_intersects,left=T)
    checkpnt= st_drop_geometry(horizons_db_pnt) %>% group_by(geoblock_id) %>% dplyr::summarise(n=n())
    
    # Check that there are enough points in each geological block
    if(any(checkpnt$n)<1000){
      messeges_str="At least one of the geological blocks has too few points to perform the interpolation,
      please check the map and add points as needed."
      showModal(modalDialog(
        title = "Data warning: ",
        messeges_str,
        easyClose = TRUE,
        footer = NULL
      )) 
    }
  } else {
    horizons_db_pnt$geoblock_id=1
  }
  ### 4.1.2 Ran Model Throw blocks ---------------------------------------------
  int_lst=list()
  for(i in 1:max(work_zone$geoblock_id,na.rm = T)){
    check_zone_i=dplyr::filter(work_zone,geoblock_id==i)
    check_db_pnt_i=dplyr::filter(horizons_db_pnt,geoblock_id==i)
    if (nrow(check_zone_i)>0 & nrow(horizons_db_pnt_i)>0){
      work_zone_i=check_zone_i
      horizons_db_pnt_i=check_db_pnt_i
      work_zone_sp_i=sf::as_Spatial(st_zm(work_zone_i, drop = TRUE, what = "ZM"))
      
      ## 4.2 Build Grid ============================================================
      message("4.2 Build Grid")
      raw_grid=raster(extent(work_zone_sp_i), resolution = c(grid_reso,grid_reso),
                      crs = proj4string(work_zone_sp_i)) %>%
        raster::extend(., c(1,1))
      grid_sf=st_as_sf(rasterToPolygons(raw_grid))
      raw_grid_pl=st_intersection(grid_sf,work_zone_i)
      grid_df=st_centroid(grid_sf) %>% st_coordinates(.) %>% 
        as_tibble()
      colnames(grid_df)=c("lon","lat")
      
      # 5. Interpolate #############################################################
      message("5. Interpolate")
      if (algorithm_s=="Kriging"){
        ## 5.1 Classical algorithm =================================================
        message("5.1 Classical algorithm")
        ### 5.1.1 Kriging ----------------------------------------------------------
        message(paste0("Interpolate By: ", algorithm_s))
        
        # Build and check clean DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        xyz=horizons_db_pnt_i %>%  st_coordinates(.) %>% 
          as_tibble(.) %>% 
          mutate(Z=horizons_db_pnt_i$Elevation) %>% 
          dplyr::distinct(.,X,Y,.keep_all = T)
        
        sing_ratio=round(100*(nrow(horizons_db_pnt_i)-nrow(xyz))/nrow(horizons_db_pnt_i),0)
        if(sing_ratio>10){
          warning(paste0("You lost over thene ",sing_ratio,"% of the points as a result of singularity." ) )
        } else if (sing_ratio>50 | nrow(xyz)<10){
          stop("There are not enough points to perform the interpolation.")
        }
        
        # Build Boudoirs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        work_bounds=nngeo::st_remove_holes()
        seg = st_segmentize(work_bounds, units::set_units(300,m))
        seg_pnt= st_cast(seg, 'POINT')
        seg_df=as_tibble(as_Spatial(seg_pnt)@coords) %>% 
          dplyr::rename_all((~c("x","y"))) %>% 
          dplyr::distinct(.,.keep_all = T)
        seg_lst=list(data.frame(seg_df$x,seg_df$y))
        # Load & Ran Custom interpolation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
        crs_kriging=function(x, y, response, model = "spherical", lags = 10, 
                             pixels = 100, polygons) 
        {
          # Sub functions --------------------------------------------------------------
          onedim <- function(x, nn) {
            .C("onedimdist", PACKAGE="kriging",
               as.double(x), 
               as.integer(nn),
               v = as.double(rep(0, nn*nn)))$v
          }
          
          twodim <- function(x, y, nn) {
            .C("twodimdist", PACKAGE="kriging", 
               as.double(x), 
               as.double(y), 
               as.integer(nn),
               d = as.double(rep(0, nn*nn)))$d
          }
          krig.vg <- function(D, V, cutoff, lags) {
            W <- matrix(0, lags, 3)
            for(i in 1:lags) {
              W[i,1] <- length(D[(D<(i*cutoff/lags)) & (D>((i-1)*cutoff/lags))])/2
              W[i,2] <- mean(D[(D<(i*cutoff/lags)) & (D>((i-1)*cutoff/lags))]) # Average 
              W[i,3] <- sum(V[(D<(i*cutoff/lags)) & (D>((i-1)*cutoff/lags))])/(4*W[i,1]) # Semivariance
            }
            return(W)
          }
          krig.fit <- function(D, nugget, range, sill, model, nn) {
            if(model=="spherical") modelID <- 0
            if(model=="exponential") modelID <- 1
            if(model=="gaussian") modelID <- 2
            return(.C("krigfit", PACKAGE="kriging",
                      as.double(D),
                      as.double(nugget),
                      as.double(range),
                      as.double(sill),
                      as.integer(modelID),
                      as.integer(nn),
                      a = as.double(rep(1, (nn+1)*(nn+1))))$a)
          }
          
          krig.grid <- function(blx, bly, trx, try, pixels) {
            pixel <- max((trx-blx), (try-bly)) / (pixels-1)
            xpixels <- ceiling((trx-blx)/pixel)
            ypixels <- ceiling((try-bly)/pixel)
            
            G.grid <- .C("kriggrid", PACKAGE="kriging",
                         as.double(blx),
                         as.double(bly),
                         as.double(pixel),
                         as.integer(xpixels),
                         as.integer(ypixels),
                         gx = as.double(rep(0, xpixels * ypixels)),
                         gy = as.double(rep(0, xpixels * ypixels)))
            
            return(data.frame(x = G.grid$gx, y = G.grid$gy, pixel=pixel))
          }
          
          krig.polygons <- function(X, Y, polygons) {
            nn <- length(X)
            nlist <- length(polygons)
            npoly <- rep(0, nlist+1)
            
            polygonsx <- polygonsy <- {}
            for(i in 1:nlist) {
              polygonsx <- c(polygonsx, polygons[[i]][,1])
              polygonsy <- c(polygonsy, polygons[[i]][,2])
              npoly[i+1] <- length(polygonsx)
            }
            
            G <- .C("krigpolygons", 
                    as.integer(nn),
                    as.double(X),
                    as.double(Y),
                    as.integer(nlist),
                    as.integer(npoly),
                    as.double(polygonsx),
                    as.double(polygonsy),
                    Gn = as.integer(0),
                    Gx = as.double(rep(0, nn)),
                    Gy = as.double(rep(0, nn)))
            
            return(data.frame(x = G$Gx[1:G$Gn], y = G$Gy[1:G$Gn]))
          }
          
          krig.pred <- function(x, y, response, Gx, Gy, invA, nugget, range, sill, model, nn) {
            if(model=="spherical") modelID <- 0
            if(model=="exponential") modelID <- 1
            if(model=="gaussian") modelID <- 2
            return(.C("krigpred", PACKAGE="kriging",
                      as.double(x),
                      as.double(y),
                      as.double(response),
                      as.double(Gx),
                      as.double(Gy),
                      as.double(invA),
                      as.double(nugget),
                      as.double(range),
                      as.double(sill),
                      as.integer(modelID),
                      as.integer(nn),
                      as.integer(length(Gx)),
                      as.integer(nn+1),
                      as.integer(1),
                      G.pred = as.double(rep(0, length(Gx))))$G.pred)
          }
          
          imageloop <- function(x, y, z, a, b) {
            nn <- length(z)
            na <- length(a)
            nb <- length(b)
            
            loop <- .C("krigimage",
                       as.integer(x),
                       as.integer(y),
                       as.double(z),
                       as.integer(nn),
                       as.integer(a),
                       as.integer(b),
                       as.integer(na),
                       as.integer(nb),
                       i = as.double(rep(0, na*nb)),
                       j = as.integer(rep(0, na*nb)))
            
            loop$i[loop$j==0] <- NA
            return(matrix(loop$i, na, nb, byrow=T))
          }
          # Main function --------------------------------------------------------------
          nn <- length(response)
          p <- 2
          library(kriging)
          D <- twodim(x, y, nn)
          V <- onedim(response, nn)
          cutoff <- sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2)/3
          W <- krig.vg(as.matrix(D, nn, nn), as.matrix(V, nn, nn), cutoff, 
                       lags)
          fit.vg <- lm(W[, 3] ~ W[, 2])$coefficients
          nugget <- as.numeric(fit.vg[1])
          range <- max(W[, 2])
          sill <- nugget + as.numeric(fit.vg[2]) * range
          a <- 1/3
          A <- krig.fit(D, nugget, range, sill, model, nn)
          # CRS update S#############
          # Fit interpolation to polygon grid
          x_grid=polygons[[1]][[1]]
          y_grid=polygons[[1]][[2]]
          G <- krig.grid(min(x_grid), min(y_grid), max(x_grid), max(y_grid), pixels)
          # CRS update E#############
          
          pixel <- unique(G$pixel)
          if (!is.null(polygons)) {
            G <- krig.polygons(G$x, G$y, polygons)
          }
          G.pred <- krig.pred(x, y, response, G$x, G$y, c(solve(matrix(A, 
                                                                       nn + 1, nn + 1))), nugget, range, sill, model, nn)
          o <- list(model = model, nugget = nugget, range = range, 
                    sill = sill, pixel = pixel, map = data.frame(x = G$x, 
                                                                 y = G$y, pred = G.pred), semivariogram = data.frame(distance = W[, 
                                                                                                                                  2], semivariance = W[, 3]))
          class(o) <- "kriging"
          o
        }
        kriged=crs_kriging(as.numeric(xyz$X), as.numeric(xyz$Y), as.numeric(xyz$Z),
                           polygons=seg_lst,
                           model=ap_lst$kriging_mdl,
                           pixels=ap_lst$kriging_pxl,
                           lags=ap_lst$kriging_lags)
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
        
        int <-df2rst(int_df,grid_reso,work_zone_sp_i)
        int_krg<-int
        
      } else if
      (algorithm_s %notin% c("Kriging", "IDW") )
      {
        ## 5.2 Machine learning algorithms =========================================
        message("5.2 Machine learning algorithms")
        ## after: https://swilke-geoscience.net/post/spatial_ml/
        
        sample_sf  <- st_as_sf(
          horizons_db_pnt_i, coords = c("Longitude", "Latitude")
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
        z=horizons_db_pnt_i$Elevation
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
              predictions_nn, top = max(horizons_db_pnt_i$Elevation), bottom = min(horizons_db_pnt_i$Elevation)
            )
          )
          
          int <-df2rst(int_df=res_nn,grid_reso,work_zone_sp_i)
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
              predictions_svm, top = max(horizons_db_pnt_i$Elevation), bottom = min(horizons_db_pnt_i$Elevation)
            )
          )
          
          int <-df2rst(int_df=res_svm,grid_reso,work_zone_sp_i)
          int_svm=int
          
        }
        
        if (algorithm_s == "Random Forests"){
          ### 5.2.3 Random Forests -------------------------------------------------
          message(paste0("Interpolate By: ", algorithm_s))
          
          rf_input <- cbind(
            z = horizons_db_pnt_i$Elevation,
            sample_dm
          )
          if(ap_lst$rf_normalize==T){
            mod_rf_norm <- ranger(z~., data = n_input, num.trees = ap_lst$trees_n, mtry = ap_lst$mtry)
            predictions_rfnorm <- predict(mod_rf_norm, grid_dnorm) %>% .$prediction
            res_rf <- cbind(
              grid_df,
              z = denormalize(
                predictions_rfnorm, top = max(horizons_db_pnt_i$Elevation), bottom = min(horizons_db_pnt_i$Elevation)
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
          int <-df2rst(int_df=res_rf,grid_reso,work_zone_sp_i)
          int_rf=int
        }
        
      }
      int_lst[[i]]=int  
    } else{
      
      messeges_str=paste0("You have lost block number: ", i ," because it has no data in it.")
      showModal(modalDialog(
        title = "Data warning: ",
        messeges_str,
        easyClose = TRUE,
        footer = NULL
      )) 
      
    }
  } 
  
  # Edit 02072022 E################
  
  
  # 6.Post Processing ##########################################################
  message("6.Post Processing")
  ## 6.1 Combine interpolation elements ========================================
  tra_lst=lapply(int_lst,FUN = rast)
  int = raster(mosaic(terra::src(tra_lst)))

  ## 6.2 Increase Resolution ===================================================
  ss <- raster(resolution=c(grid_reso*0.1,grid_reso*0.1), crs=proj4string(int), ext=extent(int)) 
  int4export <- resample(int, ss)
  
  ## 6.3 Cut with uppers =======================================================
  ### 6.3.1 Outcrops (surface Unit) --------------------------------------------
  if(!is.null(surface_unit_st)==T){
    DTM_outcrops=raster::mask(crop(DTM_rst,surface_unit_st),surface_unit_st)
    DTM_outcrops_rs=resample(DTM_outcrops, ss)
    int4export=raster::mosaic(DTM_outcrops_rs,int4export,fun=max)
    if(!is.null(unit_bounds_st)==T){
      int4export=raster::mask(crop(int4export,unit_bounds_st),unit_bounds_st)
    }
  }
  
  ### 6.3.2 Upper Layer --------------------------------------------------------
  if(!is.na(rst_cutter)==T & !is.null(upper_layer)==T){
    upper_layer_rs <- resample(upper_layer, int4export)
    s <- stack(int4export, upper_layer_rs)
    rc <- function(int4export,upper_layer_rs) {ifelse(int4export>=upper_layer_rs,upper_layer_rs-rst_cutter,int4export)} 
    overlay_rs <- overlay(s, fun=rc)
    out_rs=raster::mask(int4export,upper_layer_rs,inverse=T)
    int4export=raster::mosaic(overlay_rs,out_rs,fun=mean)
  }
  ### 6.3.3 Topography ---------------------------------------------------------
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

# Supportive Functions ######################################################### 
# Update Wells list on the geo model tub ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geo_obs_updt=function(INDEX,Geology_Description_ss,CS_type){
  INDEX_obs = INDEX %>% 
    dplyr::filter(type==CS_type) %>% 
    dplyr::rename("{CS_type}":=f_ID)
  
  Geology_Description_trg<<- Geology_Description_ss %>% 
    subset(.,,c(CS_type)) %>% 
    left_join(.,subset(INDEX_obs,,c(CS_type,"f_name"))) %>% 
    dplyr::filter(!is.na(f_name)) %>% 
    dplyr::distinct(f_name,.keep_all = F)
  return(Geology_Description_trg)
}



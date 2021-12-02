#  Cross Section Model ##################################################################################
message("Cross Section Model")
# 0.Initiation =========================================================================================
message("0.Iinitiation")
# 0.1 Get Running Parameters ---------------------------------------------------------------------------
message("0.1 Get Running Parameters")
cs_crs="+proj=longlat +datum=WGS84"
Type_of_runing="t"

# Unit Test
if(Type_of_runing=="u_t"){
  Background_path="G:/Geohydrology"
  pb <- progress::progress_bar$new(total = 100)
  pb$tick()
  tictoc::tic()
  charts=cs_model(in_param="t")
  tictoc::toc()
  print(charts$cs_preview)
  print(charts$cs_html)
  ggsave2(paste0("RB/Products","/Preview_CS",".pdf"),charts$cs_preview,width=14,height=7)
  print(charts$cs_ppt, target = paste0("RB/Products","/Editable_CS",".pptx"))
  htmlwidgets::saveWidget(charts$cs_html,selfcontained = FALSE, file=paste0("RB/Products","/Interactive_CS",".html"))
}
# Function
cs_model=function(in_param)
{
  # 1. Get Data ==========================================================================================
  message("1.Get Data")
  
  # 1.1 GIS Data -----------------------------------------------------------------------------------------
  message("1.1 GIS Data")
  
  # 1.1.1 GIS Data - test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(class(in_param)=="character"){
    message("1.1.1 GIS Data - test")
    Background_path="G:/Geohydrology"
    #Prodact_path=paste0(Background_path,"/CS_Model_V01/Products/temp")
    drawn_polyline=st_read(paste0(Background_path,'/Apps/External_Data/drawn_polyline.csv'))
    Geology_Descriptions_slc=read.csv(paste0(Background_path,'/Apps/External_Data/Geology_Descriptions_slc.csv')) %>% as.data.table(.,key="well_id")#  Local Test file
    transforms_st=read.csv(paste0(Background_path,'/Apps/External_Data/transforms.csv'))
    season="Summer-Autumn"
    measurement_year=FALSE # 2018 # FALSE
    title_CS="Test CS"
    cs_id="A"
    well_id="shd_id"
    CS_type="groups" # materials ; formations ; groups
    Build_Solids=F
    Projection_Line=T
    Buffer=2
    label_size=30
    Interval_between_labeles=300
    Lower_Limit=-1500
    Surface_Smooth=1.2
    Resolution=100
    vertical_resolution=50
    DEM_line_thickness=0.2
    Save_as_PowerPoint=paste0(Background_path,'/CS_Model_V01/Products/hydrology_cs.pptx')
    Use_tamplate =F
    Background=NULL #as.character(read.csv(paste0(Background_path,'/Apps/External_Data/Background_nat_ES.csv'))$V1) ;str_i=1 #NULL #
    geological_cs_surf="geomap"# geomap # "geomap_free_colors" # "blind"
    country="Israel" # "Indefinite"
    geology_200=sf::st_read(paste0("data/Background_layers/BaseMaps/","geology_200_V4.shp"))
    geology_50=sf::st_read(paste0("data/Background_layers/BaseMaps/","geology_50_V6.shp"))
    additional_layers_lst=readRDS(file="G:/Geohydrology/Apps/External_Data/QA_elements/additional_geology_ES_rast.RData") # list()
  }
  
  # 1.1.2 GIS Data - Serves Application ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(class(in_param)=="list"){
    message("1.1.3 GIS Data - app Application")
    
    # Default Elements
    well_id="shd_id"
    
    # Basic Parameter
    drawn_polyline=in_param$drawn_polyline
    Geology_Descriptions_slc=in_param$Geology_Descriptions_slc
    transforms_st=in_param$transforms_st
    Background=in_param$Background;str_i=1
    Build_Solids=in_param$Build_Solids#if(!is.null(in_param$Build_Solids)){Build_Solids=T}else{Build_Solids=F}
    title_CS=as.character(in_param$title_CS)
    CS_type=in_param$CS_type
    Export=T ; Prodact_path=in_param$Prodact_path
    Save_as_PowerPoint=T
    
    # Advance Parameters
    season=in_param$season
    measurement_year=if(!is.na(in_param$measurement_year)==T){in_param$measurement_year}else{F}
    cs_id=as.character(in_param$cs_id)
    transforms=in_param$transforms
    # Deliverable Properties
    label_size=in_param$label_size
    Interval_between_labeles=in_param$Interval_between_labeles
    Lower_Limit=in_param$Lower_Limit
    Surface_Smooth=in_param$Surface_Smooth
    Resolution=in_param$Resolution
    vertical_resolution=in_param$vertical_resolution
    DEM_line_thickness=in_param$DEM_line_thickness
    country=in_param$country
    Use_tamplate =in_param$Use_tamplate
    
    Projection_Line=in_param$Projection_Line
    Buffer=in_param$Buffer
    geological_cs_surf=in_param$geological_cs_surf
    geology_200=in_param$geology_200
    geology_50=in_param$geology_50
    additional_layers_lst=in_param$additional_layers_lst
  }
  # 1.2 Background layers --------------------------------------------------------------------------------
  message("1.2 Background layers")
  dem_pth = "data/DEMs"
  drawn_polygon=st_buffer(drawn_polyline,dist=1000,crs=4326)
  actiev_library_n=str_split(Background,"//s") %>% as.data.table() %>% t(.) %>% as.data.table()
  
  if(country=="Israel" & exists("DTM_rst",where=additional_layers_lst)==F){
    DTM_rst=raster::crop(raster(paste0(dem_pth,"/DTM.tif")),drawn_polygon)
  } else if (exists("DTM_rst",where=additional_layers_lst)==T) {
    DTM_rst=raster::crop(additional_layers_lst$DTM_rst,drawn_polygon)
    names(DTM_rst)="DTM"
  } else if (country=="Indefinite" & exists("DTM_rst",where=additional_layers_lst)==F){
    DTM_rst=raster::crop(raster(paste0(dem_pth,"/DTM_30m.tif")),drawn_polygon)
    names(DTM_rst)="DTM"
  }
  # 1.2.1 DEMs - Static ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("1.2.1 DEMs - Static")
  # Get Static DEMs
  
  if(NROW(actiev_library_n)>0){
    actiev_library=actiev_library_n %>% dplyr::transmute(basin=as.character(str_sub(V1, str_i,-str_i)))
    
    # Set Connection to DEMs Library
    Static_Background = as.data.table(read_excel(paste0(dem_pth,"/DEMs_md_V2.xlsx"),sheet = "Static_Background"))
    DEMs.files=dplyr::inner_join(Static_Background,actiev_library)
    
    # insert DEMs files to list
    DEMs.list=list()
    for(i in 1:NROW(DEMs.files)){
      DEM_pth=as.character(DEMs.files[i,]$pth) 
      DEM_nam=as.character(DEMs.files[i,]$name) 
      DEM=raster(DEM_pth)
      if(  as.character(crs(DEM))!="+proj=longlat +datum=WGS84 +no_defs" & 
           as.character(crs(DEM))!="+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs"   )  {
        message(paste0("Fix CRS: ",DEM_nam))
        DEM=projectRaster(DEM, crs="+proj=longlat +datum=WGS84 +no_defs")
      } 
      names(DEM)=DEM_nam
      assign(names(DEM), DEM)
      DEMs.list[i]=DEM
    }
    static_DEM_i=i
  }
  
  if(exists("DEM_rst",where=additional_layers_lst)==T) {
    # 1.2.2 DEMs - Additional ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    message("1.2.2 DEMs - Additional")  
    if(NROW(actiev_library_n)==0){
      DEMs.list=list()
      i=0
      Static_Background = as.data.table(read_excel(paste0(dem_pth,"/DEMs_md_V2.xlsx"),sheet = "Static_Background"))
      DEMs.files=dplyr::filter(Static_Background,basin=="NULL")
      Background="NULL"
    } else {
      DEMs.files=bind_rows(DEMs.files,dplyr::filter(Static_Background,basin=="NULL")) 
    }
    DEM_rst_i=raster::crop(additional_layers_lst$DEM_rst,drawn_polygon)
    DEMs.list[i+1]=DEM_rst_i
    static_DEM_i=i+1
  }
  
  
  # 1.2.3 DEMs - Dynamic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("1.2.3 DEMs - Dynamic")
  D_DEMs.files=NULL
  if(NROW(actiev_library_n)>0){
    
    # Set Connection to DEMs Library
    d_dem_pth = "data/DEMs/DEMs_dynamic"
    Dynamic_Background = as.data.table(read_excel(paste0(dem_pth,"/DEMs_md_V2.xlsx"),sheet = "Dynamic_Background"))
    
    # Filter Dynamic Data by year and basins 
    D_DEMs.files_bsn=dplyr::inner_join(Dynamic_Background,actiev_library)
    
    if(measurement_year>0){
      D_DEMs.files=dplyr::filter(D_DEMs.files_bsn,
                                 str_detect(name,as.character(measurement_year))==T)
    } else{
      D_DEMs.files=D_DEMs.files_bsn %>%
        mutate(year=as.numeric(str_sub(name,-4)),
               prm=as.character(str_sub(name,1,(str_length(name)-5)))) %>%
        group_by(prm,basin) %>% 
        slice(which.max(year)) %>%
        dplyr::select(.,-year)
    }
    
    # Get Dynamic DEMs
    if(NROW(D_DEMs.files)>0){
      D_DEMs.list=list()
      # insert Dynamic DEMs files to list
      for(i in 1:NROW(D_DEMs.files)){
        D_DEM_pth=as.character(D_DEMs.files[i,]$pth)
        DEM=raster(D_DEM_pth)
        if(as.character(crs(DEM))!="+proj=longlat +datum=WGS84 +no_defs"){
          DEM=projectRaster(DEM, crs="+proj=longlat +datum=WGS84 +no_defs")
        } 
        D_DEMs.list[names(DEM)]=DEM
      }
    }
  }
  
  # 1.2.4 Wells ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("1.2.4 Wells")
  
  well_pth='data/Background_layers/Wells'
  
  perfortations =fread(paste0(well_pth,"/Perforations.csv"),
                       colClasses = c("character", "character","numeric",
                                      "numeric","numeric", "numeric", "numeric","numeric", "numeric","character",
                                      "character")) %>% mutate(well_id=as.integer(shd_id))
  
  Wl = fread(paste0(well_pth,"/WL_Full_2000_1.csv")) %>%  mutate(well_id=as.integer(shd_id))
  Cl = fread(paste0(well_pth,"/WQ_Full_2000_1.csv")) %>%  mutate(well_id=as.integer(shd_id))
  
  # 1.3 Design elements - Colors, Logo and Others inputs -------------------------------------------------
  message("1.3 Design elements - Colors, Logo and Others inputs")
  
  design_pth='data/Design_elements'
  
  # 1.3.1 Colour indexs - Static ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  INDEX = as.data.frame(read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),sheet = "Index"))
  INDEX_DEMs = as.data.frame(read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),
                                        sheet = "Index_DEMs",
                                        col_types=c("text","text","text","text","text")))
  
  # 1.3.2 Colour indexs - Dynamic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("1.3.2 Colour indexs - Dynamic")
  
  if(NROW(D_DEMs.files)>0){
    Index_DEMs_Dynamic = data.frame(matrix(ncol = 5, nrow = 0))
    x = c("D.E.Ms", "f_name", "f_colour","Colour_Expreation","Colour_Expreation_list")
    colnames(Index_DEMs_Dynamic) = x
  }
  
  # 1.3.3 Logos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("1.3.3 Logos")
  
  CRSlogo = jpeg::readJPEG(paste0(design_pth,"/app_logo.jpg"), native = TRUE) 
  IHS_logo = jpeg::readJPEG(paste0(design_pth,"/IHS_logo.jpg"), native = TRUE) 
  GSI_logo = jpeg::readJPEG(paste0(design_pth,"/GSI_logo.jpg"), native = TRUE) 
  
  # 2.layers Building =====================================================================================
  message("2.layers Building")
  
  # 2.1  Wells Data maining - Base & Virtual -----------------------------------------
  message("2.1  Wells Layer  Base & Virtual, op & t Running Formt")
  CS_model_system = Geology_Descriptions_slc %>% mutate(depth=top_layer,
                                                        top_layer=elv-top_layer,
                                                        bot_layer=elv-bot_layer)%>%mutate(well_id=as.integer(well_id))
  # 2.2 Wells layer ------------------------------------------------------------------
  message("2.2 Wells layer")
  
  # 2.2.1 remove duplicates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.2.1 remove doplicates")
  CS_model_system_unit=CS_model_system %>% group_by(well_id,name,Longitude,Latitude)%>%
    dplyr::summarise(top=max(top_layer),
                     bot=min(bot_layer)) %>%
    ungroup()%>%
    dplyr::arrange(.,Longitude)
  
  # 2.2.2  Distance & topo vector ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.2.2  Distance & topo vector")
  st_crs(drawn_polyline)<-4326
  numOfPoints=round(st_length(drawn_polyline)/Resolution,digits = 0) 
  seg = st_segmentize(drawn_polyline, units::set_units(numOfPoints,m))
  tictoc::tic()
  seg_pnt= st_cast(seg, 'POINT')[,c("geometry")] %>% 
    mutate(dst=as.numeric(st_distance(.)[,1]),
           DTM=raster::extract(DTM_rst,
                               .,cellnumbers=TRUE,sp=TRUE,along=T)@data$DTM,
           Longitude=st_coordinates(.)[,1],
           Latitude=st_coordinates(.)[,2],
           id=seq(1:nrow(.))
    )
  tictoc::toc()
  seg_df=st_drop_geometry(seg_pnt) %>% as.data.table(.) %>% 
    dplyr::rename(.,"Longitude_cs"="Longitude","Latitude_cs"="Latitude")
  
  # 2.2.3  Wells distance matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.2.3  Wells distance matrix")
  join_dt=RANN::nn2(subset(seg_df,,c("Longitude_cs", "Latitude_cs")),
                    subset(CS_model_system_unit,,c("Longitude", "Latitude")),
                    k = 1, searchtype = "radius", radius =1000*Buffer) %>% 
    Reduce(cbind,.) %>%  as.data.frame(.) %>% rename_all(~(c("id","dst2cs"))) %>% 
    mutate(dst2cs=round(dst2cs/0.011,2))
  
  # 2.2.4 Wells core file & Distance alert ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.2.4 Wells core file & Distance alert")
  CS_model_system_unit_dst=bind_cols(CS_model_system_unit,join_dt) %>% 
    left_join(.,seg_df,by="id") %>% 
    mutate(
      dst_alrt=case_when(
        (dst2cs <0.1 & abs(top-DTM)>10)==T ~ T,
        TRUE                               ~ F
      )
    ) %>% as_tibble(.)
  
  dst_unit=subset(CS_model_system_unit_dst,,c("well_id","dst")) %>% dplyr::arrange(dst)
  
  # 2.2.5 Fully geology description file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  message("2.2.5 Fully geology description file")
  CS_model_system_ll = left_join(CS_model_system,
                                 subset(CS_model_system_unit_dst,,c(setdiff(names(CS_model_system_unit_dst),names(CS_model_system)),"well_id"))
                                 ,by="well_id") %>%
    dplyr::arrange(dst,well_id,top_layer)
  
  # 2.3 Perfortations layer ----------------------------------------------------
  message("2.3 Perfortations layer")
  
  perfortations_CS = dplyr::inner_join(subset(CS_model_system_unit_dst,,c("well_id","dst","DTM","top")),
                                       perfortations,by="well_id")
  
  if(isTRUE(Projection_Line)){
    # 2.4 Project wells to Cross Section ---------------------------------------
    message("Project wells to Cross Section")
    CS_model_system_unit_dst = CS_model_system_unit_dst %>% 
      mutate(diff2top=DTM-top,
             top=DTM)
    
    CS_model_system_ll=left_join(CS_model_system_ll,
                                 subset(CS_model_system_unit_dst,,c("well_id","diff2top"))) %>% 
      mutate(top_layer=top_layer+diff2top,
             bot_layer=bot_layer+diff2top,
             elv=elv+diff2top)
    
    perfortations_CS=left_join(perfortations_CS,
                               subset(CS_model_system_unit_dst,,c("well_id","diff2top"))) %>% 
      mutate(across(starts_with(c("top","bot")),~(.+diff2top)))
    
  } else {
    Projection_dst_2=data.frame(well_id=0,X=0, Y=0, dst=0, bot=0, Projection_dst_min=0, ID=0, Xproj=0, Yproj=0, deltaX=0, deltaY=0, Orientation=0)
    T_alpha=0
  }
  
  # 2.4 WL and Cl layer --------------------------------------------------------
  message("2.4 WL and Cl layer")
  
  # 2.4.1 Wl ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.4.1 Wl")
  
  # Optional - Specific year - seasonal WL values
  if(is.character(season)==TRUE){
    if(season=="Winter-Spring"){
      Wl=Wl %>% dplyr::filter(.,month_>0 & month_<6)}
    if(season=="Summer-Autumn"){
      Wl=Wl %>% dplyr::filter(.,month_>8 & month_<12)}
  }
  
  # Default - Laset year
  WL_y=Wl %>% group_by(well_id) %>% dplyr::summarise(wl=last(WL),
                                                     wl_year=last(year_))
  
  # Optional - Specific year
  if(measurement_year>0){
    WL_y=Wl %>% subset(.,year_==measurement_year,) %>% group_by(well_id) %>% dplyr::summarise(wl=last(WL),
                                                                                              wl_year=last(year_))
  }
  wl_CS =  subset(CS_model_system_unit,,c("well_id"))  %>% dplyr::inner_join(.,WL_y,by="well_id") %>%
    left_join(.,dst_unit,by="well_id")
  
  
  # 2.4.2 Cl ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.4.2 Cl")
  
  # Optional - Specific year - seasonal WL values
  
  if(is.character(season)==TRUE){
    if(season=="Winter-Spring"){
      Cl=Cl %>% dplyr::filter(.,month_>0 & month_<6)}
    if(season=="Summer-Autumn"){
      Cl=Cl %>% dplyr::filter(.,month_>8 & month_<12)}
  }
  
  
  # Default - Laset year
  Cl_y=Cl %>% group_by(well_id) %>% dplyr::summarise(cl=last(Cl),
                                                     cl_year=last(year_))
  
  # Optional - Specific year
  if(measurement_year>0){
    Cl_y=Cl%>% subset(.,year_==measurement_year,) %>% group_by(well_id) %>% dplyr::summarise(cl=last(Cl),
                                                                                             cl_year=last(year_)) # Default - Laset year
  }
  
  cl_CS =  subset(CS_model_system_unit,,c("well_id"))  %>% dplyr::inner_join(.,Cl_y,by="well_id") %>%
    left_join(.,dst_unit,by="well_id")
  
  cl_0=mean(CS_model_system_unit$bot)
  
  # 2.4.3 WLCl ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.4.3 WLCl")
  
  wlcl_CS=full_join(wl_CS,cl_CS, by = c("well_id", "dst")) %>%
    left_join(.,subset(CS_model_system_unit,,c("well_id","bot","top")), by = "well_id") %>%   # Set cl position in the CS
    mutate(cl_location=ifelse(is.na(wl)==T,rowMeans(cbind(top,bot),na.rm=T),wl),
           wl=as.numeric(wl))
  # 2.5 DEM layers -------------------------------------------------------------
  message("2.5 Build all DEMs Lines")
  
  # Populate first DEM by the DTM
  CS_points_sdf=as_Spatial(subset(seg_pnt,,c("id"))) 
  DTM=setDF(setDT(st_drop_geometry(seg_pnt)))
  DEMs=DTM
  
  # Surface smooth by moving average
  if(is.numeric(Surface_Smooth)==TRUE){
    maDTM=ma(DEMs$DTM,Surface_Smooth);colnames(maDTM)=c('maDTM')
    DEMs=cbind(DEMs,maDTM) %>% mutate(DTM=maDTM)
    DEMs=dplyr::select(DEMs,-starts_with("maDTM"))
  }
   bb=1
  # Add rasters by loop
  if(NROW(actiev_library_n)>0 | exists("DEM_rst",where=additional_layers_lst)==T){
    for(i in 1:static_DEM_i){
      print(i)
      # for test mode  -> DEMs.list[[i]]= raster("G:/Geohydrology/Apps/External_Data/eaocen_rst_old.tif")
      DEM_ID=as.character(DEMs.files$name[i])
      dem_i = data.frame(dem_i_elv=as.numeric(raster::extract(DEMs.list[[i]],
                                                              CS_points_sdf,cellnumbers=T,sp=F,along=T)[,2]))
      DEMs=cbind(DEMs,dem_i) %>% mutate(DEMs, "{DEM_ID}":=ifelse(dem_i_elv>DTM,DTM,dem_i_elv)) %>% dplyr::select(.,-dem_i_elv)
    }
  }
 
  
  # 2.6 DEM layers - Dynamic ------------------------------------------------------------------
  message("2.6 DEM layers - Dynamic")
  
  if(NROW(D_DEMs.files)>0){
    # Select Base Lyer df
    DBBL=subset(DEMs.files,!is.na(base_layer))
    for (i in 1:NROW(DBBL)){
      # 2.6.1 Build Base layer line  for each basin ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      message("2.7.1 Build Base layer line  for each basin")
      DBBL_nam=paste0(DBBL$basin[i],"_","Base")
      DBBL_nam_i=dplyr::filter(DEMs.files,!is.na(base_layer))$name[i]
      DEMs=DEMs %>% mutate("{DBBL_nam}":=DEMs[,DBBL_nam_i])
      
      # 2.6.2 Build all DEMs Lines - Dynamic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      message("2.6.2 Build all DEMs Lines - Dynamic")
      D_DEMs.fltr=D_DEMs.list[grep(DBBL$basin[i], names(D_DEMs.list))]  
      
      if(NROW(D_DEMs.fltr)>0){
        for(j in 1:NROW(D_DEMs.fltr)){                          
          print(j)
          # Intersect raster values by the CS line
          D_DEM_ID=as.character(names(D_DEMs.fltr[j]))
          d_dem_ij = data.frame(d_dem_ij_elv=as.numeric(raster::extract(D_DEMs.fltr[[j]],
                                                                        CS_points_sdf,cellnumbers=T,sp=F,along=T)[,2]))
          DEMs=cbind(DEMs,d_dem_ij) %>%
            mutate("{D_DEM_ID}":=ifelse(d_dem_ij_elv>"{DBBL_nam}",d_dem_ij_elv,"{DBBL_nam}")) %>%
            dplyr::select(.,-d_dem_ij_elv)
        } 
      } else{message(paste0("No dynamic data in the Basin: ",DBBL$basin[i]))}
      
    }
    
    # 2.6.3 Set Dynamic Index_DEMs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    message("2.6.3 Set Dynamic Index_DEMs")
    
    for(j in 1:NROW(D_DEMs.files)){
      D_DEMnm_j=names(D_DEMs.list[[j]])
      Index_DEMs_Dynamic[j,1]=D_DEMnm_j
      Index_DEMs_Dynamic$f_name[j]=D_DEMnm_j
      #  Water Level DEM
      if(str_detect(D_DEMnm_j, "WL")==T){
        Index_DEMs_Dynamic$f_colour[j]=as.character("#3C7DC4")}
      #  Cl DEM
      if(str_detect(D_DEMnm_j, "Cl")==T){
        Index_DEMs_Dynamic$f_colour[j]=as.character("#e00d0a")}
      #  indfine dynmic DEM
      if(str_detect(D_DEMnm_j, "Cl")!=T & str_detect(D_DEMnm_j, "WL")!=T){
        Index_DEMs_Dynamic$f_colour[j]=as.character("#e0de0a")}
      
      Index_DEMs_Dynamic$Colour_Expreation[j]=paste0(D_DEMnm_j," = ",'"',Index_DEMs_Dynamic$f_colour[j],'"')
      Index_DEMs_Dynamic$Colour_Expreation_list[j]=paste0(Index_DEMs_Dynamic$Colour_Expreation[j],",")
    }
    
    # 2.6.4 Set Dynamic cols_DEMs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    message("2.6.4 Set Dynamic cols_DEMs")
    
    cols_DEMs_dynamic=c()
    cols_DEMs=ColourExpreation_DEM()
    for(j in 1:NROW(D_DEMs.files)){
      f_colour=Index_DEMs_Dynamic$f_colour[j]
      cols_DEMs_dynamic[j]=c(temp_name = f_colour)
      names(cols_DEMs_dynamic)[j] <- Index_DEMs_Dynamic[j,1]
    }
    
    # 2.6.5 Combine Dynamic to Static ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    message("2.6.5 Combine Dynamic to Static")
    
    INDEX_DEMs=bind_rows(INDEX_DEMs,Index_DEMs_Dynamic)
    cols_DEMs=c(cols_DEMs,cols_DEMs_dynamic)
  }
  
  # 2.7 Peeper DEMs data to ggplot -----------------------------------------------------------
  message("2.7 Preper DEMs data to ggplot")
  DEMs_mlt=reshape2::melt(as.data.frame(dplyr::select(DEMs,-c("id","Longitude","Latitude"))),id=c("dst"))    
  DEMs_cst=DEMs
  DEMs=DEMs_mlt %>% mutate(D.E.Ms = as.character(variable)) %>% dplyr::inner_join(.,DTM,by="dst") %>%
    mutate(Depth=DTM-value)
  # 2.7.1 Build hierarchy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.7.1 Build hierarchy")
  hierarchy=Static_Background[basin %in% Background & !is.na(hierarchy),] %>% dplyr::arrange(.,basin,hierarchy) %>% 
    dplyr::left_join(INDEX_DEMs,by=c("name"="D.E.Ms")) %>% dplyr::distinct(.,hierarchy,basin,.keep_all = T)
  # 2.7.2 Build Exposed Layer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.7.2 Build Exposed Layer")
  exposed_df=DEMs_mlt %>% 
    dplyr::rename(.,name=variable,elv=value) %>%
    left_join(.,hierarchy,, by = "name") %>%
    dplyr::filter(.,hierarchy>0,) %>%
    group_by(dst) %>%
    filter(elv == max(elv,na.rm = T)) %>%
    dplyr::rename(.,exposed=name)
  DEMs_exp=left_join(DEMs_cst,subset(exposed_df,,c("dst","exposed","f_colour")),by = "dst")
  
  # 2.7.3 Build active libraries list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.7.3 Build active librarys list")
  librarys=dplyr::distinct(hierarchy,basin,.keep_all = F) %>%
    dplyr::filter(.,basin!="national",)
  
  # 2.8 Transforms layer --------------------------------------------------------------------
  message("2.8 Transforms layer")
  # 2.8.2 Extract Falut Layer & DTM sdf ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("2.8.1 Extract Falut Layer & DTM sdf")
  
  if(NROW(transforms_st)>0){
    join_dt=RANN::nn2(subset(seg_df,,c("Longitude_cs", "Latitude_cs")),
                      subset(transforms_st,,c("Longitude", "Latitude")),
                      k = 1, searchtype = "radius", radius = 0.01) %>% 
      Reduce(cbind,.) %>%  as.data.frame(.) %>% rename_all(~(c("id","dst2cs"))) %>% 
      dplyr::select(.,-dst2cs)
    transforms_DEM=cbind(transforms_st,join_dt) %>% left_join(.,seg_df) %>% 
      subset(.,,c("DTM","dst"))
  } else {
    # 2.8.3 Transforms layer - Inactive ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    message("2.8.2 Transforms layer- Inactive")
    transforms_DEM=data.frame(Longitude=CS_model_system_unit_dst$Longitude[1],
                              Latitude=CS_model_system_unit_dst$Latitude[1],
                              DTM=CS_model_system_unit_dst$top[1],
                              dst=0) # 2595.072
    T_alpha=0
  }
  if(geological_cs_surf!="blind"){
    # 2.9 Geology map Cross Section -------------------------------------------------------
    message("2.9 Geology map Cross Section")
    basemap_pth="data/Background_layers/BaseMaps/"
    
    sf::sf_use_s2(FALSE)
    # 2.9.1 Geology map 1:200,000  intersection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    CS_geopoints_200=setDT(st_join(st_as_sf(CS_points_sdf),geology_200,left = T,largest=T))[,c("Code","Symbol")]
    geoIDX_200=read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),sheet = "geoINDEX200")
    CS_geopoints_200IDX=left_join(CS_geopoints_200,geoIDX_200,by="Code")
    colnames(CS_geopoints_200IDX) = paste("200", colnames(CS_geopoints_200IDX), sep = "_")
    
    # 2.9.2 Geology map 1:50,000  intersection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    CS_geopoints_50=setDT(st_join(st_as_sf(CS_points_sdf),geology_50,left = T,largest=T))[,c("Code","Symbol")]
    geoIDX_50=read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),sheet = "geoINDEX50")
    CS_geopoints_50IDX=left_join(CS_geopoints_50,geoIDX_50,by="Code")
    colnames(CS_geopoints_50IDX) = paste("50", colnames(CS_geopoints_50IDX), sep = "_")
    sf::sf_use_s2(T)
    
    # 2.9.3 Build DTM symbology DF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    CS_geopoints=cbind(CS_geopoints_50IDX,CS_geopoints_200IDX) %>%
      transmute(Code=ifelse(!is.na(`50_Code`),`50_Code`,`200_Code`),
                Symbol=ifelse(!is.na(`50_Symbol`),as.character(`50_Symbol`),as.character(`200_Symbol`)),
                ladel=ifelse(!is.na(`50_ladel`),`50_ladel`,`200_ladel`),
                hex=ifelse(!is.na(`50_hex`),`50_hex`,`200_hex`))
    
    DTM_CS_df=cbind(DTM,CS_geopoints)
    
  }else{
    DTM_CS_df=DTM %>% mutate(Symbol="Surface",ladel="Surface")
  }
  
  # 3. Cross Section Elemints Creation  ==================================================
  message("3. Cross Section Elemints Creation")
  
  # 3.1 Set Dimensions -----------------------------------------------------------------------
  message("3.1 Set Dimensions")
  
  # 3.1.1 Horizontal Dimensions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("3.1.1 Horizontal Dimensions")
  
  prop_fac=max(CS_model_system_ll$dst)*length(CS_model_system_unit)/700
  n_wells=NROW(CS_model_system_unit)
  
  # 3.1.2 Vertical Dimensions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("3.1.2 Vertical Dimensions")
  
  max_DEM=max(DTM$DTM,na.rm = T)                             # Topographic max
  delta_y=abs(max(DTM$DTM,na.rm = T)-min(DTM$DTM,na.rm = T)) # Gradient max
  max_depth=max(abs(CS_model_system_ll$depth))               # Wells depth max
  min_elevation=min(min(DEMs$value), min(CS_model_system_ll$bot_layer)) # Topographic min
  total_max=max(delta_y,max_depth)                           # Total max
  prop_fac_y=max_depth/(delta_y*100)
  vertical_stretch=round(max(CS_model_system_unit_dst$dst)/(max_DEM-min_elevation),0)
  
  # 3.1.4 User Definitions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("3.1.4 User Definitions")
  
  ls=as.numeric(as.character(label_size))                      # label size
  space=as.numeric(as.character(Interval_between_labeles))     # Interval between labeles
  
  # 3.2 Set Orientation ------------------------------------------------------------------------
  message("3.2 set Orientation")
  
  # 3.2.1 Cross section orientation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("3.2.1 Cross section orientation")
  azimuth_df=data.frame(az=c(0,45,90,135,180,225,270,315,360),
                        dir=c("N-S","SW-NE","E-W","NW-SE","N-S","NE-SW","W-E","SE-NW","S-N"))
  cs_azimuth=nngeo::st_azimuth(seg_pnt[1,],seg_pnt[nrow(seg_pnt),])
  azimuth_df=azimuth_df %>% 
    mutate(ctg=abs(azimuth_df$az-cs_azimuth)) %>% 
    dplyr::arrange(ctg) 
  cs_diraction=unlist(str_split(azimuth_df$dir[1],"-"))
  diraction_start=cs_diraction[1]
  diraction_end=cs_diraction[2]
  
  # 3.2.2 Wells orientation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("3.2.2 Wells orientation")
  # Calc orientation
  cmudp=CS_model_system_unit_dst %>%  mutate(az=
                                               nngeo::st_azimuth(
                                                 st_as_sf(., coords = c("Longitude", "Longitude"), crs = 4326) ,
                                                 st_as_sf(., coords = c("Longitude_cs", "Longitude_cs"), crs = 4326))
  ) %>% 
    as.data.table(.,key="az")
  azimuth_dt=setDT(azimuth_df,key="az")
  
  cmudp_dt=azimuth_dt[cmudp,roll="nearest"] %>% 
    mutate(dir=str_split(dir,"-") %>% map_chr(., 1))
  
  # Build chart elements
  cmudp_fltr=dplyr::filter(cmudp_dt,dst2cs>0.01)
  cmudp_fltr_t=dplyr::filter(cmudp_dt,dst_alrt==T)
  p_font=0.07*ls
  p_font_t=0.9*ls
  if(nrow(cmudp_fltr)==0){cmudp_fltr=cmudp_dt[1,];p_font=0}
  if(nrow(cmudp_fltr_t)==0){cmudp_fltr_t=cmudp_dt[1,];p_font_t=0}
  
  # 3.3 Set Colors ---------------------------------------------------------------------------
  message("3.3 Set Colours")
  
  INDEX1=INDEX %>% filter(.,type==CS_type) %>% mutate(f_ID=as.character(f_ID)) %>% setDT(key="f_ID")
  # Select CS type
  f_ID_df=subset(CS_model_system_ll,,c(CS_type)) %>% #data.frame(CS_model_system_ll[which(names(CS_model_system_ll)%in%CS_type)]) %>%
    dplyr::rename(.,"f_ID"=CS_type) %>%
    mutate(f_ID=as.character(f_ID))
  CS_f_ID=cbind(CS_model_system_ll,f_ID_df) %>% setDT(key="f_ID") 
  CS_model_system_ll=CS_f_ID[INDEX1,nomatch=0]   
  #CS_model_system_ll=CS_f_ID %>% dplyr::inner_join(.,INDEX1,by="f_ID")                        # attribut color to formation 
  cols=ColourExpreation()                                                             # Get colour vector for scale_fill_manual
  
  # 3.3.1 Set DEMs Colours ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("3.3.1 Set DEMs Colours")
  
  if(NROW(actiev_library_n)>0 | exists("DEM_rst",where=additional_layers_lst)==T){
    DEMs=DEMs%>% dplyr::inner_join(.,INDEX_DEMs,by="D.E.Ms")
    if(NROW(D_DEMs.files)==0){cols_DEMs=ColourExpreation_DEM()}
  } else{ DEMs=DEMs%>%mutate(D.E.Ms="DTM",f_name="DTM")}
  
  # 3.3.2 Set Geo Map Colours ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("3.3.2 Set Geo Map Colours")
  
  if(geological_cs_surf=="geomap"){
    cols_geo=DTM_CS_df$hex                                   
    names(cols_geo)=DTM_CS_df$Symbol
    surf_status=T
    geotitle="Geology along Surface"
    GSIlogosize=0.07
    GSItxtsize=10
    backcol= "gray70"
  } else if(geological_cs_surf=="geomap_free_colors"){
    l=length(table(DTM_CS_df$Symbol)) 
    geopal=colorRampPalette(c("#083275","#174182","#23539e","#6600FFFF","#0000FFFF","#0066FFFF","#00CCFFFF","#00FFCCFF",
                              "#00FF66FF","#00FF00FF","#66FF00FF","#CCFF00FF","#FFCC00FF","#FF6600FF","#FF0000FF")) 
    cols_geo=geopal(l)
    surf_status=T
    geotitle="Geology along Surface"
    GSIlogosize=0.07
    GSItxtsize=9
    backcol= NA
  }  else{
    l=length(table(DTM_CS_df$dst)) 
    geopal=colorRampPalette(c("black")) 
    cols_geo=geopal(l)
    surf_status=F
    geotitle="Geology along Surface-Inactive"
    GSIlogosize=0.0007
    GSItxtsize=0
    backcol= NA
  } 
  
  
  
  # 3.4 Build Legand ---------------------------------------------------------------------------
  message("3.4 Build Legand")
  legand_df=subset(CS_model_system_ll,,c("f_name","MainDatabaseCode"))
  dp=duplicated(legand_df$f_name)
  legand_dp=legand_df[!dp,]
  legand_str=dplyr::arrange(legand_dp,MainDatabaseCode)
  legand_elment=legand_str$f_name
  
  # 3.5 Set Formation Switch ------------------------------------------------------------------
  message("3.5 Set Formation Switch")
  
  CS_model_system_formation=CS_model_system_ll %>% group_by(well_id)%>%
    mutate(top_well=max(top_layer))%>% ungroup(.)%>%
    group_by(well_id,name,f_name,Longitude,Latitude,dst,top_well)%>%
    dplyr::summarise(Elevation=min(bot_layer)) %>% ungroup(.)%>%
    dplyr::arrange(.,well_id,desc(Elevation))%>%
    mutate(Depth=as.character(top_well-Elevation),
           Thickness=ifelse(well_id==lag(well_id),as.numeric(Depth)-lag(as.numeric(Depth)),as.numeric(Depth)))%>%
    mutate(Distance=dst+prop_fac/2)
  
  # 4 Cross Section Bulding  =================================================================================
  message("4.  Cross Section Bulding")
  
  # 4.1 Labels and Data Fixing ------------------------------------------------------------------------------
  message("4.1 Labels and Data Fixing")
  
  # 4.1.1 Labels Fixing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("4.1.1 Labels Fixing")
  
  wl_CS=wlcl_CS%>% mutate(`Water Level`=as.numeric(wl),
                          Year=wl_year,
                          Distance=dst-prop_fac*1.5)
  
  wq_CS=wlcl_CS%>% mutate(Concentration=cl,
                          concentration_l=cl-max(wl),
                          Year=cl_year,
                          Distance=dst+prop_fac*1.5)
  
  CS_model_system_ll=CS_model_system_ll %>% left_join(.,subset(CS_model_system_formation,,c(well_id,Elevation,Depth,f_name,Thickness)),by=c("well_id","f_name"))%>%
    mutate(`Unit Name`=f_name)
  
  
  DEM_plot_df=NULL
  if(NROW(actiev_library_n)>0 | exists("DEM_rst",where=additional_layers_lst)==T){
    DTM_plot_df <- na.omit(DTM_CS_df) %>% mutate(Distance=dst,Elevation=DTM)%>%
      mutate(x_next = lead(Distance), y_next = lead(Elevation))
    DEM_plot_df <- dplyr::filter(DEMs,D.E.Ms!="DTM") %>%
      mutate(Distance=dst,`Unit Name`=as.factor(f_name),Elevation=value)
  } else{
    DTM_plot_df <- na.omit(DTM_CS_df)%>% mutate(Distance=dst,Elevation=DTM)
    DEM_plot_df=read.csv("data/DEMs/DEM_plot_empty.csv") %>%
      mutate(Elevation=max(CS_model_system_ll$top_layer))
    cols_DEMs="#000000"
  }

  # 4.1.2 Cross section title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("4.1.2 Cross section title")
  if(isTRUE(Projection_Line)){
    title_CS=paste0(title_CS," - Projected to line")
  }
  
  
  # 4.1.3 Dimension Fixing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("4.1.3 Dimension Fixing")
  
  total_y_min=round(min(CS_model_system_ll$Elevation,na.rm = T)-prop_fac, digits = -2)
  total_y_max=round(max(DTM_plot_df$Elevation,na.rm = T)+prop_fac, digits = -2)
  if(NROW(actiev_library_n)>0){
    total_y_min=min(total_y_min,round(min(DEM_plot_df$Elevation,na.rm = T)-prop_fac,digits = -2))
  }
  
  total_x_max=round(max(CS_model_system_unit_dst$dst+prop_fac),-2)
  
  # 4.1.4 Build DEMs legend ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("4.1.4 Build DEMs legend")
  if(NROW(actiev_library_n)>0){
    DEMs_Legand=DEM_plot_df %>% group_by(f_name,f_colour,Colour_Expreation,D.E.Ms) %>%
      dplyr::summarise(m_dst=mean(dst,na.rm=T),
                       m_elv=mean(value,na.rm=T)) %>%
      mutate(legand_DEMS=str_replace_all(D.E.Ms, "_", " "))
  } else{DEMs_Legand=read.csv("data/DEMs/DEMs_Legand_empty.csv")%>% mutate(m_elv=total_y_max)}
  
  # 4.1.5 Build Titles Elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("4.1.5 Build X/Y title Elements")
  cs_title_x=max(CS_model_system_unit_dst$dst)/2
  cs_title_y=max_DEM+space*1.4
  x_title_l=min(CS_model_system_unit_dst$dst)-prop_fac_y*5
  x_title_r=max(CS_model_system_unit_dst$dst)+prop_fac_y*5
  y_title_r=max_DEM+0.9*space
  xy_title_lab=paste0(round(min(CS_model_system_unit_dst$Longitude),2)," / ",round(min(CS_model_system_unit_dst$Latitude),2))
  
  # 4.1.5 Data Fixing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message("4.1.6 Data Fixing")
  
  perforation_alpha=1
  if(max(perfortations_CS$dst)==-Inf){
    perfortations_CS=CS_model_system_ll%>% dplyr::filter(.,top_layer==max(top_layer)) %>%
      mutate(bot_e=bot_layer,
             top_e=top_layer,
             bot_d=NA,
             top_d=NA,
             dim=NA,
             type=NA)
    perforation_alpha=0}
  
  wl_alpha=1
  if(max(wl_CS$Distance)==-Inf){
    wl_CS=CS_model_system_ll%>% dplyr::filter(.,top_layer==max(top_layer)) %>%
      mutate(Distance=dst,
             `Water Level`=bot_layer,
             Year=NA)
    wl_alpha=0}
  
  wq_alpha=1
  if(max(wq_CS$Distance)==-Inf){
    wq_CS=CS_model_system_ll%>% dplyr::filter(.,top_layer==max(top_layer)) %>%
      mutate(Distance=dst,
             Concentration=top_layer,
             Year=NA)
    wq_alpha=0}
  
  
  # 4.2 Cross section Creation ----------------------------------------------------------
  message("4.2 Cross section Creation")
  
  p = ggplot() +
    #Title Panel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = max_DEM+0.7*space, ymax = max_DEM+1.5*space),
              fill="gray70",alpha=0.9)
  if(Build_Solids==T){
    #DEMs poly ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (j in 1:NROW(librarys)){
      if (librarys$basin[j] %in% c("coastal")){
        hierarchy_j=as.data.table(hierarchy)[basin==librarys$basin[j]]} else{
          hierarchy_j=as.data.table(hierarchy)[basin %in% c("national",librarys$basin[j])] }
      
      for (i in 1:NROW(hierarchy_j)){
        if (i==1){
          top_i="DTM"
          base_i=hierarchy_j$name[i]
          hex_fill="gray"
          DEMs_exp_i=as.data.table(na.omit(DEMs_exp[,c("id","dst",top_i,base_i)]))
          setnames(DEMs_exp_i, c(top_i,base_i), c("top_i","base_i"))
        }else{
          top_i_n=hierarchy_j$name[i-1]
          base_i_n=hierarchy_j$name[i]
          
          hex_fill=as.character(hierarchy_j$f_colour[i-1])
          DEMs_exp_i=as.data.table(DEMs_exp[,c("id","dst","DTM","exposed",top_i_n,base_i_n)])
          setnames(DEMs_exp_i, c(top_i_n,base_i_n), c("top_i","base_i"))
          DEMs_exp_i=DEMs_exp_i %>%
            mutate(
              top_i=ifelse(is.na(top_i) & as.character(exposed) == as.character(base_i_n),DTM,top_i),
              top_i=ifelse(top_i<base_i,base_i,top_i)
            )
        }
        p=p+
          geom_ribbon(data=DEMs_exp_i, aes(x=dst,ymin=as.numeric(top_i), ymax=as.numeric(base_i)),fill=hex_fill,alpha=0.3)
      }
    }  
  }
  
  #DTM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p=p+  
    geom_segment(data=DTM_plot_df,aes(xend = x_next, yend = y_next,x=Distance,y=Elevation,colour = Symbol,group=1),show.legend = surf_status, size=1) +
    scale_colour_manual(values=cols_geo,name=geotitle,labs(color = geotitle)) +
    new_scale("colour") +
    # Background layers names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_text(data=DEMs_Legand,aes(x=0,y=m_elv,label=legand_DEMS,fontface = "bold",
                                   alpha=0.1,colour = D.E.Ms),
              size = ifelse(NROW(actiev_library_n)>0 | exists("DEM_rst",where=additional_layers_lst)==T,0.05*ls,0),angle=45) +
    scale_colour_manual(values=cols_DEMs,guide = FALSE) +
    new_scale("new_aes") +
    #DEMs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_path(data=DEM_plot_df, aes(x=Distance,y=Elevation,colour=`Unit Name`,label=Depth),na.rm = T,
              size=ifelse(NROW(actiev_library_n)>0 | exists("DEM_rst",where=additional_layers_lst)==T,0.7,0),show.legend =T,linetype = "dashed") +
    scale_fill_manual(values=cols_DEMs,guide = FALSE) +
    # base - formation and location ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_rect(data=CS_model_system_ll,
              aes(xmin = dst, xmax = dst+prop_fac, ymin = bot_layer,
                  ymax = top_layer,fill=`Unit Name`,text =  paste("Top Depth: ", depth,
                                                                  "<br>Top Elevation: ", round(elv-depth,0))))+
    scale_fill_manual(values =cols,name=paste0("Subsurface Units (",CS_type,")"),breaks=legand_elment) +
    
    # perforations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_rect(data = as.data.frame(perfortations_CS),
              aes(xmin = dst+prop_fac*1.3, xmax = dst+prop_fac*1.6, ymin = bot_e, ymax = top_e,text =  paste("Top Perforations: ", top_d,
                                                                                                             "<br>Bot Perforations: ", bot_d)), fill ="black")+
    # wl ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_point(data =wl_CS,aes(x = Distance,y=`Water Level`,text =  paste("Year: ", Year)),color ="blue", fill ="blue",shape=25,size = 0.05*ls
    )+ # signe
    geom_label(data =wl_CS,aes(x = Distance,y=`Water Level`+0.25*space,label = round(`Water Level`,0)), colour ="blue",size = 0.05*ls,alpha=0.5)+ # value
    # cl ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_label(data =wq_CS,aes(x =Distance,y=cl_0+as.numeric(Concentration),label = round(as.numeric(Concentration),0)), colour ="red",size = 0.05*ls,alpha=0.5)+ # value
    # General elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # cs_id ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_text(aes(x = min(seg_df$dst)-prop_fac_y*5,y=max_DEM+1.4*space,label =cs_id), colour ="black",size =0.1*ls,vjust = "inward")+
    geom_text(aes(x = max(seg_df$dst)+prop_fac_y*5,y=max_DEM+1.4*space,label =paste0(cs_id,"'")),colour ="black",size =0.1*ls,vjust = "inward")+
    # Diraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_text(aes(x = min(seg_df$dst)-prop_fac_y*5,y=max_DEM+1.2*space,label =diraction_start), colour ="black",size = 0.1*ls,vjust = "inward")+
    geom_text(aes(x = max(seg_df$dst)+prop_fac_y*5,y=max_DEM+1.2*space,label =diraction_end), colour ="black",size = 0.1*ls,vjust = "inward")+
    # X/Y ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_text(aes(x = x_title_l, y=y_title_r, label=xy_title_lab), colour ="black",size = 0.1*ls,vjust = "inward")+
    geom_text(aes(x = x_title_r, y=y_title_r, label=xy_title_lab), colour ="black",size = 0.1*ls,vjust = "inward")+
    # vertical_stretch ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_text(aes(x = 0-prop_fac*1.5, y=min_elevation, label=paste0("Vertical Stretch= 1:",vertical_stretch)),
              colour ="black",angle = 90,size = 0.1*ls,fontface = "italic")+
    # Axis Labels  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    scale_x_continuous(breaks=seq(0, max(seg_df$dst)+prop_fac_y*5, 1000))+
    scale_y_continuous(breaks=seq(total_y_min-space, round(max_DEM+space, digits = 0), 100))+
    
    # labels ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    labs(x="Distance [m]",y="Elevation [masl]")+
    theme(axis.text=element_text(size=0.4*ls),
          axis.text.x = element_text(colour="black",size=0.3*ls,angle=45,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=0.3*ls,angle=0,hjust=.5,vjust=.5,face="plain"))+
    geom_label(aes(x = cs_title_x, y=cs_title_y,label =title_CS), colour ="black",alpha=0.5,size =0.2*ls,vjust = "inward")+
    
    # Transforms ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_linerange(data=na.omit(transforms_DEM),aes(x=dst,ymin=total_y_min*0.5,ymax=DTM,text =  paste("Distanse: ", dst)),color="gray5",size=0.3,alpha=0.9)+
    geom_linerange(data=na.omit(transforms_DEM),aes(x=dst-50,ymin=DTM-abs(total_y_min)*0.20,ymax=DTM-abs(total_y_min)*0.10,text=NULL),color="gray5",size=0.3,alpha=0.3)+
    geom_linerange(data=na.omit(transforms_DEM),aes(x=dst+50,ymin=DTM-abs(total_y_min)*0.20,ymax=DTM-abs(total_y_min*0.10),text=NULL),color="gray5",size=0.3,alpha=0.3)+
    # Projection Line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_label(data =cmudp_fltr,
               aes(x = dst,y=bot-0.5*space,label =paste0(dir," ",round(1000*dst2cs,0)," m")),
               colour ="black",size = 0.07*ls,alpha=0.5,fontface="italic")+
    geom_label(data = cmudp_fltr_t,
               aes(x = dst,y=bot-0.5*space,label =paste0(dir," ",round(1000*dst2cs,0)," m")),
               colour ="red",size = 0.09*ls,alpha=0.5,fontface="italic")+
    
    # name, top and bot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    geom_text(data = CS_model_system_unit_dst,
              aes(x = dst,y=top+0.3*space,label =round(top,0)), colour ="black",size =0.05*ls,alpha=0.9)+ # top
    geom_text(data = CS_model_system_unit_dst,
              aes(x = dst,y=bot-0.2*space,label =paste0("TD=",round(bot,0))), colour ="black",size = 0.05*ls,alpha=0.5)+ # bot
    geom_text(data = CS_model_system_unit_dst,
              aes(x = dst,y=top+0.8*space,label =name), colour ="black",angle = 45,size =0.1*ls,alpha=0.9)+ # name
    # Reset Final Dims ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    scale_y_continuous(breaks = seq(total_y_min,total_y_max, by=vertical_resolution),
                       limits = c(total_y_min,max(max_DEM+1.5*space,total_y_max)))+
    scale_x_continuous(breaks = seq(0,total_x_max, by=Resolution*5))
  # limits ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(is.numeric(Lower_Limit)==TRUE){
    CS_Lower_Limit=dplyr::filter(CS_model_system_unit_dst,Lower_Limit>bot)
    p=p+ylim(Lower_Limit,max_DEM+1.4*space)
    if(NROW(CS_Lower_Limit)>0){
      p=p+
        geom_text(data = CS_Lower_Limit,
                  aes(x = dst,y=Lower_Limit,label ="Cut"), colour ="black",angle = 45,size =0.1*ls,alpha=0.5) # cutting
    }
    total_y_min=Lower_Limit
  }
  
  # 4.3 Additives -----------------------------------------------------------------------------
  message("4.3 Additives")
  
  # 4.3.1 Credits ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  credits=c("Surface geology: Israeli Geological Survey",
            "Subsurface geology (Most): The Israeli Hydrological Service/ Israeli Geological Survey/ The Israeli Geophysical Institute")
  if(NROW(actiev_library_n)>0){
    libraris_credits= filter(Static_Background,basin %in% actiev_library_n &
                               !is.na(credit))
    if(nrow(libraris_credits)>0){
      libraris_credits=unique(Static_Background$credit)
      credits=paste(credits,libraris_credits,sep = "; ")
    }                   
  }
  
  
  pd=ggdraw() +
    draw_label("Draft", color = "#C0A0A0", size = 140, angle = 45,alpha = 0.4) +
    draw_plot(p +
                theme_half_open(font_size = 10,line_size = 0)+
                theme(plot.background = element_rect(fill = backcol, color = NA),
                      axis.text.x = element_text(angle=45,hjust=.5,vjust=.5)))+
    draw_image(IHS_logo,.91, .01, .12, .1)+
    #draw_image(GSI_logo,.90, .08, GSIlogosize, GSIlogosize)+
    draw_image(CRSlogo,.86, .05, .14, 1.75)+
    draw_text("Product of CSMS software",
              color = "blue",fontface="italic", size = 11, x=.925, y=.87)+
    draw_text(credits,color = "black",fontface="italic", size = GSItxtsize, x=0.09, y=c(0.09,0.07),hjust =0)
  
  # 4.4 Editable Cross section as .pptx -------------------------------------------------------------------
  message("4.4 Editable Cross section as .pptx")
  
  doc=read_pptx() %>%
    add_slide(., layout = "Title and Content", master = "Office Theme") %>%
    ph_with(rvg::dml(x=doc,  ggobj=pd, left=0.1, top=0.1, width=9, height=6),
            location = ph_location_type(type = "body"))
  
  # 4.5 Interactive Cross section as .html -------------------------------------------------------------------
  message("4.5 Interactive Cross section as .html")
  cs_html =ggplotly(p)
  
  for (k in 1:nrow(DTM_plot_df)) {
    cs_html <- add_segments(cs_html,
                            data = DTM_plot_df[k,],inherit=FALSE, 
                            x = ~Distance, xend = ~x_next,
                            y = ~Elevation, yend = ~y_next,
                            alpha = 1, size = I(2),text=~Symbol,  line=list(color=~hex)
    )
  }
  cs_html=cs_html %>% layout(showlegend = F,
                             xaxis = list(showgrid = F),
                             yaxis = list(showgrid = F),
                             title=title_CS,
                             paper_bgcolor="rgba(255,255,255,1)",
                             plot_bgcolor="rgba(200,200,200,1)")
  
  # 5. Return Cross Section to the App =====================================================================
  message("5. Return Cross Section to the App")
  cs_raw=p+theme(legend.position = "none")
  cs_preview=pd
  cs_ppt=doc
  cs_data=list("total_y_min"=total_y_min,"CS_line"=as_Spatial(drawn_polyline),"CS_points_sdf"=CS_points_sdf,"CS_dtm_df"=DTM_plot_df,"DEM_plot_df"=DEM_plot_df,"wells_plot_df"=CS_model_system_ll,"prop_fac"=prop_fac,"actiev_library_n"=actiev_library_n,"cs_id"=cs_id)
  charts_lst=list("cs_raw"=cs_raw,"cs_preview"=cs_preview,"cs_ppt"=cs_ppt,"cs_html"=cs_html,"cs_data"=cs_data)
  message("Cross Section was Successfully Completed")
  return(charts_lst)
  gc()
}
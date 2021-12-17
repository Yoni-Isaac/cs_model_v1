#  Geology Horizon Builder ##############################################################################
message("Geology Horizon Builder")
# 0.Initiation =========================================================================================
message("0.Iinitiation")
# 0.1 Get Running Parameters ---------------------------------------------------------------------------
message("0.1 Get Running Parameters")
Background_path="G:/Geohydrology"
gm_crs="+proj=longlat +datum=WGS84"
Type_of_runing="t"

# 1. Create New Point #######################################################
if(Type_of_runing=="u_t"){
  # This UT ran Just in debug moode of the app
  new_point_ll=horizonewpnt(
    Select_horizon=input$Select_horizon,
    cs_horizons=cs_horizons,
    plot_click=input$plot_click,
    DEM_plot_df=charts$cs_data$DEM_plot_df
  )
}
# Func
horizonewpnt=function(Select_horizon,cs_horizons,plot_click,DEM_plot_df){
  # Get Horizon Properties
  horizon_id=as.character(Select_horizon)
  pnt_horizon=dplyr::filter(cs_horizons,f_name==horizon_id,)
  # Get Chart coordinates 
  new_point=data.frame(Distance = round(plot_click$x,0), Elevation = round(plot_click$y,2),Horizon=horizon_id)
  # Get Map coordinates
  cs_pnts_dt=as.data.table(DEM_plot_df)[,c("dst","Longitude","Latitude")]
  colnames(cs_pnts_dt)=c("Distance","Longitude","Latitude")
  cs_pnts_dt$Range=cs_pnts_dt$Distance
  # Join 
  new_point_ll=cs_pnts_dt[as.data.table(new_point),on="Distance", roll="nearest"]
  new_point_ll$Range=round(abs(new_point_ll$Range-new_point_ll$Distance),0)
  new_point_ll$method="manual"
  return(new_point_ll)
}

# 2. Fill Horizons ##########################################################
# Unit Test
if(Type_of_runing=="u_t"){
  fill_horizons_df=fill_horizons(
    tab_raw=read.csv(paste0(Background_path,'/Apps/External_Data/tab_raw_segment.csv')),
    res=10,
    int_mathos="linear" # "spline"
  )
  horizon_dfc=rbind(fill_horizons_df,subset(tab_raw,,c(names(fill_horizons_df))))
  h_C=ggplot(horizon_dfc) +
    aes(x = Distance, y = Elevation, colour = method) +
    geom_point(shape = "circle", size = 1.5,alpha=0.5) +
    scale_color_hue(direction = 1) +
    theme_minimal()
  h_C
  h_CA=ggplotly()
}
# Func
fill_horizons=function(tab_raw,res,int_mathos){
  # 2.1 Get Horizons Dims ===================================================
  tab_dims=tab_raw %>% group_by(Segment) %>% 
    summarise(start=min(Distance,na.rm=T),
              end=max(Distance,na.rm=T),
              dist=end-start,
              nods=dist/res)
  # 2.2 Fill Horizons =======================================================
  if(int_mathos=="linear"){int_func=approx;int_method = "linear"}
  if(int_mathos=="spline"){int_func=spline;int_method = "natural"}
  int_lst=list()
  for (i in 1:nrow(tab_dims)){
    # Get Segment -----------------------------------------------------------
    print(i)
    tab_dims_i=tab_dims[i,]
    tab_raw_i=dplyr::filter(tab_raw,Segment==tab_dims_i$Segment) %>% dplyr::arrange(.,Distance)
    if(nrow(tab_raw_i)>1){
    # 2.2.1 Interpolate -----------------------------------------------------
    int=data.frame(Elevation=numeric())
    int_df=int
    if(int_mathos=="linear") {
      for(j in 2:nrow(tab_raw_i)){
        tab_raw_j=tab_raw_i[(j-1):j,] 
        tab_dims_j=tab_raw_j %>%
          summarise(start=min(Distance,na.rm=T),
                    end=max(Distance,na.rm=T),
                    dist=end-start,
                    nods=dist/res)
        int_j=data.frame(Elevation=c(int_func(tab_raw_j$Elevation,
                                              n=tab_dims_j$nods,
                                              #yleft=tab_raw_j$Elevation[j], yright=tab_raw_j$Elevation[j-1],
                                              method = int_method,rule=2:1, f=0)[[2]]))
        int_j_df=int_j%>% 
          mutate(res=res,
                 Distance=(cumsum(res)-res)+tab_dims_j$start,
                 Horizon=tab_raw_j$Horizon[1],
                 Segment=i,
                 method = int_mathos) %>% 
          dplyr::select(-res)
        int_df=dplyr::distinct(rbind(int_df,int_j_df)) 
      }
    }
    if(int_mathos=="spline") {
      int=data.frame(Elevation=c(int_func(tab_raw_i$Elevation,n=tab_dims_i$nods, method = int_method)[[2]]))
      # 2.2.2 Calc Distance ---------------------------------------------------
      int_df=int%>% 
        mutate(res=res,
               Distance=(cumsum(res)-res)+tab_dims_i$start,
               Horizon=tab_raw_i$Horizon[1],
               Segment=i,
               method = int_mathos) %>% 
        dplyr::select(-res)
    }
    int_lst[[i]]=int_df  
    } else {
      message(paste("Segment number ",i, " disqualified - not enough points."))
    }
  }
  int_horizons_df=Reduce(rbind,int_lst)
  int_horizons_df$ID=as.character(tab_raw$ID[1])
  return(int_horizons_df)
}
# 3. Coordinate Horizons ####################################################
# Unit Test
if(Type_of_runing=="u_t"){
  fill_horizons_coord=coordinate_horizons(
    fill_horizons=fill_horizons_df,
    DEM_plot_df=read.csv(paste0(Background_path,'/Apps/External_Data/cs_pnts_dt.csv')),
    max_range=10
  )
}
# Func
coordinate_horizons=function(fill_horizons,DEM_plot_df,max_range){
  # 3.1 Get Coordinates From the Cross Section ------------------------------ 
  cs_pnts_dt=as.data.table(DEM_plot_df)[,c("dst","Longitude","Latitude")]
  colnames(cs_pnts_dt)=c("Distance","Longitude","Latitude")
  cs_pnts_dt$Range=cs_pnts_dt$Distance
  fill_horizons_ll=as.data.table(cs_pnts_dt)[as.data.table(fill_horizons),on="Distance", roll="nearest"] %>% 
    mutate(Range=round(abs(Range-Distance),0)) %>% 
    dplyr::filter(Range<=max_range) 
  return(fill_horizons_ll)
}

# 4. Combine Sources #######################################################
# Unit Test
if(Type_of_runing=="u_t"){
  fullhorizon=combine_sources(
    wells=charts$cs_data$wells_plot_df,
    manual_pnt=read.csv(paste0(Background_path,'/Apps/External_Data/tab_raw.csv')),
    intp_pnt=fill_horizons_coord
  )
}
# Func
combine_sources=function(wells,manual_pnt,intp_pnt){
  common_cols= c("Distance","Longitude","Latitude","Range","Elevation","Horizon","method","ID")
  manual_pnt$method="manual"
  
  wells_fltr=subset(wells,,c("dst","Longitude","Latitude","Elevation","f_name")) %>% 
    dplyr::rename(.,Distance=dst,Horizon=f_name) %>% 
    dplyr::filter(.,Horizon %in% manual_pnt$Horizon) %>% 
    mutate(Range=0,
           method="log",
           ID=manual_pnt$ID[1])
  comb_horizons=rbind(subset(manual_pnt,,common_cols),
                      subset(intp_pnt,,common_cols),
                      subset(wells_fltr,,common_cols)) %>% 
    distinct(., .keep_all = TRUE) %>% 
    dplyr::arrange(.,Horizon,Distance) 
  
  return(comb_horizons)
}

combine_m_sources=function(wells,manual_pnt){
  common_cols= c("Distance","Longitude","Latitude","Range","Elevation","Horizon","method","ID")
  manual_pnt$method="manual"
  
  wells_fltr=subset(wells,,c("dst","Longitude","Latitude","Elevation","f_name")) %>% 
    dplyr::rename(.,Distance=dst,Horizon=f_name) %>% 
    dplyr::filter(.,Horizon %in% manual_pnt$Horizon) %>% 
    mutate(Range=0,
           method="log",
           ID=manual_pnt$ID[1])
  comb_horizons=rbind(subset(manual_pnt,,common_cols),
                      subset(wells_fltr,,common_cols)) %>% 
    distinct(., .keep_all = TRUE) %>% 
    dplyr::arrange(.,Horizon,Distance) 
  
  return(comb_horizons)
}
# 5. Link Nodes ################################################################
# Unit Test
if(Type_of_runing=="u_t"){
  current_line=st_read(paste0(Background_path,'/Apps/External_Data/drawn_polyline.csv')) %>% st_set_crs(4326)
  lines_db=st_read(paste0(Background_path,'/Apps/External_Data/lines_db.shp')) %>% st_set_crs(4326)
  horizons_db=read.csv("G:/Geohydrology/Apps/External_Data/horizons_db.csv")
  nodes_links_df=nodes_linker(current_line,lines_db,horizons_db)
}
# Func
nodes_linker=function(current_line,lines_db,horizons_db){
  horizons_db_st=st_as_sf(horizons_db, coords = c("Longitude", "Latitude"), crs = 4326,remove=F) %>% 
    mutate(cs_id=str_sub(ID,1,1))
  # Intersection
  juc_df=horizons_db[0,]
  for (i in 1:nrow(lines_db)){
    print(i)
    # Get Intersection to CS
    lines_db_i=lines_db[i,]
    juc_pnt_i=st_intersection(current_line,lines_db_i)#  %>% st_cast("POINT") 
    # Get Horizon data
    horizons_db_st_i=dplyr::filter(horizons_db_st,cs_id == lines_db_i$cs_id)

    juc_i_horizons=horizons_db_st_i %>% 
      group_by(Horizon) %>% 
      group_nest() %>% # Experimental function dplyr 1.0.7
      mutate(closest_pnt=unlist({map(data,function(df) nngeo::st_nn(juc_pnt_i,df$geometry))})) %>% 
      unnest(cols = c(data)) %>% 
      group_by(Horizon) %>% 
      mutate(n=1,n=cumsum(n)) %>% 
      dplyr::filter(n==closest_pnt) %>% 
      dplyr::select(.,names(juc_df)) %>% na.omit(.)
    juc_df=bind_rows(juc_df,juc_i_horizons)
    }
  return(juc_df)
}


# 6. Clean temporals ###########################################################
cln_tmprl=function(){
fill_horizons_coord<<-NULL
horizons<<-NULL
tab_raw<<-NULL
tab<<-NULL  
}

# 7. Fix horizons ##############################################################
if(Type_of_runing=="u_t"){
  write.csv(tab, paste0(Background_path,'/Apps/External_Data/tab.csv'))  #  Local Test file
  write.csv(charts$cs_data$DEM_plot_df, paste0(Background_path,'/Apps/External_Data/DEM_plot_df.csv'))  #  Local Test file
  write.csv(horizons, paste0(Background_path,'/Apps/External_Data/horizons.csv'))  #  Local Test file
  write.csv(horizons_db, paste0(Background_path,'/Apps/External_Data/horizons_db.csv'))  #  Local Test file
  st_write(lines_db,paste0(Background_path,'/Apps/External_Data/lines_db.shp'))
  st_write(lines_db,paste0(Background_path,'/Apps/External_Data/current_line.shp'))
  fix_lst=fix_horizons(tab,
                       DEM_plot_df=charts$cs_data$DEM_plot_df,
                       cs_id=input$cs_id,
                       horizons,
                       horizons_db,
                       current_line,
                       lines_db,
                       tor="t")
}
# FUNC
fix_horizons=function(tab,DEM_plot_df,cs_id,horizons,horizons_db,current_line,lines_db,tor){
  #FIX
  tab_coord=coordinate_horizons(
    fill_horizons=tab,
    DEM_plot_df=DEM_plot_df,
    max_range=10
  )
  acth=dplyr::distinct(tab_coord,Horizon,Segment)
  
  rng4repair=tab_coord %>% group_by(Horizon,Segment) %>% 
    summarise(st=min(Distance,na.rm = T),
              ed=max(Distance,na.rm = T))
  
  ### Extract Proper Ranges ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  h2repair=subset(DEM_plot_df,
                  `Unit Name` %in% acth$Horizon,
                  c(Elevation,Distance,`Unit Name`,Longitude,Latitude)) %>% 
    dplyr::rename(Horizon=`Unit Name`)
  
  hproper=left_join(h2repair,rng4repair) %>% 
    mutate(erase=case_when(
      Distance %between% list(st, ed) ~ 1,
      TRUE  ~ 0
    )
    ) %>% group_by(Distance,Horizon) %>% 
    mutate(erase=case_when(
      max(erase,na.rm = T)>0 ~ T,
      TRUE  ~ F
    )
    )  %>% 
    dplyr::filter(erase==F) %>% 
    dplyr::select(-c(Segment,st,ed)) %>% 
    dplyr::distinct_(.dots = names(.))  %>% 
    mutate(ID=tab_coord$ID[1],
           method="Source",
           Segment=NA)
  
  ### Join FIX to Proper ranges ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  common_cols=c("Distance","Longitude","Latitude","Elevation","Horizon","method","ID")
  horizons=bind_rows(subset(hproper,,c(common_cols)),
                     subset(tab_coord,,c(common_cols))) %>% 
    dplyr::arrange(Horizon,Distance)
  
  ### Add 2 DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(is.null(horizons_db)==T){
    horizons_db=dplyr::distinct(horizons,Horizon,Distance,Elevation,ID,.keep_all = T)
  } else {
    horizons_db=rbind(horizons_db,horizons)
  }
  
  current_line$cs_id=cs_id
  if (cs_id!="A"){
    lines_db=bind_rows(current_line,lines_db)
  } else {
    lines_db=current_line
  }

  ### Export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Open Download option
  out_h_nme<<-"Fixed_CS-"
  shinyjs::show("download_horizon")
  
  if(tor=="t"){
    # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    write.csv(horizons_db, paste0(Background_path,'/Apps/External_Data/horizons_db.csv'))  #  Local Test file
    st_write(lines_db,paste0(Background_path,'/Apps/External_Data/lines_db.shp'),delete_dsn=TRUE)
    # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
  }
  # Export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fix_lst=list("horizons_db"=horizons_db,"lines_db"=lines_db)
  return(fix_lst)
  # Clean temporal ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cln_tmprl()  
}

# 8. Add horizons 2 DB  ########################################################
if(Type_of_runing=="u_t"){
  add_lst=add_horizons(tab,
                       DEM_plot_df=charts$cs_data$DEM_plot_df,
                       wells=charts$cs_data$wells_plot_df,
                       manual_pnt=tab_raw[ID==paste0(as.character(input$cs_id),as.character(input$cs_id),"'"),],
                       cs_id=input$cs_id,
                       horizons,
                       horizons_db,
                       current_line,
                       lines_db,
                       tor="t")
}
# FUNC
add_horizons=function(tab,DEM_plot_df,wells,manual_pnt,cs_id,horizons,horizons_db,current_line,lines_db,tor){
  ### Create ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tab_coord=coordinate_horizons(
    fill_horizons=tab,
    DEM_plot_df=DEM_plot_df,
    max_range=10
  )
  
  horizons=combine_sources(
    wells=wells,
    manual_pnt=manual_pnt,
    intp_pnt=tab_coord
  )
  ### Add 2 DB~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(is.null(horizons_db)==T){
    horizons_db=dplyr::distinct(horizons,Horizon,Distance,Elevation,ID,.keep_all = T)
  } else {
    horizons_db=rbind(horizons_db,horizons)
  }
  
  current_line$cs_id=cs_id
  if (cs_id!="A"){
    lines_db=bind_rows(current_line,lines_db)
  } else {
    lines_db=current_line
  }
  ### Export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Open Download option
  out_h_nme<<-"Horizons_CS-"
  shinyjs::show("download_horizon")
  
  if(tor=="t"){
    # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    write.csv(horizons_db, paste0(Background_path,'/Apps/External_Data/horizons_db.csv'))  #  Local Test file
    st_write(lines_db,paste0(Background_path,'/Apps/External_Data/lines_db.shp'),delete_dsn=TRUE)
    # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  }
  # Export ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  add_lst=list("horizons_db"=horizons_db,"lines_db"=lines_db)
  return(add_lst)
  # Clean temporal ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cln_tmprl()  
}




























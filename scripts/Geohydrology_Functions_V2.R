# founc. Geohydrology Functions #######################################################################
message("founc. Geohydrology Functions")
# founc.1 pattern recognizer ======================================================================
#message("founc.1 pattern recognizer")
pattern_recognizer=function(P_monthly,year,month,var_ex,Proj_int,Proj_end){
        
        # Mean production in wells over the Projection Period ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        pmp_proj_sum<<-P_monthly %>% dplyr::filter(.,as.numeric(year)>Proj_int & as.numeric(year)<=Proj_end) %>% group_by(shd_id,month) %>%
                summarise(Q_mean=mean(Q_MC,na.rm=T)) %>%
                ungroup(.)%>%mutate(shd_id=as.character(shd_id))
        
        # The monthly production pattern in wells ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        yearly_pattren_well<<-pmp_proj_sum  %>% group_by(shd_id) %>%
                summarise(Q_sum=sum(Q_mean,na.rm=T))
        
        yearly_pattren_well_r<<-pmp_proj_sum %>% left_join(.,yearly_pattren_well,by="shd_id") %>%
                mutate(relativ_pup=as.numeric(Q_mean)/as.numeric(Q_sum)) %>%
                mutate(relativ_pup=ifelse(is.nan(relativ_pup)==T,1/12,relativ_pup))
        
        
        # The general production pattern in the basin ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        yearly_pattren=pmp_proj_sum  %>% group_by(month) %>% summarise(Q_sum=sum(Q_mean,na.rm=T))
        yearly_pattren_sum=sum(yearly_pattren$Q_sum)
        yearly_pattren_r<<-yearly_pattren  %>% mutate(relativ_pup=as.numeric(Q_sum)/as.numeric(yearly_pattren_sum))%>%
                mutate(relativ_pup=ifelse(is.nan(relativ_pup)==T,1/12,relativ_pup))
        sum(yearly_pattren_r$relativ_pup) # Convergence test - goal value 1
}

# founc.2 Time series Builder =====================================================================
#message("founc.2 Time series Builder")

Time_series_builder=function(df,t_0,t_int,t_end,reg){
        
        time_series=df %>% mutate(time_step=t_0)
        for (i in t_int:t_end){
                time_series_i=df %>% mutate(time_step=i)
                time_series=bind_rows(time_series,time_series_i)
        }
        time_series<<-time_series
        return(time_series)
}

# founc.3 Trends decomposer =====================================================================
#message("founc.3 Trends decomposer")
trends_decomposer=function(df,var,variable,regression_period){
        
        # Define varibels ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        colnames(df)[which(colnames(df) == var)] <- 'flow'
        colnames(df)[which(colnames(df) == variable)] <- 'date'
        freq=12
        
        # Summraise the df ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        date_trend=df %>% mutate(year=year(date),
                                 month=month(date)) %>% group_by(year,month) %>%
                summarise(flow=mean(flow,na.rm=T))
        
        # Time Serise (ts) bulding ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        t_int_m=min(date_trend$month)
        t_int_y=min(date_trend$year)
        t_end_m=max(date_trend$month)
        t_end_y=max(date_trend$year)
        ts_trend=ts(date_trend$flow, start = c(t_int_y, t_int_m), end = c(t_end_y, t_end_m), frequency = freq)
        
        # Trends decompose ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        stlRes_trend<<- stl(ts_trend, s.window = "periodic")
        
        # Trends Check ``````````````````````````````````````````````````````````````````````````
        trend_decompose_plot= decompose(ts_trend, type="mult") # type="additive"
        plot (trend_decompose_plot)
        
        
        # Get trends Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        trend_decompose_df<<-stlRes_trend[["time.series"]] %>% data.frame(as.matrix(.), date=time(.)) %>%
                mutate(date=as.numeric(date),
                       gu=1,
                       abs_rand=abs(remainder),
                       norm_rand=as.vector(scale(remainder, center = T, scale = F)))
        
        
        # Random
        n=length(trend_decompose_df$remainder)
        random_patamters<<-trend_decompose_df %>% filter(date>(Proj_int-regression_period)) %>%
                summarise(rand_max=max(remainder,na.rm=T),
                          rand_mean=mean(remainder,na.rm=T),
                          rand_min=min(remainder,na.rm=T),
                          rand_sd=sd(remainder,na.rm=T),
                          rand_pop_sd = rand_sd*sqrt((n-1)/(n)),
                          rand_z = (n - rand_sd) / rand_pop_sd )
        
        # Trend
        trend_patamters<<-trend_decompose_df %>% filter(date>(Proj_int-regression_period)) %>% group_by(gu) %>%
                do(mod = lm(trend ~ date, data = .))    %>% #
                mutate(Slope = round(summary(mod)$coeff[2],2), #
                       Intercept=round(summary(mod)$coeff[1],2),
                       mean.resid =mean(resid(mod)^2), #
                       sum.resid = sum(resid(mod)^2), #
                       rsq = round(summary(mod)$r.squared,2)) #
}


# founc. 3.1 Trends decomposer advanced =========================================================
#message("founc.3.1 Trends decomposer advanced")

trends_decomposer_advanced=function(df,var,variable,regression_period,Proj_int,Randomness_type){
        
        # Define varibels ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        colnames(df)[which(colnames(df) == var)] <- 'flow'
        colnames(df)[which(colnames(df) == variable)] <- 'date'
        freq=12
        
        # Summraise the df ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        date_trend=df %>% mutate(year=year(date),
                                 month=month(date)) %>% group_by(year,month) %>%
                summarise(flow=mean(flow,na.rm=T))
        
        # Time Serise (ts) bulding ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        t_int_m=min(date_trend$month)
        t_int_y=min(date_trend$year)
        t_end_m=max(date_trend$month)
        t_end_y=max(date_trend$year)
        ts_trend=ts(date_trend$flow, start = c(t_int_y, t_int_m), end = c(t_end_y, t_end_m), frequency = freq)
        
        # Trends decompose ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        stlRes_trend<<- stl(ts_trend, s.window = "periodic")
        
        # Trends Check ``````````````````````````````````````````````````````````````````````````
        trend_decompose_plot= decompose(ts_trend, type="mult") # type="additive"
        plot (trend_decompose_plot)
        
        
        # Get trends Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        trend_decompose_df=stlRes_trend[["time.series"]] %>% data.frame(as.matrix(.), date=time(.)) %>%
                mutate(date=as.numeric(date),
                       gu=1)
        
        
        
        colnames(trend_decompose_df)[which(colnames(trend_decompose_df) == Randomness_type)] <- 'Rand'
        
        trend_decompose_df<<-trend_decompose_df %>%
                mutate( abs_rand=abs(Rand),
                        norm_rand=as.vector(scale(Rand, center = T, scale = F)))
        
        # Random
        n=length(trend_decompose_df$Rand)
        random_patamters<<-trend_decompose_df %>% filter(date>(Proj_int-regression_period)) %>%
                summarise(rand_max=max(Rand,na.rm=T),
                          rand_mean=mean(Rand,na.rm=T),
                          rand_min=min(Rand,na.rm=T),
                          rand_sd=sd(Rand,na.rm=T),
                          rand_pop_sd = rand_sd*sqrt((n-1)/(n)),
                          rand_z = (n - rand_sd) / rand_pop_sd )
        
        # Trend
        trend_patamters<<-trend_decompose_df %>% filter(date>(Proj_int-regression_period)) %>% group_by(gu) %>%
                do(mod = lm(Rand ~ date, data = .))    %>% #
                mutate(Slope = round(summary(mod)$coeff[2],2), #
                       Intercept=round(summary(mod)$coeff[1],2),
                       mean.resid =mean(resid(mod)^2), #
                       sum.resid = sum(resid(mod)^2), #
                       rsq = round(summary(mod)$r.squared,2)) #
}




# founc.4 Time series duplecator =============================================================

# Test elements
# df=iris
# index=iris$Species
# ts = seq(date_from,date_to,by="day") %>% as.data.frame() %>% dplyr::rename(.,"time"=".")
# Test elements

Time_series_duplecator=function(df,index,ts){
        reg_index_start=index[1]
        
        time_series=ts %>% mutate(reg_index=reg_index_start)
        reg_index=as.data.frame(unique(index))
        n=nrow(reg_index)
        
        for (i in 2:n){
                reg=reg_index[i,1]
                
                time_series_i=ts %>% mutate(reg_index=reg)
                time_series=bind_rows(time_series,time_series_i)
        }
        time_series<<-time_series
}

# founc.5 read excel allsheets =============================================================
#message("founc.5 read excel allsheets")

library(readxl)
read_excel_allsheets <- function(filename, tibble = FALSE) {
        # I prefer straight data.frames
        # but if you like tidyverse tibbles (the default with read_excel)
        # then just pass tibble = TRUE
        sheets <- readxl::excel_sheets(filename)
        x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
        if(!tibble) x <- lapply(x, as.data.frame)
        names(x) <- sheets
        x
}

# founc.6 Release Memory ===========================================================================
#message("founc.6 Release Memory")
# founc.6.1 improved list of objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=30) {
        napply <- function(names, fn) sapply(names, function(x)
                fn(get(x, pos = pos)))
        names <- ls(pos = pos, pattern = pattern)
        obj.class <- napply(names, function(x) as.character(class(x))[1])
        obj.mode <- napply(names, mode)
        obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
        obj.prettysize <- napply(names, function(x) {
                format(utils::object.size(x), units = "auto") })
        obj.size <- napply(names, object.size)
        obj.dim <- t(napply(names, function(x)
                as.numeric(dim(x))[1:2]))
        vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
        obj.dim[vec, 1] <- napply(names, length)[vec]
        out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
        names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
        if (!missing(order.by))
                out <- out[order(out[[order.by]], decreasing=decreasing), ]
        if (head)
                out <- head(out, n)
        out
}

# founc.6.2 shorthand ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lsos <- function(..., n=30) {
        .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

# founc.7 Biuld line of points along the CS ------######### Inactive ##########------------------------
#message("founc.7 Biuld line of points along the CS")

point_line = function(CS_model_system_unit) {
        
        # func-7.1 set const paramters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        message("func-7.1 set const paramters")
        
        X=CS_model_system_unit$X
        Y=CS_model_system_unit$Y
        n=length(X)
        vectort=data.frame()
        
        # func-7.2 set cumulative polyline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        message("func-7.2 set cumulative polyline")
        
        for(i in 1:(n-1)){
                
                # func-7.2.1 Pythagoras calculation for determining distance resolution ~~~~~~~~~~~~~~~
                
                pilength=((X[i+1]-X[i])^2+(Y[i+1]-Y[i])^2)^0.5  # Pythagoras distance
                rez=100 # resolution?
                nupoints=round(pilength/rez,0) # number of points
                xlength=abs(X[i+1]-X[i])/nupoints # Distance between the points on the X-axis
                
                # func-7.2.2 Construction of the  X vector ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                xvector=t(matrix(1:nupoints,1)) %>% as.data.frame() ;colnames(xvector) = c("X") # Construction
                
                # func-7.2.3 Populating of the  X vector ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                statrtX=X[i] # Initial value determining
                
                xvector=as.data.frame(xvector) %>% mutate(X=statrtX, # Populating
                                                          lengthU=xlength,
                                                          lengthc=cumsum(lengthU),
                                                          X=X+lengthc) %>%
                        subset(.,,c(X))
                
                
                # func-7.2.4 Construction of the  Y vector ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                # set linear model
                lmvector=as.data.frame(CS_model_system_unit[i:(i+1),])
                a=lm(Y ~ X, data = lmvector)$coefficients[[2]]
                b=lm(Y ~ X, data = lmvector)$coefficients[[1]]
                # Populating
                vector=as.data.frame(xvector) %>% mutate(Y=a*X+b)
                
                # func-7.2.5 Set cumulative Vector ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                vectort=na.omit(rbind.data.frame(vectort,vector))
                vectort=do.call(data.frame,lapply(vectort, function(x) replace(x, is.infinite(x),NA)))
                vectort=na.omit(vectort)
                
        }
        
        # func-7.3 convertion to polyline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        message("func-7.3 convertion to polyline")
        
        xy = as.data.frame(subset(vectort,,c(X,Y)))
        vectort_shp = SpatialPointsDataFrame(coords = xy, data = vectort,proj4string = CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs"))
        return(vectort_shp)
        
        
        #  coordinates(vectort)=~X+Y
        # proj4string(vectort)=CRS("++proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +units=m +no_defs") # set it to UTM
        #  vectort<<-vectort # Intput for geo_transform function
}

# founc.8 Create DEM lines ------------------------------------------------------------------------
#message("founc.8 Create DEM lines")
DEM_polyline = function(CS_points_sdf,DEM){
        # Get The Elevations Along the CS
        elevations = raster::extract(DEM,CS_points_sdf,cellnumbers=TRUE,sp=TRUE,along=T) 
        elevations_df = as.data.table(elevations)
        # Get The Distance Along the CS
        dst_df = data.table(dst=pointDistance(CS_points_sdf, lonlat=T) %>% .[,1])
        DEM_CS_dst=cbind(elevations_df,dst_df) %>% dplyr::arrange(dst)
        return(DEM_CS_dst)
}

# founc.9 Calcolate Transforms location --------------------------------------------------------------
#message("founc.9 Calcolate Transforms location")
geo_transform <- function(vectort,transforms_df,buffer_width,trs_pth){
        
        # func-9.1 Create line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        polyline=as.data.frame(vectort)                             # 1.1 Get point data
        
        coordinates(polyline)=~X_WGS84+Y_WGS84                                 # 1.2 Convert dataframe to layer of points
        
        proj4string(polyline)= CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs") #CRS("++proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +units=m +no_defs") # set it to UTM
        
        spLines(polyline)                                           # 1.3 Convert points to lines
        
        polyline_1 <- buffer(polyline, width=buffer_width)          # 1.4 Convert polyline to polygon
        
        
        # func-9.2 Transforms layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        #transforms_df <- arc.open(transforms) %>% arc.select(.,fields="ObjectID")          # 2.1 get tranform layer and convert to df
        
        n=nrow(transforms_df)                                                              # 2.2 get tranforms indexs
        transforms_matrix <- matrix(0,n,3)
        
        Tpath=paste0(trs_pth,"/Transforms/TJ/TJ-FAULTS.shp") # # 2.3 get full tranform layer and convert to df
        
        transforms_full <- readOGR(dsn=Tpath, layer='TJ-FAULTS')
        
        
        # func-9.3 set transform location ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        for (i in 1:n){
                # func-9.3.1 select transform ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                transforms_sub=transforms_full %>% subset(.,ObjectID==transforms_df$ObjectID[i])
                
                transforms_poly <- buffer(transforms_sub, width=5000)
                
                # func-9.3.2 Intersect with the CS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                transforms_intersect= raster::intersect(polyline_1,transforms_poly)
                
                if (is.null(structure(transforms_intersect))==TRUE){
                        print("do not intresected")
                }
                # func-9.3.3 Position on the CS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if (is.null(structure(transforms_intersect))==FALSE){
                        Starting_point=as.data.frame(vectort) %>% subset(.,,c("X_WGS84","Y_WGS84")) %>% .[1,] %>% as.data.frame(.)
                        
                        transforms_dst=transforms_intersect@bbox%>%
                                rowMeans(.)%>%
                                t(.)
                        
                        transforms_dst_1=as.data.frame(transforms_dst)
                        colnames(transforms_dst_1)=c("X_WGS84","X_WGS84")
                        
                        pilength= transforms_dst_1 %>% mutate(dx=X-Starting_point$X,
                                                              dy=Y-Starting_point$Y,
                                                              dst=(dx^2+dy^2)^0.5)
                        transforms_matrix[i,1]=pilength$dst
                        transforms_matrix[i,2]=pilength$X
                        transforms_matrix[i,3]=pilength$Y
                }
        }
        
        return(transforms_matrix)
        
}

# founc.10 Calcolate Projection line -----------------------------------------------------------------
#message("founc.10 Calcolate Projection line")
geo_Projection <- function(Projection_sp,Projection_df,CS_model_system_unit_dst,sdist){
        
        # ffounc.10.1  Sampling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        lgth <- SpatialLinesLengths(Projection_df)
        lsub <- Projection_sp[1,]
        ns <- round( (lgth[1] / sdist), digits=0)
        lsamp <- spsample(lsub, n=ns, type="regular", offset=c(0.5,0.5))
        
        results <- as.data.frame(SpatialPointsDataFrame(lsamp,data.frame(id=1:length(lsamp))))
        
        colnames(results)=c("ID","X","Y")
        
        # founc.10.2 Set Distanse matrix to the progecrion line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Projection_dst = pointDistance(subset(CS_model_system_unit_dst,,c("X","Y")), subset(results,,c("X","Y")), lonlat=FALSE)
        Projection_dst_min=apply(Projection_dst,1,min)
        
        # founc.10.3 Get min dist for each well ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Projection_dst_1<-cbind(subset(CS_model_system_unit_dst,,c("well_id","X","Y","dst","bot")),Projection_dst_min)
        
        
        # founc.10.4 Set Projection Orientation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Projection_dir = pointDistance(subset(results,,c("X","Y")), subset(CS_model_system_unit_dst,,c("X","Y")), lonlat=FALSE)
        Projection_dir_min=apply(Projection_dir,1,min)
        Projection_dir_min_1<-cbind(results,Projection_dir_min)
        colnames(Projection_dir_min_1)=c("ID","Xproj","Yproj","Projection_dst_min")
        Projection_dst_2<<-left_join(Projection_dst_1,Projection_dir_min_1,by="Projection_dst_min") %>%
                mutate(deltaX=Xproj-X,
                       deltaY=Yproj-Y,
                       Orientation=ifelse(deltaX>0 & deltaY>0,"SW",
                                          ifelse(deltaX<0 & deltaY<0,"NE",
                                                 ifelse(deltaX<0 & deltaY>0,"SE",
                                                        ifelse(deltaX>0 & deltaY<0,"NW",
                                                               ifelse(deltaX/abs(deltaY)>0.99,"W",
                                                                      ifelse(deltaX/abs(deltaY)<(-0.99),"E",
                                                                             ifelse(deltaY/abs(deltaX)>0.99,"N",
                                                                                    ifelse(deltaY/abs(deltaX)>(-0.99),"N","")))))))))
        
        
}

# founc.11 Extract Legend ---------------------------------------------------------------------------
#message("founc.11 Extract Legend")
g_legend <- function(a.gplot){
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)}

# founc.12 moving avrage ----------------------------------------------------------------------------
#message("founc.12 moving avrage")
ma <- function(arr,n){
        res = as.vector(arr)
        for(i in n:(length(arr)-n)){
                res[i] = mean(arr[(i-n):(i+n)])
        }
        as.data.frame(res)
}

# founc.13 Colour Standart --------------------------------------------------------------------------
#message("founc.13 Colour Standart")
ColourExpreation=function(){
        c("Not Classified" ="#F5F5F5",
          "DED SEA-KURKAR-HULA"="#C7EBFA",
          "SAQIYE-KEFAR GELA'DI"="#F3CC00",
          "AVEDAT"="#FFD27B",
          "MOUNT SCOPUS"="#C2B600",
          "JUDEA UPPER"="#55BB48",
          "JUDEA AQUICLOD"="#FFCC71",
          "JUDEA LOWER"="#82B000",
          "JUDEA GROUP UNDIV"="#55BB48",
          "KURNUB"="#BDD7CC",
          "ARAD"="#7ECCBC",
          "RAMON"="#CFA6CF",
          "NEGEV"="#D18717",
          "YAM SUF"="#FF8000",
          "TASHTIT"="#646464",
          "Not Clear / Loss"="#FFFFFF",
          "Not Classified"="#FFFFFF",
          "SAHAF-ALLUVIUM"="#F5F5F5",
          "Zemah"="#F5F5F5",
          "Notera"="#CACACC",
          "Ded Sea Riff Valley"="#828282",
          "Sabkha"="#787878",
          "Dune"="#828282",
          "DED SEA-KURKAR-HULA"="#C7EBFA",
          "Lisan"="#FDF8D7",
          "Samara"="#66A384",
          "Mazar"="#F6B0B0",
          "Arava Cgl"="#F6A6A6",
          "Amora"="#F3CEA0",
          "Ein Yahav Volc"="#F69292",
          "Sedom"="#8DFFAB",
          "Zeit  (RAS MALA'AB Gr)"="#F67E7E",
          "S.Gharib (RAS MALA'AB Gr)"="#F68888",
          "Belayim (RAS MALA'AB Gr)"="#F67E7E",
          "Kareem (RAS MALA'AB Gr)"="#F68888",
          "Rudis (Ghar Andal Gr)"="#F67E7E",
          "Nukhal (Ghar Andal Gr)"="#F68888",
          "Heimur"="#F67E7E",
          "Raham Cgl"="#F68888",
          "Hazeva"="#FFF300",
          "Alluviom Dand Duns, Feefs Sabkhae, Lake Deposits"="#F68888",
          "Pleshet"="#E1D7EB",
          "Ahuzam"="#E9E3DC",
          "Golan Volcanics"="#FF5F17",
          "Ortal Volcanics"="#F3645C",
          "Ubediye"="#C7EBFA",
          "Hazor"="#E9E1D7",
          "Ruman Volcanics"="#E64097",
          "SAQIYE-KEFAR GELA'DI"="#F3CC00",
          "Yafo"="#F2FF94",
          "Noa"="#F3CC00",
          "Rishon Lezion Volcs."="#A500A5",
          "Afridar Clastics"="#F3CC00",
          "Afiq"="#F3CC00",
          "Bit Nir"="#E9D766",
          "Sheva"="#0E3BA0",
          "shiqma"="#F3CC00",
          "Mavqim"="#F3CC00",
          "Ziqim"="#F3CC00",
          "Ziqlag"="#FFFF00",
          "National Park Volcs."="#A500A5",
          "Nahal'oz Clast"="#878077",
          "Lakish"="#A2FCC4",
          "Bit Guvrin"="#AB9F50",
          "Ashdod Clastics"="#F3CC00",
          "Cover Basalt"="#EE3124",
          "Gesher"="#E9D9AB",
          "Bira"="#F5F3CC",
          "Faggas Tuff"="#F01464",
          "Um Sabuna Cgl"="#E61E00",
          "Zemah"="#F5F5F5",
          "Intran Basalt"="#D23200",
          "Hordos"="#FFF300",
          "Lower Basalt"="#4187B5",
          "E'n Gev"="#E61E00",
          "Susita"="#DC28A0",
          "Fiq"="#D23214",
          "Suez Volc"="#C83C1E",
          "Abu Zamina"="#BE4628",
          "Alluviom Dand Duns, Feefs Sabkhae, Lake Deposits"="#B45032",
          "Qeziot"="#AA5A3C",
          "Taka"="#A06446",
          "AVEDAT"="#FFD27B",
          "Metrad"="#F576A0",
          "Horsha"="#FFE38D",
          "Nahal Yater"="#E1761E",
          "Nizzana"="#FFD478",
          "Mor"="#FFE364",
          "Maresha"="#FFD250",
          "Adulam"="#FFE364",
          "Bar Kokhaba"="#FAA450",
          "Timrat"="#FFD478",
          "MOUNT SCOPUS"="#C2B600",
          "Taqiye"="#F3CC00",
          "Ghareb"="#EEEB66",
          "Hatrurim"="#D7E3D5",
          "Mishash"="#D9551F",
          "Menuha"="#C2B600",
          "Sayyarim"="#8A7940",
          "Zihor"="#6BC200",
          "E'in Zetim"="#C2B600",
          "Bat Shelomo Tuff"="#80835E",
          "JUDEA UPPER"="#55BB48",
          "Bina"="#80FF00",
          "Dalyya"="#D1FFEB",
          "Weradim"="#CCFF8F",
          "Kfar Shaul"="#A3FF82",
          "Aminadav"="#A4FF83",
          "Nezer"="#80FF00",
          "Gerofit (Wata)"="#80FF00",
          "Shivta"="#C2D931",
          "Derorim"="#5F9F00",
          "Ora Shale (Abu Qada)"="#CCAE6C",
          "Tamar"="#CCFF8F",
          "Avnon"="#B7EB00",
          "Yotvata"="#7B797B",
          "Zafit"="#A4FF83",
          "Yirka"="#FFC7A9",
          "Sakhnin"="#CCFF8F",
          "Sumaq"="#B7EB00",
          "Muhraka"="#CCFF8F",
          "Yanuh"="#CFDE9A",
          "Zikharon"="#EB0670",
          "Arqan"="#7BC832",
          "JUDEA AQUICLOD"="#DCAA",
          "Moza"="#FFCC71",
          "Bet Meir"="#AB9F50",
          "Dir Hanna"="#CDE300",
          "Um el Fahm Tuff"="#FA6464",
          "E'in Yorqe'am"="#FFCC71",
          "JUDEA LOWER"="#82B000",
          "Kesalon"="#FFC7A9",
          "Soreq"="#CFDE9A",
          "Givat Ye'arim"="#91B000",
          "Kefira"="#B0B678",
          "Qatana"="#BDD7CC",
          "E'in Qinya"="#BDD7CC",
          "Tamon"="#C89664",
          "Yagor"="#91B000",
          "Yakhini"="#82B000",
          "Talme Yafe"="#BDD7CC",
          "Zalmon"="#82B000",
          "Item"="#C8C882",
          "Ziv"="#C8C8C8",
          "Havyon"="#91B000",
          "Hazera"="#AAAAAA",
          "Hatera"="#A0A0A0",
          "Kamon"="#91B000",
          "Hidra&Rama"="#BDD7CC",
          "JUDEA GROUP UNDIV"="#55BB48",
          "Jundeia Chalk"="#98E66C",
          "Shefeya Tuff"="#FF8D7A",
          "Khuriebe Chalk"="#84D280",
          "Um A Tuss Tuff"="#7AC876",
          "Bet Oren LST"="#84BE80",
          "Furadis Tuff"="#7AB476",
          "I'sfiye Chalk"="#84AA80",
          "Igzim Tuff"="#7AA076",
          "Negba"="#55BB48",
          "Charmel Chalk Complex"="#7A8C76",
          "Charmel Tuff Complex"="#848280",
          "KURNUB"="#BDD7CC",
          "Uza - Upper"="#BDD7CC",
          "Uza - Lower"="#BDD7CC",
          "Malhata - Upper"="#BDD7CC",
          "Malhata - Medial"="#BDD7CC",
          "Malhata - Lower"="#CD8E33",
          "Ramon Basalt"="#E64097",
          "Dargot"="#BDD7CC",
          "Zeweira - Upper"="#BDD7CC",
          "LC-3"="#A57A5B",
          "Zeweira - Lower"="#9B8465",
          "Hatira"="#CAD7A6",
          "Yakhini"="#82B000",
          "Qatana"="#BDD7CC",
          "E'in Qinya"="#BDD7CC",
          "Tamun"="#A1D6B5",
          "Ein El Assad"="#08D800",
          "Telamim"="#BDD7CC",
          "Banas/Tiasser Basalt"="#BDD7CC",
          "Helez"="#BDD7CC",
          "Tayasir Volcanics"="#A500A5",
          "Ramim"="#BDD7CC",
          "Gevar A'm"="#BDD7CC",
          "ARAD"="#3C7DC4",
          "Yam"="#C7EBFA",
          "Delta"="#C7EBFA",
          "Haluza"="#A0C4DA",
          "Haifa Bay"="#C7EBFA",
          "Beer Sheva"="#0E3BA0",
          "Kidod"="#7ACCC7",
          "Haifa Upper"="#6E877E",
          "Deborah Volcanics"="#A500A5",
          "Nir A'm"="#C7EBFA",
          "Zohar"="#7ECCBC",
          "Hermon Upper"="#7ECCBC",
          "Sherif"="#C7EBFA",
          "Karmon"="#C7EBFA",
          "Safa"="#787D88",
          "Hermon Middel"="#82877E",
          "Daya"="#C7EBFA",
          "Bir Maghara"="#82877E",
          "Mahmal"="#787D88",
          "Shederot"="#C73232",
          "Inmar Upper"="#9CC2BB",
          "Rosh Pina"="#C7EBFA",
          "Barnea"="#C7EBFA",
          "Haifa"="#C7EBFA",
          "Qeren"="#C7EBFA",
          "Asher Volcanics"="#A500A5",
          "Hermon Lower"="#787D88",
          "Inmar Lower"="#9CC2BB",
          "Nirim Upper"="#787D88",
          "Ardon"="#71CCD2",
          "Dayka"="#C7EBFA",
          "Mish'hor"="#71CCD2",
          "RAMON"="#FF0BF0",
          "Shefayim"="#C7EBFA",
          "Mohilla"="#CFA6CF",
          "Saharonim"="#FACCD2",
          "Erez Conglomerate"="#C7EBFA",
          "Gevanim"="#C7EBFA",
          "Raa'f"="#C7EBFA",
          "Budra - RAMON"="#B94679",
          "NEGEV"="#FF0400",
          "Zafir"="#C7EBFA",
          "Yamin"="#C7EBFA",
          "Budra - NEGEV"="#E11E5E",
          "Shezaf"="#C7EBFA",
          "Karmia Shale"="#C7EBFA",
          "Arqov"="#A4FF83",
          "Saa'd"="#C7EBFA",
          "Zenifim"="#C7EBFA",
          "Abo Thora"="#A55A86",
          "Um Bogma"="#9B647C",
          "Gevim OP"="#916E86",
          "YAM SUF"="#FF8000",
          "Netafim"="#F576A0",
          "Shehoret"="#C73232",
          "Timna'"="#E0E0AF",
          "A'mudei Shelomo"="#9C7E5F",
          "TASHTIT"="#141414",
          "Unnamea Cgl"="#1E1E1E",
          "Basement Complex"="#FACCD1",
          "Doroth Schist"="#4A94FF",
          "Not Classified"="#FFFFFF",
          "Not Clear / Loss"="#FFFFFF",
          "Clay" ="#323232",
          "Clay-Silt" ="#C4AF76",
          "Clay-Sand" ="#F2E85D",
          "Clay-Sandston" ="#F6F096",
          "Clay-Pebble/Conglomerate" ="#96E3F6",
          "Clay-Marl" ="#E8D9B0",
          "Clay-Chalk" ="#E6E8B0",
          "Clay-Limeston" ="#AEE8B8",
          "Clay-Dolomite" ="#5CA24C",
          "Clay-Basalt" ="#C320C3",
          "Clay-Cliff" ="#54899F",
          "Clay-Shales" ="#E43A20",
          "Clay-Flint/Chert" ="#E90C10",
          "Clay-Bitumen" ="#243131",
          "Clay-Evaporites" ="#B82CAB9",
          "Silt" ="#C4AF76",
          "Silt-Clay" ="#928D47",
          "Silt-Sand" ="#F4EC79",
          "Silt-Sandston" ="#C6E9C6",
          "Silt-Pebble/Conglomerate" ="#BFDED3",
          "Silt-Marl" ="#E7E0B0",
          "Silt-Chalk" ="#CAE8B4",
          "Silt-Limeston" ="#85C582",
          "Silt-Dolomite" ="#8F6187",
          "Silt-Basalt" ="#8B54B1",
          "Silt-Cliff" ="#9C615F",
          "Silt-Shales" ="#796570",
          "Silt-Flint/Chert" ="#196079",
          "Silt-Bitumen" ="#243131",
          "Silt-Evaporites" ="#7517E75",
          "Sand" ="#F2E85D",
          "Sand-Clay" ="#949164",
          "Sand-Silt" ="#ADC9B6",
          "Sand-Sandston" ="#EFE4A3",
          "Sand-Pebble/Conglomerate" ="#BEE5D3",
          "Sand-Marl" ="#CBE0B4",
          "Sand-Chalk" ="#A1C57E",
          "Sand-Limeston" ="#B884BD",
          "Sand-Dolomite" ="#589575",
          "Sand-Basalt" ="#D32D71",
          "Sand-Cliff" ="#318CB0",
          "Sand-Shales" ="#843528",
          "Sand-Flint/Chert" ="#E90C10",
          "Sand-Bitumen" ="#2B3131",
          "Sand-Evaporites" ="#BE1BC97",
          "Sandston" ="#F6F096",
          "Sandston-Clay" ="#648A94",
          "Sandston-Silt" ="#D6C493",
          "Sandston-Sand" ="#ECE886",
          "Sandston-Pebble/Conglomerate" ="#A2E5D7",
          "Sandston-Marl" ="#A2BD7E",
          "Sandston-Chalk" ="#D484B9",
          "Sandston-Limeston" ="#81B8AB",
          "Sandston-Dolomite" ="#A06E36",
          "Sandston-Basalt" ="#6858C2",
          "Sandston-Cliff" ="#3C5D68",
          "Sandston-Shales" ="#E43A20",
          "Sandston-Flint/Chert" ="#206179",
          "Sandston-Bitumen" ="#747053",
          "Sandston-Evaporites" ="#D51D98B",
          "Pebble/Conglomerate" ="#96E3F6",
          "Pebble/Conglomerate-Clay" ="#8D8571",
          "Pebble/Conglomerate-Silt" ="#D5CB93",
          "Pebble/Conglomerate-Sand" ="#D0E88A",
          "Pebble/Conglomerate-Sandston" ="#A9C971",
          "Pebble/Conglomerate-Marl" ="#D57CB9",
          "Pebble/Conglomerate-Chalk" ="#9DB8A7",
          "Pebble/Conglomerate-Limeston" ="#C9916C",
          "Pebble/Conglomerate-Dolomite" ="#359986",
          "Pebble/Conglomerate-Basalt" ="#73287A",
          "Pebble/Conglomerate-Cliff" ="#54899F",
          "Pebble/Conglomerate-Shales" ="#8B3629",
          "Pebble/Conglomerate-Flint/Chert" ="#699F9B",
          "Pebble/Conglomerate-Bitumen" ="#8B8C47",
          "Pebble/Conglomerate-Evaporites" ="#D71DDA7",
          "Marl" ="#E8D9B0",
          "Marl-Clay" ="#8C8D71",
          "Marl-Silt" ="#B9CB97",
          "Marl-Sand" ="#A7C554",
          "Marl-Sandston" ="#DC88AC",
          "Marl-Pebble/Conglomerate" ="#75B6CA",
          "Marl-Chalk" ="#E59168",
          "Marl-Limeston" ="#5EBCBC",
          "Marl-Dolomite" ="#40693E",
          "Marl-Basalt" ="#C320C3",
          "Marl-Cliff" ="#435D68",
          "Marl-Shales" ="#D4744B",
          "Marl-Flint/Chert" ="#80BC8F",
          "Marl-Bitumen" ="#8D9063",
          "Marl-Evaporites" ="#A71D6D7",
          "Chalk" ="#E6E8B0",
          "Chalk-Clay" ="#708D75",
          "Chalk-Silt" ="#90A861",
          "Chalk-Sand" ="#DA8490",
          "Chalk-Sandston" ="#A5BC9A",
          "Chalk-Pebble/Conglomerate" ="#BD8E8B",
          "Chalk-Marl" ="#7BB4B8",
          "Chalk-Limeston" ="#698C74",
          "Chalk-Dolomite" ="#5CA24C",
          "Chalk-Basalt" ="#7A297A",
          "Chalk-Cliff" ="#8C9C8A",
          "Chalk-Shales" ="#EB913E",
          "Chalk-Flint/Chert" ="#82C0AB",
          "Chalk-Bitumen" ="#5D8A93",
          "Chalk-Evaporites" ="#D01D1B4",
          "Limeston" ="#AEE8B8",
          "Limeston-Clay" ="#476A3F",
          "Limeston-Silt" ="#C3679C",
          "Limeston-Sand" ="#A3B87E",
          "Limeston-Sandston" ="#ED955B",
          "Limeston-Pebble/Conglomerate" ="#52B9DB",
          "Limeston-Marl" ="#868570",
          "Limeston-Chalk" ="#E6E8B0",
          "Limeston-Dolomite" ="#476A3F",
          "Limeston-Basalt" ="#C3679C",
          "Limeston-Cliff" ="#A3B87E",
          "Limeston-Shales" ="#ED955B",
          "Limeston-Flint/Chert" ="#52B9DB",
          "Limeston-Bitumen" ="#868570",
          "Limeston-Evaporites" ="#CF1D9B4",
          "Dolomite" ="#5CA24C",
          "Dolomite-Clay" ="#7A297A",
          "Dolomite-Silt" ="#8C9C8A",
          "Dolomite-Sand" ="#EB913E",
          "Dolomite-Sandston" ="#82C0AB",
          "Dolomite-Pebble/Conglomerate" ="#5D8A93",
          "Dolomite-Marl" ="#E8D9B0",
          "Dolomite-Chalk" ="#8C8D71",
          "Dolomite-Limeston" ="#B9CB97",
          "Dolomite-Basalt" ="#DA8490",
          "Dolomite-Cliff" ="#A5BC9A",
          "Dolomite-Shales" ="#BD8E8B",
          "Dolomite-Flint/Chert" ="#7BB4B8",
          "Dolomite-Bitumen" ="#858C70",
          "Dolomite-Evaporites" ="#B31D9B8",
          "Basalt" ="#C320C3",
          "Basalt-Clay (Litearit)" ="#C320C3",
          "Basalt-Silt" ="#D4744B",
          "Basalt-Sand (Gromosoil)" ="#C320C3",
          "Basalt-Sandston" ="#8D9063",
          "Basalt-Pebble/Conglomerate  (Scoria)" ="#C320C3",
          "Basalt-Marl" ="#8D8571",
          "Basalt-Chalk" ="#D5CB93",
          "Basalt-Limeston" ="#D0E88A",
          "Basalt-Dolomite" ="#A9C971",
          "Basalt-Cliff" ="#75B6CA",
          "Basalt-Shales" ="#E68968",
          "Basalt-Flint/Chert" ="#7ABCB8",
          "Basalt-Bitumen" ="#698C74",
          "Basalt-Evaporites" ="#8A1B682",
          "Cliff" ="#54899F",
          "Cliff-Clay" ="#8B3629",
          "Cliff-Silt" ="#699F9B",
          "Cliff-Sand" ="#8B8C47",
          "Cliff-Sandston" ="#F6F096",
          "Cliff-Pebble/Conglomerate" ="#648A94",
          "Cliff-Marl" ="#D6C493",
          "Cliff-Chalk" ="#ECE886",
          "Cliff-Limeston" ="#D2ECA7",
          "Cliff-Dolomite" ="#79C2A1",
          "Cliff-Basalt" ="#D57CB9",
          "Cliff-Shales" ="#E59168",
          "Cliff-Flint/Chert" ="#5EBCBC",
          "Cliff-Bitumen" ="#40693E",
          "Cliff-Evaporites" ="#BD175BE",
          "Shales" ="#E43A20",
          "Shales-Clay" ="#206179",
          "Shales-Silt" ="#747053",
          "Shales-Sand" ="#F2E85D",
          "Shales-Sandston" ="#949164",
          "Shales-Pebble/Conglomerate" ="#ADC9B6",
          "Shales-Marl" ="#EDE086",
          "Shales-Chalk" ="#EEECA3",
          "Shales-Limeston" ="#A2E5D7",
          "Shales-Dolomite" ="#A2BD7E",
          "Shales-Basalt" ="#D484B9",
          "Shales-Cliff" ="#81B8AB",
          "Shales-Flint/Chert" ="#359986",
          "Shales-Bitumen" ="#73287A",
          "Shales-Evaporites" ="#861A9AC",
          "Flint/Chert" ="#E90C10",
          "Flint/Chert-Clay" ="#2B3131",
          "Flint/Chert-Silt" ="#C4AF76",
          "Flint/Chert-Sand" ="#928D47",
          "Flint/Chert-Sandston" ="#DDCF86",
          "Flint/Chert-Pebble/Conglomerate" ="#C4E5A9",
          "Flint/Chert-Marl" ="#EFE4A3",
          "Flint/Chert-Chalk" ="#BEE5D3",
          "Flint/Chert-Limeston" ="#CBE0B4",
          "Flint/Chert-Dolomite" ="#A1C57E",
          "Flint/Chert-Basalt" ="#B884BD",
          "Flint/Chert-Cliff" ="#589575",
          "Flint/Chert-Shales" ="#D32D71",
          "Flint/Chert-Bitumen" ="#3C5D68",
          "Flint/Chert-Evaporites" ="#CE1826C",
          "Bitumen" ="#243131",
          "Bitumen-Clay" ="#323232",
          "Bitumen-Silt" ="#C4AF76",
          "Bitumen-Sand" ="#F2E85D",
          "Bitumen-Sandston" ="#F6F096",
          "Bitumen-Pebble/Conglomerate" ="#96E3F6",
          "Bitumen-Marl" ="#E8D9B0",
          "Bitumen-Chalk" ="#E6E8B0",
          "Bitumen-Limeston" ="#AEE8B8",
          "Bitumen-Dolomite" ="#5CA24C",
          "Bitumen-Basalt" ="#C320C3",
          "Bitumen-Cliff" ="#54899F",
          "Bitumen-Shales" ="#E43A20",
          "Bitumen-Flint/Chert" ="#E90C10",
          "Bitumen-Evaporites" ="#B82CAB9",
          "Evaporites" ="#B82CAB9",
          "Evaporites-Clay" ="#323232",
          "Evaporites-Silt" ="#C4AF76",
          "Evaporites-Sand" ="#F2E85D",
          "Evaporites-Sandston" ="#F6F096",
          "Evaporites-Pebble/Conglomerate" ="#96E3F6",
          "Evaporites-Marl" ="#E8D9B0",
          "Evaporites-Chalk" ="#E6E8B0",
          "Evaporites-Limeston" ="#AEE8B8",
          "Evaporites-Dolomite" ="#5CA24C",
          "Evaporites-Basalt" ="#C320C3",
          "Evaporites-Cliff" ="#54899F",
          "Evaporites-Shales" ="#E43A20",
          "Evaporites-Flint/Chert" ="#E90C10",
          "Riolit" ="#DC2828",
          "Riolit-Granite" ="#DCDC28",
          "Riolit-Porphyry" ="#FFF",
          "Riolit-Gabbro" ="#1414BE",
          "Riolit-Diorite" ="#289696",
          "Riolit-Gneiss" ="#C828C8",
          "Riolit-Schist" ="#9683BB",
          "Riolit-Amphibolite" ="#A78228",
          "Granite" ="#DCDC28",
          "Granite-Riolit" ="#DC2828",
          "Granite-Porphyry" ="#FFF",
          "Granite-Gabbro" ="#1414BE",
          "Granite-Diorite" ="#289696",
          "Granite-Gneiss" ="#C828C8",
          "Granite-Schist" ="#9683BB",
          "Granite-Amphibolite" ="#A78228",
          "Porphyry" ="#FFF",
          "Porphyry-Riolit" ="#DC2828",
          "Porphyry-Granite" ="#DCDC28",
          "Porphyry-Gabbro" ="#1414BE",
          "Porphyry-Diorite" ="#289696",
          "Porphyry-Gneiss" ="#C828C8",
          "Porphyry-Schist" ="#9683BB",
          "Porphyry-Amphibolite" ="#A78228",
          "Gabbro" ="#1414BE",
          "Gabbro-Riolit" ="#DC2828",
          "Gabbro-Granite" ="#DCDC28",
          "Gabbro-Porphyry" ="#FFF",
          "Gabbro-Diorite" ="#289696",
          "Gabbro-Gneiss" ="#C828C8",
          "Gabbro-Schist" ="#9683BB",
          "Gabbro-Amphibolite" ="#A78228",
          "Diorite" ="#289696",
          "Diorite-Riolit" ="#DC2828",
          "Diorite-Granite" ="#DCDC28",
          "Diorite-Porphyry" ="#FFF",
          "Diorite-Gabbro" ="#1414BE",
          "Diorite-Gneiss" ="#C828C8",
          "Diorite-Schist" ="#9683BB",
          "Diorite-Amphibolite" ="#A78228",
          "Gneiss" ="#C828C8",
          "Gneiss-Riolit" ="#DC2828",
          "Gneiss-Porphyry" ="#FFF",
          "Gneiss-Gabbro" ="#1414BE",
          "Gneiss-Diorite" ="#289696",
          "Gneiss-Gneiss" ="#C828C8",
          "Gneiss-Schist" ="#9683BB",
          "Gneiss-Amphibolite" ="#A78228",
          "Schist" ="#9683BB",
          "Schist-Riolit" ="#DC2828",
          "Schist-Granite" ="#DCDC28",
          "Schist-Porphyry" ="#FFF",
          "Schist-Gabbro" ="#1414BE",
          "Schist-Diorite" ="#289696",
          "Schist-Gneiss" ="#C828C8",
          "Schist-Amphibolite" ="#A78228",
          "Amphibolite" ="#A78228",
          "Amphibolite-Riolit" ="#DC2828",
          "Amphibolite-Granite" ="#DCDC28",
          "Amphibolite-Porphyry" ="#FFF",
          "Amphibolite-Gabbro" ="#1414BE",
          "Amphibolite-Diorite" ="#289696",
          "Amphibolite-Gneiss" ="#C828C8",
          "Amphibolite-Schist" ="#9683BB",
          "Not Clear / Loss" ="#E6E6E6",
          "Not Classified" ="#C8C8C8",
          "Quaternary" ="#787878",
          "Quaternary-Holocene" ="#A0A1A2",
          "Quaternary-Pleistocene" ="#B4B4B4",
          "Quaternary-Pleistocene-Late" ="#B4B4B4",
          "Quaternary-Pleistocene-Middle" ="#B4B4B4",
          "Quaternary-Pleistocene-Calabrian" ="#B4B4B4",
          "Quaternary-Pleistocene-Gelasian" ="#B4B4B4",
          "Neogene" ="#F6CECE",
          "Neogene-Pliocene" ="#F6CECE",
          "Neogene-Pliocene-Piacenzian" ="#F6CECE",
          "Neogene-Pliocene-Zanclean" ="#F6CECE",
          "Neogene-Miocene" ="#F6CE78",
          "Neogene-Miocene-Messinian" ="#F6CE79",
          "Neogene-Miocene-Tortonian" ="#F6CE7A",
          "Neogene-Miocene-Serravallian" ="#F6CE7B",
          "Neogene-Miocene-Langhian" ="#F6CE7C",
          "Neogene-Miocene-Burdigalian" ="#F6CE7D",
          "Neogene-Miocene-Aquitanian" ="#F6CE7E",
          "Paleogene" ="#F5C981",
          "Paleogene-Oligocene" ="#F6CA82",
          "Paleogene-Oligocene-Chattian" ="#F7CB83",
          "Paleogene-Oligocene-Rupelian" ="#F8CC84",
          "Paleogene-Eocene" ="#F5DA81",
          "Paleogene-Eocene-Priabonian" ="#F5DA81",
          "Paleogene-Eocene-Bartonian" ="#F5DA81",
          "Paleogene-Eocene-Lutetian" ="#F5DA81",
          "Paleogene-Eocene-Ypresian" ="#F5DA81",
          "Paleogene-Paleocene" ="#FF8000",
          "Paleogene-Paleocene-Thanetian" ="#FF8010",
          "Paleogene-Paleocene-Selandian" ="#FF8020",
          "Paleogene-Paleocene-Danian" ="#FF8030",
          "Cretaceous" ="#C9B86A",
          "Cretaceous-Late-Senonian" ="#C9B86A",
          "Cretaceous-Late-Senonian-Maastrichtian" ="#C9B86A",
          "Cretaceous-Late-Senonian-Campanian" ="#C9B86A",
          "Cretaceous-Late-Senonian-Santonian" ="#C9B86A",
          "Cretaceous-Late-Senonian-Coniacian" ="#C9B86A",
          "Cretaceous-Late-Turonian" ="#3ADF00",
          "Cretaceous-Late-Turonian - upper" ="#3AC800",
          "Cretaceous-Late-Turonian - lower" ="#14C800",
          "Cretaceous-Late-Cenomanian" ="#14C800",
          "Cretaceous-Early" ="#B43130",
          "Cretaceous-Early-Albian" ="#B43140",
          "Cretaceous-Early-Aptian" ="#86B440",
          "Cretaceous-Early-Barremian" ="#86B450",
          "Cretaceous-Early-Hauterivian" ="#86B460",
          "Cretaceous-Early-Valanginian" ="#86B470",
          "Cretaceous-Early-Berriasian" ="#86B480",
          "Jurassic" ="#ACFA58",
          "Jurassic-Late" ="#ACFA59",
          "Jurassic-Late-Tithonian" ="#ACFA5A",
          "Jurassic-Late-Kimmeridgian" ="#ACFA5B",
          "Jurassic-Late-Oxfordian" ="#ACFA5C",
          "Jurassic-Middle" ="#ACFA5D",
          "Jurassic-Middle-Callovian" ="#ACFA5E",
          "Jurassic-Middle-Bathonian" ="#ACFA5F",
          "Jurassic-Middle-Bajocian" ="#ACFA60",
          "Jurassic-Middle-Aalenian" ="#ACFA61",
          "Jurassic-Early" ="#ACFA62",
          "Jurassic-Early-Toarcian" ="#ACFA63",
          "Jurassic-Early-Pliensbachian" ="#ACFA64",
          "Jurassic-Early-Sinemurian" ="#ACFA65",
          "Jurassic-Early-Hettangian" ="#ACFA66",
          "Triassic" ="#DF7410",
          "Triassic-Late" ="#DF7420",
          "Triassic-Late-Rhaetian" ="#DF7430",
          "Triassic-Late-Norian" ="#DF7440",
          "Triassic-Late-Carnian" ="#DF7450",
          "Triassic-Middle" ="#DF7460",
          "Triassic-Middle-Ladinian" ="#DF7470",
          "Triassic-Middle-Anisian" ="#DF7480",
          "Triassic-Early" ="#DF7490",
          "Triassic-Early-Olenekian" ="#DF74A0",
          "Triassic-Early-Induan" ="#DF74B0",
          "Permian" ="#1DFD70",
          "Permian-Lopingian" ="#2DFD70",
          "Permian-Lopingian-Changhsingian" ="#3DFD70",
          "Permian-Lopingian-Wuchiapingian" ="#4DFD70",
          "Permian-Guadalupian" ="#5DFD70",
          "Permian-Guadalupian-Capitanian" ="#6DFD70",
          "Permian-Guadalupian-Wordian" ="#7DFD70",
          "Permian-Guadalupian-Roadian" ="#8DFD70",
          "Permian-Cisuralian" ="#9DFD70",
          "Permian-Cisuralian-Kungurian" ="#ADFD70",
          "Permian-Cisuralian-Artinskian" ="#BDFD70",
          "Permian-Cisuralian-Sakmarian" ="#CDFD70",
          "Permian-Cisuralian-Asselian" ="#DDFD70",
          "Carbon" ="#FF0BF0",
          "Carbon-Pennsylvanian" ="#1001C0",
          "Carbon-Pennsylvanian-Gzhelian" ="#1012C1",
          "Carbon-Pennsylvanian-Kasimovian" ="#1023C2",
          "Carbon-Pennsylvanian-Moscovian" ="#1034C3",
          "Carbon-Pennsylvanian-Bashkirian" ="#1045C4",
          "Carbon-Mississippian" ="#1056C5",
          "Carbon-Mississippian-Serpukhovian" ="#1067C6",
          "Carbon-Mississippian-Visean" ="#1078C7",
          "Carbon-Mississippian-Tournaisian" ="#1089C8",
          "Devonian" ="#FF0400",
          "Devonian-Late" ="#100040",
          "Devonian-Late-Famennian" ="#101040",
          "Devonian-Late-Frasnian" ="#102040",
          "Devonian-Middle" ="#103040",
          "Devonian-Middle-Givetian" ="#104040",
          "Devonian-Middle-Eifelian" ="#105040",
          "Devonian-Early" ="#106040",
          "Devonian-Early-Emsian" ="#107040",
          "Devonian-Early-Pragian" ="#108040",
          "Devonian-Early-Lochkovian" ="#109040",
          "Silurian" ="#FF8000",
          "Silurian-Pridoli" ="#FF8010",
          "Silurian-Ludlow" ="#FF8020",
          "Silurian-Ludlow-Ludfordian" ="#FF8030",
          "Silurian-Ludlow-Gorstian" ="#FF8040",
          "Silurian-Wenlock" ="#FF8050",
          "Silurian-Wenlock-Homerian" ="#FF8060",
          "Silurian-Wenlock-Sheinwoodian" ="#FF8070",
          "Silurian-Llandovery" ="#FF8080",
          "Silurian-Llandovery-Telychian" ="#FF8090",
          "Silurian-Llandovery-Aeronian" ="#FF80A0",
          "Silurian-Llandovery-Rhuddanian" ="#FF80B0",
          "Ordovician" ="#7CDA81",
          "Ordovician-Late" ="#7DDA81",
          "Ordovician-Late-Hirnantian" ="#7EDA81",
          "Ordovician-Late-Katian" ="#7FDA81",
          "Ordovician-Late-Sandbian" ="#80DA81",
          "Ordovician-Middle" ="#81DA81",
          "Ordovician-Middle-Darriwilian" ="#82DA81",
          "Ordovician-Middle-Dapingian" ="#83DA81",
          "Ordovician-Early" ="#84DA81",
          "Ordovician-Early-Floian" ="#85DA81",
          "Ordovician-Early-(formerly Arenig)" ="#86DA81",
          "Ordovician-Early-Tremadocian" ="#87DA81",
          "Cambrian" ="#FF8043",
          "Cambrian-Furongian" ="#FF8044",
          "Cambrian-Furongian-Stage 10" ="#FF8045",
          "Cambrian-Furongian-Jiangshanian" ="#FF8046",
          "Cambrian-Furongian-Paibian" ="#FF8047",
          "Cambrian-Series 3" ="#FF8048",
          "Cambrian-Series 3-Guzhangian" ="#FF8049",
          "Cambrian-Series 3-Drumian" ="#FF804A",
          "Cambrian-Series 3-Stage 5" ="#FF804B",
          "Cambrian-Series 2" ="#FF804C",
          "Cambrian-Series 2-Stage 4" ="#FF804D",
          "Cambrian-Series 2-Stage 3" ="#FF804E",
          "Cambrian-Terreneuvian" ="#FF804F",
          "Cambrian-Terreneuvian-Stage 2" ="#FF8050",
          "Cambrian-Terreneuvian-Fortunian" ="#FF8051",
          "Precambrian" ="#646464",
          "Precambrian-Proteozoic" ="#646464",
          "Precambrian-Archean" ="#646464",
          "Precambrian-Hadean" ="#646464",
          "Not Clear / Loss" ="#FFFFFF",
          "Not Classified" ="#FFFFFF")
}

ColourExpreation_DEM=function(){
        
        c(DTM= "#000000",
          Kurkar_Base_A = "#F6CE78",
          Kurkar_Base_B = "#C9B86A",
          Kurkar_Base_C = "#FF8000",
          Kurkar_top_B = "#C9B86A",
          Kurkar_top_C = "#FF8000",
          Kurkar_top_D = "#B43130",
          Saqia_Base = "#B43130",
          Bina_Top = "#3DE00A",
          Taqia_Top = "#B43130",
          Arad_Top = "#3C7DC4",
          Arava_Judea_Top = "#3C7DC4",
          Arava_Judea_Base = "#299C05",
          Dargot_Top = "#B97A47",
          LC3_Top = "#A57A5B",
          Malhata_Top = "#D79829",
          Uza_Top = "#F5B6B0",
          Zeweria_L_Top = "#AF8451",
          Zeweria_U_Top = "#9B8465",
          Moza_Top = "#FFCC71",
          Moza_local1_Top = "#FF0400",
          Moza_local2_Top = "#FF0400",
          Moza_local3_Top = "#FF0400",
          Yagur_Top = "#0B630B",
          Yakeni_Top = "#063806",
          Judea_top = "#3DE00A",
          Saltwater_Aquifer_Top = "#E11E5E",
          Arkan_Zichron_Top = "#9B8465",
          Avedat_Top = "#F6CE78",
          Bina_Top = "#3DE00A",
          Kurnub_Top = "#F5B6B0",
          Mt_Scopus_Top = "#FF8000",
          Pleistocene_Top = "#C9B86A",
          Saqia_Top = "#B43130",
          Yagur_Top = "#3DE00A",
          Basalt_Base = "#E00AA8",
          Beer_Sheva_HalouzaP1_Base = "#E11E5E",
          Beer_Sheva_HalouzaP1_Top = "#E11E5E",
          Beer_Sheva_HalouzaP2_Base = "#E11E5E",
          Beer_Sheva_HalouzaP2_Top = "#E11E5E",
          Egl_Base = "#F5F312",
          Egl_Top = "#F5F312",
          HermonP1_Base = "#3C7DC4",
          HermonP1_Top = "#3C7DC4",
          HermonP2_Base = "#3C7DC4",
          HermonP2_Top = "#3C7DC4",
          HermonP3_Top = "#3C7DC4",
          IntL2_Base = "#F6CE78",
          IntL2_Top = "#F6CE78",
          JudeaP1_Base = "#3DE00A",
          JudeaP1_Top = "#3DE00A",
          JudeaP2_Base = "#3DE00A",
          JudeaP2_Top = "#3DE00A",
          JudeaP3_Base = "#3DE00A",
          JudeaP3_Top = "#3DE00A",
          Kidud_Base = "#F528C0",
          Kidud_Top = "#F528C0",
          Miloi2AradL1_Base = "#D79829",
          Miloi2AradL1_Top = "#D79829",
          Kurnub_Top = "#F5B6B0",
          Miloi_Base = "#F6CE78",
          Sabha_Yutvata_Base = "#E11E5E",
          Weradim_Top = "#3DE00A",
          Aminadav_Top="#A4FF83",
          GivatYearim_Top="#91B000",
          Judea_Base="#BDD7CC",
          Kesalon_Top="#FFC7A9",
          KfarShaul_Top="#A3FF82",
          Moza_Top="#FFCC71",
          Soreq_Top="#CFDE9A",
          KfarShaul_Top = "#A3FF82",
          Aminadav_Top = "#A4FF83",
          Moza_Top = "#FFCC71",
          BetMeir_Top = "#AB9F50",
          Kesalon_Top = "#FFC7A9",
          Soreq_Top = "#CFDE9A",
          GivatYearim_Top = "#91B000",
          Kefira_Top = "#B0B678",
          Qatana_Top = "#BDD7CC",
          Weradim_Top = "#3DE00A",
          Aminadav_Top = "#A4FF83",
          Judea_Base = "#BDD7CC",
          Top_Jura_Top = "#3C7DC4",
          Lower_Cretaceous_Top = "#DE9221",
          Aptian_Top = "#BDD7CC",
          Lower_Cenomanian_Top = "#82B000",
          Middle_Cenomanian_Top = "#FFCC71",
          Upper_Cenomanian_Top = "#CCFF8F",
          Bina_Top = "#80FF00",
          MontScopus_Top = "#C2B600",
          Avdat_Top = "#FFD27B",
          Neogen_Top = "#F3CC00",
          Quaternary_Base = "#F6CE78",
          Upper_Judah_Top = "#80FF00",
          Lower_Jodea_Top = "#CFDE9A",
          Kurnub_Top = "#F5B6B0",
          Kurnub_Base = "#3C7DC4",
          alluvium_Top = "#C4B99F",
          eocene_Top = "#F6CE78",
          senon_Top = "#C2B600",
          judeaUP_Top = "#55BB48",
          judeaMID_Top = "#FFCC71",
          judeaLO_Top = "#82B000",
          Kurnob_Top = "#BDD7CC",
          Model_Base = "#BDD7CC",
          user_geology_layer = "#FF0000"#,
          )
        
}

# founc.14 Test input Elemnts ########################################################################
#message("founc.14 Test input Elemnts")

input_element_test=function(input_type,test_type,DB,validation_value,iv,message){
        
        fn=test_type
        fn_class=str_sub(deparse(substitute(fn)),13,-3)
        test_worked=FALSE
        
        # founc - 14.1 Select and run Test ===========================================================
        # Select DB type -----------------------------------------------------------------------------
        if (input_type=="vector"){
                # Select Test Type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if(str_detect(fn_class,"is")==TRUE){
                        print("is test")
                        DB_class=lapply(DB,class)[[1]]
                        v=fn(DB_class)==validation_value
                        test_worked=TRUE}
                if(str_detect(fn_class,"length")==TRUE){
                        print("length test")
                        fn_1=fn[[1]]
                        ctira=fn[[2]]
                        DB_length=fn_1(DB[,1])
                        if(ctira==">"){v=DB_length>validation_value}
                        if(ctira=="<"){v=DB_length<validation_value}
                        test_worked=TRUE}
                if(str_detect(fn_class,"max")==TRUE || str_detect(fn_class,"min")==TRUE){
                        print("max/min test")
                        DB_edge=fn(DB[,1],na.rm=T)
                        if(str_detect(fn_class,"max")==TRUE){v=DB_edge>validation_value} else {v=DB_edge>validation_value}
                        test_worked=TRUE}
                if(any(str_detect(deparse(test_type),"between"))==TRUE){
                        print("between test")
                        DB_between=all(dplyr::between(DB[,1],validation_value[1],validation_value[2]))
                        v=DB_between==TRUE
                        test_worked=TRUE}
                
        }
        
        if (input_type=="data.frame"){
                # Select Test Type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if(str_detect(fn_class,"is")==TRUE){
                        print("is test")
                        DB_class=unlist(lapply(DB, fn))
                        v=(all(DB_class)==validation_value)
                        test_worked=TRUE}
                if(str_detect(fn_class,"dim")==TRUE){
                        print("dim test")
                        DB_dim=fn(DB)
                        if(DB_dim[1]<validation_value[1] || DB_dim[2]<validation_value[2]){v=FALSE} else {v=TRUE}
                        test_worked=TRUE}
                if(str_detect(fn_class,"max")==TRUE || str_detect(fn_class,"min")==TRUE){
                        print("max/min test")
                        DB_edge=unlist(lapply(DB, fn))
                        if(str_detect(fn_class,"max")==TRUE){v=(any(DB_edge)>validation_value)} else {v=(all(DB_edge)>validation_value)}
                        test_worked=TRUE}
                if(any(str_detect(deparse(test_type),"between"))==TRUE){
                        print("between test")
                        DB_between=unlist(lapply(DB, function(x) dplyr::between(x,validation_value[1],validation_value[2])))
                        v=(all(DB_between)==TRUE)
                        test_worked=TRUE}
                
        }
        if (input_type=="Shape-Point" || input_type=="Shape-Line" || input_type=="Shape-Polygon"){
                # Select Test Type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                if(str_detect(test_type,"Size")==TRUE){
                        print("Size test")
                        DB_extent=DB@extent ; DB_size=((DB_extent[3]-DB_extent[1])*(DB_extent[4]-DB_extent[2]))/10^6
                        valid_extent=validation_value[[1]]@extent ; valid_size=((valid_extent[3]-valid_extent[1])*(valid_extent[4]-valid_extent[2]))/10^6
                        size_diff=DB_size/valid_size
                        if(size_diff<validation_value[[2]] || size_diff>validation_value[[3]]) {v=FALSE} else {v=TRUE}
                        test_worked=TRUE}
                if(str_detect(test_type,"Distance")==TRUE){
                        print("Distance test")
                        DB_extent=DB@extent ; DB_cent=c(mean(DB_extent[3],DB_extent[1]),mean(DB_extent[4],DB_extent[2]))
                        valid_extent=validation_value[[1]]@extent ; valid_cent=c(mean(valid_extent[3],valid_extent[1]),mean(valid_extent[4],valid_extent[2]))
                        distance_diff_X=abs(DB_cent[1]-valid_cent[1])
                        distance_diff_Y=abs(DB_cent[2]-valid_cent[2])
                        if(distance_diff_X>validation_value[[2]] || distance_diff_Y>validation_value[[2]]) {v=FALSE} else {v=TRUE}
                        test_worked=TRUE}
                if(str_detect(test_type,"Projection")==TRUE){
                        print("Projection test")
                        DB_projection=DB@shapeinfo$WKT
                        v=str_detect(DB@shapeinfo$WKT,"Israel_TM_Grid")
                        test_worked=TRUE}
        }
        
        if (test_worked==TRUE){
                # founc - 14.2 Export the results to the Log table ===========================================
                validation_temp=data.frame(matrix(ncol = 6, nrow = 1)) %>% dplyr::rename(.,id=X1,input_type=X2,element=X3,test_type=X4,status=X5,message=X6)
                
                validation_temp$id[1]=iv
                validation_temp$input_type[1]=input_type
                validation_temp$element[1]=deparse(substitute(DB))
                #if (class(fn)=="function") {validation_temp$test_type[1]=as.character(str_sub(deparse(substitute(fn)),13,-3))} else (validation_temp$test_type[1]= as.character(test_type))
                if (v==TRUE) {validation_temp$status[1]= "Normal"} else (validation_temp$status[1]= "Abnormal")
                if (v==TRUE) {validation_temp$message[1]= "Go on"} else (validation_temp$message[1]= message)
                
                if (iv==1){validation_df<<-validation_temp}
                if (iv>1){validation_df<<-dplyr::bind_rows(validation_df,validation_temp)}
                iv<<-iv+1
                
                # founc - 14.3 Export the results as a message ==============================================
                if (v==FALSE & str_detect(message,"erorr")==TRUE){stop(message)}
                else if (v==FALSE & str_detect(message,"warning")==TRUE){print(message)}
                else {print("go on")}
        }
        if (test_worked==FALSE){print("test failed")}
}


# founc.15 Meta Dada of lists ################################################################################
#message("founc.15 Meta Dada of lists")

list_meta_data=function(list,list_contents,elements){
        #15.1 Biuld Meta Data Frame ==========================================================================
        ii=length(list)
        jj=length(elements)
        meta_data_frame=data.frame(matrix(nrow = ii, ncol = jj))
        colnames(meta_data_frame)=elements
        #15.2 Get elements ===================================================================================
        if(str_detect(list_contents,"rasters")==TRUE){
                for (j in 1:jj){
                        # 15.2.1 Extract List Data to the Meta Dataframe -------------------------------------
                        
                        if(elements[j]!="min" & elements[j]!="max"){
                                ld=lapply(1:ii, function(i)get(elements[j])(list[[i]]))%>%as.data.frame(.)%>%t(.)
                                meta_data_frame[,j]=ld[,1]
                        }
                        if(elements[j]=="min"){
                                ld=lapply(1:ii, function(i) list[[i]]@data@min)%>%as.data.frame(.)%>%t(.)
                                meta_data_frame[,j]=ld[,1]
                        }
                        if(elements[j]=="max"){
                                ld=lapply(1:ii, function(i) list[[i]]@data@max)%>%as.data.frame(.)%>%t(.)
                                meta_data_frame[,j]=ld[,1]
                        }
                }
        }
        meta_data_frame
}

# founc.16 Run Time Monitor #################################################################################
#message("founc.16 Run Time Monitor")
run_time_monitor=function(Time_df,Cunck,View_plot){
        message(Cunck)
        
        time_v=as.numeric(Sys.time()-relative_time)
        cumulative_v=as.numeric(Sys.time()-cumulative_time)
        
        # 16.1 Biuld runtime dataframe ======================================================================
        time_df[it,]<<-c(NO=it,Cunck,time_v,cumulative_v,ViewPlot=View_plot)
        it<<-it+1
        relative_time<<-Sys.time()
        
        # 16.2 Biuld runtime plot ===========================================================================
        if (View_plot!=FALSE){
                
                # 16.2.1 Build Text data frame ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                chunck_text_df=time_df%>%dplyr::filter(.,ViewPlot!=FALSE)
                
                # 16.2.2 Build  draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                time_run_C<<-ggplot() +
                        ggtitle("Runtime monitoring")+
                        geom_ribbon(data=time_df,aes(x=as.numeric(cumulative),ymin=0, ymax=as.numeric(NO)),fill="blue",alpha=.4)+
                        geom_text(data = chunck_text_df,
                                  aes(x = as.numeric(cumulative),y=as.numeric(NO),label = Chunck),angle = 45, colour ="black",size =3,alpha=0.5)+
                        labs(x="Time [Sec]",y="Cunck - NO")+
                        theme(plot.title = element_text(color="red", size=14, face="bold.italic"))
                print(time_run_C)
                
        }
        gc()
}


# founc.17 Lat Long to UTM ##############################################################################
#message("founc.17 Lat Long to UTM")

LongLatToUTM<-function(x,y){
        xy <- data.frame(ID = 1:length(x), X = x, Y = y)
        coordinates(xy) <- c("X", "Y")
        proj4string(xy) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +no_defs ")
        isreal_TM_Grid_m="++proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +units=m +no_defs"
        res <- spTransform(xy, isreal_TM_Grid_m)
        return(as.data.frame(res))
}


# founc.18 data frame to shp by OGR #####################################################################
#message("founc.18 data frame to shp by OGR")
# Unit test
# df2shp_test=read.csv("G:/Layers/Geohydrology/Geohydrology/code/Geohydrology_Rproj/test_verbs/df2shp.csv")
# temp_path=paste0(Background_path,"/Geohydrology/code/Geohydrology_Rproj/test_verbs/temp_verbs")
# tictoc::tic()
# df2shp_sp=df2shp(df2shp_test,temp_path,"df2shp_sp.shp","sp")
# tictoc::toc()
# Unit test

df2shp=function(df,temp_path,shp_name,type_file){
        xy = as.data.frame(subset(df,,c(X,Y)))
        isreal_TM_Grid="++proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +units=m +no_defs"
        
        shp_file = SpatialPointsDataFrame(coords = xy, data = df,proj4string = CRS(isreal_TM_Grid))
        
        do.call(file.remove, list(list.files(temp_path, full.names = TRUE)))
        
        writeOGR(shp_file, dsn=temp_path,
                 layer=shp_name,driver="ESRI Shapefile")
        
        if (type_file=="sp"){(return(shp_file))}
        if (type_file=="st"){(return(st_read(temp_path)))}
        
}

# founc.19 Quantitative forcast analysis ###############################################################
#message("founc.19 Quantitative forcast analysis")

# x=foracst_peacks
# y=Hydrograph_peak
# join_value="datetime"
# diff_unit="day"
forcast_analyzer=function(x,y,join_value,diff_unit){
        # Set varibels
        colnames(x)[which(colnames(x) == join_value)] <- 'join_value'
        colnames(y)[which(colnames(y) == join_value)] <- 'join_value'
        xx = x %>% mutate(join_value_x=join_value) %>% setDT(.)
        yy = y %>% mutate(join_value_y=join_value) %>% setDT(.)
        # Set Resolution
        if(diff_unit=="hour"){du=60^2}
        if(diff_unit=="day"){du=24*60^2}
        if(diff_unit=="week"){du=7*24*60^2}
        # Analyze results
        xy=xx[yy, on = .(join_value), roll = "nearest"] %>%
                mutate(hit_miss=(as.double(join_value_x)-as.double(join_value_y))/du,
                       hit_miss_cat=ifelse(abs(hit_miss)<1,1,0))
        return(xy)
}

# founc.20 folder to dataframe #########################################################################
#message("founc.20 Quantitative forcast analysis")

# db_path=Model_peaks_paths$run_fold
# col_typ='-Tcccc--' # column: c = character, i = integer, n = number, d = double, l = logical, f = factor, D = date, T = date time, t = time, ? = guess, or _/- to skip the column.
# col_nms=c("datetime","flowDB_ID","long_flowDB","lat_flowDB","Q")
# db_idx=Model_peaks_paths$join_date
# db_typ="peack" # "gap"

db2df=function(db_path,col_typ,col_nms,db_idx,db_typ){
        for(i in NROW(db_path):1){
                print(i)
                path_i=db_path[i]
                df_i=read_delim(path_i,delim=",",col_types = col_typ,col_names=col_nms) %>%
                        mutate_at(vars(-datetime),funs(as.numeric(.)))%>%setDT(.)
                # Cutting overlapping series
                df_i$run_date=db_idx[i]
                if(db_typ=="peack"){df_i$run_id=i}else{df_i$run_id=-999}
                if(i<NROW(db_path)){df_i=df_i %>% dplyr::filter(.,datetime<start_i)}
                # Bind Serises
                if(i==NROW(db_path)){date_forcast=df_i}else{date_forcast=dplyr::bind_rows(date_forcast,df_i)}
                # Get runnig time range
                start_i=min(date_forcast$datetime)
        }
        return(date_forcast)
}

# founc.21 Performance Analysis ########################################################################
#message("founc.21 Performance Analysis")

pb_test<-function(){
        pb$tick()$print()
        print(pb$i)
        print(pb$last_update-pb$init_time)
}

# founc.22 filtering data farame in nested DB #########################################################
#message("founc.23 filtering data farame in nested DB")

# Models_ts_st= Models_ts %>%
#         mutate(date_forcast=purrr::map(date_forcast,element=Model_IDX,
#                                        filter_field="flowDB_ID",operator="==",filter_nest))

filter_nest=function(nest_col,element,filter_field,operator){
        
        # Set Variabels
        fi=as.character(filter_field)
        df=as.data.frame(nest_col)%>% dplyr::rename(.,"fd"=fi)
        op=as.character(operator)
        # Filtering
        if(op=="<"){filter_df<-df %>% dplyr::filter(.,fd<element)}
        if(op=="<="){filter_df<-df %>% dplyr::filter(.,fd<=element)}
        if(op==">"){filter_df<-df %>% dplyr::filter(.,fd>element)}
        if(op==">="){filter_df<-df %>% dplyr::filter(.,fd>=element)}
        if(op=="=="){filter_df<-df %>% dplyr::filter(.,fd==element)}
        if(op=="!="){filter_df<-df %>% dplyr::filter(.,fd!=element)}
        colnames(filter_df)[which(colnames(filter_df) == "fd")] <- fi
        return(as.data.frame(filter_df))
}

# founc.23 Inherit column from main nested DB to DF column  ###########################################
#message("founc.23 Inherit column from main nested DB to DF column")

#Unit Test
# poly_nest_DB_Davide_1=poly_nest_DB_Davide %>%
#         mutate(data=purrr::map(.x=data,y=-999,field_name="Soil_moisture",.f=inherited_column))
#Unit Test

inherited_column=function(x,y,field_name){
        
        fieldEx=colnames(x)[which(colnames(x) == field_name)]
        if(length(fieldEx)>0){Ex=fieldEx==field_name}else{Ex=F}
        if(Ex==T){x=dplyr::select(x,-fieldEx)}
        x_IDX= x %>% mutate(.,new_col=y)
        colnames(x_IDX)[which(colnames(x_IDX) == "new_col")] <- field_name
        return(x_IDX)
}

# founc.24 Rename column of nested DB #################################################################
#message("founc.24 Rename column of nested DB")

refield=function(nest_col,new_name,old_name){
        
        fnew=as.character(new_name)
        fold=as.character(old_name)
        nest_col=as.data.frame(nest_col)%>% dplyr::rename(.,"fd"=fold)
        colnames(nest_col)[which(colnames(nest_col) == "fd")] <- fnew
        return(nest_col)
        
}


# founc.25 Read all excel Files from Path ############################################################
#message("founc.25 Read all excel Files from Path")

# Unit Test
#PB_Update=read_excel_files(folderPath="Q:/Projects/Open/Active/Pan_Biny/RAW/PB_Profile_RAW_update_2019",sheetName="Data")
# ts_DB=read_excel_files_v2(folderPath="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/FloodsOnline_DB",
#                        sheetName="Sheet 1",
#                        range_time=c(ed=as.Date("2020-06-01"),st=as.Date("2019-10-01")-1),
#                        pattern="Measure")
# Unit Test

read_excel_files_v2=function(folderPath,sheetName,pattern,range_time){
        # Filter by Pattren - optional ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(missing(pattern)==F){ptr=pattern}else(ptr = NULL)
        # Build Meta Data file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        folder_df=list.files(path=folderPath,pattern=ptr) %>% as.data.frame() %>% dplyr::rename(.,"filename"=".") %>%
                mutate(filepath=list.files(path=folderPath,full.names = T,pattern=ptr),
                       ctime=file.info(filepath)$ctime,
                       size=file.info(filepath)$size)
        # Filter by time of files - optional ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(missing(range_time)==F){
                #folder_df=folder_df %>% dplyr::filter(.,ctime>=as_datetime(range_time))
                folder_df=as.data.table(folder_df)[(as.Date(ctime)>=range_time[2] & as.Date(ctime)<=range_time[1]),]
        }
        
        # Build files Colector Function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        read_excel_core=function(filepath,sheetN){
                readxl::read_excel(filepath, sheet = sheetN)
        }
        # Get Files -------------------------------------------------------------------------------------
        folder_db=folder_df %>% mutate(.,df=purrr::map2(filepath,sheetName,.f=read_excel_core))
        
        return(folder_db)
}


# founc.26 Extract column from main nested DB to DF column  ###########################################
#message("founc.26 Extract column from main nested DB to DF column")

#Unit Test
# Models_ts_st_1=Models_ts_st %>%
#         mutate(Q_cosmo_value=purrr::map(.x=date_forcast,field_name="Q_cosmo_value",.f=extract_column))
#Unit Test

extract_column=function(x,field_name){
        
        fieldEx=colnames(x)[which(colnames(x) == field_name)]
        if(length(fieldEx)>0){Ex=fieldEx==field_name}else{Ex=F}
        if(Ex==T){x=dplyr::select(x,fieldEx)}
        colnames(x)[which(colnames(x) == "new_col")] <- field_name
        return(x)
}

# founc.27 Save data frame to multisheet excel file ###################################################
#message("founc.27 Save data frame to multisheet excel file")

# Unit test
# df_test=read_excel("G:/Layers/Geohydrology/Geohydrology/code/Representative_Wells/df2wb_test_.xlsx")
# df2wb(df=df_test,
#       col2sheet="cell_n",
#       wb_path=paste0("G:/Layers/Geohydrology/Geohydrology/code/Representative_Wells/wb","_.xlsx"))
# Unit test

df2wb=function(df,col2sheet,wb_path){
        require("openxlsx")
        # Split to list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        colnames(df)[which(colnames(df) == col2sheet)] <- 'c2s'
        dgf=df %>% group_by(c2s)
        lst=group_split(dgf)
        keys_df=group_keys(dgf)
        # Build excel file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        wb = createWorkbook()
        for (i in 1:NROW(keys_df)){
                dta=as.data.frame(lst[i])
                s_name=keys_df$c2s[i]
                addWorksheet(wb,sheetName=s_name)
                writeData(wb, sheet=i,x=dta,colNames = TRUE)
        }
        # Export excel file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        saveWorkbook(wb, wb_path,overwrite=T)
        rm(wb)
}

# founc.28 df to shp by sf ======================================================================
#message("founc.28 df to shp by sf")
# Unit test
# df2sf2shp_test=read.csv("G:/Layers/Geohydrology/Geohydrology/code/Geohydrology_Rproj/test_verbs/df2sf2shp.csv")
# tictoc::tic()
# st_df_shp=df2sf2shp(df2sf2shp_test)
# tictoc::toc()
# Unit test
df2sf2shp=function(df){
        
        # Split df to spatial and Values Elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ddf=df
        xy = as.data.frame(subset(ddf,,c(x,y)))
        md=as.data.frame(subset(ddf,,-c(x,y)))
        
        # Set Dims ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        isreal_TM_Grid="++proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +units=m +no_defs"
        pts = matrix(,nrow(ddf),2)
        pts[,1]=ddf$x
        pts[,2]=ddf$y
        
        # Biuld SpatialPointsDataFrame Via st and sf elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        st_df_mp = st_sfc(st_multipoint(pts, dim = "XY"))
        st_df_p = st_cast(x = st_df_mp, to = "POINT")
        st_df_pj=st_sf(st_df_p,crs=isreal_TM_Grid)
        st_shp=as(st_df_pj, 'Spatial')
        
        # Join Back Values Elements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        df_shp=spCbind(st_shp,md)
        return(df_shp)
}

# founc.29 Zip file to data table ======================================================================
#message("founc.29 Zip file to data table")

# Inputs
# wrf_zip="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/RB/GFS-05.03.2020-GMT00.zip"
# cosmo_zip="G:/Layers/Geohydrology/Hydrometeorolgy/data/GDB/Tebular/COSMO_DB/Cosmo_partDA_0503202012_fix.zip"
# cosmo_zip=paste0(Background_path,"/Hydrometeorolgy/data/GDB/Tebular/COSMO_DB/","Cosmo_partDA_03_24_2020_00_1.zip")
# cosmo_ts=zip2dt(zip_pth=cosmo_zip,trget_fls=".csv",skip_n=0)%>%setDT()

# Function
zip2dt=function(zip_pth,trget_fls,skip_n){
        # Extract zip file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        folder_pth=str_remove(zip_pth,".zip")
        folder_df=utils::unzip(zipfile=zip_pth, files = NULL, list = T, overwrite = T,
                               junkpaths = T, exdir = ".", unzip = "internal",
                               setTimes = T) %>% as.data.table() %>%
                mutate(file_pth=paste0(zip_pth,"/",Name))%>%
                mutate(file=str_detect(file_pth,trget_fls))%>%
                dplyr::filter(.,file==T)
        # compress to tbl ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for(i in 1:NROW(folder_df)){
                print(i)
                dt_i <- read.table(unz(zip_pth, folder_df$Name[i]), header=T, quote="\"", sep=",",skip = skip_n)
                print(names(dt_i))
                if(i==1){dt=dt_i}else{dt=rbind(dt,dt_i)}}
        return(dt)
}
# founc.30 Save df to Amazon S3 Bucket ======================================================================
#message("founc.30 Save df to Amazon S3 Bucket")
# Unit test
# s3BucketName <- "csdems"
# saveDataDirect(data=CS_model_system,file_name="CS_model_system.csv",bucket_nm=s3BucketName)
# Function
saveDataDirect <- function(data,file_name,bucket_nm) {
        s3write_using(data, FUN = write.csv,row.names = F,bucket = bucket_nm ,object = file_name)
}

# founc.31 geometry2points ==================================================================================
# message("ounc.31 geometry2points")
# Unit test
# geom_st_tst=st_read('G:/Layers/Geohydrology/Geohydrology/Apps/RB/transforms.csv')
# tictoc::tic()
# transforms_ll=geometry2points(geom_st=geom_st_tst,id="ObjectID")
# tictoc::toc()
# Unit test


geometry2points=function(geom_st,id){
        # Get Geometry
        geom_sp=sf::as_Spatial(st_zm(geom_st, drop = TRUE, what = "ZM"))
        geom_typ=as.character(class(geom_sp))
        if(geom_typ== "SpatialLinesDataFrame" | geom_typ== "SpatialLines"){
                geom_lst=lapply(1:NROW(geom_st), function(i) unlist(as.list(geom_sp@lines[[i]]@Lines)))
        }
        if(geom_typ== "SpatialPolygonsDataFrame" | geom_typ== "SpatialPolygons"){
                geom_lst=lapply(1:NROW(geom_st), function(i) unlist(as.list(geom_sp@polygons[[i]]@Polygons)))
        }
        # Convert to points by features
        points_df=as.data.frame(geom_lst[[1]][[1]]@coords) %>% mutate(ID=as.character(geom_st[[id]][1]))
        for (i in 2:NROW(geom_st)){
                points_df_i=as.data.frame(geom_lst[[i]][[1]]@coords) %>% mutate(ID=as.character(geom_st[[id]][i]))
                points_df=rbind(points_df,points_df_i)
        }
        points_df=dplyr::rename(points_df,"x"="V1","y"="V2")
        return(points_df)
}

# founc.32 read .vtk =================================================================================================
# message("ounc.31 geometry2points")
# Unit test
#aa=read_vtk("G:/Layers/Geohydrology/RB/Judea 2.vtk", item = c("points", "triangles", "normals"))
# Unit test
read_vtk<-function(filename, item = c("points","triangles", "normals")){
        item=match.arg(item)
        
        if(!file.exists(filename)) stop("Cannot read: ",filename)
        con=file(filename,open='rb',encoding='ASCII')
        on.exit(close(con))
        magic=readLines(con,n=1)
        if(regexpr("# vtk DataFile Version [234]",magic,ignore.case =T)<0)
                stop("Bad header line in file: ",filename)
        
        title=readLines(con,1)
        encoding=readLines(con,1)
        if(regexpr("ASCII",encoding,ignore.case=TRUE)<0)
                stop("Can only read ASCII encoded VTK pointsets")
        
        datasetLine=toupper(readLines(con,1))
        if(regexpr("^DATASET",datasetLine)<0)
                stop("Missing DATASET line")
        
        datasetType=sub("DATASET\\s+(\\w+)","\\1",datasetLine)
        
        validDatasetTypes<-c("STRUCTURED_POINTS", "STRUCTURED_GRID",
                             "UNSTRUCTURED_GRID", "POLYDATA", "RECTILINEAR_GRID", "FIELD")
        
        if(!datasetType%in%validDatasetTypes)
                stop(datasetType," is not a valid VTK dataset type")
        # if(datasetType!="POLYDATA")
        #   stop("ReadVTKLandmarks can currently only read POLYDATA.",
        #        " See http://www.vtk.org/VTK/img/file-formats.pdf for details.")
        
        pointsLine=toupper(readLines(con,1))
        if(regexpr("POINTS",pointsLine)<0)
                stop("Missing POINTS definition line")
        ptinfo=unlist(strsplit(pointsLine,"\\s+",perl=TRUE))
        if(length(ptinfo)!=3)
                stop("Unable to extract points information from POINTS line",pointsLine)
        nummarkers=as.integer(ptinfo[2])
        if(is.na(nummarkers))
                stop("Unable to extract number of points from POINTS line:",pointsLine)
        datatype=ptinfo[3]
        if(!datatype%in%toupper(c("unsigned_char", "char", "unsigned_short", "short", "unsigned_int", "int",
                                  "unsigned_long", "long", "float", "double")))
                stop("Unrecognised VTK datatype: ",datatype)
        
        points=scan(con,what=1.0,n=3*nummarkers,quiet=TRUE)
        
        # VTK seems to be hardcoded for 3D
        if (item == "points"){
                m=matrix(points,ncol=3,byrow=T)
                colnames(m)=c("X","Y","Z")
                attr(m,"file")=filename
                attr(m,"title")=title
                attr(m,"vtk_datatype")=datatype
        }
        if (item != "points"){
                triangLine=toupper(readLines(con,1))
                if(length(triangLine)==0){
                        warning("No data on polygons found")
                        return(NULL)
                }
                if(regexpr("POLYGONS",triangLine)<0)
                        stop("Missing POLYGONS definition line")
                lninfo=unlist(strsplit(triangLine,"\\s+",perl=TRUE))
                if(length(lninfo)!=3)
                        stop("Unable to extract connection information from POLYGONS line:",triangLine)
                nummconns=as.integer(lninfo[2])
                if(is.na(nummconns))
                        stop("Unable to extract number of connections from POLYGONS line:",triangLine)
                datatype=lninfo[3]
                triang=scan(con,what=1.0,n=4*nummconns,quiet=TRUE)
        }
        if (item == "triangles"){
                m=matrix(triang,ncol=4,byrow=T)[,-1]
                attr(m,"file")=filename
                attr(m,"title")=title
                attr(m,"vtk_datatype")= "int"
        }
        if (item == "normals"){
                normalsLine=toupper(readLines(con,1))
                if(length(normalsLine)==0){
                        warning("No data on Normals found")
                        return(NULL)
                }
                if(regexpr("NORMALS",normalsLine)<0)
                        stop("Missing NORMALS definition line")
                ninfo=unlist(strsplit(normalsLine,"\\s+",perl=TRUE))
                if(length(ninfo)!=3)
                        stop("Unable to extract connection information from POLYGONS line",triangLine)
                datatype=ninfo[3]
                if(!datatype%in%toupper(c("unsigned_char", "char", "unsigned_short", "short", "unsigned_int", "int",
                                          "unsigned_long", "long", "float", "double")))
                        stop("Unrecognised VTK datatype: ",datatype)
                normals=scan(con,what=1.0,n=3*nummarkers,quiet=TRUE)
                m=matrix(normals,ncol=3,byrow=T)
                attr(m,"file")=filename
                attr(m,"title")=title
                attr(m,"vtk_datatype")=datatype
        }
        m
}

# founc.33 GMS Solid to raster =======================================================================================
# message("founc.33 GMS Solid to raster")
# Unit test
# solid_pth="G:/Layers/Geohydrology/RB/Judea 2.vtk"
# part="top"
# base_elv=-400
# resolution=0.96
# Judea_rst=solid2raster(solid_pth="G:/Layers/Geohydrology/RB/Judea 2.vtk",
#                        part="top",
#                        base_elv=NULL,
#                        tolerance=0.99)
# plot(Judea_rst)
# Unit test

solid2raster=function(solid_pth,part,base_elv,tolerance){
        # Read Solid Fron .vtk file
        sld=read_vtk(solid_pth, item = c("points", "triangles", "normals")) %>% as.data.frame(.)
        # Extract part (top || bot) from the solid matirx
        prt=if(part=="top"){max}else{min}
        df=sld %>% group_by(X,Y) %>%
                summarise(Z=prt(Z,na.rm=T))
        if(is.numeric(base_elv)==T){
                df=df  %>%
                        dplyr::filter(Z>base_elv)
        }
        # Build sp pixsel
        xy =subset(df,,c(X,Y))
        isreal_TM_Grid="+init=epsg:2039"
        sdf = SpatialPointsDataFrame(coords = xy, data = subset(df,,Z),proj4string = CRS(isreal_TM_Grid))  %>%
                spTransform(.,crs("+proj=longlat +datum=WGS84"))
        pxsl = SpatialPixelsDataFrame(sdf, tolerance = tolerance, sdf@data)
        # Biuld Raster
        r = raster(pxsl[,'Z'])
        
        # Fill NA Gaps
        fill.na <- function(x, i=5) {
                if( is.na(x)[i] ) {
                        return( round(mean(x, na.rm=TRUE),0) )
                } else {
                        return( round(x[i],0) )
                }
        }
        
        r2=focal(r, w = matrix(1,3,3), fun = fill.na,pad = TRUE, na.rm = FALSE )
        
        return(r2)
}







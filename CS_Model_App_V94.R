tictoc::tic()
library(cli)
library(shiny)
library(shinycssloaders)
library(shinybusy)
library(shinyjs)
library(shinyBS)
library(shinyLP)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(leafpm)
library(htmlwidgets)
library(sp)
library(lwgeom)
library(sf)
library(maptools)
library(mapview)
library(mapedit)
library(Orcs)
library(rgdal)
library(dplyr)
library(readxl)
library(googledrive)
library(data.table)
library(raster)
library(DT)
library(stringr)
library(openxlsx)
library(readr)
library(ggrepel)
library(ggplot2)
library(ggnewscale)
library(plotly)
library(officer)
library(grid)
library(utils)
library(aws.s3)
library(tibble)
library(proj4)
library(purrr)
library(tidyverse)
library(shinyalert)
library(shinyWidgets)
library(cowplot)
library(magick)
library(slickR)

################## App Directory ########################
# G:\Geohydrology\Apps\CS_Model_V01
################## App Directory ########################

# Source Tools Scrips and functions ============================================================
message("Source Tools Scrips and functions")

Background_path="G:/Geohydrology" # %%%%%%%%% Change while moved to unplugged  %%%%
# Prodact_path=paste0(Background_path,"/Geohydrology/Apps/CS_Model_V01/Products")
source('scripts/Geohydrology_Functions_V2.R', encoding = 'UTF-8')
source('scripts/CS_Model_Code_V40.R', encoding = 'UTF-8') #debugSource
source('scripts/Horizons_Model_Code_V9.R', encoding = 'UTF-8') #debugSource
debugSource('scripts/Maps_Code_V2.R', encoding = 'UTF-8') #debugSource
debugSource('scripts/Geology_Model_Code_V1.R', encoding = 'UTF-8') #debugSource

options(shiny.maxRequestSize = Inf)
options(shiny.trace = F)
options(shiny.fullstacktrace = T)
options(shiny.reactlog=T) 
Sys.setlocale("LC_ALL", "Hebrew")
# Clean before deploy: "C:\Users\yoniis\AppData\Local\Temp"

# Global Variables -------------------------------------------------------------
modeldialog_status="Inactive"
`%notin%` <<- Negate(`%in%`)
charts=NULL
tor="t" # w for web ; t for test
cs_id_i=1

# Set Aotintication File =======================================================================
message("Set Aotintication File")
Password_df=data.frame(username = c("Security","Gov","Academy"),
                       Password = c("hydro2020@1","hydro2020@2","hydro2020@3"),
                       auth_lvl = c("Security","Gov","Academy"),
                       stringsAsFactors = FALSE
)

# Get Base Layers ==============================================================================
message("Get Base Layers")
# Local Path -----------------------------------------------------------------------------------
transforms_pth= 'data/Background_layers/Transforms/TJ_FAULTS_ll.shp'
basemap_pth=paste0("data/Background_layers/BaseMaps/")
design_pth='data/Design_elements'
well_pth='data/Background_layers/Wells'
dem_pth = "data/DEMs"

# Set Base Layers ===============================================================================
message("Set Base Lyers")

# TJ Faluts 
transforms_ll=sf::st_read(transforms_pth)
transforms_shp=sf::as_Spatial(st_zm(transforms_ll, drop = TRUE, what = "ZM"))
crs(transforms_shp)=sp::CRS("+proj=longlat +datum=WGS84")
transforms_df=transforms_shp %>% as.data.frame(.) %>% subset(.,,"ObjectID")

# Libraries Boundaries
library_dms=sf::st_read(paste0(dem_pth,"/library_dms_V2",".shp"))%>% 
  dplyr::filter(.,basin != "national")

# Geology - Base map
geology_50=sf::st_read(paste0(basemap_pth,"geology_50_V6.shp"))
geology_50_proxy=geology_50[1,]
geoIDX_50=read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),sheet = "geoINDEX50")

geology_200=sf::st_read(paste0(basemap_pth,"geology_200_V4.shp"))
geology_200_proxy=geology_200[1,]
geoIDX_200=read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),sheet = "geoINDEX200")

geoIDXs=bind_rows(subset(geoIDX_200,,c("hex","Code")),subset(geoIDX_50,,c("hex","Code")))
geopal=colorFactor(palette = geoIDXs$hex,
                   domain = geoIDXs$Code,
                   ordered=T,
                   na.color = "#808080")


# National Tiles
hebrew_str <- locale(encoding = "ISO-8859-8")
localtiles_df=readr::read_csv(paste0(design_pth,"/localtiles_V1.csv"),locale=hebrew_str) 


# Geology - Tiles Grid
geogrid= dplyr::filter(sf::st_read(paste0(basemap_pth,"geogrid_V10.shp")),!is.na(resoid) & resoid!=0,)

# Virtual Wells - Template
Virtual_int=fread(paste0(well_pth,"/Virtual_Wells.csv"),colClasses=c("character","character","character",
                                                                     "double","double","double","character","character","character","double","double","double",
                                                                     "character","character","integer"))
Virtual_dt=Virtual_int
INDEX_DEMs = as.data.frame(read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),
                                      sheet = "Index_DEMs",
                                      col_types=c("text","text","text","text","text")))

cs_ids=read.csv(paste0(design_pth,"/cs_ids_V1.csv"))

# CS Horizons 
tab_raw=NULL
tab_tbl=NULL
horizons_db=NULL
segment_id=0
lines_db<<-NULL
cs_tagging=NULL
dlt_optn="active"
initial_view="inactive"

# Geology Model Builder
algorithms_s=c("Support Vector Machine","Random Forests","Neural Networks","Kriging","IDW")
unit_bounds_st=NULL
geology_blocks_st=NULL
gmgrid_pnt=NULL
geology_map_act=NULL
upper_rst=NULL
geomdl=NULL
#Geology_Description_ss=NULL

# Additional Layers
additional_layers_df=read.csv(paste0(design_pth,"/additional_layers_ids_V1.csv"))
additional_layers_lst=list()

# Visualization
noHide_status<<-F
zoom_old=10

# Info System =================================================================================

msgactionBttn = function(infoId,color,c_label){
  ab = actionBttn(inputId=infoId,
                  style="gradient",
                  color = color,
                  size="xs",
                  label = c_label,
                  icon = icon("info-circle")) 
  return(ab)
}

creditactionBttn = function(infoId,color,c_label,icon){
  ab = actionBttn(inputId=infoId,
                  style="gradient",
                  color = color,
                  size="md",
                  label = c_label,
                  icon = icon) 
  return(ab)
}

msgalart = function(c_title,c_text){
  sa=shinyalert(
    title = c_title,
    text = c_text,
    closeOnEsc = T,
    closeOnClickOutside = T,
    html = T,
    type = "",
    showConfirmButton = F,
    showCancelButton = F,
    #confirmButtonText = "OK",
    #confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = T
  )
  return(sa)
}
# Run App ######################################################################################
message("################################### Run App ###################################")
tictoc::toc()
# UI ===========================================================================================
ui <- fluidPage(
  # Busy Status
  add_busy_spinner(
    spin = "double-bounce",
    color = "#112446",
    timeout = 100,
    position ="bottom-left",
    onstart = TRUE,
    margins = c(10, 10),
    height = "50px",
    width = "50px"
  ),
  # Set Main map 
  tags$style(HTML("
                  #mainmap {
                  position: absolute;
                  
                  top: 35px;
                  }
                  ")),
  # Set Geology model map 
  tags$style(HTML("
                  #geo2d_map {
                  position: absolute;
                  
                  top: 35px;
                  }
                  ")),
  # conditionalPanel(
  #   condition = "input.tabs=='Build Geology Model'",
  #   useShinyjs(),
  #   leafletOutput("geo2d_map", width="100%", height = "100%")
  # ),
  # Browser title
  list(tags$head(HTML('<link rel="icon", href="app_icon.jpg",
                        type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="CSMS 2.1.1"
      )
  ),
  
  fluidRow(
    div(
      navbarPage(id = "tabs",
                 title=div(img(src="app_icon_small_black.jpg"), "Cross Section Model System"),
                 inverse = F, # for diff color view
                 theme = shinytheme("flatly"),
                 useShinyjs(),
                 ## Home panel ==================================================
                 message("Home panel"),
                 
                 tabPanel("Home", icon = icon("home"),
                          span(actionButton("info", 
                                            icon = icon("info-circle"),
                                            style="color: #fff; background-color: #2c3e50; border-color: #2e6da4",
                                            width = "120px",
                                            label = HTML("<span style='font-size:1.1em 'text-align:justify'><br />Info (Inactive)</span>")
                          ),
                          style = "position:absolute;right:2em;"),
                          HTML("<div style='vertical-align:middle; text-align:center'> 
                                  <img src='app_panel_1.jpg' width='1400' height='500' alt='This is alternate text'></img>
                             </div>"),
                          HTML("<div style='vertical-align:middle; text-align:center'> 
                                  <h2 style='text-align:middle;'>Welcome to CSMS<sup>®</sup> Softwar, Explore & Analysis your deep data.</h2>
                             </div>"),
                          slickROutput("slickr", width="1400px")#,
                 ),
                 
                 ## Map panel ===================================================
                 message("Map panel"),
                 
                 tabPanel("Map", icon = icon("location-arrow"),
                          ### Sidebar panel for Aoturization --------------------
                          message("Sidebar panel for Aoturization"),
                          fluidRow(
                            hr(),
                            leafletOutput("mainmap",height = "1950px",width = "3400px"),
                            column(width = 5, style = "background-color:#2c3e50; opacity: 0.8;",
                                   ###  Control actionButton Column --------------------------------------------------------------
                                   column(width = 3,offset = 1,
                                          passwordInput("PW", creditactionBttn(infoId="Password_info",color="default",c_label="Registered User",icon=img(src = "IHS_logo.jpg",
                                                                                                                                                         height = 36, width = 38)),placeholder=NULL),
                                          # Geohydrology Layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          uiOutput("modelUI"),
                                          # Additional Layers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          selectInput("additional_layers_type",
                                                      creditactionBttn(infoId="additional_layers_type_info",color="default",c_label="Additional Layers",
                                                                       icon= tags$i(class = "fas fa-layer-group", style="font-size: 34px; color: gray")),
                                                      multiple=F,
                                                      selected=NULL,
                                                      choices=additional_layers_df$type),
                                          fileInput('additional_layers',
                                                    label="",
                                                    accept = c(
                                                      '.tif',
                                                      ".shp",".dbf",".sbn",".sbx",".shx",".prj"
                                                    ),
                                                    multiple=T
                                          )
                                   ),
                                   column(width = 3,offset = 0,
                                          selectInput("geomap",
                                                      creditactionBttn(infoId="geomap_info",color="default",c_label="Geological Maps",
                                                                       icon=img(src = "GSI_logo.jpg", height = 34, width = 34)),
                                                      multiple=F,
                                                      selected="Regional-Low",
                                                      choices=c("Local-High","Regional-Low")),
                                          selectInput("country",
                                                      creditactionBttn(infoId="country_info",color="default",c_label="Country",
                                                                       icon= tags$i(class = "fas fa-flag", style="font-size: 34px; color: grey")),
                                                      multiple=F,
                                                      selected="Israel",
                                                      choices=c("Israel","Indefinite")),
                                          actionButton("login", # Refresh Button
                                                       icon = tags$i(class = "fas fa-redo", style="font-size: 40px"),
                                                       style="color: #000000; background-color: #FFFFFF; border-color: #FFFFFF",
                                                       width = "70px",
                                                       label =""
                                          )
                                   ),
                                   column(width = 3,offset = 0.5,
                                          fileInput('file1',
                                                    creditactionBttn(infoId="Refresh_info",color="default",c_label="Upload wells",
                                                                     icon= tags$i(class = "fas fa-file-upload", style="font-size: 34px; color: gray")),
                                                    accept = c(
                                                      'text/csv',
                                                      'text/comma-separated-values',
                                                      'text/tab-separated-values',
                                                      'text/plain',
                                                      '.csv',
                                                      '.tsv'
                                                    )
                                          )
                                   ),
                                   column(width = 3,offset = 0.5,
                                          fileInput('csgrid',
                                                    creditactionBttn(infoId="csgrid_info",color="default",c_label="Upload CS Grid",
                                                                     icon= tags$i(class = "fas fa-file-upload", style="font-size: 34px; color: brown")),
                                                    accept = c(
                                                      'text/csv',
                                                      'text/comma-separated-values',
                                                      'text/tab-separated-values',
                                                      'text/plain',
                                                      '.csv',
                                                      '.tsv'
                                                    )
                                          )
                                   )
                            )
                          ),
                          class = "span1"),
                 
                 ## Data panel ==================================================
                 message("Data panel"),
                 
                 tabPanel("Wells Meta Data",  icon = icon("database"),
                          fluidRow(
                            # Buffer Selection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
                            numericInput("Buffer",  msgactionBttn(infoId="Buffer_info",color="default",c_label="Search strip width around the CS [km]:"),
                                         min = 0.01, max = 2, value = 0.4,step=0.01)
                          ),
                          DTOutput("CS_model_system")),
                 
                 ## Cross Section panel =========================================
                 message("Data panel"),
                 
                 tabPanel("Cross Section panel",  icon = icon("google-wallet"),
                          ### Sidebar panel for Inputs -------------------------------------------------------------
                          message("Sidebar panel for Inputs"),
                          fluidPage(
                            column(2,
                                   # Initiation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   tags$head(tags$script(src = "message-handler.js")),
                                   actionButton("Get",
                                                icon = icon("google-wallet"),
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                width = "200px",
                                                label = HTML("<span style='font-size:1.3em;'><br />Run</span>")
                                   ),
                                   titlePanel("Model Parameters"),
                                   
                                   # Build Solids ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   checkboxInput("Build_Solids", msgactionBttn(infoId="Build_Solids_info",color="default",c_label="Build_Solids:"),value = F),
                                   
                                   # Cross Section Type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   radioButtons(inputId="CS_type",label=  msgactionBttn(infoId="CS_type_info",color="default",c_label="Cross Section Type:"),
                                                selected = NULL,
                                                choices=c("Groups" = "groups",
                                                          "Formations" = "formations",
                                                          "Materials" = "materials")),
                                   # Surface geology ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput(inputId="geological_cs_surf",creditactionBttn(infoId="geological_cs_surf_info",color="default",c_label="Surface Geology",
                                                                                             icon=img(src = "GSI_logo.jpg", height = 18, width = 18)),
                                               multiple=F,
                                               selected="Geomap",
                                               choices=c("Geomap"="geomap",
                                                         "Geomap With Free Colors"="geomap_free_colors",
                                                         "Blind"="blind"),
                                   ),
                                   # Measurement Year ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput(inputId="measurement_year",label =  msgactionBttn(infoId="measurement_year_info",color="default",c_label="Measurement Year:"),
                                                min = 1970,value=NULL, max = 2020,step=1), #  value = 2018
                                   # Season ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput(inputId="season",label =  msgactionBttn(infoId="season_info",color="default",c_label="Measurement Season:"),
                                               multiple=F,
                                               selected="",
                                               choices=c("","Summer-Autumn","Winter-Spring"),
                                   ),
                                   # Resolution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Resolution",  msgactionBttn(infoId="Resolution_info",color="default",c_label="Horizontal Grid Resolution:"),
                                                min = 10, max = 1000, value = 50,step=10),
                                   # vertical Resolution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("vertical_resolution",  msgactionBttn(infoId="vertical_resolution_info",color="default",c_label="Vertical Grid Resolution:"),
                                                min = 1, max = 500, value = 50,step=5),
                                   # Title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   textInput("title_CS",  msgactionBttn(infoId="title_CS_info",color="default",c_label="Title:"),value="Test CS"),
                                   # ID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("cs_id",  msgactionBttn(infoId="cs_id_info",color="default",c_label="Cross Section ID:"),
                                               multiple=F,
                                               selected="A",
                                               choices=cs_ids$cs_id,
                                   ),
                                   # Transforms ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   #checkboxInput("transforms","Transforms:",value = T),
                                   # Projection Line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   checkboxInput("Projection_Line", msgactionBttn(infoId="Projection_Line_info",color="default",c_label="Match wells to line:"),value = F),
                                   # label_size ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("label_size",  msgactionBttn(infoId="label_size_info",color="default",c_label="Label Size:"),
                                                min = 10, max = 90, value = 50,step=10),
                                   # Interval_between_labeles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Interval_between_labeles",  msgactionBttn(infoId="Interval_between_labeles_info",color="default",c_label="Labeles Interval & Upper Limit:"),
                                                min = 50, max = 400, value = 200,step=10),
                                   # Lower Limit ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Lower_Limit",  msgactionBttn(infoId="Lower_Limit_info",color="default",c_label="Lower Limit:"),
                                                min = -4000, max = 0, value = NA,step=100), 
                                   # Surface Smooth ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Surface_Smooth",  msgactionBttn(infoId="Surface_Smooth_info",color="default",c_label="Surface Smooth:"),
                                                min = 0, max = 2, value = 0,step=0.1),
                                   # DEM Line Thickness ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("DEM_line_thickness",  msgactionBttn(infoId="DEM_line_thickness_info",color="default",c_label="DEM Line Thickness:"),
                                                min = 0, max = 2, value = 1,step=0.1)#,
                                   
                            ),
                            ### Slider Panel for Preview ------------------------
                            column(9,offset=0,
                                   shinyjs::useShinyjs(),
                                   textOutput("cs_messages"),
                                   plotOutput("cs_chart",height = "1200px",width = "2140px"),
                                   fluidRow(downloadButton('cs_download_ppt', 'Download Editable'),
                                            downloadButton('cs_download_html', 'Download Interactive')
                                   )
                                   
                            )
                          ),
                 ),
                 
                 ## Build Horizons =============================================
                 message("Build Horizons"),
                 
                 tabPanel(title="Build Horizons",icon = icon("bacon"),
                          fluidPage(
                            ### Sidebar panel for Inputs -------------------------------------------------------------
                            column(width = 2,
                                   # Classifier - Point Size ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("hpoint_size",  msgactionBttn(infoId="hpoint_size_info",color="primary",c_label="Point Size:"),
                                                min = 1, max = 10, value = 1,step=1),
                                   # Classifier - add Values Between points by interpolation ~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("h_int", msgactionBttn(infoId="h_int_info",color="primary",c_label="Join Points:"),
                                               multiple=F,
                                               choices=c("linear","spline","NaN"),
                                               selected="linear"),
                                   # Classifier - Set the density of the interpolation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("h_res",  msgactionBttn(infoId="h_res_info",color="primary",c_label="Join Density [m]:"),
                                                min = 0, max = 1000, value = 10,step=1),
                                   # Classifier - Select what to class ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("Select_horizon_by", msgactionBttn(infoId="Select_horizon_by_info",color="primary",c_label="Classifer:"),
                                               multiple=F,
                                               choices=c("Wells","Hydrogeology libraries"),
                                               selected="Hydrogeology libraries"),
                                   # Classifier - Select Horizon ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("Select_horizon", msgactionBttn(infoId="Select_horizon_info",color="primary",c_label="Select horizon:"),
                                               multiple=F,
                                               choices=INDEX_DEMs$f_name),
                                   # Classifier - Eraser sensitivity [m]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("Eraser_sensitivity", msgactionBttn(infoId="Eraser_sensitivity_info",color="primary",c_label="Eraser sensitivity [m]:"),
                                                min = 0, max = 1000, value = 10,step=5),
                                   fluidRow(
                                     # Classifier - Create /Add to Horizon DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                     actionButton(inputId = "add2hdb",
                                                  icon = icon("database"),
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                  width = "204px",
                                                  label = HTML("<span style='font-size:1.3em;'><br />Create/Add to DB</span>")
                                     )
                                   ),
                                   fluidRow(
                                     # Classifier - Repair Horizon ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                     actionButton(inputId = "repair",
                                                  icon = icon("tools"),
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                  width = "204px",
                                                  label = HTML("<span style='font-size:1.3em;'><br />Repair Horizon</span>")
                                     )
                                   ),
                                   # Classifier - Download Horizon ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   fluidRow(
                                     shinyjs::hidden(
                                       downloadButton("download_horizon",
                                                      icon = icon("accusoft"),
                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                      width = "100px",
                                                      label = HTML("<span style='font-size:1.3em;'><br />Download Horizons</span>")
                                       )
                                     )
                                   )
                                   
                            ),
                            ### Slider Panel for Edit Horizon --------------------------------------------------------
                            column(9,
                                   plotOutput("cs_tagging",height = "1200px",width = "2140px",click = "plot_click"),
                                   DT::dataTableOutput("info"),
                                   # Segment creator by right click
                                   tagList(
                                     tags$script(
                                       "$(function(){
                                          $(this).bind('contextmenu', function (e) {
                                            e.preventDefault()
                                            Shiny.setInputValue('plop', Math.random());
                                          });
                                        });"
                                     )
                                   )
                            )
                          )
                 ),
                 ## Build Geology Model ========================================
                 message("Build Geology Model"),
                 tabPanel(title="Build Geology Model",icon = icon("accusoft"),
                          fluidRow(
                            ### Background product view ----------------------------------------------------
                            uiOutput("geo_view"),
                            ### Sidebar panel for Inputs ---------------------------------------------------
                            column(width = 2,
                                   hr(),
                                   # Model Builder - Run Geology Model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   actionButton("RGM",
                                                icon = icon("accusoft"),
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                width = "200px",
                                                label = HTML("<span style='font-size:1.3em;'><br />Run</span>")
                                   ),
                                   # Model Builder - Horaizon type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("horizon_type", msgactionBttn(infoId="horizon_type_info",color="primary",c_label="Horizon Type:"),
                                               multiple=F,
                                               choices=c("Base","Top"),
                                               selected="Base"),
                                   # Model Builder - Horizon unit ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("horizon_unit", msgactionBttn(infoId="horizon_unit_info",color="primary",c_label="Horizon Unit:"),
                                               multiple=T,
                                               choices=NULL),
                                   # Model Builder - not included CS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("notincluded", msgactionBttn(infoId="notincluded_info",color="primary",c_label="Not included CS's:"),
                                               multiple=T,
                                               choices=NULL),
                                   # Model Builder - surface Geology ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("surface_unit", msgactionBttn(infoId="surface_unit_info",color="primary",c_label="Surface Unit:"),
                                               multiple=T,
                                               choices=NULL),
                                   # Model Builder - Observation Points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   fluidRow(
                                     column(width = 7,
                                            selectInput("observation_points", msgactionBttn(infoId="observation_points_info",color="primary",c_label="observation_points:"),
                                                        multiple=T,
                                                        choices=NULL),
                                     ),
                                     column(width = 7,
                                            selectInput("obs_notincluded", msgactionBttn(infoId="obs_notincluded_info",color="primary",c_label="Not included Obs':"),
                                                        multiple=T,
                                                        choices=NULL),
                                     ),
                                     column(2,
                                            checkboxInput("obs_inclod",label =""),
                                            )
                                   ),
                                   # Model Builder - Unit Boundary ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   fluidRow(
                                     column(width = 7,
                                            fileInput('unit_Bounds',
                                                      msgactionBttn(infoId="unit_Bounds_info",color="primary",c_label="Unit Boundary:"),
                                                      accept = c(
                                                        '.tif',
                                                        ".shp",".dbf",".sbn",".sbx",".shx",".prj"
                                                      ),
                                                      multiple=T
                                            ),
                                     ),
                                     column(width = 2,
                                            actionButton("rst_Bounds", # Refresh Button
                                                         icon = tags$i(class = "fas fa-redo", style="font-size: 20px"),
                                                         label =""
                                            )
                                     )
                                   ),
                                   
                                   
                                   # Model Builder - Geology Blocks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   fluidRow(
                                     column(width = 7,
                                            fileInput('geology_blocks',
                                                      msgactionBttn(infoId="geology_blocks_info",color="primary",c_label="Geology Blocks:"),
                                                      accept = c(
                                                        '.tif',
                                                        ".shp",".dbf",".sbn",".sbx",".shx",".prj"
                                                      ),
                                                      multiple=T
                                            ),
                                            
                                     ),
                                     column(width = 2,
                                            actionButton("rst_blocks", # Refresh Button
                                                         icon = tags$i(class = "fas fa-redo", style="font-size: 20px"),
                                                         label =""
                                            )
                                     )
                                   ),
                                   # Model Builder - Geology Blocks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   fluidRow(
                                     column(width = 7,
                                            fileInput('upper_layer',
                                                      msgactionBttn(infoId="upper_layer_info",color="primary",c_label="Upper Layer:"),
                                                      accept = c(
                                                        '.tif'),
                                                      multiple=T
                                            ),
                                            
                                     ),
                                     column(width = 2,
                                            actionButton("rst_upper", # Refresh Button
                                                         icon = tags$i(class = "fas fa-redo", style="font-size: 20px"),
                                                         label =""
                                            ),
                                            checkboxInput("rst_cutter",label =""),
                                     )
                                   ),
                                   # Model Builder - Grid Resolution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   numericInput("grid_res", msgactionBttn(infoId="grid_res_info",color="primary",c_label="Grid Resolution [m]:"),
                                                min = 100, max = 3000, value = 1000,step=500),
                                   # Model Builder - Interpolation Algorithm ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("interpolation_algorithm", msgactionBttn(infoId="interpolation_algorithm_info",color="primary",c_label="Interpolation Algorithm:"),
                                               multiple=F,
                                               choices=algorithms_s,
                                               selected="Support Vector Machine"),
                                   # Model Builder - Algorithm Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   uiOutput("algosUI"),
                                   
                                   # Model Builder - Export model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                   selectInput("expm_v", msgactionBttn(infoId="iexpm_v_info",color="primary",c_label="Export Model to:"),
                                               multiple=F,
                                               choices=c("XYZ Grid", "Raster","Contour"),
                                               selected="Raster"),
                                   uiOutput("expmUI"),
                                   
                                   
                            ),
                            ### Slider Panel for Model Preview & edit--------------------------
                            column(9,offset=0,
                                   shinyjs::useShinyjs(),
                                   fluidRow(
                                     # View type ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                     shinyWidgets::prettyRadioButtons("geo_dims",label=NULL,
                                                                      choices = c("2D","3D", "Edit"),
                                                                      selected = "2D",
                                                                      inline =T,
                                                                      shape ="square",
                                                                      outline=T,
                                                                      fill=T,
                                                                      thick=T),
                                     # Interpolation Process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                     textOutput("geo_messages" ),
                                   )
                                   # plotOutput("clibration_plot",height = "1200px",width = "2140px")
                            )
                          )
                 )
      )
      ,
      class="span7")
  )
)

# Server ======================================================================================
server <- function(input, output, session) {
  # Load Info System --------------------------------------------------------------------------
  observeEvent(input$info,once = F,priority=1, handlerExpr={
    shinyalert(
      title = "Welcome to CSMS<sup>®</sup> Software",
      text = "<h4 style='text-align:justify;'>This software is the interrogation system of the National Geohydrology Database of Israel.</h4>
            <p style='text-align:justify;'><b>The Geohydrology National Database</b> includes geological data from 9,000 groundwater wells, geotechnical wells, oil & gas wells. About 70 geological spatial layers that maps the subsoil at a national and regional level and about 120 hydrological layers that maps the groundwater and salinity levels in the various aquifers in Israel.</p>.
            <p style='text-align:justify;'><b>CSMS software</b> allows examination and presentation of the data, this by creation of standard Cross Sction along lines defined by the user. The information present by geological and hydrogeological cross sections. The sections can be exported for further editing and updating as Power Point files (<a href='https://csexamp.s3.us-east-2.amazonaws.com/Editable_CS-Exmp_V2.pptx'>see example</a>) or HTML files (<a href='https://csexamp.s3.us-east-2.amazonaws.com/Interactive_CS_exmp_V3.html'>see example</a>)</p>.
            <p style='text-align:justify;'><em><b>Until 01.01.2022, the software is in beta version and free to use without the need to enter a password</b></em>. Fore initial training, and to obtain sample files for building a personal database, please contact <b>CRS hydrology</b> by email: <a href='mailto:CRShydrology@gmail.com'>CRShydrology@gmail.com</a>.</p>.
            <p style='text-align:justify;'>However, in order to gain access to the Geohydrology National Database, please contact the <b>Hydrogeology Division of the Water Authority</b> with a data request fee by email: <a href='mailto:YakovL20@water.gov.ill'>YakovL20@water.gov.il</a> </p>.
            <h4 style='text-align:center;'>Clicking OK constitutes acceptance of the <a href='https://csexamp.s3.us-east-2.amazonaws.com/terms-of-use-CSMS_V1.pdf'>terms of use</a> of the software </h4>",
      closeOnEsc = F,
      closeOnClickOutside = F,
      html = T,
      type = "info",
      showConfirmButton = T,
      showCancelButton = F,
      confirmButtonText = "OK",
      confirmButtonCol = "#1274a1",
      timer = 0,
      imageUrl = "",
      animation = T,
      immediate=T
    )})
  message("Load Info System")
  # Galery Slider ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$slickr <- renderSlickR({
    imgs_df <- list.files("www/", pattern=".png", full.names = TRUE) %>% as.data.frame() %>%
      dplyr::rename(.,"filepth"=".") %>% 
      mutate(Id=as.numeric(gsub('www/app_animation_|.png', '',filepth))) %>% 
      dplyr::arrange(.,Id)
    
    slickR(imgs_df$filepth)+ settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 1200, speed=5000)
  })
  
  
  # Argument Info System ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$geomap_info,once = F, handlerExpr={msgalart("Geology maps grid","<p style='text-align:left'>You can choose between two grids:
                                                                 <p style='text-align:left'>1.<b>Low-resolution</b> grid of 1:200,000 square meters.</p>
                                                                 <p style='text-align:left'>2.<b>High resolution grid</b> of 1:50,000 square meters.</p>
                                                                 <p style='text-align:left'>The low-resolution grid allows you to load up to 6,000 boreholls and up to 2 geological tiles in each view. The high-resolution grid allows you to load up to 15 boreholls at most and load only one geologycal tile per view.</p>")})
  observeEvent(input$country_info,once = F, handlerExpr={msgalart("Country","<p style='text-align:justify;'>Country Selection defines the coordinate system as a local (metric) system and displays background layers belonging to the selected country.</p>")})
  observeEvent(input$Password_info,once = F, handlerExpr={msgalart("Password","<p style='text-align:justify;'>In order to gain access to the Geohydrology National Database, please contact the <b>Hydrogeology Division of the Water Authority</b> with a data request fee by email: <a href='mailto:YakovL20@water.gov.il'>YakovL20@water.gov.il</a> </p>")})
  observeEvent(input$additional_layers_type_info,once = F, handlerExpr={msgalart("Additional Layers","<p style='text-align:justify;'>Additional layers can be loaded for display on the geoloy model. Also, using this parameter you can override general parameters provided by the software database, such as the Height Model (DTM) in local databases. The layer type must be set using the <b>Additional layer type</b> parameter. Full details on the type of layers that can be displayed appear when the layer type is selected. </p>")})
  
  observeEvent(input$Refresh_info,once = F, handlerExpr={msgalart("Refresh wells data base","<p style='text-align:justify;'>Access to the database can be obtained by requesting data from the <b>Hydrogeology Division of the Water Authority</b>by email: <a href='mailto:YakovL20@water.gov.il'>YakovL20@water.gov.il</a>. It is also possible to build a database independently that includes wells that do not exist in the <b>National Database</b>, an example of the database can be downloaded at the <a href='https://csexamp.s3.us-east-2.amazonaws.com/Geology_Description_Examp.csv'>link</a>. The meta-data file appears in the <a href='https://csexamp.s3.us-east-2.amazonaws.com/Geology_Description_Metadata.csv'>link</a>. For further explanations and coding system files, please contact us by email: <a href='mailto:CRShydrology@gmail.com'>CRShydrology@gmail.com</a>. </p>")})
  observeEvent(input$Gridfile_info,once = F, handlerExpr={msgalart("Upload Grid of Cross Sections - Inactive","<p style='text-align:justify;'>Dev Database</a>. </p>")})
  
  observeEvent(input$Background_info,once = F, handlerExpr={msgalart("Active only for authorized users after entering a password","Hydrogeology libraries contain spatial, dynamic and static, data in each of the listed areas. The data displayed on the cross section by dott-lines (<a href='https://csexamp.s3.us-east-2.amazonaws.com/Background_info-Exmp_V1.pptx'>see example</a>).")})
  observeEvent(input$Build_Solids_info,once = F, handlerExpr={msgalart("Active only After selection of hydrogeology library and active only for a single library","Solids are representation of two-dimensional surfaces between the layers in the subsoil according to the order of the layers")})
  observeEvent(input$CS_type_info,once = F, handlerExpr={msgalart("Cross Section Type",
                                                                  "<p style='text-align:left'>The Cross section can be represented in three levels:</p>
                                                                          <p style='text-align:left'>1. Geological groups such as <i>'Jaffa'</i>, <i>'Mount Scopus'</i> and <i>'Judea'</i>.</p>
                                                                          <p style='text-align:left'>2. Geological formations such as <i>'Bina'</i>, <i>'Kfar Shaul'</i> and <i>'Moza'</i>.</p>
                                                                          <p style='text-align:left'>3. Lithological composition of the rock along the log, such as <i>'sand'<i>, </i>'claly sand'</i>, <i>'limestone'</i>, etc.</p>
                                                                          <p style='text-align:left'>The overall list of categories appears in the <a href='https://csexamp.s3.us-east-2.amazonaws.com/CS_type_info-Exmp_V1.xlsx'>following link</a>.</p>")})
  observeEvent(input$geological_cs_surf_info,once = F, handlerExpr={msgalart("Surface geology along the Cross Section","<p style='text-align:left'>With this parameter the surface geology along the section can be obtained on the basis of 1: 50,000 maps and 1: 200,000 maps. When there is no high-resolution data, a free color palette can be used to highlight the differences between the Formations")})
  observeEvent(input$cs_id_info,once = F, handlerExpr={msgalart("Cross Section ID","The Cross section Identified by tags marked with <b>X</b> and <b>X'</b> at the beginning and end of the cross-section (respectively). It is customary to display the cross-section from <b>A</b> in alphabetical order until the last cross-sections in the study area</p>")})
  observeEvent(input$title_CS_info,once = F, handlerExpr={msgalart("Cross Section Title","The Cross-Section title is defined in this parameter, usually while specifying the study area and the direction of the section, for example: <i><b>'Depth section across the western mountain aquifer from northwest to southeast'</i></b>.")})
  observeEvent(input$season_info,once = F, handlerExpr={msgalart("Season","<p style='text-align:left'>Water-Levels & Water-Quality (in this version salinity only) are presented according to the following division:</p>
                                                                          <p style='text-align:left'>1. Last value - when the argument is not marked.</p>
                                                                          <p style='text-align:left'>2. Last value for months 1-5 when <b>Winter-Spring</b> option selected.</p>
                                                                          <p style='text-align:left'>3. Last value for months 6-12 when <b>Summer-Autumn</b> option selected.</p>")})
  observeEvent(input$measurement_year_info,once = F, handlerExpr={msgalart("Measurement Year","<p style='text-align:left'>Water-Levels & Water-Quality (in this version salinity only) are presented according to the following division:</p>
                                                                          <p style='text-align:left'>1. Last value - when the argument is not marked.</p>
                                                                          <p style='text-align:left'>2. Last value for the selected year.</p>")})
  observeEvent(input$Projection_Line_info,once = F, handlerExpr={msgalart("Projection Line (Match wells to line)","In selecting of this parameter the elevations of the well (including the geological units and the perforations) will be attached to the surface. If the projection creates a significant distortion (over then 10 m' diffrance of the elevation at distance of less than 100 m from the cross section), the text box at the  bottom of the well log will be painted to red.")})
  observeEvent(input$label_size_info,once = F, handlerExpr={msgalart("Label Size","This parameter controls the size of all the alphanumeric components in the Cross-Section such as the axis titles, the caption on the axis, the titles on the logs in each well, and so on.")})
  observeEvent(input$Interval_between_labeles_info,once = F, handlerExpr={msgalart("Interval Between Labeles","This parameter controls the vertical distance between all the alphanumeric components in the Cross-section such as the axis titles, the caption on the axes, the titles on the logs and so on. It is recommended to use about <b>200-300</b> in Cross_Section in a mountainous area and <b>50-100</b> in coastal area.")})
  observeEvent(input$Lower_Limit_info,once = F, handlerExpr={msgalart("Lower Limit","This parameter controls the base of the Cross-Section (at a metric height above sea level) Any Well that crosses the base height is marked with the word <b>'cut'</b> in diagonal writing at the base of the log.")})
  observeEvent(input$Surface_Smooth_info,once = F, handlerExpr={msgalart("Surface Smooth","This parameter controls the level of smoothness of the spatial data (surface and other subterranean spatial data such as geological formations). This parameter is <b>especially important when the cross-section does not pass in a straight line</b> and a gap is created between the point information and spatial information as can be seen in <a href='https://csexamp.s3.us-east-2.amazonaws.com/Surface_Smooth_info-Exmp_V1.pptx'>the example</a>.")})
  observeEvent(input$Buffer_info,once = F, handlerExpr={msgalart("Buffer width","By this paramter you can to set the search zone around of wells around the cross section. <b>Changing the parameter changes the search area even without the need to run the CS again.</b>")})
  observeEvent(input$Resolution_info,once = F, handlerExpr={msgalart("Resolution","By this paramter you can to set the resoloution in Y axis.")})
  observeEvent(input$vertical_resolution_info,once = F, handlerExpr={msgalart("vertical Resolution","By this paramter you can to set the resoloution in X axis.")})
  observeEvent(input$DEM_line_thickness_info,once = F, handlerExpr={msgalart("DEM Line Thickness","By this paramter you can to set the thickness of all spatial layers in the Cross-Section.")})
  observeEvent(input$Update_points_info,once = F, handlerExpr={msgalart("Update Points","Show the new horizon points on the  cross section")})
  observeEvent(input$Select_horizon_info,once = F, handlerExpr={msgalart("Select Geology or hydrology horizon","Select a horizon from one of the horizons shown in the section and rebuild it.")})
  observeEvent(input$Select_horizon_by_info,once = F, handlerExpr={msgalart("Select the data source for the classification","Classification can be based either on existing data from existing hydrogeological libraries or on data that exists in wells and does not exist in existing hydrogeological libraries.")})
  observeEvent(input$Eraser_sensitivity_info,once = F, handlerExpr={msgalart("Erase Point form the cross section","With this parameter it is possible to control the number of points to erase. Choose a <b>larger</b> distance to delete more points at once.")})
  observeEvent(input$hpoint_size_info,once = F, handlerExpr={msgalart("Set horizon points size","Use this variable to determine the size of the point on the cross section.")})
  observeEvent(input$h_int_info,once = F, handlerExpr={msgalart("Enable interpolation between points along the horizon","Use this variable to select whether, and what type of interpolation to use to connect points along the horizon.")})
  observeEvent(input$h_res_info,once = F, handlerExpr={msgalart("Set the resolution of the interpolation between points along the horizon","Use this variable to set the resolution of the interpolation.")})
  
  # Open login ----------------------------------------------------------------------------------------------
  message("Open login")
  geo_v=reactive({input$geomap})
  mode_v=reactive({input$PW})
  db_v=reactive({input$file1})
  hz_v=reactive({input$file2})
  output$loginUI=renderUI({
    if(as.character(mode_v())=="" & (as.character(db_v())=="" | as.character(hz_v())=="")){
      actionButton("login", 
                   icon = icon("unlock"),
                   style="color: #fff; background-color: '#337ab7'; border-color: #2e6da4",
                   width = "65px",
                   label = HTML("<span style='font-size:1.3em 'text-align:justify'><br />Login</span>")
      )
    }
    if(as.character(mode_v())=="" & as.character(db_v())!=""){dgree='#337ab7'}
    if(as.character(mode_v())!="" & as.character(db_v())!=""){dgree='#337ab7'}
  })
  
  
  # Load Geohydrology librarys ------------------------------------------------------------------------------
  message("Load Geohydrology librarys")
  
  output$modelUI=renderUI({
    # UI For registered user ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(as.character(mode_v()) %in% Password_df$Password){
      verticalLayout(
        useShinyalert(),
        selectInput(inputId="Background",label = msgactionBttn(infoId="Background_info",color="default",c_label="Subsurface Librarys: "),
                    multiple=T,
                    choices=c("National"="national",
                              "Coastal"="coastal",
                              "Westren Mt."="westren",
                              "Eastren Mt."="eastren",
                              "North Eastren Mt."="north_eastren",
                              "Shkhem-Tubas"="Shkhem_Tubas",
                              "Negev-Arava"="negev_arava",
                              "Arava-Sinai"="Arava_Sinai",
                              #"Western Galilee"="western_galilee",
                              "Golan-Kinneret"="golan_knneret",
                              "Hermon-Hula"="hermon_hula",
                              "Carmel"="carmel",
                              "Jerusalem"="jerusalem"),
                    selected = "national",)
      )}
    # UI For Public users ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else{
      verticalLayout(
        useShinyalert(),
        selectInput(inputId="Background",label = msgactionBttn(infoId="Background_info",color="warning",c_label="Subsurface libraries: "),
                    multiple=T,
                    choices=c("National"="national"),
                    selected = "national",))}
  }) # Close Mode
  # Load Base Map ------------------------------------------------------------------------------
  message("Load Base Map")
  output$mainmap <- renderLeaflet({
    leaflet() %>% 
      setView(lng=35.2,lat=32.55,zoom=10) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
  })
  
  # Get Wells DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data <- reactive({ 
    req(input$file1) 
    inFile=input$file1 
    df=fread(inFile$datapath,,
             colClasses=c(materials="character",formations="character",groups="character")) %>% 
      dplyr::group_by(name) %>% 
      mutate(max_depth=max(bot_layer,na.rm = T)) %>%  as.data.table(.)
    return(df)
  })
  
  # Load Wells -----------------------------------------------------------------------------
  observeEvent((input$login | !is.null(input$file1)), {
    # Clean exist data
    Virtual_dt<<-Virtual_int
    charts<<-NULL
    horizons_db<<-NULL
    {
      if(!is.null(input$file1)){
        message("Rander Wells Map")
        # Build Initial Wells map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Geology_Description_ss =data() %>% mutate(well_id=as.character(shd_id)) %>% 
          group_by(well_id) %>%
          mutate(max_depth=max(bot_layer,na.rm = T)) %>% 
          subset(.,,c("well_id","name","name_he","Longitude","Latitude","elv","max_depth",
                      "materials","formations","groups","top_layer","bot_layer","LEVEL_DES","LEVEL_DES_CD","AQUIFER_CD","SOURCE_DES"))%>% 
          as.data.table(.,key="well_id")
        
        CS_model_system = unique(Geology_Description_ss,by="well_id") 
        
        # Set Spatial layer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        if(input$country=="Indefinite" | max(CS_model_system$Longitude)<100){
          CS_model_system_sf = st_as_sf(CS_model_system, coords = c("Longitude", "Latitude"), crs =4326,remove=F)
        } else {
          crs_id=subset(localtiles_df,country==input$country,crs)
          CS_model_system_sf<-st_as_sf(CS_model_system,
                                       coords = c("Longitude", "Latitude"), crs =as.numeric(crs_id),remove=T) %>% 
            st_transform(.,crs =4326)
          ll_df=as_tibble(st_coordinates(CS_model_system_sf))
          CS_model_system_sf$Longitude=ll_df$X
          CS_model_system_sf$Latitude=ll_df$Y
          CS_model_system<-st_drop_geometry(CS_model_system_sf)
          Geology_Description_ss<-dplyr::select(Geology_Description_ss,-c("Longitude", "Latitude")) %>% 
            left_join(.,subset(CS_model_system,,c("well_id","Longitude","Latitude")))
          
          messeges_str="The coordinate system has been converted from a local projection to an international projection.
                        There may be a deviation of few meters of the wells location."
          showModal(modalDialog(
            title = "Projection seting: ",
            messeges_str,
            easyClose = TRUE,
            footer = NULL
          ))
          
        }
        # Set DB's to Environment ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        assign("Geology_Description_ss",Geology_Description_ss,envir = .GlobalEnv)
        assign("CS_model_system_sf",CS_model_system_sf,envir = .GlobalEnv)
        assign("CS_model_system",CS_model_system,envir = .GlobalEnv)
        
        # Get type of Geological grid ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
        if(as.character(geo_v())=="Regional-Low"){geogrid$resoid=200}
        
        output$mainmap <- renderLeaflet({
          loc=as.data.frame(st_coordinates(st_centroid(st_union(CS_model_system_sf))))
          
          # Load Initial Wells map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          viewdif=c(lng=loc$X+0.1,lat=loc$Y-0.4,zoom=10.2)
          if(input$country=="Indefinite"){
            wellsmap(CS_model_system,geogrid,transforms_shp,viewdif)
          } else {
            localtiles_dfs=subset(localtiles_df,country==input$country,)
            wellsmap_n(CS_model_system,geogrid,transforms_shp,viewdif,localtiles_dfs)
          }
          
        }) 
      }
    } 
    observeEvent(input$Background,{
      req(input$Background!="national")
      # Get Active Libraries Dimensions -------------------------------------------------
      library_dms_act=dplyr::filter(library_dms,basin %in% input$Background) %>% 
        dplyr::filter(.,basin != "national")
      proxy_mainmap=leafletProxy(
        mapId = "mainmap",
        session = session
      ) %>%
        clearGroup(group="libraris") %>% 
        addPolygons(data=library_dms_act,
                    color= "black",
                    fillColor= "gray",
                    label = NULL,
                    fill=F,
                    weight = 6,
                    fillOpacity =0.2,
                    smoothFactor = 3,
                    group="libraris") # %>%
      # addLabelOnlyMarkers(
      #   data=CS_model_system,
      #   labelOptions=labelOptions(textsize = "30px",
      #                             sticky=T),
      #   layerId=NULL,
      #   lat=~Latitude,
      #   lng=~Longitude,
      #   label=~name,
      #   group = "welnames")
      # 
    })
    
    # Show wells labeles in noHide mode ---------------------------------------------------
    observeEvent(input$mainmap_zoom,{
      req(isTRUE(exists("CS_model_system")))
      # print(input$mainmap_zoom)
      # print(paste0("zoom_old:",zoom_old))
      req((input$mainmap_zoom>=14.5 & zoom_old<14.5) | # Drill in
            (input$mainmap_zoom<14.5 & zoom_old>=14.5)   # Drill out
      )
      zoom_old<<-input$mainmap_zoom
      if (input$mainmap_zoom>=14.5){
        message("no Hide names")
        proxy_mainmap=leafletProxy(
          mapId = "mainmap",
          session = session
        ) %>%
          clearGroup(group="welnames") %>% 
          addLabelOnlyMarkers(
            data=CS_model_system,
            labelOptions=labelOptions(textsize = "15px",
                                      opacity=0.5,
                                      noHide = T,
                                      sticky=T),
            layerId=NULL,
            lat=~Latitude,
            lng=~Longitude,
            label=~name,
            group = "welnames")
      }
      if (input$mainmap_zoom<14.5){
        message("Back to normal view")
        proxy_mainmap=leafletProxy(
          mapId = "mainmap",
          session = session
        ) %>%
          clearGroup(group="welnames") %>% 
          addLabelOnlyMarkers(
            data=CS_model_system,
            labelOptions=labelOptions(textsize = "30px",
                                      noHide = F,
                                      sticky=T),
            layerId=NULL,
            lat=~Latitude,
            lng=~Longitude,
            label=~name,
            group = "welnames")
      }
      
    })
  }) # Close Login
  
  
  # Build Geology Selected Layer ------------------------------------------------------------------
  observeEvent(input$mainmap_draw_new_feature, {
    req(input$mainmap_draw_stop)
    
    feature_type <- input$mainmap_draw_new_feature$geometry$type
    # Select by Polygon ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(feature_type %in% c("Rectangle","Polygon")) {
      message("Select By Polygon")
      #get the coordinates of the polygon
      polygon_coordinates=input$mainmap_draw_new_feature$geometry$coordinates[[1]]
      
      #transform them to an sp Polygon
      drawn_polygon=Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      #drawn_polygon_sp=SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
      
      drawn_polygon_st=st_as_sf(SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))) %>% st_set_crs(.,4326)
      selected_gotiles=st_intersection(geogrid,drawn_polygon_st)
      
      # use intersect to identify selected items
      #selected_gotiles=raster::intersect(sf::as_Spatial(st_zm(geogrid, drop = TRUE, what = "ZM")),drawn_polygon_sp) # st_intersection(geogrid,st_as_sf(drawn_polygon_sp)) #
      # Check the selection 
      maxtiles=ifelse(input$geomap=="Regional-Low",300,100)
      if(nrow(selected_gotiles)<maxtiles){
        # Define selected tiles
        if(input$geomap == "Regional-Low"){
          selected_gotiles$resoid=200
          geology_200_fltr=dplyr::filter(geology_200,geoid %in% selected_gotiles$geoid)
          geology_50_fltr=geology_50[0,]
        } else {
          selected_gotiles_200=dplyr::filter(selected_gotiles,resoid==200)$geoid
          geology_200_fltr=dplyr::filter(geology_200,geoid %in% dplyr::filter(selected_gotiles,resoid==200)$geoid)
          geology_50_fltr=dplyr::filter(geology_50,geoid %in% dplyr::filter(selected_gotiles,resoid==50)$geoid)
        }
        
        # Build Proxy Layers for empty selection
        if(NROW(geology_200_fltr)==0){geology_200_fltr=geology_200_proxy}
        if(NROW(geology_50_fltr)==0){geology_50_fltr=geology_50_proxy}
        
        comm_cols=c("Name_Eng","Code","geometry","resoid")
        geology_map_act<<-bind_rows(subset(geology_200_fltr,,comm_cols),subset(geology_50_fltr,,comm_cols))
        
        # Set base proxy map
        proxy_basemap=leafletProxy(
          mapId = "mainmap",
          session = session
        ) 
        
        proxy_mainmap=proxy_basemap %>% 
          clearGroup(group="geology_tiles") %>% 
          addPolygons(data=geology_map_act,
                      color= ~geopal(Code),
                      fillColor= ~geopal(Code),
                      label = ~Name_Eng,
                      fill = T,
                      weight = 0,
                      fillOpacity = 0.4,
                      smoothFactor = 1,
                      group="geology_tiles") %>%
          clearGroup(group="welnames") %>% 
          addLabelOnlyMarkers(
            data=CS_model_system,
            labelOptions=labelOptions(textsize = "15px",
                                      opacity=0.5,
                                      noHide = F,
                                      sticky=T),
            layerId=NULL,
            lat=~Latitude,
            lng=~Longitude,
            label=~name,
            group = "welnames") 
      } else {
        showModal(
          modalDialog(
            title = "There seems to be some issues with your selection: ",
            "Error: You have selected too many geological tiles, select up to 81 ''Regional-Low'' tiles or up to 10 ''Local-high'' tiles",
            easyClose = T,
            size="l",
            footer = NULL
          )
        )  
      }
    }
    
    
  })
  
  
  # Additional layers ----------------------------------------------------------------------
  # Set ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$additional_layers_type,{
    req(str_count(input$additional_layers_type)>1) 
    msg_s=as.character(subset(additional_layers_df,type==input$additional_layers_type,mssg))
    showModal(
      modalDialog(
        title = "Layer settings: ",
        msg_s,
        easyClose = T,
        size="l",
        footer = NULL
      )
    )
  })
  # Load ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$additional_layers,{
    # Get files
    req(input$additional_layers)
    req(str_count(input$additional_layers_type)>1) 
    inFile=input$additional_layers
    # Set base proxy map
    proxy_basemap=leafletProxy(
      mapId = "mainmap",
      session = session
    )
    # Digital Terrain Model (raster)
    if(inFile$type=="image/tiff" & input$additional_layers_type=="Digital Terrain Model (raster)"){
      DTM_rst=raster(inFile$datapath)
      if(as.character(crs(DTM_rst))!="+proj=longlat +datum=WGS84 +no_defs"){
        DTM_rst=projectRaster(DTM_rst, crs = 4326)
      }
      additional_layers_lst$DTM_rst=DTM_rst
      additional_layers_lst<<-additional_layers_lst
    } 
    # Geological layer (raster)
    else if (inFile$type=="image/tiff" & input$additional_layers_type=="Geological layer (raster)"){
      DEM_rst=raster(inFile$datapath)
      if(as.character(crs(DEM_rst))!="+proj=longlat +datum=WGS84 +no_defs"){
        DEM_rst=projectRaster(DTM_rst, crs = 4326)
      }
      names(DEM_rst)="user_geology_layer"
      additional_layers_lst$DEM_rst=DEM_rst
      additional_layers_lst<<-additional_layers_lst
    }
    # Digital Terrain Model (contours)
    else if (any(str_detect(inFile$name,".shp")) & input$additional_layers_type=="Digital Terrain Model (contours)") {
      shp_path <- reactive({input$additional_layers})
      user_shp <- Read_Shapefile(shp_path)
      user_shp=user_shp() %>% st_transform(.,crs=4326)
      colnames(user_shp)<-c("elv","geometry")
      proxy_mainmap=add_element(main_map=proxy_basemap,
                                ad_lyr=user_shp,
                                type=input$additional_layers_type)
    } else {
      # Inactive options
      showModal(
        modalDialog(
          title = "Layer settings: ",
          "Inactive",
          easyClose = T,
          size="l",
          footer = NULL
        )
      )
    }
  })
  
  # Select Wells & Faluts List  ------------------------------------------------------------
  data_slc=reactive({
    message("Select Wells & Faluts List")
    # use the draw_stop event to detect when users finished drawing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    req(input$mainmap_draw_stop)
    feature_type <- input$mainmap_draw_stop$shape
    
    # Select By Line ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(feature_type=="Line") {
      message("Select By Line")
      # Coordinates 2 polyline
      print("Coordinates 2 polyline")
      polyline_coordinates <- input$mainmap_draw_new_feature$geometry$coordinates
      polyline_coordinates_mt <- Reduce(rbind,polyline_coordinates)
      drawn_polyline_st= st_as_sf(as.data.frame(polyline_coordinates_mt),
                                  coords = c("V1", "V2"), crs = 4326,remove=T) %>% 
        rownames_to_column(.) %>% dplyr::arrange(desc(rowname)) %>%
        dplyr::select(-rowname) %>%
        dplyr::summarise(do_union=F) %>%
        sf::st_cast("LINESTRING")
      
      # Selected wells
      print("Selected wells")
      drawn_polygon = st_buffer(drawn_polyline_st, dist=input$Buffer*1000)
      CS_wells_coordinates=st_as_sf(CS_model_system,coords = c("Longitude","Latitude"),crs=4326)
      CS_model_system_slc=setDF(st_drop_geometry(st_intersection(CS_wells_coordinates,drawn_polygon)))
      Geology_Descriptions_slc=Geology_Description_ss[Geology_Description_ss$well_id %in% CS_model_system_slc$well_id, ]  
      
      # selected Faults
      transforms_st=st_as_sf(transforms_shp) ;st_crs(transforms_st)=4326
      junc_points=(st_intersection(drawn_polyline_st,st_crop(transforms_st,drawn_polyline_st)))
      if(nrow(junc_points)>0){
        # Split fault with several meeting points with the CS line       
        for (i in 1:nrow(junc_points)){
          junc_points$type[i]=class(junc_points$geometry[i])
        }
        junc_points_multi=dplyr::filter(junc_points,type=="sfc_MULTIPOINT")
        junc_points_pnt=dplyr::filter(junc_points,type!="sfc_MULTIPOINT")
        if(nrow(junc_points_multi)>0){
          junc_points_pnt2=st_cast(junc_points_multi,"POINT")
          junc_points_all=bind_rows(junc_points_pnt,junc_points_pnt2)
        } else {
          junc_points_all=junc_points_pnt
        }
        transforms_fltr=as.data.frame(st_coordinates(junc_points_all))[,1:2] 
      } else{
        transforms_fltr=transforms_st[0,]
      }
      if(nrow(transforms_fltr)>0){
        colnames(transforms_fltr)=  c("Longitude", "Latitude")
      } else{print("No active Faults")}
      
      if(tor=="t"){
        # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        write.csv(Geology_Descriptions_slc, paste0(Background_path,'/Apps/External_Data/Geology_Descriptions_slc.csv'))  #  Local Test file
        write.csv(transforms_fltr, paste0(Background_path,'/Apps/External_Data/transforms.csv'),row.names = F)
        st_write(drawn_polyline_st, paste0(Background_path,'/Apps/External_Data/drawn_polyline.csv'),layer_options = "GEOMETRY=AS_WKT",delete_dsn=TRUE)
        # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      }
      
      # print to Tab the name of the CS_wells
      print("print to Tab the name of the CS_wells")
      CS_model_system_ss=subset(CS_model_system_slc,,c("name","max_depth","LEVEL_DES","SOURCE_DES","AQUIFER_CD"))
      colnames(CS_model_system_ss)=c("Name","Depth [m]","Descriptions","Source","Aquifer")
      
      if(nrow(CS_model_system_ss)<2 & NROW(Virtual_dt)==1){
        showModal(
          modalDialog(
            title = "Wrong point selection: ",
            "Error: It is not possible to run the cross-section since less than two wells or virtual points have been selected along the cross section.",
            easyClose = T,
            size="l",
            footer = NULL
          )
        )
        modeldialog_status="Active"
      }
      # Populate Results List
      tablet_filter<<-"inactive"
      print("Populate Results List")
      slc_lst=list("Geology_Descriptions_slc"=Geology_Descriptions_slc,"transforms_fltr"=transforms_fltr,"CS_model_system_ss"=CS_model_system_ss,"drawn_polyline"=drawn_polyline_st)
      return(slc_lst)
    }
  })
  # Render Wells Data Table ----------------------------------------------------------------
  message("Render Wells Data Table")
  output$CS_model_system <- renderDT(
    data_slc()[["CS_model_system_ss"]],
    filter = "top"
  )
  
  observeEvent(input$CS_model_system_search_columns,{
    req(!is.null(input$CS_model_system_rows_current))
    tablet_filter<<-"active"
  })
  
  # Create Virtual Wells ----------------------------------------------------------------
  Virtual_cre=observeEvent(input$mainmap_draw_new_feature,{
    req(input$mainmap_draw_stop)
    feature_type <- input$mainmap_draw_new_feature$geometry$type
    
    if(feature_type=="Point") {
      message("Create Virtual Wells")
      # Build Virtual point
      Virtual_dt_i <-tibble(Longitude=as.numeric(input$mainmap_draw_new_feature$geometry$coordinates[1]),
                            Latitude=as.numeric(input$mainmap_draw_new_feature$geometry$coordinates[2])) %>%
        mutate(well_id=as.character(-999+nrow(Virtual_dt)),
               name=paste0("Virtual Point - ",nrow(Virtual_dt)),
               name_he=paste0("Virtual Point - ",nrow(Virtual_dt)),
               materials=-999,
               formations=-999,
               groups=-999,
               top_layer=0,
               bot_layer=50,
               max_depth=999,
               SOURCE_DES=NA,
               LEVEL_DES=NA,
               AQUIFER_CD=NA)
      
      # Get markers elevation
      dem_pth = "data/DEMs"
      DTM_rst=raster(paste0(dem_pth,"/DTM.tif"))
      Virtual_sp_i = SpatialPointsDataFrame(coords = subset(Virtual_dt_i,,c(Longitude,Latitude)), data = Virtual_dt_i,proj4string = CRS("+proj=longlat +datum=WGS84"))
      elevations = raster::extract(DTM_rst,Virtual_sp_i,cellnumbers=TRUE,sp=TRUE,along=T)
      Virtual_sp_i$elv=elevations$DTM
      # Load to Virtual data frame
      Virtual_dt<<-rbind(Virtual_dt,subset(as.data.table(Virtual_sp_i),,names(Virtual_dt)))
      print(Virtual_dt)
      return(Virtual_dt)
    }
  })
  
  # Run Cross Section Model ####################################################
  observeEvent(input$Get,
               ## Preprocess ===================================================
               {if(!is.null(input$file1) & is.null(input$file2)){
                 {
                   ### Get Well DT & CS line -----------------------------------
                   Geology_Descriptions_slc=data_slc()[["Geology_Descriptions_slc"]]
                   drawn_polyline=data_slc()[["drawn_polyline"]]
                   message("Rander Cross Section Model")
                   ### Inputs Test ---------------------------------------------
                   messeges_lst=list();i_msg=1
                   if(is.null(input$mainmap_draw_stop)){
                     messeges_lst[[i_msg]]="Error 1: No line marked along the map.";i_msg=i_msg+1
                   }
                   else {
                     if(NROW(Geology_Descriptions_slc)==0 & NROW(Virtual_dt)==1 ){
                       messeges_lst[[i_msg]]="Error 2: No wells or Virtual points were selected for the Cross Section.";i_msg=i_msg+1
                     }
                     else if (all(str_detect(Geology_Descriptions_slc$LEVEL_DES,"Lithology")==T) && input$CS_type!="materials"  & NROW(Virtual_dt)==1){
                       messeges_lst[[i_msg]]="Error 3: Cross Section type does not match the type of the selected wells.";i_msg=i_msg+1
                     }
                     else if (length(input$Background)>1 && input$Build_Solids==T){
                       messeges_lst[[i_msg]]="Warning 1: You have selected more than two libraries and choose ''solid build'' option, this may create distortions in the Cross Section view.";i_msg=i_msg+1}
                   }
                   if(!is.null(charts)) {
                     if(charts$cs_data$cs_id==input$cs_id){
                       messeges_lst[[i_msg]]="Warning 2: Cross section with this ID already exists in the system. It is not possible to build a geological model with the help of the cross section.";i_msg=i_msg+1
                       cln_tmprl()
                       dlt_optn="inactive"
                       rm(charts)
                     }
                   }
                   
                   if(length(messeges_lst)>0 & modeldialog_status=="Inactive"){
                     messeges_str=as.character(unlist(messeges_lst))
                     showModal(modalDialog(
                       title = "There seems to be some issues with the data you entered: ",
                       messeges_str,
                       easyClose = TRUE,
                       footer = NULL
                     ))
                   }
                   ### Filter by manual selecting ------------------------------
                   if(length(input$CS_model_system_rows_current)<length(table(Geology_Descriptions_slc$well_id)) & tablet_filter=="active"){
                     wells_md_fltr=as.data.frame(data_slc()[["CS_model_system_ss"]])[input$CS_model_system_rows_current,]
                     Geology_Descriptions_slc=dplyr::filter(Geology_Descriptions_slc,name %in% wells_md_fltr$Name,)
                   }
                   if(nrow(Virtual_dt)>1) {
                     Geology_Descriptions_slc=bind_rows(Geology_Descriptions_slc,Virtual_dt[2:nrow(Virtual_dt),])
                     rm(Virtual_dt)
                   }
                   ### Progress Bar Initiation ---------------------------------
                   withProgress(message = 'Cross Section building in progress',
                                detail = 'This may take a while...', value = 0, min=0,max=360,
                                expr = {
                                  incProgress(1/360)
                                  ###  Load Parameters -------------------------
                                  message("Load Parameters")
                                  CS_Prameters<-list(
                                    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MODEL PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                    drawn_polyline=drawn_polyline,
                                    Geology_Descriptions_slc=Geology_Descriptions_slc,
                                    transforms_st=data_slc()[["transforms_fltr"]],
                                    title_CS=input$title_CS,
                                    cs_id=input$cs_id,
                                    CS_type=input$CS_type,
                                    season=input$season,
                                    measurement_year=input$measurement_year,
                                    Projection_Line=input$Projection_Line,
                                    Buffer=input$Buffer,
                                    label_size=input$label_size,
                                    Interval_between_labeles=input$Interval_between_labeles,
                                    Lower_Limit=input$Lower_Limit,
                                    Surface_Smooth=input$Surface_Smooth,
                                    Resolution=input$Resolution,
                                    vertical_resolution=input$vertical_resolution,
                                    DEM_line_thickness=input$DEM_line_thickness,
                                    Background=input$Background,
                                    Build_Solids=input$Build_Solids,
                                    Virtual_Wells=input$Virtual_Wells,
                                    geological_cs_surf=input$geological_cs_surf,
                                    country=input$country,
                                    geology_200=geology_200,
                                    geology_50=geology_50,
                                    additional_layers_lst=additional_layers_lst
                                    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MODEL PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                  )
                                  
                                  ## Run CS Model ==============================
                                  message("Run CS Model")
                                  withCallingHandlers({
                                    shinyjs::html("cs_messages", "")
                                    charts<<-cs_model(in_param=CS_Prameters)
                                    # Directly Sub products
                                    initial_view<<-"active"
                                    current_line<<-st_as_sf(charts$cs_data$CS_line) %>% st_set_crs(4326)
                                    cln_tmprl()
                                  },
                                  message = function(m) {
                                    shinyjs::html(id = "cs_messages", html = m$message, add = F)
                                  }
                                  )
                                  ### Render Preview Visualization -------------
                                  output$cs_chart <- renderPlot({
                                    req(charts$cs_preview)
                                    charts$cs_preview
                                  })
                                }) # End Progress Bar
                   ## Download Cross Sections ==================================
                   output$cs_download_ppt <-  downloadHandler(
                     message("Download Cross Sections"),
                     filename = function() { 
                       paste("Editable_CS-", Sys.Date(), ".pptx", sep="")
                     },
                     content = function(file) {
                       print(charts$cs_ppt, target = file)
                     })
                   output$cs_download_html <-  downloadHandler(
                     message("Download Cross Sections"),
                     filename = function() { 
                       paste("Interactive_CS-", Sys.Date(), ".html", sep=""
                       )
                     },
                     content = function(file) {
                       htmlwidgets::saveWidget(charts$cs_html,selfcontained = T, file=file)
                     })  
                   ### Update Cross section ID ---------------------------------
                   cs_id_i<<-cs_id_i+1
                   updateSelectInput(session,
                                     inputId="cs_id",
                                     selected=cs_ids$cs_id[cs_id_i],
                                     choices = cs_ids$cs_id
                   )
                   
                 }  # End Cross Section
               }
               }) # End of Cross Section Rendering
  
  # Geology Horizons System ####################################################
  observeEvent(input$tabs,{
    req(input$tabs == "Build Horizons")
    message("Geology Horizons System - Active")
    ## Active Elements =========================================================
    observeEvent(input$cs_id,{
      dlt_optn="active"
    })
    
    ### Update Selection list --------------------------------------------------
    observeEvent({input$tabs
      input$Select_horizon_by},{
        req(input$tabs == "Build Horizons")
        req(is.null(charts)!=T)
        message("Update Selection list")
        
        if(input$Select_horizon_by=="Wells"){
          cs_horizons<<-dplyr::distinct(charts$cs_data$wells_plot_df,f_name,.keep_all = T)
          cols_classf<<-ColourExpreation()
        }
        if(input$Select_horizon_by=="Hydrogeology libraries"){
          cs_horizons<<-dplyr::distinct(charts$cs_data$DEM_plot_df,f_name,.keep_all = T)
          cols_classf<<-ColourExpreation_DEM()
        }
        if (is.null(cs_horizons)){cs_horizons <- character(0)}
        
        updateSelectInput(session,
                          inputId="Select_horizon",
                          label = "Select horizon:",
                          choices = as.character(cs_horizons$f_name)
        )
      })
    
    ## Load initial View =======================================================
    ### Editable Cross Section -------------------------------------------------
    if(is.null(cs_tagging)==T){
      output$cs_tagging<-renderPlot({
        req(charts$cs_raw)
        req(initial_view=="active")
        initial_view="done"
        if(!is.null(horizons_db)){
          # Get Junction Points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          message("Get Junction Points")
          nodes_links_df=nodes_linker(current_line,lines_db,horizons_db,charts)
          # Render Initial CS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          message("Render Initial CS")
          
          if (nrow(nodes_links_df)>0){
            cs_tagging<<-charts$cs_raw+
              new_scale_color()+
              geom_label(data=nodes_links_df,aes(x=Distance,y=Elevation,color=Horizon,size=5,label=ID))+
              scale_colour_manual(values=ColourExpreation_DEM())
            cs_tagging  
          } else{
            message("There is no Junction Points")
            cs_tagging<<-charts$cs_raw
            cs_tagging
          }
        } else {
          cs_tagging<<-charts$cs_raw
          cs_tagging}
      })
    }
    # Close Initial view option until next run ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(initial_view=="done"){initial_view="inactive"}
    ### Set Initial Table ------------------------------------------------------
    if(!is.null(tab_raw)==T){tab_raw=NULL}
    message("Set Initial Table")
    clickposition_history <- reactiveVal(data.frame(Distance = numeric(),Longitude=numeric(),
                                                    Latitude=numeric(),Range=numeric(),
                                                    Elevation = numeric(),Horizon=character(),
                                                    Segment=numeric(),method=character(),ID=character()))
    #### Add selected box Column ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(nrow(clickposition_history())==0){
      output$info = DT::renderDataTable({
        tab_raw <<- clickposition_history()
        slc_tbl<-DT::datatable(tab_raw,
                               class="cell-border",
                               selection='none',escape=F)
        slc_tbl
      }) 
    }
  }) # End of Load initial view
  
  ## Build Horizons ==========================================================
  ### Set Segments -----------------------------------------------------------
  SegmentsListen <- reactive({list(input$plop,input$Select_horizon)})
  observeEvent(input$plop,{segment_id<<-segment_id+1}) # Right click
  observeEvent(input$Select_horizon,{segment_id<<-segment_id+1}) # New horizon
  
  observeEvent(input$plot_click, {
    print(tab_raw)
    message("Build Horizons")
    new_point_ll=horizonewpnt(
      Select_horizon=input$Select_horizon,
      cs_horizons=cs_horizons,
      plot_click=input$plot_click,
      DEM_plot_df=charts$cs_data$DEM_plot_df
    )
    new_point_ll$ID=paste0(as.character(charts$cs_data$cs_id),as.character(charts$cs_data$cs_id),"'")
    new_point_ll$Segment=segment_id
    ### Check point class ----------------------------------------------------
    if(nrow(tab_raw)>0){
      message("Check point class")
      tab_check=tab_raw %>%
        mutate(int_dist=((Distance-new_point_ll$Distance)^2+(Elevation-new_point_ll$Elevation)^2)^0.5,
               pnt2erase=ifelse(int_dist<input$Eraser_sensitivity & Horizon==input$Select_horizon,1,0)) %>% 
        dplyr::filter(.,pnt2erase==0)
      
      if (nrow(tab_check)<nrow(tab_raw)) { # nrow(tab_raw)>0
        if(dlt_optn=="active"){
          message("Delete Point") # Delete Point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          tab_raw<<-dplyr::distinct(dplyr::select(tab_check,-int_dist,-pnt2erase),Distance,Elevation,ID,.keep_all = T) %>% arrange(desc(Distance))
        }
      } else {
        message("Add Point") # Join to Current DB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tab_raw<<-dplyr::distinct(rbind(tab_raw, new_point_ll),Distance,Elevation,ID,.keep_all = T) %>% arrange(desc(Distance))
        dlt_optn="active"
      }
    } else { # New Point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      message("New Point")
      tab_raw<<-new_point_ll
    }
    ### Connect between points -----------------------------------------------
    if(input$h_int!="NaN" & nrow(tab_raw)>1){
      message("Connect between points")
      # Fill horizons by group 
      horizons2fill=as.data.frame(t(table(tab_raw$Horizon))) %>% rename_all(~(c("ID","Horizon","n")))
      tab_lst=list()
      for (i in 1:nrow(horizons2fill)){
        n_horizon=horizons2fill$n[i]
        act_horizon=filter(tab_raw,Horizon==horizons2fill$Horizon[i]) 
        if (n_horizon>1) {
          tab_lst[[i]]=fill_horizons(
            tab_raw=act_horizon,
            res=input$h_res,
            int_mathos=input$h_int
          )
        }else{
          tab_lst[[i]]=subset(act_horizon,,c("Elevation","Distance","Horizon","Segment","method","ID"))
        }
      }
      tab<<-Reduce(rbind,tab_lst)  
    } else{
      message("Do not connect between points")
      tab<<-subset(tab_raw,,c("Elevation","Distance","Horizon","Segment","method","ID"))
    }
    
    if(tor=="t"){
      # %%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      write.csv(tab_raw, paste0(Background_path,'/Apps/External_Data/tab_raw.csv'))  #  Local Test file
      write.csv(charts$cs_data$DEM_plot_df, paste0(Background_path,'/Apps/External_Data/cs_pnts_dt.csv'))  #  Local Test file
      write.csv(charts$cs_data$wells_plot_df, paste0(Background_path,'/Apps/External_Data/wells_plot_df.csv'))  #  Local Test file
      #%%%%%%%%%%%%%%%%%%%%%%%%% Test Elements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    }
    
    ### Render Cross Section & and table -------------------------------------
    cols_DEMs=ColourExpreation_DEM()
    output$cs_tagging <- renderPlot({
      req(cs_tagging)
      message("Render Cross Section & and table")
      
      tab_view=tab_raw
      if(input$h_int!="NaN" & nrow(tab_raw)>1){
        common_cols= c("Elevation","Distance","Horizon","Segment","method","ID")
        tab_view=rbind(subset(tab_raw,,common_cols),subset(tab,,common_cols))
      }
      cs_tagging+
        new_scale_fill() +
        geom_point(data=tab_view,
                   shape = 21,
                   colour = "black",
                   stroke = 0,
                   aes(x=Distance,y=Elevation,fill=Horizon,
                       size=ifelse(method=="manual",as.numeric(input$hpoint_size),
                                   0.5*as.numeric(input$hpoint_size))
                   )) +
        scale_fill_manual(values=cols_classf)
    })
    ### Render Table ---------------------------------------------------------
    # output$info = DT::renderDataTable({
    #   message("Render Table")
    #   tab_tbl = dplyr::filter(tab_raw,Horizon==input$Select_horizon)
    #   slc_tbl<-DT::datatable(tab_tbl,
    #                          class="cell-border",
    #                          selection='none',escape=F)  
    #   slc_tbl
    # })  
  })
  
  ## Export Products =========================================================
  ### Set Download button ----------------------------------------------------
  output$download_horizon <-downloadHandler(
    message("Download Horizons"),
    filename = function() { 
      paste(out_h_nme,paste0(as.character(charts$cs_data$cs_id),as.character(charts$cs_data$cs_id),"'-"), Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(horizons_db,file, row.names = F)
    }
  )
  
  
  ### Add to Data Base -------------------------------------------------------
  observeEvent(input$add2hdb,{
    req(nrow(tab_raw)>0)
    message("Add to Data Base")
    add_lst=add_horizons(tab,
                         DEM_plot_df=charts$cs_data$DEM_plot_df,
                         wells=charts$cs_data$wells_plot_df,
                         manual_pnt=tab_raw[ID==paste0(as.character(charts$cs_data$cs_id),as.character(charts$cs_data$cs_id),"'"),],
                         cs_id=charts$cs_data$cs_id,
                         horizons,
                         horizons_db,
                         current_line,
                         lines_db,
                         tor=tor) 
    horizons_db<<-dplyr::distinct(add_lst$horizons_db,ID,Distance,Horizon,.keep_all = TRUE)
    lines_db<<-add_lst$lines_db
  })
  
  observeEvent(input$repair,{
    ### FIX Ranges -----------------------------------------------------------
    req(input$h_int!="NaN" & nrow(tab_raw)>1)
    req(nrow(charts$cs_data$DEM_plot_df)>0)
    req(nrow(tab)>0)
    message("FIX Ranges")
    fix_lst=fix_horizons(tab,
                         DEM_plot_df=charts$cs_data$DEM_plot_df,
                         cs_id=charts$cs_data$cs_id,
                         horizons,
                         horizons_db,
                         current_line,
                         lines_db,
                         tor=tor)
    horizons_db<<-dplyr::distinct(fix_lst$horizons_db,ID,Distance,Horizon,.keep_all = TRUE)
    lines_db<<-fix_lst$lines_db
  })  
  
  
  # Geology Model System #######################################################  
  ## Load External DB ==========================================================
  ### Set reactive connection --------------------------------------------------
  horizons_rct <- reactive({ 
    req(input$csgrid) 
    csFile=input$csgrid 
    df=fread(csFile$datapath,
             colClasses=c(Distance="numeric",	Longitude="numeric",	Latitude="numeric",	Elevation="numeric",
                          Horizon="character",	method="character",	ID="character")) %>% 
      dplyr::distinct(.,ID,Distance,Horizon,.keep_all = TRUE)
    return(df)
  })
  
  observeEvent(input$csgrid,{
    req(horizons_rct())
    ### Build Initial DBs ------------------------------------------------------
    horizons_db<<-horizons_rct() %>% dplyr::group_by(ID) #  horizons_rct()
    lines_db<<-st_as_sf(horizons_db, coords = c("Longitude", "Latitude"), crs = 4326,remove=T) %>% 
      mutate(cs_id=str_sub(ID,1,1)) %>% subset(.,,c("cs_id","Distance","Elevation","geometry")) %>% 
      group_by(cs_id)  %>%
      dplyr::arrange(cs_id,Distance) %>% 
      dplyr::summarise(do_union=F) %>%
      sf::st_cast("LINESTRING")
    
    ### Classifier - Update Cross section ID -----------------------------------
    cs_id_sct= cs_ids %>%  mutate(n=row_number()) %>% 
      dplyr::filter(.,cs_id==lines_db$cs_id[nrow(lines_db)])
    cs_id_i<<-cs_id_sct$n+1
    updateSelectInput(session,
                      inputId="cs_id",
                      selected=cs_ids$cs_id[cs_id_i],
                      choices = cs_ids$cs_id
    )
    
    ### Map - Change base proxy map --------------------------------------------
    proxy_basemap=leafletProxy(
      mapId = "mainmap",
      session = session
    ) 
    proxy_mainmap=proxy_basemap %>% 
      clearGroup(group="lines_db") %>% 
      addPolylines(data=lines_db,
                   color="blue",
                   label = ~cs_id,
                   fill = F,
                   weight = 2,
                   opacity = 0.4,
                   smoothFactor = 1,
                   group="lines_db")
    
    ### Update Model Builder Elements ------------------------------------------
    cs_id_sct= cs_ids %>%  mutate(n=row_number()) %>% 
      dplyr::filter(.,cs_id==lines_db$cs_id[nrow(lines_db)])
    cs_id_i<<-cs_id_sct$n+1
    updateSelectInput(session,
                      inputId="cs_id",
                      selected=cs_ids$cs_id[cs_id_i],
                      choices = cs_ids$cs_id
    )  
  })    
  
  ## Set Interpolation Algorithm ===============================================
  algorithm_v=reactive({input$interpolation_algorithm})
  output$algosUI=renderUI({
    if(as.character(algorithm_v())=="Kriging"){
      column(width = 9, style = "background-color:#2c3e50; opacity: 0.8;",
             # Model Builder - Kriging model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             selectInput("kriging_mdl", msgactionBttn(infoId="kriging_mdl_info",color="primary",c_label="Kriging model:"),
                         multiple=F,
                         choices=c("spherical","exponential","gaussian"),
                         selected="spherical"),
             # Model Builder - Kriging pixels ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             numericInput("kriging_pxl", msgactionBttn(infoId="kriging_pxl_info",color="primary",c_label="Pixels:"),
                          min = 100, max = 1000, value = 300,step=50),
             # Model Builder - Kriging lags ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             numericInput("kriging_lags", msgactionBttn(infoId="kriging_lags_info",color="primary",c_label="Lags:"),
                          min = 1, max = 10, value = 3,step=1)
      )
    } else if (as.character(algorithm_v())=="Neural Networks") {
      column(width = 9, style = "background-color:#2c3e50; opacity: 0.8;",
             # Model Builder - Neural Networks, range ~~~~~~~~~~~~~~~~~~~~~~~~~~
             numericRangeInput("layers_rng", msgactionBttn(infoId="layers_rng_info",color="primary",c_label="Layers Range:"),
                               min = 0, max = 1000, value =  c(50, 300),step=50),
             # Model Builder - Neural Networks, number ~~~~~~~~~~~~~~~~~~~~~~~~~
             numericInput("layers_n", msgactionBttn(infoId="layers_n_info",color="primary",c_label="Number of layers:"),
                          min = 1, max = 10, value = 4,step=1)
      )
    } else if (as.character(algorithm_v())=="Random Forests") {
      column(width = 9, style = "background-color:#2c3e50; opacity: 0.8;",
             # Model Builder - Random Forests, normalize ~~~~~~~~~~~~~~~~~~~~~~~
             checkboxInput("rf_normalize",  msgactionBttn(infoId="rf_normalize_info",color="primary",c_label="normalize:"),
                           value = FALSE, width = NULL),
             # Model Builder - Random Forests, trees ~~~~~~~~~~~~~~~~~~~~~~~~~~~
             numericInput("trees_n", msgactionBttn(infoId="trees_n_info",color="primary",c_label="Number of trees:"),
                          min = 100, max = 3000, value = 1000,step=100),
             # Model Builder - Random Forests, mtry ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             numericInput("mtry", msgactionBttn(infoId="mtry_info",color="primary",c_label="mtry:"),
                          min = 10, max = 1000, value = 100,step=10)
      )
    } else if (as.character(algorithm_v())=="Support Vector Machine") {
      column(width = 9, style = "background-color:#2c3e50; opacity: 0.8;",
             # Model Builder - Support Vector Machine ,type ~~~~~~~~~~~~~~~~~~~~
             selectInput("svm_typ", msgactionBttn(infoId="svm_typ_info",color="primary",c_label="SVM type:"),
                         multiple=F,
                         choices=c("eps-svr","nu-svr","eps-bsvr"),
                         selected="eps-svr"),
             # Model Builder - Support Vector Machine, kernel ~~~~~~~~~~~~~~~~~~
             selectInput("kernel", msgactionBttn(infoId="kernel_info",color="primary",c_label="Kernel:"),
                         multiple=F,
                         choices=c("polydot","rbfdot","tanhdot","laplacedot","besseldot"),
                         selected="polydot"),
             # Model Builder - Support Vector Machine, C value ~~~~~~~~~~~~~~~~~
             numericInput("svmc_v", msgactionBttn(infoId="svmc_v_info",color="primary",c_label="C:"),
                          min = 1, max = 50, value = 25,step=1)
      )
      
    }
  })
  
  ### Check grid resolution to WGS84 ---------------------------------------------
  
  observeEvent(input$grid_res,{
    if (algorithm_v()=="Kriging" & input$grid_res<1000){
      showModal(
        modalDialog(
          title = "Interpolation warning: ",
          "Warning: The interpolation resolution is too high to use the kriginig method. Please select an alternative method or 
          chack thet there is cross sections near to the eadges of the unit.",
          easyClose = T,
          size="l",
          footer = NULL
        )
      ) 
    }
    
  })
  
  
  ## Update Tab Elements =======================================================
  observeEvent(input$tabs,{
    req(input$tabs == "Build Geology Model")
    message("update Tab Elements")  
    # Horizon unit 
    if(!is.null(horizons_db)==T){
      horizon_unit_ids= horizons_db %>%  dplyr::distinct(Horizon,.keep_all = F)
      updateSelectInput(session,
                        inputId="horizon_unit",
                        selected=horizon_unit_ids$Horizon[1],
                        choices = horizon_unit_ids$Horizon
      )
    }
    
    ### Model Builder - Update Cross section ID --------------------------------
    if(!is.null(horizons_db)==T){
      notincluded_ids=dplyr::distinct(horizons_db,ID)
      
      cs_id_sct= cs_ids %>%  mutate(n=row_number()) %>% 
        dplyr::filter(.,cs_id==lines_db$cs_id[nrow(lines_db)])
      cs_id_i<<-cs_id_sct$n+1
      updateSelectInput(session,
                        inputId="notincluded",
                        selected=NULL,
                        choices =notincluded_ids$ID
      )
    }
    ### surface Geology --------------------------------------------------------
    if(!is.null(geology_map_act)==T){
      sf::sf_use_s2(F)
      geology_map_ids=st_drop_geometry(geology_map_act) %>%   dplyr::distinct(Name_Eng,.keep_all = F)
      sf::sf_use_s2(T)
      updateSelectInput(session,
                        inputId="surface_unit",
                        selected=geology_map_ids$Name_Eng[1],
                        choices = geology_map_ids$Name_Eng
      )  
    }
    
    ### Observation Points -----------------------------------------------------
    if(!is.null(Geology_Description_ss)==T){
      
      INDEX = as.data.frame(read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),sheet = "Index")) %>% 
        #dplyr::filter(type==input$CS_type) %>% 
        dplyr::rename("{input$CS_type}":=f_ID)
      Geology_Description_trg<<- Geology_Description_ss %>% 
        subset(.,,c(input$CS_type)) %>% 
        dplyr::distinct(groups,.keep_all = F) %>% 
        left_join(.,subset(INDEX,,c(input$CS_type,"f_name")))
      
      
      updateSelectInput(session,
                        inputId="observation_points",
                        selected=Geology_Description_trg$f_name[1],
                        choices = Geology_Description_trg$f_name
      )
      obs_notincluded_c=subset(Geology_Description_ss,,c("name")) %>% dplyr::arrange(name) 
      updateSelectInput(session,
                        inputId="obs_notincluded",
                        selected = NULL,
                        choices = obs_notincluded_c
      )
    }
    
    observeEvent(input$obs_inclod,{
      req(input$obs_inclod==T)
      req(!is.null(Geology_Description_ss)==T)
      messeges_str="You have chosen to include the points in the interpolation directly and not through the cross-sections.
      This can cause distortions in the results and a longer duration of the interpolation process."
      showModal(modalDialog(
        title = "Data warning: ",
        messeges_str,
        easyClose = TRUE,
        footer = NULL
      )) 
    })
    
    ## Set & Switch View =======================================================
    # Set base 2D map
    if(is.null(c(geomdl,upper_rst, unit_bounds_st))==T){
      output$geo2d_map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = F)) %>% 
          setView(lng=35.2,lat=32.55,zoom=10) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          htmlwidgets::onRender("
                function(el,x) {
                    geo2d_map = this;
                }
            ")  %>%
          htmlwidgets::onRender("function(el, x) {
                 L.control.zoom({ position: 'bottomright' }).addTo(this)}")
        
      })
      
      geo_dims_v=reactive({input$geo_dims})
      output$geo_view=renderUI({
        #req()
        if(as.character(geo_dims_v()) %in% c("2D", "Edit")){
          leafletOutput("geo2d_map",height = "1950px",width = "2400px")
        } else if (as.character(geo_dims_v())=="3D") {
          # All your dreams....
        }
      })
      
    }
    
  })
  
  ## Load External layers ======================================================
  ### Load Unit Boundary -------------------------------------------------------
  observeEvent(input$unit_Bounds,{
    # Get files
    req(length(input$unit_Bounds)==4) 
    inFile=input$unit_Bounds
    
    if(any(str_detect(inFile$name,".shp"))) {
      shp_path <- reactive({input$unit_Bounds})
      unit_bounds_exp <- Read_Shapefile(shp_path)
      unit_bounds_st <<- unit_bounds_exp() %>% st_transform(.,crs=4326) 
      
      # Set base proxy map
      proxy_geo2d_map=leafletProxy(
        mapId = "geo2d_map",
        session = session
      ) %>%
        clearGroup(group="geo_bounds") %>% 
        addPolygons(data=unit_bounds_st,
                    color= "black",
                    fillColor= "gray",
                    label = NULL,
                    fill=F,
                    weight = 6,
                    fillOpacity = 0.2,
                    smoothFactor = 0,
                    group="geo_bounds")
    } 
  })
  
  # Clean load
  observeEvent(input$rst_Bounds,{
    unit_bounds_st<<-NULL
    proxy_geo2d_map=leafletProxy(
      mapId = "geo2d_map",
      session = session
    ) %>%
      clearGroup(group="geo_bounds")
  })
  
  ### Load Geology Blocks ------------------------------------------------------
  observeEvent(input$geology_blocks,{
    # Get files
    req(length(input$geology_blocks)==4) 
    inFile=input$geology_blocks
    
    if(any(str_detect(inFile$name,".shp"))) {
      shp_path <- reactive({input$geology_blocks})
      geology_blocks_exp <- Read_Shapefile(shp_path)
      geology_blocks_st <<- geology_blocks_exp() %>% st_transform(.,crs=4326) 
      # Update base proxy map
      proxy_geo2d_map=leafletProxy(
        mapId = "geo2d_map",
        session = session
      ) %>%
        clearGroup(group="geo_blocks") %>% 
        addPolylines(data=geology_blocks_st,
                     color= "black",
                     fillColor= "gray",
                     label = NULL,
                     fill=F,
                     weight = 0.5,
                     fillOpacity =1,
                     smoothFactor = 3,
                     group="geo_blocks")
      
    } 
  })
  
  # Clean load
  observeEvent(input$rst_blocks,{
    unit_bounds_st<<-NULL
    proxy_geo2d_map=leafletProxy(
      mapId = "geo2d_map",
      session = session
    ) %>%
      clearGroup(group="geo_blocks")
  })
  
  ### Load Upper Layer ---------------------------------------------------------
  observeEvent(input$upper_layer,{
    # Get files
    req(length(input$upper_layer)==4 | length(input$upper_layer)==1) 
    inFile=input$upper_layer
    
    if(inFile$type=="image/tiff") {
      upper_rst=raster(inFile$datapath)
      if(as.character(crs(upper_rst))!="+proj=longlat +datum=WGS84 +no_defs"){
        upper_rst=projectRaster(upper_rst, crs = 4326)
      }
      names(upper_rst)="upper_rst"
      upper_rst<<-upper_rst
      
      # Set Ranges
      up_rng = seq(minValue(upper_rst),maxValue(upper_rst), length.out=10)
      up_pal = colorNumeric("Blues", up_rng, na.color = "transparent")
      # Update base proxy map
      
      proxy_geo2d_map=leafletProxy(
        mapId = "geo2d_map",
        session = session
      ) %>%
        clearGroup(group="geo_upper") %>% 
        addRasterImage(upper_rst,
                       color = up_pal,
                       opacity = 0.8,
                       group="geo_upper") %>%
        leaflet::addLegend(pal = up_pal,
                           values = up_rng,
                           title = "Upper Layer [m amsl]",
                           position = "topright",
                           group="geo_upper")
    } 
  })
  
  # Clean load
  observeEvent(input$rst_upper,{
    upper_rst<<-NULL
    proxy_geo2d_map=leafletProxy(
      mapId = "geo2d_map",
      session = session
    ) %>%
      clearGroup(group="geo_upper")
  })
  ## Set Export Target =========================================================
  # Export Model -  Frontend
  expm_rct=reactive({input$expm_v})
  output$expmUI=renderUI({
    if(as.character(expm_rct())=="Raster"){
      # Download to
      column(12,
             downloadButton("dnl2rst",'Download 2 Raster')  
      )
    } else if (as.character(expm_rct())=="XYZ Grid") {
      column(12,
             # Upload Grid
             fileInput('gomdl_grid',
                       creditactionBttn(infoId="csgrid_info",color="default",c_label="Upload XY file",
                                        icon= tags$i(class = "fas fa-th", style="font-size: 22px; color: blue")),
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv'
                       )
             ),
             # Download to  
             downloadButton("dnl2grid",'Download 2 Your Grid')
      )
    } else if (as.character(expm_rct())=="Contour") {
      column(12,
             # Set Resolution
             numericInput("contour_res", msgactionBttn(infoId="contour_res_info",
                                                       color="primary",c_label="contour Resolution [-]:"),
                          min = 10, max = 100, value = 10,step=10
             ),
             # Download to  
             downloadButton("dnl2cont",'Download 2 Contour')
      )
      
    }
  })
  
  # Export Model - Backed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # Raster 
  output$dnl2rst <-  downloadHandler(
    message("Download Geology Model"),
    filename = function() { 
      paste("Geology Model_", Sys.Date(), ".tif", sep=""
      )
    },
    content = function(file) {
      writeRaster(geomdl$rst,selfcontained = T, filename=file)
    })
  
  # Personal XYZ
  observeEvent(input$gomdl_grid,{
    req(as.character(expm_rct())=="XYZ Grid") 
    gmFile=fread(input$gomdl_grid$datapath) %>%
      as_tibble(.) %>% 
      mutate(across(.cols = everything(), .fns = toupper),
             across(.cols = everything(), .fns = as.numeric))
    nms=names(gmFile)
    if("X" %in% nms & input$country!="Indefinite") {
      crs_id=subset(localtiles_df,country==input$country,crs)
      gmgrid_pnt<<-st_as_sf(gmFile,
                            coords = c("X", "Y"), crs =as.numeric(crs_id),remove=F) %>% 
        st_transform(.,crs =4326)
      
      messeges_str="The coordinate system has been converted from a local projection to an international projection.
                        There may be a deviation of few meters of the grid points."
      showModal(modalDialog(
        title = "Projection seting: ",
        messeges_str,
        easyClose = TRUE,
        footer = NULL
      )) 
      
      
    } else {
      gmgrid_pnt<<-st_as_sf(gmFile,
                            coords = c("LON", "LAT"), crs =4326,remove=T)
    }
  })
  # gomdl_grid_rct <- reactive({ 
  #   
  #   return(gmgrid_pnt)
  # })
  
  output$dnl2grid <-  downloadHandler(
    message("Download Geology Model"),
    filename = function() { 
      paste("Geology Model_", Sys.Date(), ".csv", sep=""
      )
    },
    content = function(file) {
      fwrite(as.data.table(geomdl$xyz), file=file, row.names=F)
    })
  
  output$dnl2cont <-  downloadHandler(
    message("Download Geology Model"),
    filename = function() { 
      paste("Geology Model_", Sys.Date(), ".csv", sep=""
      )
    },
    content = function(file) {
      sf::st_write(geomdl$cont, dsn=file ,delete_dsn=T,
                   layer_options = "GEOMETRY=AS_WKT")
    })
  
  ## Run Geology Model ========================================================= 
  observeEvent(input$RGM,{
    req(nrow(horizons_db)>0)
    req(!is.null(input$horizon_unit)==T)
    req(!is.null(Geology_Description_ss)==T)
    ### Set Parameters ---------------------------------------------------------
    horizon_type=input$horizon_type
    horizons_db_i = horizons_db %>% dplyr::filter(Horizon==input$horizon_unit)
    if(!is.null(geology_map_act)==T){
      surface_unit_st = geology_map_act %>% dplyr::filter(Name_Eng %in% input$surface_unit)
    } else {
      surface_unit_st=NULL
    }
    
    INDEX = as.data.frame(read_excel(paste0(design_pth,"/INDEX_National_V5.xlsm"),sheet = "Index")) %>% 
      dplyr::filter(type %in% input$CS_type) %>% 
      dplyr::rename("{input$CS_type}":=f_ID)
    
    obs_points_i <<- Geology_Description_ss %>% 
      dplyr::filter(name %notin% input$obs_notincluded) %>% 
      subset(.,,c("well_id","Longitude","Latitude","elv","top_layer","bot_layer",input$CS_type)) %>% 
      left_join(.,subset(INDEX,,c(input$CS_type,"f_name"))) %>% 
      dplyr::filter(f_name %in% input$observation_points) %>% 
      dplyr::distinct_all(,.keep_all = T)
     
    
    if(input$horizon_type=="Base"){
      obs_points_u <<- obs_points_i %>% group_by(well_id,elv,Longitude,Latitude) %>% 
        dplyr::summarise(targ_dpt=max(bot_layer,na.rm = T)) 
    } else{
      obs_points_u <<- obs_points_i %>% group_by(well_id,elv,Longitude,Latitude) %>% 
        dplyr::summarise(targ_dpt=min(top_layer,na.rm = T)) 
    }
    
    # Algorithm Parameters
    if(as.character(algorithm_v())=="Kriging"){
      ap_lst=list(kriging_mdl=input$kriging_mdl, kriging_pxl=input$kriging_pxl, kriging_lags=input$kriging_lags)
    } else if (as.character(algorithm_v())=="Neural Networks") {
      ap_lst=list(layers_rng=input$layers_rng, layers_n=input$layers_n)
    } else if (as.character(algorithm_v())=="Random Forests") {
      ap_lst=list(rf_normalize=input$rf_normalize, trees_n=input$trees_n,mtry=input$mtry)
    } else if (as.character(algorithm_v())=="Support Vector Machine") {
      ap_lst=list(svm_typ=input$svm_typ, kernel=input$kernel,svmc_v=input$svmc_v)
    }
    
    ## Oprate Model ------------------------------------------------------------
    withProgress(message = 'Geology Model building in progress',
                 detail = 'This may take few minth', value = 0, min=0,max=360,
                 expr = {    
                   tictoc::tic()
                   withCallingHandlers({
                     shinyjs::html("geo_messages", "")
                     geomodel<<-line2horizon(
                       horizons_db_i=horizons_db_i,
                       notincluded=input$notincluded,
                       surface_unit_st=surface_unit_st,
                       country=input$country,
                       grid_reso=0.00001*input$grid_res, # Convert resolution to dd
                       obs_points_u=obs_points_u,
                       obs_inclod=input$obs_inclod,
                       unit_bounds_st=unit_bounds_st,
                       geology_blocks_st=geology_blocks_st,
                       algorithm_s=input$interpolation_algorithm,
                       ap_lst=ap_lst,
                       upper_layer=upper_rst,
                       rst_cutter=input$rst_cutter
                     )
                   },
                   message = function(m) {
                     shinyjs::html(id = "geo_messages", html = m$message, add = F)
                   }
                   )
                   tictoc::toc() 
                 })
    ### Set outpost ------------------------------------------------------------
    geomdl=list()
    
    # Raster 
    geomdl$rst=geomodel
    
    # Contour
    if(as.character(expm_rct())=="Contour"){
      geomdl$cont=st_as_sf(rasterToContour(geomodel,nlevels = input$contour_res))
    } else {
      geomdl$cont=st_as_sf(rasterToContour(geomodel,nlevels = 10))
    }
    
    # XYZ Grid
    if(as.character(expm_rct())=="XYZ Grid" & !is.null(gmgrid_pnt)==T){
      geomdl$xyz = gmgrid_pnt %>%  mutate(int_z=raster::extract(geomodel,.)) 
    }
    
    # #Upper layer
    if(!is.null(upper_rst)==T){
      upper_rs=raster::resample(upper_rst,geomodel)
      above_rst=upper_rs-geomodel
      geomdl$above_rst = above_rst
      
      above_cont=st_as_sf(rasterToContour(above_rst,nlevels = 200))%>% 
        dplyr::filter(level>0) %>% 
        dplyr::mutate(level=as.numeric(level))
      geomdl$zero_cont = above_cont  %>% 
        dplyr::filter(level==min(above_cont$level,na.rm = T))
    }
    
    
    geomdl<<-geomdl
    
    ### Update 2D map ----------------------------------------------------------
    output$geo2d_map <<- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = F)) %>% 
        setView(lng=35.2,lat=32.55,zoom=10) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        htmlwidgets::onRender("
                function(el,x) {
                    geo2d_map = this;
                }
            ")  %>%
        htmlwidgets::onRender("function(el, x) {
                 L.control.zoom({ position: 'bottomright' }).addTo(this)}")
      
    })
    
    
    obs_points4map <<- left_join(obs_points_u,subset(Geology_Description_ss,,c("well_id","name","Longitude","Latitude")))
    horizons_db4map  <<- dplyr::filter(horizons_db_i,ID %notin% input$notincluded) 
    
    proxy_geo2d_map=leafletProxy(
      mapId = "geo2d_map",
      session = session
    )
    
    if (!is.null(geomdl$above_rst)==T){
      proxy_geo2d_map<<-extra4geo2d(geo2d_map=proxy_geo2d_map,
                                    horizons_db_i=horizons_db4map,
                                    geomdl,
                                    obs_points = obs_points4map,
                                    horizon_unit=input$horizon_unit) 
    } else {
      proxy_geo2d_map<<-updt_geo2d_map(geo2d_map=proxy_geo2d_map,
                                       horizons_db_i=horizons_db4map,
                                       geomdl,
                                       obs_points = obs_points4map,
                                       horizon_unit=input$horizon_unit)
    }
    
    
  })
  
  ## Edit Geology model ========================================================
  observeEvent(input$geo_dims,{
    req(!is.null(geomdl)==T)
    req(!is.null(horizons_db)==T)
    #req(!is.null(proxy_geo2d_map)==T)
    if (as.character(input$geo_dims)=="Edit"){}
  })
  
  # End of Geology model
  
} # End of Server --------------------------------------------------------------

shinyApp(ui, server,options = list(port=7990))




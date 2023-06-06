#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import r2d3
#' @importFrom jsonlite toJSON
#' @importFrom magrittr "%>%"
#' @import ggplot2
#' @import dplyr
#' @import capture
#' @importFrom utils "read.csv"
#' @import grDevices
#' @noRd

#database credentials to load data. if running on linux (server) use service, else windows authentication
get_db_connect <- function(){
  b_server <<- as.logical(Sys.info()["sysname"] == "Linux")
  
  ##Credentials: 
  # make connection to SQL database
  #database details
  #db_connection_string <<- if(b_server){paste0("driver={FreeTDS};servername=CBAS-PDDB-04; database=NZST-WKG; Uid=CBAS2\\nzst-db04-svce; Pwd=HakHikHuk04!")
    
  # create connection to SQL
  if(b_server){
  db_connection_string <<- paste0("driver={FreeTDS};servername=CBAS-PDDB-04; database=NZST-WKG; Uid=CBAS2\\nzst-db04-svce; Pwd=HakHikHuk04!")
  conn <- dbConnect(odbc::odbc(), .connection_string = db_connection_string)
  }else{
  #"driver={SQl Server};
  # server=CBAS-PDDB-04a;
  #database=NZST-WKG"} #this version for running on desktop (windows)
  conn <- dbConnect(odbc::odbc(),
            Driver = "ODBC Driver 17 for SQL Server",
            Server = "CBAS-PDDB-04",
            Database = "NZST-WKG",
            Trusted_Connection = "yes"
  )
  }
  return(conn)
}

app_ui <- function(request) {
 
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    
##Matomo tracking 
#     tags$head(HTML(
#       "<script type='text/javascript'>
#   var _paq = window._paq = window._paq || [];
#   /* tracker methods like 'setCustomDimension' should be called before 'trackPageView' */
#   _paq.push(['trackPageView']);
#   _paq.push(['enableLinkTracking']);
#   (function() {
#     var u='//help.cbas.cloud/matomo/';
#     _paq.push(['setTrackerUrl', u+'matomo.php']);
#     _paq.push(['setSiteId', '9']);
#     var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
#     g.type='text/javascript'; g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
#   })();
# </script>"
#     )),
    

    
    
    fluidPage(
   
      tags$style(HTML('
      body {background-color: black;
            color: white;  /*text color */
      }
      .skin-blue {
         border-bottom: 5px solid #1D70B8;
         margin: 0;
         padding: 0;
      }
      #/*Turn background of user guide only to white*/
      #.tabbable {background-color: white;
      #     color: black;  /*text color */
      #}
    
  
  /*Tell buttons to rotate  */
    .rotate {
    position: absolute;
    top: 10;
    left: 60px;
    height: 40px;
    background: black;
    transform: rotate(90deg);
    transform-origin: left top;
    padding:10px;
    }
.rotate {
display:inline;
  width: calc(100vh -100px);
  height: 50px;
    float: left;
    #width: 5%;
}
.rotate button{
    display:inline-block;
    float: left;
     

}
.fluidpanels {

      display:inline-block;
      float:right;
      margin-left: 100px;
    
     width: calc(100vw - 100px);
     height: 100%;

      }

#.page {
#overflow:auto;
#}
                                 ')),
      
  
      
   div(class='skin-blue',
      fluidRow(height='40px',
               column(2, img(src="www/HMGov_NZST_v2.png",align="left",height='60px')),
               
               column(8, ""),
               column(2,img(src="www/NZST_WhiteOnBlack.png", align = "right",height="60px"))
      )),
   
   div(class='page',
   div(class='rotate',
       actionButton(inputId ="userguide", label ='User Guide'),
       (if(golem::get_golem_options("admin_mode")){
         actionButton(inputId ="adminpage", label ='Admin Page')}),
       actionButton(inputId ="systemmap", label ='System Map'),
   ),
      #tags$hr(),
   
   #Hide tabs menu
  tags$style("#tabs { display:none; }"), 
   #Define panels
   div(class='fluidpanels',

      tabsetPanel(id="tabs",
                  tabPanel(title = "UserGuide" 
                           ,includeMarkdown(app_sys("app/www/Intro.md"))
                           ,fluidRow(column(3, offset=4,tags$video(id="video",src="www/BEIS_explainer.mp4",type="video/mp4",width="500px",height="500px",controls="controls")))
                           ,includeMarkdown(app_sys("app/www/Instructions.md"))
                           ),
                  (if(golem::get_golem_options("admin_mode")){
                    tabPanel(title = "AdminPage" 
                    ,includeMarkdown(app_sys("app/www/Admin_intro.md"))
                    ,selectInput(inputId ="dataname","Select data name",choices= dbGetQuery(conn=get_db_connect(),"EXEC [usr].[usp_get_data_names]")[,'data_name']) 
                    ,selectInput(inputId ="dataversion","Select version to load",choices= c("-")) 
                    ,br(),br(),br(),br(),br(),br(),br(),br(),br() #blank space
                    ,includeMarkdown(app_sys("app/www/push_to_live_desc.md"))
                    ,actionButton(inputId ="pushtolive", label ='Push selected data to live')
                    ,actionButton(inputId ="deleteversion", label ='Delete selected data from database')
                  )
                  }),
                  tabPanel(title="SystemMap"
                    ,mod_r2d3_ui("r2d3_ui_1")
                  )
                  )))
      ) #End of fluidPage
    ) #End of tagList
  } #end of app_ui
    
#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resou`rce_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  add_resource_path(
    'assests', app_sys('app/assests')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'NetZeroSystemsTool'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


#' r2d3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' 



mod_r2d3_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      
              tags$div(
                id = "result-block",
                d3Output(ns("d3Plot")
                         ,width="100%",  height =  "calc(100vh - 100px)"
                ))
    )
  )
  
}
    
#' r2d3 Server Functions
#'
#' @noRd 
mod_r2d3_server <- function(id,dataname=NULL,dataversion=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    if(golem::get_golem_options("admin_mode")){
      nodes <- dbGetQuery(conn=get_db_connect(),paste("EXEC [usr].[usp_get_nodes] ","'",dataname,"','",dataversion,"'",sep=''))
      edges <- dbGetQuery(conn=get_db_connect(),paste("EXEC [usr].[usp_get_edges] ","'",dataname,"','",dataversion,"'",sep=''))
      user = "admin"
    } else {
    if(golem::get_golem_options("load_from_file")){
      nodes <- read.csv("inst/app/data/Jan2023_nodes.csv")
      edges <- read.csv("inst/app/data/Jan2023_edges.csv")
    } else {
      nodes <- dbGetQuery(conn=get_db_connect(),"EXEC [usr].[usp_get_nodes_live]")
      edges <- dbGetQuery(conn=get_db_connect(),"EXEC [usr].[usp_get_edges_live]")
    }
    user = "nonadmin"
    }
    map_list <- list(nodes = nodes,links = edges)
    data <- toJSON(map_list)
    
    #Send that json from the session to our javascript
    session$sendCustomMessage(type="jsondata",data)
    
    output$d3Plot<-renderD3({
      data<-data
      
    r2d3(css="inst/app/data/net_zero.css", data=data, script = "inst/app/www/js/main.js"
         ,dependencies = list("inst/app/www/js/canvas-toBlob.js","inst/app/www/js/FileSaver.min.js","inst/app/www/js/map_export.js","inst/app/www/js/changes_tab.js"
                              ,"inst/app/www/js/meta_data.js","inst/app/www/js/side_legends.js"
                              ,"inst/app/www/js/data_funcs.js","inst/app/www/js/svg_drawing.js"
                              ,"inst/app/www/js/loops_tab.js","inst/app/www/js/edit_nodes.js"
                              ,"inst/app/www/js/hull_functions.js","inst/app/www/js/overview_map.js","inst/app/www/js/algorithm_funcs.js","inst/app/www/js/scenario_funcs.js")
         ,d3_version = "4",options = list(user=user))
    })
  })
}





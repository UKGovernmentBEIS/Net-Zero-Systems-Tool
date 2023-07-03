#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @importFrom shiny reactive callModule observe renderUI
#' @noRd


app_server <- function( input, output, session) {
    # List the first level callModules here

  observeEvent(input$userguide,{
    session$reload()
    updateTabItems(session, "tabs", "UserGuide")
    #updateSelectInput(session,inputId ="dataname","Select data to load",choices= dbGetQuery(conn=get_db_connect(),"EXEC [usr].[usp_get_data_names]")[,'data_name']) 
  })
  
  observeEvent(input$adminpage,{
    updateTabItems(session, "tabs", "AdminPage")
    updateSelectInput(session,inputId ="dataname","Select data to load",choices= dbGetQuery(conn=get_db_connect(),"EXEC [usr].[usp_get_data_names]")[,'data_name']) 
    updateSelectInput(session,inputId ="dataversion","Select version to load",choices=paste('v',dbGetQuery(conn=get_db_connect(),paste("EXEC [usr].[usp_get_data_versions] ","'",input$dataname,"'",sep=''))[,'data_version'],sep='')) 
    })

  observeEvent(input$systemmap,{
    if(golem::get_golem_options("admin_mode")){
      mod_r2d3_server("r2d3_ui_1",dataname=input$dataname,dataversion=gsub('v','',input$dataversion))
    }else{
      mod_r2d3_server("r2d3_ui_1")
    }
      updateTabItems(session, "tabs", "SystemMap")
  })
  
  observeEvent(input$pushtolive,{
      dbGetQuery(conn=get_db_connect(),paste("EXEC [usr].[usp_set_live] ","'",input$dataname,"','",gsub('v','',input$dataversion),"'",sep=''))
      print(paste("Pushing ",input$dataname,"','",input$dataversion,"'"," to live",sep=''))
  })
  
  observeEvent(input$deleteversion,{
    dbGetQuery(conn=get_db_connect(),paste("EXEC [usr].[usp_delete_version] ","'",input$dataname,"','",gsub('v','',input$dataversion),"'",sep=''))
    updateSelectInput(session,inputId ="dataname","Select data to load",choices= dbGetQuery(conn=get_db_connect(),"EXEC [usr].[usp_get_data_names]")[,'data_name']) 
    updateSelectInput(session,inputId ="dataversion","Select version to load",choices=paste('v',dbGetQuery(conn=get_db_connect(),paste("EXEC [usr].[usp_get_data_versions] ","'",input$dataname,"'",sep=''))[,'data_version'],sep='')) 
    print(paste("Deleting ",input$dataname,"','",input$dataversion,sep=''))
  })
  
  observeEvent(input$dataname,{
    updateSelectInput(session,inputId ="dataversion","Select version to load",choices=paste('v',dbGetQuery(conn=get_db_connect(),paste("EXEC [usr].[usp_get_data_versions] ","'",input$dataname,"'",sep=''))[,'data_version'],sep='')) 
  })
  
  observeEvent(input$outputdataall,{
    data_output <-input$outputdataall
    if (exists("data_output")) {
      data_nodes <- data_output[['nodes']]
      data_nodes_unlist <- do.call(rbind, lapply(data_nodes,function(x) as.data.frame(x)))
      data_edges <- data_output[['links']]
      data_edges_unlist <- do.call(rbind, lapply(data_edges,function(x) as.data.frame(x)))
      insert_date = Sys.time()
      
      data_edges_unlist['insertdate']=insert_date
      data_nodes_unlist['insertdate']=insert_date
      
      data_edges_unlist['LiveData']=0
      data_nodes_unlist['LiveData']=0
      data_versions = dbGetQuery(conn=get_db_connect(),paste("EXEC [usr].[usp_get_data_versions] ","'",data_nodes_unlist[1,'data_name'],"'",sep=''))[,'data_version']
      if (length(data_versions) == 0){
        new_version = 1.0
      } else {
        new_version = max(data_versions) + 0.1
      }
      data_edges_unlist['data_version']=new_version
      data_nodes_unlist['data_version']=new_version
      
      write_to_sql(data_nodes_unlist,data_edges_unlist) 
      print("Written to database")
    }
  })
}


write_to_sql <- function(nodes_df,edges_df){
  
  dbWriteTable(conn=get_db_connect(),
               name=DBI::Id(
                 schema = "usr"
                 ,table = "NZST_Edges"
               ),
               value=edges_df,
               #field.types=c(Justification...quantifiable="nvarchar(2255)",Source.organisation="nvarchar(2255)"),
               append=TRUE)
  
  dbWriteTable(conn=get_db_connect(),
               name=DBI::Id(
                 schema = "usr"
                 ,table = "NZST_Nodes"
               ),
               value=nodes_df,
               append=TRUE)
}


##Below not needed unless want logins...see original to see how to incorporate if so

# USER <- callModule(mod_loginUI_server, "ui_1")
# 
# # creating appropiate page depending on whether user is allowed access or not. ui1 for wrong credentials
# # and ui2 for successful logins
# observe({
#   
#   if (USER$Logged) {
#     output$page <- renderUI({ui2("ui_2", df= dummy_df)})}
#   
#   else{
#     
#    output$page <- renderUI({ui_1("ui_1")})}
#     
#  
# })
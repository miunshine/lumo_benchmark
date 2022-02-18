#---get the following packages:

#install.packages(c('RSQLite' , 'DBI', 'ggplot2', 'shiny'))

library(RSQLite)
library(DBI)
library(ggplot2)
library(shiny)

# enter your path to database
con <- dbConnect(drv=RSQLite::SQLite(), dbname= "~/ ..... /all-lumosql-benchmark-data-combined.sqlite" )


ds_list <- dbGetQuery(con, paste0("select distinct value from run_data where key = 'option-datasize' order by value") )[,1]
os_list <- dbGetQuery(con, paste0("select distinct value from run_data where key = 'os-version' order by value") )[,1]
be_list <- append(dbGetQuery(con, paste0("select distinct value from run_data where key = 'backend-version' order by value") )[,1], 'NA' )


#----- shiny app
ui <- fluidPage(
  headerPanel(title = "LumoSQL Benchmark Filter"),
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("ds",
                   "Datasize",
                   ds_list,
                   ds_list[1] ),
      radioButtons(inputId = "os",
                   label = "Operating System Version",
                   choices = os_list,
                   selected = os_list[1] ),
      checkboxGroupInput("be",
                         "Backend Version",
                         be_list,
                         be_list[1] )
    ),
    
    mainPanel(
      plotOutput(outputId = "theplot")
    )
  ))


server <- function(input, output, session) {
  globaldf <- reactive({
    
#-----find runs with selected criteria  
    for(j in input$ds){
      for(k in input$os){
        
        idees <- data.frame('run_id')
        colnames(idees) <- c('run_id')
        for (i in input$be){
          iii <- dbGetQuery(con, paste0("select run_id from run_data where (key = 'tests-ok' and value = '17')
                                      intersect select run_id from run_data where (key = 'backend-version' and value = '",i,"')
                                      intersect select run_id from run_data where (key = 'option-datasize' and value = '",j,"')
                                      intersect select run_id from run_data where (key = 'os-version' and value = '",k,"')
                                      "))
          
          idees <- rbind(idees, iii)
        }}}
    
#---- collect info about the runs    
    mat <- matrix(ncol = 4)
    df <- as.data.frame(mat)
    colnames(df) <- list('run_id', 'time', 'sqlite_version' , 'pointer')
    
    idees <- idees[-1,]
    for (h in idees){
      lll <- dbGetQuery(con, paste0("select value from run_data where key in ('sqlite-version') and run_id = '",h,"' "))
      bbb <- dbGetQuery(con, paste0("select value from run_data where key in ('backend-version','cpu-type', 'cpu-comment', 'disk-comment','word-size') and run_id = '",h,"' "))
      timez <- dbGetQuery(con, paste0("select value from test_data where run_id = '",h,"' and key in ('real-time') ") )
      duration <- sum(as.numeric(timez[,1]) )
      pointer <- paste(as.character(bbb[,1]),collapse = ' ')
      
      darow <- data.frame(h, duration, lll[,1], pointer  )
      colnames(darow) <- colnames(df)
      df <- rbind(df, darow)
    }
    
    
    
#--- generate plot    
    pl <-  ggplot(data=df, aes(x=sqlite_version, y=time, group=pointer, colour=pointer )) +
      geom_line()+
      geom_point()+
      theme(legend.position="left")
    
    return(pl)
    
  })
  
  output$theplot <- renderPlot(globaldf())
  
}

shinyApp(ui, server)

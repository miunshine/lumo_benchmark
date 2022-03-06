#---get the following packages:

#install.packages(c('RSQLite' , 'DBI', 'ggplot2', 'shiny', 'ggpubr', 'ggiraph'))

library(RSQLite)
library(DBI)
library(ggplot2)
library(shiny)
library(ggpubr)
library(ggiraph)

# enter your path to database
# the database with our results can be downloaded from the address below
# wget https://lumosql.org/dist/benchmarks-to-date/all-lumosql-benchmark-data-combined.sqlite
con <- dbConnect(drv=RSQLite::SQLite(), dbname= "~/ ..... /all-lumosql-benchmark-data-combined.sqlite" )



nnn <- dbGetQuery(con, paste0("select run_id from run_data where (key = 'tests-ok' and value = '17') "))
for (i in nnn[,1]){
  l = dbGetQuery(con, paste0("select value from run_data where key in ('backend-version') and run_id = '",i,"' "))
  if (length(l[,1]) == 0){
    line <- data.frame(i, 'backend-version', 'NA')
    colnames(line) <- list('run_id','key','value')
    dbAppendTable(con, "run_data", line)
  }
}


ds_list <- dbGetQuery(con, paste0("select distinct value from run_data where key = 'option-datasize' order by value") )[,1]
os_list <- dbGetQuery(con, paste0("select distinct value from run_data where key = 'os-version' order by value") )[,1]
be_list <- dbGetQuery(con, paste0("select distinct value from run_data where key = 'backend-version' order by value") )[,1]


#----- shiny app
ui <- shinyUI(fluidPage(
  headerPanel(title = "LumoSQL Benchmark Filter"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("ds",
                  "Datasize",
                  ds_list,
                  ds_list[1]
      ),
      checkboxGroupInput(inputId = "os",
                         label = "Operating System Version",
                         choices = os_list,
                         selected = '5.15.23' ),
      checkboxGroupInput("be",
                         "Backend Version",
                         be_list,
                         '0.9.29' ),
      width = 3
    ),
    
    mainPanel(
      ggiraphOutput("theplot"),

      plotOutput(outputId = "thelegend")
    )
  )
))

server <- shinyServer(function(input,output) {
  
  
  globaldf <- reactive({
    
    #-----find runs with selected criteria
    for(j in input$ds){
      idees <- data.frame('run_id')
      colnames(idees) <- c('run_id')
      for(k in input$os){
        for (i in input$be){
          iii <- dbGetQuery(con, paste0("select run_id from run_data where (key = 'tests-ok' and value = '17')
                                      intersect select run_id from run_data where (key = 'backend-version' and value = '",i,"')
                                      intersect select run_id from run_data where (key = 'option-datasize' and value = '",j,"')
                                      intersect select run_id from run_data where (key = 'os-version' and value = '",k,"')
                                      "))
          
          idees <- rbind(idees, iii)
        }}}
    
    #-----error message when no data found
    if (length(idees[,1]) == 1){
      validate(
        need(length(idees[,1]) == 0, "No data in this selection")
      )
      
    }
    
    
    #---- collect info about the runs
    mat <- matrix(ncol = 4)
    df <- as.data.frame(mat)
    colnames(df) <- list('run_id', 'time', 'sqlite_version' , 'pointer')
    
    
    idees <- idees[-1,]
    
    for (h in idees){
      lll <- dbGetQuery(con, paste0("select value from run_data where key in ('sqlite-version') and run_id = '",h,"' "))
      if (as.character(lll) == ''){
        lll <- c('3.18.2')
        lll <- as.data.frame(lll)
      }
      
      bbb <- dbGetQuery(con, paste0("select value from run_data where key in ('os-version','backend-name', 'backend-version','cpu-type', 'cpu-comment', 'disk-comment','word-size') and run_id = '",h,"' "))
      timez <- dbGetQuery(con, paste0("select value from test_data where run_id = '",h,"' and key in ('real-time') ") )
      duration <- sum(as.numeric(timez[,1]) )
      pointer <- paste(as.character(bbb[,1]),collapse = ' ')
      
      darow <- data.frame(h, duration, lll[,1], pointer  )
      
      colnames(darow) <- colnames(df)
      df <- rbind(df, darow)
      
    }
    
    df <- df[-1,]
    
    
    
    return(df)
    
  })
  
  output$theplot <- renderggiraph({
    gg <- ggplot(globaldf(), aes(x=sqlite_version, y=time, group=pointer, color=pointer))
    gg <- gg + 
      geom_line_interactive(aes(tooltip = pointer, data_id = pointer), size = 1.3)+
      theme(legend.position="none")

    girafe(ggobj = gg, width_svg = 8, height_svg = 6,
           options = list(
             opts_hover_inv(css = "opacity:0.1;"),
             opts_hover(css = "stroke-width:2;")
           ))
  })

  output$thelegend <- renderPlot({
    pl <-  ggplot(data=globaldf(), aes(x=sqlite_version, y=time, group=pointer, colour=pointer )) +
      geom_line()+
      geom_point()+
      theme(legend.position="bottom")+
      guides(col = guide_legend(nrow = 23))

    leg <- get_legend(pl)
    as_ggplot(leg)
  })
  
})
shinyApp(ui, server)


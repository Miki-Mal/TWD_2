


library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(dplyr)
ui <- fluidPage(
  titlePanel("Movie buissnes"),
  
  fluidRow(
    column(4,
           dateRangeInput("daterange", "Date range:",
                          start  = "2013-01-01",
                          end    = "2016-01-01",
                          min    = "1980-01-01",
                          max    = "2016-12-12",
                          format = "mm/yyyy",
                          startview = "year",
                          separator = " - ")),
    
    column(4,
           selectInput("select","Time step", 
                       choices = list("Month" = 1, "Year" = 2,
                                      "10 Year" = 3), selected = 2))
    
  ),
    fluidRow(
    uiOutput("go_buttons")
    ),
  DTOutput('tbl'),
  shinydashboard::box(title = "See more", 
                      shiny::actionButton(inputId='ab1', label="Dettalis", icon = icon("th")))
  )
# Month calculating Function 
monnb <- function(d) { 
lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
lt$year*12 + lt$mon } 

dane <-  read.csv2("data_layer1.csv")

dane <- dane [,-1]

dane$profit <- floor(dane$revenue/2.4 - dane$budget)
dane <- dane[,c(5,6,3,2)]


server <- function(input, output, session) {
  
  
  # to store observers and make sure only once is created per button
  obsList <- list()
  
  output$go_buttons <- renderUI({
    
    
    #Setting beggin and end date 
    date <- (paste(as.character(input$daterange), collapse = " - "))

    begin_date <- (substring(date,1,7))
    begin_date <- paste0(begin_date,"-01")
    end_date <-  substring(date,14,20)
    end_date <- paste0(end_date,"-28")
    # Selecting data range and calculating buttons number by selected time step 
    # By Month 
    number_of_buttons <- 5
    if (input$select== 1){

      number_of_buttons <- monnb(end_date)-monnb(begin_date)
      # Buttons Names 
      Buttons_n <<- c(1:number_of_buttons)
      d <- as.Date(begin_date)
      Buttons_n[1] <<- substring(as.character(d),1,7)
      for (i in 2:number_of_buttons){
        month(d) <- month(d)+1
        Buttons_n[i] <<- substring(as.character(d),1,7)
      }
      
    }
    #By Year
    
    if (input$select==2){
     
      number_of_buttons <- ceiling(abs(time_length(interval(as.Date(end_date), as.Date(begin_date)), "years")))
      #Buttons Names
      
      Buttons_n <<- c(1:number_of_buttons)
      d <- as.Date(begin_date)
      Buttons_n[1] <<- substring(as.character(d),1,4)
      for (i in 2:number_of_buttons){
        year(d) <- year(d)+1
        Buttons_n[i] <<- substring(as.character(d),1,4)
      }
    }
    
    
    
    
    buttons <- as.list(1:number_of_buttons)
    buttons <- lapply(buttons, function(i)
    {
      btName <- paste0("go_btn",i)
      # creates an observer only if it doesn't already exists
      if (is.null(obsList[[btName]])) {
        
        # make sure to use <<- to update global variable obsList
        obsList[[btName]] <<- observeEvent(input[[btName]], {
          # Prepering Table to show 
          # For Year selection 
          
          if (input$select == 2){
            dane_button <- dane %>% filter(Buttons_n[i]==as.character(year(as.Date(release_date,"%d/%m/%Y"))))
            dane_button <- dane_button %>% arrange(desc(profit)) %>% slice(1:10)
           
          }
          if (input$select == 1 ){
            monnb <- function(d) { 
              lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
              lt$year*12 + lt$mon } 
            
            dane_button <- dane %>% filter(monnb(as.Date(release_date,"%d/%m/%Y"))==monnb(paste0(Buttons_n[i],"-01")))
            
            dane_button <- dane_button %>% arrange(desc(profit)) %>% slice(1:10)
            
          }
 
          # Prepering Month Selection
          sketch <- htmltools::withTags(table(
            class = 'display',
            thead(
              tr(
                
                th(colspan = 5, paste0("Most Profitable Movie : ",dane_button[1,1]))
                ),
              tr(
                th("Title"),
                th("Profit in $"),
                th("Reles Date"),
                th("Production Company")
              )
            )
          ))
          
          output$tbl = renderDT(
            # Prepering tabel titel 
            
     
            
          datatable(dane_button, container = sketch, rownames = FALSE,filter = "none",options = list(dom = 't')))
        })
      }
     
        actionButton(btName,Buttons_n[i])
      
    }
    )
  })
  
}

shinyApp(ui, server)
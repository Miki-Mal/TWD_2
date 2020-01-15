


library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(dplyr)
ui <- fluidPage(
  
  tabsetPanel(
    id = "inTabset",

  tabPanel("Movie business",
           
  
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
  actionButton("tab", "See More")),
  tabPanel("Movies in selected Time",
           titlePanel( paste0( "Selected time")),
           tabsetPanel(
             #wszystike filmy
             tabPanel("All films back then", dataTableOutput("all_film")), 
             #wykres premier i zarobków filmów
             tabPanel("Flims box office", plotlyOutput("films_boxoffice")), 
             #wykres zarobków producentów w danym czasie
             tabPanel("Producers' earnings", plotlyOutput("prod"))
           )))
  )
# Month calculating Function 
monnb <- function(d) { 
lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
lt$year*12 + lt$mon } 

dane <-  read.csv2("data_layer1.csv")

dane <- dane [,-1]

dane$profit <- floor(dane$revenue/2.4 - dane$budget)
dane <- dane[,c(5,6,3,2)]
dane$release_date <- as.Date( dane$release_date,"%d/%m/%Y")


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
    # Observer to change tab 
    observeEvent(input$tab, {
      updateTabsetPanel(session, "inTabset",
                        selected = "Movies in selected Time")
    })
    
    
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
            begin <-  as.Date((paste0(Buttons_n[i],"-01-01")),"%Y-%m-%d")
            end <- as.Date(paste0(Buttons_n[i],"-12-31"),"%Y-%m-%d")
            print(begin)
           
          }
          if (input$select == 1 ){
            monnb <- function(d) { 
              lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
              lt$year*12 + lt$mon } 
            
            dane_button <- dane %>% filter(monnb(as.Date(release_date,"%d/%m/%Y"))==monnb(paste0(Buttons_n[i],"-01")))
            
            dane_button <- dane_button %>% arrange(desc(profit)) %>% slice(1:10)
            begin <-  as.Date(paste0(Buttons_n[i],"-01"),"%Y-%m-%d")
            end <- as.Date(paste0(Buttons_n[i],"-28"),"%Y-%m-%d")
            
            
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
          
          data_period_r <- reactive({
            dane %>%
              arrange( desc( profit)) %>%
              filter( release_date >= as.Date( begin) ) %>%
              filter( release_date <= as.Date( end) )
          })
          #tabelka wzytskich filmów
          output$all_film <- renderDataTable(
            data_period_r(),
            rownames = FALSE
          )
          #plotly premier i zarobków filmów
          output$films_boxoffice <- renderPlotly(
            plot_ly( data = data_period_r(), x = ~release_date, y = ~profit,
                     #TODO dodać linie (lolipop)
                     mode = 'markers', 
                     #TODO kolor do poprawy (może wytwórni)
                     marker = list( color = ~profit),
                     text = ~title,
                     #TODO dodać wytwórnie jeśli to możliwe
                     hovertemplate = paste('<b>%{text}</b><br>',
                                           '<i>Profit</i>: $%{y}<br>',
                                           '<i>Release date</i>: %{x}<br>'
                     )
            ) %>%
              layout(
                #nazwy osi
                yaxis = list( title = "Box office"),
                xaxis = list( title = "Release date")
              )
          )
          
        })
      }
     
        actionButton(btName,Buttons_n[i])
      
    }
    )
  })
  
}

shinyApp(ui, server)
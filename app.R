


library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(dplyr)
library(plotly)
<<<<<<< HEAD
=======
library(tidyverse)

>>>>>>> d901d3b0c6a94d7a2b7e3a6afd43fd3d90d0474b

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
           uiOutput("titlet_layer2"),
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
dane2<-dane
dane1<-select(dane,c(2,3,4,5))
dane <- dane [,-1]
dane$profit <- floor(dane$revenue/2.4 - dane$budget)
dane <- dane[,c(5,6,3,2)]
dane1$profit <- floor(dane1$revenue/2.4 - dane1$budget)
dane1$release_date <- as.Date( dane1$release_date,"%d/%m/%Y")
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
          
          ## Data prepeing 
     
          dane1<-filter(dane1,release_date >= as.Date(begin))
          dane1<-filter(dane1,release_date<= as.Date(end))
          dane1<-aggregate(dane1$profit,by = list(Name=dane1$production_companies), FUN = sum)
          dane1<-rename(dane1,value = x)
          dane1<-arrange(dane1,desc(value))
          dane1<-slice(dane1,1:10)#Można dać do N by wybierało n najmocniejszych firm
          names2<-select(dane1,Name)
          name<-unlist(names2,use.names = FALSE)
          dane2$profit <- floor(dane2$revenue/2.4 - dane2$budget)
          dane2$release_date <- as.Date( dane2$release_date,"%d/%m/%Y")
          dane2<-filter(dane2,release_date >= as.Date(begin))
          dane2<-filter(dane2,release_date<= as.Date(end))
          dane2<-subset(dane2, production_companies %in% name)
          names3<-add_column(names2,profit = 0,release_date = begin)
          names2<-add_column(names2,profit = 0,release_date = end)
          names4<-rbind(names2,names3)
          names4$revenue<-NA
          names4$title<-NA
          names4$budget<-NA
          names4$X<-NA
          names4<-rename(names4,production_companies = Name)
          dane2<-rbind(dane2,names4)
          dane2<-arrange(dane2,release_date)
          dane2<-group_by(dane2,production_companies)
          dane2<-mutate(dane2,suma = cumsum(profit))
          
 
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
          output$titlet_layer2 <- renderText({paste0("Time form ",as.character(begin)," to ",as.character(end))})
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
          output$prod<- renderPlotly({
            plot_ly(dane2,x = ~release_date,y = ~suma, type = "scatter", mode = "lines",color =~production_companies ) %>%
              layout(title = list(text="Całkowite przychody w okresie",x=0.4),
                     xaxis = list(title = "Test"),
                     yaxis = list(title = "Przychody $"))})
          
        })
      }
     
        actionButton(btName,Buttons_n[i])
      
    }
    )
  })
  
}

shinyApp(ui, server)
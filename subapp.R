library(shiny)
library(DT)
library(dplyr)
library(plotly)

#data początkowa i końcowa
begin <- "2017-01-01"
end <- "2018-01-01"

#to samo co w głównej app
dane <-  read.csv2("data_layer1.csv")
dane <- dane [,-1]
dane$profit <- floor(dane$revenue/2.4 - dane$budget)
dane <- dane[,c(5,6,3,2)]

#dodatkowawo zamiana release_date jako czas
dane$release_date <- as.Date( dane$release_date,"%d/%m/%Y")

ui <- fluidPage(
  titlePanel( paste0( "Time period from ", begin, " to ", end)),
      tabsetPanel(
        #wszystike filmy
        tabPanel("All films back then", dataTableOutput("all_film")), 
        #wykres premier i zarobków filmów
        tabPanel("Flims box office", plotlyOutput("films_boxoffice")), 
        #wykres zarobków producentów w danym czasie
        tabPanel("Producers' earnings", plotlyOutput("prod"))
    )

)

server <- function(input, output, session) {
  #przefiltrowanie danych pod wzgledem daty początkowej i końcowej
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
}

shinyApp(ui,server)

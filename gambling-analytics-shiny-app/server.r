#Start of Server

options(shiny.maxRequestSize=30*1024^2)

#Libraries Deployed
library(shinycssloaders)
library(shiny)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(plotly)
library(plyr)
library(dplyr)
library(DT)
library(data.table)
library(lubridate)
library(shinydashboard)
library(shinythemes)
library(tidyr)
library(shinyWidgets)
library(rsconnect)

server <- function(input, output,session) {
  
  basetable <- read.csv("./basetable.csv", stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    
    if(is.null(basetable))
    {return ()}
    else
    {
        data<-basetable
    }
    
    if(input$SelectG=="Male")
    {
      data <- data[data$Gender == 'M',]
    }
    
    else if (input$SelectG=="Female")
    {
      data <- data[data$Gender == 'F',]
    }
    else{
      data
    }
    },options = list(searching = FALSE),rownames=FALSE)
 
  
  dataset <- reactive({
    basetable[sample(nrow(basetable), input$sampleSize),]
  })
  
  
#Sports Book Plots  
  
  #Plot for SportsBook Variable Relationship Viz  
output$SPPlot <- renderPlotly({
    
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point()+ geom_smooth(method=lm)+ labs( x = input$x, y = input$y)
    
    
  })

  #Plot for SportsBook Variable Relationship Viz
  output$SPMval <- renderPlotly({

    p <- ggplot(basetable, aes_string(x = input$x3, y = input$y3)) + 
      geom_point() + geom_smooth(method=lm)
  })
  

  
#Poker Plots
  
  output$PKPlot <- renderPlotly({
    
    #Plot for Poker Variable Relationship Viz
    p <- ggplot(basetable, aes_string(x = input$xp, y = input$yp, color = input$color)) + 
      geom_point()+ geom_smooth(method=lm)+ labs( x = input$xp, y = input$yp , title= "Buy and Sell Relationship")
    
  })
  
  
#Casino Plots
  
  #Plot for Casino Variable Relationship Viz
  output$CSPlot <- renderPlotly({
    

    p <- ggplot(basetable, aes_string(x = input$xc, y = input$yc, color = input$color)) + 
      geom_point()+ geom_smooth(method=lm)+ labs( x = input$xc, y = input$yc , title= "Winnings, bets and stakes relationship")
    
  })

  #Plot for Casino Variable Relationship Viz
  output$CSMVal <- renderPlotly({

    p <- ggplot(basetable, aes_string(x = input$x3c, y = input$y3c)) + 
      geom_point() + geom_smooth(method=lm) + labs( x = input$x3c, y = input$y3c , title= "Bets and monetary value")
  })
  
  
#Games Plots
  
  #Plot for Games Variable Relationship Viz
  output$GMPlot <- renderPlotly({
    
    p <- ggplot(basetable, aes_string(x = input$xg, y = input$yg, color = input$color)) + 
      geom_point()+ geom_smooth(method=lm)+ labs( x = input$xg, y = input$yg , title= "Winnings, bets and stakes relationship")
    
  })
  
  
  #Plot for Games Variable Relationship Viz
  output$GMMVal <- renderPlotly({

    p <- ggplot(basetable, aes_string(x = input$x3g, y = input$y3g)) + 
      geom_point() + geom_smooth(method=lm) + labs( x = input$x3g, y = input$y3g , title= "Bets and monetary value")
  })
  
  
#All Products Plots
  
  number_selected <- reactive({
    req(input$selected_number) # ensure availablity of value before proceeding
    filter(basetable,numberofProducts %in% input$selected_number)
  })
  
  output$plotnumber <- renderPlotly({
    p <- number_selected() %>%
      group_by(numberofProducts) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~numberofProducts, values = ~count) %>%
      add_pie(hole = 0.6)%>%
      layout(showlegend = FALSE , title= "Total Number of products")
  })
  
  
  
  app_selected <- reactive({
    req(input$selected_app) # ensure availablity of value before proceeding
    filter(number_selected(),Application.Description %in% input$selected_app)
  })
  
  output$plotapp <- renderPlotly({
    plot_ly(app_selected(), x = ~Application.Description)
  })
  
  output$allP <- renderPlotly({
    # build graph with ggplot syntax
    p <- ggplot(app_selected(), aes_string(x = input$xall, y = input$yall)) + 
      geom_point() + geom_smooth(method=lm)
  })
  
  
  
  gender_selected <- reactive({
    req(input$selected_gender) # ensure availablity of value before proceeding
    filter(basetable,Gender %in% input$selected_gender)
  })
  
  
  continent_selected <- reactive({
    req(input$continent) # ensure availablity of value before proceeding
    filter(gender_selected(),Continents %in% input$continent)
  })
  
  
  # Demographics Plots
  
  #Plot for Continents Viz
  output$plotcontinent <- renderPlotly({
    p <- continent_selected() %>%
      group_by(Continents) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~Continents, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Customers per Continent",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  #Plot for Age Viz  
  output$agehist <- renderPlotly({
    plot_ly(continent_selected(), x = ~AGE,type= "histogram" , xbins= list(end = input$xbins[2], size = 1, start = input$xbins[1]))%>%
      layout(title = "Age distribution")
    
  })  
  
}


#End of Server.


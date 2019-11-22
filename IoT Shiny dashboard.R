library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(skin = "green",
  
  dashboardHeader(title = "Energy consumption",
                  titleWidth = 150
  ),
  dashboardSidebar(
    width = 150,
    sidebarMenu(
      # first menu to select dashboard 
      menuItem(
        text = "Dashboard", 
        tabName = "dashboard", 
        icon = icon("dashboard")
        ),
      menuItem(
        text = "Charts", 
        tabName = "menu2",
        icon = icon("bar-chart-o"),
        badgeLabel = "new", 
        badgeColor = "green"
        ),
    valueBoxOutput("approvalBox")
    )
    ),
    
  dashboardBody(
    
    fluidRow(
      
      tabBox(
        title = "Usage per weekday per Sub Meter", id = "ScatterViolin", width = "100%", height = NULL,
        tabPanel(
          title = "Kitchen", 
          plotOutput(outputId = "scatter_violin_1")
          ),
        tabPanel(
          title =  "Laundry", 
          plotOutput(outputId = "scatter_violin_2")
          ),
        tabPanel(
          title = "Climate area",
          plotOutput(outputId = "scatter_violin_3")
          )
        ),
      
      box(
        plotOutput(outputId = "plotGA", height = "500px")
        ),
      box(
        plotOutput(outputId = "plotSM1", height = "500px")
        ),
      box(
        plotOutput(outputId = "plotSM2", height = "500px")
          ),
      box(
        plotOutput(outputId = "plotSM3", height = "500px")
        )
      )
    )
)


server <- function(input, output) {
 
   output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "32%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green", width = 2,
    )
  })
   
   output$scatter_violin_1 <- renderPlot({
     ggplot(data=daily, aes(x=wday, y=Sub_metering_1)) +
            geom_violin(aes(fill=wday)) +
            geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
            theme_minimal()
  })
   
   output$scatter_violin_2 <- renderPlot({
     ggplot(data=daily, aes(x=wday, y=Sub_metering_2)) +
       geom_violin(aes(fill=wday)) +
       geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
       theme_minimal()
   })
   
   output$scatter_violin_3 <- renderPlot({
     ggplot(data=daily, aes(x=wday, y=Sub_metering_3)) +
       geom_violin(aes(fill=wday)) +
       geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
       theme_minimal()
   })

  output$plotGA <- renderPlot({
          plot(Future_season_GA, 
           main = "Forecast GA", 
           xlab = "Date", 
           ylab = "Amount", 
           lwd = 3)
  })
  
  output$plotSM1 <- renderPlot({
    plot(Future_season_SM1, 
         main = "Forecast SM1", 
         xlab = "Date", 
         ylab = "Amount", 
         lwd = 3)
  })
  
  output$plotSM2 <- renderPlot(
    plot(Future_season_SM2, 
         main = "Forecast SM2", 
         xlab = "Date", 
         ylab = "Amount", 
         lwd = 3)
  )
  
  output$plotSM3 <- renderPlot({
    plot(Future_season_SM3, 
         main = "Forecast SM3", 
         xlab = "Date", 
         ylab = "Amount", 
         lwd = 3)
  })
}

shinyApp(ui, server)                  


library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(skin = "green",
                    
                    dashboardHeader(title = "Energy consumption",
                                    titleWidth = 200
                    ),
                    dashboardSidebar(
                      width = 200,
                      sidebarMenu(
                        # first menu to select dashboard 
                        menuItem(
                          text = "Dashboard", 
                          tabName = "dashboard", 
                          icon = icon("dashboard")
                        ),
                        dateRangeInput("daterange3", "Date range:",
                                       start  = "2007-01-01",
                                       end    = "2011-12-31",
                                       format = "yy/mm/dd",
                                       separator = " - "),
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
                          title = "YoY Usage & pred Dec 2010-2012 per weekday",
                          id = "Violinplot", height = NULL,
                          tabPanel(
                            title = "Kitchen", 
                            plotOutput(outputId = "Violin_1_mean")
                          ),
                          tabPanel(
                            title =  "Laundry", 
                            plotOutput(outputId = "Violin_2_mean")
                          ),
                          tabPanel(
                            title = "Climate area",
                            plotOutput(outputId = "Violin_3_mean")
                          )
                        ),
                        
                        tabBox(
                          title = "Usage per weekday per Sub Meter", 
                          id = "ScatterViolin", height = NULL,
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
                        column(width = 6,
                        box(
                          title = "Conclusions YoY TOTAL power use & predictions", 
                          width = NULL, 
                          background = "light-blue",
                          "In general 

the inner plots show the predicted usage for Dec 2010 till 2012 based on 
forecasting with mean and compares with the mean and distribution of historical use. 
NA's are filled with loc(Last observation carried forward). 

Wider sections of the violin plot represent a higher probability of observations 
taking a given value, the thinner sections correspond to a lower probability.

All Violin plots show the overall distribution with less outliers in the predictions. 

In the Kitchen area it is predicted that usage will be more stable around a 
slightly higher mean. And that variance in power usage is mostly in the weekends. 

In the Laundry area a decrease in mean is predicted for all days except 
Tuesday and Wednesday. 

The use of water heater and airconditioner is predicted to grow in 
generals and especialliy for weekdays."
                        )
                      ),
                      column(width = 6,
                      box(
                        title = "Conclusions historical use of power 2007-Dec 2010", 
                        width = NULL, 
                        background = "light-blue",
                        "In general 

the inner light blue dot shows the Median of measured historical power usage 
between 2007 and Dec. 2010 within a Violin plot including the entire distribution 
of measurements.

Wider sections of the violin plot represent a higher probability of observations 
taking a given value, the thinner sections correspond to a lower probability.

Power usage in the kitchen is historically more in the weekends with the biggest 
variance in scope on Saturday and the most use at Sunday. 

Power usage in the Laundry area is also the highest in the weekends with a 
variance in scope for usage more focussed on Friday and Sunday. 


The use of water heater and airconditioner is steady and high throughout the 
                        week with highest use on Sunday and lowest on Wednesday. 
                  The largest variance in scope is on Thursday and Tuesday."
                      )
                    ),
                        box(solidHeader = TRUE,
                            collapsible = TRUE,
                          plotOutput(outputId = "plotGA", height = "500px")
                        ),
                        box(solidHeader = TRUE,
                            collapsible = TRUE,
                          plotOutput(outputId = "plotSM1", height = "500px")
                        ),
                    column(width = 6,
                           box(
                             title = "Conclusions YoY trend TOTAL", 
                             width = NULL, 
                             background = "light-blue",
                             "Starting April 2011 

a decrease in total (Global Active Power) is predicted that continues roughly
till November, where high usage is predicted, and for December 2011. Summer months
months 2008 & 2010 show a similar trend."
                           )
                    ),
                    column(width = 6,
                           box(
                             title = "Conclusions YoY trend KITCHEN per weekday
                             (2007-Dec 2010)", 
                             width = NULL, 
                             background = "light-blue",
                             "Predictions for the kitchen
                             
are calculated with mean and looking at historical usage. Predictions based on 
this show that kitchen appliances usage on Friday are expected to increase 
significantly and compared to other years"
                           )
                    ),
                        box(solidHeader = TRUE,
                            collapsible = TRUE,
                          plotOutput(outputId = "plotSM2", height = "500px")
                        ),
                        box(solidHeader = TRUE,
                            collapsible = TRUE,
                          plotOutput(outputId = "plotSM3", height = "500px")
                        ),
                    column(width = 6,
                           box(
                             title = "Conclusions YoY trend LAUNDRY per weekday", 
                             width = NULL, 
                             background = "light-blue",
                             "YoY a decreasing usage of power in the laundry area
                             is expected for all weekdays."
                           )
                    ),
                    column(width = 6,
                           box(
                             title = "Conclusions YoY trend Climate control per weekday
                             (2007-Dec 2010)", 
                             width = NULL, 
                             background = "light-blue",
                             "For Climate control 
                             
especially usage on Thurs. and Fri. are expected to decrease. With highest 
                             usage in the weekend."
                           )
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
      stat_summary(fun.data=mean_sdl, mult=1, 
                   geom="pointrange", color="lightblue") + 
      theme_minimal()
  })
  
  output$scatter_violin_2 <- renderPlot({
    ggplot(data=daily, aes(x=wday, y=Sub_metering_2)) +
      geom_violin(aes(fill=wday)) +
      stat_summary(fun.data=mean_sdl, mult=1, 
                   geom="pointrange", color="lightblue") + 
      theme_minimal()
  })
  
  output$scatter_violin_3 <- renderPlot({
    ggplot(data=daily, aes(x=wday, y=Sub_metering_3)) +
      geom_violin(aes(fill=wday)) +
      stat_summary(fun.data=mean_sdl, mult=1, 
                   geom="pointrange", color="lightblue") + 
      theme_minimal()
  })
  
  output$Violin_1_mean <- renderPlot({
    ggplot(data=New_Total_SM1_mean, aes(x = wday, group = wday)) +
             geom_violin(aes(y = mean), fill = "blue", alpha = 0.6) + 
             geom_violin(aes(y = pred), fill = "red", alpha = 0.6)
  })
  
  output$Violin_2_mean <- renderPlot({
      ggplot(data=New_Total_SM2_mean, aes(x = wday, group = wday)) +
      geom_violin(aes(y = mean), fill = "orange", alpha = 0.6) + 
      geom_violin(aes(y = pred), fill = "yellow", alpha = 0.6)
  })
  
  output$Violin_3_mean <- renderPlot({
    ggplot(data=New_Total_SM3_mean, aes(x = wday, group = wday)) +
      geom_violin(aes(y = mean), fill = "green", alpha = 0.6) + 
      geom_violin(aes(y = pred), fill = "turquoise", alpha = 0.6)
  })
  
  output$plotGA <- renderPlot({
    plot(Future_season_GA, 
         main = "Forecast GA", 
         xlab = "Date", 
         ylab = "Amount", 
         lwd = 3)
  })
  
  output$plotSM1 <- renderPlot({
    plot(SeasonplotSM1_mean, 
         main = "Forecast SM1", 
         xlab = "Date", 
         ylab = "Amount", 
         lwd = 3)
  })
  
  output$plotSM2 <- renderPlot(
    plot(SeasonplotSM2_mean, 
         main = "Forecast SM2", 
         xlab = "Date", 
         ylab = "Amount", 
         lwd = 3)
  )
  
  output$plotSM3 <- renderPlot({
    plot(SeasonplotSM3_mean, 
         main = "Forecast SM3", 
         xlab = "Date", 
         ylab = "Amount", 
         lwd = 3)
  })
}

shinyApp(ui, server)                  
 
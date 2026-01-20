## app.R ##

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(dplyr)
library(reshape2)
library(RColorBrewer)


## Import dataframe
EV <- read.csv("C:\\Users\\Nadhirah\\OneDrive\\Documents\\KAKAK\\PROJECTS\\Electric Vehicle Project\\ElectricCarData_Clean.csv")
colSums(is.na(EV))
EV[is.na(EV)] <- 0

# Define the colors for the plots
plot_colors <- scale_fill_manual(values = c("#FF9999", "#FF6666", "#FF3333", "#CC0000", "#990000"))

ui <- dashboardPage(
  skin = "blue",  # Choose a color scheme for the dashboard
  dashboardHeader(title = "Electric Vehicle Dashboard"),
  dashboardSidebar(
    pickerInput(
      "brand", "Choose a car brand", choices = sort(unique(EV$Brand)),
      options = list(`actions-box` = TRUE),  # Enable "Select All" feature
      multiple = TRUE  # Allow multiple selections
    ),
    sliderInput("Price", "Under Price Selected in (Euro):", min = min(EV$PriceEuro), max = max(EV$PriceEuro), value = mean(EV$PriceEuro)),
    selectInput("RapidCharge", "Available with Rapid Charge?:", choices = unique(EV$RapidCharge))
  ),
  dashboardBody(
    tabBox(
      id = "tabset1", height = "100%", width = "100%",
      tabPanel("Page 1: Basic Description Dataset", fluidPage(
        plotOutput("plot1", height = "35vh"),
        plotOutput("plot2", height = "40vh"))),
      tabPanel("Page 2: Price Analysis by Continent", fluidPage(
        selectInput("Continent", "Select Continent:", choices = unique(EV$Continent)),
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"),
            plotOutput("plot4", height = "35vh"),
            plotOutput("plot5", height = "35vh")
          )
        ),
        plotOutput("plot3", height = "35vh")
      )),
      tabPanel("Page 3: Performance Analysis with Power Train", fluidPage(
        splitLayout(
          cellWidths = c("50%", "50%"),
          fluidRow(
            plotOutput("plot6", height = "35vh"),
            plotOutput("plot7", height = "40vh")
          ),
          plotOutput("plot8", height = "75vh")
        )
      )),
      tabPanel("Page 4: Efficiency Analysis with Plug Type", fluidPage(
        plotOutput("plot9", height = "40vh"),
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"),
            plotOutput("plot10", height = "35vh"),
            plotOutput("plot11", height = "35vh")
          )
      ))
    )
  ),
  tags$head(
    tags$style(HTML("
      .tab-content {
        height: calc(70vh - 70px) !important;
      }
      .tab-pane {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100%;
      }
      .skin-blue .main-header .logo {
        background-color: #003366;
        color: #FFFFFF;
        font-size: 24px;
        font-weight: bold;
      }
      .skin-blue .main-header .navbar {
        background-color: #003366;
      }
      .skin-blue .main-sidebar {
        background-color: #001f3f;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #001f3f;
        color: #FFFFFF;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a {
        background-color: #001f3f;
        color: #FFFFFF;
      }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
        background-color: #001f3f;
        color: #FFFF00;
      }
      .box {
        border-top-color: #003366;
      }
    "))
  ))
)

server <- function(input, output) {
  # Filter data based on selected brand, price, and rapid charge availability
  filtered_data <- reactive({
    EV %>%
      filter(Brand %in% input$brand, PriceEuro <= input$Price, RapidCharge == input$RapidCharge)
  })
  
  # Create a bar plot for Power Train distribution
  output$plot1 <- renderPlot({
    agg1 <- EV %>%
      count(Brand) %>%
      top_n(10, wt = n) 
    ggplot(data = agg1, aes(x = reorder(Brand, -n), y = n,fill= Brand)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Top 10 Frequently Brands of Electric Vehicles",
           x = "Brand", y = "Number of Models")
  })
  
  # Create a histogram for Efficiency distribution
  output$plot2 <- renderPlot({
    ggplot(EV, aes(x = PriceEuro)) +
      geom_histogram(binwidth = 10000, color = "steelblue", fill = "skyblue") +
      labs(title = "Distribution of Price of Electric Vehicles in Euro",
           x = "Price in Euro",
           y = "Frequency")
  })
  
  # Create a histogram for Price by Body Style
  output$plot3 <- renderPlot({
    filtered_body_data <- EV %>% filter(Continent == input$Continent)
    ggplot(filtered_body_data, aes(x = PriceEuro)) +
      geom_histogram(binwidth = 10000, color = "black", fill = "turquoise") +
      labs(
        title = paste("Price Distribution for", input$Continent),
        x = "Price in Euros",
        y = "Frequency"
      )
  })
  
# Create a bar plot for Brand vs Average Price with color based on selected Continent
output$plot4 <- renderPlot({
  filtered_data <- EV %>%
    filter(Continent == input$Continent)
  colourCount <- length(unique(filtered_data$Brand))
  getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
  
  agg3 <- filtered_data %>%
    group_by(Brand) %>%
    summarise(AvgPrice = mean(PriceEuro, na.rm = TRUE))
  
  ggplot(data = agg3, aes(x = reorder(Brand, AvgPrice), y = AvgPrice, fill = Brand)) +
    geom_bar(stat = "identity") +
    labs(
      title = paste("Average Price of Electric Vehicles by Brand in", input$Continent),
      x = "Brand",
      y = "Average Price (Euro)"
    ) +
    theme(legend.position = "right")+
    scale_fill_manual("Brand", values = getPalette(colourCount))
})

  output$plot5 <- renderPlot({
    filtered_body_data2 <- EV %>% filter(Continent == input$Continent)
    agg4 <- filtered_body_data2 %>%
      count(BodyStyle)
    df <- data.frame(BodyStyle = agg4$BodyStyle, n = agg4$n)
    ggplot(df, aes(x = "", y = n, fill = BodyStyle)) +
      geom_col(color = "black") +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +  
      geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
      ggtitle(paste("Body Style Designed in Electric Vehicles in", input$Continent))
  })
  
  # Create a pie chart for Power Train distribution
  output$plot6 <- renderPlot({
    agg5 <- filtered_data() %>%
      count(PowerTrain)
    df <- data.frame(PowerTrain = agg5$PowerTrain, n = agg5$n)
    ggplot(df, aes(x = "", y = n, fill = PowerTrain)) +
      geom_col(color = "black") +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Set3") +  
      geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
      ggtitle("Power Train Used in Selected Electric Vehicles")
  })
  
  # Create a scatter plot for Price vs. Range
  output$plot7 <- renderPlot({
    df2 <- filtered_data()
    ggplot(df2, aes(x = PriceEuro, y = Range_Km)) +
      geom_point(color = "orange", size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, color = "red", linetype = "dashed") +
      labs(
        title = "Price vs. Range",
        x = "Price (Euro)",
        y = "Range to Drive (Km)"
      )
  })
  
  # Create a boxplot for Range grouped by Power Train
  output$plot8 <- renderPlot({
    ggplot(filtered_data(), aes(x = PowerTrain, y = Range_Km)) +
      geom_boxplot(fill = "darkred", color = "black") +
      xlab("Power Train") +
      ylab("Range in Km") +
      ggtitle("Boxplot of Range (Km) Grouped by Power Train")
  })
  
  # Create a boxplot for Fast Charge Rate grouped by Plug Type
  output$plot9 <- renderPlot({
    ggplot(filtered_data(), aes(y = PlugType, x = FastCharge_KmH)) +
      geom_boxplot(fill = "darkorange", color = "black") +
      ylab("Plug Type") +
      xlab("Fast Charge Rate in Km/H") +
      ggtitle("Boxplot of Fast Charge Rate (Km/H) Grouped by Plug Type")
  })
  
    output$plot10<-renderPlot({
        agg7 <- filtered_data() %>%
          count(PlugType)
        data1 <- data.frame(PlugType = agg7$PlugType, countType = agg7$n)
        ggplot(data1, aes(x = PlugType, y = countType, fill = PlugType)) +
          geom_bar(stat = "identity")+
          scale_fill_brewer(palette = "Set2") +  
          geom_text(aes(label = countType), position = position_stack(vjust = 0.5)) +
          ggtitle("Plug Type Used in Selected Electric Vehicles")
    })
    
    output$plot11 <- renderPlot({
      df3 <- filtered_data()
      ggplot(df3, aes(x = Efficiency_WhKm, y = Range_Km)) +
        geom_point(color = "darkgreen", size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "darkblue", linetype = "dashed") +
        labs(
          title = "Efficiency Power Usage vs. Range to Drive",
          x = "Efficiency Power Usage (Wh/Km)",
          y = "Range to Drive (Km)"
        )
    })
}

shinyApp(ui, server)

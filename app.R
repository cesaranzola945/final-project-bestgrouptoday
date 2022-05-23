
# link of the shiny app for the final project
# https://cesaranzola945.shinyapps.io/Final_project_DataandProg/

library(tidyverse)
library(shiny)
library(plotly)
library(scales)
library(DT)
library(lubridate)
library(WDI)
library(ggthemes)
library(rsconnect)

# # set path
# setwd("D:/Users/ASUS/Documents/Universidades/University of Chicago/U Chicago - Harris/Academics/Winter 2022/Data and Programing for Public Policy II/Class materials/Class 8 Shiny")

# variables WDI to choose
var_display <- c("co2_emissions","unemployment","female_labor_force","female_male_labor_rat") 

# variables Refinitiv to choose
var_display_refin <- c("esg_score","emissions_scope1","emissions_total","water_to_revenue","coal_prodn","women_overall_pct","women_board_pct","salary_gap","waste_reduction") 


# load database from refinitiv
load("Data/data_refinitiv.Rda")

data_fin$year<-as.Date(ISOdate(data_fin$year, 1, 1))
country_list <- unique(data_fin$country)

# load database from world bank WDI and adjust it to match refinitiv data
date0 <- 2017
date1 <- 2021

indicators<- c("EN.ATM.CO2E.PC","SL.UEM.TOTL.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.CACT.FM.ZS")
data_wdi <- WDI(country = "all", indicator = indicators, start = date0, end = date1, extra = TRUE)

data_wdi <-
data_wdi %>% 
  mutate(country = ifelse(country == "Russian Federation" ,"Russia",country),
         country = ifelse(country == "Korea, Rep." ,"South Korea",country),
         country = ifelse(country == "Hong Kong SAR, China" ,"Hong Kong",country))

data_wdi <-
data_wdi %>% 
  filter(country %in% country_list)

data_wdi <- 
data_wdi %>% 
  rename("co2_emissions"       = "EN.ATM.CO2E.PC",
          "unemployment"       = "SL.UEM.TOTL.ZS",
          "female_labor_force" = "SL.TLF.CACT.FE.ZS",
          "female_male_labor_rat" = "SL.TLF.CACT.FM.ZS")

data_wdi$year<-as.Date(ISOdate(data_wdi$year, 1, 1))

ui <- fluidPage(
  # Title 
  h1(id="big-heading", "ESG Metrics Observatory",align = "center"),
  tags$style(HTML("#big-heading{color: green;}")),
fluidRow(
  fluidRow(
    column(width = 6,
           tags$img(src = "https://d11jve6usk2wa9.cloudfront.net/platform/10747/assets/logo.png",
                    height = 70,
                    width = 150),
    ),
    column(width = 6,
           tags$img(src = "https://d11jve6usk2wa9.cloudfront.net/platform/10747/assets/logo.png",
                    height = 70,
                    width = 150))
  ),
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        'dateRange_WDI',
        label = 'Date Range - WDI',
        start = "2017/01/01", end = "2021/01/01",
        min = "2017/01/01", max = "2021/01/01",
        separator = " to ", format = "yyyy/mm/dd"
      ),
      dateRangeInput(
        'dateRange_ESG',
        label = 'Date Range - ESG Metrics',
        start = "2017/01/01", end = "2021/01/01",
        min = "2017/01/01", max = "2021/01/01",
        separator = " to ", format = "yyyy/mm/dd"
        ),
       selectInput("country_n", "Country:", 
                  choices=country_list
        ),
      selectInput("var_wdi", "WDI ESG Metric:", 
                  choices=var_display
      ),
      selectInput("var_refi", "Company ESG Metric:", 
                  choices=var_display_refin
      ),
      p(strong("Dashbord Instructions")),
      p("This dashbord is design to study Environmental Social and Governance (ESG) metrics that are used by 
        investors to study private companies and financial funds. In this dashbord you can find 1) The world development indicators by the World bank
        2) Company level ESG metrics and polynomial regression of its trend in time 3) an scatter plot between the metrics and their ESG score that is
        used by financial markets to target companies with sustainable vision"),
      p(strong("Dates Instruction")),
      p("Given that the data is in a yearly frequency to change the dates please ONLY select the first day of every year")
      ),
    mainPanel(
      plotlyOutput(outputId = "WDItimeSeries"),
      plotlyOutput(outputId = "timeSeries"),
      plotlyOutput(outputId = "scatterplot"),
    )
  )
  )
)

server <- function(input, output) {
  
  # Time series of World Bank metrics
  output$WDItimeSeries <- renderPlotly({
    
    df<-
      data_wdi %>%
      filter(year>=as.double(input$dateRange_WDI[1]) & year<=as.double(input$dateRange_WDI[2])) %>%   
      filter(country == input$country_n)
    
    plt<-ggplot(data = df)+
      geom_line(aes(x = year, y = .data[[input$var_wdi]]))+
      labs(title = "World Development Indicator Variable",
           caption = "Information source from World Bank WDI")+
      xlab("Date")+
      ylab("Metric")+
      scale_x_continuous(breaks = unique(df$year))+
      theme_economist()+
      theme(plot.title = element_text(hjust = 0.5,size = 20),
            axis.title = element_text(size = 15),
            plot.caption = element_text(size = 10))
    
    ggplotly(plt)
  })
  
  # Time series of ESG metrics
  output$timeSeries <- renderPlotly({
    
    df<-
    data_fin %>%
      filter(year>=as.double(input$dateRange_ESG[1]) & year<=as.double(input$dateRange_ESG[2])) %>%   
      filter(country == input$country_n)
    
    plt<-ggplot(data = df)+
      geom_smooth(aes(x = year, y = .data[[input$var_refi]]))+
      geom_point(aes(x = year, y = .data[[input$var_refi]]))+
      labs(title = "ESG Metric",
           caption = "The black dots are companies observations for the variable. Source: REFINITIV")+
      xlab("Date")+
      ylab("Metric")+
      scale_x_continuous(breaks = unique(df$year))+
    theme_economist()+
      theme(plot.title = element_text(hjust = 0.5,size = 20),
            axis.title = element_text(size = 15),
            plot.caption = element_text(size = 10))
    
    ggplotly(plt)
  })
  
  # Scatter plot between ESG score and selected variable
  output$scatterplot <- renderPlotly({
    
    df<-
      data_fin %>%
      filter(year==as_date("2021/01/01"))
    
plt <- ggplot(data = df)+
      geom_point(aes(x = .data[[input$var_refi]], y = esg_score))+
      geom_smooth(aes(x = .data[[input$var_refi]], y = esg_score),method = lm,color = "red",se = FALSE)+
      labs(title = "ESG score VS Company ESG Metric (2021,All countries)",
           caption = "Information source from Refinitiv. Shows scatter plot between ESG score and selected variable")+
      xlab("Company ESG Metric")+
      ylab("ESG Score")+
      theme_economist()+
      theme(plot.title = element_text(hjust = 0.5,size = 16),
            axis.title = element_text(size = 12),
            plot.caption = element_text(size = 12))
    
    ggplotly(plt)
  })
}

shinyApp(ui = ui, server = server)

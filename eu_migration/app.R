#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(eurostat)
library(ggplot2)
library(tidyverse)
library(sf)

# Downloading and manipulating the tabular data
data <- get_eurostat("migr_imm8", time_format = "num") %>%
  # Subset to NUTS-3 level
  dplyr::filter(age == "TOTAL", sex== "T", agedef == "COMPLET") 
names(data)[names(data) == "geo"] <- "id"

# Downloading geospatial data
geodata <- read_sf("https://raw.githubusercontent.com/eurostat/Nuts2json/master/pub/v2/2021/3035/20M/0.json")

# Merge with attribute data with geodata
maps <- inner_join(geodata, data, by="id")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Immigration by Age and Sex"),

    # Select input for the chosen country and year
    sidebarLayout(
        sidebarPanel(
            numericInput("TIME_PERIOD",
                      "Year",
                      value=2010),
            selectInput("id",
                        "Country",
                        choices = list(
                          "Austria" =	"AT",	
                          "Belgium" =	"BE",	
                          "Bulgaria" =	"BG",
                          "Switzerland" = "CH",
                          "Cyprus" =	"CY",
                          "Czech Republic" =	"CZ",
                          "Germany" = "DE",
                          "Denmark" =	"DK",
                          "Estonia" =	"EE",
                          "Greece" = "EL",
                          "Spain" =	"ES",
                          "Finland"	= "FI",
                          "France" =	"FR",
                          "Croatia" = "HR",
                          "Hungary" =	"HU",
                          "Ireland" = "IE",
                          "Iceland" = "IS",
                          "Italy" = "IT",
                          "Liechtenstein" = "LI",
                          "Lithuania"	= "LT",
                          "Luxembourg" = "LU",
                          "Latvia" =	"LV",
                          "North Macedonia" = "MK",
                          "Malta"	= "MT",
                          "Netherlands" =	"NL",
                          "Norway" = "NO",
                          "Poland" =	"PL",
                          "Portugal" =	"PT",
                          "Romania" =	"RO",
                          "Sweden"	= "SE",
                          "Slovenia" =	"SI",
                          "Slovakia" =	"SK",
                          "United Kingdom"	= "UK"))
        ),

        # Show a plot of the country and year
        mainPanel(
          plotOutput(outputId = "map"),
          plotOutput(outputId = "timetrend"),
        )
    )
)

# Define server logic required 
server <- function(input, output) {

  output$timetrend <- renderPlot({
    ggplot(maps%>%filter(id == input$id),aes(x=TIME_PERIOD,y=values))+
      geom_line(aes(colour = "blue"))+
      labs(title = "Trend of Immigration of the EU countries", 
           x = "Year", y = "Frequency")
  })

  output$map <- renderPlot({
    ggplot(maps%>%filter(TIME_PERIOD == input$TIME_PERIOD))+
      geom_sf(aes(fill=values))+
      scale_fill_gradient(low = "red", high = "green", name = "Number of Immigrants", label = scales::comma)+
      labs(title = "The Number of Immigrants in a Year")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

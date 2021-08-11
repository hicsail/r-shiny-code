library(shiny)
library(httr)
library(jsonlite)
library(dplyr)


# Define UI ----
ui <- fluidPage(
  titlePanel("Get Covid-19 Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose Country:", c(
        "United States" = "united-states",
        "China" = "china",
        "United Kingdom" = "united-kingdom"
      )),
      dateRangeInput("dates", label = h3("Date range")),
      
      hr(),
      fluidRow(column(4, verbatimTextOutput("value")))
    ),
    mainPanel(
      textOutput("data")
    )
  ),
)


covid_api <- function(path) {
    url <- modify_url("https://api.covid19api.com", path = path)
    # url <- "https://api.covid19api.com/status/404"
    
    resp <- RETRY("GET", url=url, times = 5, pause_base = 1, pause_cap = 1)
    
    if (http_error(resp)) {
      x <- data.frame("CountryName" = "", "RecentCases" = 0, "Status" = resp$status)
    
    } else {
      this.raw.content <- rawToChar(resp$content)
      this.content <- fromJSON(this.raw.content)
      allCases <- this.content$Cases
      recentCases <- tail(allCases, n=1)
      countryName <- this.content$Country[1]
      
      x <- data.frame("CountryName" = countryName, "RecentCases" = recentCases, "Status" = resp$status)
      
    }
}


# Define server logic ----
server <- function(input, output) {
  
  output$data <- renderText({
    countrySlug <- switch(input$country,
      "united-states" = "united-states",
      "china" = "china",
      "united-kingdom" = "united-kingdom")
    
    firstDate <- input$dates[1]
    secondDate <- input$dates[2]
    
    path <- paste("/total/country/", countrySlug, "/status/confirmed?from=", firstDate, "00:00:00Z&to=", secondDate, "T00:00:00Z", collapse = NULL)
    path <- gsub(" ", "", path)
    
    c <- covid_api(path=path)
    
    if (c$Status == 200) {
      paste("There have been", c$RecentCases, "total cases in", c$CountryName, "between", firstDate, "and", secondDate)
    } else {
      paste(http_status(c$Status))
    }
    })
  }

# Run the app ----
shinyApp(ui = ui, server = server)


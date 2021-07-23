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
      ))
    ),
    mainPanel(
      textOutput("data")
    )
  ),
)

# Define server logic ----
server <- function(input, output) {
  
  output$data <- renderText({
    url  <- "https://api.covid19api.com"
    countrySlug <- switch(input$country,
      "united-states" = "united-states",
      "china" = "china",
      "united-kingdom" = "united-kingdom")
    
    path <- paste("total/country/", countrySlug, "/status/confirmed?from=2021-07-20T00:00:00Z&to=2021-07-21T00:00:00Z", collapse = NULL)
    path <- gsub(" ", "", path)
    
    timoutepath <- "http://httpbin.org/delay/5"
    
    
    for (i in 1:5) {
      raw.result <- GET(url=url, path=path, timeout(5))
      status <- status_code(raw.result)
      if (status == 200) {
        this.raw.content <- rawToChar(raw.result$content)
        this.content <- fromJSON(this.raw.content)
        allCases <- this.content$Cases
        recentCases <- tail(allCases, n=1)
        countryName <- this.content$Country[1]
        timeoutTruth <- FALSE
        break;
      }
      timeoutTruth <- TRUE
      next
    }
    Sys.sleep(1)
    print(class(timeoutTruth))
    
    if (timeoutTruth == TRUE) {
      paste("You have timed out. Status code: ", status)
    } else {
      paste("You have", toString(recentCases), "cases in", countryName)
    }
    
    })
  }

# Run the app ----
shinyApp(ui = ui, server = server)


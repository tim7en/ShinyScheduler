library(shiny)
library(openair)
library(chron)
library(openair)
source("functions.R")

if (interactive()) {
  ui <- fluidPage(
    fluidRow(
      column(
        width = 4,
        br(),
        verbatimTextOutput("welcome"),
        uiOutput("Nurses"),
        DTOutput("nurse_state")
      ),
      column(
        width = 4,
        br(),
        verbatimTextOutput("atwork"),
        verbatimTextOutput("athome"),
        dateRangeInput("daterange", "Date range:"),
        verbatimTextOutput("range"),
        br(),
        plotOutput("calenPlot")
      ),
      column(
        width = 4,
        br(),
        verbatimTextOutput("callin_text"),
        br(),
        numericInput("N", "Number", 0),
        br(),
        verbatimTextOutput("callin_names")
      )
    )
  )

  server <- function(input, output) {
    l <<- list("Ray Labayen", "Lola Labayen", "Timur Sabitov", "Harper Weiczorak", "Michael Weiczorak")
    names(l) <<- l
    l[c(1, 2, 3)] <<- lapply(l[c(1, 2, 3)], copy.df_1)
    # resting nurses
    l[c(4, 5)] <<- lapply(l[c(4, 5)], copy.df_0)


    output$range <- renderPrint({
      print(paste0("From: ", as.character(input$daterange[1])))
      print(paste0("To: ", as.character(input$daterange[2])))
    })


    output$Nurses <- renderUI({
      selectInput(inputId = "xE", label = "Active nurses", choices = names(l))
    })


    nurse_state <- reactive({
      req(input$xE)
      l[[which(names(l) == input$xE)]]
    })

    output$nurse_state <- renderDT({
      nurse_state()
    })

    output$welcome <- renderText({
      ("Welcome, this app is a prototype of scheduling app. It is designed for hospitals, and this is an example.")
    })

    atwork <- reactive({
      today_shift <- c("Ray Labayen", "Lola Labayen", "Timur Sabitov")
      c <- paste0("Today in the hospital these nurses are active: ", paste(today_shift, collapse = ", "))
    })

    output$atwork <- renderText({
      atwork()
    })

    athome <- reactive({
      today_home <- c("Harper Weiczorak", "Michael Weiczorak")
    })

    output$athome <- renderText({
      c <- paste0("Today these nurses are vacant: ", paste(athome(), collapse = ", "))
    })


    callin_text <- reactive({
      "How many nurses would you like to call in ?"
    })

    output$callin_text <- renderText({
      callin_text()
    })

    # Several issues

    # Need to filter out dates, do not select last year January as well.
    callin <- reactive({
      if (input$N == 0) {

      } else {
        call.in <- function(x) { # Based on least overtime
          x <- x[x[, 1] >= input$daterange[1] & x[, 1] <= input$daterange[2], ]
          length(which(x[, 2] == 4)) # Overtime
        }
        nurses_to_call <- lapply(l[which(names(l) %in% athome())], call.in)
        paste0("Call for: ", paste(names(sort(unlist(nurses_to_call), decreasing = FALSE)[c(1:input$N)])), collapse = ", ")
      }
    })

    output$callin_names <- renderText({
      callin()
    })

    output$calenPlot <- renderPlot({
      datRange <- seq(input$daterange[1], input$daterange[2], by = 1)
      year_selected <- format(input$daterange[1], "%Y")
      date <- seq(as.Date(paste(year_selected, "/1/1", sep = "")), as.Date(Sys.time()), "days")
      state <- replicate(length(date), 0)
      my.df <- cbind.data.frame(date, state)
      my.df$state[which(my.df$date %in% datRange)] <- 1
      calendarPlot(my.df,
        cols = c("lightblue", "orange"),
        breaks = c(0, 1),
        pollutant = "state", year = c(year_selected, format(Sys.time(), "%Y"))
      )
    })
  }

  shinyApp(ui, server)
}

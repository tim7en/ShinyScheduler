library(shiny)
library(openair)
library(chron)
library(openair)
library(DT)
library(dplyr)
library(dygraphs)
library(xts)
library(crayon)
source("functions.R")

if (interactive()) {
  ui <- fluidPage(
    fluidRow(
      style = "height:40vh",
      column(
        width = 4,
        br(),
        htmlOutput("welcome"),
        br(),
        uiOutput("Nurses"),
        fluidRow(
          style = "background-color:#99FFFF;",
          column(
            2,
            align = "center",
            htmlOutput("text1")
          ),
          column(
            2,
            align = "center",
            htmlOutput("text2")
          ),
          column(
            2,
            align = "center",
            htmlOutput("text3")
          ),
          column(
            3,
            align = "center",
            htmlOutput("text4")
          ),
          column(
            2,
            align = "center",
            htmlOutput("text5")
          )
        ),
        br(),
        tabsetPanel(
          tabPanel("Nurse history", DT::DTOutput("nurse_state"), style = "height:auto;"),
          tabPanel("Plot last 7 days", dygraphOutput("nurse_state_plot"))
        )
      ),
      column(
        width = 4,
        br(),
        verbatimTextOutput("atwork"),
        textOutput("atwork_names"),
        tags$head(tags$style("#atwork_names{color: blue;
                                 font-size: 12px;
                             font-style: italic;
                            }")),

        br(),
        verbatimTextOutput("athome"),
        textOutput("athome_names"),
        tags$head(tags$style("#athome_names{color: green;
                                 font-size: 12px;
                             font-style: italic;
                            }")),
        br(),
        verbatimTextOutput("callin_text"),
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: blue}")),
        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: purple}")),

        fluidRow(
          column(
            width = 7,
            sliderInput("N", "Call in", 0, 10, 0, 1)
          ),
          column(
            br(),
            br(),
            width = 2, align = "center",
            actionButton("Submit1", "Submit"), # updated from July 28
            br()
          ),
          column(
            br(),
            br(),
            width = 2, align = "center",
            actionButton("Cancel1", "Cancel"),
            br()
          )
        ),
        verbatimTextOutput("callin_names"),
        br(),
        verbatimTextOutput("sendhome_text"),

        fluidRow(
          column(
            width = 7,
            sliderInput("N_2", "Send home", 0, 10, 0, 1)
          ),
          column(
            br(),
            br(),
            width = 2, align = "center",
            actionButton("Submit2", "Submit"), # updated from July 28
            br()
          ),
          column(
            br(),
            br(),
            width = 2, align = "center",
            actionButton("Cancel2", "Cancel"),
            br()
          )
        ),

        br(),
        verbatimTextOutput("sendhome_names"),
        br()
      ),
      column(
        width = 4,
        dateRangeInput("daterange", "Date range:"),
        verbatimTextOutput("range"),
        br(),
        plotOutput("calenPlot")
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
      selectInput(inputId = "xE", label = "Active nurses, history", choices = names(l))
    })


    nurse_state <- reactive({
      req(input$xE)
      l <- l[[which(names(l) == input$xE)]]
      return(l)
    })

    # Colors, numbers and description (0 - was off, 1 - was active, 2 - on hold, 3 - sent home, 4 - overtime (call in))
    output$nurse_state <- renderDT({
      datatable(nurse_state(), options = list(pageLength = 10)) %>%
        formatStyle("state", target = "row", backgroundColor = styleEqual(c(0, 1, 2,3, 4 ), c("#fc8a8a", "green","yellow", "orange","blue"))) #
    })

    output$nurse_state_plot <- renderDygraph({
      data <- nurse_state()
      data <- as.xts(x = data$state, order.by = data$date)

      t <- format(Sys.Date(), "%Y-%m-%d")
      t_7 <- format(Sys.Date() - 7, "%Y-%m-%d")

      p <- dygraph(data, main = "Nurse activity") %>%
        dySeries("V1", label = "Activity State") %>%
        dyOptions(stepPlot = TRUE, fillGraph = TRUE) %>%
        dyRangeSelector(dateWindow = c(t_7, t))
    })

    output$welcome <- renderText({
      "<span style=\"color:blue\">Welcome, this app is a prototype of scheduling app. It is designed for hospitals, and this is an example.</span>"
    })

    #
    output$text1 <- renderText({
      "<span style=\"color:green\">Active</span>"
    })
    output$text2 <- renderText({
      "<span style=\"color:red\">Vacant</span>"
    })
    output$text3 <- renderText({
      "<span style=\"color:blue\">Overtime</span>"
    })
    output$text4 <- renderText({
      "<span style=\"color:orange\">Sent home</span>"
    })
    output$text5 <- renderText({
      "<span style=\"color:yellow\">On call</span>"
    })

    atwork <- reactive({
      c <- paste0("Today in the hospital these nurses are active: ")
    })

    output$atwork <- renderText({
      atwork()
    })

    atwork_names <- reactive({
      today_shift <- c("Ray Labayen", "Lola Labayen", "Timur Sabitov")
    })

    output$atwork_names <- renderText({
      paste(atwork_names(), collapse = ", ")
    })

    athome <- reactive({
      today_home <- c("Harper Weiczorak", "Michael Weiczorak")
    })

    output$athome <- renderText({
      c <- paste0("Today these nurses are off: ")
    })

    output$athome_names <- renderText({
      paste0(athome(), collapse = ", ")
    })


    callin_text <- reactive({
      "How many nurses would you like to call in ?"
    })

    output$sendhome_text <- renderText({
      "How many nurses would you like to send home ?"
    })

    output$callin_text <- renderText({
      callin_text()
    })

    callin <- reactive({
      if (input$N == 0) {

      } else {
        
        call.in <- function(x) { # Based on least overtime
          if (x[nrow(x),2] == 2) { #If the nurse is on call, call her!
            return (99999)
          } else {
            x <- x[x[, 1] >= input$daterange[1] & x[, 1] <= input$daterange[2], ]
            length(which(x[, 2] == 4)) # Else, look at the overtime!
          }
        }
        
        nurses_to_call <- lapply(l[which(names(l) %in% athome())], call.in)
        names(sort(unlist(nurses_to_call), decreasing = FALSE)[c(1:input$N)])
      }
    })

    output$callin_names <- renderText({
      paste0(callin(), collapse = ", ")
    })

    sendhome <- reactive({
      if (input$N_2 == 0) {

      } else {
        send.home <- function(x) { # Based on overtime
          x <- x[x[, 1] >= input$daterange[1] & x[, 1] <= input$daterange[2], ]
          length(which(x[, 2] == 4)) # Overtime
        }
        nurses_to_send <- lapply(l[which(names(l) %in% atwork_names())], send.home)

        names(sort(unlist(nurses_to_send), decreasing = TRUE)[c(1:input$N_2)])
      }
    })

    output$sendhome_names <- renderText({
      paste0(sendhome(), collapse = ", ")
    })

    output$calenPlot <- renderPlot({
      datRange <- seq(input$daterange[1], input$daterange[2], by = 1)
      year_selected <- format(input$daterange[1], "%Y")
      date <- seq(as.Date(paste(year_selected, "/1/1", sep = "")), as.Date(Sys.time()), "days")
      state <- replicate(length(date), 0)
      my.df <- cbind.data.frame(date, state)
      my.df$state[which(my.df$date %in% datRange)] <- 1
      my.df <- my.df[my.df[, 1] >= input$daterange[1] & my.df[, 1] <= input$daterange[2], ]
      calendarPlot(my.df,
        cols = c("lightblue", "orange"),
        breaks = c(0, 1), key = FALSE,
        pollutant = "state", year = c(year_selected, format(Sys.time(), "%Y"))
      )
    })
  }

  shinyApp(ui, server)
}

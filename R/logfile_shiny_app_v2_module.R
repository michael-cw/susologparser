#' UI element for the shiny application to parse Survey Solutions log files
#'
#'
#' @keywords internal
#' @noRd


logFileAnalyzerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns('file1'), 'Choose ZIP File Containing Log Files', accept = '.zip'),
    shiny::selectizeInput(ns("event1"), "Event 1", choices = NULL),
    shiny::selectizeInput(ns("event2"), "Event 2", choices = NULL),
    shiny::selectizeInput(ns("event3"), "Event 3", choices = NULL),
    downloadButton(ns("downloadData"), "Download Selected Events")
  )
}

#' @keywords internal
#' @noRd

logFileAnalyzerUI_table <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("selectedEvents"))
  )
}

#' Server logic for the shiny application to parse Survey Solutions log files
#'
#'
#' @keywords internal
#' @noRd

logFileAnalyzer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values for the events, log data, and event choices
    logData <- reactiveVal(data.frame())
    eventChoices <- reactiveValues(event1 = NULL, event2 = NULL, event3 = NULL)

    # Observe file upload
    observe({
      file <- input$file1
      if (is.null(file)) return()

      waiter::waiter_show(
        color = "rgba(13, 71, 161, 0.7)",
        html = tagList(
          waiter::spin_fading_circles(),
          "Parsing Log Files ..."
        )
      )

      # Unzip and process each log file
      tempDir <- tempdir()
      unzip(file$datapath, exdir = tempDir)
      logFiles <- list.files(tempDir, pattern = "\\.log$", full.names = TRUE)

      allParsedEvents <- lapply(logFiles, function(logFile) {
        content <- readLines(logFile)

        parsedEvents <- list()
        currentMessage <- ""
        for (line in content) {
          if (grepl("^\\d{2}:\\d{2}:\\d{2}", line)) {
            if (currentMessage != "") {
              parsedEvents[[length(parsedEvents)]]$Message <- currentMessage
              currentMessage <- ""
            }
            parsedEvents[[length(parsedEvents) + 1]] <- list(
              Date = str_extract(line, "^\\d{2}:\\d{2}:\\d{2}"),
              Event = str_extract_all(line, "\\[([^]]+)\\]")[[1]],
              Message = str_trim(str_replace(line, ".*\\]\\s*", ""))
            )
          } else {
            if (!grepl(line, currentMessage, fixed = TRUE)) {
              currentMessage <- paste0(currentMessage, " ", line)
            }
          }
        }
        if (currentMessage != "") {
          parsedEvents[[length(parsedEvents)]]$Message <- currentMessage
        }

        parsedEventsDataFrame <- do.call(rbind, lapply(parsedEvents, function(e) {
          data.frame(
            FileName = tools::file_path_sans_ext(basename(logFile)),
            Date = e$Date,
            Event1 = e$Event[1],
            Event2 = e$Event[2],
            Event3 = e$Event[3],
            Message = e$Message,
            stringsAsFactors = FALSE
          )
        }))
        return(parsedEventsDataFrame)
      })

      eventsData <- do.call(rbind, allParsedEvents)
      logData(eventsData)

      eventChoices$event1 <- makeEventChoices(eventsData$Event1)
      shiny::updateSelectizeInput(session, "event1", choices = names(eventChoices$event1), server = TRUE)
      waiter::waiter_hide()
    })


    # Update event2 based on event1
    observeEvent(input$event1, {
      if (is.null(input$event1) || !input$event1 %in% names(eventChoices$event1)) {
        shiny::updateSelectizeInput(session, "event2", choices = setNames("", ""), server = TRUE)
        return()
      }
      selectedEvent1 <- eventChoices$event1[[input$event1]]
      subsetEvents <- logData()[logData()$Event1 == selectedEvent1, ]
      eventChoices$event2 <- makeEventChoices(subsetEvents$Event2)
      shiny::updateSelectizeInput(session, "event2", choices = names(eventChoices$event2), server = TRUE)
      # Reset event3 if event1 changes
      eventChoices$event3 <- makeEventChoices(subsetEvents$Event3)
      shiny::updateSelectizeInput(session, "event3", choices = names(eventChoices$event3), server = TRUE)
    })

    # Update event3 based on event2
    observeEvent(input$event2, {
      if (is.null(input$event2) || is.null(eventChoices$event2) || !input$event2 %in% names(eventChoices$event2)) {
        eventChoices$event3 <- NULL
        shiny::updateSelectizeInput(session, "event3", choices = setNames("", ""), server = TRUE)
        return()
      }
      selectedEvent1 <- eventChoices$event1[[input$event1]]
      selectedEvent2 <- eventChoices$event2[[input$event2]]
      subsetEvents <- logData()[logData()$Event1 == selectedEvent1 & logData()$Event2 == selectedEvent2, ]
      eventChoices$event3 <- makeEventChoices(subsetEvents$Event3)
      shiny::updateSelectizeInput(session, "event3", choices = names(eventChoices$event3), server = TRUE)
    })


    # Function to make event choices with counts and sorted
    makeEventChoices <- function(eventData) {
      if (any(is.na(eventData)) || length(eventData) == 0) {
        # Return an empty named vector when there are no events
        return(setNames("", ""))
      }
      eventCounts <- sort(table(eventData), decreasing = TRUE)
      setNames(object = names(eventCounts), nm = paste(names(eventCounts), "(", eventCounts, ")", sep = ""))
    }

    # Display selected events
    output$selectedEvents <- renderDT({
      filteredData <- logData()

      if (!is.null(input$event1) && input$event1 %in% names(eventChoices$event1)) {
        selectedEvent1 <- eventChoices$event1[[input$event1]]
        filteredData <- filteredData[filteredData$Event1 == selectedEvent1, ]
      } else {
        return(datatable(data.frame()))
      }

      if (!is.null(input$event2) && input$event2 %in% names(eventChoices$event2)) {
        selectedEvent2 <- eventChoices$event2[[input$event2]]
        filteredData <- filteredData[filteredData$Event2 == selectedEvent2, ]
      } else {
        return(datatable(data.frame()))
      }

      if (!is.null(input$event3) && !is.null(eventChoices$event3) && input$event3 %in% names(eventChoices$event3)) {
        selectedEvent3 <- eventChoices$event3[[input$event3]]
        filteredData <- filteredData[filteredData$Event3 == selectedEvent3, ]
      }
      req(nrow(filteredData)) > 0
      datatable(filteredData, options = list(order = list(list(1, 'asc'))), rownames = FALSE)
    })


    # Download handler for selected events
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("selected-events-", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        filteredData <- logData()
        if (!is.null(input$event1)) {
          selectedEvent1 <- eventChoices$event1[[input$event1]]
          filteredData <- filteredData[filteredData$Event1 == selectedEvent1, ]
        }
        if (!is.null(input$event2)) {
          selectedEvent2 <- eventChoices$event2[[input$event2]]
          filteredData <- filteredData[filteredData$Event2 == selectedEvent2, ]
        }
        if (!is.null(input$event3)) {
          selectedEvent3 <- eventChoices$event3[[input$event3]]
          filteredData <- filteredData[filteredData$Event3 == selectedEvent3, ]
        }
        write.csv(filteredData, file, row.names = FALSE)
      }
    )

  })
}



# library(shiny)
# library(stringr)
# library(DT)
# library(waiter)
#
# ui <- fluidPage(
#   waiter::use_waiter(),
#   titlePanel("Log File Event Analyzer"),
#   sidebarLayout(
#     sidebarPanel(
#       logFileAnalyzerUI("logFileModule") # Use the module UI here
#     ),
#     mainPanel(
#       logFileAnalyzerUI_table("logFileModule") # Use the table UI here
#     )
#   )
# )
#
# server <- function(input, output, session) {
#   logFileAnalyzer("logFileModule") # Call the module server
# }
#
# shinyApp(ui, server)

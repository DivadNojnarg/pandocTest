library(shiny)
library(bslib)
library(pandoc)
library(gh)
library(mirai)
library(DT)

addResourcePath("www", "res")

ui <- page_fluid(
  div(
    class = "d-flex justify-content-center",
    selectInput(
      inputId = "pandoc_version",
      label = "Select Pandoc Version",
      choices = NULL
    ),
    textOutput("pandoc_current")
  ),
  input_task_button("install", "Install"),
  actionButton("clear", "Clear Pandoc versions"),
  DTOutput("pandoc_table"),
  actionButton("convert", "Convert rtf from html")
)

server <- function(input, output, session) {
  installed_versions <- reactiveVal(pandoc_installed_versions())
  pandoc_current <- reactiveVal(as.character(pandoc_version()))

  # Install any existing system version
  if (
    !is.null(isolate(pandoc_current())) &&
      !pandoc_is_installed(isolate(pandoc_current()))
  ) {
    showNotification(sprintf(
      "Default Pandoc version %s not installed, installing now...",
      isolate(pandoc_current())
    ))
    pandoc_install(isolate(pandoc_current()))
  }

  install_pandoc <- ExtendedTask$new(function(...) {
    mirai(
      {
        pandoc::pandoc_install(v)
        while (!pandoc::pandoc_is_installed(v)) {
          FALSE
        }
        return(v)
      },
      ...
    )
  })

  bind_task_button(install_pandoc, "install")

  observe({
    updateSelectInput(
      session,
      "pandoc_version",
      choices = pandoc_available_releases()[
        !(pandoc_available_releases() %in% installed_versions())
      ]
    )
  }) |>
    bindEvent(installed_versions(), ignoreNULL = FALSE)

  observe({
    install_pandoc$invoke(v = input$pandoc_version)
    showNotification(sprintf(
      "Installing new pandoc version %s...",
      input$pandoc_version
    ))
  }) |>
    bindEvent(input$install)

  observe({
    pandoc_activate(install_pandoc$result(), quiet = TRUE)
    installed_versions(pandoc_installed_versions())
    pandoc_current(install_pandoc$result())
    showNotification(sprintf(
      "Pandoc version %s activated.",
      install_pandoc$result()
    ))
  }) |>
    bindEvent(install_pandoc$result())

  observe({
    lapply(pandoc_installed_versions(), pandoc_uninstall)
    installed_versions(pandoc_installed_versions())
    pandoc_current(NULL)
    showNotification("All Pandoc versions cleared.")
  }) |>
    bindEvent(input$clear)

  output$pandoc_table <- renderDT({
    data.frame(version = installed_versions())
  })

  output$pandoc_current <- renderText(
    pandoc_current() %||% "No Pandoc version activated."
  )

  observe({
    showNotification(sprintf(
      "Converting test file with pandoc version %s ...",
      pandoc_current()
    ))

    tryCatch(
      {
        with_pandoc_version(
          pandoc_current(),
          pandoc_convert(
            file = "./sample.rtf",
            from = "rtf",
            to = "htm",
            output = "res/sample.html"
          )
        )

        showModal(
          modalDialog(
            title = "Conversion Complete",
            tags$iframe(
              src = paste0("www/sample.html"),
              width = "100%",
              height = "500px"
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )
      },
      error = function(e) {
        str(e)
        showNotification(
          paste("Error during conversion."),
          type = "error"
        )
      }
    )
  }) |>
    bindEvent(input$convert)
}

shinyApp(ui, server)

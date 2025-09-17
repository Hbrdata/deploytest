#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      # golem::golem_welcome_page(),
      # Remove this line to start building your UI
      fluidPage(
        selectInput(
          "n_breaks",
          label = "Number of bins in histogram (approximate):",
          choices = c(10, 20, 35, 50),
          selected = 20
        ),

        checkboxInput(
          "individual_obs",
          label = strong("Show individual observations"),
          value = FALSE
        ),

        checkboxInput(
          "density",
          label = strong("Show density estimate"),
          value = FALSE
        ),

        plotOutput("main_plot", height = "300px"),

        # Display this only if the density is shown
        conditionalPanel(condition = "input.density == true",
                         sliderInput(
                           "bw_adjust",
                           label = "Bandwidth adjustment:",
                           min = 0.2, max = 2, value = 1, step = 0.2)
        )
      )


    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "deploytest"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}



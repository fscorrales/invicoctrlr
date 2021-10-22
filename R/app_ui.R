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
    shinydashboardPlus::dashboardPage(
      skin = "green",
      header = shinydashboardPlus::dashboardHeader(
        title = "R INVICO"
      ),
      sidebar = shinydashboardPlus::dashboardSidebar(
        shinydashboard::menuItem(
          text = "Actualizar",
          tabName = "boxes",
          icon = icon("briefcase")
        ),
        shinydashboard::menuItem(
          text = "Actualizar",
          tabName = "boxes2",
          icon = icon("amazon")
        ),
        id = "sidebar"
      ),
      body = shinydashboard::dashboardBody(),
      footer = shinydashboardPlus::dashboardFooter(
        left = "By Fernando S. Corrales",
        right = base::paste0("Version ", get_package_version())
      ),
      controlbar = shinydashboardPlus::dashboardControlbar(shinydashboardPlus::skinSelector())
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
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'invicoctrlr'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}


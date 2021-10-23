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
    bs4Dash::dashboardPage(
      title = "Basic Dashboard",
      fullscreen = TRUE,
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "R INVICO",
          color = "primary",
          href = "https://www.google.fr",
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
        ),
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = icon("bars"),
        controlbarIcon = icon("th"),
        fixed = FALSE,
        leftUi = tagList(
          bs4Dash::dropdownMenu(
            badgeStatus = "info",
            type = "notifications",
            bs4Dash::notificationItem(
              inputId = "triggerAction2",
              text = "Error!",
              status = "danger"
            )
          ),
          bs4Dash::dropdownMenu(
            badgeStatus = "info",
            type = "tasks",
            bs4Dash::taskItem(
              inputId = "triggerAction3",
              text = "My progress",
              color = "orange",
              value = 10
            )
          )
        ),
        rightUi = bs4Dash::dropdownMenu(
          badgeStatus = "danger",
          type = "messages",
          bs4Dash::messageItem(
            inputId = "triggerAction1",
            message = "message 1",
            from = "Divad Nojnarg",
            image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
            time = "today",
            color = "lime"
          )
        )
      ),
      sidebar = bs4Dash::dashboardSidebar(
        skin = "light",
        status = "primary",
        elevation = 3,
        bs4Dash::sidebarUserPanel(
          image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
          name = "Welcome Onboard!"
        ),
        bs4Dash::sidebarMenu(
          bs4Dash::sidebarHeader("Header 1"),
          bs4Dash::menuItem(
            "Item 1",
            tabName = "item1",
            icon = icon("sliders")
          ),
          bs4Dash::menuItem(
            "Item 2",
            tabName = "item2",
            icon = icon("id-card")
          )
        )
      ),
      controlbar = bs4Dash::dashboardControlbar(
        skin = "light",
        pinned = TRUE,
        collapsed = FALSE,
        overlay = FALSE,
        bs4Dash::controlbarMenu(
          id = "controlbarmenu",
          bs4Dash::controlbarItem(
            title = "Set Color",
            bs4Dash::skinSelector()
          ),
          bs4Dash::controlbarItem(
            "Item 2",
            "Simple text"
          )
        )
      ),
      footer = bs4Dash::dashboardFooter(
        left = "By Fernando S. Corrales",
        right = base::paste0("Version ", get_package_version())
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "item1",
            shiny::fluidRow(
              lapply(1:3, FUN = function(i) {
                bs4Dash::sortable(
                  width = 4,
                  p(class = "text-center", paste("Column", i)),
                  lapply(1:2, FUN = function(j) {
                    bs4Dash::box(
                      title = paste0("I am the ", j, "-th card of the ", i, "-th column"),
                      width = 12,
                      "Click on my header"
                    )
                  })
                )
              })
            )
          ),
          bs4Dash::tabItem(
            tabName = "item2",
            bs4Dash::box(
              title = "Card with messages",
              width = 9,
              bs4Dash::userMessages(
                width = 12,
                status = "success",
                bs4Dash::userMessage(
                  author = "Alexander Pierce",
                  date = "20 Jan 2:00 pm",
                  image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
                  type = "received",
                  "Is this template really for free? That's unbelievable!"
                ),
                bs4Dash::userMessage(
                  author = "Dana Pierce",
                  date = "21 Jan 4:00 pm",
                  image = "https://adminlte.io/themes/AdminLTE/dist/img/user5-128x128.jpg",
                  type = "sent",
                  "Indeed, that's unbelievable!"
                )
              )
            )
          )
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


get_package_version <- function(pkg = "invicoctrlr"){

  base::gsub('\n\\s+', ' ',
             utils::packageDescription(pkg = pkg,
                                       fields = 'Version'))

}

set_header <- function() {

  bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(
      title = "R INVICO",
      color = "olive",
      href = "https://www.google.fr",
      image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
    ),
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = shiny::icon("bars"),
    controlbarIcon = shiny::icon("th"),
    fixed = FALSE,
    leftUi = htmltools::tagList(
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
  )

}

set_sidebar <- function() {

  bs4Dash::dashboardSidebar(
    skin = "light",
    status = "teal",
    elevation = 3,
    bs4Dash::sidebarUserPanel(
      image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
      name = "Welcome Onboard!"
    ),
    bs4Dash::sidebarMenu(
      bs4Dash::sidebarHeader("Header 1"),
      bs4Dash::menuItem(
        "Base de Datos",
        tabName = "base_de_datos",
        icon = shiny::icon("database"),
        bs4Dash::menuSubItem(
          "SIIF Recursos",
          tabName = "siif_recursos",
          icon = shiny::icon("bank"),
        ),
        bs4Dash::menuSubItem(
          "SIIF Gastos",
          tabName = "siif_gastos",
          icon = shiny::icon("industry"),
        ),
        bs4Dash::menuSubItem(
          "SIIF Presupuesto",
          tabName = "siif_presupuesto",
          icon = shiny::icon("balance-scale"),
        ),
        bs4Dash::menuSubItem(
          "SIIF Contabilidad",
          tabName = "siif_contabilidad",
          icon = shiny::icon("book"),
        ),
        bs4Dash::menuSubItem(
          "R Icaro",
          tabName = "r_icaro",
          icon = shiny::icon("building"),
        )
      ),
      bs4Dash::menuItem(
        "Control",
        tabName = "control",
        icon = shiny::icon("check-square-o")
      ),
      bs4Dash::menuItem(
        "Tabla Din\u00e1mica",
        tabName = "tabla_dinamica",
        icon = shiny::icon("table")
      ),
      bs4Dash::menuItem(
        "Gr\u00e1fico",
        tabName = "grafico",
        icon = shiny::icon("pie-chart")
      )
    )
  )

}

set_body <- function() {

  bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "item1",
        shiny::fluidRow(
          lapply(1:3, FUN = function(i) {
            bs4Dash::sortable(
              width = 4,
              htmltools::p(class = "text-center", paste("Column", i)),
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
}

set_controlbar <- function(){

  bs4Dash::dashboardControlbar(
    # div(style="overflow-y: scroll"),
    skin = "light",
    pinned = FALSE,
    collapsed = TRUE,
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
  )

}

set_footer <- function() {

  bs4Dash::dashboardFooter(
    left = "By Fernando S. Corrales",
    right = base::paste0("Version ", get_package_version())
  )

}



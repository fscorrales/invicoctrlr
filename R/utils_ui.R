get_package_version <- function(pkg = "invicoctrlr"){

  base::gsub('\n\\s+', ' ',
             utils::packageDescription(pkg = pkg,
                                       fields = 'Version'))

}

tab_id <- as.list(c(
  bd = "01_00_base_de_datos",
  bd_presupuesto = "01_01_siif_presupuesto",
  bd_tesoreria = "01_02_siif_tesoreria",
  bd_gastos_fondos = "01_03_siif_gastos_fondos",
  bd_contabilidad = "01_04_siif_contabilidad",
  bd_clasificadores = "01_05_siif_clasificadores",
  bd_sgf = "01_06_sgf",
  bd_sscc = "01_07_sscc",
  bd_sgo = "01_08_sgo",
  bd_icaro = "01_09_icaro",
  ctr = "02_00_control",
  ctrl_recursos = "02_01_recursos",
  ctrl_gastos = "02_02_gastos",
  ctrl_remamente = "02_03_remamente",
  td = "03_00_tabla_dinamica"
))


set_header <- function() {

  bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(
      title = "R INVICO",
      color = "olive",
      href = "https://github.com/fscorrales/invicoctrlr",
      image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
    ),
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = shiny::icon("bars"),
    controlbarIcon = shiny::icon("th"),
    fixed = FALSE
    # leftUi = htmltools::tagList(
    #   bs4Dash::dropdownMenu(
    #     badgeStatus = "info",
    #     type = "notifications",
    #     bs4Dash::notificationItem(
    #       inputId = "triggerAction2",
    #       text = "Error!",
    #       status = "danger",
    #       icon = shiny::icon("exclamation-triangle")
    #     )
    #   ),
    #   bs4Dash::dropdownMenu(
    #     badgeStatus = "info",
    #     type = "tasks",
    #     bs4Dash::taskItem(
    #       inputId = "triggerAction3",
    #       text = "My progress",
    #       color = "orange",
    #       value = 10
    #     )
    #   )
    # )
    # rightUi = bs4Dash::dropdownMenu(
    #   badgeStatus = "danger",
    #   type = "messages",
    #   bs4Dash::messageItem(
    #     inputId = "triggerAction1",
    #     message = "message 1",
    #     from = "Divad Nojnarg",
    #     image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
    #     time = "today",
    #     color = "lime"
    #   )
    # )
  )

}

set_sidebar <- function() {

  bs4Dash::dashboardSidebar(
    skin = "light",
    status = "teal",
    elevation = 3,
    collapsed = TRUE,
    # bs4Dash::sidebarUserPanel(
    #   image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
    #   name = "Welcome Onboard!"
    # ),
    bs4Dash::sidebarMenu(
      # bs4Dash::sidebarHeader("Header 1"),
      bs4Dash::menuItem(
        "Base de Datos",
        tabName = "base_de_datos",
        icon = shiny::icon("database"),
        bs4Dash::menuSubItem(
          "SIIF Presupuesto",
          tabName = tab_id$bd_presupuesto,
          icon = shiny::icon("balance-scale"),
        ),
        bs4Dash::menuSubItem(
          "SIIF Tesorer\u00eda",
          tabName = tab_id$bd_tesoreria,
          icon = shiny::icon("money-bill-alt"),
        ),
        bs4Dash::menuSubItem(
          "SIIF Gastos y Fondos",
          tabName = tab_id$bd_gastos_fondos,
          icon = shiny::icon("credit-card"),
        ),
        bs4Dash::menuSubItem(
          "SIIF Contabilidad",
          tabName = tab_id$bd_contabilidad,
          icon = shiny::icon("columns"),
        ),
        bs4Dash::menuSubItem(
          "SIIF Clasificadores",
          tabName = tab_id$bd_clasificadores,
          icon = shiny::icon("book"),
        ),
        bs4Dash::menuSubItem(
          "Sist. Gesti\u00f3n Fciera.",
          tabName = tab_id$bd_sgf,
          icon = shiny::icon("file-alt"),
        ),
        bs4Dash::menuSubItem(
          "Sist. Seg. Ctas. Ctes.",
          tabName = tab_id$bd_sscc,
          icon = shiny::icon("university"),
        ),
        bs4Dash::menuSubItem(
          "Sist. Gesti\u00f3n Obras",
          tabName = tab_id$bd_sgo,
          icon = shiny::icon("industry"),
        ),
        bs4Dash::menuSubItem(
          "Icaro",
          tabName = tab_id$bd_icaro,
          icon = shiny::icon("link"),
        )
      ),
      bs4Dash::menuItem(
        "Control",
        tabName = "control",
        icon = shiny::icon("check-square"),
        bs4Dash::menuSubItem(
          "Recursos - Ingresos",
          tabName = tab_id$ctrl_recursos,
          icon = shiny::icon("hand-holding-usd"),
        ),
        bs4Dash::menuSubItem(
          "Gastos - Pagos",
          tabName = tab_id$ctrl_gastos,
          icon = shiny::icon("shopping-cart"),
        ),
        bs4Dash::menuSubItem(
          "Remanente",
          tabName = tab_id$ctrl_remamente,
          icon = shiny::icon("wallet"),
        )
      ),
      bs4Dash::menuItem(
        "Tabla Din\u00e1mica",
        tabName = tab_id$td,
        icon = shiny::icon("table")
      ),
      bs4Dash::menuItem(
        "Gr\u00e1fico",
        tabName = "grafico",
        icon = shiny::icon("chart-pie")
      )
    )
  )

}

set_body <- function() {

  bs4Dash::dashboardBody(
    # tags$head(htmltools::includeCSS("inst/app/www/custom.css")),
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = tab_id$bd_presupuesto,
        (paste0("mod_", tab_id$bd_presupuesto, "_ui")) %>%
          do.call(list(tab_id$bd_presupuesto))
      ),
      bs4Dash::tabItem(
        tabName = tab_id$bd_tesoreria,
        (paste0("mod_", tab_id$bd_tesoreria, "_ui")) %>%
          do.call(list(tab_id$bd_tesoreria))
      ),
      bs4Dash::tabItem(
        tabName = tab_id$bd_gastos_fondos,
        (paste0("mod_", tab_id$bd_gastos_fondos, "_ui")) %>%
          do.call(list(tab_id$bd_gastos_fondos))
      ),
      bs4Dash::tabItem(
        tabName = tab_id$bd_contabilidad,
        (paste0("mod_", tab_id$bd_contabilidad, "_ui")) %>%
          do.call(list(tab_id$bd_contabilidad))
      ),
      bs4Dash::tabItem(
        tabName = tab_id$bd_sgf,
        (paste0("mod_", tab_id$bd_sgf, "_ui")) %>%
          do.call(list(tab_id$bd_sgf))
      ),
      bs4Dash::tabItem(
        tabName = tab_id$bd_sscc,
        (paste0("mod_", tab_id$bd_sscc, "_ui")) %>%
          do.call(list(tab_id$bd_sscc))
      ),
      bs4Dash::tabItem(
        tabName = tab_id$bd_icaro,
        (paste0("mod_", tab_id$bd_icaro, "_ui")) %>%
          do.call(list(tab_id$bd_icaro))
      ),
      bs4Dash::tabItem(
        tabName = tab_id$ctrl_recursos,
        (paste0("mod_", tab_id$ctrl_recursos, "_ui")) %>%
          do.call(list(tab_id$ctrl_recursos))
      ),
      bs4Dash::tabItem(
        tabName = tab_id$ctrl_remamente,
        (paste0("mod_", tab_id$ctrl_remamente, "_ui")) %>%
          do.call(list(tab_id$ctrl_remamente))
      ),
      bs4Dash::tabItem(
        tabName = tab_id$td,
        (paste0("mod_", tab_id$td, "_ui")) %>%
          do.call(list(tab_id$td))
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
    # htmltools::div(
    #   class = "p-3",
    #   bs4Dash::skinSelector()
    #   )
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



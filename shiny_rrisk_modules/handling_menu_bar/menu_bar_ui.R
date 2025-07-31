# UI
menu_bar_ui <- function(shiny_rrisk_version)
{
  navbarPage(
    #title = tags$div(tags$img(src = "shiny-rrisk_logo_test.jpg",
    #                          width = "250"), 
    #                 ""),
    title       = paste("shiny rrisk", shiny_rrisk_version),
    id          = "menu_bar",
    selected    = "1",
    position    = "fixed-top",
    collapsible = TRUE,
    bslib::nav_panel(title = "", value = "1"),
    bslib::nav_panel(title = "NEW", value = "NEW"),
    bslib::nav_menu(
      title = "OPEN",
      bslib::nav_item(
        fileInput(inputId  = "open_model_file", 
                  label    = "Open model file",
                  multiple = FALSE,
                  accept   = c("application/json", ".rrisk"))
      ),
      "preset models",
      bslib::nav_spacer(),
      "exposure models",
      bslib::nav_panel(
        title = "HEV in liver sausages",
        value = "HEV"
      ),
      bslib::nav_panel(
        title = "Y. enterocolitica in minced meat",
        value = "YERSINIA"
      ),
      bslib::nav_panel(
        title = "Alizarin Red S in eel (2D MC)",
        value = "ALIZARIN"
      ),
      bslib::nav_spacer(),
      "exposure based risk models",
      bslib::nav_panel(
        title = "L. monocytogenes in cheese",
        value = "MINAS"
      ),
      bslib::nav_panel(
        title = "E. coli in hamburger meat (2D MC)",
        value = "POUILLOT"
      ),
      bslib::nav_spacer(),
      "risk models",
      bslib::nav_panel(
        title = "E. lignosellus in asparagus",
        value = "ASPARAGUS"
      ),
    ),
    bslib::nav_menu(
      title = "DOWNLOAD ...",
      bslib::nav_panel(
      title = downloadLink(outputId = "download_model", 
                           label    = "... MODEL",
                           style    = "display:inline"),
      value = "SAVE"
      ),
      bslib::nav_panel(
        title = downloadLink(outputId = "download_report", 
                             label    = "... REPORT",
                             style    = "display:inline"),
        value = "REPORT"
      ),
      bslib::nav_panel(
        title = downloadLink(outputId = "download_results", 
                             label    = "... RESULTS",
                             style    = "display:inline"),
        value = "RESULTS"
      )
    ),
    bslib::nav_panel(
      title = "Disclaimer, Manual, and Contact",
      value = "DISCLAIMER"
    )
  )
}
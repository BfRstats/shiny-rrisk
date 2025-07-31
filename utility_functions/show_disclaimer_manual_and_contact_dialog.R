show_disclaimer_manual_and_contact_dialog <- function()
{
  showModal(
    modalDialog(
      titel = "Disclaimer, Manual, and Contact",
      tags$h3("Contact"),
      tags$p("shiny-rrisk(at)bfr.bund.de"),
      tags$hr(),
      tags$h3("Manual"),
      tags$p("Here is a link to the manual."),
      tags$hr(),
      tags$h3("Impressum und rechtliche Hinweise"),
      tags$b("Bundesinstitut für Risikobewertung (BfR)"), 
      tags$p("Anstalt des öffentlichen Rechts", tags$br(), 
             "Max-Dohrn-Straße 8-10 10589 Berlin"),
      tags$p("Telefon: +49 30 18412 0", tags$br(),
             "Telefax: +49 30 18412 4970", tags$br(),
             "E-Mail: bfr@bfr.bund.de", tags$br(),
             "Website: www.bfr.bund.de"),
      tags$p("Gesetzlich vertreten durch den Präsidenten Prof. Dr. Dr. Andreas Hensel", tags$br(),
             "Aufsichtsbehörde: Bundeministerium für Landwirtschaft, Ernährung und Heimat (BMLEH)", tags$br(),
             "Ust.-IdNr. des BfR: DE 165893448"),
      tags$hr(),
      tags$h3("Gewährleistungsauschluss und Haftungsregelung"),
      tags$p("Die Inhalte auf dieser Webseite dienen ausschließlich informatorischen Zwecken. Wir bemühen uns, die Inhalte möglichst aktuell zu halten. Mit der Bereitstellung der Inhalte ist aber keine Gewährleistung und keine Garantie für Richtigkeit und Vollständigkeit verbunden. Jede Nutzerin und jeder Nutzer der Inhalte ist selbst dafür verantwortlich, sich über die für sie und ggf. ihren Betrieb / ihr Unternehmen geltenden rechtlichen Bestimmungen zu informieren und diese einzuhalten. Das BfR nimmt keine diesbezügliche Prüfung vor. Es ist nicht gewährleistet, dass durch die Nutzung der Inhalte dieser Webseite die geltenden rechtlichen Regelungen eingehalten werden. Unbeschadet des Vorstehenden haftet das BfR nicht für Schäden aus der Nutzung der Inhalte dieser Webseite. Für Schäden aus der Verletzung des Lebens, des Körpers und der Gesundheit sowie für sonstige Schäden, die aus einer vorsätzlichen oder grob fahrlässigen Pflichtverletzung des BfR, eines gesetzlichen Vertreters oder Erfüllungsgehilfen beruhen, gelten abweichend von dem vorstehenden Satz ausschließlich die gesetzlichen Bestimmungen.
"),
      tags$hr(),
      tags$h3("Urheber- und Nutzungsrechte"),
      tags$p("Sämtliche Inhalte dieser Webseite sind geistiges Eigentum des BfR. Nutzerinnen und Nutzer dürfen die Inhalte für persönliche oder amtliche, nicht kommerzielle Zwecke anwenden. Weitergehende Nutzungsrechte, insbesondere hinsichtlich der Weitergabe, Vervielfältigung, Veränderung und einem Download bedürfen der ausdrücklichen vorherigen schriftlichen Erlaubnis des BfR. Für kommerzielle Zwecke dürfen die Inhalte dieser Webseite in keinem Fall verwendet werden.
"),
      size   = "l",
      footer = tagList(modalButton("CLOSE"))
    )
  )
}
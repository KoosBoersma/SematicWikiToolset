#
#
# Aquo Beheertool ui.R
#
# Versie:      $Revision: 769 $
# Datum:       $Date: 2022-10-07 17:40:03 +0200 (vr, 07 okt 2022) $
# Auteur:      $Author: mwelling $
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#
library(shiny)
library(shinydashboard)
library(DT)
library(markdown)

# Deze code is nodig om te voorkomen dat het tabblad kan worden gesloten terwijl het nog bezig is 
# of er wijzigingen uitstaan
jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };
          window.history.back = function() { return "Please use the button on the webpage"; };'

# TRUE of FALSE: setting die gebruikt wordt om test of productie te onderscheiden
lTest <- TRUE

{
  if (lTest) {
    lOmgevingen <- c("Test redactie" = "test.redactie"
                    ,"Acceptatie redactie" = "acceptatie.redactie"
                    )
    lSelected <- "acceptatie.redactie"
    lTitle <- "TEST Verwerken van wijzigingen in Aquo Wiki TEST"
    lSkin <- 'yellow'
  }
  else {
    lOmgevingen <- c("Test redactie" = "test.redactie"
                     ,"Acceptatie redactie" = "acceptatie.redactie"
                     ,"Redactie" = "redactie"
    )
    lSelected <- "redactie"
    lTitle <- "Verwerken van wijzigingen in Aquo Wiki"
    lSkin <- 'blue'
  }
}

body <- dashboardBody(
  tags$head(tags$script(jscode)), # voorkom back button, refresh ed, zie jscode hierboven
  tabsetPanel(id = "tabset1", 
              type = "pills",
              # Tabblad voor domeinwaarden
              tabPanel("Domeinwaarden", value = "Domeinwaarden",
                       fluidRow(
                         # Blok 1.1: Linker rechthoek op scherm
                         box(column(6,textInput("botUserName", "Gebruiker")),
                             column(6,passwordInput("botPassword", "Wachtwoord")),
                             column(6,selectInput("Omgeving","Omgeving",lOmgevingen,selected = lSelected)),
                             column(4,HTML("<b>Status</b>"),h4(textOutput("loggedInBox"))),
                             br(),br(),br(),br(),br(),
                             column(2,actionButton("Inloggen", "Inloggen")),
                             #column(3,infoBoxOutput("loggedInBox")),
                             width = 4
                         ),
                         # Blok 1.2: Middelste rechthoek op scherm
                         box(column(7,selectInput("Domeintabel","Te wijzigen domeintabel",choices=NULL,selectize = FALSE)),
                             column(2,selectInput("sepCSV","Scheidingsteken",choices=c("," = ",",";" = ";"),selected = ";")),
                             br(),
                             column(3,downloadButton(outputId = "DownloadCSVButton", label = "Download CSV bestand", width = 200)),
                             #br(),br(),br(),
                             column(12,fileInput("CSVDomeinwaarden", "CSV bestand met domeinwaarden",multiple = FALSE,accept = "csv")),
                             width = 5
                         ),
                         # Blok 1.3 Rechter rechthoek op scherm
                         box(sliderInput("MaxInlezen", "Aantal in te lezen domeinwaarden:",value=250,min=250,max=30000,step=250),
                             column(3,checkboxInput("historieOphalen","Ook historie",value = FALSE)),
                             column(5,actionButton("ControlerenDomeinwaarden", "Controleer domeinwaarden")),
                             column(4,actionButton("Wijzigen", "Wijzig domeinwaarden")),
                             width = 3
                         )
                       ),
                       # Blok 2 waarin de waarden van de ingelezen CSV met domeinwaarden komen te staan
                       fluidRow(box(div(style = 'overflow-x: scroll', DT::dataTableOutput('wijzigingen'))
                                    ,width = 40
                       )
                       ),
                       # Blok 3 waarin de waarden van de ingelezen CSV komen te staan
                       fluidRow(box(div(style = 'overflow-x: scroll', DT::dataTableOutput('domValuesDF'))
                                    ,width = 40
                       )
                       )
              ),
              # Tabblad voor toevoegen waarnemingssoort
              tabPanel(value = "Waarnemingssoort", title = "Waarnemingssoort",
                       # Blok 1 waarin een nieuwe waarnemingssoort kan worden toegevoegd
                       fluidRow(box
                                   # 6x dropdown voor de 6 compoinenten waaruit een WNS bestaat
                                   (column(2,selectizeInput("wnsTypering","Typering",choices=NULL)),
                                    column(2,selectizeInput("wnsGrootheid","Grootheid",choices=NULL)),
                                    column(2,selectizeInput("wnsChemischeStofObject","ChemischeStofObject",choices=NULL,options = list(maxOptions=5000))),
                                    column(2,selectizeInput("wnsEenheid","Eenheid",choices=NULL)),
                                    column(2,selectizeInput("wnsHoedanigheid","Hoedanigheid",choices=NULL)),
                                    column(2,selectizeInput("wnsCompartiment","AnalyseCompartiment",choices=NULL)),
                                    
                                    # Id (in te voeren) en afgeleid de code (WNS<id>)
                                    column(1,numericInput("wnsId","Id",value = NULL)),
                                    column(1,HTML("<b>Code</b>"),h4(textOutput("wnsCodes"))),
                                     
                                    # Status t/m Wijzigingsnummer in te voeren
                                    column(1,selectInput("wnsStatus","Status",choices=NULL,selectize = FALSE)),
                                    column(1,dateInput("wnsBeginGeldigheid","Begin geldigheid",
                                                       value = toString(Sys.Date()+1),max = "2100-01-01",format = "yyyy-mm-dd")),
                                    column(1,dateInput("wnsEindGeldigheid","Eind geldigheid",
                                                       value = "2100-01-01",max = "2100-01-01",format = "yyyy-mm-dd")),
                                    column(1,textInput("wnsWijzigingsnummer","Wijzigingsnummer")),
                                    
                                    # Omschrijving wordt afgeleid uit de codes van de 6 gekozen componenten
                                    column(2,HTML("<b>Omschrijving</b>"),h4(textOutput("wnsOmschrijving"))),
                                    # Status geeft aan of de WNS is gecontroleerd
                                    column(2,HTML("<b>Status</b>"),h4(textOutput("wnsToevoegStatus"))),
                                    # Omgeving waaraan WNS zal worden toegevoegd
                                    column(1,selectInput("wnsOmgeving","Omgeving",lOmgevingen,selected = lSelected)),
                                                         #,selected = "redactie")),
                                    br(),br(),br(),br(),br(),
                                    # Knop om WNS toe te voegen
                                    column(1,actionButton("wnsToevoegen", "Toevoegen")),
                                    #                                    column(2),br(),br(),br(),
                                    
                                    #                           column(1,textInput("wnsGerelateerd","Gerelateerd")),
                                    width = 80
                       )),
                       # Blok 2 voor het beëindigen van een WNS
                       fluidRow(box(column(5,textInput(inputId = "WNSCodeHistoriseren", label = "Te beëindigen Waarnemingssoort"))
                                    ,br()
                                    ,column(3,actionButton("OpzoekenWNS", h5(HTML("<b>Opzoeken</b>"))))
                                    ,width = 5),
                                box(column(3,HTML("<b>Omschrijving</b>"),h4(textOutput("WNSOmschrijvingHistoriseren")))
                                    ,column(2,HTML("<b>Begin geldigheid</b>"),h4(textOutput("WNSBeginGeldigheidHistoriseren")))
                                    ,column(2,HTML("<b>Eind  geldigheid</b>"),h4(textOutput("WNSEindGeldigheidHistoriseren")))
                                    ,column(2,textInput("wnsWijzigingsnummerHistoriseren","Wijzigingsnummer"))
                                    ,br()
                                    ,column(3,actionButton("BeeindigenWNS", h5(HTML("<b>Beëindigen</b>"))))
                                    ,width = 7)
                                ),
                       # Blok 3 om CSV's met inhoud van de gekozen domeintabel op de Wiki te plaatsen (omgeving zoals gekozen in blok 1)
                       fluidRow(box(column(6,selectInput("DomeintabelUpload","Domeintabel te vervangen CSV",choices=NULL,selectize = FALSE))
                                    ,br()
                                    ,column(3,actionButton("WikiUploadCSV", h5(HTML("<b>Nieuwe CSV op Wiki plaatsen</b>"))))
                                    ,width = 5))
              ),
              # Tabblad voor toevoegen/wijzigen van begrippen
              tabPanel("Begrippen", value = "Begrippen",
                       fluidRow(
                         # Blok 1 om een CSVs met begrippen in te lezen
                         box(column(1,HTML("<b>Omgeving</b>"),h4(textOutput("omgevingBegrippen"))),
                             column(1,HTML("<b>Loginstatus</b>"),h4(textOutput("loggedInBoxBegrippen"))),
                             column(4,fileInput("CSVBegrippen", "CSV bestand met begrippen",multiple = FALSE,accept = "csv")),
                             column(1,selectInput("sepCSVBegrippen","Scheidingsteken",choices=c("," = ",",";" = ";"),selected = ";")),
                             br(),
                             column(2,actionButton("ControlerenBegrippen", "Controleer begrippen")),
                             column(1,actionButton("WijzigenBegrippen", "Wijzig begrippen")),
                             width = 8
                         ),
                         # Blok 2 om een CSV van begrippen te downloaden met een specifiek Wijzigingsnummer
                         box(column(4,textInput("WijzigingsnummerBegrippen", "Wijzigingsnummer")),
                             br(),
                             column(2,checkboxInput("guidBegrippen","Guid?")),
                             column(1,downloadButton("DownloadCSVBegrippen","Download CSV", width=1000)),
                             width = 4
                         )
                       ),
                       # Blok 3 waarin de waarden van de ingelezen CSV met begrippen komen te staan
                       fluidRow(box(div(style = 'overflow-x: scroll', DT::dataTableOutput('wijzigingenBegrippen'))
                                    ,width = 40
                       )
                       )
              ),
              # Tabblad voor het downloaden van CSV’s
              # Wordt eigenlijk nooit gebruikt
              tabPanel(value = "CSVDownload", title = "CSV's downloaden",
                       fluidRow(box(checkboxInput("downloadselectall","Alle/geen"),
                                    checkboxGroupInput(inputId = "DomeintabellenDownload",label = "Domeintabellen",choices = NULL,
                                                       selected = NULL),
                                    width=4
                       ),
                       box(column(width = 1,
                                  checkboxInput("geenBiotaxon","Biotaxon uitsluiten",value = TRUE),
                                  checkboxInput("geenParameter","Parameter uitsluiten",value = TRUE)
                       ),
                       width=2
                       ),
                       box(column(width = 1,
                                  #textOutput("gekozen"),
                                  downloadButton(outputId = "DownloadCSVAllButton", label = "Download CSV bestanden")
                       ),
                       width=4
                       ),
                       width = 8
                       )
                       #width = 2)
              ),
              # Tabblad waar vroeger de help opstond, nu te vinden op: https://redactie.aquo.nl/index.php/ShinyBeheerHelptekst
              # Tabblad kan verwijderd worden
              tabPanel("Help", value = "Help",
                       fluidRow(box(h1(htmlOutput("Helplink"))), width = 12
                       )
                       
                       # 2022-02-20 Vorige versie verwees naar  lokale helpfile, nu via Wiki
                       # fluidRow(box(includeHTML("helptekst.html"), width = 12 
                       # )
              ),
              # Tabblad met informatie over de tool zelf, bijv. her versienummer
              # Vinkje voor historiewijzigen stuurt de controle op validiteit van Begin - en Eind geldigheid
              tabPanel("Info", value = "Info",
                       box(h4(textOutput("Versie")),
                           h4(textOutput("VersieDatum")),
                           h4(textOutput("VersieAuteur")),
                           br(),br(),
                           h4(checkboxInput("historieWijzigen","Ook historie wijzigen",value = FALSE)),
                           width = 4)
              )
  )
)

# Put them together into a dashboardPage
# Standaard onderdeel van een Shiny applicatie
# Dit deel heeft blauwe titelbalk waarmee je de productieversie herkent
# te gebruiken voor publicatie naar https://aquo.shinyapps.io/Beheertool/
ui = dashboardPage(dashboardHeader(title = lTitle,titleWidth = 1440), #1920
                  dashboardSidebar(disable = TRUE),
                  body, skin = lSkin
)
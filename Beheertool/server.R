#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/ 

library(shiny)
library(readr)
library(data.table)
library(jsonlite)
library(httr)
library(RCurl)
library(rapportools)
library(uuid)
library(stringi)
library(stringr)
library(shinyalert)
library(leaflet)
library(tidyr)
library(readxl)

# Define server logic required to draw a histogram 
server <- function(input, output,session) {
  print(environment())
  # Voor versie tonen op scherm
  Versie   <- "$Revision: 802 $"
  Datum    <- "$Date: 2022-12-06 14:09:19 +0100 (di, 06 dec 2022) $"
  Auteur   <- "$Author: mwelling $"
  
  # Inlezen van algemene functies
  source("./Hulpfuncties.r")

  # globalValues wordt gebruikt om generieke gegevens op te slaan, alleen
  # Type is (Shiny) reactiveValues omdat die als enige mogen worden gebruikt om aan schermvelden toe te kennen
  globalValues <- reactiveValues()
  globalValues$loginStatus <- "Inloggen svp"
  globalValues$wnsStatus <- "Te controleren"
  globalValues$wnsOmschrijving <- NULL
  globalValues$wnsCodes <- NULL
  globalValues$WNSOmschrijvingHistoriseren <- NULL
  globalValues$WNSBeginGeldigheidHistoriseren <- NULL
  globalValues$WNSEindgeldigheidHistoriseren <- NULL
  globalValues$omgeving <- "redactie"
  
  # domeinwaardeCategorie wordt gebruikt afhankelijk van het type domeintabel 
  # de Categorie van de bijbehorende domeinwaarden te bepalen
  domeinwaardeCategorie   <- NULL
  domeinwaardeCategorie["Domeintabel"]                <- "Domeinwaarden"
  domeinwaardeCategorie["Domeintabelverzamellijst"]   <- "DomeinwaardenVerzamellijsten"
  
  # domeinwaardeElementType wordt gebruikt afhankelijk van het type domeintabel 
  # het Elementtype van de bijbehorende domeinwaarden te bepalen
  domeinwaardeElementType <- NULL
  domeinwaardeElementType["Domeintabel"]              <- "Domeinwaarde"
  domeinwaardeElementType["Domeintabelverzamellijst"] <- "DomeinwaardeVerzamellijst"
  
  lEnv <- environment()
  datumFmMaandTijd <- "%d %B %Y %H:%M:%OS"
  datumFmDDMMYYYY  <- "%d-%m-%Y"
  datumFmYYYYMMDD  <- "%Y-%m-%d"
  defaultEncoding <- "UTF-8"
  
  # Tonen van de SVN versiegegevens
  output$Versie <- renderText({ gsub("Revision","Subversion versie",gsub("\\$","",Versie)) })
  output$VersieDatum <- renderText({ gsub("Date","Laatse wijziging",gsub("\\$","",Datum)) })
  output$VersieAuteur <- renderText({ gsub("Author","Door",gsub("\\$","",Auteur)) })
  
  
  options(shiny.maxRequestSize = 60*1024^2)
  wDomWaarden <- data.frame(matrix(ncol = 0, nrow = 0)) # dataframe voor in te lezen csv domeinwaarden
  wBegrippen <- data.frame(matrix(ncol = 0, nrow = 0))  # dataframe voor in te lezen csv begrippen
  metadataBegrippen <- NULL                             # vector waarin de metadata van een begrip staat
#!  filenaam <- NULL
  beheertoolControlesDomeinen <- NULL                   # dataframe voor domeincontroles uit BeheertoolControles.xlsx
  beheertoolControlesBegrippen <- NULL                  # dataframe voor begripcontroles uit BeheertoolControles.xlsx
  metadataDomWaarden <- NULL                            # vector waarin de metadata van de gekozen domeintabel 
                                                        #   (tabelafhankelijk!) staat
  metadataWNS <- NULL                                   # vector waarin de metadata van domeintabel WNS staat

  alleDomeinTabellen <- NULL                            # vector met lijst van alle domeintabellen (dus ook verzamellijsten)
                                                        #   Bij Speciaal:Vragen [[Categorie:Domeintabellen]]
  alleDomeinGuid <- NULL                                # vector met de Guids van alle domeintabellen
                                                        #   aanroepen: alleDomeinGuid["<domeintabel naam>"]
                                                        #   voorbeeld: alleDomeinGuid["AfsluitmiddelType"]
  alleDomeinElementtype <- NULL                         # vector met type domeintabel voor alle domeintabellen
                                                        #   aanroepen: alleDomeinElementtype["<domeintabel naam>"]
                                                        #   voorbeeld: alleDomeinElementtype["AfsluitmiddelType"]

  domeinTabellen <- NULL                                # vector met lijst van alle gewone domeintabellen (geen verzamellijsten)
                                                        #   Bij Speciaal:Vragen [[Elementtype::Domeintabel]]
  domeinGuid <- NULL                                    # vector met de Guids van alle gewone domeintabellen
                                                        #   aanroepen: domeinGuid["<domeintabel naam>"]
                                                        #   voorbeeld: domeinGuid["AfsluitmiddelType"]
  domeinElementtype <- NULL                             # vector met type domeintabel voor gewone domeintabellen
                                                        #   aanroepen: domeinElementtype["<domeintabel naam>"]
                                                        #   voorbeeld: domeinElementtype["AfsluitmiddelType"]
  domeinTabellenVerzamellijst <- NULL                   # vector met lijst van alle domeintabelverzamellijsten
                                                        #   Bij Speciaal:Vragen [[Elementtype::Domeintabelverzamellijst]]
  domeinGuidVerzamellijst <- NULL                       # vector met de Guids van alle domeintabelverzamellijsten
                                                        #   aanroepen: domeinGuidVerzamellijst["<domeintabel naam>"]
                                                        #   voorbeeld: domeinGuidVerzamellijst["Waarnemingssoort"]

  domainsJson <- NULL                                   # Complexe Lijst met alle informatie van alle domeintabellen
  domValuesDF <- NULL                                   # dataframe voor het tonen van alle domeinwaarden in de gekozen domeintabel
                                                        #   wordt getoond als laatste blok ("inhoud tabel") op tabblad Domeinwaarden
  WNSBeeindigen <- NULL                                 # dataframe voor de te beëindigen WNS
  files <- NULL                                         # variabele voor het wegschrijven van CSV bestanden
  fileName <- NULL                                      # variabele voor het wegschrijven van CSV bestanden
  domValuesCSV <- NULL                                  # dataframe met domeinwaarden voor weg te schrijven CSV bestand
  singleFile <- NULL                                    # Boolean variabele die aangeeft of het om meerdere 
                                                        # weg te schrijven bestanden gaat
  
  baseUrl  <- "https://test.aquo.nl/api.php"            # variabele met de startstring van de wiki omgeving, gebruikt bij post
                                                        #   en inloggen
  tekstUrl <- "https://test.aquo.nl/index.php"          # variabele met de startstring van de wiki omgeving, gebruikt bij get
  
  lLimit <- 250                                         # aantal op te halen objecten bij een http get
  lStartPage <- 0                                       # teller voor als er meer dan <lLimit> waarden zijn op te halen
                                                        #  er wordt dan met pagina's van 250 waarden opgehaald
  curl <- getCurlHandle()                               # handler om curl aanroepen te kunnen doen

  # variabele die formaat, heading en sortering van de op te halen gegevens aanstuurt -> formaat = JSON
  opmaakJson <- paste("%2F&format=json&link=none&headers=show&searchlabel=JSON&class=sortable+wikitable+smwtable"
                      ,"&theme=bootstrap&offset=lOffset&limit=lLimit"
                      ,"&mainlabel=&prettyprint=true&unescape=true"
                      ,sep="")
  
  # zorgen dat de R environment van onderstaande functies gelijk is aan die van server.R
  environment(ophalenDomeintabel) <- environment()
  environment(ophalenBegrippen) <- environment()
  environment(wijzigDomeinwaardeInWiki) <- environment()
  
  #-------------------------------------------------------------------------------------
  # Inloggen
  #-------------------------------------------------------------------------------------
  observeEvent(input$Inloggen, {
    req(input$botUserName,input$botPassword)
    baseUrl  <<- paste("https://",input$Omgeving,".aquo.nl/api.php",sep="")
    tekstUrl <<- paste("https://",input$Omgeving,".aquo.nl/index.php",sep="")
    
    # lReturn bevat na aanroep een lijst van variabelen die verderop aan globale variabelen wordt toegekend
    lReturn <- ophalenLijstDomeintabellen(pOmgeving         = input$Omgeving
                                         ,pUserName         = input$botUserName
                                         ,pPassword         = input$botPassword
                                         ,pHistorieOpvragen = input$historieOphalen
                                         ,pOpmaakJson       = opmaakJson
    )

    globalValues$loginStatus <- unlist(lReturn[1], recursive=FALSE)

    if (globalValues$loginStatus == "Ingelogd") {
      # Ophalen excel met te controleren beperkingen
      URL <- paste(tekstUrl,"/Speciaal:Doorverwijzen/file/BeheertoolControles.xlsx",sep = "")

      GET(URL, write_disk(tf <- tempfile(fileext = ".xlsx")))
      
      # col_types om verderop bij replace_na problemen te voorkomen
      dfControles <- read_excel(tf,1
                               ,col_types = c("text","text","text","text","numeric","text","text","text","text","text","text","text","text"))
      
      # alle character kolommen NA --> "", behalve Tabel en Lengte
      cols <- colnames(dfControles)
      cols <- cols[!cols %in% c("Tabel","Lengte")]
      dfControles2 <- dfControles[,cols]
      dfControles2[is.na(dfControles2)] <- ""
      beheertoolControlesDomeinen <<- cbind(dfControles$Tabel,dfControles$Lengte,dfControles2)
      beheertoolControlesDomeinen <<- beheertoolControlesDomeinen[which(beheertoolControlesDomeinen$Actief == "J"),]
      colnames(beheertoolControlesDomeinen) <<- c("Tabel","Lengte",cols)

      # col_types om verderop bij replace_na problemen te voorkomen
      dfControlesBegrippen <- read_excel(tf,2
                                        ,col_types = c("text","text","text","numeric","text","text","text","text","text","text","text"))
      # alle character kolommen NA --> "", behalve Lengte
      cols <- colnames(dfControlesBegrippen)
      cols <- cols[!cols %in% c("Lengte")]
      dfControles2 <- dfControlesBegrippen[,cols]
      dfControles2[is.na(dfControles2)] <- ""
      beheertoolControlesBegrippen <<- cbind(dfControlesBegrippen$Lengte,dfControles2)
      beheertoolControlesBegrippen <<- beheertoolControlesBegrippen[which(beheertoolControlesBegrippen$Actief == "J"),]
      colnames(beheertoolControlesBegrippen) <<- c("Lengte",cols)
      
      domainsJson                 <<- unlist(lReturn[ 2], recursive=FALSE)
      alleDomeinTabellen          <<- unlist(lReturn[ 3], recursive=FALSE)
      alleDomeinGuid              <<- unlist(lReturn[ 4], recursive=FALSE)
      alleDomeinElementtype       <<- unlist(lReturn[ 5], recursive=FALSE)
      domeinTabellen              <<- unlist(lReturn[ 6], recursive=FALSE)
      domeinGuid                  <<- unlist(lReturn[ 7], recursive=FALSE)
      domeinElementtype           <<- unlist(lReturn[ 8], recursive=FALSE)
      domeinTabellenVerzamellijst <<- unlist(lReturn[ 9], recursive=FALSE)
      domeinGuidVerzamellijst     <<- unlist(lReturn[10], recursive=FALSE)

      # hier wordt de dropdown list voor de te kiezen domeintabellen gevuld (tabblad Domeinwaarden)
      updateSelectInput(session, "Domeintabel",
                        choices = domeinTabellen, # update choices
                        selected = NULL)
      # hier worden de check boxes voor de te downloaden domeintabellen gevuld (tabblad CSV's downloaden)
      updateCheckboxGroupInput(session, "DomeintabellenDownload",
                               choiceNames = domeinTabellen, choiceValues = domeinTabellen,
                               selected = NULL)
      # hier wordt de dropdown list voor de te kiezen up te loaden CSV van een domeintabel gevuld
      # (tabblad Waarnemingssoort)
      updateSelectInput(session, "DomeintabelUpload",
                        choices = domeinTabellenVerzamellijst, # update choices
                        selected = NULL)
    }
    else {
      updateSelectInput(session, "Domeintabel",
                        choices = c("Niet ingelogd" = "Niet ingelogd"), # update choices
                        selected = NULL)
    }
    # Eenmalig ophalen van de metadata voor begrippen
    # Ophalen metadata domeintabel is hier niet zinvol omdat de gebruiker nog geen domeintabel heeft gekozen!
    metadataBegrippen <<- bepaalMetadataBegrip(input$Omgeving,opmaakJson)
  })
  
  #---------------------------------------------------------------
  # Zetten van de status ingelogd of niet op tabblad Domeinwaarden
  #---------------------------------------------------------------
  output$loggedInBox <- renderText({
    #HTML('<i class="fas fa-times"></i>')
    message(globalValues$loginStatus)
    globalValues$loginStatus
  })  
  
  #------------------------------------------------------------------------------------------------------
  # Ophalen lijst domeintabellen met metadataDomWaarden als omgeving is gewijzigd (tabblad Domeinwaarden)
  #------------------------------------------------------------------------------------------------------
  observeEvent(input$Omgeving, {
    globalValues$loginStatus <- "Inloggen svp"
    globalValues$omgeving <- input$Omgeving
    updateSelectInput(session, "Domeintabel",
                      choices = c("Niet ingelogd" = "Niet ingelogd"), # update choices
                      selected = NULL)
    
    if (is.null(input$wnsStatus)) {
      updateSelectInput(session, "wnsOmgeving",
                        selected = input$Omgeving)
    }
    
    # dataframe met domeinwaarden van ingelezen CSV leegmaken en opnieuw (dus leeg) tonen
    wDomWaarden <<- data.frame(matrix(ncol = 0, nrow = 0))
    opnieuwTonenWijzigingen(wDomWaarden,"Domeinwaarden")
    
    # dataframe met domeinwaarden van gekozen domeintabel leegmaken en opnieuw (dus leeg) tonen
    domValuesDF <<- data.frame(matrix(ncol = 0, nrow = 0))
    opnieuwTonenDomValuesDF()
    message("wijzigen omgeving")
  })
  
  #-------------------------------------------------------------------------------------
  # Bepalen metadataDomWaarden als de gekozen domeintabel is gewijzigd
  #-------------------------------------------------------------------------------------
  observeEvent(input$Domeintabel, {
    if (!(input$Domeintabel == "Niet ingelogd"))
      metadataDomWaarden <<- bepaalMetadata(input$Domeintabel,domainsJson)
    else 
      metadataDomWaarden <<- NULL

    # dataframe met domeinwaarden van ingelezen CSV leegmaken en opnieuw (dus leeg) tonen
    wDomWaarden <<- data.frame(matrix(ncol = 0, nrow = 0))
    opnieuwTonenWijzigingen(wDomWaarden,"Domeinwaarden")
    
    # dataframe met domeinwaarden van gekozen domeintabel leegmaken en opnieuw (dus leeg) tonen
    domValuesDF <<- data.frame(matrix(ncol = 0, nrow = 0))
    opnieuwTonenDomValuesDF()
  })
  
  #-------------------------------------------------------------------------------------
  # Inlezen van de CSV met domeinwaarden
  #-------------------------------------------------------------------------------------
  observeEvent(input$CSVDomeinwaarden, {
    wDomWaarden <<- read.csv(input$CSVDomeinwaarden$datapath,header=TRUE, sep = input$sepCSV, encoding = defaultEncoding, stringsAsFactors=FALSE,na.strings=c(""),check.names = FALSE)
    # Zorg dat ALLE NA omgezet worden in een lege string
    wDomWaarden[is.na(wDomWaarden)] <<- ""
    message(wDomWaarden)
    
    # Punten (.) in metadata kenmerken vervangen door een spatie
    colnames(wDomWaarden) <<- gsub("\\."," ",colnames(wDomWaarden))
    message(paste("1:",colnames(wDomWaarden),sep = "#"))
    # N.a.v. Trello kaartje dat onduidelijk is wat de oorzaak van het niet kunnen inlezen is
    # de lengte van de kolomnamen van de ingelezen CSV = 1 als het verkeerde scheidingsteken is gekozen
    if (length(unlist(colnames(wDomWaarden))) == 1) {
      showModal(modalDialog(title = "Let op! Verkeerde scheidingsteken gebruikt bij inlezen"
                            , footer = modalButton("OK")))
      wDomWaarden <<- data.frame()
    }
    else {
      # Geef een popup foutmelding als Begin geldigheid/Eind geldigheid/Id/Status ontbreken als kolom in de CSV
      if (!("Id" %in% colnames(wDomWaarden) && 
            (input$Domeintabel == "Biotaxon" || "Status" %in% colnames(wDomWaarden)) &&
            "Begin geldigheid" %in% colnames(wDomWaarden) && 
            "Eind geldigheid" %in% colnames(wDomWaarden))
         ) 
      {
        showModal(modalDialog(title = "Let op! Kolomnamen ingelezen bestand niet correct (Begin geldigheid/Eind geldigheid/Id/Status)"
                              , footer = modalButton("OK")))
        wDomWaarden <<- data.frame()
        
      }
      else {
        # Extra veld voor foutmeldingen toevoegen aan dataframe wDomWaarden met ingelezen CSV
        wDomWaarden$'Controle meldingen' <<- NA

        # Omzetten ingelezen datumformaat in YYYY-MM-DD
        for (i in 1:nrow(wDomWaarden)) {
          #message(paste("Eind geldigheid: ",wDomWaarden[i,"Eind geldigheid"]," ",nrow(wDomWaarden)))
          if (!is.na(wDomWaarden[i,"Begin geldigheid"])) wDomWaarden[i,"Begin geldigheid"] <<- bewerkDatum(wDomWaarden[i,"Begin geldigheid"])
          if (!is.na(wDomWaarden[i,"Eind geldigheid"])) wDomWaarden[i,"Eind geldigheid"] <<- bewerkDatum(wDomWaarden[i,"Eind geldigheid"])
        }
        
        message(paste("2:",colnames(wDomWaarden)))
#!        filenaam <<- gsub(".xlsx","",input$CSVDomeinwaarden$name)
#!        message(paste("file: ",filenaam))
        wDomWaarden$Id <<- as.numeric(wDomWaarden$Id)       # numeriek maken van het Id

        # Toon de ingelezen CSV in het 2e blok op tabblad Domeinwaarden
        output$wijzigingen <- DT::renderDataTable({
          DT::datatable(wDomWaarden,  caption = "Wijzigingen", filter = "top", selection="single", class = 'cell-border nowrap',
                        options = list(lengthMenu = c(1,5,25, 50, 100), pageLength = 100,scrollX = TRUE
                                       #,columnDefs = list(list(className = "nowrap", targets = "_all"))
                                       ,autoWidth = TRUE
                        )
          )
        })
      }
    }
    
  })

    
  #----------------------------------------------------------------------------------------------
  # 
  # Code voor het ophalen van de gekozen domeintabel
  # Wordt uitgevoerd als op de knop "Download CSV bestand" op tabblad Domeinwaarden wordt gedrukt
  # 
  #----------------------------------------------------------------------------------------------
  output$DownloadCSVButton  <- downloadHandler(
    filename = function() {
      paste(input$Domeintabel,".csv",sep = "") },
    content = function(file) {
      singleFile <<- TRUE
      ophalenMeerdereDomeintabellen(input$Domeintabel,input$Omgeving)
      write.table(domValuesCSV,file,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
    }
  )
  
  #----------------------------------------------------------------------------------------------
  # 
  # Code voor het ophalen van één of meer domeintabellen
  # Wordt uitgevoerd als op de knop "Download CSV bestand" op tabblad Domeinwaarden wordt gedrukt
  # 
  #----------------------------------------------------------------------------------------------
  ophalenMeerdereDomeintabellen <- function(pDomeintabel = "",pOmgeving,pCategorie=NULL,pHistorie=NULL) {
    
    # Verwijderen van alle aanwezige csv bestanden
    CSVfiles <- list.files(tempdir(), full.names = T, pattern = "*.csv")
    file.remove(CSVfiles)
    lTekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
    
    #-----------------------------------------
    # Ophalen van de inhoud van de domeintabel
    #-----------------------------------------
    for (dom in pDomeintabel) {
      message(dom)
      if (!is.null(pHistorie)) lHistorie <- pHistorie
      else lHistorie <- input$historieOphalen
      domValuesCSV <<- ophalenDomeintabel(pDomeintabel      = pDomeintabel
                                          ,pMaxRijen         = 50000
                                          ,pCategorie        = pCategorie
                                          ,pDomainsJson      = domainsJson
                                          ,pTekstUrl         = lTekstUrl
                                          ,pSepCSV           = input$sepCSV
                                          ,pHistorieOpvragen = lHistorie
                                          ,pUitBeheertool    = TRUE
      )
      
      Sys.sleep(time = 1)  # Om de Wiki wat lucht te geven tussen de domeintabellen door
      
      if (!singleFile) {
        fileName <<- paste(tempdir(),"/",dom,".csv",sep = "")
        write.table(domValuesCSV, fileName, sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
        files <<- c(fileName,files)
      }
    } # for (dom in 1:input$DomeintabellenDownload)
  }

  #-------------------------------------------------------------------------------------------
  # Uitvoeren van controles op domeinwaarden:
  # 1. Ophalen alle domeinwaarden van gekozen domeintabel
  # 2. Controleren of metadataDomWaarden gekozen domeintabel overeenkomt met de ingelezen CSV
  # 3. Controleren elke ingelezen rij op falen controles die in BeheertoolControles.xlsx staan
  #-------------------------------------------------------------------------------------------
  observeEvent(input$ControlerenDomeinwaarden, {
    message("1 - Start controleren domeinwaarden, ")
    print(environment())
    if (nrow(wDomWaarden) > 0 && globalValues$loginStatus == "Ingelogd") {
      req(input$CSVDomeinwaarden,input$Domeintabel)
      
      #------------------------------------------------------
      # 1. Ophalen alle domeinwaarden van gekozen domeintabel
      #------------------------------------------------------
      domValuesDF <<- ophalenDomeintabel(pDomeintabel      = input$Domeintabel
                                        ,pMaxRijen         = input$MaxInlezen
                                        ,pDomainsJson      = domainsJson
                                        #,pId               = NULL
                                        ,pTekstUrl         = tekstUrl
                                        #,pCategorie        = NULL
                                        ,pSepCSV           = input$sepCSV
                                        ,pHistorieOpvragen = input$historieOphalen
                                        ,pUitBeheertool    = TRUE
                                        )
        
      output$domValuesDF <- DT::renderDataTable({
        DT::datatable(domValuesDF,  caption = "Inhoud tabel", filter = "top", selection="single", class = 'cell-border nowrap',
                      options = list(lengthMenu = c(1,5,25, 50, 100), pageLength = 100,scrollX = TRUE
                                     #,columnDefs = list(list(className = "nowrap", targets = "_all"))
                                     ,autoWidth = TRUE
                      ) 
        )
      })
      
      if (nrow(domValuesDF) == input$MaxInlezen){
        showModal(modalDialog(title = "Let op! Niet alle waarden van de domeintabel zijn opgehaald, verhoog de limiet!"
                              , footer = modalButton("OK")))
      }
      else {
        if (nrow(wDomWaarden) >= 100) {
          lFactor <- trunc(nrow(wDomWaarden) / 100)
          lProgress <- 0.01
        }
        else {
          lFactor <- 1
          lProgress <- round((1 / nrow(wDomWaarden)),2)
        }
        message(paste("lFactor:",lFactor,"lProgress",lProgress))
        
        # dit geeft rechtsonder in t scherm een balkje waarin de voortgang wordt getoond
        withProgress(message = "Controleren van ingelezen wijzigingen", value = 0, {
          
          #------------------------------------------------------------------------------------------
          # 2. Controleren of metadataDomWaarden gekozen domeintabel overeenkomt met de ingelezen CSV
          #------------------------------------------------------------------------------------------
          wDomWaarden[,"Controle meldingen"] <<- ""
          wDomWaarden[,"Controle meldingen"] <<- as.character(wDomWaarden[,"Controle meldingen"])
          okMetadata <- TRUE
          for (i in 1:length(metadataDomWaarden)) {
            if (!(metadataDomWaarden[i] %in% colnames(wDomWaarden))) {
              message(paste("Wijzigingen metadata-kolom ontbreekt:",metadataDomWaarden[i],sep = " "))
              okMetadata <- FALSE
            }
          }
          if (!(okMetadata)) {
            wDomWaarden[,"Controle meldingen"] <<- "Metadata domeintabel wijkt af van CSV"
          }
          else {
            for (i in 1:length(colnames(wDomWaarden))) {
              #message(colnames(wDomWaarden)[i])
              if (!(colnames(wDomWaarden)[i] %in% c('Guid','Controle meldingen'))
                  && !(colnames(wDomWaarden)[i] %in% metadataDomWaarden)) {
                message(paste("Metadata wijzigingen-kolom ontbreekt:",colnames(wDomWaarden)[i],sep = " "))
                okMetadata <- FALSE
              }
            }
            if (!(okMetadata)) {
              wDomWaarden[,"Controle meldingen"] <<- "Metadata domeintabel wijkt af van CSV"
            }
          }
          
          
          #-----------------------------------------------------------------------------------------------------------------
          # 3. Controleren elke ingelezen rij op falen controles die in BeheertoolControles.xlsx staan
          #    Loop over alle rijen (en dus kenmerken) in DF beheertoolControlesDomeinen, per rij:
          #    a Controleren of Guid voorkomt als iets anders dan domeinwaarde
          #    b Controleren kenmerken geselecteerde rij t.o.v. BeheertoolControles.xlsx
          #      Loop over alle rijen (controles) in DF beheertoolControlesDomeinen en selecteer een te controleren kenmerk
          #-----------------------------------------------------------------------------------------------------------------
          if (okMetadata) {
            for (i in 1:nrow(wDomWaarden)) {
              
              # Voortgangsbalkje conditioneel verhogen o.b.v. de voortgang
              if (i %% lFactor == 0) incProgress(lProgress)
              message(paste("2 - Controleren rij (#) met Id ():",i,wDomWaarden[i,"Id"],sep = " " ))
              
              domvalExist <- as.data.frame(matrix(ncol = 1, nrow = 0))
              ok <- TRUE
              message(paste("3 -"),toString(wDomWaarden[i,"Guid"]))
              
              #--------------------------------------------------------------
              # 3a Controleren of Guid voorkomt als iets anders dan domeinwaarde
              #--------------------------------------------------------------
              if (nchar(toString(wDomWaarden[i,"Guid"])) > 0) {
                
                # Ophalen bestaande domeinwaarde uit opgehaalde lijst met domeinwaarden
                domvalExist <- domValuesDF[which(domValuesDF$Guid == wDomWaarden[i,"Guid"]),]
                #message(str(domvalExist))
                message(paste("Aantal bestaande gevonden:",nrow(domvalExist),sep = " - "))
                if (nrow(domvalExist) == 0) {
                  categorie <- paste("?title=Speciaal:Vragen&x=-5B-5B",gsub("-","-2D",wDomWaarden[i,"Guid"]),"-5D-5D",sep="")
                  beperking <- NULL
                  kenmerken <- "%2F-3FElementtype"
                  lOpmaakJson <- str_replace(opmaakJson,"lLimit",toString(lLimit))
                  json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
                  #message(json_file)
                  req <- httr::GET(json_file, curl=curl)
                  # 2022-09-27: door lengte content te checken voorkomen foutmelding
                  jsonContent <- httr::content(req, "text", encoding="UTF-8")
                  if ((req$status_code == 200 && nchar(jsonContent) == 0)) {
                    ok <- TRUE
                  } 
                  else {
                     ok <- FALSE
                      message(paste("Guid wel bekend maar geen domeinwaarde van",input$Domeintabel))
                      wDomWaarden[i,"Controle meldingen"] <<- paste("Guid wel bekend maar geen domeinwaarde van",input$Domeintabel)
                  }
                  
                }
              } # nchar(toString(wDomWaarden[i,"Guid"])) > 0
              
              #---------------------------------------------------------------------------------------------------------------
              # 3b Controleren kenmerken geselecteerde rij t.o.v. BeheertoolControles.xlsx (in DF beheertoolControlesDomeinen)
              #    1 Vul DF dfControles met 
              #      i   alle rijen waarvan t kenmerk voorkomt in de metadata van de domeintabel (input$Domeintabel)
              #      ii  alleen de rijen met Tabel == input$Domeintabel
              #      iii plus rijen uit 1 met lege Tabel waarvan t kenmerk niet voorkomt in ii
              #    2 Loop over alle rijen (controles) in DF dfControles
              #      a Verplichte velden
              #      b Numerieke velden
              #      c Lengte velden
              #      d Vergelijken met ander kenmerk
              #      e Vergelijken met dynamische waarde
              #      f Controle op nieuwe groep
              #      g Controle veld wijzigbaar (alleen bestaande Domeinwaarden)
              #      h Controle veld uniek
              #---------------------------------------------------------------------------------------------------------------
              
              #      i   alle rijen waarvan t kenmerk voorkomt in de metadata van de domeintabel (input$Domeintabel)
              dfControlesHulp <- beheertoolControlesDomeinen[which(beheertoolControlesDomeinen$Kenmerk %in% metadataDomWaarden),]
              #      ii  alleen de rijen met Tabel == input$Domeintabel
              dfControles     <- dfControlesHulp[which(dfControlesHulp$Tabel == input$Domeintabel),]
              #      iii plus rijen uit 1 met lege Tabel waarvan t kenmerk niet voorkomt in ii
              dfControlesHulp2 <- dfControlesHulp[which(is.na(dfControlesHulp$Tabel)),]
              dfControlesCheck <- dfControles
              for (voorwaarde in 1:nrow(dfControlesHulp2)) {
                if (!(dfControlesHulp2[voorwaarde,]$Kenmerk %in% dfControlesCheck$Kenmerk)) {
                  
                  dfControles <- rbind(dfControles,dfControlesHulp2[voorwaarde,])
                }
              }
              message(paste("Te controleren kenmerken voor",input$Domeintabel))
              message(dfControles$Kenmerk)
              
              if (ok && nrow(dfControles) > 0) {
                for (voorwaarde in 1:nrow(dfControles)) {
                  lKenmerk <- dfControles[voorwaarde,]$Kenmerk

                  # 3b 2a Controle op verplichte velden
                  message(paste("Te controleren kenmerk:",lKenmerk," - ","  te controleren waarde:",tidyr::replace_na(as.character(wDomWaarden[i,lKenmerk]),"")))
                  if (dfControles[voorwaarde,]$Verplicht == "J" &&
                      tidyr::replace_na(as.character(wDomWaarden[i,lKenmerk]),"") == "")
                  {
                    message(paste("Verplicht:",lKenmerk))
                    wDomWaarden[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wDomWaarden[i,"Controle meldingen"],"")
                                                                  ,paste(lKenmerk,"is verplicht")
                                                                  ,sep="<br/>")
                    ok <- FALSE
                  }
                  #message("Na controle verplicht")
                  
                  # 3b 2b Controle op numerieke velden
                  if (dfControles[voorwaarde,]$Datatype == "Integer" &&
                      grepl("[^0-9]", wDomWaarden[i,lKenmerk]))
                  {
                    message(paste("Integer:",lKenmerk))
                    wDomWaarden[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wDomWaarden[i,"Controle meldingen"],"")
                                                                  ,paste(lKenmerk,"is geen geheel getal")
                                                                  ,sep="<br/>")
                    ok <- FALSE
                  }
                  #message("Na controle numeriek")
                  
                  # 3b 2c Controle op lengte velden
                  if (dfControles[voorwaarde,]$Lengte != "" &&
                      nchar(toString(tidyr::replace_na(as.character(wDomWaarden[i,lKenmerk]),""))) > 
                      tidyr::replace_na(dfControles[voorwaarde,]$Lengte,1048576)) # was 1024 wat te kort was, nu 2^20
                  {
                    message(paste("Veld te lang:",lKenmerk))
                    wDomWaarden[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wDomWaarden[i,"Controle meldingen"],"")
                                                                  ,paste(lKenmerk,"is langer dan",dfControles[voorwaarde,]$Lengte)
                                                                  ,sep="<br/>")
                    ok <- FALSE
                  }
                  #message("Na controle lengte")
                  
                  # 3b 2d Controle op vergelijken met ander kenmerk
                  # - Als SoortWijziging leeg, dan altijd
                  # - Als SoortWijziging Toevoegen, dan alleen controleren met lege Guid
                  # - Als SoortWijziging Wijzigen, dan alleen controleren met gevulde Guid
                  if (dfControles[voorwaarde,]$VergelijkenMetKenmerk != "" &&
                      (dfControles[voorwaarde,]$SoortWijziging == "" ||
                       (dfControles[voorwaarde,]$SoortWijziging == "Toevoegen" &&
                        wDomWaarden[i,"Guid"] == ""
                       ) ||
                       (dfControles[voorwaarde,]$SoortWijziging == "Wijzigen" &&
                        wDomWaarden[i,"Guid"] != ""
                       )
                      )
                     ) {
                    if (!dfControles[voorwaarde,]$Datatype == "Datum" || !input$historieWijzigen) {
                      lVergelijkenMetKenmerk <- dfControles[voorwaarde,]$VergelijkenMetKenmerk
                      lVergelijking <- dfControles[voorwaarde,]$Vergelijking
                      message(paste("dynamische controle:",lVergelijkenMetKenmerk,lVergelijking))
                      lCondition <- paste("\"",str_trim(toString(tidyr::replace_na(wDomWaarden[i,lKenmerk],"")),side = "both"),"\""
                                          ," ",lVergelijking," "
                                          ,"\"",str_trim(toString(tidyr::replace_na(wDomWaarden[i,lVergelijkenMetKenmerk],"")),side = "both"),"\""
                                          ,sep = "")
                      message(paste("dynamische conditie:",lCondition))
                      lControle <- eval(parse(text=lCondition))
                      if (!lControle) {
                        wDomWaarden[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wDomWaarden[i,"Controle meldingen"],"")
                                                                      ,paste("Onwaar:",lKenmerk,lVergelijking,lVergelijkenMetKenmerk)
                                                                      ,sep="<br/>")
                        ok <- FALSE
                      }
                    }
                  }
                  #message("Na controle vergelijken ander kenmerk")
                  
                  # 3b 2e Controle op vergelijken met dynamische waarde
                  # - Als SoortWijziging leeg, dan altijd
                  # - Als SoortWijziging Toevoegen, dan alleen controleren met lege Guid
                  # - Als SoortWijziging Wijzigen, dan alleen controleren met gevulde Guid
                  if (dfControles[voorwaarde,]$VergelijkenDynamisch != "" &&
                      (dfControles[voorwaarde,]$SoortWijziging == "" ||
                       (dfControles[voorwaarde,]$SoortWijziging == "Toevoegen" &&
                        wDomWaarden[i,"Guid"] == ""
                       ) ||
                       (dfControles[voorwaarde,]$SoortWijziging == "Wijzigen" &&
                        wDomWaarden[i,"Guid"] != ""
                       )
                      )
                     ) 
                  {
                    if (!dfControles[voorwaarde,]$Datatype == "Datum" || !input$historieWijzigen) {
                      lwaarde <- dfControles[voorwaarde,]$VergelijkenDynamisch
                      lVergelijkenDynamisch <- NA
                      while (tidyr::replace_na(str_locate(lwaarde,"\\|")[2],0) > 0) {
                        lVergelijkenDynamisch <- c(lVergelijkenDynamisch,substr(lwaarde,1,str_locate(lwaarde,"\\|")[2]-1))
                        lwaarde  <- substr(lwaarde,str_locate(lwaarde,"\\|")[2]+1,1024)
                      }
                      lVergelijkenDynamisch <- tidyr::replace_na(as.character(lVergelijkenDynamisch),as.character(lwaarde))
                      lVergelijking <- dfControles[voorwaarde,]$Vergelijking
                      lCondition <- ""
                      for (contr in 1:length(lVergelijkenDynamisch)) {
                        if (contr > 1) lCondition <-paste(lCondition,"|| ")
                        lCondition <- paste(lCondition,"\"",str_trim(toString(tidyr::replace_na(as.character(wDomWaarden[i,lKenmerk]),"")),side = "both"),"\""
                                           ,lVergelijking
                                           ,"toString(",lVergelijkenDynamisch[contr],")"
                                           ,sep = "")
                      }
                      message(lCondition)
                      lControle <- eval(parse(text=lCondition))
                      if (!lControle) {
                        wDomWaarden[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wDomWaarden[i,"Controle meldingen"],"")
                                                                      ,paste("Onwaar:",lKenmerk,lVergelijking
                                                                             ,dfControles[voorwaarde,]$VergelijkenDynamisch)
                                                                      ,sep="<br/>")
                        ok <- FALSE
                      }
                    }
                  }
                  #message("Na controle vergelijken dynamische waarde")
                  
                  #--------------- 3b 2f Controle nieuwe groep
                  if (lKenmerk == "Groep" && !(toString(wDomWaarden[i,"Groep"]) == "")) {
                    groepExist <- domValuesDF[which(domValuesDF$Groep == wDomWaarden[i,"Groep"]),]
                    if (nrow(groepExist) == 0)
                      wDomWaarden[i,"Controle meldingen"] <<- paste(wDomWaarden[i,"Controle meldingen"],"Nieuwe groep",sep="<br/>")
                  }
                  #message("Na controle nieuwe groep")
                  
                  # 3b 2g Controles op bestaande domeinwaarden
                  if (nrow(domvalExist) > 0)
                  {
                    #message("4 - Controleren bestaande domeinwaarden")
                    
                    # Controle veld wijzigbaar
                    if (dfControles[voorwaarde,]$Wijzigbaar == "N" &&
                        toString(wDomWaarden[i,lKenmerk]) != 
                        toString(domvalExist[,lKenmerk])) {
                      wDomWaarden[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wDomWaarden[i,"Controle meldingen"],"")
                                                                    ,paste(lKenmerk,"is niet wijzigbaar")
                                                                    ,sep="<br/>")
                      ok <- FALSE
                    }
                  } # Controles op bestaande domeinwaarden
                  #message("Na controle bestaande domeinwaarde")
                  
                  # 3b 2h Controle veld uniek
                  if (dfControles[voorwaarde,]$Uniek == "J" &&
                      toString(tidyr::replace_na(as.character(wDomWaarden[i,lKenmerk]),"")) != "" &&
                      # trello kaartje https://trello.com/c/RYwrbX3h/437-shiny-aanpassen-domtabl-parameter-casnr-nvt
                      # Uniek niet controleren als de waarde wordt uitgesloten in de BeheertoolControles.xlsx
                      # Enige implemenatie nu is voor Parameter CASNummer
                      toString(tidyr::replace_na(as.character(wDomWaarden[i,lKenmerk]),"")) != dfControles[voorwaarde,]$Uitsluiten) {

                    lUniekOK <- controleerObjectUniek(pElementtype     = domeinwaardeElementType[domeinElementtype[input$Domeintabel]]
                                                     ,pDomeinTabel     = alleDomeinGuid[input$Domeintabel]
                                                     ,pKenmerk         = lKenmerk
                                                     ,pKenmerkWaarde   = wDomWaarden[i,lKenmerk]
                                                     ,pBeginGeldigheid = wDomWaarden[i,"Begin geldigheid"]
                                                     ,pEindGeldigheid  = wDomWaarden[i,"Eind geldigheid"]
                                                     ,pWijzigingen     = wDomWaarden
                                                     ,pGuid            = wDomWaarden[i,"Guid"]
                                                     ,pOmgeving        = input$Omgeving
                                                     ,pOpmaakJson      = opmaakJson
                                                     )
                    if (!lUniekOK) {
                      wDomWaarden[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wDomWaarden[i,"Controle meldingen"],"")
                                                                    ,paste(lKenmerk,"is niet uniek")
                                                                    ,sep="<br/>")
                      message(paste(lKenmerk,"is niet uniek"))
                    }
                    
                      
                  }       # Controle veld uniek
                  #message("Na controle uniek")
                  
                }           # for (voorwaarde in 1:nrow(dfControles))

                #--------------- Juist maken van de controle melding
                wDomWaarden[i,"Controle meldingen"] <<- gsub("NA<br/>","",wDomWaarden[i,"Controle meldingen"])
                wDomWaarden[i,"Controle meldingen"] <<- gsub("^<br/>","",wDomWaarden[i,"Controle meldingen"])
                #message(nchar(stri_trim(toString(wDomWaarden[i,"Controle meldingen"]))))
                if (nchar(stri_trim(toString(wDomWaarden[i,"Controle meldingen"]))) %in% c(0,2) ||
                    wDomWaarden[i,"Controle meldingen"] == "Nieuwe groep" ||
                    wDomWaarden[i,"Controle meldingen"] == "Groep wordt gewijzigd"
                ) wDomWaarden[i,"Controle meldingen"] <<- paste("OK",wDomWaarden[i,"Controle meldingen"])
              }   # if (ok)
            }   # for (i in 1:nrow(wDomWaarden))
          } # if (ok)
          message("3 - Na controleren alle rijen")
        }) # withProgress
        
        message("98 - Voor refresh van de wijzigingen")
        opnieuwTonenWijzigingen(wDomWaarden,"Domeinwaarden")
      }
      
      message("99 - Einde controleren")
      
    }
  })
  
  opnieuwTonenDomValuesDF <- function() {
    output$domValuesDF <- DT::renderDataTable({
      DT::datatable(domValuesDF,  caption = "Inhoud tabel", filter = "top", selection="single", class = 'cell-border nowrap',
                    options = list(lengthMenu = c(1,5,25, 50, 100), pageLength = 100,scrollX = TRUE
                                   #,columnDefs = list(list(className = "nowrap", targets = "_all"))
                                   ,autoWidth = TRUE
                    ) 
      )
    })
  }
  
  opnieuwTonenWijzigingen <- function(pWijzigingen,pType) {
    # Opnieuw tonen van de wijzigingen met evt. meldingen in kolom Controle meldingen
    if (nrow(pWijzigingen) == 0) {
      if (pType == "Domeinwaarden") {
        output$wijzigingen <<- DT::renderDataTable({
          DT::datatable(escape = F, pWijzigingen,  caption = "Wijzigingen", filter = "top", selection="single", class = 'cell-border nowrap',
                        options = list(lengthMenu = c(1,5,25, 50, 100), pageLength = 100,scrollX = TRUE
                                       #,columnDefs = list(list(className = "nowrap", targets = "_all"))
                                       ,autoWidth = TRUE
                        )
          )
        })
      }
      else {
        output$wijzigingenBegrippen <<- DT::renderDataTable({
          DT::datatable(escape = F, pWijzigingen,  caption = "Wijzigingen", filter = "top", selection="single", class = 'cell-border nowrap',
                        options = list(lengthMenu = c(1,5,25, 50, 100), pageLength = 25,scrollX = TRUE
                                       #,columnDefs = list(list(className = "nowrap", targets = "_all"))
                                       ,autoWidth = TRUE
                        )
          )
        })
      }
    }
    else {
      wijzigingenDT <- pWijzigingen[,c(ncol(pWijzigingen),1:ncol(pWijzigingen)-1)]
      #message(paste("Aantal wijzigingen:",nrow(wijzigingenDT),"Kolomnamen:",colnames(wijzigingenDT)))
      
      #---------------------------- settings voor formatteren output
      # Afhankelijk van de waarde in de kolom "Controle meldingen" de achtergrondkleur van de kolom instellen 
      rijKleur <- "(/Metadata begrip wijkt af van CSV/).test(value) ? 'tomato' :
      (/Metadata domeintabel wijkt af van CSV/).test(value) ? 'tomato' : ''"
      GroepGeel <- "(/Nieuwe groep/).test(value) ? 'yellow' :
      (/Groep wordt gewijzigd/).test(value) ? 'tomato' :
      (/Metadata domeintabel wijkt af van CSV/).test(value) ? 'tomato' : ''"
      # OK groen
      ControleMeldGroen <-  "(/OK/).test(value) ? 'lightgreen' :
      (/OK Nieuwe groep/).test(value) ? 'lightgreen' :
      (/Onwaar/).test(value) ? 'tomato' :
      (/lijst van/).test(value) ? 'tomato' :
      (/langer/).test(value) ? 'tomato' :
      (/niet uniek/).test(value) ? 'tomato' :
      (/niet wijzigbaar/).test(value) ? 'tomato' :
      (/verplicht/).test(value) ? 'tomato' :
      (/Domeinwaarde is NIET op de redactie-omgeving aanwezig/).test(value) ? 'tomato' :
      (/Metadata begrip wijkt af van CSV/).test(value) ? 'tomato' :
      (/Metadata domeintabel wijkt af van CSV/).test(value) ? 'tomato' : 
      (/Guid wel bekend maar geen domeinwaarde van/) ? 'tomato' : ''"
      
      if (pType == "Domeinwaarden") {
        output$wijzigingen <<- DT::renderDataTable({
          DT::datatable(escape = F, wijzigingenDT,  caption = "Wijzigingen", filter = "top", selection="single", class = 'cell-border nowrap',
                        options = list(lengthMenu = c(1,5,25, 50, 100), pageLength = 100,scrollX = TRUE
                                       #,columnDefs = list(list(className = "nowrap", targets = "_all"))
                                       ,autoWidth = TRUE
                        )
          ) %>%
            formatStyle(1:ncol(wijzigingenDT),"Controle meldingen", backgroundColor = JS(rijKleur)) %>%
            formatStyle("Controle meldingen", backgroundColor = JS(ControleMeldGroen))
        })
      }
      else {
        output$wijzigingenBegrippen <<- DT::renderDataTable({
          DT::datatable(escape = F, wijzigingenDT,  caption = "Wijzigingen", filter = "top", selection="single", class = 'cell-border nowrap',
                        options = list(lengthMenu = c(1,5,25, 50, 100), pageLength = 25,scrollX = TRUE
                                       #,columnDefs = list(list(className = "nowrap", targets = "_all"))
                                       ,autoWidth = TRUE
                        )
          ) %>%
            formatStyle(1:ncol(wijzigingenDT),"Controle meldingen", backgroundColor = JS(rijKleur)) %>%
            formatStyle("Controle meldingen", backgroundColor = JS(ControleMeldGroen))
        })
        
      }
    }
  }

  # Dit levert een popup met waarschuwing op wanneer je wijzigingen op een domeintabel gaat doorvoeren
  modalMeldingWijzigen <- function() {
    modalDialog(
      title = "Let op! Je gaat wijzigingen doorvoeren op domeintabel:",
      input$Domeintabel,
      
      footer = tagList(
        modalButton("Annuleer"),
        actionButton("ok", "OK")
      )
    )
  }
  
  #-------------------------------------------------------------------------------------
  # Doorvoeren van de wijzigingen
  #-------------------------------------------------------------------------------------
  observeEvent(input$Wijzigen, {
    req(input$CSVDomeinwaarden,baseUrl,input$botUserName,input$botPassword)
    
    if (nrow(wDomWaarden) > 0)
      showModal(modalMeldingWijzigen())
    
  })
  
  #-------------------------------------------------------------------------------------
  # Dit gedeelte gaat daadwerkelijk alle domeinwaarden wijzigen in de Wiki.
  # In een loop over wDomWaarden worden alle rijen met controlemelding "OK" opgepakt
  # Per rij wordt de wikipagina (te ien bij Bron bewerken) opgebouwd en met een POST
  # in de Wiki verwerkt.
  #
  # Deze code wordt uitgevoerd als in de popup op OK wordt geklikt.
  #
  # Voor nieuwe domeinwaarden (lege Guid) wordt de Guid bepaald
  #-------------------------------------------------------------------------------------
  
  observeEvent(input$ok, {
    message("Wijzigen")
    removeModal()
    
    wikitekst <- NULL
    
    curl <- getCurlHandle()
    ## Inloggen: Retrieve the login token
    botToken <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", baseUrl), curl=curl ) )$query$tokens$logintoken
    loginParams=list('lgtoken'=botToken,'lgname'=input$botUserName,'lgpassword'=input$botPassword)
    loginResponse <- content( POST(sprintf("%s?action=login&format=json", baseUrl) , body=loginParams, curl=curl) )$login$result
    editToken <- content( GET(sprintf("%s?action=query&meta=tokens&type=csrf&format=json", baseUrl), curl=curl ))$query$tokens$csrftoken
    
    message(paste("Het editToken:",editToken))
    tijdelijk <<- editToken
    
    lengte <- nrow(wDomWaarden)
    
    doorgaan <- TRUE
    if (nrow(wDomWaarden) >= 100) {
      lFactor <- trunc(nrow(wDomWaarden) / 100)
      lProgress <- 0.01
    }
    else {
      lFactor <- 1
      lProgress <- trunc(100 / nrow(wDomWaarden))
    }
    
    # Wederom een voortgangsbalk rechts onderin het scherm
    withProgress(message = "Wijzigingen verwerken", value = 0, {
      for (j in 1:lengte)
      {
        if (j %% lFactor == 0) incProgress(lProgress)
        if (doorgaan && substring(wDomWaarden[j,"Controle meldingen"],1,2) == "OK") { # Alleen OK gecontroleerde domeinwaarden
          velden <- NULL           # Hierin wordt "|<kenmerk>=<waarde> vastgelegd, bij "|Id=12"
          for (g in 1:length(metadataDomWaarden)) {
            if (wDomWaarden[j,metadataDomWaarden[g]] != "")   # alleen gevulde kenmerken opslaan in de Wiki
            {
              # 2022-02-21 Nodig omdat kenmerk niet in spreadsheet hoeft voor te komen!
              # Verplicht == "V" betekent dat het veld niet mag worden opgeslagen in de Wiki, dit geldt nu voor Status 
              if (!(nrow(beheertoolControlesDomeinen[which(beheertoolControlesDomeinen$Kenmerk == metadataDomWaarden[g]),] > 0) &&
                    beheertoolControlesDomeinen[which(beheertoolControlesDomeinen$Kenmerk == metadataDomWaarden[g]),]$Verplicht == "V")
              ) {
                # Plakken van nieuw wijzigingsnummer vóór het reeds bestaande
                lKopieWijzigingsNr <- domValuesDF[which(domValuesDF$Guid == wDomWaarden[j,"Guid"]),]$Wijzigingsnummer
                if (metadataDomWaarden[g] == "Wijzigingsnummer" &&
                    !is.null(lKopieWijzigingsNr)
                )
                {
                  velden[metadataDomWaarden[g]] <- trimws(paste("|",metadataDomWaarden[g],"=",
                                                      paste(wDomWaarden[j,metadataDomWaarden[g]],", ",lKopieWijzigingsNr,sep=''),"\n"
                                                      ,sep=''),
                                                whitespace = "\\, ")  # \\ is escape voor de komma
                }
                else {
                  velden[metadataDomWaarden[g]] <- paste("|",metadataDomWaarden[g],"=",wDomWaarden[j,metadataDomWaarden[g]],"\n",sep='')
                }
              }
            }
          }
          #message(paste("Guid voor toekennen:"),wDomWaarden[j,"Guid"])
          if (nchar(toString(wDomWaarden[j,"Guid"])) < 10) {
              wDomWaarden[j,"Guid"] <<- paste("Id-",uuid::UUIDgenerate(),sep="")
          }
          
          # Afhandeling voorkeurslabel voor Biotaxon
          if (input$Domeintabel == "Biotaxon") 
            lVoorkeurslabel <- wDomWaarden[j,"Naam"]
          else
            lVoorkeurslabel <- wDomWaarden[j,"Omschrijving"]
          
          # Opbouwen van pagina voor de betreffende domeinwaarde
          wikitekst[j] <- paste("{{#element:\n",
                                paste("|Elementtype=",domeinwaardeElementType[domeinElementtype[input$Domeintabel]],"\n",sep = ""),
                                "|Paginanaam=",wDomWaarden[j,"Guid"],"\n",
                                "|Voorkeurslabel=",lVoorkeurslabel,"\n",
                                paste(gsub("=NA\n","=\n",velden),collapse=''),  # hier worden de verschillende velden geplaatst
                                "|GUID=",wDomWaarden[j,"Guid"],"\n",
                                "|Breder=",alleDomeinGuid[input$Domeintabel],"\n",
                                "}}",sep = '')

          editParams=list('title'=toString(wDomWaarden[j,"Guid"]),'text'=wikitekst[j],'token'=editToken,'format'='json')

          message("title----------------------------------------")
          message(editParams$title)
          message("text----------------------------------------")
          message(editParams$text)
          message("token----------------------------------------")
          message(editParams$token)
          message("format----------------------------------------")
          message(editParams$format)
          
          editResponse <- content( POST(sprintf("%s?action=edit&format=json", baseUrl) , body=editParams, curl=curl) )
          editStatus <- editResponse$edit$result
          message("Het resultaat ")
          if (nchar(toString(editStatus)) == 0 || toString(editStatus) != "Success") {
            doorgaan <- FALSE
            showModal(modalDialog(title = paste("Fout in rij ",j," met Id:",wDomWaarden[j,"Id"]), footer = modalButton("OK")))
          }
          message("Verwerking:",editStatus)
          Sys.sleep(0.1)    # om te voorkomen dat wijzigingen te snel aan de Wiki worden aangeboden, daar kan die soms niet tegen
        }
        else message(paste(wDomWaarden[j,"Omschrijving"]," niet verwerken"))
      } #for
    })
    
    message("voor de shinyalert")
    Sys.sleep(0.5)
    
    # Opnieuw ophalen van de domeinwaarden voor de tabel om in laatste blok te tonen
    domValuesDF <<- ophalenDomeintabel(pDomeintabel      = input$Domeintabel
                                       ,pMaxRijen         = input$MaxInlezen
                                       ,pDomainsJson      = domainsJson
                                       ,pTekstUrl         = tekstUrl
                                       ,pSepCSV           = input$sepCSV
                                       ,pHistorieOpvragen = input$historieOphalen
                                       ,pUitBeheertool    = TRUE
                                      )
    opnieuwTonenDomValuesDF()
    
    # Ook opnieuww tonen van de wijzigingen omdat nieuwe domeinwaarden een Guid hebben gekregen
    opnieuwTonenWijzigingen(wDomWaarden,"Domeinwaarden")
    
    showModal(modalDialog(title = "Klaar", footer = modalButton("OK")))
    
  })

  #---------------------------------------------------------------------------------------------
  #
  # Dit gedeelte is voor het tabblad "CSV's downloaden", staat op de nominatie om uit te faseren
  #
  #---------------------------------------------------------------------------------------------
  
  observeEvent(input$downloadselectall, {
    if (input$downloadselectall == TRUE) {
      message("allemaal selecteren")
      domSelected <- domeinTabellen
      #message(domeinTabellen)
      if (input$geenBiotaxon)  domSelected <- domSelected[domSelected != "Biotaxon"]
      if (input$geenParameter) domSelected <- domSelected[domSelected != "Parameter"]
      updateCheckboxGroupInput(session,"DomeintabellenDownload", selected = domSelected)
    }
    else {
      message("geen selecteren")
      updateCheckboxGroupInput(session,"DomeintabellenDownload", selected = as.list(NULL))
    }
  }) 
  
  observeEvent(input$geenBiotaxon, {
    if (!(input$geenBiotaxon)) {
      message("wel biotaxon")
      domSelected <- input$DomeintabellenDownload
      domSelected <- append(domSelected,"Biotaxon")
    }
    else {
      message("geen biotaxon")
      domSelected <- input$DomeintabellenDownload[input$DomeintabellenDownload != "Biotaxon"]
    }
    updateCheckboxGroupInput(session,"DomeintabellenDownload", selected = domSelected)
  }) 
  
  observeEvent(input$geenParameter, {
    if (!(input$geenParameter)) {
      message("wel parameter")
      domSelected <- input$DomeintabellenDownload
      domSelected <- append(domSelected,"Parameter")
    }
    else {
      message("geen parameter")
      domSelected <- input$DomeintabellenDownload[input$DomeintabellenDownload != "Parameter"]
    }
    updateCheckboxGroupInput(session,"DomeintabellenDownload", selected = domSelected)
  }) 
  
  
  output$DownloadCSVAllButton  <- downloadHandler(
    filename = function() {
      "AquoDomeinTabellen.zip" },
    content = function(file) {
      singleFile <<- FALSE
      ophalenMeerdereDomeintabellen(input$DomeintabellenDownload,input$Omgeving)
      files <- c(fileName,files)
      #create the zip file
      zip(file,files)
    }
  )
  
  output$gekozen <- renderText({
    DomeintabellenDownload <- paste(input$DomeintabellenDownload, collapse = ", ")
    paste("You chose", DomeintabellenDownload)
  })
  
  #---------------------------------------------------------------------------------------
  #
  # Vanaf hier functionaliteit voor tabblad Waarnemingssoort
  #
  #---------------------------------------------------------------------------------------
  
  wnsTyperingGuid <- NULL                               # Lijst met Guid's voor de Typering
  wnsGrootheidGuid <- NULL                              # Lijst met Guid's voor de Grootheid
  wnsEenheidGuid <- NULL                                # Lijst met Guid's voor de Eenheid
  wnsHoedanigheidGuid <- NULL                           # Lijst met Guid's voor de Hoedanigheid
  wnsCompartimentGuid <- NULL                           # Lijst met Guid's voor de Compartiment
  wnsChemischeStofObjectGuid <- NULL                    # Lijst met Guid's voor de ChemischeStofObject
  wnsTyperingCodes <- NULL                              # Lijst met Codes voor de Typering, basis voor de dropdown list
  wnsGrootheidCodes <- NULL                             # Lijst met Codes voor de Grootheid, basis voor de dropdown list
  wnsEenheidCodes <- NULL                               # Lijst met Codes voor de Eenheid, basis voor de dropdown list
  wnsHoedanigheidCodes <- NULL                          # Lijst met Codes voor de Hoedanigheid, basis voor de dropdown list
  wnsCompartimentCodes <- NULL                          # Lijst met Codes voor de Compartiment, basis voor de dropdown list
  wnsChemischeStofObjectCodes <- NULL                   # Lijst met Codes voor de ChemischeStofObject, basis voor de dropdown list
  wnsTyperingCodesGuid <- NULL                          # Lijst met Guid's voor de Typering, geindexeerd met code
  wnsGrootheidCodesGuid <- NULL                         # Lijst met Guid's voor de Grootheid, geindexeerd met code
  wnsEenheidCodesGuid <- NULL                           # Lijst met Guid's voor de Eenheid, geindexeerd met code
  wnsHoedanigheidCodesGuid <- NULL                      # Lijst met Guid's voor de Hoedanigheid, geindexeerd met code
  wnsCompartimentCodesGuid <- NULL                      # Lijst met Guid's voor de Compartiment, geindexeerd met code
  wnsChemischeStofObjectCodesGuid <- NULL               # Lijst met Guid's voor de ChemischeStofObject, geindexeerd met code
  
  #-------------------------------------------------------------------------------------------------
  #
  # Deze functie handelt alles af rondom één van de 6 bestandedelen van een WNS
  # 1. parameter pRelatie bepaalt voor welke onderdeel
  # 2. parameter pInputVeld is de naam van de corresponderende dropdown list
  # 3. parameter pElementType wordt alleen nog met "Domeinwaarde" gevuld bij aanroep.
  #    Zit er historisch in omdat er eerder wel verschillen waren ("DomeinwaardeTechnisch")
  # 4. parameter pElementGuid bevat de Guid van de domeintabel waaruit wordt geselecteerd
  #
  # - De 3 variabelen (lijsten) wns<pRelatie>Guid, wns<pRelatie>Codes en wns<pRelatie>CodesGuid 
  #   worden gevuld.
  # - De corresponderende dropdown list wordt met codes gevuld
  #
  #-------------------------------------------------------------------------------------------------
  haalWNSRelatiesOp <- function(pRelatie,pInputVeld,pElementType,pElementGuid,pCurl,pTekstUrl){
    lVector <- c()
    lVectorGuid <- c()
    lVectorCodes <- c()
    lVectorCodesOpGuid <- c()
    
    lTypeTabel <- paste("-5B-5BElementtype%3A%3A",pElementType,"-5D-5D-20",sep = "")
    beperking <- paste("-5B-5BBreder%3A%3A",pElementGuid,"-5D-5D",sep = "")
    # Trello kaartje https://trello.com/c/8Xsgnq5L/452-dropdown-lijsten-op-tabblad-waarnemingssoort-worden-niet-meer-gevuld
    # Status G of Geldig
    beperking <- paste(beperking,"-5B-5BStatus%3A%3AGeldig%20%7C%7C%20G-5D-5D",sep = "")
    categorie <- paste("?title=Speciaal:Vragen&x=",lTypeTabel, sep = "")
    kenmerken <- "%2F-3FVoorkeurslabel%2F-3FCodes"
    #message(opmaakJson)
    lOpmaakJson <- paste(str_replace(opmaakJson,"lLimit",toString(lLimit)),"&sort=Codes&order=asc",sep = "")
    
    lDoorgaan <- TRUE
    gevonden <- TRUE
    lOffset <- lStartPage
    
    while (lDoorgaan){
      json_file <- maakTekstURL(pTekstUrl,categorie,beperking,kenmerken,str_replace(lOpmaakJson,"lOffset",toString(lOffset)))
      #message(json_file)
      req <- httr::GET(json_file, curl=pCurl)
      #message(content(req, "text", encoding="UTF-8"))
      if (req$status_code == 200 && length(req$content) > 0) {
        domainsJson <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))$results
        message(paste(toString(Sys.time()),"Aantal waarden in de Wiki:",length(domainsJson)+lOffset, sep = " "))
      }
      
      for (di in 1:length(domainsJson)) {
        dj <- di + lOffset
        #lVector[di] <- paste(dQuote(domainsJson[[di]]$printouts$Voorkeurslabel,FALSE)," = ",dQuote(domainsJson[[di]]$fulltext,FALSE),sep = "")
        #lVector[dj] <- domainsJson[[di]]$printouts$Voorkeurslabel
        lVectorCodes[dj] <- domainsJson[[di]]$printouts$Codes
        lVectorGuid[lVectorCodes[dj]] <- domainsJson[[di]]$fulltext
        lVectorCodesOpGuid[domainsJson[[di]]$fulltext] <- domainsJson[[di]]$printouts$Codes
      }
      lOffset <- lOffset + lLimit
      if (length(domainsJson) < lLimit) {
        lDoorgaan <- FALSE
      }
    }
    message(paste("Aantal",pRelatie,":",length(lVector)))
    updateSelectInput(session, pInputVeld,choices = lVectorCodes,selected = NULL)
    return(list(lVectorGuid,lVectorCodes,lVectorCodesOpGuid))
  }
  
  #-------------------------------------------------------------------------------------------------
  #
  # Deze functie vult alle 6 de dropdown listst en handelt verdere zaken af vóórdat de gebruiker
  # het tabblad "Waarnemingssoort" kan gebruiken
  # 
  # - Roept voor elk van de 6 componenten de functie haalWNSRelatiesOp aan
  # - Omdat dit best even duurt worden 2 meldingen in een popup gegeven, bij start en bij einde
  #
  #-------------------------------------------------------------------------------------------------
  wnsVulDropDownLists <- function (){
    
    updateSelectInput(session, "wnsStatus",
                      choices = c("Concept" = "C","Geldig" = "G","Historisch" = "H"),
                      selected = "G"
    )
    
    wnsBaseUrl  <- paste("https://",input$wnsOmgeving,".aquo.nl/api.php",sep="")
    wnsTekstUrl <- paste("https://",input$wnsOmgeving,".aquo.nl/index.php",sep="")
    wnsCurl <- getCurlHandle()
    ## Retrieve the login token
    botToken <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", wnsBaseUrl), curl=wnsCurl ) )$query$tokens$logintoken
    loginParams=list('lgtoken'=botToken,'lgname'=input$botUserName,'lgpassword'=input$botPassword)
    loginResponse <- content( POST(sprintf("%s?action=login&format=json", wnsBaseUrl) , body=loginParams, curl=wnsCurl) )$login$result
    if (length(loginResponse) > 0 && loginResponse == "Success") {
      message(loginResponse)
      
      # Geef een melding dat het even gaat duren
      showModal(modalDialog(title = "Ophalen van de componenten van de waarnemingssoort. Dit duurt even..."
                            , footer = modalButton("OK")))
      
      #-----------------------------------------------------------------------------------
      # Typering en Grootheid worden uit de domeintabel Parameter gehaald.
      # Om niet alle domeinwaarden op te halen wordt nog een extra beperking op Groep meegegeven.
      # Voor Typering: Typering of TyperingMarien
      # Voor Grootheid: Grootheid
      #-----------------------------------------------------------------------------------
      lijst <<- haalWNSRelatiesOp("Typering","wnsTypering","Domeinwaarde"
                                  ,paste(gsub("-","-2D","Id-0eafa483-2875-4c94-890d-66258a6b4d88")
                                        ,"-5D-5D-5B-5BGroep%3A%3ATypering%20%7C%7C%20TyperingMarien",sep = ""),wnsCurl,wnsTekstUrl)
      wnsTyperingGuid <<- lijst[[1]]
      wnsTyperingCodes <<- lijst[[2]]
      wnsTyperingCodesGuid <<- lijst[[3]]
      lijst <<- haalWNSRelatiesOp("Grootheid","wnsGrootheid","Domeinwaarde"
                                  ,paste(gsub("-","-2D","Id-0eafa483-2875-4c94-890d-66258a6b4d88")
                                        ,"-5D-5D-5B-5BGroep%3A%3AGrootheid",sep = ""),wnsCurl,wnsTekstUrl)
      wnsGrootheidGuid <<- lijst[[1]]
      wnsGrootheidCodes <<- lijst[[2]]
      wnsGrootheidCodesGuid <<- lijst[[3]]
      lijst <<- haalWNSRelatiesOp("Eenheid","wnsEenheid","Domeinwaarde"
                                  ,gsub("-","-2D","Id-04f4f467-021b-4218-baa8-9742ed977c61"),wnsCurl,wnsTekstUrl)
      wnsEenheidGuid <<- lijst[[1]]
      wnsEenheidCodes <<- lijst[[2]]
      wnsEenheidCodesGuid <<- lijst[[3]]
      lijst <<- haalWNSRelatiesOp("Hoedanigheid","wnsHoedanigheid","Domeinwaarde"
                                  ,gsub("-","-2D","Id-7169dd0a-813b-4cf1-86ab-9bbc52b113a4"),wnsCurl,wnsTekstUrl)
      wnsHoedanigheidGuid <<- lijst[[1]]
      wnsHoedanigheidCodes <<- lijst[[2]]
      wnsHoedanigheidCodesGuid <<- lijst[[3]]
      lijst <<- haalWNSRelatiesOp("Compartiment","wnsCompartiment","Domeinwaarde"
                                  ,gsub("-","-2D","Id-6134f3bb-6048-431d-a130-01290d84172c"),wnsCurl,wnsTekstUrl)
      wnsCompartimentGuid <<- lijst[[1]]
      wnsCompartimentCodes <<- lijst[[2]]
      wnsCompartimentCodesGuid <<- lijst[[3]]
      lijst <<- haalWNSRelatiesOp("ChemischeStofObject","wnsChemischeStofObject","Domeinwaarde"
                                  ,paste(gsub("-","-2D","Id-0eafa483-2875-4c94-890d-66258a6b4d88")
                                        ,"-5D-5D-5B-5BGroep%3A%3AChemischeStof%20%7C%7C%20Object%20%7C%7C%20Microbiologie",sep = ""),wnsCurl,wnsTekstUrl)
      wnsChemischeStofObjectGuid <<- lijst[[1]]
      wnsChemischeStofObjectCodes <<- lijst[[2]]
      wnsChemischeStofObjectCodesGuid <<- lijst[[3]]
      
      showModal(modalDialog(title = "Klaar...", footer = modalButton("OK")))
    }
  }

  
  #--------------------------------------------------------------------------------------------------
  #
  # Deze code gaat af wanneer naar het tabblad "Waarnemingssoort" wordt genavigeerd
  # 
  # - Indien nodig worden de dropdown lijsten gevuld door functie wnsVulDropDownLists() aan te roepen
  #
  #--------------------------------------------------------------------------------------------------
  observeEvent(input$tabset1, {
    
    if(input$tabset1 == "Waarnemingssoort"){
      req(input$botUserName,input$botPassword,globalValues$loginStatus)
      if (globalValues$loginStatus == "Ingelogd" &&
          !(nchar(input$wnsTypering) > 0) &&
          !(nchar(input$wnsGrootheid) > 0) &&
          !(nchar(input$wnsChemischeStofObject) > 0) &&
          !(nchar(input$wnsEenheid) > 0) &&
          !(nchar(input$wnsHoedanigheid) > 0) &&
          !(nchar(input$wnsCompartiment) > 0)
      ) {
          message("Vullen van de dropdown WNS")
          metadataWNS <<-bepaalMetadataWNS(input$Omgeving,opmaakJson)

          wnsVulDropDownLists()
        }
    }
  })
  
  #--------------------------------------------------------------------------------------------------
  #
  # Deze code gaat af wanneer op de knop "Toevoegen" op tabblad "Waarnemingssoort" wordt gedrukt
  # 
  # Controles (uitgevoerd door controleerWNS():
  # - uniciteit van de velden Id, Codes en Omschrijving
  # - Begin geldigheid morgen (tenzij hsitorie verwerken)
  # - Typering & Grootheid/ChemischestofObject niet tegelijk vullen
  # 
  #--------------------------------------------------------------------------------------------------
  observeEvent(input$wnsToevoegen, {
    req(input$wnsId,input$wnsBeginGeldigheid,input$wnsEindGeldigheid,input$botUserName,input$botPassword)

    globalValues$wnsStatus <<- NULL
    
    #------------------------------------------------------------------
    # Inloggen voor WNS omdat je hier een andere omgeving kunt aangeven
    #------------------------------------------------------------------
    wnsBaseUrl  <- paste("https://",input$wnsOmgeving,".aquo.nl/api.php",sep="")
    wnsTekstUrl <- paste("https://",input$wnsOmgeving,".aquo.nl/index.php",sep="")
    wnsCurl <- getCurlHandle()
    
    controleerWNS <- function(pVeld,pWaarde,pTekstUrl,pCurl) {
      categorie <- "?title=Speciaal:Vragen&x=-5B-5BElementtype%3A%3ADomeinwaardeVerzamellijst-5D-5D-20"
      beperking <- paste("-5B-5BBreder%3A%3A",gsub("-","-2D","Id-a225c399-903d-4ee1-ec2f-521b4f255cb5"),"-5D-5D",sep = "")
      beperking <- paste(beperking,"-5B-5B",pVeld,"%3A%3A",pWaarde,"-5D-5D",sep = "")
      kenmerken <- "%2F-3FVoorkeurslabel%2F-3FBegin geldigheid%2F-3FEind geldigheid"
      lOpmaakJson <- str_replace(opmaakJson,"lLimit",toString(lLimit))

      json_file <- maakTekstURL(pTekstUrl,categorie,beperking,kenmerken,str_replace(lOpmaakJson,"lOffset",toString(lStartPage)))
      message(json_file)
      req <- httr::GET(json_file, curl=pCurl)
      #message(content(req, "text", encoding="UTF-8"))
      if (req$status_code == 200 && length(req$content) > 0) {
        wnsJson <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))$results

        message(paste("WNS met",pVeld,pWaarde,":",length(wnsJson)))
        wnsBeginGeldigheid <- bewerkDatum(wnsJson[[1]]$printouts$'Begin geldigheid'$raw)
        wnsEindGeldigheid <- bewerkDatum(wnsJson[[1]]$printouts$'Eind geldigheid'$raw)
        if (toString(wnsBeginGeldigheid) <= toString(input$wnsEindGeldigheid) &&
            toString(wnsEindGeldigheid) >= toString(input$wnsBeginGeldigheid)) {
          globalValues$wnsStatus <<- str_trim(paste(tidyr::replace_na(globalValues$wnsStatus,"")
                                                    ,paste(pVeld,"niet uniek"),sep=" ")
                                              ,side = "both")
          return(wnsJson[[1]]$fulltext)
        }
      }
      else {
        message(paste("Geen WNS met",pVeld,pWaarde))
        return("")
      }
    }
    
    ## Retrieve the login token
    botToken <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", wnsBaseUrl), curl=wnsCurl))$query$tokens$logintoken
    loginParams=list('lgtoken'=botToken,'lgname'=input$botUserName,'lgpassword'=input$botPassword)
    loginResponse <- content( POST(sprintf("%s?action=login&format=json", wnsBaseUrl) , body=loginParams, curl=wnsCurl))$login$result
    if (length(loginResponse) > 0 && loginResponse == "Success") {
      message(loginResponse)
      editToken <- content( GET(sprintf("%s?action=query&meta=tokens&type=csrf&format=json", wnsBaseUrl), curl=wnsCurl))$query$tokens$csrftoken
      
      #--------------------------------------------------
      # Controles of Id, Codes of Omschrijving al bestaan
      #--------------------------------------------------
      lGuid <- controleerWNS("Id",input$wnsId,wnsTekstUrl,wnsCurl)
      message(lGuid)
      lGuid <- controleerWNS("Codes",globalValues$wnsCodes,wnsTekstUrl,wnsCurl)
      lGuid <- controleerWNS("Omschrijving",zoekString(globalValues$wnsOmschrijving),wnsTekstUrl,wnsCurl)
      
      # Controle Begin geldigheid op morgen als geen historie te verwerken
      if (!input$historieWijzigen && input$wnsBeginGeldigheid <= Sys.Date()) {
        globalValues$wnsStatus <<- str_trim(paste(tidyr::replace_na(globalValues$wnsStatus,""),"Begindatum moet in de toekomst liggen",sep=" ")
                                            ,side = "both")
      }
      if (nchar(input$wnsTypering) > 0) {
        if (nchar(input$wnsGrootheid) > 0 || nchar(input$wnsChemischeStofObject) > 0) {
          globalValues$wnsStatus <<- str_trim(paste(tidyr::replace_na(globalValues$wnsStatus,"")
                                                    ,"Typering & Grootheid/ChemischestofObject niet tegelijk vullen",sep=" ")
                                              ,side = "both")
        }
      }
      
      #--------- Toevoegen van de nieuwe WNS
      if (is.null(globalValues$wnsStatus)) {

        message("Correct dus toevoegen")
        lGuid <- paste("Id-",uuid::UUIDgenerate(),sep="")

        message(globalValues$wnsOmschrijving)
        if (!is.na(lGuid)) {
          wikitekst <- paste("{{#element:\n",
                             "|Elementtype=DomeinwaardeVerzamellijst\n",
                             "|Paginanaam=",lGuid,"\n",
                             "|Voorkeurslabel=",globalValues$wnsOmschrijving,"\n",
                             "|GUID=",lGuid,"\n",
                             "|Id=",input$wnsId,"\n",
                             "|Codes=",globalValues$wnsCodes,"\n",
                             "|Typering=",replace_na(wnsTyperingGuid[input$wnsTypering],""),"\n",
                             "|Grootheid=",replace_na(wnsGrootheidGuid[input$wnsGrootheid],""),"\n",
                             "|ChemischeStofObject=",replace_na(wnsChemischeStofObjectGuid[input$wnsChemischeStofObject],""),"\n",
                             "|Eenheid2=",replace_na(wnsEenheidGuid[input$wnsEenheid],""),"\n",
                             "|Hoedanigheid2=",replace_na(wnsHoedanigheidGuid[input$wnsHoedanigheid],""),"\n",
                             "|Compartiment2=",replace_na(wnsCompartimentGuid[input$wnsCompartiment],""),"\n",
                             "|Omschrijving=",globalValues$wnsOmschrijving,"\n",
                             "|Breder=","Id-a225c399-903d-4ee1-ec2f-521b4f255cb5","\n",
                             "|Begin geldigheid=",input$wnsBeginGeldigheid,"\n",
                             "|Eind geldigheid=",input$wnsEindGeldigheid,"\n",
                             "|Wijzigingsnummer=",input$wnsWijzigingsnummer,"\n",
                             "|Status=",input$wnsStatus,"\n",
                             "}}",sep = '')
          editParams=list('title'=toString(lGuid),'text'=wikitekst,'token'=editToken,'format'='json')
          
          message("title----------------------------------------")
          message(editParams$title)
          message("text----------------------------------------")
          message(editParams$text)
          message("token----------------------------------------")
          message(editParams$token)
          message("format----------------------------------------")
          message(editParams$format)
          
          editResponse <- content( POST(sprintf("%s?action=edit&format=json", wnsBaseUrl) , body=editParams, curl=wnsCurl) )
          editStatus <- editResponse$edit$result
          message("Het resultaat ")
          if (nchar(toString(editStatus)) == 0 || toString(editStatus) != "Success") {
            doorgaan <- FALSE
            showModal(modalDialog(title = paste("Fout bij het opvoeren"), footer = modalButton("OK")))
          }
          
          globalValues$wnsStatus <<- "Oke"
        }
      }
    }
  })
  
  #---------------------------------------------------------------------------
  # De omschrijving van een WNS wordt automatisch bepaald als concatenatie van
  # de codes van de 6 verschillende onderdelen, allen omsloten met [].
  #
  # Bijv. [AFMD] [] [] [/m] [105oC] [AL]
  #---------------------------------------------------------------------------
  wnsBepaalOmschrijving <- function(){
    globalValues$wnsOmschrijving <- 
      paste("[",replace_na(input$wnsTypering,""),"] ",
            "[",replace_na(input$wnsGrootheid,""),"] ",
            "[",replace_na(input$wnsChemischeStofObject,""),"] ",
            "[",replace_na(input$wnsEenheid,""),"] ",
            "[",replace_na(input$wnsHoedanigheid,""),"] ",
            "[",replace_na(input$wnsCompartiment,""),"]",
            sep = "")
  }

  # Ken de Status uit de globale variabele toe aan het statusveld op het scherm
  output$wnsToevoegStatus <- renderText({
    message(globalValues$wnsStatus)
    globalValues$wnsStatus
  })  

  # Ken de Codes uit de globale variabele toe aan het codeveld op het scherm
  output$wnsCodes <- renderText({
    globalValues$wnsCodes
  })
  
  # Ken de Omschrijving uit de globale variabele toe aan het omschrijvingsveld op het scherm
  output$wnsOmschrijving <- renderText({
    globalValues$wnsOmschrijving
  })
  
  #---------------------------------------------------------------------------------------------------------
  # Als één van onderstaande velden in t scherm van waarde wijzigt wordt de status op "Te controleren" gezet
  #---------------------------------------------------------------------------------------------------------
  
  observeEvent(input$wnsTypering, {
    wnsBepaalOmschrijving()
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsGrootheid, {
    wnsBepaalOmschrijving()
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsChemischeStofObject, {
    wnsBepaalOmschrijving()
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsEenheid, {
    wnsBepaalOmschrijving()
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsHoedanigheid, {
    wnsBepaalOmschrijving()
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsCompartiment, {
    wnsBepaalOmschrijving()
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsId, {
    globalValues$wnsCodes <- str_replace(paste("WNS",input$wnsId,sep = ""),"NA","")
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsStatus, {
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsBeginGeldigheid, {
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsEindGeldigheid, {
    globalValues$wnsStatus <- "Te controleren"
  })
  
  observeEvent(input$wnsOmgeving, {
    req(globalValues$loginStatus)
    if (globalValues$loginStatus == "Ingelogd") {
      message(paste("Wijzigen WNSomgeving",globalValues$loginStatus))
      wnsVulDropDownLists()
    }
  })
  
  #---------------------------------------------------------------
  # Blok 2: beëindigen van een WNS
  #---------------------------------------------------------------
  
  observeEvent(input$OpzoekenWNS, {
    # ophalenWaarnemingssoort staat in Hulpfuncties.r
    WNSBeeindigen <<- ophalenWaarnemingssoort(input$WNSCodeHistoriseren,metadataWNS,input$Omgeving,opmaakJson)
    message(colnames(WNSBeeindigen))
    
    # Globals toekennen om in het scherm te kunnen tonen
    globalValues$WNSOmschrijvingHistoriseren <- WNSBeeindigen[1,"Omschrijving"]
    globalValues$WNSBeginGeldigheidHistoriseren <- WNSBeeindigen[1,"Begin geldigheid"]
    globalValues$WNSEindGeldigheidHistoriseren <- WNSBeeindigen[1,"Eind geldigheid"]
    message(colnames(WNSBeeindigen))
    message(WNSBeeindigen)
  })
  
  output$WNSOmschrijvingHistoriseren <- renderText({
    globalValues$WNSOmschrijvingHistoriseren
  })
  
  output$WNSBeginGeldigheidHistoriseren <- renderText({
    globalValues$WNSBeginGeldigheidHistoriseren
  })
  
  output$WNSEindGeldigheidHistoriseren <- renderText({
    globalValues$WNSEindGeldigheidHistoriseren
  })
  
  # Daadwerkelijk beëindigen van een WNS
  observeEvent(input$BeeindigenWNS, {
    req(input$wnsWijzigingsnummerHistoriseren)
    if (!is.null(globalValues$WNSOmschrijvingHistoriseren)) {
      # Zetten van de juiste datum en status
      WNSBeeindigen[1,"Wijzigingsnummer"] <- trimws(paste(input$wnsWijzigingsnummerHistoriseren,', '
                                                          ,WNSBeeindigen[1,"Wijzigingsnummer"],sep = ""),
                                                    whitespace = "\\, ")
      WNSBeeindigen[1,"Eind geldigheid"] <- bewerkDatum(Sys.Date())
      WNSBeeindigen[1,"Status"] <- "H"
      # wijzigDomeinwaardeInWiki staat in Hulpfuncties.r
      wijzigDomeinwaardeInWiki(pOmgeving          = input$wnsOmgeving
                              ,pDomeintabel       = "Waarnemingssoort"
                              ,pMetadata          = metadataWNS
                              ,pUserName          = input$botUserName
                              ,pPassword          = input$botPassword
                              ,pWijzigingen       = WNSBeeindigen
                              ,pMaxRijen          = 1
                              ,pSepCSV            = input$sepCSV
                              ,pHistorieOpvragen  = FALSE
                              ,pUitBeheertool     = FALSE
                              ) 
      showModal(modalDialog(title = paste("Waarnemingssoort",WNSBeeindigen[1,"Codes"],"beëindigd per vandaag.")
                            , footer = modalButton("OK")))
    }
  })
  
  #---------------------------------------------------------------------------------------------------
  # Blok 3: uploaden van een CSV: Biotaxon, Parameter, Waardebepalingsmetthode en Waarnemingssoort
  #---------------------------------------------------------------------------------------------------
  
  observeEvent(input$WikiUploadCSV, {
    req(input$DomeintabelUpload)
    #directory <- setwd(tempdir())
    singleFile <<- TRUE
    
    lMetadata <- bepaalMetadata(input$DomeintabelUpload,domainsJson)
    message(lMetadata)
    
    # Inloggen
    wnsBaseUrl <- paste("https://",input$wnsOmgeving,".aquo.nl/api.php",sep="")
    wnsCurl <- getCurlHandle()
    botToken <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", wnsBaseUrl), curl=curl ) )$query$tokens$logintoken
    loginParams=list('lgtoken'=botToken,'lgname'=input$botUserName,'lgpassword'=input$botPassword)
    loginResponse <- content( POST(sprintf("%s?action=login&format=json", wnsBaseUrl) , body=loginParams, curl=curl) )$login$result
    editToken <- content( GET(sprintf("%s?action=query&meta=tokens&type=csrf&format=json", wnsBaseUrl), curl=curl ))$query$tokens$csrftoken
    
    
    #---------------------------------------------------------------------------------------------------
    # Voor waarnemingssoort worden 4 CSV's ge-upload:
    # - WaarnemingssoortMetGUID.csv
    # - WaarnemingssoortZonderGUID.csv
    # - WaarnemingssoortMetCodes.csv
    # Hiervoor moet na ophalen
    #---------------------------------------------------------------------------------------------------
    if (input$DomeintabelUpload == "Waarnemingssoort") {
      ophalenMeerdereDomeintabellen(pDomeintabel=input$DomeintabelUpload,pOmgeving=input$wnsOmgeving,pCategorie="DomeinwaardenVerzamellijsten",pHistorie=TRUE)
      message("Het gaat om waarnemingssoort")
      
      domValuesCSV[is.na(domValuesCSV)] <<- ""
      col_order <- c("Id","Codes","Omschrijving","Typering","Grootheid","ChemischeStofObject"
                     ,"Eenheid2","Hoedanigheid2","Compartiment2","Status","Begin geldigheid","Eind geldigheid","Guid")
      col_names <- c("Id","Codes","Omschrijving","Typering","Grootheid","ChemischeStofObject"
                     ,"Eenheid","Hoedanigheid","Compartiment","Status","Begin geldigheid","Eind geldigheid","Guid")
      uploadDF <- domValuesCSV[,col_order]
      for (col in colnames(uploadDF)) uploadDF[,col] <- gsub("^.*?, 0, 1, ","",uploadDF[,col])
      colnames(uploadDF) <- col_names
      
      #------- Schrijven van WNS met Guid
      fileName <- paste(input$DomeintabelUpload,"MetGUID.csv",sep = "")
      fileNameInclDir <- paste(tempdir(),"/",fileName,sep = "")
      write.table(uploadDF,fileNameInclDir,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
      bestand <- httr::upload_file(path = fileNameInclDir)
      URL <- paste(wnsBaseUrl,"?action=upload&format=json&ignorewarnings=1",sep = "")
      result <- httr::POST(URL, config = list(authenticate(input$botUserName,input$botPassword)),body=list(filename=fileName,token=editToken, file=bestand))
      message(paste(fileName,content(result)))
      
      #------- Schrijven van WNS zonder Guid
      fileName <- paste(input$DomeintabelUpload,"ZonderGUID.csv",sep = "")
      fileNameInclDir <- paste(tempdir(),"/",fileName,sep = "")
      col_names_no_guid <- c("Id","Codes","Omschrijving","Typering","Grootheid","ChemischeStofObject"
                            ,"Eenheid","Hoedanigheid","Compartiment","Status","Begin geldigheid","Eind geldigheid")
      write.table(uploadDF[,col_names_no_guid],fileNameInclDir,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
      bestand <- httr::upload_file(path = fileNameInclDir)
      URL <- paste(wnsBaseUrl,"?action=upload&format=json&ignorewarnings=1",sep = "")
      result <- httr::POST(URL, config = list(authenticate(input$botUserName,input$botPassword)),body=list(filename=fileName,token=editToken, file=bestand))
      message(paste(fileName,content(result)))
      
      #------- Schrijven van WNS o.b.v. code
      uploadDF <- domValuesCSV[,col_order]
      colnames(uploadDF) <- col_names
      for (i in 1:nrow(uploadDF)) {
        # Bij onderstaande velden wordt ook de omschrijving opgehaald achter de Guid, die wordt er weer vanaf gesloopt
        tryCatch(
          {
            if (uploadDF[i,"Typering"] != "")
              uploadDF[i,"Typering"] <- wnsTyperingCodesGuid[substr(uploadDF[i,"Typering"],1,39)]
            if (uploadDF[i,"Grootheid"] != "")
              uploadDF[i,"Grootheid"] <- wnsGrootheidCodesGuid[substr(uploadDF[i,"Grootheid"],1,39)]
            if (uploadDF[i,"Eenheid"] != "")
              uploadDF[i,"Eenheid"] <- wnsEenheidCodesGuid[substr(uploadDF[i,"Eenheid"],1,39)]
            if (uploadDF[i,"Compartiment"] != "")
              uploadDF[i,"Compartiment"] <- wnsCompartimentCodesGuid[substr(uploadDF[i,"Compartiment"],1,39)]
            if (uploadDF[i,"ChemischeStofObject"] != "")
              uploadDF[i,"ChemischeStofObject"] <- wnsChemischeStofObjectCodesGuid[substr(uploadDF[i,"ChemischeStofObject"],1,39)]
            if (uploadDF[i,"Hoedanigheid"] != "")
             uploadDF[i,"Hoedanigheid"] <- wnsHoedanigheidCodesGuid[substr(uploadDF[i,"Hoedanigheid"],1,39)]
          },
          warning = function(cond){
            message(paste("Warning",substr(uploadDF[i,"Typering"],1,39),substr(uploadDF[i,"Grootheid"],1,39),substr(uploadDF[i,"Eenheid"],1,39)
                          ,substr(uploadDF[i,"Hoedanigheid"],1,39),substr(uploadDF[i,"Compartiment"],1,39),substr(uploadDF[i,"ChemischeStofObject"],1,39),paste = " - "))
          },
          error = function(cond){
            message(paste("Error",uploadDF[i,"Typering"],uploadDF[i,"Grootheid"],uploadDF[i,"Eenheid"]
                          ,uploadDF[i,"Hoedanigheid"],uploadDF[i,"Compartiment"],uploadDF[i,"ChemischeStofObject"],paste = " - "))
          }
        )
      }
      fileName <- paste(input$DomeintabelUpload,"MetCodes.csv",sep = "")
      fileNameInclDir <- paste(tempdir(),"/",fileName,sep = "")
      colnames(uploadDF) <- col_names
      write.table(uploadDF[,col_names_no_guid],fileNameInclDir,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
      bestand <- httr::upload_file(path = fileNameInclDir)
      URL <- paste(wnsBaseUrl,"?action=upload&format=json&ignorewarnings=1",sep = "")
      result <- httr::POST(URL, config = list(authenticate(input$botUserName,input$botPassword)),body=list(filename=fileName,token=editToken, file=bestand))
      message(paste(fileName,content(result)))
      
    }
    
    if (input$DomeintabelUpload == "Biotaxon") {
      ophalenMeerdereDomeintabellen(pDomeintabel=input$DomeintabelUpload,pOmgeving=input$wnsOmgeving,pCategorie="Domeinwaarden",pHistorie=TRUE)
      message("Het gaat om Biotaxon")
      fileName <- paste(input$DomeintabelUpload,".csv",sep = "")
      fileNameInclDir <- paste(tempdir(),"/",fileName,sep = "")
      col_order <- c("Id","Naam","Auteur","Taxonniveau","Taxonouder","Verwijsnaam","Naam Nederlands"
                     ,"TWNmutatiedatum","TWNstatus","Begin geldigheid","Eind geldigheid","Gerelateerd","Guid")
      uploadDF <- domValuesCSV[,col_order]
      colnames(uploadDF) <- col_order
      uploadDF[is.na(uploadDF)] <- ""
      write.table(uploadDF,fileNameInclDir,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
      bestand <- httr::upload_file(path = fileNameInclDir)
      URL <- paste(wnsBaseUrl,"?action=upload&format=json&ignorewarnings=1",sep = "")
      result <- httr::POST(URL, config = list(authenticate(input$botUserName,input$botPassword)),body=list(filename=fileName,token=editToken, file=bestand))
      message(content(result))
    }

    if (input$DomeintabelUpload == "Waardebepalingsmethode") {
      ophalenMeerdereDomeintabellen(pDomeintabel=input$DomeintabelUpload,pOmgeving=input$wnsOmgeving,pCategorie="Domeinwaarden",pHistorie=TRUE)
      message("Het gaat om Waardebepalingsmethode")
      fileName <- paste(input$DomeintabelUpload,"MetGUID.csv",sep = "")
      fileNameInclDir <- paste(tempdir(),"/",fileName,sep = "")
      col_order <- c("Id", "Codes", "Omschrijving", "Groep", "Titel", "Begin geldigheid", "Eind geldigheid", "Status", "Gerelateerd","Guid")
      uploadDF <- domValuesCSV[,col_order]
      colnames(uploadDF) <- col_order
      uploadDF[is.na(uploadDF)] <- ""
      write.table(uploadDF,fileNameInclDir,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
      bestand <- httr::upload_file(path = fileNameInclDir)
      URL <- paste(wnsBaseUrl,"?action=upload&format=json&ignorewarnings=1",sep = "")
      result <- httr::POST(URL, config = list(authenticate(input$botUserName,input$botPassword)),body=list(filename=fileName,token=editToken, file=bestand))
      message(content(result))
      
      fileName <- paste(input$DomeintabelUpload,".csv",sep = "")
      fileNameInclDir <- paste(tempdir(),"/",fileName,sep = "")
      col_order <- col_order[!col_order == "Guid"]
      uploadDF <- domValuesCSV[,col_order]
      colnames(uploadDF) <- col_order
      uploadDF[is.na(uploadDF)] <- ""
      write.table(uploadDF,fileNameInclDir,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
      bestand <- httr::upload_file(path = fileNameInclDir)
      URL <- paste(wnsBaseUrl,"?action=upload&format=json&ignorewarnings=1",sep = "")
      result <- httr::POST(URL, config = list(authenticate(input$botUserName,input$botPassword)),body=list(filename=fileName,token=editToken, file=bestand))
      message(content(result))
      
    }
    
    if (input$DomeintabelUpload == "Parameter") {
      ophalenMeerdereDomeintabellen(pDomeintabel=input$DomeintabelUpload,pOmgeving=input$wnsOmgeving,pCategorie="Domeinwaarden",pHistorie=TRUE)
      message("Het gaat om Parameter")
      fileName <- paste(input$DomeintabelUpload,"MetGUID.csv",sep = "")
      fileNameInclDir <- paste(tempdir(),"/",fileName,sep = "")
      col_order <- c("Id", "CASnummer","Codes", "Omschrijving", "Groep", "Begin geldigheid", "Eind geldigheid", "Status", "Gerelateerd","Guid")
      uploadDF <- domValuesCSV[,col_order]
      colnames(uploadDF) <- col_order
      uploadDF[is.na(uploadDF)] <- ""
      write.table(uploadDF,fileNameInclDir,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
      bestand <- httr::upload_file(path = fileNameInclDir)
      URL <- paste(wnsBaseUrl,"?action=upload&format=json&ignorewarnings=1",sep = "")
      result <- httr::POST(URL, config = list(authenticate(input$botUserName,input$botPassword)),body=list(filename=fileName,token=editToken, file=bestand))
      message(content(result))
      
      fileName <- paste(input$DomeintabelUpload,".csv",sep = "")
      fileNameInclDir <- paste(tempdir(),"/",fileName,sep = "")
      col_order <- col_order[!col_order == "Guid"]
      uploadDF <- domValuesCSV[,col_order]
      colnames(uploadDF) <- col_order
      uploadDF[is.na(uploadDF)] <- ""
      write.table(uploadDF,fileNameInclDir,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
      bestand <- httr::upload_file(path = fileNameInclDir)
      URL <- paste(wnsBaseUrl,"?action=upload&format=json&ignorewarnings=1",sep = "")
      result <- httr::POST(URL, config = list(authenticate(input$botUserName,input$botPassword)),body=list(filename=fileName,token=editToken, file=bestand))
      message(content(result))
      
    }
    
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------
  # Vanaf hier functionaliteit voor tabblad Begrippen
  #-----------------------------------------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------
  # Inlezen van de CSV met begrippen
  #-------------------------------------------------------------------------------------
  observeEvent(input$CSVBegrippen, {
    wBegrippen <<- read.csv(input$CSVBegrippen$datapath,header=TRUE, sep = input$sepCSVBegrippen, encoding = defaultEncoding
                                      , stringsAsFactors=FALSE,na.strings=c(""), check.names = FALSE)
    # Zorg dat ALLE NA omgezet worden in een lege string
    wBegrippen[is.na(wBegrippen)] <<- ""
    #message(wBegrippen)
    colnames(wBegrippen) <<- gsub("\\."," ",colnames(wBegrippen))
    # N.a.v. Trello kaartje dat onduidelijk is wat de oorzaak van het niet kunnen inlezen is
    message(paste("1: kolomnamen wijzigingBegrippen",unlist(colnames(wBegrippen)),sep = "#"))
    if (length(unlist(colnames(wBegrippen))) == 1) {
      showModal(modalDialog(title = "Let op! Verkeerde scheidingsteken gebruikt bij inlezen"
                            , footer = modalButton("OK")))
      wBegrippen <<- data.frame()
    }
    else {
      if (!("Begin geldigheid" %in% colnames(wBegrippen) &&
            "Eind geldigheid" %in% colnames(wBegrippen)
      )
      )
      {
        showModal(modalDialog(title = "Let op! Kolommen ontbreken in ingelezen bestand (Begin geldigheid/Eind geldigheid)"
                              , footer = modalButton("OK")))
        wBegrippen <<- data.frame()
      }
      else {
        wBegrippen$'Controle meldingen' <<- NA
        #colnames(wBegrippen)[1] <<- "Guid"
        # vervangen van punt door spatie in column names
        
        # Omzetten ingelezen datumformaat in YYYY-MM-DD
        for (i in 1:nrow(wBegrippen)) {
          #message(paste("Eind geldigheid: ",wBegrippen[i,"Eind geldigheid"]," ",nrow(wBegrippen)))
          if (!is.na(wBegrippen[i,"Begin geldigheid"])) wBegrippen[i,"Begin geldigheid"] <<- bewerkDatum(wBegrippen[i,"Begin geldigheid"])
          if (!is.na(wBegrippen[i,"Eind geldigheid"])) wBegrippen[i,"Eind geldigheid"] <<- bewerkDatum(wBegrippen[i,"Eind geldigheid"])
          #message(paste(wBegrippen[i,"Begin geldigheid"],wBegrippen[i,"Eind geldigheid"],sep=" - "))
        }
        
        #message(paste("2:",colnames(wBegrippen)))
        output$wijzigingenBegrippen <- DT::renderDataTable({
          DT::datatable(wBegrippen,  caption = "Begrippen", filter = "top", selection="single", class = 'cell-border nowrap',
                        options = list(lengthMenu = c(1,5,25, 50, 100), pageLength = 100,scrollX = TRUE
                                       #,columnDefs = list(list(className = "nowrap", targets = "_all"))
                                       ,autoWidth = TRUE
                        )
          )
        })
      }
    }
 })
  
  output$loggedInBoxBegrippen <- renderText({
    globalValues$loginStatus
  })
  
  output$omgevingBegrippen <- renderText({
    globalValues$omgeving
  })
  
  #-------------------------------------------------------------------------------------
  # Uitvoeren van controles op begrippen:
  #  Controleren of metadata begrippen overeenkomt met de ingelezen CSV
  # Wijze van controleren is analoog aan de controle van domeinwaarden
  #-------------------------------------------------------------------------------------
  observeEvent(input$ControlerenBegrippen, {
    message("1 - Start controleren begrippen, ")
    print(environment())
    if (nrow(wBegrippen) > 0 && globalValues$loginStatus == "Ingelogd") {
    req(input$CSVBegrippen)
      if (nrow(wBegrippen) >= 100) {
        lFactor <- trunc(nrow(wBegrippen) / 100)
        lProgress <- 0.01
      }
      else {
        lFactor <- 1
        lProgress <- round(1/nrow(wBegrippen),2)
      }
      message(paste("lFactor:",lFactor,"lProgress",lProgress))
      withProgress(message = "Controleren van ingelezen begrippen", value = 0, {
        
        #---------------------------------------
        # Controleren of metadata overeenkomt
        #---------------------------------------
        wBegrippen[,"Controle meldingen"] <<- ""
        okMetadata <- TRUE
        for (i in 1:length(metadataBegrippen)) {
          if (!(metadataBegrippen[i] %in% colnames(wBegrippen))) {
            message(paste("Wijzigingen metadata-kolom ontbreekt:",metadataBegrippen[i],sep = " "))
            okMetadata <- FALSE
          }
        }
        if (!(okMetadata)) {
                    wBegrippen[,"Controle meldingen"] <<- "Metadata begrip wijkt af van CSV"
        }
        else {
          for (i in 1:length(colnames(wBegrippen))) {
            #message(colnames(wBegrippen)[i])
            if (!(colnames(wBegrippen)[i] %in% c('Guid','Controle meldingen'))
                && !(colnames(wBegrippen)[i] %in% metadataBegrippen)) {
              message(paste("Metadata wBegrippen-kolom ontbreekt:",colnames(wBegrippen)[i],sep = ""))
              okMetadata <- FALSE
            }
          }
          if (!(okMetadata)) {
            wBegrippen[,"Controle meldingen"] <<- "Metadata begrip wijkt af van CSV"
          }
        }
        if (okMetadata) {
          for (i in 1:nrow(wBegrippen)) {
            if (i %% lFactor == 0) incProgress(lProgress)
            message(paste("2 - Controleren rij (#) met Id ():",i,wBegrippen[i,"Id"],sep = " " ))
            
            ok <- TRUE
            message(paste("3 -"),toString(wBegrippen[i,"Guid"]))
            if (nchar(toString(wBegrippen[i,"Guid"])) > 0) {
              
              # Ophalen bestaande begrip
              begripExist <- ophalenBegrippen(pGuid             = wBegrippen[i,"Guid"]
                                             ,pMaxRijen         = 1
                                             ,pOmgeving         = input$Omgeving
                                             ,pGuidOphalen      = TRUE
                                             ,pOpmaakJson       = opmaakJson
                                             ,pUitBeheertool    = FALSE     # nu wel, zit al in with progress loop
                                             ,pSmaller          = FALSE)

              #----------------------------------------------------------------
              # Als begin geldigheid 1800-01-01 is dan wijst de Guid van 
              # de gecontroleerde rij niet naar een begrip maar iets anders
              #----------------------------------------------------------------
              if (nrow(begripExist) > 0 && begripExist[1,"Begin geldigheid"] == "Geen begrip") {
                ok <- FALSE
                message("Guid wel bekend maar geen begrip")
                wBegrippen[i,"Controle meldingen"] <<- "Guid wel bekend maar geen begrip"
              }
            }
            # else {
            #   # controleren of Guid voorkomt als iets anders dan een begrip --> dan fout
            #   categorie <- paste("?title=Speciaal:Vragen&x=-5B-5B",gsub("-","-2D",wBegrippen[i,"Guid"]),"-5D-5D",sep="")
            #   beperking <- NULL
            #   kenmerken <- "%2F-3FElementtype"
            #   lOpmaakJson <- str_replace(opmaakJson,"lLimit",toString(lLimit))
            #   json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
            #   #message(json_file)
            #   req <- httr::GET(json_file, curl=curl)
            #   # 2022-09-27: door lengte content te checken voorkomen foutmelding
            #   jsonContent <- httr::content(req, "text", encoding="UTF-8")
            #   if ((req$status_code == 200 && nchar(jsonContent) == 0)) {
            #     ok <- TRUE
            #     begripExist <- data.frame(matrix(ncol = 0, nrow = 0))
            #   } 
            #   else {
            #     ok <- FALSE
            #     message("Guid wel bekend maar geen begrip")
            #     wBegrippen[i,"Controle meldingen"] <<- "Guid wel bekend maar geen begrip"
            #   }
              
            # }

            if (ok && nrow(beheertoolControlesBegrippen) > 0) {
              for (voorwaarde in 1:nrow(beheertoolControlesBegrippen)) {
                lKenmerk <- beheertoolControlesBegrippen[voorwaarde,]$Kenmerk
                #message(paste("Te controleren kenmerk:",lKenmerk))
                
                #--------------------------------------------------------
                # Controles uit BeheertoolControles.xlsx
                #--------------------------------------------------------
                if (lKenmerk %in% metadataBegrippen)
                {
                  # Controle op verplichte velden
                  message(paste("Te controleren kenmerk:",lKenmerk," - te controleren waarde:",replace_na(wBegrippen[i,lKenmerk],"")))
                  if (beheertoolControlesBegrippen[voorwaarde,]$Verplicht == "J" &&
                      toString(tidyr::replace_na(wBegrippen[i,lKenmerk],"")) == "")
                  {
                    message(paste("Verplicht:",lKenmerk))
                    wBegrippen[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wBegrippen[i,"Controle meldingen"],"")
                                                                  ,paste(lKenmerk,"is verplicht")
                                                                  ,sep="<br/>")
                    ok <- FALSE
                  }
                  #message("na verplicht")
                  
                  # Controle op numerieke velden
                  if (beheertoolControlesBegrippen[voorwaarde,]$Datatype == "Integer" &&
                      grepl("[^0-9]", wBegrippen[i,lKenmerk]))
                  {
                    message(paste("Integer:",lKenmerk))
                    wBegrippen[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wBegrippen[i,"Controle meldingen"],"")
                                                                  ,paste(lKenmerk,"is geen geheel getal")
                                                                  ,sep="<br/>")
                    ok <- FALSE
                  }
                  #message("na integer")
                  
                  # Controle op verwijzing naar ander begrip/collectie/symbool
                  okVerwijzing <- TRUE
                  if (wBegrippen[i,lKenmerk] != "" &&
                      beheertoolControlesBegrippen[voorwaarde,]$Datatype %in% c("Begrip","Collectie","Symbool")) {
                    okVerwijzing <- FALSE
                    lCollectie <- NULL
                    for (rVerwijzing in c("Begrip","Collectie","Symbool")) {
                      if (!okVerwijzing &&
                          beheertoolControlesBegrippen[voorwaarde,]$Datatype == rVerwijzing &&
                          nchar(toString(tidyr::replace_na(wBegrippen[i,lKenmerk],""))) > 0)
                      {
                        message(paste("Verwijzing",rVerwijzing,":",lKenmerk))
                        lVerwijzingen <- strsplit(wBegrippen[i,lKenmerk],",")
                        for (j in 1:length(lVerwijzingen[[1]])) {
                          lVerwijzing <- lVerwijzingen[[1]][j]
                          message(lVerwijzing)
                          if (beheertoolControlesBegrippen[voorwaarde,]$Datatype %in% c("Begrip","Symbool")) {
                            lOk <- ophalenBegrip(lVerwijzing,input$Omgeving,metadataBegrippen,opmaakJson)
                            lVerwijzingType <- lOk[[1]]$printouts$Elementtype$fulltext
                            if (length(lOk) > 0 && lVerwijzingType == rVerwijzing) okVerwijzing <- TRUE
                            else                                                   okVerwijzing <- FALSE
                          }
                          else if (beheertoolControlesBegrippen[voorwaarde,]$Datatype == "Collectie") {
                            lCollGuid <- ophalenCollectieGuid(lVerwijzing,input$Omgeving,opmaakJson)
                            if (is.na(lCollGuid)) okVerwijzing <- FALSE
                            else {
                              okVerwijzing <- TRUE
                              lCollectie <- paste(lCollectie,lCollGuid,sep = ",")
                            }
                          }
                          else message("Fout bij bepalen begrip (bestaat niet of is geen",rVerwijzing,lVerwijzing)
                        }
                      }
                    }
                  }
                  if (!okVerwijzing) {
                    wBegrippen[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wBegrippen[i,"Controle meldingen"],"")
                                                                           ,paste(lKenmerk,"is geen (lijst van)"
                                                                                  ,beheertoolControlesBegrippen[voorwaarde,]$Datatype)
                                                                           ,sep="<br/>")
                    ok <- FALSE
                  }
                  #message("na begrip/collectie/symbool")
                  
                  
                  # Controle op lengte velden

                  if (wBegrippen[i,lKenmerk] != "" &&
                      beheertoolControlesBegrippen[voorwaarde,]$Lengte != "" &&
                      nchar(toString(tidyr::replace_na(wBegrippen[i,lKenmerk],""))) >
                      tidyr::replace_na(beheertoolControlesBegrippen[voorwaarde,]$Lengte,1048576)) # was 1024 wat te kort was, nu 2^20
                  {
                    message(paste("Veld te lang:",lKenmerk))
                    wBegrippen[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wBegrippen[i,"Controle meldingen"],"")
                                                                  ,paste(lKenmerk,"is langer dan",beheertoolControlesBegrippen[voorwaarde,]$Lengte)
                                                                  ,sep="<br/>")
                    ok <- FALSE
                  }
                  #message("na lengte")
                  
                  # Controle op vergelijken met ander kenmerk
                  # - Als SoortWijziging leeg, dan altijd
                  # - Als SoortWijziging Toevoegen, dan alleen controleren met lege Guid
                  # - Als SoortWijziging Wijzigen, dan alleen controleren met gevulde Guid
                  if (beheertoolControlesBegrippen[voorwaarde,]$VergelijkenMetKenmerk != "" &&
                      (beheertoolControlesBegrippen[voorwaarde,]$SoortWijziging == "" ||
                       (beheertoolControlesBegrippen[voorwaarde,]$SoortWijziging == "Toevoegen" &&
                        wBegrippen[i,"Guid"] == ""
                       ) ||
                       (beheertoolControlesBegrippen[voorwaarde,]$SoortWijziging == "Wijzigen" &&
                        wBegrippen[i,"Guid"] != ""
                       )
                      )
                  ) {
                    message("Controle op vergelijken met ander kenmerk")
                    if (!beheertoolControlesBegrippen[voorwaarde,]$Datatype == "Datum" || !input$historieWijzigen) {
                      lVergelijkenMetKenmerk <- beheertoolControlesBegrippen[voorwaarde,]$VergelijkenMetKenmerk
                      lVergelijking <- beheertoolControlesBegrippen[voorwaarde,]$Vergelijking
                      message(paste("dynamische controle:",lVergelijkenMetKenmerk,lVergelijking))
                      lCondition <- paste("\"",str_trim(toString(tidyr::replace_na(wBegrippen[i,lKenmerk],"")),side = "both"),"\""
                                          ," ",lVergelijking," "
                                          ,"\"",str_trim(toString(tidyr::replace_na(wBegrippen[i,lVergelijkenMetKenmerk],"")),side = "both"),"\""
                                          ,sep = "")
                      message(paste("dynamische conditie:",lCondition))
                      lControle <- eval(parse(text=lCondition))
                      if (!lControle) {
                        wBegrippen[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wBegrippen[i,"Controle meldingen"],"")
                                                                      ,paste("Onwaar:",lKenmerk,lVergelijking,lVergelijkenMetKenmerk)
                                                                      ,sep="<br/>")
                        ok <- FALSE
                      }
                    }
                  }
                  #message("Na controle op vergelijken met ander kenmerk")
                  
                  # Controle op vergelijken met dynamische waarde
                  # - Als SoortWijziging leeg, dan altijd
                  # - Als SoortWijziging Toevoegen, dan alleen controleren met lege Guid
                  # - Als SoortWijziging Wijzigen, dan alleen controleren met gevulde Guid
                  if (beheertoolControlesBegrippen[voorwaarde,]$VergelijkenDynamisch != "" &&
                      (beheertoolControlesBegrippen[voorwaarde,]$SoortWijziging == "" ||
                       (beheertoolControlesBegrippen[voorwaarde,]$SoortWijziging == "Toevoegen" &&
                        wBegrippen[i,"Guid"] == ""
                       ) ||
                       (beheertoolControlesBegrippen[voorwaarde,]$SoortWijziging == "Wijzigen" &&
                        wBegrippen[i,"Guid"] != ""
                       )
                      )
                  ) 
                  {
                    message("Controle op vergelijken met dynamische waarde")
                    if (!beheertoolControlesBegrippen[voorwaarde,]$Datatype == "Datum" || !input$historieWijzigen) {
                      lwaarde <- beheertoolControlesBegrippen[voorwaarde,]$VergelijkenDynamisch
                      lVergelijkenDynamisch <- NA
                      lVergelijkenDynamisch <- as.character(lVergelijkenDynamisch)
                      while (tidyr::replace_na(str_locate(lwaarde,"\\|")[2],0) > 0) {
                        lVergelijkenDynamisch <- c(lVergelijkenDynamisch,substr(lwaarde,1,str_locate(lwaarde,"\\|")[2]-1))
                        lwaarde  <- substr(lwaarde,str_locate(lwaarde,"\\|")[2]+1,1024)
                      }
                      lVergelijkenDynamisch <- tidyr::replace_na(lVergelijkenDynamisch,lwaarde)
                      lVergelijking <- beheertoolControlesBegrippen[voorwaarde,]$Vergelijking
                      lCondition <- ""
                      for (contr in 1:length(lVergelijkenDynamisch)) {
                        if (contr > 1) lCondition <-paste(lCondition,"|| ")
                        lCondition <- paste(lCondition,"\"",str_trim(toString(tidyr::replace_na(wBegrippen[i,lKenmerk],"")),side = "both"),"\""
                                            ,lVergelijking
                                            ,"toString(",lVergelijkenDynamisch[contr],")"
                                            ,sep = "")
                      }
                      message(lCondition)
                      lControle <- eval(parse(text=lCondition))
                      if (!lControle) {
                        wBegrippen[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wBegrippen[i,"Controle meldingen"],"")
                                                                      ,paste("Onwaar:",lKenmerk,lVergelijking
                                                                             ,beheertoolControlesBegrippen[voorwaarde,]$VergelijkenDynamisch)
                                                                      ,sep="<br/>")
                        ok <- FALSE
                      }
                    }
                  }
                  #message("Na controle op vergelijken met dynamische waarde")
                  
                  # Controles op bestaande begrip
                  if (nrow(begripExist) > 0)
                  {
                    message("4 - Controleren bestaande begrip")
                    
                    # Controle veld wijzigbaar
                    if (beheertoolControlesBegrippen[voorwaarde,]$Wijzigbaar == "N" &&
                        toString(wBegrippen[i,lKenmerk]) != 
                        toString(begripExist[,lKenmerk])) {
                      wBegrippen[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wBegrippen[i,"Controle meldingen"],"")
                                                                    ,paste(lKenmerk,"is niet wijzigbaar")
                                                                    ,sep="<br/>")
                      ok <- FALSE
                    }
                    
                  } # Controles op bestaande begrippen
                  
                  # Controle veld uniek
                  if (beheertoolControlesBegrippen[voorwaarde,]$Uniek == "J" &&
                      wBegrippen[i,lKenmerk] != "" &&
                      wBegrippen[i,"Controle meldingen"] == "") {
                    begripUniek <- controleerObjectUniek(pElementtype     = "Begrip"
                                                        ,pKenmerk         = lKenmerk
                                                        ,pKenmerkWaarde   = wBegrippen[i,lKenmerk]
                                                        ,pBeginGeldigheid = wBegrippen[i,"Begin geldigheid"]
                                                        ,pEindGeldigheid  = wBegrippen[i,"Eind geldigheid"]
                                                        ,pWijzigingen     = wBegrippen
                                                        ,pGuid            = wBegrippen[i,"Guid"]
                                                        ,pOmgeving        = input$Omgeving
                                                        ,pOpmaakJson      = opmaakJson
                    )
                    if (!begripUniek) {
                      wBegrippen[i,"Controle meldingen"] <<- paste(tidyr::replace_na(wBegrippen[i,"Controle meldingen"],"")
                                                                             ,paste(lKenmerk,"is niet uniek")
                                                                             ,sep="<br/>")
                      ok <- FALSE
                    }
                  }
                  
                }
                
              }  # for (voorwaarde in 1:nrow(beheertoolControlesBegrippen))
              
              #--------------- Juist maken van de controle melding
              wBegrippen[i,"Controle meldingen"] <<- gsub("NA<br/>","",wBegrippen[i,"Controle meldingen"])
              wBegrippen[i,"Controle meldingen"] <<- gsub("^<br/>","",wBegrippen[i,"Controle meldingen"])
              #message(nchar(stri_trim(toString(wBegrippen[i,"Controle meldingen"]))))
              if (nchar(stri_trim(toString(wBegrippen[i,"Controle meldingen"]))) %in% c(0,2) ||
                  wBegrippen[i,"Controle meldingen"] == "Nieuwe groep" ||
                  wBegrippen[i,"Controle meldingen"] == "Groep wordt gewijzigd"
              ) wBegrippen[i,"Controle meldingen"] <<- paste("OK",wBegrippen[i,"Controle meldingen"])
            }   # if (ok)
            
            # Plakken van nieuw wijzigingsnummer voor het oude
            if (nchar(wBegrippen[i,"Guid"]) > 0) {
              lOk <- ophalenBegrip(wBegrippen[i,"Guid"],input$Omgeving,metadataBegrippen,opmaakJson)
              
              lOudWnr <- lOk[[1]]$printouts$Wijzigingsnummer
              if (length(lOudWnr) > 0) {
                wBegrippen[i,"Wijzigingsnummer"] <<- trimws(paste(wBegrippen[i,"Wijzigingsnummer"]
                                                                            ,', ',lOudWnr,sep = ""),
                                                                      whitespace = "\\, ")
              }
            }
            
          }   # for (i in 1:nrow(wBegrippen))
        } # if (ok)
        message("3 - Na controleren alle rijen")
      }) # withProgress
      
      opnieuwTonenWijzigingen(wBegrippen,"Begrippen")
    }
    
    message("99 - Einde controleren")
    
  })
  
  # Dit levert een popup met waarschuwing op wanneer je wijzigingen op begrippen gaat doorvoeren
  modalMeldingWijzigenBegrippen <- function() {
    modalDialog(
      title = "Let op! Je gaat wijzigingen doorvoeren op begrippen",
      
      footer = tagList(
        modalButton("Annuleer"),
        actionButton("okBegrippen", "OK")
      )
    )
  }
  
  #-------------------------------------------------------------------------------------
  # Doorvoeren van de wijzigingen op begrippen
  #-------------------------------------------------------------------------------------
  observeEvent(input$WijzigenBegrippen, {
    req(input$CSVBegrippen,baseUrl,input$botUserName,input$botPassword)
    
    if (nrow(wBegrippen) > 0)
      showModal(modalMeldingWijzigenBegrippen())
    
  })
  
  observeEvent(input$okBegrippen, {
    message("Wijzigen begrippen")
    removeModal()
    
    wikitekst <- NULL
    
    curl <- getCurlHandle()
    ## Retrieve the login token
    botToken <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", baseUrl), curl=curl ) )$query$tokens$logintoken
    loginParams=list('lgtoken'=botToken,'lgname'=input$botUserName,'lgpassword'=input$botPassword)
    loginResponse <- content( POST(sprintf("%s?action=login&format=json", baseUrl) , body=loginParams, curl=curl) )$login$result
    editToken <- content( GET(sprintf("%s?action=query&meta=tokens&type=csrf&format=json", baseUrl), curl=curl ))$query$tokens$csrftoken
    
    message(paste("Het editToken:",editToken))
    tijdelijk <<- editToken
    
    lengte <- nrow(wBegrippen)
    
    doorgaan <- TRUE
    if (nrow(wBegrippen) >= 100) {
      lFactor <- trunc(nrow(wBegrippen) / 100)
      lProgress <- 0.01
    }
    else {
      lFactor <- 1
      lProgress <- trunc(100 / nrow(wBegrippen))
    }
    withProgress(message = "wBegrippen verwerken", value = 0, {
      for (j in 1:lengte)
      {
        if (j %% lFactor == 0) incProgress(lProgress)
        if (doorgaan && substring(wBegrippen[j,"Controle meldingen"],1,2) == "OK") { #message(paste("aantal wBegrippen: ",lengte))
          velden <- NULL

          for (g in 1:length(metadataBegrippen)) {
            message(paste("Te controleren kenmerk op toegestaan:",metadataBegrippen[g]))
            if (wBegrippen[j,metadataBegrippen[g]] != "")
            {
              # 2022-02-21 Nodig omdat kenmerk niet in spreadsheet hoeft voor te komen!
              if (!(nrow(beheertoolControlesBegrippen[which(beheertoolControlesBegrippen$Kenmerk == metadataBegrippen[g]),] > 0) &&
                   beheertoolControlesBegrippen[which(beheertoolControlesBegrippen$Kenmerk == metadataBegrippen[g]),]$Verplicht == "V")
              ) {
                lCheckCollectie <- beheertoolControlesBegrippen[which(beheertoolControlesBegrippen$Kenmerk == metadataBegrippen[g]),]
                if (nrow(lCheckCollectie) > 0 && lCheckCollectie[1,]$Datatype == "Collectie") {
                  lCollectie <- NULL
                  lVerwijzingen <- strsplit(wBegrippen[j,metadataBegrippen[g]],",")
                  for (k in 1:length(lVerwijzingen[[1]])) {
                    lVerwijzing <- lVerwijzingen[[1]][k]
                    message(paste("Dit is m",lVerwijzing))
                    lCollGuid <- ophalenCollectieGuid(lVerwijzing,input$Omgeving,opmaakJson)
                    lCollectie <- paste(lCollectie,lCollGuid,sep = ",")
                  }
                  lCollectie <- gsub("^,","",lCollectie) # remove leading comma
                  velden[metadataBegrippen[g]] <- paste("|",metadataBegrippen[g],"=",lCollectie,"\n",sep='')
                }
                else {
                  velden[metadataBegrippen[g]] <- paste("|",metadataBegrippen[g],"=",wBegrippen[j,metadataBegrippen[g]],"\n",sep='')
                }
              }
            }
          }
          #message(paste("Guid voor toekennen:"),wBegrippen[j,"Guid"])
          if (nchar(toString(wBegrippen[j,"Guid"])) < 10) {
            wBegrippen[j,"Guid"] <<- paste("Id-",uuid::UUIDgenerate(),sep="")
          }
          
          if (nchar(toString(wBegrippen[j,"Guid"])) > 10) {
            wikitekst[j] <- paste("{{#element:\n",
                                  "|Elementtype=Begrip","\n",
                                  #"|Paginanaam=",wBegrippen[j,"Guid"],"\n",
                                  paste(gsub("=NA\n","=\n",velden),collapse=''),
                                  "}}",sep = '')

            editParams=list('title'=toString(wBegrippen[j,"Guid"]),'text'=wikitekst[j],'token'=editToken,'format'='json')
            
            message("title----------------------------------------")
            message(editParams$title)
            message("text----------------------------------------")
            message(editParams$text)
            message("token----------------------------------------")
            message(editParams$token)
            message("format----------------------------------------")
            message(editParams$format)
            
            editResponse <- content( POST(sprintf("%s?action=edit&format=json", baseUrl) , body=editParams, curl=curl) )
            editStatus <- editResponse$edit$result
            message("Het resultaat ")
            if (nchar(toString(editStatus)) == 0 || toString(editStatus) != "Success") {
              doorgaan <- FALSE
              showModal(modalDialog(title = paste("Fout in rij ",j," met Id:",wBegrippen[j,"Id"]), footer = modalButton("OK")))
            }
            message("Verwerking:",editStatus)
            Sys.sleep(0.1)
          }
        }
        else message(paste(wBegrippen[j,"Label (nl)"]," niet verwerken"))
      } #for
    })
    
    message("voor de shinyalert")
    Sys.sleep(0.5)
    
    opnieuwTonenWijzigingen(wBegrippen,"Begrippen")
    
    showModal(modalDialog(title = "Klaar", footer = modalButton("OK")))
    
  })

  output$DownloadCSVBegrippen <- downloadHandler(
    filename = paste("Begrippen",input$WijzigingsnummerBegrippen,".csv",sep = ""),
    content = function(file) {
      req(input$botUserName,input$botPassword,input$WijzigingsnummerBegrippen)
      #directory <- setwd(tempdir())
      # Ophalen begrippen met functie uit Hulpfuncties.r
      begrippenCSV <- ophalenBegrippen(pWijzigingsnummer = input$WijzigingsnummerBegrippen
                                      ,pMaxRijen         = 50000
                                      ,pOmgeving         = input$Omgeving
                                      ,pGuidOphalen      = input$guidBegrippen
                                      ,pOpmaakJson       = opmaakJson
                                      ,pUitBeheertool    = TRUE
                                      ,pSmaller          = FALSE)
      # 2022-09-27: Voorkomen openen nieuw tabblad als er geen begrippen zijn gevonden
      if (nrow(begrippenCSV) > 0) {
        for (i in 1:nrow(begrippenCSV)) {
          begrippenCSV[i,"Categorie van"] <- ophalenCollectieLabel(begrippenCSV[i,"Categorie van"],input$Omgeving,opmaakJson)
        }
      }
      write.table(begrippenCSV,file,sep = input$sepCSV, na = "", row.names = F, col.names = T, fileEncoding = defaultEncoding)
    }
  )
  
  output$Helplink <- renderText({
    paste(a("Shiny app help staat in de Wiki",href=paste("https://",input$Omgeving,".aquo.nl/index.php/ShinyBeheerHelptekst",sep = ""), target="_blank"))
  })
  
}
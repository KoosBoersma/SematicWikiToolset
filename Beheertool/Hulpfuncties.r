# Versie   : "$Revision: 802 $"
# Datum    : "$Date: 2022-12-06 14:09:19 +0100 (di, 06 dec 2022) $"
# Auteur   : "$Author: mwelling $"

# Opgenomen functies:
# - vulbeperking                Voor aanmaken http call
# - maakTekstURL                Voor aanmaken http call
# - zoekString                  Zoekstring geschikt maken voor http call
# - bepaalMetadata              Bepalen metadata domeintabel
# - bepaalMetadataBegrip        Bepalen metadata begrip
# - ophalenLijstDomeintabellen
# - bewerkDatum                 Datumformaat geschikt maken
# - ophalenDomeintabel          Ophalen gegevens van een specifieke domeintabel
# - wijzigDomeinwaardeInWiki    Aanpassen van een domeinwaarde in de Wiki
# - ophalenBegrip               Ophalen van een begrip o.b.v. de guid
# - ophalenBegrippen            Ophalen van een lijst van begrippen o.b.v. een wijzigingsnummer of guid
# - controleerObjectUniek       Controle of  een specifiek elementtype (o.b.v. een kenmerk) vaker voorkomt in de tijd
# - ophalenCollectieGuid        Ophalen van de guid van een collectie o.b.v. het label (nl)
# - ophalenCollectieLabel       Ophalen van de guids van collecties o.b.v. een lijst van label (nl)
# - wijzigBegrippenInWiki       Wijzigen van een lijst van begrippen in de Wiki
# - verwijderBegrippenUitWiki   Verwijderen van een lijst van begrippen uit de Wiki
# - bepaalMetadataWNS           Ophalen van de metadata van de domeintabel waarnemingssoort
# - ophalenWaarnemingssoort     Ophalen van een waarnemingssoort o.b.v. de code
# - 
# - 
# - 
#  

# Eigenschap:Subotype
#
# speciaal:vragen om toegestane relaties van Begrip op te halen
# [[Subotype::ElementtypeRelation]][[-Has subobject::Begrip]]
#
# ?Elementrelatie
# ?Eindpunten
#
#
# speciaal:vragen om toegestane eigenschappen van Begrip op te halen
# [[Elementtypecategorie::categorie:Begrippen]]
#
# ?Elementtype-eigenschappen


# Functie gebruikt bij het vullen van de Json string voor de speciaal:vragen aanroep
vulbeperking <- function(beperking,object,conditie){
  returnstring <- paste(beperking,"-5B-5B",object,conditie,"-5D-5D",sep="")
  return (returnstring)
}

# Functie voor het samenvoegen van de onderdelen van de Json string voor de speciaal:vragen aanroep
maakTekstURL <- function(tekstURL,categorie,beperking,kenmerken,opmaak){
  returnstring <- paste(tekstURL,categorie,beperking,kenmerken,opmaak,sep="")
  return (returnstring)
}

# Functie die een zoekstring geschikt maakt voor een http call
# Returnwaarde is character string met de verbeterde waarde
# Parameters
#   pString               De te verbeteren string
zoekString <- function(pString,pAddWildcard=TRUE) {
  lString <- gsub("\\-","-2D",pString)
  lString <- gsub("\\*","-2A",lString)
  lString <- gsub("\\/","-2F",lString)
  lString <- gsub("\\(","-28",lString)
  lString <- gsub("\\)","-29",lString)
  lString <- gsub("\\[","-5B",lString)
  lString <- gsub("\\]","-5D",lString)
  lString <- gsub("\\{","-7B",lString)
  lString <- gsub("\\}","-7D",lString)
  lString <- gsub("\\|","-7C",lString)
  lString <- gsub("\n","-0A",lString)
  lString <- gsub("\r","-0D",lString)
  if (pAddWildcard) lString <- paste("~*",lString,"*",sep = "")
  
  message(lString)
  return(lString)
}

# Functie die de kenmerken van de metadata van een domeintabel bepaalt
# Returnwaarde is character vector met de kenmerken
# Parameters
#   pDomeintabel          Naam van de domeintabel
#   pDomainsJson          Lijst van de domeintabellen (opgehaald met functie ophalenLijstDomeintabellen)
#                         List format
bepaalMetadata <- function(pDomeintabel,pDomainsJson,pWijzigingsdatum=FALSE) {
  lMetadata <- NULL
  for (m in 1:length(pDomainsJson)) {
    if (pDomainsJson[[m]]$printouts$Voorkeurslabel == pDomeintabel) {
      for (i in 1:length(pDomainsJson[[m]]$printouts$Metadata) ) {
        lMetadata[i] <- pDomainsJson[[m]]$printouts$Metadata[i]
      }
    }
  }
  # 7 april 2021 niet doen voor Biotaxon
  if (!pDomeintabel == "Biotaxon") {
    if (!("Status" %in% lMetadata)) lMetadata[length(lMetadata)+1] <- "Status"
    if (!("Wijzigingsnummer" %in% lMetadata)) lMetadata[length(lMetadata)+1] <- "Wijzigingsnummer"
    if (pWijzigingsdatum)
      if (!("Wijzigingsdatum" %in% lMetadata)) lMetadata[length(lMetadata)+1] <- "Wijzigingsdatum"
#    if (!("Modification date" %in% lMetadata)) lMetadata[length(lMetadata)+1] <- "Modification date"
  }
  message(paste("Lijst domeintabellen:","Bepalen metadata gekozen domeintabel",pDomeintabel,length(lMetadata),sep=" "))
  return(lMetadata)
}

# Functie die de kenmerken van de metadata van het concept Begrip bepaalt
# Returnwaarde is character vector met de kenmerken
# Parameters
#   pOmgeving             Omgeving (test/acceptatie/www evt. met redactie
#   pOpmaakJson           String met opmaak voor de Json speciaal:vragen aanroep
bepaalMetadataBegrip <- function(pOmgeving,pOpmaakJson,pSmaller = FALSE) {
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
#  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/api.php",sep="")
  
  curl <- getCurlHandle()

  categorie <- "?title=Speciaal:Vragen&x=-5B-5BElementtypecategorie%3A%3Acategorie%3ABegrippen-5D-5D"
#  categorie <- "?action=ask%26query=-5B-5BElementtypecategorie%3A%3Acategorie%3ABegrippen-5D-5D"
  beperking <- NULL
  kenmerken <- "%2F-3FElementtype-2Deigenschappen"
  lOpmaakJson <- str_replace(pOpmaakJson,"lLimit","500")
  lOpmaakJson <- str_replace(lOpmaakJson,"lOffset","0")
  json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
  #message(json_file)
  req <- httr::GET(json_file, curl=curl)
  if ((req$status_code == 200))
    lMetadata <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))$results$Begrip$printouts$`Elementtype-eigenschappen`
  # Onderstaande kenmerken zijn geen onderdeel van de metadata maar moeten wel worden opgehaald
  lMetadata <- lMetadata[!lMetadata == "Import"]
  lMetadata[length(lMetadata)+1] <- "Breder"
  lMetadata[length(lMetadata)+1] <- "Categorie van"
  lMetadata[length(lMetadata)+1] <- "Gerelateerd"
  lMetadata[length(lMetadata)+1] <- "Heeft onderdeel"
  lMetadata[length(lMetadata)+1] <- "Gerepresenteerd door"
  lMetadata[length(lMetadata)+1] <- "Gebruik voor"
  lMetadata[length(lMetadata)+1] <- "Is onderdeel van"
  if (pSmaller) lMetadata[length(lMetadata)+1] <- "Smaller"
  message(paste("Bepalen metadata begrippen, aantal:",length(lMetadata),sep=" "))
  if (!("Status" %in% lMetadata)) lMetadata[length(lMetadata)+1] <- "Status"
  if (!("Wijzigingsnummer" %in% lMetadata)) lMetadata[length(lMetadata)+1] <- "Wijzigingsnummer"
  if (!("Guid" %in% lMetadata)) lMetadata[length(lMetadata)+1] <- "Guid"
  return(lMetadata)
}

# Functie die de lijst met aanwezige domeintabellen ophaalt
# Returnwaarde is een lijst met daarin de volgende onderdelen:
#   - loginStatus                       "Ingelogd" of "Inloggen mislukt"
#   - domainsJson                       Lijst met de domeintabellen in list format
#   - alleDomeinTabellen                Vector met de namen van alle domeintabellen van een van de volgende 
#                                       categorieën:
#                                         Domeintabel
#                                         Domeintabelverzamellijst
#   - alleDomeinGuid                    Vector met Guid van alle domeintabellen in alleDomeinTabellen
#   - alleDomeinElementtype             Elementtype van de domeinwaarde behorend bij de domeintabellen in 
#                                       alleDomeinTabellen
#   - domeinTabellen                    Idem aan alleDomeinTabellen maar dan voor categorieën:
#                                         Domeintabel
#   - domeinGuid                        Elementtype van de domeinwaarde behorend bij de domeintabellen in
#                                       domeinTabellen
#   - domeinElementtype                 Idem aan alleDomeinElementtype voor domeinTabellen
#   - domeinTabellenVerzamellijst       Vector met de namen van alle domeintabellen van de categorie
#                                       Domeintabelverzamellijst
#   - domeinGuidVerzamellijst           Elementtype van de domeinwaarde behorend bij de domeintabellen in
#                                       Domeintabelverzamellijst
#
# Parameters
#   pOmgeving                           Omgeving (test/acceptatie/www evt. met redactie
#   pUserName                           Gebruikersnaam voor de wiki omgeving
#   pPassword                           Bijbehorend wachtwoord
#   pHistorieOpvragen                   Ook hiostorische domeintabellen opvragen? TRUE/FALSE
#   pOpmaakJson                         String met opmaak voor de Json speciaal:vragen aanroep
#
ophalenLijstDomeintabellen <- function (pOmgeving
                                        ,pUserName
                                        ,pPassword
                                        ,pHistorieOpvragen
                                        ,pOpmaakJson
                                        ) 
{
  loginStatus <- NULL
  domainsJson <- NULL
  alleDomeinTabellen <- NULL
  alleDomeinGuid <- NULL
  alleDomeinElementtype <- NULL
  domeinTabellen <- NULL
  domeinGuid <- NULL
  domeinElementtype <- NULL
  domeinTabellenVerzamellijst <- NULL
  domeinGuidVerzamellijst <- NULL

  baseUrl  <- paste("https://",pOmgeving,".aquo.nl/api.php",sep="")
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  
  curl <- getCurlHandle()
  ## Retrieve the login token
  botToken <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", baseUrl), curl=curl ) )$query$tokens$logintoken
  loginParams=list('lgtoken'=botToken,'lgname'=pUserName,'lgpassword'=pPassword)
  loginResponse <- content( POST(sprintf("%s?action=login&format=json", baseUrl) , body=loginParams, curl=curl) )$login$result
  if (length(loginResponse) > 0 && loginResponse == "Success") {
    editToken <- content( GET(sprintf("%s?action=query&meta=tokens&type=csrf&format=json", baseUrl), curl=curl ))$query$tokens$csrftoken
    #message(editToken)
    loginStatus <- "Ingelogd"
    
    categorie <- "?title=Speciaal:Vragen&x=-5B-5BElementtype%3A%3ADomeintabel%20%7C%7C%20Domeintabelverzamellijst-5D-5D-20"
    if (!pHistorieOpvragen)
      beperking <- paste("-5B-5BEind geldigheid::>>",gsub("-","-2D",toString(Sys.Date()-1)),"-5D-5D",sep="")
    else 
      beperking <- NULL
    kenmerken <- "%2F-3FElementtype%2F-3FId%2F-3FVoorkeurslabel%2F-3FOmschrijving%2F-3FBegin-20geldigheid%2F-3FEind-20geldigheid%2F-3FMetadata"
    lOpmaakJson <- paste(str_replace(pOpmaakJson,"lLimit","500"),"&sort=Voorkeurslabel&order=asc",sep = "")
    json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
    #message(json_file)
    req <- httr::GET(json_file, curl=curl)
    #message(str(req))
    if ((req$status_code == 200))
      domainsJson <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))$results
    
    for (di in 1:length(domainsJson)) {
      if (domainsJson[[di]]$printouts$Elementtype$fulltext %in% c("Domeintabel","Domeintabelverzamellijst")) {
        alleDomeinTabellen[di] <- domainsJson[[di]]$printouts$Voorkeurslabel
        alleDomeinGuid[alleDomeinTabellen[di]] <- domainsJson[[di]]$fulltext
        
        # 2022-02-17: Parameter en Waardebepalingsmethode keihard naar Domeintabel voor juiste elementtype Domeinwaarde
        if (domainsJson[[di]]$printouts$Voorkeurslabel %in% c("Biotaxon","Parameter","Waardebepalingsmethode")) {
          alleDomeinElementtype[alleDomeinTabellen[di]] <- "Domeintabel"
        }
        else {
          alleDomeinElementtype[alleDomeinTabellen[di]] <- domainsJson[[di]]$printouts$Elementtype$fulltext
        }
        
        #message(paste(alleDomeinTabellen[di],alleDomeinElementtype[alleDomeinTabellen[di]],sep = " "))
      }
      if (domainsJson[[di]]$printouts$Elementtype$fulltext %in% c("Domeintabel") ||
          # 2022-02-17: tijdelijk domeintabellen Parameter en Waardebepalingsmethode hard meenemen in de lijst
          domainsJson[[di]]$printouts$Voorkeurslabel %in% c("Biotaxon","Parameter","Waardebepalingsmethode")
      ) {
        domeinTabellen[di] <- domainsJson[[di]]$printouts$Voorkeurslabel
        domeinGuid[domeinTabellen[di]] <- domainsJson[[di]]$fulltext
        if (domeinTabellen[di] %in% c("Biotaxon","Parameter","Waardebepalingsmethode")) {
          domeinElementtype[domeinTabellen[di]] <- "Domeintabel"
          message(paste(domeinTabellen[di],domeinElementtype[domeinTabellen[di]],sep = " "))
        }
        else domeinElementtype[domeinTabellen[di]] <- domainsJson[[di]]$printouts$Elementtype$fulltext
        
      }
      else {
#      if (domainsJson[[di]]$printouts$Elementtype$fulltext %in% c("Domeintabelverzamellijst")) {
        domeinTabellenVerzamellijst[di] <- domainsJson[[di]]$printouts$Voorkeurslabel
        domeinGuidVerzamellijst[domeinTabellenVerzamellijst[di]] <- domainsJson[[di]]$fulltext
        
        #message(paste(domeinTabellenVerzamellijst[di],sep = " "))
      }
    }
    alleDomeinTabellen <- alleDomeinTabellen[!is.na(alleDomeinTabellen)]
    alleDomeinGuid     <- alleDomeinGuid[!is.na(alleDomeinGuid)]
    
    domeinTabellen    <- domeinTabellen[!is.na(domeinTabellen)]
    domeinGuid        <- domeinGuid[!is.na(domeinGuid)]
    
    domeinTabellenVerzamellijst    <- domeinTabellenVerzamellijst[!is.na(domeinTabellenVerzamellijst)]
    domeinGuidVerzamellijst        <- domeinGuidVerzamellijst[!is.na(domeinGuidVerzamellijst)]
    
    domeinTabellenVerzamellijst <- c('Biotaxon','Parameter','Waardebepalingsmethode','Waarnemingssoort')
  }
  else {
    loginStatus <- "Inloggen mislukt"
  }
  lReturn <- list(loginStatus
                  ,domainsJson
                  ,alleDomeinTabellen
                  ,alleDomeinGuid
                  ,alleDomeinElementtype
                  ,domeinTabellen
                  ,domeinGuid
                  ,domeinElementtype
                  ,domeinTabellenVerzamellijst
                  ,domeinGuidVerzamellijst
                  )
  return(lReturn)
}

# format(as.Date(substr('1/2011/8/30/0/0/0/0',3,100)),"%d %B %Y %H:%M:%S")

bewerkDatum <- function(pDatum,pTaal="NL-EN") {
  lDatum <- toString(pDatum)
  #message(paste("Te verwerken datum:",lDatum))
  
  # als het formaat 1/YYYY/MM/DD... is
  if (grepl("^1/[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}.*$",lDatum)) {
    #message(lDatum)
    lPositie <- str_locate(lDatum,"/")[1]+1
    #message(paste("Positie:",lPositie))
    lLength <- nchar(lDatum)
    lDatum <- substring(lDatum,lPositie,lLength)
    #message(lDatum)
    # Verwijderen tijdcomponent
    #lTijd <- if_else(is.na(str_locate(lDatum,"/0/0/0/0")[1]),0,str_locate(lDatum,"/0/0/0/0")[1])
    lTijd <- str_locate(lDatum,"/0/0/0/0")[1]
    if (is.na(lTijd)) lTijd <- 0
    if (lTijd > 0) {
      lDatum <- str_replace(lDatum,"/0/0/0/0","")
      lDatum <- toString(lubridate::parse_date_time(lDatum,orders="ymd"))
    }
    else {
      lTijd <- str_locate(lDatum,"/0")[1]
      if (is.na(lTijd)) lTijd <- 0
      if (lTijd > 0) {
        lDatum <- str_replace(lDatum,"/0","")
        lDatum <- toString(lubridate::parse_date_time(lDatum,orders="ymdHMS"))
      }
      else {
        lDatum <- toString(lubridate::parse_date_time(lDatum,orders="ymd"))
      }
    }
  }
  else {
    if (grepl("[[:alpha:]]", lDatum)) {
      if (pTaal == "NL-EN") {
        if (!grepl("Dutch",Sys.getlocale(category = "LC_ALL"),fixed = TRUE)) {
          lDatum <- gsub("januari","january",lDatum)
          lDatum <- gsub("februari","february",lDatum)
          lDatum <- gsub("maart","march",lDatum)
          lDatum <- gsub("mei","may",lDatum)
          lDatum <- gsub("juni","june",lDatum)
          lDatum <- gsub("juli","july",lDatum)
          lDatum <- gsub("augustus","august",lDatum)
          lDatum <- gsub("oktober","october",lDatum)
        }
      }
      else {
        lDatum <- gsub("january","januari",lDatum)
        lDatum <- gsub("february","februari",lDatum)
        lDatum <- gsub("march","maart",lDatum)
        lDatum <- gsub("may","mei",lDatum)
        lDatum <- gsub("june","juni",lDatum)
        lDatum <- gsub("july","juli",lDatum)
        lDatum <- gsub("august","augustus",lDatum)
        lDatum <- gsub("october","oktober",lDatum)
      }
      lDatum <- str_replace(lDatum,"00:00:00","")
      if (is.na(lubridate::parse_date_time(lDatum,orders="dBy"))) lDatum <- toString(lubridate::parse_date_time(lDatum,orders="yBd"))
      else lDatum <- toString(lubridate::parse_date_time(lDatum,orders="dBy"))
    }
    else {
      lDate <- lubridate::parse_date_time(lDatum,orders="ymd")
      if (is.na(lDate)) lDate <- ""
      lDatum <- toString(lDate)
    }
  }
  #    message(paste("  Verwerkte datum:",lDatum))
  #return (str_replace_all(lDatum,"-","/"))
  return (lDatum)
}

# Functie die alle waarden van een domeintabel ophaalt
# Returnwaarde is een dataframe met alle kenmerken die in de metadata van de domeintabel
#
# Parameters
#   pDomeintabel                        Naam van de op te halen domeintabel
#   pMaxRijen                           Maximum aantal op te halen domeinwaarden
#   pId=NULL                            Indien gevuld alleen domeinwaarde met dat Id ophalen          
#   pTekstUrl=NULL                      Json string om speciaal:Vragen mee uit te voeren
#   pCategorie=NULL                     Categorie van de domeintabel
#   pDomainsJson                        List met alle domeintabellen (excl. waarden)
#   pSepCSV                             Separator te gebruiken voor inlezen CSV
#   pHistorieOpvragen                   Ook hiostorische domeintabellen opvragen? TRUE/FALSE
#   pUitBeheertool=FALSE                Op TRUE zetten als aangeroepen uit Shiny app
#   pWijzingsdatum=FALSE                Op TRUE zetten als je ook de wijzigingsdatum in metadata wilt
#
ophalenDomeintabel <- function(pDomeintabel
                               ,pMaxRijen
                               ,pId=NULL
                               ,pTekstUrl=NULL
                               ,pCategorie=NULL
                               ,pDomainsJson
                               ,pSepCSV
                               ,pHistorieOpvragen
                               ,pUitBeheertool=FALSE
                               ,pWijzingsdatum=FALSE
                               ) {
  #-----------------------------------------
  # Ophalen van de inhoud van de domeintabel
  #-----------------------------------------
  library(shiny)

  #message("1 - Start ophalenDomeintabel")
  #print(environment())
  #print(parent.env(environment()))
  
  ophalenWaarden <- function() {
    #message("start ophalenWaarden")
    #print(environment())
    #print(parent.env(environment()))
    domValuesDFloc <- data.frame(matrix(ncol = length(lMetadata)+1, nrow = 0))
    colnames(domValuesDFloc) <- columnNames
    json_file <- maakTekstURL(lTekstUrl,categorie,beperking,kenmerken
                              ,paste(str_replace(lOpmaakJson,"lOffset",toString(lOffset)),"&sort=Id,Begin%20geldigheid,Eind%20geldigheid&order=asc,asc,asc",sep = ""))
    curlLoc <- getCurlHandle()
    #message(json_file)
    req <- httr::GET(json_file, curl=curlLoc)
    # 2022-09-27: door lengte content te checken voorkomen foutmelding
    jsonContent <- httr::content(req, "text", encoding="UTF-8")
    if ((req$status_code != 200 || nchar(jsonContent) == 0)) gevonden <<- FALSE
												
						 
	 
    else {
      tryCatch(
        {
          domValuesJson <- jsonlite::fromJSON(jsonContent)$results
          #message(length(domValuesJson))
          message(paste(toString(Sys.time()),"Aantal waarden in de Wiki:",length(domValuesJson)+lOffset, sep = " "))
        },
        warning = function(w){
          gevonden <<- FALSE
							 
        },
        error = function(e){
          gevonden <<- FALSE
							 
          #},
          # finally={
          #   message("Geen domeinwaarden opgehaald")
        }
      )
    }
					   
    if (gevonden) {
      #message(paste("Aantal opgehaalde waarden:",length(domValuesJson)))
      for (i in 1:length(domValuesJson)) {
        domValuesDFloc[i,"Guid"] <- domValuesJson[[i]]$fulltext
        lColumns <- colnames(domValuesDFloc)
        lColumns <- lColumns[!lColumns %in% c("Guid")]
        for (x in lColumns) {
          if (length(unlist(domValuesJson[[i]]$printouts[x]) > 0 && is.na(unlist(domValuesJson[[i]]$printouts[x])))) {
            if (x == "Begin geldigheid" || x == "Eind geldigheid" || x == "Wijzigingsdatum") {
              domValuesDFloc[i,x] <- bewerkDatum((unlist(domValuesJson[[i]]$printouts[x][[1]]$raw)))
            }
            else {
              # 2022-09-27: Trello kaartje https://trello.com/c/5lkNQhr4/427-export-vanuit-shiny-van-domeintabel-waterketenfunctienaam-geeft-bij-de-kolom-symbool-driedubbele-informatie
              # Symbool ook uit elkaar halen
              relaties <- c("Gerelateerd","Symbool")
              if (x %in% relaties) {
                for (x in relaties) {
                  if (length(unlist(domValuesJson[[i]]$printouts[x][[1]]$fulltext)) > 0) {
                    relatie <- NULL
                    for (k in 1:length(unlist(domValuesJson[[i]]$printouts[x][[1]]$fulltext))) {
                      #message("er is lengte")
                      if (k == 1) {
                        relatie <- unlist(domValuesJson[[i]]$printouts[x][[1]]$fulltext[1])
                      }
                      else {
                        #message("lengte > 1")
                        relatie <- paste(relatie,unlist(domValuesJson[[i]]$printouts[x][[1]]$fulltext[k]),sep=",")
                      }
                    }
                    #message(relatie)
                    domValuesDFloc[i,x] <- relatie
                  }
                }
              }
              else {
                domValuesDFloc[i,x] <- toString(unlist(domValuesJson[[i]]$printouts[x]))
              }
            }
          }
        }
      }
      lOffset <<- lOffset + lLimit
      if (!(length(domValuesJson) == lLimit && lOffset+lLimit < pMaxRijen)) lDoorgaan <<- FALSE
    }
    else {
      lDoorgaan <<- FALSE
      message("Domeinwaarden bestaat niet")
    }
    return(domValuesDFloc)
  }
  environment(ophalenWaarden) <- environment()
  
  message(paste("ophalen gegevens voor:",pDomeintabel))
  #if (!is.null(pCurl)) curl <- getCurlHandle() else curl <- pCurl
  if (!is.null(pTekstUrl))
    lTekstUrl <- pTekstUrl
  else
    lTekstUrl <- pTekstUrl
  
  #Biotxon ophalen uit CSV, behalve als pCategorie gevuld, dan uit Wiki om nieuwe CSV te kunnen schrijven
  if (pDomeintabel == "Biotaxon" && is.null(pCategorie)) {
    URL <- paste(lTekstUrl,"/Speciaal:Doorverwijzen/file/Biotaxon.csv",sep = "")
    message("URL voor ophalen csv voor Biotaxon",URL)
    GET(URL, write_disk(tf <- tempfile(fileext = ".csv")))
    domValuesDF <- read.csv(tf,sep = pSepCSV, encoding = defaultEncoding)
    colnames(domValuesDF) <- gsub("\\."," ",colnames(domValuesDF))
    if (!is.null(pId)) domValuesDF <- domValuesDF[which(domValuesDF$Id == pId),]
  }
  else {
    if (length(pCategorie) == 0) {
      categorie <- paste("?title=Speciaal:Vragen&x=-5B-5BCategorie%3A",
                         domeinwaardeCategorie[alleDomeinElementtype[pDomeintabel]],"-5D-5D-20",sep = "")
    }
    else {
      categorie <- paste("?title=Speciaal:Vragen&x=-5B-5BCategorie%3A",pCategorie,"-5D-5D-20",sep = "")
    }
    beperking <- paste("-5B-5BBreder::",gsub("-","-2D",alleDomeinGuid[pDomeintabel]),"-5D-5D",sep="")
    if (!is.null(pId))
      beperking <- paste(beperking,"-5B-5BId::",pId,"-5D-5D",sep="")
    if (!pHistorieOpvragen)
      beperking <- paste(beperking,"-5B-5BEind geldigheid::>>",gsub("-","-2D",toString(Sys.Date()-1)),"-5D-5D",sep="")
    lMetadata <- bepaalMetadata(pDomeintabel,pDomainsJson,pWijzingsdatum)
    kenmerken <- NULL
    for (i in 1:length(lMetadata)) kenmerken <- paste(kenmerken,"%2F-3F",lMetadata[i],sep="")
    columnNames <- list()
    for (i in 1:length(lMetadata)) columnNames[[i]] <- lMetadata[i]
    columnNames[[length(columnNames)+1]] <- "Guid"
    domValuesDF <- data.frame(matrix(ncol = length(lMetadata)+1, nrow = 0))
    colnames(domValuesDF) <- columnNames
    lOpmaakJson <- str_replace(opmaakJson,"lLimit",toString(lLimit))
    
    lOffset <- lStartPage
    lDoorgaan <- TRUE
    gevonden <- TRUE
    
    #----------------------------------------------------------------
    # Ophalen van domeinwaarden
    #----------------------------------------------------------------
    
    if (pUitBeheertool) {
      lProgress <- lLimit / pMaxRijen
      message(lProgress)
      withProgress(message = "Domeinwaarden ophalen", value = 0, {
        while (lDoorgaan) {
          incProgress(lProgress)
          domValuesDFlokaal <- ophalenWaarden()
          domValuesDF <- rbind(domValuesDF,domValuesDFlokaal)
        }
      })
      
    }
    else {
      message("Niet via de beheertool")
      while (lDoorgaan) {
        domValuesDFlokaal <- ophalenWaarden()
        domValuesDF <- rbind(domValuesDF,domValuesDFlokaal)
        write.table(domValuesDFlokaal
                    , paste("C:/temp/Domeintabellen/WikiUpdate/WNS historische verwijzingen/"
                            ,paste("lCHM",lOffset-lLimit,sep = "-"),".csv",sep=""), sep = ";", na = "", row.names = F, col.names = T, fileEncoding = "UTF8")
      }
    }
  }
  domValuesDF$Id <- as.numeric(domValuesDF$Id)
  # Zorg dat alle NA zijn vervangen door ""
  domValuesDF[is.na(domValuesDF)] <- ""
  return(domValuesDF)
}

wijzigDomeinwaardeInWiki  <- function (pOmgeving
                                      ,pDomeintabel
                                      ,pMetadata
                                      ,pUserName
                                      ,pPassword
                                      ,pWijzigingen
                                      ,pMaxRijen
                                      ,pSepCSV
                                      ,pHistorieOpvragen
                                      ,pUitBeheertool=FALSE
) 
{
  baseUrl  <- paste("https://",pOmgeving,".aquo.nl/api.php",sep="")
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  
  wikitekst <- NULL
   
  curl <- getCurlHandle()
  ## Retrieve the login token
  botToken <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", baseUrl), curl=curl ) )$query$tokens$logintoken
  loginParams=list('lgtoken'=botToken,'lgname'=pUserName,'lgpassword'=pPassword)
  loginResponse <- content( POST(sprintf("%s?action=login&format=json", baseUrl) , body=loginParams, curl=curl) )$login$result
  editToken <- content( GET(sprintf("%s?action=query&meta=tokens&type=csrf&format=json", baseUrl), curl=curl ))$query$tokens$csrftoken
  
  message(paste("Het editToken:",editToken))
  
  lengte <- nrow(pWijzigingen)

  doorgaan <- TRUE
  velden <- NULL
  velden2 <- NULL

  if (nrow(pWijzigingen) >= 100) {
    lFactor <- trunc(nrow(pWijzigingen) / 100)
    lProgress <- 0.01
  }
  else {
    lFactor <- 1
    lProgress <- trunc(100 / nrow(pWijzigingen))
  }
#  withProgress(message = "Wijzigingen verwerken", value = 0, {
    for (j in 1:lengte)
    {
      if (j %% 100 == 0) message(paste(Sys.time(),j,"wijzigingen doorgevoerd",sep = " "))
#      if (j %% lFactor == 0) incProgress(lProgress)
      if (doorgaan && substring(pWijzigingen[j,"Controle meldingen"],1,2) == "OK") { #message(paste("aantal wijzigingen: ",lengte))
        for (g in 1:length(pMetadata)) {
          velden[pMetadata[g]] <- paste("|",pMetadata[g],"=",pWijzigingen[j,pMetadata[g]],"\n",sep='')
          velden2[pMetadata[g]] <- pWijzigingen[j,pMetadata[g]]
          #velden[g] <- paste("|",pMetadata[g],"=",pWijzigingen[j,pMetadata[g]],"\n",sep='')
        }
        #message(paste("Guid voor toekennen:"),pWijzigingen[j,"Guid"])
        if (nchar(toString(pWijzigingen[j,"Guid"])) < 10) {
          if (pOmgeving %in% c("test","acceptatie","www")) {
            if (pOmgeving %in% c("test","acceptatie")) {
              baseUrl2  <- str_replace(baseUrl,".aquo",".redactie.aquo")
              tekstUrl2 <- str_replace(tekstUrl,".aquo",".redactie.aquo")
            }
            else {
              baseUrl2  <- str_replace(baseUrl,"www","redactie")
              tekstUrl2 <- str_replace(tekstUrl,"www","redactie")
            }

            curl2 <- getCurlHandle()
            ## Retrieve the login token
            botToken2 <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", baseUrl2), curl=curl2 ) )$query$tokens$logintoken
            loginParams2=list('lgtoken'=botToken2,'lgname'=pUserName,'lgpassword'=pPassword)
            loginResponse2 <- content( POST(sprintf("%s?action=login&format=json", baseUrl2) , body=loginParams2, curl=curl2) )$login$result
            if (length(loginResponse) > 0 && loginResponse == "Success") {
              domValuesRedactie <- ophalenDomeintabel(pDomeintabel      = pDomeintabel
                                                      ,pMaxRijen         = pMaxRijen
                                                      ,pId               = velden2["Id"]
                                                      ,pTekstUrl         = tekstUrl2
                                                      ,pDomainsJson      = domainsJson
                                                      ,pSepCSV           = pSepCSV
                                                      ,pHistorieOpvragen = pHistorieOpvragen
                                                      ,pUitBeheertool    = pUitBeheertool
              )
              #ophalenDomeintabel(pDomeintabel,pMaxRijen,velden2["Id"],tekstUrl2)


              #@ Moet dit geen <<- zijn voor pWijzigingen ??
              if (nrow(domValuesRedactie) > 0)
                pWijzigingen[j,"Guid"] <- domValuesRedactie[which(domValuesRedactie$'Begin geldigheid' == velden2["Begin geldigheid"]),"Guid"]
              else
                pWijzigingen[j,"Guid"] <- "NA"
            }
            message(paste("Opgehaalde Guid:",pWijzigingen[j,"Guid"]))
          }
          else {
            pWijzigingen[j,"Guid"] <<- paste("Id-",uuid::UUIDgenerate(),sep="")
          }
        }

        # Afhandeling voorkeurslabel voor Biotaxon
        if (pDomeintabel == "Biotaxon")
          lVoorkeurslabel <- pWijzigingen[j,"Naam"]
        else
          lVoorkeurslabel <- pWijzigingen[j,"Omschrijving"]
        if (nchar(toString(pWijzigingen[j,"Guid"])) > 10) {
          wikitekst[j] <- paste("{{#element:\n",
                                paste("|Elementtype=",domeinwaardeElementType[alleDomeinElementtype[pDomeintabel]],"\n",sep = ""),
                                "|Paginanaam=",pWijzigingen[j,"Guid"],"\n",
                                "|Voorkeurslabel=",lVoorkeurslabel,"\n",
                                paste(gsub("=NA\n","=\n",velden),collapse=''),
                                "|GUID=",pWijzigingen[j,"Guid"],"\n",
                                "|Breder=",alleDomeinGuid[pDomeintabel],"\n",
                                "}}",sep = '')

          editParams=list('title'=toString(pWijzigingen[j,"Guid"]),'text'=wikitekst[j],'token'=editToken,'format'='json')

          # message("title----------------------------------------")
          # message(editParams$title)
          # message("text----------------------------------------")
          # message(editParams$text)
          # message("token----------------------------------------")
          # message(editParams$token)
          # message("format----------------------------------------")
          # message(editParams$format)

          editResponse <- content( POST(sprintf("%s?action=edit&format=json", baseUrl) , body=editParams, curl=curl) )
          editStatus <- editResponse$edit$result
          #message("Het resultaat ")
          if (nchar(toString(editStatus)) == 0 || toString(editStatus) != "Success") {
            doorgaan <- FALSE
            showModal(modalDialog(title = paste("Fout in rij ",j," met Id:",pWijzigingen[j,"Id"]), footer = modalButton("OK")))
          }
          message(paste("Verwerking:",pWijzigingen[j,"Guid"],editStatus))
          Sys.sleep(0.1)
        }
        else
          pWijzigingen[j,"Controle meldingen"] <<- "Domeinwaarde is NIET op de redactie-omgeving aanwezig"
      }
      else message(paste(pWijzigingen[j,"Omschrijving"]," niet verwerken"))
    } #for
#  })
  
}

ophalenBegrip <- function(pGuid
                          ,pOmgeving
                          ,pMetadata
                          ,pOpmaakJson
                          
) {
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  
  lReturn <- NULL
  curl <- getCurlHandle()
  categorie <- NULL
  beperking <- paste("?title=Speciaal:Vragen&x=-5B-5B",gsub("-","-2D",pGuid),"-5D-5D",sep = "")
  kenmerken <- "%2F-3FElementtype"
  for (i in 1:length(pMetadata)) kenmerken <- paste(kenmerken,"%2F-3F",pMetadata[i],sep="")
  lOpmaakJson <- str_replace(pOpmaakJson,"lLimit","1")
  lOpmaakJson <- str_replace(lOpmaakJson,"lOffset","0")
  json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
  #message(json_file)
  req <- httr::GET(json_file, curl=curl)
  if (req$status_code == 200 && length(req$content) > 0)  {
    lJson <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))$results
    l_elementtype <- lJson[[1]]$Elementtype$fulltext
    return(lJson)
  }
  return(lReturn)
}

ophalenBegrippen <- function(pWijzigingsnummer=NULL
                            ,pGuid=NULL
                            ,pMaxRijen
                            ,pOmgeving
                            ,pGuidOphalen
                            ,pOpmaakJson
                            ,pUitBeheertool=FALSE
                            ,pSmaller= FALSE
) {
  
  #-----------------------------------------------
  # Ophalen van begrippen obv een wijzigingsnummer
  #-----------------------------------------------
  library(shiny)
  
  ophalenWaardenBegrippen <- function() {
    begrippenDFloc <- data.frame(matrix(ncol = length(lMetadata), nrow = 0))
    colnames(begrippenDFloc) <- lMetadata
    json_file <- maakTekstURL(lTekstUrl,lcategorie,lbeperking,lkenmerken,str_replace(lOpmaakJson,"lOffset",toString(lOffset)))
    message(json_file)
    curlLoc <- getCurlHandle()
    req <- httr::GET(json_file, curl=curlLoc)
    # 2022-09-27: door lengte content te checken voorkomen foutmelding
    jsonContent <- httr::content(req, "text", encoding="UTF-8")
    if (req$status_code != 200 || nchar(jsonContent) == 0) {
      message(paste("Het gaat niet goed",lOffset,req$status_code))
      gevonden <<- FALSE
      lDoorgaan <<- FALSE
      lFout <- FALSE
    }
    else {
      tryCatch(
        {
          begrippenJson <- jsonlite::fromJSON(jsonContent)$results
          #message(length(begrippenJson))
          message(paste(toString(Sys.time()),"Aantal waarden in de Wiki:",length(begrippenJson)+lOffset, sep = " "))
          lFout <- FALSE
        },
        warning = function(w){
          gevonden <<- FALSE
          message(paste("Waarschuwing",w))
          lFout <- TRUE
        },
        error = function(e){
          gevonden <<- FALSE
          lFout <- TRUE
          message(paste("Fout",e))
          #},
          # finally={
          #   message("Geen domeinwaarden opgehaald")
        }
      )
      if (gevonden) {
        #message(paste("Aantal opgehaalde waarden:",length(begrippenJson)))
        for (i in 1:length(begrippenJson)) {
          if (!gevonden || begrippenJson[[i]]$printouts$Elementtype$fulltext != "Begrip") {
            lDoorgaan <<- FALSE
            gevonden <<- FALSE
            lFout <- TRUE
            message("Opgehaalde object is geen begrip")
          }
          else {
            if (pGuidOphalen) begrippenDFloc[i,"Guid"] <- begrippenJson[[i]]$fulltext
            lColumns <- colnames(begrippenDFloc)
            lColumns <- lColumns[!lColumns %in% c("Guid")]
            for (x in lColumns) {
              if (length(unlist(begrippenJson[[i]]$printouts[x]) > 0 && is.na(unlist(begrippenJson[[i]]$printouts[x])))) {
                if (x == "Datum start" || x == "Datum eind" || x == "Datum gewijzigd" || x == "Begin geldigheid" || x == "Eind geldigheid") {
                  begrippenDFloc[i,x] <- bewerkDatum((unlist(begrippenJson[[i]]$printouts[x][[1]]$raw)))
                }
                else {
                  if (x %in% c("Breder","Categorie van","Gerelateerd","Heeft onderdeel","Gerepresenteerd door","Gebruik voor","Smaller")) {
                    if (length(unlist(begrippenJson[[i]]$printouts[x][[1]]$fulltext)) > 0) {
                      gerelateerd <- NULL
                      for (k in 1:length(unlist(begrippenJson[[i]]$printouts[x][[1]]$fulltext))) {
                        #message("er is lengte")
                        if (k == 1) {
                          gerelateerd <- unlist(begrippenJson[[i]]$printouts[x][[1]]$fulltext[1])
                        }
                        else {
                          #message("lengte > 1")
                          gerelateerd <- paste(gerelateerd,unlist(begrippenJson[[i]]$printouts[x][[1]]$fulltext[k]),sep=",")
                        }
                      }
                      #message(gerelateerd)
                      begrippenDFloc[i,x] <- gerelateerd
                    }
                  }
                  else {
                    begrippenDFloc[i,x] <- toString(unlist(begrippenJson[[i]]$printouts[x]))
                  }
                }
              }
            }
          }
          lOffset <<- lOffset + lLimit
          if (!(length(begrippenJson) == lLimit && lOffset+lLimit <= pMaxRijen)) lDoorgaan <<- FALSE
        }
      }
      else {
        lDoorgaan <<- FALSE
        lFout <- TRUE
        message("Begrippen bestaan niet")
      }
    }
    
    # Als er iets is foutgegaan dan Df met één rij teruggeven met onwerkelijke datum, wordt hogerop afgevangen
    if (lFout) {
      begrippenDFloc <- data.frame(matrix(ncol = length(lMetadata), nrow = 0))
      colnames(begrippenDFloc) <- lMetadata
      begrippenDFloc[1,"Begin geldigheid"] <- "Geen begrip"
    }
    message("Einde ophalenWaardenBegrippen")
    return(begrippenDFloc)
  }
  environment(ophalenWaardenBegrippen) <- environment()
  
  message(paste("ophalen gegevens voor wijziging/Guid:",pWijzigingsnummer,pGuid))

  lTekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  if (!is.null(pWijzigingsnummer)) {
    lcategorie <- "?title=Speciaal:Vragen&x=-5B-5BCategorie%3ABegrippen-5D-5D"
    lbeperking <- paste("-5B-5BWijzigingsnummer%3A%3A",gsub("-","-2D",pWijzigingsnummer),"-5D-5D",sep = "")
  }
  else {
    if (!is.null(pGuid)) {
      lcategorie <- NULL
      lbeperking <- paste("?title=Speciaal:Vragen&x=-5B-5B",gsub("-","-2D",pGuid),"-5D-5D",sep = "")
    }
    else {
      lcategorie <- "?title=Speciaal:Vragen&x=-5B-5BCategorie%3ABegrippen-5D-5D"
      lbeperking <- NULL
    }
  }
  lMetadata <- bepaalMetadataBegrip(pOmgeving,pOpmaakJson,pSmaller)
  if (is.na(match("Guid",lMetadata))) {
    lMetadata[length(lMetadata)+1] <- "Guid"
  }
    #message(lMetadata)
  lkenmerken <- NULL
  for (i in 1:length(lMetadata)) lkenmerken <- paste(lkenmerken,"%2F-3F",lMetadata[i],sep="")
  lkenmerken <- paste(lkenmerken,"%2F-3F","Elementtype",sep="")
  begrippenDF <- data.frame(matrix(ncol = length(lMetadata), nrow = 0))
  colnames(begrippenDF) <- lMetadata

  lOpmaakJson <- str_replace(pOpmaakJson,"lLimit",toString(min(c(lLimit,pMaxRijen))))

  lOffset <- lStartPage
  lDoorgaan <- TRUE
  gevonden <- TRUE
  
  #----------------------------------------------------------------
  # Ophalen van begrippen
  #----------------------------------------------------------------
  
  if (pUitBeheertool) {
    lProgress <- lLimit / pMaxRijen
    message(lProgress)
    withProgress(message = "Begrippen ophalen", value = 0, {
      while (lDoorgaan) {
        incProgress(lProgress)
        begrippenDFlokaal <- ophalenWaardenBegrippen()
        begrippenDF <- rbind(begrippenDF,begrippenDFlokaal)
      }
    })
    
  }
  else {
    message("Niet via de beheertool")
    while (lDoorgaan) {
      begrippenDFlokaal <- ophalenWaardenBegrippen()
      begrippenDF <- rbind(begrippenDF,begrippenDFlokaal)
      #write.table(begrippenDFlokaal
      #            , paste("C:/temp/Domeintabellen/WikiUpdate/WNS historische verwijzingen/"
      #                    ,paste("lCHM",lOffset-lLimit,sep = "-"),".csv",sep=""), sep = ";", na = "", row.names = F, col.names = T, fileEncoding = "UTF8")
    }
  }
  # Zorg dat alle NA zijn vervangen door ""
  begrippenDF[is.na(begrippenDF)] <- ""
  message("Einde ophalenBegrippen")
  return(begrippenDF)

}

controleerObjectUniek <- function(pElementtype
                                 ,pDomeinTabel=""
                                 ,pKenmerk
                                 ,pKenmerkWaarde
                                 ,pBeginGeldigheid
                                 ,pEindGeldigheid
                                 ,pWijzigingen
                                 ,pGuid
                                 ,pOmgeving
                                 ,pOpmaakJson
                          
) {
  message(paste("Controle uniek: "),paste(pElementtype,pKenmerk,pKenmerkWaarde,pBeginGeldigheid,pEindGeldigheid,pGuid))
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  
  lBeginGeldigheid <- bewerkDatum(pBeginGeldigheid)
  lEindGeldigheid  <- bewerkDatum(pEindGeldigheid)
  lGuidWiki <- "xxx"
  
  # Eerst controleren of er overlappende wijzigingen in de CSV zitten
  # - zelfde kenmerk
  # - overlap in periode
  inWijzingen <- pWijzigingen[which(pWijzigingen[,pKenmerk] == pKenmerkWaarde),]
  lAantalKenmerk <- nrow(inWijzingen)
  #message(paste("1 Aantal overlappende wijzigingen:",nrow(inWijzingen)))
  inWijzingen <- inWijzingen[which(as.Date(inWijzingen$'Begin geldigheid') <= lEindGeldigheid),]
  #message(paste("2 Aantal overlappende wijzigingen:",nrow(inWijzingen)))
  inWijzingen <- inWijzingen[which(inWijzingen$'Eind geldigheid' >= lBeginGeldigheid),]
  
  if (lAantalKenmerk == 1 && inWijzingen[1,"Guid"] != "") lGuidWiki <- inWijzingen[1,"Guid"]
  #essage(paste("3 Aantal overlappende wijzigingen:",nrow(inWijzingen),"lAantalKenmerk:",lAantalKenmerk))
  
  # 2 of meer met overlap in de wijzigingen is fout
  if (nrow(inWijzingen) >= 2) return(FALSE)
  else {
    # Er is dus geen overlap in de aangeleverde wijzigingen
    # Als er meer dan 1 met t zelfde kenmerk zijn gevonden
    if (lAantalKenmerk > 1) {
      # Als het om een beëindiging gaat dan is de Guid gevuld en is geen overlap voldoende
      if (pGuid != "") {
        lGuidWiki <- inWijzingen[1,"Guid"]
        #message(paste("Beëindigen dus oké Guid:",lGuidWiki))
      }
      # Anders checken of hetzelfde kenmerk hiervoor beeïndigd wordt
      else  {
        lBeginGeldigheidMinEen <- bewerkDatum(as.Date(pBeginGeldigheid)-1)
        inWijzingenOK <- pWijzigingen[which(pWijzigingen[,pKenmerk] == pKenmerkWaarde),]
        #message(paste("1 Aantal aansluitende wijzigingen:",nrow(inWijzingenOK)))
        inWijzingenOK <- inWijzingenOK[which(inWijzingenOK$'Eind geldigheid' <= lBeginGeldigheidMinEen),]
        #message(paste("2 Aantal aansluitende wijzigingen:",nrow(inWijzingenOK)))
        inWijzingenOK <- inWijzingenOK[which(substring(inWijzingenOK$'Controle meldingen',1,2) == "OK"),]
        #message(paste("3 Aantal aansluitende wijzigingen:",nrow(inWijzingenOK)))
        
        # Als er één waarde te beëindigen is dan die Guid nemen
        if (nrow(inWijzingenOK) == 1) lGuidWiki <- inWijzingenOK[1,"Guid"]
        #message(paste("Guid van de te beeindigen waarde:",lGuidWiki))
        if (nrow(inWijzingenOK) == 0 && pGuid == "") return(FALSE)
      }
    }
  }
  
  # Nu controleren tegen de Wiki database
  curl <- getCurlHandle()
  categorie <- NULL
  kenmerken <- "%2F-3FBegin geldigheid%2F-3FEind geldigheid"
  lOpmaakJson <- str_replace(pOpmaakJson,"lLimit","250")
  lOpmaakJson <- str_replace(lOpmaakJson,"lOffset","0")

  # Alle waarden met hetzelfde kenmerk en overlap in periode
  beperking <- paste("?title=Speciaal:Vragen&x=[[Elementtype::",pElementtype,"]]",sep = "")
  if (pDomeinTabel != "") beperking <- paste(beperking,"[[Breder::",gsub("-","-2D",pDomeinTabel),"]]",sep = "")
  beperking <- paste(beperking,"[[",pKenmerk,"-3A-3A",zoekString(pKenmerkWaarde,FALSE),"]]"
                     ,"[[Begin geldigheid-3A-3A-3C-3D=",str_replace_all(pEindGeldigheid,"/","-"),"]]"
                     ,"[[Eind geldigheid-3A-3A-3E-3D",str_replace_all(pBeginGeldigheid,"/","-"),"]]"
                     ,sep = "")
  
  json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
  #message(json_file)
  req <- httr::GET(json_file, curl=curl)
  if (req$status_code == 200 && length(req$content) > 0)  {
    lJson <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))$results
    for (x in 1:length(lJson)) {
      lBegin <- bewerkDatum(lJson[[x]]$printouts$`Begin geldigheid`[2])
      lEind  <- bewerkDatum(lJson[[x]]$printouts$`Eind geldigheid`[2])
      lGuid  <- lJson[[x]]$fulltext
      #message(paste(x,":",lBegin,pBeginGeldigheid,lEind,pEindGeldigheid,lGuid,lGuidWiki,"xxx",sep="#"))
      if (lBegin <= pEindGeldigheid && lEind >= pBeginGeldigheid && lGuid == lGuidWiki) return(TRUE)
    }
    #message("Geen corresponderende waarde in Wiki om te beeindigen")
    return(FALSE)
  }
  else return(TRUE)
}

ophalenCollectieGuid <- function(pLabelNL
                                ,pOmgeving
                                ,pOpmaakJson
                          
) {
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  
  lReturn <- NA
  curl <- getCurlHandle()
  categorie <- NULL
  beperking <- paste("?title=Speciaal:Vragen&x=-5B-5BElementtype%3A%3ACollectie-5D-5D-5B-5BLabel%20(nl)::",pLabelNL,"-5D-5D",sep = "")
  kenmerken <- "%2F-3FElementtype"
  lOpmaakJson <- str_replace(pOpmaakJson,"lLimit","1")
  lOpmaakJson <- str_replace(lOpmaakJson,"lOffset","0")
  json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
  #message(json_file)
  req <- httr::GET(json_file, curl=curl)
  if (req$status_code == 200 && length(req$content) > 0)  {
    lJson <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))$results
    lGuid <- lJson[[1]]$fulltext
    return(lGuid)
  }
  return(lReturn)
}

ophalenCollectieLabel <- function(pGuids
                                 ,pOmgeving
                                 ,pOpmaakJson
                                 
) {
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  
  lGuids <- strsplit(pGuids,",")
  lReturn <- NA
  for (j in 1:length(lGuids[[1]])) {
    lGuid <- str_trim(lGuids[[1]][j])
    message(lGuid)
    curl <- getCurlHandle()
    categorie <- NULL
    beperking <- paste("?title=Speciaal:Vragen&x=-5B-5B",gsub("-","-2D",lGuid),"-5D-5D",sep = "")
    kenmerken <- "%2F-3FElementtype%2F-3FLabel%20(nl)"
    lOpmaakJson <- str_replace(pOpmaakJson,"lLimit","1")
    lOpmaakJson <- str_replace(lOpmaakJson,"lOffset","0")
    json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
    #message(json_file)
    req <- httr::GET(json_file, curl=curl)
    if (req$status_code == 200 && length(req$content) > 0)  {
      lJson <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))$results
      if (is.na(lReturn)) lReturn <- lJson[[1]]$printouts$`Label (nl)`
      else lReturn <- paste(lReturn,lJson[[1]]$printouts$`Label (nl)`,sep = ",")
    }
  }
  return(lReturn)
}



wijzigBegrippenInWiki <- function(pOmgeving
                                 ,pUserName
                                 ,pPassword
                                 ,pWijzigingenBegrippen
                                 ,pBeheertoolControlesBegrippen
                                 ,pMetadataBegrippen
                                 ,pUitBeheertool=FALSE
) {
  message("Wijzigen begrippen")

  baseUrl  <- paste("https://",pOmgeving,".aquo.nl/api.php",sep="")
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  wikitekst <- NULL
  
  curl <- getCurlHandle()
  ## Retrieve the login token
  botToken <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", baseUrl), curl=curl ) )$query$tokens$logintoken
  loginParams=list('lgtoken'=botToken,'lgname'=pUserName,'lgpassword'=pPassword)
  loginResponse <- content( POST(sprintf("%s?action=login&format=json", baseUrl) , body=loginParams, curl=curl) )$login$result
  editToken <- content( GET(sprintf("%s?action=query&meta=tokens&type=csrf&format=json", baseUrl), curl=curl ))$query$tokens$csrftoken
  
  message(paste(Sys.time(),"Het editToken:",editToken))
  tijdelijk <<- editToken
  
  lengte <- nrow(pWijzigingenBegrippen)
  
  doorgaan <- TRUE

  for (j in 1:lengte)
  {
    if (doorgaan && substring(pWijzigingenBegrippen[j,"Controle meldingen"],1,2) == "OK") { #message(paste("aantal pWijzigingenBegrippen: ",lengte))
      velden <- NULL
      #message(paste(j,pWijzigingenBegrippen[j,"Label (nl)"]))
      for (g in 1:length(pMetadataBegrippen)) {
        if (pWijzigingenBegrippen[j,pMetadataBegrippen[g]] != "" &&
            pBeheertoolControlesBegrippen[pMetadataBegrippen[g],]$Verplicht != "V"
            )
        {
          velden[pMetadataBegrippen[g]] <- paste("|",pMetadataBegrippen[g],"=",pWijzigingenBegrippen[j,pMetadataBegrippen[g]],"\n",sep='')
        }
      }
      #message(paste("Guid voor toekennen:"),pWijzigingenBegrippen[j,"Guid"])
      if (nchar(toString(pWijzigingenBegrippen[j,"Guid"])) < 10) {
        pWijzigingenBegrippen[j,"Guid"] <<- paste("Id-",uuid::UUIDgenerate(),sep="")
      }
      
      if (nchar(toString(pWijzigingenBegrippen[j,"Guid"])) > 10) {
        wikitekst[j] <- paste("{{#element:\n",
                              "|Elementtype=Begrip","\n",
                              #"|Paginanaam=",pWijzigingenBegrippen[j,"Guid"],"\n",
                              paste(gsub("=NA\n","=\n",velden),collapse=''),
                              "}}",sep = '')
        
        editParams=list('title'=toString(pWijzigingenBegrippen[j,"Guid"]),'text'=wikitekst[j],'token'=editToken,'format'='json')
        
        # message("title----------------------------------------")
        # message(editParams$title)
        # message("text----------------------------------------")
        # message(editParams$text)
        # message("token----------------------------------------")
        # message(editParams$token)
        # message("format----------------------------------------")
        # message(editParams$format)
        
        editResponse <- content( POST(sprintf("%s?action=edit&format=json", baseUrl) , body=editParams, curl=curl) )
        editStatus <- editResponse$edit$result
        #message("Het resultaat ")
        if (nchar(toString(editStatus)) == 0 || toString(editStatus) != "Success") {
          doorgaan <- FALSE
        }
        message(paste("Verwerking:",editStatus,j,pWijzigingenBegrippen[j,"Label (nl)"],pWijzigingenBegrippen[j,"Guid"]))
        Sys.sleep(0.1)
      }
    }
    else message(paste(pWijzigingenBegrippen[j,"Label (nl)"]," niet verwerken"))
    if (j %% 100 == 0) message(paste("Voortgang:",j,Sys.time()))
  } #for

  message(paste(Sys.time(),"voor de shinyalert"))

}

verwijderBegrippenUitWiki <- function(pOmgeving
                                      ,pUserName
                                      ,pPassword
                                      ,pVerwijderenBegrippen
) {
  message("Verwijder begrippen")
  
  baseUrl  <- paste("https://",pOmgeving,".aquo.nl/api.php",sep="")
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  
  curl <- getCurlHandle()
  botToken <- content( GET( sprintf("%s?action=query&meta=tokens&type=login&format=json", baseUrl), curl=curl ) )$query$tokens$logintoken
  loginParams=list('lgtoken'=botToken,'lgname'=pUserName,'lgpassword'=pPassword)
  loginResponse <- content( POST(sprintf("%s?action=login&format=json", baseUrl) , body=loginParams, curl=curl) )$login$result
  
  editToken <- content( GET(sprintf("%s?action=query&meta=tokens&type=csrf&format=json", baseUrl), curl=curl ))$query$tokens$csrftoken
  if (toString(editToken) != "+\\") {
    message(Sys.time())
    for (i in 1:nrow(pVerwijderenBegrippen)) {
    #for (i in 2:50) {
      pagina <- pVerwijderenBegrippen[i,"Guid"]
      editParams=list('title'=pagina,'token'=editToken,'format'='json')
      editResponse <- content( POST(sprintf("%s?action=delete&format=json&title=%s", baseUrl,pagina), body=editParams, curl=curl) )
      editStatus <- editResponse$delete$logid
      if (length(editStatus) > 0 && !is.na(editStatus)) {
        message(paste(str_pad(toString(i),5,"left"," "), pagina, "Verwijderd (editStatus):", toString(editStatus), Sys.time(), sep = " "))
      }
      else {
        message(paste("Verwijderen lukt niet van pagina: ",pagina))
        if (length(editStatus) > 0 && !(editStatus > 0)) {
          message("30 seconden wachten, dan nieuw token")
          Sys.sleep(30)
          editToken <- content( GET(sprintf("%s?action=query&meta=tokens&type=csrf&format=json", baseUrl), curl=curl ))$query$tokens$csrftoken
          message(paste("nieuw token",editToken,sep = " "))
          editResponse <- content( POST(sprintf("%s?action=delete&format=json&title=%s", baseUrl,pagina), body=editParams, curl=curl) )
          editStatus <- editResponse$delete$logid
        }
      }
    }
    message(Sys.time())
  }
}

bepaalMetadataWNS <- function(pOmgeving
                              ,pOpmaakJson) 
{
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")

  curl <- getCurlHandle()
  
  categorie <- "?title=Speciaal:Vragen&x=-5B-5BElementtype%3A%3ADomeintabelverzamellijst-5D-5D-5B-5BVoorkeurslabel%3A%3AWaarnemingssoort-5D-5D"
  beperking <- NULL
  kenmerken <- "%2F-3FMetadata"
  lOpmaakJson <- str_replace(pOpmaakJson,"lLimit","500")
  lOpmaakJson <- str_replace(lOpmaakJson,"lOffset","0")
  json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
  message(json_file)
  req <- httr::GET(json_file, curl=curl)
  if ((req$status_code == 200))
    lMetadata <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))[2]$results[[1]]$printouts$Metadata
  if (!("Status" %in% lMetadata)) lMetadata[length(lMetadata)+1] <- "Status"
  if (!("Wijzigingsnummer" %in% lMetadata)) lMetadata[length(lMetadata)+1] <- "Wijzigingsnummer"
  message(paste("Bepalen metadata WNS, aantal:",length(lMetadata),sep=" "))
  return(lMetadata)
}

ophalenWaarnemingssoort <- function(pWNScode
                                  ,pMetadata
                                  ,pOmgeving
                                  ,pOpmaakJson
                          
) {
  tekstUrl <- paste("https://",pOmgeving,".aquo.nl/index.php",sep="")
  
  lWNS <- as.data.frame(matrix(ncol = (length(pMetadata)+2), nrow = 1))
  lMetadata <- c(pMetadata,c("Guid","Controle meldingen"))
  colnames(lWNS) <- lMetadata
  lWNS[1,"Controle meldingen"] <- "OK"
  curl <- getCurlHandle()
  categorie <- NULL
  kenmerken <- NULL
  beperking <- paste("?title=Speciaal:Vragen&x=-5B-5BElementtype%3A%3ADomeinwaardeVerzamellijst-5D-5D-5B-5BCodes%3A%3A",pWNScode,"-5D-5D",sep = "")
  for (i in 1:length(pMetadata)) kenmerken <- paste(kenmerken,"%2F-3F",pMetadata[i],sep="")
  lOpmaakJson <- str_replace(pOpmaakJson,"lLimit","1")
  lOpmaakJson <- str_replace(lOpmaakJson,"lOffset","0")
  json_file <- maakTekstURL(tekstUrl,categorie,beperking,kenmerken,lOpmaakJson)
  #message(json_file)
  req <- httr::GET(json_file, curl=curl)
  if (req$status_code == 200 && length(req$content) > 0)  {
    lJson <- jsonlite::fromJSON(httr::content(req, "text", encoding="UTF-8"))$results[[1]]

    lWNS[1,"Guid"] <- lJson$fulltext
#    message(paste("Guid:",lWNS[1,"Guid"]))
    for (i in 1:length(pMetadata))  {
      lkenmerk <- lJson$printouts[[i]]
      lname <- names(lJson$printouts[i])
      #message(length(lkenmerk))
      if (length(lkenmerk) > 0) {
        if (typeof(lkenmerk) == "list") {
          if (length(lkenmerk) > 1) {
            for (j in 1:length(lkenmerk)) {
              lfield <- names(lkenmerk[j])
              if (lfield == "fulltext") {
                lWNS[1,lname] <- lkenmerk[[j]]
              }
              else {
                if (lfield == "raw") {
                  lWNS[1,lname] <- bewerkDatum(lkenmerk[[j]])
                }
              }
 #             message(paste(i,j,lname,lkenmerk[[j]]))
            }
          }
        }
        else {
          lkenmerk <- lJson$printouts[i]
          lWNS[1,lname] <- lkenmerk
#          message(paste(i,names(lkenmerk),lkenmerk))
        }
      }
      else
      {
        lWNS[1,lname] <- NA
 #       message(paste(i,lname,pMetadata[i],"is leeg"))
      }
    }
  }
  return(lWNS)
}

library(shiny)
library(shinythemes)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(formattable)
library(DT)
library(billboarder)




fluidPage({
  tagList(
    # Bootstrap header
    tags$header(class = "navbar navbar-default navbar-static-top",
                includeCSS("www/styles.css"),
                tags$div(class = "container-fluid",
                         tags$div(class = "navbar-header",
                                  tags$div(class = "navbar-brand", "Contrat de performance des IEPP")
                         ),
                         tags$span(icon("graduation-cap"), class = "main-icon")
  
                )
            ),

  fluidPage(theme = shinythemes::shinytheme("united"),
              sidebarLayout(position = "left",
                            column(width = 4,
                                   wellPanel(
                                     selectInput("Region","Region",c("GOLFE_LOME","MARITIME","PLATEAUX","CENTRALE","KARA","SAVANES")),
                                     selectInput("Inspection","Inspection", as.character(sort(unique(iepp[,iepp$IEPP]))))
                                   )),
               mainPanel( tabsetPanel(id = "tabs",
                                  tabPanel("Contexte", tags$br(),
                                                   tags$h2("Quelques données de contexte"),
                                            fluidRow(
                                                tags$div(class="card",uiOutput("table"))),
                                            fluidRow(
                                                   tags$h2("Resultat aux CEPD"),
                                                   tags$div(class="card",formattableOutput("moyen1")),
                                                   tags$details(tags$p("Moyenne_des_Notes:Moyennne des notes de l'ensemble des cadidats dans la matière",tags$br(),
                                                   "Prc_note_sup_egal_10: Pourcentage des élèves une note supérieure ou égale à 10 dans la matiere"))
                                                   
                                                   ),
                                           fluidRow(tags$h2("Comparaison de quelques indicateurs de l'inspection avec la Region et le Togo"),
                                                    tags$div(class="card",billboarderOutput(outputId = "indica")
                                             
                                           )),

                                           fluidRow(tags$h2("Repartition Géographique des Ecoles de l'inspection"),
                                                     tags$div(class="card",leafletOutput("myMap"))
                                                   )

                                          ),
                               tabPanel("Contrat", tags$hr(),
                                        fluidRow(tags$h2("Le Montant"), 
                                                 tags$hr(),
                                                 tags$div(class="card",uiOutput("Contrat")) 
                                                 ),
                                        fluidRow( tags$h2("Télécharger le Contrat"), 
                                                  tags$hr(),
                                                  tags$div(class="card",uiOutput("moyen3")) 
                                                 
                                                 
                                                 )),
                                tabPanel("Visite", tags$br(),
                                                   tags$hr(),
                                          fluidRow(tags$h2("Synthèse des sorties pédagogique"),tags$hr(),
                                                          
                                          dateRangeInput("dateRange", "Séléctionner la période:",
                                          start = min(data_bulletin$Date_visite),end = max(data_bulletin$Date_visite)),
                                          tags$div(class = "card",uiOutput("synthese"))
                                           ),
                                           tags$hr(),
                                           fluidRow(tags$h2("Les bulletins des sorties pédagogique"),tags$hr(),
                                           tags$div(class="card",DT::DTOutput("bulletin"))),
                                           tags$hr(),
                                           fluidRow(tags$h2("Les visites Effectuées"),tags$hr(),
                                                     tags$div(class="card",DT::DTOutput("visite"))),
                                          
                                           tags$br(),        
                                           fluidRow(tags$h2("Télécharger le bulletin d'une sortie pédagogique"),tags$hr(),
                                           tags$div(class = "card",uiOutput("telech"),
                                           radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                                                  inline = TRUE),
                                           p(downloadButton("report", "Télécharger",class = "btn-primary" )))

                                                     
                                                   )
                                          ),
                                          tabPanel("Ressource", tags$br(),
                                                   includeMarkdown("ressources.Rmd")
                                                   #source("login.R")
                                          )
                              )
                            )
              )
  ))
  
})
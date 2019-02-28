library(shiny)
library(leaflet)
library(dplyr)
library(curl)
library(dplyr)
library(kableExtra)
library(formattable)
library(htmlwidgets)
library(tibble)
library(DT)
library(lubridate)
library(rmarkdown)
library(billboarder)
library(scales)
library(htmltools)

#source("import.R")
shinyServer(function(input, output,session) {
  
  observe({
    input$Region
    shiny::updateSelectInput(session, "Inspection",
                      label = "Inspection",
                      choices=as.character(sort(unique(iepp$IEPP[toString(input$Region)==iepp$DRE])))
    )
    
  })
  

  tags$p("Quelques donnees de contexte")
  
  output$table <- renderUI({
    # Create a Bootstrap-styled table
 
    tags$table(class = "table",

               tags$tbody(
                 tags$tr(
                   tags$td("Total Ecole"),
                   tags$td(tags$span(style = 
                     "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp[iepp$IEPP==input$Inspection,c("ENSEMBLE")])))),
                   tags$td("Effectif Eleve"),
                   tags$td(tags$span(style = 
                     "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp[iepp$IEPP==input$Inspection,c("Annee_2_Total_Eff")]))))
                 ),
                 tags$tr(
                   tags$td("EPP"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp[iepp$IEPP==input$Inspection,c("EPP")])))),
                   tags$td("Privees"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp[iepp$IEPP==input$Inspection,c("PRIVE")]))))
                 ),
                 tags$tr(
                   tags$td("EDIL"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp[iepp$IEPP==input$Inspection,c("EDIL")])))),
                   tags$td("% Ecoles accessibles"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",french_percent(as.numeric((iepp[iepp$IEPP==input$Inspection,c("accs")]/iepp[iepp$IEPP==input$Inspection,c("ENSEMBLE")])))))
                 ),
                 tags$tr(
                   tags$td("% Ecoles Urbaines"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",french_percent(as.numeric(((iepp[iepp$IEPP==input$Inspection,c("urbain")])/(iepp[iepp$IEPP==input$Inspection,c("ENSEMBLE")])))))),
                   tags$td("% Ecoles Electrifiées"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",french_percent(as.numeric(((iepp[iepp$IEPP==input$Inspection,c("electricite")])/(iepp[iepp$IEPP==input$Inspection,c("ENSEMBLE")])))))
                 )),
                 tags$tr(
                   tags$td("% Ecoles latrines Fonc."),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",french_percent(as.numeric(((iepp[iepp$IEPP==input$Inspection,c("latrines")])/(iepp[iepp$IEPP==input$Inspection,c("ENSEMBLE")])))))),
                   tags$td("% Ecoles Point d'eau"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",french_percent(as.numeric(((iepp[iepp$IEPP==input$Inspection,c("eau")])/(iepp[iepp$IEPP==input$Inspection,c("ENSEMBLE")]))))))
                 ),
                 tags$tr(
                   tags$td("Effectif Enseignants"),
                   tags$td(tags$span(style = 
                      "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp[iepp$IEPP==input$Inspection,c("Ens_Total")])))),
                   tags$td("Salles occupees"),
                   tags$td(tags$span(style = 
                      "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp[iepp$IEPP==input$Inspection,c("Salle")]))))
                 )

               )
    )
  })


  data <-reactive({
    fr <- iepp2[iepp2$IEPP==input$Inspection,c("Francais_T")]
    math <- iepp2[iepp2$IEPP==input$Inspection,c("Maths_T")]
    lec <- iepp2[iepp2$IEPP==input$Inspection,c("Lecons_T")]
    ens <- iepp2[iepp2$IEPP==input$Inspection,c("Ensemble_T")]
    dat0 <- cbind(fr,math,lec,ens)
    dat0 <-t(dat0)
    rownames(dat0) <- NULL
    dat1<- data.frame(dat0,stringsAsFactors=FALSE)
    colnames(dat1)<-c("Moyenne_des_Notes")
    rownames(dat1)<-c("Francais","Maths","Lecons","Ensemble")
    fr0 <- iepp2[iepp2$IEPP==input$Inspection,c("Sup10_Fran")]
    math0 <- iepp2[iepp2$IEPP==input$Inspection,c("Sup10_Math")]
    lec0 <- iepp2[iepp2$IEPP==input$Inspection,c("Sup10_Lec")]
    ens0 <- iepp2[iepp2$IEPP==input$Inspection,c("Sup10_Global")]
    dat2 <- cbind(fr0,math0,lec0,ens0)
    dat2 <-t(dat2)
    rownames(dat2) <- NULL
    colnames(dat2)<-c("note_sup_egal_10")
    data <-cbind(dat1,dat2)
    
    data %>%
     rownames_to_column('Matiere') %>%
     mutate(Prc_note_sup_egal_10=note_sup_egal_10*100) %>%
     select(Matiere,Moyenne_des_Notes,Prc_note_sup_egal_10)
    
  })
  

  
  output$moyen1 <- renderFormattable({
    
    formattable(data(),list(
      Moyenne_des_Notes = formatter("span",
                        style = x ~ ifelse(x <=10 , style(color = "red", font.weight = "bold"), NA)),
      Prc_note_sup_egal_10 = formatter("span",
                        style = x ~ ifelse(x<=50 , style(color = "red", font.weight = "bold"), NA))
     )
    )
    
  })
  

  output$myMap <- renderLeaflet({
                               
      carte <- reactive({  
        ecole %>%
          filter(ecole$IEPP == input$Inspection)
      })  
      wardpal <- colorFactor(viridis(5), carte()$Statut)
      
      leaflet(data = carte()) %>%
        addTiles() %>%
        addPolygons(data = carte(),fill = FALSE) %>%
                    #weight = 2,
                    #opacity = 1,
                    #color = 'gray',
                    #smoothFactor = 0.5,
                    #dashArray = '3',
                    #fillOpacity = 0.7) %>%
        addCircleMarkers(lng = carte()$Longitude,
                         lat = carte()$Latitude,
                         radius = 4,
                         fillColor = wardpal(carte()$Statut),
                         stroke = FALSE,
                         fillOpacity = 0.8,
                         popup = paste("Nom Etablissement : ", carte()$NOM_ETABLISSEMENT, "<br>","<hr>",
                                       "Effectif garçons : ",carte()$eff_G, "<br>",
                                       "Effectif filles : ",carte()$eff_F,"<br>",
                                       "Effectif Enseignant : ",carte()$Nb_Total_Ens, "<br>",
                                       "Effectif Ens. Fonct. : ",carte()$ens_Fonc, "<br>",
                                       "Nbr salle en dur : ",carte()$NB_SALLES_Dur,"<br>"
                         ),label = htmlEscape(carte()$NOM_ETABLISSEMENT),labelOptions = lapply(1:nrow(carte()), function(x) {
                           labelOptions(opacity = 0.9)
                         })) %>%
        #addMarkers(lng = carte()$Longitude,
        #           lat = carte()$Latitude,label = ~htmlEscape(carte()$NOM_ETABLISSEMENT)) %>%
        addLegend(pal = wardpal,values = carte()$Statut) %>%
        addMiniMap()
      
}) 
 
  dat1 <- reactive({
    data_bulletin %>%
      filter(as_date(Date_visite) >= input$dateRange[1], as_date(Date_visite) <= input$dateRange[2]) %>%
      select(Date_visite,Nom_Ecole=identification_nom_etablissement,etablissement,Nom_Enseignant=enseignant_nom_prenom,Diplome=enseignant_diplom_academique,Classe=enseignant_classe) %>%
      filter(data_bulletin$identification_inspection==input$Inspection)
    
  }) 
  
  dat2<-reactive({
    data_visite %>%
      filter(as_date(Date_visite) >= input$dateRange[1], as_date(Date_visite) <= input$dateRange[2]) %>%
      select(Date_visite,Nom_Ecole=identification_nom_etablissement,Nom_Enseignant=enseignant_nom_prenom,Diplome=enseignant_diplom_academique,Classe=enseignant_classe) %>%
      filter(data_visite$identification_inspection==input$Inspection)
  })
  

  output$synthese <- renderUI({
    
    
    tags$table(class = "table",
               
               tags$tbody(
                 tags$tr(
                   tags$td("Nombre de grilles remplies sur la période"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",nrow(dat2()))),
                   tags$td("Nombre de bulletins remplis sur la période"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",nrow(dat1())))
                 )))
    
  })
  
  #############################Synthèse#####################
  ######## Bulletin ###############################
  
#  output$synthese1 <- DT::renderDT({
#  datatable(    
#    data_visite %>%
#      filter(data_visite$identification_inspection==input$Inspection) %>%
#      filter(as_date(Date_visite) >= input$dateRange[1], as_date(Date_visite) <= input$dateRange[2]) %>%
#      select(Date_visite,Ecole=identification_nom_etablissement,Enseignant=enseignant_nom_prenom) , options = list(pageLength = 5))
#   })

  ######## visite ###############################
#  output$synthese2 <- DT::renderDT({
#  datatable(data_bulletin %>%
#            filter(data_bulletin$identification_inspection==input$Inspection)%>%
#            filter(as_date(Date_visite) >= input$dateRange[1], as_date(Date_visite) <= input$dateRange[2]) %>%
#            select(Date_visite,Ecole=identification_nom_etablissement,Enseignant=enseignant_nom_prenom), options = list(pageLength = 5))
#  })
  
 ######################bulletin############### 

  
  output$bulletin <- DT::renderDT({
    datatable(dat1(),options = list(pageLength = 5))
  })

  
  #######################visite#############  
  

  
  output$visite <- DT::renderDT({
    datatable(dat2(),options = list(pageLength = 5))
  })
 
  
  output$telech <- renderUI(
    selectInput("Ecole","Ecole", as.character(sort(unique(dat1()$etablissement))))
    
  )
  
  output$report <- downloadHandler(
    filename = function() {
      paste(input$Ecole, sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('bulletin.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "bulletin.Rmd", overwrite = TRUE)
      
      # data_bulletin %>%
      #  filter(identification_inspection==input$Inspection) %>%
      # select(identification_inspection)-> inspection
      
     # parametre <- list(insp=dat1()[dat1()$identification_inspection==input$Inspection,c("identification_inspection")])
      
      out <- rmarkdown::render("bulletin.Rmd", params=list(insp=input$Inspection,ecole=input$Ecole),
      switch(input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()),envir=new.env(parent = globalenv()),clean=F)
      file.rename(out, file)
    }

    
  )
  
  output$Contrat <- renderUI({
    
    
    tags$table(class = "table",
               
               tags$tbody(
                 tags$tr(
                   tags$td("Rapport Envoye"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",iepp2[iepp2$IEPP==input$Inspection,c("Rapport_recu")])),
                   tags$td("Contenu du Rapport"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",iepp2[iepp2$IEPP==input$Inspection,c("contenu_rapports")]))
                 ),
                 tags$tr(
                   tags$td("Effectif des Enseignants Prevus"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp2[iepp2$IEPP==input$Inspection,c("Nbre_Ens_prevu")])))),
                   tags$td("Effectif des enseignants formes"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp2[iepp2$IEPP==input$Inspection,c("Nbre_Ens_form")]))))
                 ),
                 tags$tr(
                   tags$td("Nombre de visites prevues IEPP"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp2[iepp2$IEPP==input$Inspection,c("Nbr_visite_prevu_IEN")])))),
                   tags$td("Nombre de visites réalisees IEPP"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp2[iepp2$IEPP==input$Inspection,c("Nbr_visite_realises_IEN")]))))
                 ),
                 tags$tr(
                   tags$td("Nombre de visites prevues CP"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp2[iepp2$IEPP==input$Inspection,c("Nbr_visite_prevu_CP")])))),
                   tags$td("Nombre de visites réalisees CP"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp2[iepp2$IEPP==input$Inspection,c("Nbr_visite_realises_CP")]))))
                 ),
                 
                 tags$tr(
                   tags$td("Montant du Contrat"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp2[iepp2$IEPP==input$Inspection,c("Montant_subv")])))),
                   tags$td("Montant depenses apres trois mois"),
                   tags$td(tags$span(style = 
                                       "border: 2px solid orange;border-radius: 5px; display:inline-block;",number(as.numeric(iepp2[iepp2$IEPP==input$Inspection,c("Montant_decaisse")]))))
                 )

                 
               )
    )
    
  })
  ##############################
  
  output$indica <- renderBillboarder({  
    
    indicateurs_graph <- reactive({
      indicateurs %>%
        select(Indic,input$Inspection,input$Region,TOGO)})
    
    billboarder() %>%
      bb_barchart(indicateurs_graph())%>%
      bb_y_grid(show = TRUE) %>% 
      bb_legend(show = TRUE,position = "inset", inset = list(anchor = "top-right")) %>%
      bb_labs(title = "Les principaux indicateurs (en %)",
              caption = "Source des données: Tableau de Bord/DPEE")
    
  })

  

})





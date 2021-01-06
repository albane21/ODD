
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(markdown)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(shinyBS)
library(tidyr)


########### Version 1 

shinyApp(  
ui = tagList(
    navbarPage(
      theme = "cerulean",  # <--- To use a theme, uncomment this
      "Observatoire des ODD IDF",
      tabPanel("Présentation de l'observatoire",
               mainPanel(
                 tabsetPanel(
                   tabPanel("Objectif",
                                h5(" L'objectif de cet observatoire est de présenter l'évolution dans le temps de différents indicateurs au niveau communal.")
                            ),                                
                   tabPanel("Présentation des ODD",
                            h5("Les 17 Objectifs de Développement Durable (ODD ou Agenda 2030) ont été adoptés en septembre 2015 par 193 pays aux Nations Unies, 
à la suite des Objectifs du Millénaire pour le Développement (OMD). Ils constituent un plan d’action pour la paix, l’humanité, la planète et la prospérité, 
nécessitant la mise en oeuvre de partenariats multi-acteurs. Ils ambitionnent de transformer nos sociétés en éradiquant la pauvreté et en assurant une transition 
juste vers un développement durable d'ici à 2030.")),
                   tabPanel("Données utilisées", 
                           h5("La géographie utilisée pour les indicateurs communaux est celle du 1er janvier 2020. Celle-ci est disponible à l'adresse suivante: https://www.insee.fr/fr/information/2028028")
                           ),
                   tabPanel("Explication des groupes de communes", 
                            h5("L'objectif de cet observatoire est d'élaborer un outil qui permet de situer la situation de sa commune, par rapport à la situation de communes semblables."),
                            h5("Nous avons donc créé des groupes rassemblants les communes similaires de le région Ile-de-France, en utilisant les critères suivants:"),
                            h5("-Le nombre d'habitants de la commune"),
                            h5("-Le revenu médian des ménages de la commune"),
                            h5("-Le potentiel fiscal de la commune")
                            )
                     )
                   )
               ),

      tabPanel("Choix de l'objectif de développement durable",
               h4("Veuillez sélectionner un objectif de développement durable, puis l'indicateur que souhaitez observer."),
               selectInput("ODD1", "Sélection de l'objectif de développement durable", 
                           choices =  unique(as.character(Description_indicateurs$ODD))     
                          ),
               selectInput("INDICATEUR", "Sélection de l'indicateur", 
                           choices =  sort(unique(as.character(Description_indicateurs$INDICLIB)))     
                          ),
                box(
             title = textOutput("description_indicateur"),
             textOutput("description_indicateur1"),
             status = "info",
             solidHeader = TRUE,
             width = 18
             )
               
               
               ), 
        
        
      tabPanel("Choix de la commune",
               h4("Veuillez sélectionner un département, puis la commune désirée."),
               selectInput("DEP", "Sélection du département", 
                           choices =  sort(unique(as.character(Donnees_indicateurs$LIBDEP)))     
                          ),
               selectInput("COM", "Sélection de la commune", 
                           choices =  sort(unique(as.character(Donnees_indicateurs$LIBGEO)))
                              
                          ),
                 selectInput("ODD", "Sélection de l'objectif de développement durable", 
                           choices =  unique(as.character(Donnees_indicateurs$ODD))     
                          ),
               mainPanel(
                   box(
                       title = textOutput("Titre_ODD"),
                       textOutput("ExplicationODD"),
                       textOutput("filtered_data_DEP_COM()"),
                       plotOutput(outputId = "plot", height = "500px", width="500px")

                       )
                   
                   
               )
              
               )
    )
), 

 server = function(input, output) {
     
     YearsData <- Donnees_indicateurs %>% gather(type,Year,
                                      Y2017, Y2016)

    ggplot(plotData,aes(x=Gene.Symbol,y=Normalised.count,color=type) + 
       geom_line()    ## For a line plot 
   
     output$plot <- 
     renderPlot({
      ggplot(filtered_data(), aes(ANNEE, VALEUR)) +
        geom_line(data=subset(filtered_data(),LIBGEO==input$COM), color="purple",stat="identity") +
        geom_line(data=subset(filtered_data(),LIBGEO=="IDF"), color="blue",stat="identity") +
        geom_line(data=subset(filtered_data(),LIBGEO=="Groupe 1"), color="yellow",stat="identity") +
        geom_point(data=subset(filtered_data(),LIBGEO=="Groupe 1"),stat="identity") +
        geom_point(data=subset(filtered_data(),LIBGEO=="IDF"))+
        geom_point(data=subset(filtered_data(),LIBGEO==input$COM))+
        labs(x = "Année", y = "Valeur")
     })
     
    output$barplot <- 
     renderPlot({
      ggplot(trying, mapping = aes(x=YEAR, y=PART, fill=LIBGEO)) +
        geom_bar(stat="identity", position = "dodge") +
        scale_fill_viridis(discrete=TRUE, name="")+
        theme_ipsum()+
        labs(x = "Année",y = "Part des 18-25 ans non insérés")
    })
  
   output$ExplicationODD <- 
     renderText({paste("Voici le positionnement de la commune", input$COM, "en ce qui concerne l'", input$ODD)
    })
     
    filtered_data <- 
     reactive({dplyr::filter(Donnees_indicateurs, Donnees_indicateurs$LIBGEO==input$COM, Donnees_indicateurs$ODD==input$ODD)
   })
     
    filtered_data_ODD_INDIC <-
     reactive({dplyr::filter(Description_indicateurs, Description_indicateurs$ODD==input$ODD1)
   })
     
     output$description_indicateur <-
       renderText({input$INDICATEUR
    })
     
    output$Titre_ODD <-
       renderText({paste(input$COM, ":", input$ODD)
    })
     
     filtered_data_1 <- 
     reactive({dplyr::filter(Description_indicateurs, Description_indicateurs$INDICLIB
==input$INDICATEUR)
   })
     
      output$description_indicateur1 <-
       renderText({filtered_data_1()$INDICDESC
    })
     
 ## Not working : filtrer le nom des communes selon le département sélectionné dans le menu déroulant    
     
 #   filtered_data_DEP_COM <- 
 #    reactive({dplyr::filter(Donnees_indicateurs, Donnees_indicateurs$LIBDEP==input$DEP)
 #  }) 
     

 }
)

 output$plot <- renderPlot({
       ggplot(filtered_data(), mapping = aes(x=filtered_data()$ANNEE, y=filtered_data()$VALEUR, fill=LIBGEO)) +
        geom_bar(stat="identity", position = "dodge") +
        scale_fill_viridis(discrete=TRUE, name="")+
        theme_ipsum()+
        labs(x = "Année", y = "Valeur")
    })

 plot(x= filtered_data()$ANNEE, y= range(filtered_data()$VALEUR, na.rm=TRUE), type="p", xlab="Année", ylab="Part de la population pauvre")
  })

 output$evolution <- 
     renderPlot({
      ggplot(X18_25_non_insere, aes(YEAR, PART)) +
        geom_line(data=subset(X18_25_non_insere,LIBGEO=="Groupe 1"), color="purple",stat="identity") +
        geom_line(data=subset(X18_25_non_insere,LIBGEO=="IDF"), color="blue",stat="identity") +
        geom_line(data=subset(X18_25_non_insere,LIBGEO=="Vaujours"), color="yellow",stat="identity") +
        geom_point(data=subset(X18_25_non_insere,LIBGEO=="Groupe 1"),stat="identity") +
        geom_point(data=subset(X18_25_non_insere,LIBGEO=="IDF"))+
        geom_point(data=subset(X18_25_non_insere,LIBGEO=="Vaujours"))+
        labs(x = "", y = "Part des 18-25 ans non insérés")
     })

  ########### Version 2 : dashboard


     
 
    



 box(
             title = "Vaujours",
             h5("La commune de ")
             ),
             box(
             title = "ODD8: Taux des 18-25 ans non insérés",
             footer = "Explication de l'indicateur",
             status = "info",
             solidHeader = TRUE,
             width = 18,
             plotOutput("barplot")
             ),
             box(
             title = "ODD8: Taux des 18-25 ans non insérés",
             footer = "Explication de l'indicateur",
             status = "info",
             solidHeader = TRUE,
             width = 18,
             plotOutput("evolution")
             ),
             infoBox(
             title = "Evolution ODD1",
             value = "en %",
             subtitle = "Entre 2006 et 2016",
             icon = icon("line-chart"),
             fill = TRUE,
             color = "light-blue",
             width = 4
             ),
             valueBox(
             value = "+ 38%",
             subtitle = "Evolution sur 5 ans",
             color = "green",
             width = 4
             ),
             tabBox(
             title = "Informations",
             width = 4,
             tabPanel(title = "Prix médian", "contenu 1"),
             tabPanel(title = "Nombre", "contenu 2")
             )
             )













# Lecture des données--------------------
#read.csv() #A remplir plus tard avec la base de données finalisée

# Essai d'une base de donnée pour avoir grahique barplot avec uniquement certaines localisations => à automatiser pour chacune des communes et du groupe associé
trying = X18_25_non_insere[X18_25_non_insere$LIBGEO =="IDF"| X18_25_non_insere$LIBGEO=="Groupe 1"|X18_25_non_insere$LIBGEO=="Vaujours",]


## Création du header----------------------  

    header <- 
        dashboardHeader(
            title = HTML("Observatoire des ODD IDF"),
            disable = FALSE,   	
            titleWidth = 300               
    )

##Ajout du logo de la DRIEE en haut à gauche : ajouter le code URL du logo    
header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='http://www.driee.ile-de-france.developpement-durable.gouv.fr/',
                                             tags$img(src='ODD/www/index.png')) #,height='67',width='228.6', align = 'left'

## Création du siderbar ------------------------------

  siderbar <-  
    dashboardSidebar(
            width = 200,
      sidebarMenu(
        menuItem("Présentation de l'observatoire", tabName = "obs"),
        menuItem("Communes", tabName = "commune_choice", icon = icon("list-ol")),
        div( id = 'sidebar_cr',
             conditionalPanel("input.sidebar === 'commune_choice'",
                               selectizeInput("Liste_Communes",
                                              "Choix de la commune", 
                                              choices =  Donnees_communes$LIBDEP, 
                                             selected = NULL,  width = "200px",
                                              multiple = T), #,
                              actionButton('btn_build_country_report', 
                                            paste0('Bilan de ta commune'),
                                            icon = icon('wrench')))),
        menuItem("ODD1", tabName = "ODD1"),
        menuItem("ODD2", tabName = "ODD2"),
        menuItem("ODD3", tabName = "ODD3"),
        menuItem("ODD4", tabName = "ODD4"),
        menuItem("ODD5", tabName = "ODD5"),
        menuItem("ODD6", tabName = "ODD6"),
        menuItem("ODD7", tabName = "ODD7"),
        menuItem("ODD8", tabName = "ODD8"),
        menuItem("ODD9", tabName = "ODD9"),
        menuItem("ODD10", tabName = "ODD10"),
        menuItem("ODD11", tabName = "ODD11"),
        menuItem("ODD12", tabName = "ODD12"),
        menuItem("ODD13", tabName = "ODD13"),
        menuItem("ODD14", tabName = "ODD14"),
        menuItem("ODD15", tabName = "ODD15")
      )
    )

 body <-       
 dashboardBody(
     
             tabItems(
                 
                 tabItem(tabName = "obs",
                h2("Présentation de l'observatoire des Objectifs de Développement Durable pour le territoire d'Ile-de-France"), 
                h5(" L'objectif de cet observatoire est de présenter l'évolution dans le temps de différents indicateurs au niveau communal.")
                        ),
    
             tabItem(
             "commune_choice",
             box(
             title = "Vaujours",
             h5("La commune de ")
             ),
             box(
             title = "ODD8: Taux des 18-25 ans non insérés",
             footer = "Explication de l'indicateur",
             status = "info",
             solidHeader = TRUE,
             width = 18,
             plotOutput("barplot")
             ),
             box(
             title = "ODD8: Taux des 18-25 ans non insérés",
             footer = "Explication de l'indicateur",
             status = "info",
             solidHeader = TRUE,
             width = 18,
             plotOutput("evolution")
             ),
             infoBox(
             title = "Evolution ODD1",
             value = "en %",
             subtitle = "Entre 2006 et 2016",
             icon = icon("line-chart"),
             fill = TRUE,
             color = "light-blue",
             width = 4
             ),
             valueBox(
             value = "+ 38%",
             subtitle = "Evolution sur 5 ans",
             color = "green",
             width = 4
             ),
             tabBox(
             title = "Informations",
             width = 4,
             tabPanel(title = "Prix médian", "contenu 1"),
             tabPanel(title = "Nombre", "contenu 2")
             )
             ),
                        
             ),
             skin = "red"
             )
 

ui <- 
   dashboardPage(header , siderbar , body )

##Ouvrir application
shinyApp(ui, server)

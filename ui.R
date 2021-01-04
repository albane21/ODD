
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(markdown)
library(tidyverse)
library(hrbrthemes)
library(viridis)
windowsFonts()

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

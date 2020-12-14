
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
        menuItem("Présentation de l'observatoire", tabName = "vue"),
        menuItem("Communes", tabName = "commune_choice", icon = icon("list-ol")),
        div( id = 'sidebar_cr',
             conditionalPanel("input.sidebar === 'commune_choice'",
                               selectizeInput("Liste_Communes",
                                              "Choisi la commune de ton choix", 
                                              choices =  "LIBGEO", 
                                             selected = NULL,  width = "200px",
                                              multiple = T), #,
                              actionButton('btn_build_country_report', 
                                            paste0('Découvre ton bilan'),
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
             tabItem(
             "vue",
             box(
             title = "Vaujours",
             footer = textOutput("présentation_commune"),
             status = "info",
             solidHeader = TRUE,
             width = 18
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
             ),
             skin = "red"
             )
 

ui <- 
   dashboardPage(header , siderbar , body )

##Ouvrir l'application
shinyApp(ui, server)

##--------------Chargement des packages------------------

library(shiny)
library(dplyr)
library(here)
library(tidyverse)
library(shinydashboard)
library(leaflet)
library(sf)
library(shinycustomloader)
library(RColorBrewer)
library(ggplot2)
library(markdown)
library(viridis)
library(shinyBS)
library(tidyr)
library(ggthemes)
library(readxl)

# définition de l'habillage carto

url_tile = "https://wxs.ign.fr/{apikey}/geoportail/wmts?&REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER={layer}&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}"
opt_tile <- providerTileOptions(
  apikey= 'pratique', # choisirgeoportail pratique
  layer= 'ORTHOIMAGERY.ORTHOPHOTOS', # GEOGRAPHICALGRIDSYSTEMS.MAPS ORTHOIMAGERY.ORTHOPHOTOS /  GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2
  minZoom = 0,
  maxZoom = 18,
  attribution = "IGN-F/Geoportail",
  tileSize = 256) # les tuiles du Géooportail font 256x256px

## Chargement des données ------
# -----------------------------AVEC PARIS -----------------------------------------------------------------
com2020_geom4326<- sf::read_sf(dsn ="./data/l_commune_epci_ept_bdt_s_r11_2020_geom4326.gpkg")
bdd_odd_paris <- read.csv(file = "./data/BDD_ODD_Paris.csv",sep=";",dec=",",fileEncoding = "UTF-8")
bdd_odd_paris_niveau <- read.csv(file = "./data/BDD_ODD_Paris_niveau.csv",sep=";",dec=",",fileEncoding = "UTF-8")

#conversion en caractere le champ de jointure
bdd_odd_paris<- mutate(bdd_odd_paris,codgeo = as.character(codgeo))

#apppariement des données attributaires avec les objets géographiques
odd_geom4326<-com2020_geom4326%>%inner_join(bdd_odd_paris,by=c("code_insee"="codgeo"))

#--------------------------- AVEC ARRONDISSEMENT----------------------------------------------------------
bdd_odd_arrdt<- read.csv(file = "./data/BDD_ODD_Arrondissements.csv",sep=";",dec=",",fileEncoding = "UTF-8")
com2020_arrdt_geom4326<- sf::read_sf(dsn ="./data/l_commune_arrdt_epci_ept_bdt_s_r11_2020_geom4326.gpkg")
bdd_odd_arrdt_niveau <- read.csv(file = "./data/BDD_ODD_arrdt_niveau.csv",sep=";",dec=",",fileEncoding = "UTF-8")

#conversion en caractere le champ de jointure
bdd_odd_arrdt<- mutate(bdd_odd_arrdt,codgeo = as.character(codgeo))
#apppariement des données attributaires avec les objets géographiques
odd_arrdt_geom4326<-com2020_arrdt_geom4326%>%inner_join(bdd_odd_arrdt,by=c("code_insee"="codgeo"))

# ----------------------------HABILLAGE-----------------------------------------------------------------------
dep_geom4326<- sf::read_sf(dsn ="./data/l_dep_bdt_s_r11_geom4326.gpkg")

#----------------BDD_GRAPH------------------------------#
bdd_graph <- read.csv("./data/bdd_graph.csv",sep=";",dec=",",fileEncoding = "UTF-8")

#+++++++++++++++++++ Define UI for application that draws a histogram++++++++++++++++++++++++++++++++++++++++++
ui <- tagList(
  navbarPage(
    theme = "cerulean", 
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
                          h5("-La densité humaine de la commune"),
                          h5("-Le revenu médian des ménages de la commune")
                 )
               )
             )
    ),
    
    tabPanel("Choix de l'objectif de développement durable",
             h4("Veuillez sélectionner un objectif de développement durable, puis l'indicateur que souhaitez observer."),
             checkboxInput("arrdt", "Choix indicateurs avec données arrondissements", FALSE),
             #selectInput("ind","SELECTIONNER UN INDICATEUR",choices=setNames(c(colnames(odd_geom4326)[6:41]),c('Logement suroccupé en 2017','Logement suroccupé en 2016','licenciés sportifs en 2016','Non diplomé en 2017','Non diplomé en 2012','Non diplomé en 2007',colnames(odd_geom4326)[12:41]))),
             #selectInput("ind1","SELECTIONNER UN INDICATEUR",choices=setNames(bdd_odd_paris_niveau$indic,
             #paste("ODD -",bdd_odd_paris_niveau$indic,seq="")),selected = 'ODD - 1.2'),
             
             selectInput("ind1","Sélection de l'indicateur",choices=""),
             selectInput("ind","Sélection de la sous-catégorie ",choices=""),
             #selectInput("ind","SELECTIONNER LA ndic<-(reactive",choices=""),
             verbatimTextOutput('name'),
             verbatimTextOutput('source'),
             
             # Show a plot of the generated distribution
             actionButton("reg", "Revenir à la région", icon = icon("search-minus")),
             withLoader(leafletOutput("carto", height = "500px"),
                        type="html",
                        loader="loader1"),
             #tableOutput('tableau')
             
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
                         choices =  sort(unique(as.character(bdd_graph$libdep)))     
             ),
             selectInput("COM", "Sélection de la commune",  choices = ""
             ),
             selectInput("ODD", "Sélection de l'objectif de développement durable", choices = sort(unique(as.character(bdd_graph$libodd))) 
             ),
             selectInput("Indicateur", "Sélection de l'indicateur", choices = ""    
             ),
             
             mainPanel(
               plotOutput("trendPlot")
             ),
             mainPanel(
               sidebarLayout(position = "right",
                             sidebarPanel("Explication de l'indicateur"),
                             mainPanel(textOutput("Presentation_commune"))
               )
             )
             
             
    )
    
  )
)

#+++++++++++++++++++Define server logic required to draw a histogram++++++++++++++++++++++++++++++++++++++    

server <- function(input, output,session) {
  
  indic<-reactive(input$ind)
  
  
  observeEvent(input$arrdt,{
    updateSelectInput(session,"ind1",
                      
                      choices = if (input$arrdt ==FALSE )
                      {
                        bdd_odd_paris_niveau$indic
                      }else{
                        bdd_odd_arrdt_niveau$indic
                      }
    )
  }) 
  
  
  observeEvent(input$ind1,{
    updateSelectInput(session,"ind",
                      
                      choices = if (input$arrdt ==FALSE )
                      {
                        setNames(
                          bdd_odd_paris_niveau$libvar[bdd_odd_paris_niveau$indic==input$ind1],
                          bdd_odd_paris_niveau$libelle[bdd_odd_paris_niveau$indic==input$ind1])
                      }else{
                        setNames(
                          bdd_odd_arrdt_niveau$libvar[bdd_odd_arrdt_niveau$indic==input$ind1],
                          bdd_odd_arrdt_niveau$libelle[bdd_odd_arrdt_niveau$indic==input$ind1])
                      }
    )
  }) 
  
  
  
  #================================ nom du champ  ====================================
  output$name <- renderText({
    if (input$arrdt ==FALSE) 
    { indic()
    }else{
      paste(indic(),"(avec arrondissements)",seq="")}
  })
  
  # output$value <- renderText({ input$arrdt })
  
  #================================  sources  ==============================================
  output$source <- renderText({'source: BDTOPO® ©IGN 2020'})
  
  
  #================================     carto   ============================================= 
  output$carto <- renderLeaflet({
    
    if (input$arrdt==FALSE)
    {
      #nom de la couche
      var_odd_geom4326<-odd_geom4326
      
      #libelle de l'indicateur pour PARIS
      libelle<-bdd_odd_paris_niveau$libelle[bdd_odd_paris_niveau$libvar==indic()]
      
      #nom de la variable indicateur concernée en fonction de l'input$
      data_geom4326 <-odd_geom4326%>%pull(indic())
      
      #données de l' indicateur concernée en fonction de l'input$ sans les valeurs 0 (ronds proportionnel) 
      odd_geom4326_sans_0 <- filter(odd_geom4326, data_geom4326 != 0)
      
      #nom de la variable indicateur concernée en fonction de l'input$ sans les valeurs 0 (ronds proportionnel) 
      data_geom4326_0 <-odd_geom4326_sans_0%>%pull(indic())
      
      
      
    }else{
      
      #nom de la couche
      var_odd_geom4326<-odd_arrdt_geom4326
      
      #libelle de l'indicateur pour l'arrondissement
      libelle<-bdd_odd_arrdt_niveau$libelle[bdd_odd_arrdt_niveau$libvar==indic()]
      
      #nom de la variable indicateur concernée en fonction de l'input$
      data_geom4326 <-odd_arrdt_geom4326%>%pull(indic())
      
      #données de l' indicateur concernée en fonction de l'input$ sans les valeurs 0 (ronds proportionnel) 
      odd_geom4326_sans_0 <- filter(odd_arrdt_geom4326, data_geom4326 != 0)
      
      #nom de la variable indicateur concernée en fonction de l'input$ sans les valeurs 0 (ronds proportionnel) 
      data_geom4326_0 <-odd_geom4326_sans_0%>%pull(indic()) 
      
    }
    
    
    
    if ( str_sub(indic(),1,3)%in% c("log","nod","cho","spo","par","pau","id1","gin","par","esp","sat","cyc","mos")
         
         & input$reg>=0)
      
    { 
      
      #---------------------------definition des couleurs ---------------------------
      
      couleur<- case_when(
        #PARIS
        indic() %in% c("logsur17","logsur16") ~'PuRd',
        indic() == "partsport16" ~'Blues',
        indic() %in% c("nodip17","nodip12","nodip07") ~'YlOrRd',
        indic() %in% c("chom1524hf17","chom1524hf12","chom1524hf07") ~'RdBu',
        indic() %in% c("chom1564hf17","chom1564hf12","chom1564hf07") ~'BrBG',
        indic() %in% c("chom5564hf17","chom5564hf12","chom5564hf07") ~'PuOr',
        indic() %in% c("sport014f16","sport1529f16","sport3059f16","sport60plusf16") ~'BuPu',     
        indic() == "partsaless15" ~'Purples',
        indic() %in% c("partvoit17","partvoit12","partvoit07") ~'Reds',
        indic() %in% c("parttec17","parttec12","parttec07") ~'Greens',
        indic() %in% c("partaut17","partaut12","partaut07") ~'YlOrBr',
        #ARRONDISSMENT   
        str_sub(indic(),1,4)=='pauv' ~'YlOrBr',
        str_sub(indic(),1,9)=='partsport' ~'Blues',
        indic() %in% c("id17","id16","id15","id14") ~'PuRd',
        str_sub(indic(),1,4)=='gini' ~'YlGnBu',
        indic() =="partnotrans17"~'OrRd',
        indic() %in% c("parttec17_a","partmarche17","partvelo17","part2roues17","espver17","cyc4km20","cyc8km20","mosf17","mosms17")~'Greens', 
        indic() %in% c("partvoit17_a","satur12") ~'Reds',
        indic()=='mosa17' ~'RdPu',
        indic()=='mosc17' ~'YlOrBr',
        indic()=='mose17' ~'Blues',
        indic()=='moseq17' ~'Purple',
        indic()=='mosea17' ~'YlOrBr',
        indic()=='moseo17' ~'Greys',
        indic()%in%c("moshc17","most17") ~'BuPu',
        indic()=='moshi17' ~'Oranges'
      )
      
      
      #######################definition des bornes ################################################
      
      
      
      #PARIS  
      if (indic() %in% c("logsur17","logsur16"))
      {bins<-c(0,2,4,6,8,10,20,40)
      }else if (indic() =="partsport16")
      {bins<-c(5,10,15,20,25,30,35,40,45,100)   
      }else if (indic()%in% c("nodip17","nodip12","nodip07"))
      {bins<-c(0,5,10,15,20,25,35,50,100)   
      }else if (indic()%in% c("chom1524hf17","chom1524hf12","chom1524hf07"))
      {bins<-c(-100,-30,-20,-15,-10,-5,0,5,10,15,20,30,100) 
      }else if (indic()%in% c("chom1564hf17","chom1564hf12","chom1564hf07"))
      {bins<-c(-100,-5,-4,-3,-2,-1,0,1,2,3,4,5,100)    
      }else if (indic()%in% c("chom5564hf17","chom5564hf12","chom5564hf07"))
      {bins<-c(-100,-5,-4,-3,-2,-1,0,1,2,3,4,5,100)   
      }else if (indic()%in% c("sport014f16","sport1529f16","sport3059f16","sport60plusf16"))
      {bins<-c(0,20,25,30,35,40,45,50,55,60,100)
      }else if (indic() =="partsaless15")
      {bins<-c(0,1,2,3,4,5,6,7,8,9,10,20,100)
      }else if (indic()%in% c("partvoit17","partvoit12","partvoit07"))
      {bins<-c(0,50,55,60,65,70,75,80,85,90,100) 
      }else if (indic()%in% c("parttec17","parttec12","parttec07"))
      {bins<-c(0,2,4,6,8,10,12,14,16,18,20,30,100)  
      }else if (indic()%in% c("partaut17","partaut12","partaut07"))
      {bins<-c(0,2,4,6,8,10,12,14,16,18,20,30,100) 
      #ARRONDISSMENT  
      }else if (str_sub(indic(),1,4)=='pauv')                      
      {bins<-c(0,5,10,15,20,25,30,35,40,45,50,100)  
      }else if (str_sub(indic(),1,9)=='partsport')                      
      {bins<-c(5,10,15,20,25,30,35,40,45,100)
      }else if (indic() %in% c("id17","id16","id15","id14"))                      
      {bins<-c(0,2,2.5,3,3.5,4,4.5,5,5.5,6,10)
      }else if (str_sub(indic(),1,4)=='gini')                      
      {bins<-c(0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40,1)
      }else if (indic() %in% c("partnotrans17","partmarche17"))                     
      {bins<-c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.5)
      }else if (indic() %in% c("partvelo17","part2roues17"))                     
      {bins<-c(0,0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.2)
      }else if (indic() %in% c("partvoit17_a","mosea17"))                     
      {bins<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
      }else if (indic()%in%c("parttec17_a","mosf17","moshc17","moshi17"))                    
      {bins<-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,1)
      }else if (indic()=="espver17")                    
      {bins<-c(0,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)
      }else if (indic()=="satur12")                    
      {bins<-c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,1)
      }else if (indic() %in% c("cyc4km20","cyc8km20"))                     
      {bins<-c(0,1,2,3,4,5,6,7,8,9,10,20)
      }else if (indic()%in% c("mosa17","mosms17","most17"))                    
      {bins<-c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.5)
      }else if (indic() %in% c("mosc17","mose17","moseq17"))                    
      {bins<-c(0,0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.5)
      }else if (indic()=="moseo17")                    
      {bins<-c(0,0.02,0.04,0.06,0.08,0.01,0.12,0.14,0.16,0.18,0.2,0.5)
      }
      
      
      # inverse la palette de couleur pour la catégorie chomage
      if (str_sub(indic(),1,3)=="cho") 
      { inv =TRUE
      }else{inv=FALSE}
      
      
      # definition de la palette de couleur
      pal <- colorBin(couleur, domain =data_geom4326, bins = bins,reverse =inv)
      
      
      leaflet(var_odd_geom4326, options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles("Esri.WorldImagery")%>%
        addTiles(group = "OpenStreetMap") %>%  #Add default OpenStreetMap map tiles
        setView(lng = 2.342,lat = 48.756, zoom = 9.47) %>%
        addMarkers(runif(20, 41, 42),runif(20, -75, -74)) %>%
        addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),overlayGroups = libelle)%>%
        addMarkers(data = var_odd_geom4326,
                   lng=~x, lat=~y,
                   label = paste(var_odd_geom4326$libgeo,": ",as.numeric(data_geom4326),seq=""),
                   options = markerOptions(opacity=0))%>%
        addPolygons(data = var_odd_geom4326,
                    #lng=~x, lat=~y,
                    fillColor =~pal(as.numeric(data_geom4326)),
                    weight = 1,
                    smoothFactor = 1,
                    opacity = 0.5,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    group =  libelle
        )  %>%
        addPolygons(data = dep_geom4326,
                    weight = 1.3,
                    smoothFactor = 1,
                    opacity = 0.8,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0)%>%
        addLegend(pal = pal, values = ~as.numeric(data_geom4326), opacity = 0.7, title = NULL,
                  position = "bottomright")
      
      
    }else{
      
      
      
      #######################definition des couleurs des cercles propotionnels ################################################
      couleur_c<- case_when(
        #PARIS
        indic() == "empass15" ~rgb(187, 11, 11, maxColorValue = 255),#cerise,
        indic() == "empcoop15" ~rgb(231, 62, 1, maxColorValue = 255), #corail approche le orange,
        indic() == "empfond15" ~rgb(173, 79, 9, maxColorValue = 255), #jaune d'or
        indic() == "empmut15" ~rgb(27, 79, 8, maxColorValue = 255), #vert foncé 
        indic() == "ecoent16" ~'purple',
        indic() == "ecoent11" ~'blue',
        indic() == "ecoent06" ~'green',
        #ARRONDISSMENT
        indic() == "consonrj17" ~rgb(34, 66, 124, maxColorValue = 255),
        indic() == "consonrj15" ~rgb(23, 101, 125, maxColorValue = 255),
        indic() == "consonrj12" ~rgb(53, 122, 183, maxColorValue = 255),
        indic() == "consonrj10" ~rgb(53, 151, 151, maxColorValue = 255),
        indic() == "consonrj05" ~rgb(43, 151, 90, maxColorValue = 255),
        indic()=='ges17'~rgb(47, 30, 14, maxColorValue = 255),
        indic()=='ges15'~rgb(237, 0, 0, maxColorValue = 255),
        indic()=='ges12'~rgb(204, 85, 0, maxColorValue = 255),
        indic()=='ges10'~rgb(243, 214, 23, maxColorValue = 255),
        indic()=='ges05'~rgb(0, 204, 203, maxColorValue = 255)
      )
      
      
      taille_c<- case_when(
        #PARIS
        indic() == "empass15" ~'0.5',
        indic()%in% c("empcoop15","empfond15","empmut15","ecoent16","ecoent11","ecoent06")  ~'1',
        str_sub(indic(),1,8)=='consonrj'~'0.01',
        str_sub(indic(),1,3)=='ges'~'1'
      )
      
      
      
      
      
      leaflet(odd_geom4326_sans_0, options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles("Esri.WorldImagery")%>%
        addTiles(group = "OpenStreetMap") %>%  #Add default OpenStreetMap map tiles
        setView(lng = 2.342,lat = 48.756, zoom = 9.47) %>%
        addMarkers(runif(20, 41, 42),runif(20, -75, -74)) %>%
        addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),overlayGroups = libelle)%>%
        addPolygons(data = dep_geom4326,
                    weight = 1,
                    smoothFactor = 1,
                    opacity = 0.8,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.5)%>%
        addPolygons(data = odd_geom4326,
                    weight = 0.5,
                    smoothFactor = 1,
                    opacity = 0.8,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0)%>%
        addPolygons(data = dep_geom4326,
                    weight = 1.3,
                    smoothFactor = 1,
                    opacity = 0.8,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0)%>%
        addCircleMarkers(data = odd_geom4326_sans_0,
                         label = paste(odd_geom4326_sans_0$libgeo,": ",as.numeric(data_geom4326_0),seq=""),
                         lng=~x, lat=~y, 
                         weight = 1,
                         opacity = 1,
                         radius = ~sqrt(as.numeric(data_geom4326_0)/pi) * as.numeric(taille_c),
                         fillOpacity = 0.7,
                         fill = TRUE,
                         stroke=TRUE,
                         popup=~as.numeric(data_geom4326_0),
                         color="white",
                         fillColor =couleur_c,
                         group=libelle
        )
      #%>%
      #addLegend(pal = pal, values = ~as.numeric(var()), opacity = 0.7, title = NULL,
      #position = "bottomright")
      
      
    }
    
    
  })   
  
  #====================================== Sélection indicateur graphiques =======================================
  
  
  observeEvent(input$DEP,{
    updateSelectInput(session,"COM",
                      
                      choices = bdd_graph$libgeo[bdd_graph$libdep==input$DEP]
    )
  }) 
  
  observeEvent(input$ODD,{
    updateSelectInput(session,"Indicateur",
                      
                      choices = bdd_graph$libelle_non_annees[bdd_graph$libodd==input$ODD]
    )
  }) 
  
  ##======================================== Graphique commune =====================================================   
  
  
  filtered_data <- reactive({
    bdd_graph_1 <- bdd_graph   
    bdd_graph_1 <- filter(bdd_graph_1, libelle_non_annees==input$Indicateur)
    bdd_graph_1 <- filter(bdd_graph_1, libgeo == input$COM)    
  })   
  
  filtered_data_com <- reactive({
    bdd_graph_2 <- bdd_graph   
    bdd_graph_2 <- filter(bdd_graph_2, libelle_non_annees==input$Indicateur)
  })   
  
  output$trendPlot <- renderPlot({
    graph_title  <- paste(input$COM, input$Indicateur,  sep=": ")
    G = ggplot(filtered_data_com(), aes(x = annee, y = valeur)) +
      geom_line(data=subset(filtered_data_com(),libgeo==input$COM), size= 2, alpha=0.8, color="blue",stat="identity") +
      geom_point(data=subset(filtered_data_com(),libgeo==input$COM),size = 3, alpha = 0.8, color="blue",stat="identity") +
    
  })
  
}
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)





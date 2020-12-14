
 server = function(input, output) {
    output$evolution <- renderPlot({
      ggplot(X18_25_non_insere, aes(YEAR, PART)) +
        geom_line(data=subset(X18_25_non_insere,LIBGEO=="Groupe 1"), color="purple",stat="identity") +
        geom_line(data=subset(X18_25_non_insere,LIBGEO=="IDF"), color="blue",stat="identity") +
        geom_line(data=subset(X18_25_non_insere,LIBGEO=="Vaujours"), color="yellow",stat="identity") +
        geom_point(data=subset(X18_25_non_insere,LIBGEO=="Groupe 1"),stat="identity") +
        geom_point(data=subset(X18_25_non_insere,LIBGEO=="IDF"))+
        geom_point(data=subset(X18_25_non_insere,LIBGEO=="Vaujours"))+
        labs(x = "", y = "Part des 18-25 ans non insérés")
    })
    output$presentation_commune <- renderText({paste("Nombre d'habitants: 7 0300 (2017)")    })
    output$barplot <- renderPlot({
      ggplot(trying, mapping = aes(x=YEAR, y=PART, fill=LIBGEO)) +
        geom_bar(stat="identity", position = "dodge") +
        scale_fill_viridis(discrete=TRUE, name="")+
        theme_ipsum()+
        labs(x = "Année",y = "Part des 18-25 ans non insérés")
    })
  }

library(shiny)
library(tidyverse)
library(jsonlite)
library(shinythemes)
library(lubridate)
# library(r2d3)
library(dplyr)
library(plotly)
library(png)
library(ggplot2)

# spotidane <- as_data_frame(read_csv("./dane.csv"))
# spotidane$msPlayed <- spotidane$msPlayed/(1000*3600)
# spotidane$endTime <- as.character(spotidane$endTime)
# spotidane$endTime <- fast_strptime(spotidane$endTime, "%Y-%m-%d %H:%M:%S",tz="UTC")
# spotidane$endTime <- as.POSIXct(spotidane$endTime)
# x1<- spotidane %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% arrange(desc(time)) %>% slice(1:10)
# choices <- unique(x1$artistName)

clickme <- readPNG('clickme.png')


ui <- fluidPage(theme = shinytheme("superhero"),
                
                sidebarLayout(
                    sidebarPanel(width=4,
                                 titlePanel("SpotiData - sprawdź swoje statystyki słuchania", windowTitle = "SpotiData"),
                                 hr(),
                                 actionButton("wiosna", "Wiosna", width = "110px"),
                                 actionButton("lato", "Lato", width = "110px"),
                                 actionButton("jesien", "Jesień", width = "110px"),
                                 actionButton("zima", "Zima", width = "110px"),
                                 actionButton("resetdat", "Reset dat", width = "220px"),
                                 hr(),
                                 dateRangeInput("daterange1", "Zakres dat:",
                                                start = "2018-12-01",
                                                end   = "2020-01-31",
                                                language = "pl",
                                                weekstart = 1,
                                                separator = " do "),
                                 hr(),
                                 fileInput('files', 'Załaduj swoje dane:', multiple = TRUE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv", ".json"),
                                           buttonLabel = "Twoje pliki:",
                                           placeholder = "Brak pliku",
                                           width = "220px"),
                                 tags$script('
                                     pressedKeyCount = 0;
                                     $(document).on("keydown", function (e) {
                                      Shiny.onInputChange("pressedKey", pressedKeyCount++);
                                      Shiny.onInputChange("key", e.which);
                                      });'),
                                 hr(),
                                htmlOutput("Opis")
                                                     ),
                    mainPanel(
                        plotOutput("distPlot",click="click", height = "672px")
                    )
                )
)

server <- function(input, output, session) {
    
  
    selected_spotidane <- reactiveValues(    #po prostu zbior wartosci reaktywnych - nie sugeruj sie nazwa
        selected = character(),     #nazwa artysty zebrana przez klikniecie na pierwszym wykresie
        x1  = data_frame(),      #ramka danych tworzona do przedstawienia pierwszego wykresu
        choices  = data_frame(),  #zbior wszystkich artystow, ktorzy byli wyswietlani 
        clicked = numeric(),   #potrzebne do sczytania wspolrzednej y klikniecia mysza na pierwszym wykresie
        click = FALSE, # flaga potrzebna do klikania i odklikiwania artystow - wazna!! decyduje ktory wykres sie wyswietla 
        comeback_possible = FALSE, # do powrotu z drugiego wykresu
        maxvalue = numeric(), #tez do powrotu - okresla polozenie guzika
        begin_date = date("2019-01-01"),   #chyba jasne - paczatkowa data zakresu
        end_date = date("2019-12-31"),   #koncowa
        arrow_index = 0,   #wylicza jaka wartosc nalezy dodać do '1:20' aby byli wyswieltani artysci z zakresu 1+array_index:20+array_index
        keep_range = TRUE,  #czy trzymac zakres skali na pierwszym wykresie
        get_range = TRUE, # czy pobrac zakres skali z pierwszego wykresu
        max_value = numeric(),#potrzebne do okreslenia rozpietosci pierwszego wykresu
        twentieth = numeric(), # do okreslenia na jakiej wysokosci ma byc obrazek
        first_plot = TRUE, #flaga uzyta tylko przy pierwszym wczytywaniu wykresu
        x = FALSE,  #za duzo tych flag, ale dziala - potrzebne bo przy pierwszym wczytaniu plikow zmienia sie daterange, co utrudnia
        window = c(FALSE, TRUE, FALSE), #ktore okno na dole ma sie wyswietlic?
        pawelplot = data.frame() #potrzebne do mojego wykresu
        
    )
    
    spotidane <-reactiveValues(  #tez nie sugerowac sie nazwa: reaktywne tylko ze inne 
        data = data.frame(),   #ramka danych zawierajaca informacje z wczytanych plikow
        toBind = data.frame()  #tymczasowa ramka wykorzystywana przy wczytywaniu danych
    )
    
    #### obserwatorzy inputow
    observeEvent(input$zima, {
        selected_spotidane$begin_date <- date(format(date("2019-01-01"),"%Y-%m-%d")) 
        selected_spotidane$end_date <- date(format(date("2019-03-31"),"%Y-%m-%d"))
        updateDateRangeInput(session, "daterange1", start =  date("2019-01-01"), end = date("2019-03-31"))
        
    })
    observeEvent(input$wiosna, {
        selected_spotidane$begin_date <- date("2019-04-01") 
        selected_spotidane$end_date <- date("2019-06-30")
        updateDateRangeInput(session, "daterange1", start =  date("2019-04-01"), end = date("2019-06-30"))
    })
    observeEvent(input$lato, {
        selected_spotidane$begin_date <- date(format(date("2019-07-01"),"%Y-%m-%d")) 
        selected_spotidane$end_date <- date(format(date("2019-09-30"),"%Y-%m-%d"))
        updateDateRangeInput(session, "daterange1", start =  date("2019-06-01"), end = date("2019-09-30"))
    })
    observeEvent(input$jesien, {
        selected_spotidane$begin_date <- date("2019-10-01") 
        selected_spotidane$end_date <- date("2019-12-31")
        updateDateRangeInput(session, "daterange1", start =  date("2019-10-01"), end = date("2019-12-31"))
        })
    observeEvent(input$resetdat, {
      selected_spotidane$begin_date <- date("2018-12-01") 
      selected_spotidane$end_date <- date("2020-01-31")
      updateDateRangeInput(session, "daterange1", start =  date("2018-12-01"), end = date("2020-01-31"))
    })
    
    observeEvent(input$daterange1, {
        selected_spotidane$begin_date <- input$daterange1[1]
        selected_spotidane$end_date <- input$daterange1[2]
        selected_spotidane$arrow_index <- 0
        if(!selected_spotidane$x){
          selected_spotidane$x <- TRUE
          }
        else{
          selected_spotidane$get_range <- TRUE
          selected_spotidane$keep_range <- FALSE
          }
        
    })
    observeEvent(input$pressedKey, {
      
        if(!selected_spotidane$click){
      if(input$key %in% c(38, 40)){
        tmp <- selected_spotidane$arrow_index + input$key -39
        if(tmp >=0){
          selected_spotidane$arrow_index <- tmp
          selected_spotidane$first_plot <- FALSE
           selected_spotidane$get_range <- FALSE
          selected_spotidane$keep_range <- TRUE
        }
      }
          
      }
        else{
            if(input$key == 39){ 
            if(which(selected_spotidane$window)==1){
                selected_spotidane$window <- c(FALSE, TRUE, FALSE)
            }
            else if(which(selected_spotidane$window)==2){
                selected_spotidane$window <- c(FALSE, FALSE, TRUE)
            }
            }
            else if(input$key == 37){
                if(which(selected_spotidane$window)==3){
                    selected_spotidane$window <- c(FALSE, TRUE, FALSE)
                }
                else if(which(selected_spotidane$window)==2){
                    cat(2)
                    selected_spotidane$window <- c(TRUE, FALSE, FALSE)
                }
            }
        }
        
      })
    
    observeEvent(input$click,{
        if(!selected_spotidane$click) {#w przypadku gdy wyswietlany jest 1. wykres
            selected_spotidane$click = TRUE
            #zapisanie wykonawcy jaki ma byc wyswietlany w drugim oknie - o ile click$y występuje
            selected_spotidane$clicked[1] <-ifelse(!is.null(input$click$y), input$click$y, selected_spotidane$clicked[1])
        }
        else{#gdy drugi wykres jest wyswietlany, zbieramy klikniecie dla przycisku powróć
            if(which(selected_spotidane$window)==1){
            if(input$click$x>0 && input$click$x<3){
                if(input$click$y<selected_spotidane$maxvalue*1.06 && input$click$y>selected_spotidane$maxvalue*1.04){
                    selected_spotidane$click = FALSE
                    selected_spotidane$window = c(FALSE, TRUE, FALSE) #reset informacji ktory wykres ma byc wyswietlony na drugim poziomie
                }
            }
                
                
            }
            else if(which(selected_spotidane$window)==2){
                
            }
            else if(which(selected_spotidane$window)==3){
                
            }
        }
        
      selected_spotidane$first_plot <- FALSE
    })
    ######
    
    ######
    # logika wczytywania plikow
    #
    observeEvent(input$files,{
        tryCatch(
            {
                spotidane$data <- data.frame()
                selected_spotidane$click = FALSE
                selected_spotidane$window = c(FALSE, TRUE, FALSE)
                for(var in 1:length(input$files$datapath)){
                    if(input$files$type[var] %in% c("text/csv", "application/csv")){
                        spotidane$toBind <-  as.data.frame(as_data_frame(read.csv(input$files$datapath[var])))
                        
                        if(all(colnames(spotidane$toBind) == c("endTime", "artistName", "trackName", "msPlayed"))){
                            spotidane$toBind$endTime <- as.character(spotidane$toBind$endTime)
                            spotidane$toBind$endTime <- substr(spotidane$toBind$endTime, start = 1, stop = nchar(spotidane$toBind$endTime)-3)
                            spotidane$data <- rbind(spotidane$data, spotidane$toBind)
                        }
                    }
                    if(input$files$type[var] %in% c("application/json", "text/json")){
                        spotidane$toBind <- as.data.frame(fromJSON(input$files$datapath[var]))
                        if(all(colnames(spotidane$toBind) == c("endTime", "artistName", "trackName", "msPlayed"))){
                            spotidane$data <- rbind(spotidane$data, spotidane$toBind)
                        }
                    }
                }
                if(nrow(spotidane$data)>0){
                spotidane$data$msPlayed <- spotidane$data$msPlayed/(1000*3600)
                spotidane$data$endTime <- as.character(spotidane$data$endTime)
                #spotidane$data$endTime <- fast_strptime(spotidane$data$endTime, "%Y-%m-%d %H:%M",tz="UTC")
                spotidane$data$endTime <- as.POSIXct(spotidane$data$endTime)
                }
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
    #
    #
    ########
      
    
    #####
    #Wczytywanie obu wykresow - ggplot
      
    output$distPlot <- renderPlot({
        if(!selected_spotidane$click){
            ### gdy nie bylo jeszcze klikniecia - pokazujemy pierwszy wykres
            # wczytanie danych
            selected_spotidane[["x1"]] <- spotidane$data %>%
                filter(endTime >= selected_spotidane$begin_date)%>%
                filter(endTime <= selected_spotidane$end_date)%>%
                group_by(artistName) %>% summarise(uniquesongs = length(unique(trackName)), time = sum(msPlayed)) %>%
                arrange(desc(time)) %>% slice((1:20) + selected_spotidane$arrow_index) %>%
                mutate(artistName = paste0(row_number() + selected_spotidane$arrow_index, ". ", artistName))
            
            if(selected_spotidane$get_range){
              selected_spotidane$max_value = selected_spotidane[["x1"]][["time"]][1] #do trzymania zakresu osi godzin
              selected_spotidane$twentieth = selected_spotidane[["x1"]][["time"]][20]
              cat(selected_spotidane$max_value)
              }
            x <- selected_spotidane[["x1"]]
            selected_spotidane$choices <- selected_spotidane[["x1"]]$artistName
            if(nrow(selected_spotidane[["x1"]])==0 || is.na(selected_spotidane[["x1"]][["artistName"]])) {#jesli nie mamy zadnych danych
                plot.new()
                text(0.5,0.5,"Wybrany zakres dat nie zwrócił żadnych wyników dla danego pliku")
            }
            else{
            p <- ggplot(selected_spotidane[["x1"]], aes(x=reorder(artistName,-time), y=time)) +
                geom_bar(stat="identity", width=.7,fill="#1D428A")+
                theme_minimal()+
                geom_text(aes(label = ifelse(time==selected_spotidane$max_value,paste0("Liczba przesłuchanych różnych utworów wykonawcy: ", uniquesongs), uniquesongs)),
                          size = 4.6, hjust = "top", nudge_y = -0.2, color = "white") +
                theme(axis.text.x = element_text(hjust = 1,size=11),
                      axis.text.y =element_text(size=15))+
                xlab(element_blank())+
                ylab("Liczba przesłuchanych godzin artysty") +
                scale_x_discrete(limits = rev(as.factor(x$artistName)), label = function(t) abbreviate(t, minlength = 20)) +
                coord_flip()
            #cat(selected_spotidane$original_range)
            if(selected_spotidane$first_plot){  #jesli ma sie pojawic obrazek - niech sie pojawi
              z <- p + annotation_raster(clickme, ymin =selected_spotidane$twentieth*1.1,
                                         ymax= selected_spotidane$twentieth*1.1 + selected_spotidane$max_value/3,
                                         xmin =0.7, xmax = 3.7)
            }
            else{z <- p}
            if(selected_spotidane$keep_range){
                z + scale_y_continuous(position = "right", limits = c(0, selected_spotidane$max_value))
            }
            else{
              z + scale_y_continuous(position = "right")
            }
            }
        }
        else{
            #####drugi poziom wykresow (po kliknieciu w pierwszy)
            lvls <- selected_spotidane$choices
            artist <- lvls[21 - round(selected_spotidane$clicked[1])]
            selected_spotidane$selected <- paste(unlist(strsplit(artist, split = " "))[-1], collapse = " ")

            if(which(selected_spotidane$window)==2){
                ### srodkowy wykres
             plot.new()
                text(0.1, 0.2, "Miejsce Filipa")
            }
            ##koniec srodkowego wykresu
            
            ##ten z lewej:
            else if(which(selected_spotidane$window) == 1){
                zwroc_czas <- function(d) {
                    weekday = weekdays(d, abbreviate = TRUE)
                    hour = format(strptime(d,"%Y-%m-%d %H:%M:%S"),'%H')
                    hour = as.numeric(hour) %/% 6 * 6
                    paste0(weekday, " ", hour, ":00")
                }
                y <- spotidane$data %>%
                     filter(endTime >= selected_spotidane$begin_date) %>%
                     filter(endTime <= selected_spotidane$end_date) %>%
                      filter(artistName == selected_spotidane$selected) %>%
                    mutate(pora = zwroc_czas(endTime)) %>%
                    count(pora)
                
                selected_spotidane$maxvalue <- max(y$n)
                
                if(nrow(y)==0) {
                    plot.new()
                    
                    text(0.5,0.5,"Wybrany zakres dat nie zwrócił żadnych wyników dla danego pliku")
                }
                else{
                    dayparts <- paste(rep(weekdays(date("2020-01-20") + 0:6, abbreviate = TRUE), each = 4), c("0:00","6:00", "12:00", "18:00"))
                    lackingtime <-which(!dayparts %in% y[["pora"]])
                    y <- rbind(y, data_frame(pora = dayparts[lackingtime], n = rep(0, length(lackingtime))))
                    ggplot(y, aes(x = factor(pora, dayparts), y = n, group = 1)) +
                        geom_point(size = 3, color = "red") +
                        geom_area(alpha = 0.1, fill = "red")+
                        geom_line(color = "red") + #ponizej - guzik do powrotu do pierwszego wykresu
                        annotate("text", x = 1.5, y =  selected_spotidane$maxvalue*1.05, label = "bold(Powróć)", parse = TRUE)+
                        annotate("segment", x=0, xend = 3, y = selected_spotidane$maxvalue*1.06, yend = selected_spotidane$maxvalue*1.06) +
                        annotate("segment", x=0, xend = 3, y = selected_spotidane$maxvalue*1.04, yend = selected_spotidane$maxvalue*1.04) +
                        annotate("segment", x=0, xend = 0, y = selected_spotidane$maxvalue*1.06, yend = selected_spotidane$maxvalue*1.04) +
                        annotate("segment", x=3, xend = 3, y = selected_spotidane$maxvalue*1.06, yend = selected_spotidane$maxvalue*1.04) +
                        theme_minimal()+
                        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                        xlab(paste0(selected_spotidane$selected, " - słuchanie w ciągu tygodnia")) +
                        ylab("Liczba odtworzeń") +
                        scale_x_discrete(breaks = paste(rep(weekdays(date("2020-01-20") + 0:6, abbreviate = TRUE), each = 2), c("0:00", "12:00")),
                                         limits = dayparts)
                }
                
            }
            ###koniec lewego wykresu
            else if(which(selected_spotidane$window) == 3){
                plot.new()
                text(0.1, 0.9, "Wykres Jacka")
            }
            
        }
    })
    })
    
    output$Opis <- renderUI({
        req(input$files)
        if(!selected_spotidane$click){
        HTML(paste("Użyj strzałek na klawiaturze", "   - sprawdź co się stanie!", sep="<br/>"))
        }
        else{
            HTML(paste("Użyj strzałek na klawiaturze", "   - tym razem na boki!", sep="<br/>"))
        }
    })
}


shinyApp(ui = ui, server = server)



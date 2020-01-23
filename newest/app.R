library(shiny)
library(tidyverse)
library(jsonlite)
library(shinythemes)
library(lubridate)
# library(r2d3)
library(dplyr)
library(plotly)

# spotidane <- as_data_frame(read_csv("./dane.csv"))
# spotidane$msPlayed <- spotidane$msPlayed/(1000*3600)
# spotidane$endTime <- as.character(spotidane$endTime)
# spotidane$endTime <- fast_strptime(spotidane$endTime, "%Y-%m-%d %H:%M:%S",tz="UTC")
# spotidane$endTime <- as.POSIXct(spotidane$endTime)
# x1<- spotidane %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% arrange(desc(time)) %>% slice(1:10)
# choices <- unique(x1$artistName)



ui <- fluidPage(theme = shinytheme("superhero"),
                
                sidebarLayout(
                    sidebarPanel(width=4,
                                 titlePanel("Analiza danych słuchania Spotify"),
                                 
                                 dateRangeInput("daterange1", "Zakres dat:",
                                                start = "2018-12-01",
                                                end   = "2020-01-31",
                                                language = "pl"),
                                 hr(),
                                 
                                 actionButton("zima", "Zima", width = "100px"),
                                 actionButton("wiosna", "Wiosna", width = "100px"),
                                 actionButton("lato", "Lato", width = "100px"),
                                 actionButton("jesien", "Jesień", width = "100px"),
                                 hr(),
                                 fileInput('files', 'Załaduj swoje dane', multiple = TRUE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv", ".json"), width = NULL)
                    ),
                    mainPanel(
                        plotOutput("distPlot",click="click", height = "800px")
                    )
                )
)

server <- function(input, output, session) {
    
  
    selected_spotidane <- reactiveValues(
        selected = character(),
        x1  = data_frame(),
        choices  = data_frame(),
        clicked = numeric(),
        songs = data_frame(),
        click = FALSE, # flaga potrzebna do klikania i odklikiwania artystow
        comeback_possible = FALSE, # do powrotu z drugiego wykresu
        maxvalue = numeric(), #tez do powrotu
        begin_date = date("2019-01-01"),
        end_date = date("2019-12-31")
    )
    
    spotidane <-reactiveValues(
        data = data.frame(),
        toBind = data.frame()
    )
    
    
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
    observeEvent(input$daterange1, {
        selected_spotidane$begin_date <- input$daterange1[1]
        selected_spotidane$end_date <- input$daterange1[2]
    })
    observeEvent(input$click,{
        if(!selected_spotidane$click) {#w przypadku gdy wyswietlany jest 1. wykres
            selected_spotidane$click = TRUE
            #zapisanie wykonawcy jaki ma byc wyswietlany w drugim oknie - o ile click$y występuje
            selected_spotidane$clicked[1] <-ifelse(!is.null(input$click$y), input$click$y, selected_spotidane$clicked[1])
        }
        else{#gdy drugi wykres jest wyswietlany, zbieramy klikniecie dla przycisku powróć
            if(input$click$x>1.2 && input$click$x<1.8){
                if(input$click$y<selected_spotidane$maxvalue*1.06 && input$click$y>selected_spotidane$maxvalue*1.04){
                    selected_spotidane$click = FALSE
                }
                
            }
        }
    })
    
    ######
    # logika wczytywaniu plikow
    #
    observeEvent(input$files,{
        # req(input$files)
        
        tryCatch(
            {
                spotidane$data <- data.frame()
                selected_spotidane$click = FALSE
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
                spotidane$data$msPlayed <- spotidane$data$msPlayed/(1000*3600)
                spotidane$data$endTime <- as.character(spotidane$data$endTime)
                spotidane$data$endTime <- fast_strptime(spotidane$data$endTime, "%Y-%m-%d %H:%M",tz="UTC")
                spotidane$data$endTime <- as.POSIXct(spotidane$data$endTime)
                
                
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
        
       # req(input$files)
        
        #updateDateRangeInput(session, "daterange1", start =  selected_spotidane$begin_date, end = selected_spotidane$end_date)
        
        if(!selected_spotidane$click){
            #cat(paste(selected_spotidane$begin_date, selected_spotidane$end_date, " "))
            selected_spotidane[["x1"]] <- spotidane$data %>%
                filter(endTime >= selected_spotidane$begin_date)%>%
                filter(endTime <= selected_spotidane$end_date)%>%
                group_by(artistName) %>% summarise(uniquesongs = length(unique(trackName)), time = sum(msPlayed)) %>%
                arrange(desc(time)) %>% slice(1:20)
            x <- selected_spotidane[["x1"]]
            selected_spotidane$choices <- selected_spotidane[["x1"]]$artistName
            if(nrow(selected_spotidane[["x1"]])==0 || is.na(selected_spotidane[["x1"]][["artistName"]])) {#jesli nie mamy zadnych danych
                plot.new()
                text(0.5,0.5,"Wybrany zakres dat nie zwrócił żadnych wyników dla danego pliku")
            }
            else{
            ggplot(selected_spotidane[["x1"]], aes(x=reorder(artistName,-time), y=time)) +
                geom_bar(stat="identity", width=.7,fill="#1D428A")+
                theme_bw()+
                geom_text(aes(label = ifelse(time==max(time),paste0("Liczba przesłuchanych różnych utworów wykonawcy: ", uniquesongs), uniquesongs)),
                          size = 4.6, hjust = "top", nudge_y = -0.2, color = "white") +
                theme(axis.text.x = element_text(hjust = 1,size=11),
                      axis.text.y =element_text(size=15))+
                xlab(element_blank())+
                ylab("Liczba przesłuchanych godzin ") +
                scale_y_continuous(position = "right") +
                scale_x_discrete(limits = rev(as.factor(x$artistName)), label = function(t) abbreviate(t, minlength = 23)) +
                coord_flip()
                
            }
        }
        else{
            #wykres po kliknieciu
            
            lvls <- selected_spotidane$choices
            selected_spotidane$selected <- lvls[21 - round(selected_spotidane$clicked[1])]
            
            zwrocliteregodzine <- function(d) {
                x = weekdays(date(format(strptime(d,"%Y-%m-%d %H:%M"),'%Y-%m-%d')))
                litera = ifelse(x=="poniedziałek", "a",
                                ifelse(x=="wtorek", "b",
                                       ifelse(x=="środa", "c",
                                              ifelse(x=="czwartek", "d",
                                                     ifelse(x=="piątek", "e",
                                                            ifelse(x=="sobota", "f","g" ))))))
                hour = format(strptime(d,"%Y-%m-%d %H:%M"),'%H')
                minute = format(strptime(d,"%Y-%m-%d %H:%M"),'%M')
                minute = as.numeric(minute) %/% 30 * 30
                paste0(litera, " ", hour, ":", minute)
            }
            y <- spotidane$data %>%
                filter(endTime >= selected_spotidane$begin_date)%>%
                filter(endTime <= selected_spotidane$end_date)%>%
                filter(artistName == selected_spotidane$selected) %>%
                mutate(weekday = weekdays(endTime)) %>%
                mutate(real = zwrocliteregodzine(endTime)) %>%
                count(weekday)
            
            selected_spotidane$maxvalue = max(y$n)
            
            if(nrow(y)==0) {
                plot.new()
                
                text(0.5,0.5,"Wybrany zakres dat nie zwrócił żadnych wyników dla danego pliku")
            }
            else{
            ggplot(y, aes(x = factor(weekday, weekdays(date("2020-01-20") + 0:6)), y = n, group = 1)) +
                geom_point( size = 5, color = "red") +
                geom_line(color = "red") + #ponizej - guzik do powrotu do pierwszego wykresu
                annotate("text", x = 1.5, y =  selected_spotidane$maxvalue*1.05, label = "bold(Powróć)", parse = TRUE)+
                annotate("segment", x=1.2, xend = 1.8, y = selected_spotidane$maxvalue*1.06, yend = selected_spotidane$maxvalue*1.06) +
                annotate("segment", x=1.2, xend = 1.8, y = selected_spotidane$maxvalue*1.04, yend = selected_spotidane$maxvalue*1.04) +
                annotate("segment", x=1.2, xend = 1.2, y = selected_spotidane$maxvalue*1.06, yend = selected_spotidane$maxvalue*1.04) +
                annotate("segment", x=1.8, xend = 1.8, y = selected_spotidane$maxvalue*1.06, yend = selected_spotidane$maxvalue*1.04) +
                theme_minimal()+
                xlab(paste0(selected_spotidane$selected, " - liczba odsłuchań w dniu tygodnia")) +
                ylab("Ilość odtworzeń")
                }
        }
    })
    })
}


shinyApp(ui = ui, server = server)



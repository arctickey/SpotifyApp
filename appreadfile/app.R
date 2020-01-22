library(shiny)
library(tidyverse)
library(jsonlite)
library(shinythemes)
library(lubridate)
library(r2d3)
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
                                 titlePanel("Time played"),
                                 
                                dateRangeInput("daterange1", "Date range:",
                                     start = "2018-12-01",
                                     end   = "2020-01-31",
                                     language = "pl"),
                                 hr(),
                                
                                 actionButton("zima", "Winter", width = "100px"),
                                 actionButton("wiosna", "Spring", width = "100px"),
                                 actionButton("lato", "Summer", width = "100px"),
                                 actionButton("jesien", "Autumn", width = "100px"),
                                 hr(),
                                 fileInput('files', 'Upload your data', multiple = TRUE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv", ".json"), width = NULL),
                                 actionButton("button", "Return to all artists")
                                 # guzik do wracania z artysty do calosci
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
        begin_date = date("2019-01-01"),
        end_date = date("2019-12-31")
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
        if(!selected_spotidane$click) {selected_spotidane$click = TRUE}

            })
    observeEvent(input$button,{
        if(selected_spotidane$click) {selected_spotidane$click = FALSE}
    })


output$distPlot <- renderPlot({
        
        req(input$files)

tryCatch(
    {
            spotidane <- data.frame()
        for(var in 1:length(input$files$datapath)){
            
                    
        if(input$files$type[var] %in% c("text/csv", "application/csv")){
            toBind <-  as.data.frame(as_data_frame(read.csv(input$files$datapath[var])))
            
            if(all(colnames(toBind) == c("endTime", "artistName", "trackName", "msPlayed"))){
                toBind$endTime <- as.character(toBind$endTime)
                toBind$endTime <- substr(toBind$endTime, start = 1, stop = nchar(toBind$endTime)-3)
                spotidane <- rbind(spotidane, toBind)
            }
        }
        if(input$files$type[var] %in% c("application/json", "text/json")){
            toBind <- as.data.frame(fromJSON(input$files$datapath[var]))
            if(all(colnames(toBind) == c("endTime", "artistName", "trackName", "msPlayed"))){
                spotidane <- rbind(spotidane, toBind)
            }
            }
        }
        spotidane$msPlayed <- spotidane$msPlayed/(1000*3600)
        spotidane$endTime <- as.character(spotidane$endTime)
        spotidane$endTime <- fast_strptime(spotidane$endTime, "%Y-%m-%d %H:%M",tz="UTC")
        spotidane$endTime <- as.POSIXct(spotidane$endTime)

        },
        error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
    }
)
        
        #updateDateRangeInput(session, "daterange1", start =  selected_spotidane$begin_date, end = selected_spotidane$end_date)
        
        if(!selected_spotidane$click){
        #cat(paste(selected_spotidane$begin_date, selected_spotidane$end_date, " "))
        x <- spotidane %>%
            filter(endTime >= selected_spotidane$begin_date)%>%
            filter(endTime <= selected_spotidane$end_date)%>%
            group_by(artistName) %>% summarise(uniquesongs = length(unique(trackName)), time = sum(msPlayed)) %>%
            arrange(desc(time)) %>% slice(1:20)


        selected_spotidane$choices <- x$artistName
        ggplot(x, aes(x=reorder(artistName,-time), y=time)) +
            geom_bar(stat="identity", width=.7,fill="#1D428A")+
            theme_minimal()+
            geom_text(aes(label = ifelse(time==max(time),paste0("Liczba przesłuchanych różnych utworów wykonawcy: ", uniquesongs), uniquesongs)),
                      size = 4.6, hjust = "top", nudge_y = -0.2, color = "white") +
            theme(axis.text.x = element_text(hjust = 1,size=11),
                  axis.text.y =element_text(size=15))+
            xlab(element_blank())+
            ylab("Hours") +
            scale_y_continuous(position = "right") +
            scale_x_discrete(limits = rev(as.factor(x$artistName)), label = function(t) abbreviate(t, minlength = 23)) +
            coord_flip()
        }
        else{
            #wykres po kliknieciu
         selected_spotidane$clicked[1] <-ifelse(!is.null(input$click$y), input$click$y, selected_spotidane$clicked[1])

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
        y <- spotidane %>%
            filter(endTime >= selected_spotidane$begin_date)%>%
            filter(endTime <= selected_spotidane$end_date)%>%
            filter(artistName == selected_spotidane$selected) %>%
            mutate(weekday = weekdays(endTime)) %>%
            mutate(real = zwrocliteregodzine(endTime))
            
            # mutate(hour = {
            #     hours = format(strptime(endTime,"%Y-%m-%d %H:%M"),'%H')
            #     minutes = as.character(as.numeric(format(strptime(endTime,"%Y-%m-%d %H:%M"),'%M')) %/% 30 * 30)
            #     minutes = ifelse(nchar(minutes)==1, paste0(0, minutes), minutes)
            #     paste(weekday, paste(hours, minutes, sep = ":"))
            # }) %>%
           # arrange(real)

        ggplot(y, aes(x = factor(weekday, weekdays(date("2020-01-20") + 0:6)))) +
            geom_point(stat = "count") +
           geom_line(stat= "count") +
            theme_minimal()+
            xlab(selected_spotidane$selected)

      
            
        }
    })
}


shinyApp(ui = ui, server = server)


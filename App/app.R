library(shiny)
library(tidyverse)
library(rjson)
library(shinythemes)
library(lubridate)
library(r2d3)

data <- as_data_frame(read_csv("./dane.csv"))
data$msPlayed <- data$msPlayed/(1000*3600)
data$endTime <- as.character(data$endTime)
data$endTime <- fast_strptime(data$endTime, "%Y-%m-%d %H:%M:%S",tz="UTC")
data$endTime <- as.POSIXct(data$endTime)
x1<- data %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% arrange(desc(time)) %>% slice(1:10)
choices <- unique(x1$artistName)




ui <- fluidPage(theme = shinytheme("superhero"),
                
                sidebarLayout(
                    sidebarPanel(width=4,
                                 titlePanel("Time played"),
                                 
                                 dateInput('date',
                                           label = 'First Available Date',
                                           value = min(data$endTime)
                                 )   ,
                                 dateInput('date1',
                                           label = 'Last available Date',
                                           value = max(data$endTime)
                                           
                                 ),
                                 actionButton("button", "Return to all artists")
                                 # guzik do wracania z artysty do calosci
                    ),
                    
                    mainPanel(
                        plotOutput("distPlot",click="click")
                    )
                )
)

server <- function(input, output) {
    
    selected_data <- reactiveValues(
        selected = data_frame(),
        x1  = data_frame(),
        choices  = data_frame(),
        songs = data_frame(),
        click = FALSE # flaga potrzebna do klikania i odklikiwania artystow
    )
    
    observeEvent({input$date
        input$date1},{
            selected_data$selected<- data %>% filter(endTime>= input$date, endTime <= input$date1)
            selected_data$selected <- as.data.frame(selected_data$selected)
            selected_data$x1<- selected_data$selected %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% 
                arrange(desc(time)) %>% slice(1:10)
            selected_data$choices <- unique(selected_data$x1$artistName)
        })
    
    # rysuje wykres dla konkretnych artystow
    observeEvent(input$click, {
        if(!selected_data$click) # sprawdzam czy juz artysta jest wybrany czy jeszcze nie
        {
            selected_data$click = TRUE
            lvls <- selected_data$choices
            name <- lvls[round(input$click$x)]
            output$distPlot <- renderPlot({
                
                y <- selected_data$selected %>% filter(artistName == name) %>%
                    group_by(song = trackName) %>% 
                    summarise(time = sum(msPlayed)) %>% 
                    arrange(desc(time)) %>% slice(1:10)
                
                selected_data$songs <- y
                ggplot(y, aes(x=reorder(song,-time), y=time)) + 
                    geom_bar(stat="identity", width=.5,fill="#1D428A")+theme_minimal()+
                    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y =element_text(size=15))+
                    xlab(element_blank())+ylab("Hours")
            })
        }
    })
    
    # rysuje wykres dla wszystkich artystow jezeli uzytkownik kliknie przycisk
    observeEvent(input$button, {
        selected_data$click = FALSE
        output$distPlot <- renderPlot({
            
            x <- selected_data$selected %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% 
                arrange(desc(time)) %>% slice(1:10)
            
            ggplot(x, aes(x=reorder(artistName,-time), y=time)) + 
                geom_bar(stat="identity", width=.5,fill="#1D428A")+theme_minimal()+
                theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y =element_text(size=15))+
                xlab(element_blank())+ylab("Hours")
        })
    })
    
    # zanim klikniemy przycisk albo wybierzemy artyste rysuje wszystkich artystow
    output$distPlot <- renderPlot({
        
        x <- selected_data$selected %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% 
            arrange(desc(time)) %>% slice(1:10)
        
        ggplot(x, aes(x=reorder(artistName,-time), y=time)) + 
            geom_bar(stat="identity", width=.5,fill="#1D428A")+theme_minimal()+
            theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y =element_text(size=15))+
            xlab(element_blank())+ylab("Hours")
    })
}


shinyApp(ui = ui, server = server)

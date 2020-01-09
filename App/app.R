
library(shiny)

library(tidyverse)
library(rjson)
library(lubridate)
library(r2d3)

data <- as_data_frame(read_csv("./dane.csv"))
data$msPlayed <- data$msPlayed/(1000*3600)
data$endTime <- as.character(data$endTime)
data$endTime <- fast_strptime(data$endTime, "%Y-%m-%d %H:%M:%S",tz="UTC")
data$endTime <- as.POSIXct(data$endTime)
x <- data %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% arrange(desc(time)) %>% slice(1:10)
ui <- fluidPage(
    
    titlePanel("Old Faithful Geyser Data"),
    
    sidebarLayout(
        sidebarPanel(width=4,
                dateInput('date',
                          label = 'First Available Date',
                          value = min(data$endTime)
                )   ,
                dateInput('date2',
                          label = 'Last available Date',
                          value = max(data$endTime)
                          
                          
        )),
        
        mainPanel(
            plotOutput("distPlot",height="500px")
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        data1 <- data %>% filter(endTime>= input$date, endTime <= input$date2)
        x <- data1 %>% group_by(artistName) %>% summarise(time = sum(msPlayed)) %>% arrange(desc(time)) %>% slice(1:10)
        ggplot(x, aes(x=artistName, y=time)) + 
            geom_bar(stat="identity", width=.5,fill="#1D428A")+theme_minimal()+
            theme(axis.text.x = element_text(angle = 45, hjust = 1,size=13),axis.text.y =element_text(size=15))+
            xlab(element_blank())+ylab("Hours")

    })
}



shinyApp(ui = ui, server = server)

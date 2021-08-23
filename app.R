library(rsconnect)
library(shiny)
library(dplyr)
#library(DT)
library(ggplot2)

library(survey)
library(tibble)
library(tidyr)
library(haven)
library(stringr)
library(stringi)
library(jtools)
library(questionr)
library(viridis)
library(forcats)
library(data.table)


results1 <- read.csv("data/DHSEquity_dsbelow080421_clean.csv")
results2 <- read.csv("data/DHSEquity_group080421.csv")
results3 <- read.csv("data/DHSEquity_ratio080421_clean.csv")
results4 <-  read.csv("data/NationalDS031721.csv")
results5 <-  read.csv("data/UnderDS031721.csv")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel(h1("Track20 Equity Model")),
  
  mainPanel(img(src = "logo_150_trans1.png", align="right"),
            h2("Introduction"),
            
            p("Track20 has established a national measure of inequity for family planning- focusing on demand satisfied by modern methods and 5 dimensions- geography, age, parity, wealth, and education.  This measure is calculated for all women using data from Demographic and Health Surveys.  It is comparable across countries and within countries over time.  Demand satisfied is used because it represents the population with need for family planning."  ),
            h3("Choose Country") ,
            selectInput("Country", 
                        label = "Choose a Country",
                        choices = unique(results1$Country),
                        selected = ""),
            h2("Results"),
            p("To create the concentration of inequity, demand satisfied is calculated for each group (i.e. women from each geographic area or women from each age group).  Groups with demand satisfied 10%, 20%, or 30% lower than the national average are deemed inequitable.  The number of inequitable groups women belong to are then calculated. This shows whether inequity is focused on a small group of women who have multiple dimensions of inequity or if inequity is spread across many groups in the population.") ,
            selectInput("InequLevel", 
                        label = "Percent Below National Average for Demand Satisfied",
                        choices = unique(results2$group),
                        selected = ""),
            fluidRow(
              plotOutput("plot1", inline = TRUE)
            ),
            br(), 
            fluidRow(
              tableOutput("table")
            ),
            br(), 
            p("We are also interested in the overall level of demand satisfied in subgroups compared to the national average.  The table below shows the spread in subgroups by dimensions."),
            fluidRow(
              plotOutput("plot2", inline = TRUE)
            ),
            br(), 
             p("Finally, we show the ratio of the highest level of demand satisfied in a domain compared to the lowest level in the domain."),
            fluidRow(
              plotOutput("plot3", inline = TRUE)
            ),
            fluidRow(
              tableOutput("table2")
            ),
            br(),
            fluidRow(
              uiOutput("tab")
              
            )
            
            
            
  ))



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  filterData = reactiveVal(results2 %>% mutate(key = 1:nrow(results2)))
  
  filtered_df <- reactive({
    
    selected_data <- filterData()  %>% filter(Country %in% input$Country | is.null(input$Country)) %>% filter(group %in% input$InequLevel)
    
    
    selected_data1 <- setDT(selected_data)[, .(frequencySum=sum(Freq)), by=.(Year,Var1)]
    
    selected_data <- full_join(selected_data, selected_data1, by=c("Year", "Var1"))
    
    selected_data <- selected_data[order(selected_data$Freq, decreasing = TRUE),] 
    selected_data$Freq<-factor(selected_data$Freq, levels = unique(selected_data$Freq) )
    
    selected_data
    
  })
  
  filterData2 = reactiveVal(results1 %>% mutate(key = 1:nrow(results1)))
  
  
  
  filtered_df2 <- reactive({
    
    selected_data2 <- filterData2()  %>% filter(Country %in% input$Country | is.null(input$Country)) 
    
    
    selected_data2
    
  })
  
  
  filterData4 = reactiveVal(results4 %>% mutate(key = 1:nrow(results4)))
  
  
  
  filtered_df4 <- reactive({
    
    selected_data4 <- filterData4()  %>% filter(Country %in% input$Country | is.null(input$Country)) 
    
    
    selected_data4
    
  })
  
  
  filterData3 = reactiveVal(results3 %>% mutate(key = 1:nrow(results3)))
  
  
  
  filtered_df3 <- reactive({
    
    selected_data3 <- filterData3()  %>% filter(Country %in% input$Country | is.null(input$Country)) 
    
    
    selected_data3
    
  })
  
  
  filterData5 = reactiveVal(results5 %>% mutate(key = 1:nrow(results5)))
  
  filtered_df5 <- reactive({
    
    selected_data5 <- filterData5()  %>% filter(Country %in% input$Country | is.null(input$Country)) %>% filter(group %in% input$InequLevel) %>% arrange(Year) %>% select(-key, -group)
    
    
    selected_data5
    
  })
  
  
  
  filterData6 = reactiveVal(results3 %>% mutate(key = 1:nrow(results3)))
  
  
  
  filtered_df6 <- reactive({
    
    selected_data6 <- filterData6()  %>% filter(Country %in% input$Country | is.null(input$Country))  %>% select(Year, Group, Min, Max)
    
    
    selected_data6
    
  })
  

  output$table <- renderTable(filtered_df5())
  output$table2 <- renderTable(filtered_df6())
  

  output$plot1<-renderPlot({
    
    ggplot(filtered_df())+
      geom_bar(aes(x=as.factor(Year), y=frequencySum, group=Freq, fill=Var1), 
               stat="identity", color="white")   +
      scale_fill_viridis_d( alpha=.4) +
      geom_text(aes(x=as.factor(Year), y=frequencySum, group=Freq, label = label1), size = 6,   position = position_stack(vjust = 0.5))  +
      labs(title=paste(input$Country, ": Concentration of Inequity:\nIn Groups ",  input$InequLevel, "% below National Average", sep=""), x="", y="Proportion of Women")+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.background =element_blank(),
            plot.background = element_blank(),
            plot.margin = unit(c(1,0,0,0), "cm"),
            axis.text = element_text(color="black", size=14) ,
            axis.title = element_text(color="black", size=14) ,
            plot.title = element_text(size=22),
            legend.position = "none")
    
    
  }, height = 600,width = 700)
  
  
  
  
  output$plot2<-renderPlot({
    
    ggplot(filtered_df2(), aes(x=Group, y=Freq, color=Group)) + 
      geom_hline(data = filtered_df4(), aes(yintercept = National), color="red") +
            geom_text(aes(label=Labels), size=4) +
            facet_grid(rows=~Year) +
      labs(title=paste(input$Country, ": Demand Satisfied", sep=""), subtitle="Compared to National Average",  x="", y="")+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x= element_text(color="black", size=14),
            panel.background =element_blank(),
            plot.background = element_blank(),
            plot.margin = unit(c(1,0,0,0), "cm"),
            axis.text = element_text(color="black", size=14) ,
            axis.title = element_text(color="black", size=14) ,
            plot.title = element_text(size=22),
            strip.text.x = element_text(size = 14),
            legend.position = "none")
    
  }, height = 600,width = 800)
  
  
  
  
  output$plot3<-renderPlot({
    
    ggplot(filtered_df3(), aes(x=Group, y=Ratio, fill=Group)) +
      geom_bar(stat="identity") +
      facet_grid(rows=~Year) +
      labs(title=paste(input$Country, ": Ratio of Minimum and Maximum Demand Satisfied", sep=""), x="", y="")+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x= element_text(color="black", size=14),
            panel.background =element_blank(),
            plot.background = element_blank(),
            plot.margin = unit(c(1,0,0,0), "cm"),
            axis.text = element_text(color="black", size=14) ,
            axis.title = element_text(color="black", size=14) ,
            plot.title = element_text(size=22),
            strip.text.x = element_text(size = 14),
            legend.position = "none")
    
  }, height = 600,width = 800)
  
  

  url <- a("Click Here", href="http://track20.org")
  output$tab <- renderUI({
    tagList("The Track20 Equity Model was developed by Kristin Bietsch, PhD and Emily Sonneveldt, PhD of the Track20 Project, funded by the Bill and Melinda Gates Foundation.  For more information, please contact kbietsch@avenirhealth.org.  For more information on Track20's work:", url)
  })


  

}







shinyApp(ui = ui, server = server)

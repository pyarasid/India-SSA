#loading the essential libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(scales) 
library(shiny)
library(plotly)
library(writexl)

#importing import sheet of the complete excel
india_import <- read_excel("Export-Import.xlsx", sheet= "Import")
View(india_import)

#renaming the columns
india_import <- india_import %>%  rename(country="Reporter Name") %>% rename(partner="Partner Name") %>% 
  rename(product_group="Product Group")

str(india_import)
ls(india_import)
#?percent

#converting into tidy data
india_import <- melt(india_import, id.vars = c("country", "partner", "product_group", "Indicator"), 
                     measure.vars = c("1989","1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
                     variable.name = "Years", value.name = "Imports")


#creating line plot for import values
p1 <- ggplot(data=india_import %>% filter(Indicator== "Import (US$ Million)"), mapping = aes(x=Years, y=Imports)) 
  p1+ geom_line(aes(group=product_group, color= product_group), size= 1) + 
      theme(axis.text.x = element_text(angle = 55, hjust=0.8), legend.position = "top") +
      scale_y_continuous(label=comma)+labs(x="Years", y="Import Value(US$, MIllion)", color="Product Group") 


#creating line plot for import share and showing the percentages as the import share of various product groups in the total share
p2 <- ggplot(data=india_import %>% filter(Indicator== "Import-Share"), mapping = aes(x=Years, y=Imports)) 
  p2 + geom_line(aes(group=product_group, color= product_group), size= 1) + 
       theme(axis.text.x = element_text(angle = 55, hjust=0.8), legend.position = "top") +
       labs(x="Years", y="Import Share (in Percentages)", color="Product Group") + 
       scale_y_continuous(labels=scales::percent_format(accuracy = 1))
  
#Creating the shinyapp
 
ui <- fluidPage(
     sidebarLayout(
       sidebarPanel(
         #select variables for product gorup
         br(),
         selectInput(inputId = "group",
                     label = "Select Product Group:",
                     choices = unique(india_import$product_group),
                     multiple = TRUE,
                     selected = "Raw materials"),
         #select variables for x axis
         sliderInput(inputId = "time",
                     label = "Select the years range:",
                     min = 1989, max=2016,
                     value = c(1989,2016),
                     step=1)
       ),
       
       #Main Panel
       mainPanel(
         plotlyOutput(outputId = "linechart", width = "100%", height = "550px")
       )
  )
)
  
#Define server function
server <- function(input, output) {
  #creating a reactive function
  plotdata <- reactive({
    req(input$group)
    india_import %>% 
      filter(product_group %in% input$group) %>% 
      filter(Years %in% seq(input$time[1], input$time[2],1)) %>% 
      filter(Indicator== "Import (US$ Million)")
 
  })
#create line chart based on the reactivity 
  output$linechart <- renderPlotly({
  ggplotly(ggplot(data=plotdata(), mapping = aes(x=Years, y=Imports))+
    geom_line(aes(group=product_group, color= product_group), size= 1) + 
      theme(axis.text.x = element_text(angle = 55, hjust=0.8, size = 10), 
            legend.text = element_text(size=10),
            legend.title = element_text(size=12, face="bold"),
            axis.title = element_text(size=12, face="bold"),
            axis.text.y = element_text(angle=45, size=10)) +
      scale_y_continuous(label=comma)+labs(x="Years", y="Import Value(US$, MIllion)", color="Product Group")) %>% 
     add_annotations(text="India's imports from Sub-Saharan Africa", xref="paper", yref="paper", x=0, xanchor="left", y=1.11, yanchor="bottom", legendtitle=TRUE, showarrow= FALSE, font=list(size=25)) %>% 
     layout(margin = list(t=80)) 
   
  })
}
  
#create chiny app object
shinyApp(ui=ui, server=server)
  

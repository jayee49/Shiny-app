require(shiny)
require(tidyverse)
require("shinyjs")

country <- read.csv("~/Desktop/Carlos III/Data Tyding and Reporting/project/country.csv",header = F)
region <- read.csv("~/Desktop/Carlos III/Data Tyding and Reporting/project/region.csv",header = F)
global <- read.csv("~/Desktop/Carlos III/Data Tyding and Reporting/project/global_situation_data-2.csv",
                   sep = "\t", header = T, fileEncoding="UCS-2LE")

global$Country = as.character(global$Country)
global$Indicator.short.code = as.character(global$Indicator.short.code)
global$Region = as.character(global$Region)

region = as.matrix.data.frame(region)
colnames(region) = region[3,]
region = as.data.frame(region[-(1:3),])
region$`WHO region` = as.character(region$`WHO region`)

country = as.matrix.data.frame(country)
colnames(country) = country[3,]
country = as.data.frame(country[-(1:3),])
country$Country = as.character(country$Country)


### find out the lost country 
global$Country[which(is.na(match(global$Country,country$Country)))] 
country$Country[which(is.na(match(country$Country,global$Country)))]

mean(global$Value)
mean(global[global$Region=="Europe",]$Value)

country$Country[which(country$Country == "Eswatini")] = "Swaziland"
country$Country[which(country$Country == "United Kingdom of Great Britain and Northern Ireland")] = "United Kingdom"

global.n.sex = merge(global,country,by.x = "Country")

list_choices <-  unique(global.n.sex$Region)[!is.na(unique(global.n.sex$Region))]
names(list_choices) <- paste(unique(global.n.sex$Region)[!is.na(unique(global.n.sex$Region))],"",sep="")


# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
                 tabPanel("data",
                          fluidPage( 
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("select", label = h3("Plot by Region"), 
                                            choices = character(0),
                                            selected = 1)
                              ), # sidebarPanel
                              mainPanel(
                                plotOutput(outputId = "plot", click = "plot_click"),
                                tableOutput("info")
                              ) # mainPanel
                            ) # sidebarLayout
                          ) # fluidPage
                 ) #  tabPanel

) # navbarPage

col_scale <- scale_colour_discrete(limits = unique(global.n.sex$Region))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Can also set the label and select items
  updateSelectInput(session, "select",
                    choices = list_choices,
                    selected = tail(list_choices, 1)
  );
  
  output$plot <- renderPlot({
    if(input$select != ""){
      # cat(file=stderr(), "input$select:", input$select == "", "\n")
      ggplot(global.n.sex %>% filter(Region == input$select), aes(x=Value, y=value..regional., colour = Region)) +
        col_scale +
        geom_point()
    }
  });
  
  output$info <- renderTable({
    if(input$select != ""){
      nearPoints(global.n.sex 
                 %>% filter(Region == input$select) 
                 %>% select(Country, Value,  Region, value..regional.), 
                 input$plot_click, threshold = 10, maxpoints = 1,
                 addDist = F)
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

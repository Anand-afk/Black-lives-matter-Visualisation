#importing all the required libraries
library(shinydashboard)
library(shiny)
library(dplyr)
library(plotly)
library(corrgram)
library(countrycode)
library(shinyWidgets)
library(DT)
#reading the csv file
data_f <- read.csv('fatal-police-shootings-data.csv',stringsAsFactors = F,header=T)
data_f <- tbl_df(data_f)





data_f$race[data_f$race == "A"] <- "asian"
data_f$race[data_f$race == "W"] <- "white"
data_f$race[data_f$race == "B"] <- "black"
data_f$race[data_f$race == "N"] <- "NAmerica"
data_f$race[data_f$race == "H"] <- "hispanic"
data_f$race[data_f$race == "O"] <- "black"
data_f$race[data_f$race == ""] <- "black"


race<-levels(factor(data_f$race))

data_f$armed[data_f$armed == "Airsoft pistol"] <- "air pistol"
data_f$armed[data_f$armed == "baseball bat and bottle"] <- "baseball bat"
data_f$armed[data_f$armed == "baseball bat and fireplace poker"] <- "baseball bat"
data_f$armed[data_f$armed == "baseball bat and knife"] <- "baseball bat"

data_f$armed[data_f$armed == "baton"] <- "baseball bat"

data_f$armed[data_f$armed == "BB gun and vehicle"] <- "BB gun"
data_f$armed[data_f$armed == "bean-bag gun"] <- "BB gun"

data_f$armed[data_f$armed == "beer bottle"] <- "bottle"
data_f$armed[data_f$armed == "car, knife and mace"] <- "knife"
data_f$armed[data_f$armed == "chain saw"] <- "chainsaw"
data_f$armed[data_f$armed == "claimed to be armed"] <- "unarmed"
data_f$armed[data_f$armed == "undetermined"] <- "unarmed"
data_f$armed[data_f$armed == "unknown weapon"] <- "unarmed"
data_f$armed[data_f$armed == "gun and car"] <- "gun"
data_f$armed[data_f$armed == "gun and knife"] <- "gun"
data_f$armed[data_f$armed == "gun and sword"] <- "gun"
data_f$armed[data_f$armed == "gun and vehicle"] <- "gun"
data_f$armed[data_f$armed == "guns and explosives"] <- "gun"
data_f$armed[data_f$armed == "hatchet and gun"] <- "hatchet"
data_f$armed[data_f$armed == "machete and gun"] <- "machete"
data_f$armed[data_f$armed == "metal hand tool"] <- "metal object"
data_f$armed[data_f$armed == "metal pipe"] <- "metal object"
data_f$armed[data_f$armed == "metal pole"] <- "metal object"
data_f$armed[data_f$armed == "metal rake"] <- "metal object"
data_f$armed[data_f$armed == "metal stick"] <- "metal object"
data_f$armed[data_f$armed == "pole and knife"] <- "pole"
data_f$armed[data_f$armed == "vehicle and gun"] <- "vehicle"
data_f$armed[data_f$armed == "vehicle and machete"] <- "vehicle"

data_f$gender[data_f$gender == "M"] <- "Male"
data_f$gender[data_f$gender == "F"] <- "Female"
data_f$gender[data_f$gender == ""]<-"Other"


levels(factor(data_f$gender))


weapons<-levels(factor(data_f$armed))
# club_names<-levels(factor(data_f$Club))
#updating the dataframe with only the required columns


#deadly weapons used
deadly<- c('gun','nail gun','vehicle', 'sword', 'lawn mower blade', 'cordless drill', 'bean-bag gun', 'gun and knife',
           'garden tool', 'pick-axe', 'glass shard', 'pole and knife', 'chainsaw','pellet gun', 'samurai sword', 'vehicle and gun','vehicle and machete',
           'ice pick', 'air pistol','grenade','bow and arrow','BB gun','gun and sword', 'machete and gun','pitchfork','ax','baseball bat and fireplace poker',
           'sharp object','knife','hammer','machete','toy weapon','hatchet','box cutter','guns and explosives','Taser','meat cleaver','crossbow',
           'straight edge razor','chain saw','spear','hatchet and gun','crowbar','gun and car','incendiary device','gun and vehicle','BB gun and vehicle',
           'baseball bat and knife','car, knife and mace')

data_f$weapon[data_f$armed%in%deadly]<-"deadly"

`%notin%` <- Negate(`%in%`)

data_f$weapon[data_f$armed %notin% deadly]<-"nondeadly"

########################################################################################################

# 
header <- dashboardHeader(title = "US Police Shootings")

# Sidebar with a slider input for number of bins 
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Weapons across races", tabName = "weapons", icon = icon("dashboard")),
              menuItem("Armed and unarmed", tabName = "arms", icon = icon("dashboard"))
  ))

body <- dashboardBody(tabItems(
  tabItem(
    #tab number 1
    tabName = "weapons",
    h3("Weapons used by criminals"),
    tags$br(),
    sidebarMenu(
      selectInput(
        inputId = "weapons",
        label = "Weapons  used:",
        choices = weapons,
        selected = "gun",
        selectize = FALSE
      )),tags$br(),
    fluidRow(
      
      actionBttn(inputId = "white",label = "White", color = "success",style = "material-flat",icon = icon("sliders"),size = "md"),
      actionBttn(inputId = "black",label = "Black",color = "success",style = "material-flat",icon = icon("sliders"),size = "md"),
      actionBttn(inputId = "asian",label = "Asian",color = "success",style = "material-flat",icon = icon("sliders"),size = "md"),
      actionBttn(inputId = "hispanic",label = "Hispanic",color = "success",style = "material-flat",icon = icon("sliders"),size = "md"),
      actionBttn(inputId = "NAmerica",label = "Native America",color = "success",style = "material-flat",icon = icon("sliders"),size = "md")
    ),
    tags$br(),
    
    fluidRow(
      column(
        height=12,
        width = 8,
        plotlyOutput("plot1")
      ),
      column(
        width = 4,
        uiOutput("img"),
        tags$br(),
        verbatimTextOutput("click")
      )
    )
  ),
  tabItem(tabName = "arms",
          tabsetPanel(
            id = "tabs",
            #subtab no. 1
            tabPanel(
              title = "Armed and unarmed",
              value = "page1",
              h3("Armed and unarmed"),
              sidebarMenu(
                radioButtons("rad1", "Choose:", c("Male" = "male", "Female" = "female"),selected = "male"),
                selectInput(
                  inputId = "race",
                  label = "Race:",
                  choices = race,
                  selected = "white",
                  selectize = FALSE
                )
              ),
              fluidRow(
                column(
                  width = 5,
                  tags$br(),
                  plotlyOutput("plot2")
                ),
                column(
                  width = 6,
                  dataTableOutput("click1")
                )
              )
            )
          )
  )
)
)


ui <- dashboardPage(skin = "purple",
                    header,
                    sidebar,
                    body
)
###############################################################################################################
#The server starts here

server <- function(input, output) {
  
  tab1 <- reactive({
    if (input$weapons!=""){
      x <- data_f %>%
        filter(armed == input$weapons)}
  })
  
  tab2 <- reactive({
    if (input$rad1 == "male"){
      x <- data_f %>%
        filter(gender == "Male")
    }
    else if (input$rad1 == "female"){
      x <- data_f %>%
        filter(gender == "Female")}
  })
  
  plotHist = function(x,y){
    output$plot1 <- renderPlotly({plot_ly(x=x, y=y, type = "bar") %>%
        layout(title = "Stats: ",
               xaxis = list(title = ""),
               yaxis = list(title = ""))
      
    })
  }
  
  plotpie = function(data_f){
    f=table(data_f$weapon)
    perc = (f/sum(f))*100
    data_f=as.data.frame(perc)
    output$plot2 <- renderPlotly({plot_ly(data=data_f, labels = ~Var1, values = ~Freq, type = 'pie') %>%
        layout(title = 'Proportion of deadly and non deadly weapons used',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))})
  }
  
  #print the basic details of the player below the photo from above
  output$click <- renderPrint({
    res<-tab1()
    d <- event_data("plotly_click")
    if (is.null(d)) paste("Click on the bar to see the list of criminals" )
    else {var <- c(d[["x"]], d[["y"]])
    res<-res%>%filter(var[1]==name)
    cat(" Name:",paste(res$name),"\n","Date of incident:",paste(res$date),
        "\n","Manner of Death:",paste(res$manner_of_death),"\n","Age:",paste(res$age),
        "\n","Gender:",paste(res$gender),"\n","City:",paste(res$city),
        "\n","State:",paste(res$state)
    )}
    
  })
  
  observeEvent(input$race, 
               {resi<-tab2()
               resi<-resi%>% filter(race == input$race)
               plotpie(resi)})
  
  # rendering the datatable, when the user clicks on the pie chart in the third tab
  output$click1 <- renderDataTable({
    e <- event_data("plotly_click")
    resik<-tab2()
    resik<-resik%>% filter(race == input$race)
    if (is.null(e)) paste("Click on a Player bar to view the name" )
    else {vars<-c(e)
      if (vars[2]==0){
        tabss<-resik%>%filter(weapon=="deadly")
        tabss%>%
          select(name,manner_of_death,age,gender,city,state)%>%
          datatable(class = "nowrap hover row-border", escape = FALSE,
                    options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
      }
      else{
        tabss<-resik%>%filter(weapon=="nondeadly")
        tabss%>%
          select(name,manner_of_death,age,gender,city,state)%>%
          datatable(class = "nowrap hover row-border", escape = FALSE,
                    options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
      }
    }
  })
  
  
  
  
  observeEvent(input$white, {res<-tab1()
  res<-res%>% filter(race == "white") %>%
    top_n(30,wt = age)
  plotHist(res$name,res$age)
  })
  #if overall clicked
  observeEvent(input$black, {res<-tab1()
  res<-res %>% filter(race == "black") %>%
    top_n(30,wt = age)
  plotHist(res$name, res$age)})
  #if age clicked
  observeEvent(input$asian, {res<-tab1()
  res<-res%>%filter(race == "asian")%>%
    top_n(30,wt = age)
  plotHist(res$name, res$age)})
  #if weight clicked
  observeEvent(input$hispanic, {res<-tab1()
  res<-res%>%filter(race == "hispanic")%>%
    top_n(30,wt = age)
  plotHist(res$name, res$age)})
  #if height clicked
  observeEvent(input$NAmerica, {res<-tab1()
  res<-res%>%filter(race == "NAmerica")%>%
    top_n(30,wt = age)
  plotHist(res$name, res$age)})
  
}

shinyApp(ui = ui, server = server)

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
data_f$race[data_f$race == "O"] <- "other"
data_f$race[data_f$race == ""] <- "other"


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


data_f$flee[data_f$flee == "Car"] <- "Fleeing"
data_f$flee[data_f$flee == "Foot"]<-"Fleeing"
data_f$flee[data_f$flee == "Other"]<-"Not fleeing"
data_f$flee[data_f$flee == ""]<-"Not fleeing"

levels(factor(data_f$body_camera))

data_f$body_camera[data_f$body_camera == "False"] <- "off"
data_f$body_camera[data_f$body_camera == "True"] <- "on"


weapons<-levels(factor(data_f$armed))
# club_names<-levels(factor(data_f$Club))
#updating the dataframe with only the required columns
threat=levels(factor(data_f$threat_level))

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
header <- dashboardHeader(title = "All Lives Matter")

# Sidebar with a slider input for number of bins 
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Weapons across races", tabName = "weapons", icon = icon("dashboard")),
              menuItem("Armed and unarmed", tabName = "arms", icon = icon("dashboard")),
              menuItem("Camera on and off", tabName = "camera", icon=icon("dashboard")),
              menuItem("Density Globe", tabName = "map", icon=icon("dashboard"))
  ))

body <- dashboardBody(tabItems(
  tabItem(
    #tab number 1
    tabName = "weapons",
    h3("Weapons used by suspects across different race before shooting"),
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
        verbatimTextOutput("click1")
      )
    )
  ),
  tabItem(tabName = "arms",
              h3("Did race play a role when they were unarmed/(non deadly weapons)"),
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
                  dataTableOutput("click2")
                )
              )
  ),
  tabItem(tabName = "camera",
            h3("Did the criminal try to flee and given what circumstances"),
              sidebarMenu(
                radioButtons("rad2", "Body Camera:", c("On" = "on", "Off" = "off"),selected = "off"),
                selectInput(
                  inputId = "threat",
                  label = "Threat level:",
                  choices = threat,
                  selected = "attack",
                  selectize = FALSE
                )
              ),
              fluidRow(
                column(
                  width = 5,
                  tags$br(),
                  plotlyOutput("plot3")
                ),
                column(
                  width = 6,
                  dataTableOutput("click3")
                )
              )
  ),
  tabItem(tabName = "map",
          h3("Density of deaths all over USA."),
          fluidRow(column( 
            width = 12,
            plotlyOutput('map')
          ),
          tags$br(),
          column(width = 6),
          dataTableOutput('click4')
          ))
)
)


ui <- dashboardPage(skin = "red",
                    header,
                    sidebar,
                    body
)
######################################################################################################################################################################
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
  
  tab3 <- reactive({
    if (input$rad2 == "on"){
      x <- data_f %>%
        filter(body_camera == "on")
    }
    else if (input$rad2 == "off"){
      x <- data_f %>%
        filter(body_camera == "off")}
  })
  
  plotHist = function(x,y){
    output$plot1 <- renderPlotly({plot_ly(x=x, y=y, type = "bar") %>%
        layout(title = "Stats: ",
               xaxis = list(title = "Names of the criminals/suspects"),
               yaxis = list(title = "Age"))
      
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
  
  
  plotbone=function(data_f){
    
    a<-table(data_f$race,data_f$flee)
    a<-as.data.frame.matrix(a)
    position<-c('asian','black','hispanic','NAmerica','other','white')
    data <- data.frame(position, a['Fleeing'], a['Not fleeing'])
    data['Gap']=a['Not fleeing']-a['Fleeing']
    data
    output$plot3<-renderPlotly({plot_ly(data, color = I("gray80")) %>%
        add_segments(x = ~Fleeing, xend = ~Not.fleeing, y = ~position, yend = ~position, showlegend = FALSE) %>%
        add_markers(x = ~Fleeing, y = ~position, name = "Fleeing", color = I("pink")) %>%
        add_markers(x = ~Not.fleeing, y = ~position, name = "Not Fleeing", color = I("blue")) %>%
        layout(
          title = "Fleeing vs not fleeing",
          xaxis = list(title = "Number of deaths"),
          margin = list(l = 65)
        )})}
  
  
  
  
  #print the basic details of the player below the photo from above
  output$click1 <- renderPrint({
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
  
  
  output$click3 <- renderDataTable({
    ez <- event_data("plotly_click")
    if (is.null(ez)) paste("Click on either of the points" )
    else {vars<-c(ez)
    if (vars[1]==1){
      re<-tab3()
      re<-re%>% filter(threat_level == input$threat)
      rowix<-re%>%filter(race==vars[4],flee=="Fleeing")
      rowix%>%
        select(name,manner_of_death,age,gender,city,state)%>%
        datatable(class = "nowrap hover row-border", escape = FALSE, 
                  options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
    }
    else if (vars[1]==2){
      re<-tab3()
      rowix<-re%>%filter(race==vars[4],flee=="Not fleeing")
      rowix%>%
        select(name,manner_of_death,age,gender,city,state)%>%
        datatable(class = "nowrap hover row-border", escape = FALSE, 
                  options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
    }
    }
  })
  
  

  observeEvent(input$threat, {
    r<-tab3()
  r<-r%>% filter(threat_level == input$threat)
  plotbone(r)
  })
  
  
  observeEvent(input$race, 
               {resi<-tab2()
               resi<-resi%>% filter(race == input$race)
               plotpie(resi)})
  
  # rendering the datatable, when the user clicks on the pie chart in the third tab
  output$click2 <- renderDataTable({
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
  
  # to plot the density globe on the 4th tab
  dfg<-as.data.frame(table(data_f$state))
  dfg
# dfg['code']=countrycode(dfg$Var1, 'country.name', 'iso3c')
 dfg['id']=c(0,1:50)
  l <- list(color = toRGB("black"), width = 0.5)
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'albers usa')
  )
  output$map<- renderPlotly({plot_geo(dfg, locationmode = 'USA-states') %>%
      add_trace(
        z = ~Freq, color = ~Freq, colors = 'Blues',
        text = ~Var1, locations = ~Var1, marker = list(line = l)
      ) %>%
      colorbar(title = 'Number of deaths', tickprefix = '') %>%
      layout(
        geo = g
      )})
  
  # for displaying data table when clicked on the country on the density globe.
  output$click4 <- renderDataTable({
    e <- event_data("plotly_click")
    if (is.null(e)) paste("Click on the state to view the names and details" )
    else {vars<-c(e)
    rom<-dfg%>% filter(vars[2]==id)
    rex<-data_f%>% filter(state == rom$Var1)
    rex %>%
      select(name,manner_of_death,age,gender,city,state)%>%
      datatable(class = "nowrap hover row-border", escape = FALSE, 
                options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
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
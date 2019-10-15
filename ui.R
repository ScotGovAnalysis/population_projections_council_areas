require(shiny)
require(shinydashboard)
require(dplyr)
require(stringr)
require(ggplot2)
require(ggrepel)
library(DT)

# common features --------------------------------------------------------------
pop<-"#2DA197"


#load("dummy_dataset.RData")
load("final_dataset.RData")

list_of_places = as.list(as.character(unique(dat$Area)))

list_of_variants = as.list(as.character(unique(dat$Projection)))

order_list_of_variants<-list("Principal", "High migration", "Low migration", 
                             "High life expectancy", "Low life expectancy", 
                             "High fertility", "Low fertility", "Zero migration")

list_of_age_group = as.list(as.character(unique(dat$Age_group)))

theme_set(theme_minimal(base_size = 16)) 


# ------------------------------------------------------------------------------
ui <- dashboardPage(title="Population projections of Scotland - National Records of Scotland", 
                    dashboardHeader(
                      title = tags$a(href='http://www.nrscotland.gov.uk',
                      tags$img(height="45", alt="NRS", src="logo.png"),
                      tags$script(
                        HTML('$(document).ready(function() {
                             $("header").find("nav").append(\'<span class="myClass"> Population Projections for Scottish Areas (2016-Based) </span>\');
                             })' )),
                      tags$head(
                        tags$style(
                                HTML('.myClass {
                                     font-size: 18px;
                                     line-height: 50px;
                                     text-align: left;
                                     padding: 0 15px;
                                     overflow: hidden;
                                     color: white;
                                     font-family: "Roboto", sans-serif !important; font-weight:400;
                                     }')),
                              
                        HTML('<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Roboto: 100,200,300,400,500,900">'))
                      )),
             
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "tab0", icon = icon("th")),
      menuItem("Total population", tabName = "tab1", icon = icon("line-chart")),  #here add name of the menu items
      menuItem("Population profile", tabName = "tab2", icon = icon("bar-chart")),
      menuItem("Change by age group", tabName = "tab3", icon = icon("bar-chart")),
      menuItem("More information", tabName = "tab4", icon = icon("info")))
   ),

# ------------------------------------------------------------------------------
  dashboardBody(
    HTML("<script src='https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js'></script>"),
    HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-91956629-1'></script>"),
    tags$script(src = "cookie_control_config.js"),
    
  #adding css file
    tags$head(
     
      includeCSS("style.css")
      
    ),
    
    tabItems(
# - Tab 0 ----------------------------------------------------------------------
      tabItem(tabName = "tab0",
        fluidPage(
          titlePanel("Introduction"),

          br(),

          h4("This interactive visualisation shows the", (strong("projected 
             population for Scottish Council areas")), "from 2016 to 2041. The projected 
             change can be broken down by age and gender and comparisons between areas can be made."),
        

          h4("Population projections have been prepared for Council areas, NHS Boards areas, Strategic Development Plan (SDP)
             areas and National Park areas of Scotland. 
             More information on these areas is available in the",
             a("Sub-national population projections", 
                href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-projections/sub-national-population-projections"
                ,target="_blank"),
             "section of the NRS website."),
      
        
          h4("As well as producing the main principal projection, variant projections using alternative 
             plausible assumptions have also been produced. Variant projections give an idea of the 
             uncertainty around projections, and show what might be expected to happen under different 
             plausible assumptions about fertility, mortality and migration.")
          
         )
         ),
    


      
# - Tab 1 ----------------------------------------------------------------------
    tabItem(tabName = "tab1",
      fluidPage(
        titlePanel("Total population"),
        
      fluidRow(
        
        # - Side menu ----------------------------------------------------------
        column(3,
           wellPanel(
             helpText("Please select options for chart"),
             br(),
             selectizeInput(inputId="Area1", label="Select Areas: ",
                                choices=list_of_places,
                                selected=NULL,
                                multiple=TRUE,
                                options=list(placeholder="Enter area name"
                                             #, maxItems=2
                                             )
                            ),

             br(),
             sliderInput(inputId="end_year1",
                         "Choose year (line):",
                         value = max(dat$Year),
                         min = min(dat$Year),
                         max = max(dat$Year),
                         sep = "",
                         step=1),

             br(),
             radioButtons(inputId = "Type1", label = "Type of change",
                          choices = list("Absolute change", "Percentage change")),
  
             br(),
             checkboxGroupInput(inputId="Variants1", 
                                label = "Projection variants",
                                choices = order_list_of_variants, 
                                selected = "Principal"))
        ),
        # - Plot ---------------------------------------------------------------
        column(9, 
          wellPanel(
            plotOutput("my_plot1"))
      ))
        
    )),
# - Tab 2 ----------------------------------------------------------------------
tabItem(tabName = "tab2",
        fluidPage(
          titlePanel("Population profile"),
          
          fluidRow(
            column(3,
                   wellPanel(
                     helpText("Please select options for chart"),
                     br(),
                     selectizeInput(inputId="Area2", 
                                    label="Select Area: ",
                                    choices=list_of_places,
                                    selected="Scotland",
                                    multiple=FALSE
                                  ),
                     
                     br(),
                     
                     sliderInput(inputId="start_year2",
                                 "Choose year (solid):",
                                 value = min(dat$Year),
                                 min = min(dat$Year),
                                 max = max(dat$Year),
                                 sep = "",
                                 step=1),
                     
                     br(),
                     sliderInput(inputId="end_year2",
                                 "Choose year (line):",
                                 value = min(dat$Year),
                                 min = min(dat$Year),
                                 max = max(dat$Year),
                                 sep = "",
                                 step=1, animate = animationOptions(interval=500)),
                     helpText("Click play to see animation"),
                     
                     br(),
                     radioButtons(inputId="Variants2", 
                                  label = "Projection variants",
                                  choices = order_list_of_variants, 
                                  selected = "Principal")
                     
                   )), #end column4
            column(9,
                   wellPanel(
                     plotOutput("my_plot2")
                     
                   ))
          ))), # tab2 end 

# - Tab 3 ----------------------------------------------------------------------
      tabItem(tabName = "tab3",
        fluidPage(
          titlePanel("Change by age group"),
          
          fluidRow(
            column(3,
               wellPanel(
                 helpText("Please select options for chart"),
                 br(),
                 
                 selectizeInput(inputId="Area3", label="Select Areas:",
                                    choices=list_of_places,
                                    selected=NULL,
                                    multiple=TRUE,
                                    options=list(placeholder="Enter area name"
                                                 #, maxItems=2
                                                 )
                                ),
                 br(),
                 sliderInput(inputId="end_year3",
                             "Choose year:",
                             value = max(dat$Year),
                             min = min(dat$Year),
                             max = max(dat$Year),
                             sep = "",
                             step=1),

                 br(),
                 radioButtons(inputId="Variants3", 
                              label = "Projection variants",
                              choices = order_list_of_variants, 
                              selected = "Principal")

          )),
          column(9,
            wellPanel(
              plotOutput("my_plot3a"),
              br(),
              plotOutput("my_plot3b")
          
              )))
          )
        ),

# - Tab 3 ----------------------------------------------------------------------
tabItem(tabName = "tab4",
  fluidPage(
    titlePanel("More information"),
    
    fluidRow(
      column(12, 
        
        br(),
        
        h4("Population projections are calculations showing what may happen to the
            population under certain assumptions about future ", strong("fertility"),
            ", ", strong("mortality"), "and", strong("migration"), ". The principal
            projection is the main figure. Variant projections are produced to give
            an indication of the inherent uncertainty of demographic behaviour."),
        
        
        h4("As well as the principal assumptions, high and low assumptions are prepared for each of the components of population
             change (fertility, life expectancy and net migration). These are used to generate what are referred to as the standard
             variants (High fertility, Low fertility, High life expectancy, Low life expectancy, High migration, Low migration)."),
  
        
        h4("A special case scenario that assumes zero outwith Scotland migration has also been produced. This allows
             the potential impact of migration on the population to be explored by comparing this variant with others."),
        
        
        
        h4("More information about the projections, their uses and their limitations can be found on the", 
          a("Sub-national population projections", href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-projections/sub-national-population-projections",
          target="_blank"), "page of the NRS website. "),
        
        br(),
        
        h4("A similar visualisation allowing comparisons of the projections for Scotland and the UK is available at ", 
           a("National population projections visualisation", href="https://scotland.shinyapps.io/population-projection-variants-scotland-uk/",
             target="_blank"),"."),
        
            br(),br(),
        
        fluidRow(
          column(4,
                 h5(strong("More information")),
                 
###CHANGE THIS LINK TO CORRECT DATA HYPERLINK###                 
                 h5("Data: ", a("Projected population of Scottish Areas, 2016-based (csv)",
                                href="https://www.nrscotland.gov.uk/files//statistics/nrs-visual/sub-nat-pop-pro-16/pop-proj-principal-2016-data-vis.csv",
                                target="_blank")),
                 
                 h5("Publication: ", a("Projected population of Scottish Areas, 2016-based",
                                       href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-projections/sub-national-population-projections/2016-based",
                                       target="_blank"))),
          column(4, 
                 br(),
                 h5("Follow us on Twitter - ", 
                    a("@NatRecordsScot", 
                      href="https://twitter.com/NatRecordsScot",
                      target="_blank")),
                 h5("See more ", 
                    a("Infographics & Visualisations", 
                      href="http://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/infographics-and-visualisations", 
                      target="_blank"))),
          column(4, 
                 br(),
                 h5(a("National Records of Scotland", href="http://www.nrscotland.gov.uk", target="_blank")),
                 
                 h5("\U00A9 Crown Copyright 2016 - ", #unicode for copyright sign
                    a("Copyright conditions", 
                      href="http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
                      target="_blank")))
          
        ) # End of fluidRow
        
        
        ),

          column(12, 
       br(), 
      p("Any feedback about this visualisation?", 
        a("Get in touch!", href="mailto:victoria.avila@nrscotland.gov.uk?cc=statisticscustomerservices@nrscotland.gov.uk&subject=Subnational%20Population%20Projections%20of%20Scotland%20visualisation"))
        ) )
        
        
           )#end column
           )#end fluid
         
        )#tab4 end
      
    ) #tab items
)



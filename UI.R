ui <- fluidPage(
  # App title ----
  titlePanel(
    p( 
      h1("Digitally United We Stand, Digitally Divided We Fall?", align = "center"),
      br(),
      h3("An Exploratory Analysis of Learning-Facility for U.S K-12 Students In the COVID-19 Era.", align = "center"),
      br()
    )
  ),
  # Main panel for displaying outputs ----
  mainPanel(width = 15,
            tabsetPanel(
              #
              tabPanel("Summary", br(), br(),
                       mainPanel(width = 15,
                                 helpText(
                                   'Context:',
                                   br(),
                                   'With the abrupt end of in-person schooling in the spring of 2020, learning opportunities available to',
                                   br(),
                                   'students varied enormously with some students receiving almost no distance instruction and others engaging',
                                   br(),
                                   'in meaningful learning. In the forced transition to remote learning, there have been a influx of literature',
                                   br(),
                                   'arguing that the pandemic will likely widen achievement gaps along socioeconomic status dimensions (SES)',
                                   br(),
                                   'given an unequal engagement with online learning resources across different SES areas, a phenomenon aptly named',
                                   br(),
                                   '“double digital divide”. “Double digital divide" means the the disparity in students’ access to individual devices',
                                   br(),
                                   'and broadband technology for educational purposes.',
                                   br(),
                                   br(),
                                   'Research questions:',
                                   br(),
                                   '1. How big was the “double digital divide” among K-12 students in the US during the COVID-19 pandemic?',
                                   br(),
                                   '2. How did that situation change over time? (Which gap was closed, which gap remained?)',
                                   br(),
                                   br(),
                                   'Data set:',
                                   br(),
                                   'The Household Pulse Survey (HPS) published by the U.S. Census Bureau is a 20-minute online survey. It studies',
                                   br(),
                                   'Americans’ experience during the pandemic, by specifically gauging the impact of COVID-19 on different social economic',
                                   br(),
                                   'aspects. I used most updated datasets from the total of 23 survey rounds from April 2020 till Jan 2021',
                                   br(),
                                   br(),
                                   'About the Author:',
                                   br(),
                                   'I am Trang Hoang, a Master of Education student at the College of Education, University of Washington.', 
                                   br(),
                                   'My primary research interest is the efficacy of educational public policies.',
                                   br(),
                                   'This application is under development. All comments and suggestions are welcomed.',
                                   br(), 
                                   'Contact:', a("trangh2@uw.edu",href = "mailto:trangh2@uw.edu",  target="_blank"))
                       )
              ),
              #
              tabPanel("Learning-Facility Access across States", br(), br(),
                       mainPanel(width = 11,
                                 #these filters are applied to all maps
                                 splitLayout(
                                   mainPanel(width = 5.5,
                                             leaflet::leafletOutput("map_Q3", 
                                                                    width  = "500px",height = "300px")),
                                   mainPanel(width = 5.5,
                                             leaflet::leafletOutput("map_Q1",
                                                                    width  = "500px", height = "300px")),
                                   tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
                                 ),
                                 splitLayout(
                                   mainPanel(width = 5,
                                             selectInput(inputId = "inputvar_Dimension", 
                                                         label = "Select a demographic characteristic",
                                                         choices = "")),
                                   mainPanel(width = 5,
                                             selectInput(inputId = "inputvar_Round", 
                                                         label = "Select a survey round (most recent first)",
                                                         choices = ""))
                                 ),
                                 splitLayout(
                                   mainPanel(width = 5,
                                             selectInput(inputId = "inputvar_Q3", 
                                                         label = "Students always have access to",
                                                         choices = c("Digital devices", "Internet"))),
                                   mainPanel(width = 5,
                                             selectInput(inputId = "inputvar_Q1", 
                                                         label = "Frequency of live contact with teachers in the last 7 days",
                                                         choices = c("4 or more days",
                                                                     "1-3 days","None")))
                                 ),
                                 withMathJax(),
                                 helpText('Notes:',
                                          br(),
                                          'These choropleth maps presents the spatial distribution of 
                                      various variables capturing digital learning access for K-12 students enrolled in private or public schools in the U.S. 
                                      Surveyed students that did not response are not included in the computation of the total figure.',
                                          br(),
                                          br(),
                                          'Data source: "', a("Household Pulse Survey Data Tables",href="https://www.census.gov/programs-surveys/household-pulse-survey/data.html", target="_blank"),
                                          '". United States Census Bureau.'
                                 )
                       )
              ),
              #
              tabPanel("Learning-Facility Access by States", br(), br(),
                       #TS plots
                       mainPanel(width = 15,
                                 splitLayout(
                                   plotly::plotlyOutput("ts_plot",
                                                        width  = "600px",height = "500px"),
                                   plotly::plotlyOutput("ts_plot_day",
                                                        width  = "600px",height = "500px")
                                 ),
                                 splitLayout(
                                   selectInput(inputId = "inputvar_state", 
                                               label = "Select a state",
                                               choices = ""),
                                   selectInput(inputId = "inputvar_roll", 
                                               label = "Select a moving-average smoothing window length",
                                               choices = c(
                                                 'No smoothing' = 1,
                                                 '4 weeks' = 2,
                                                 '6 weeks' = 3,
                                                 '8 weeks' = 4))
                                 ),
                                 splitLayout(
                                   checkboxGroupInput(inputId = "checkbox_total", 
                                                      label = "Total",
                                                      choices = "Total"),
                                   checkboxGroupInput(inputId ="checkbox_race", 
                                                      label = "Race",
                                                      choices = ""),
                                   checkboxGroupInput(inputId ="checkbox_income", 
                                                      label = "Household income",
                                                      choices = ""),
                                   checkboxGroupInput(inputId ="checkbox_parent", 
                                                      label = "Parents' education",
                                                      choices = "")
                                 ),
                                 withMathJax(),
                                 helpText('Notes:',
                                          br(),
                                          'These plots present the time series of access to digital devices and internet provided by schools or school districts. 
                                              The survey sample consists of K-12 students enrolled in private or public schools.',
                                          br(),
                                          br(),
                                          'Data source: "', a("Household Pulse Survey Data Tables",href="https://www.census.gov/programs-surveys/household-pulse-survey/data.html", target="_blank"),
                                          '". United States Census Bureau.'
                                 )
                       )
              )
            )
  )
)


#check if all required packages are installed
mypackages <- c("leaflet", "sp", "shiny", "ggplot2", "geojsonio", 
                "RColorBrewer", "dplyr")
checkpkg <- mypackages[!(mypackages %in% installed.packages()[,"Package"])];
#if not, then install the missing packages  
if(length(checkpkg)) install.packages(checkpkg, dependencies = TRUE)
#loading packages
library(shiny); 
library(plotly); library(scales)
library(leaflet); 
library(RColorBrewer); 
library(ggplot2); 
library(dplyr); library(tidyr)
library(geojsonio); 
library(sp);
library(zoo)

####Objects created outside of server----
##semesters
#05/12 - 06/11 Spring; 06/11 - 08/30 Summer
#09/01 - 12/30 Fall; 01/01 - 02/01 Winter
dates_sem <- data.frame(
  from1 = as.Date(c('2020-05-12','2020-09-01')), #beginnings of spring and fall
  to1  = as.Date(c('2020-06-11','2020-12-30')),
  col = as.factor(c('blue','green')))
dates_sem2 <- data.frame(
  from1 = as.Date(c('2020-09-16')), #beginnings of fall (for contact day data)
  to1  = as.Date(c('2020-12-30')),
  col = as.factor(c('green')))

##transfrom .json file into a spatial polygons data frame
states <- 
  geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
    what = "sp"
  )
##Survey periods: To make survey frequency consistent
  survey_periods <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                                     "1iXvi-q3OUVPu5CRAVpvQ9JVuBG0XQczf"),
                             header=TRUE,check.names=FALSE,stringsAsFactors=FALSE);
  n_rounds <- nrow(survey_periods)
  #
  survey_periods$Begin <- as.Date(survey_periods$Begin, format = '%d/%m/%Y')
  survey_periods$End <- as.Date(survey_periods$End, format = '%d/%m/%Y')
  #add biweekly indicators and dates for first 12 weeks
  survey_periods$Biweek <- recode(survey_periods$Round, 
                                  `1`='1',`2`='1',`3`='2',`4`='2',`5`='3',`6`='3',
                                  `7`='4',`8`='4',`9`='5',`10`='5',`11`='6',`12`='6',
                                  `13`='7',`14`='8',`15`='9',`16`='10',`17`='11',`18`='12',
                                  `19`='13',`20`='14',`21`='15',`22`='16',`23`='17')
  survey_periods$Biweek_begin = survey_periods$Biweek_end = 
    as.Date(rep(0, n_rounds))
  survey_periods$Biweek_begin[seq(1,12,2)] = 
    survey_periods$Biweek_begin[seq(2,12,2)] = 
    survey_periods$Begin[seq(1,12,2)]
  survey_periods$Biweek_end[seq(1,12,2)] = 
    survey_periods$Biweek_end[seq(2,12,2)] = 
    survey_periods$End[seq(2,12,2)]
  #keep other weeks unchanged
  survey_periods$Biweek_begin[c(13:23)] = survey_periods$Begin[c(13:23)]
  survey_periods$Biweek_end[c(13:23)] = survey_periods$End[c(13:23)]
  #
#function to collapse the first 12 weeks
collapse_12week <- function(df){
  df <- df %>%
    mutate(Week = survey_periods$Biweek[match(Week, survey_periods$Round)],
           Week_end = survey_periods$Biweek_end[match(Week_end, survey_periods$End)]) 
  df_upper <- df %>% filter(Week %in% c(1:6)) %>%  
    group_by_at(.vars = c(1:5))  %>%
    summarise(across(where(is.numeric), 
                     ~ mean(.x, na.rm = TRUE))) %>%
    ungroup()
  df_upper[sapply(df_upper, is.nan)] <- NA
  df_lower <- df %>% filter(!Week %in% c(1:6)) 
  df <- bind_rows(df_upper, df_lower)
  return(df) 
}

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

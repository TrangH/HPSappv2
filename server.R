server <- function(input, output, session) {
    ####Reactively load data----
    Q3 <- reactive({
      Q3_df <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                       "18ecFURKRA0PTjLfeZPO6lMrMZ59wiir1"),
               header=TRUE,check.names=FALSE,stringsAsFactors=FALSE)[-1];
      Q3_df <- Q3_df %>% mutate(Week_end = as.Date(Week_end)) %>%
        collapse_12week () %>% 
        filter(state %in% state[nchar(state) == 2] & state != 'US') %>%
        mutate(state = state.name[match(state,state.abb)]) %>%
        tidyr::replace_na(., list(state = "District of Columbia")) %>%
        #mutate(state = tidyr::replace_na(state, 'District of Columbia')) %>%
        mutate(PC = rowSums(select(.,matches('pc_always')), 
                 na.rm = TRUE),
               Internet = rowSums(
                 select(., matches('internet_always')), 
                 na.rm = TRUE)) %>%
        select(1:5, PC, Internet, Total) %>%
        mutate(PC = round(100*PC/Total,1),
               Internet = round(100*Internet/Total,1))
      Q3_df[sapply(Q3_df, is.nan)] <- NA 
      return(Q3_df)  
    })
    #
    Q4 <- reactive({
      Q4_df <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                                "1NmC4mJI4ukji6AIaiBaqmdZ5-vuALHmv"),
                        header=TRUE,check.names=FALSE,stringsAsFactors=FALSE)[-1];
      Q4_df <- Q4_df %>% mutate(Week_end = as.Date(Week_end)) %>%
        collapse_12week () %>% 
        filter(state %in% state[nchar(state) == 2] & state != 'US') %>%
        mutate(state = state.name[match(state,state.abb)]) %>%
        tidyr::replace_na(., list(state = "District of Columbia")) %>%
        #mutate(state = tidyr::replace_na(state, 'District of Columbia')) %>%
        mutate(PC = rowSums(select(.,matches('pc_school')), 
                            na.rm = TRUE),
               Internet = rowSums(
                 select(., matches('internet_school')), 
                 na.rm = TRUE)) %>%
        select(1:5, PC, Internet, Total) %>%
        mutate(PC = round(100*PC/Total,1),
               Internet = round(100*Internet/Total,1))
      Q4_df[sapply(Q4_df, is.nan)] <- NA 
      return(Q4_df)  
    })
    #
    Q1 <- reactive({
      Q1_df <-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                         "15ruoxPtiTC-WuKEPwZHv-pWH_Uh69BUa"),
              header=TRUE,check.names=FALSE,stringsAsFactors=FALSE)[-1];
      #
      Q1_df <- Q1_df %>% mutate(Week_end = as.Date(Week_end)) %>%
        collapse_12week () %>% 
        filter(state %in% state[nchar(state) == 2] & state != 'US') %>%
        mutate(state = state.name[match(state,state.abb)]) %>%
        tidyr::replace_na(., list(state = "District of Columbia")) %>%
        mutate(`None` = rowSums(select(.,matches('contact_none')), na.rm = TRUE),
               `1-3 days` = rowSums(select(., matches('contact_1d|contact_2d')), na.rm = TRUE),
               `4 or more days` = rowSums(select(., matches('contact_4d')), na.rm = TRUE)) %>%
        select(1:5, `None`,`1-3 days`, `4 or more days`) %>%
        mutate(Total = rowSums(select(., `None`,`1-3 days`, `4 or more days`), na.rm = TRUE)) %>%
        mutate(`None` = round(100*`None`/Total,1),
               `1-3 days` = round(100*`1-3 days`/Total,1),
               `4 or more days` = round(100*`4 or more days`/Total,1)) 
      Q1_df[sapply(Q1_df, is.nan)] <- NA 
      Q1_df <- Q1_df[rowSums(is.na(Q1_df[,6:ncol(Q1_df)]))!=5,] #remove rows that have all NAs
      return(Q1_df)
    })
    
    ####Time series plot----
    ##Reactive filter for TS data 
    #Time series Q3----
    ts_Q3 <- reactive({
        Q3() %>% 
        filter(state == input$inputvar_state) %>% 
        mutate(Dimension =  recode(Dimension,
             `High Income (more than $100,000)`='High income',
            `Low Income (less than $50,000)`='Low income')) %>%
        mutate(variable = 'Always have access') 
    })
    observe({
      updateSelectInput(session, "inputvar_state",
                        choices = unique(Q3()$state)
      )})
    observe({
      updateCheckboxGroupInput(session, "checkbox_total",
          label = "Total",
          choices = "Total", selected = "Total")
    })
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_race",
       label = "Race",
       choices = unique(ts_Q3()$Dimension[
         ts_Q3()$Dimension_type == 'Race']), selected = "")
    })
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_income",
          label = "Household income",
          choices = unique(ts_Q3()$Dimension[
            ts_Q3()$Dimension_type == 'Income']), selected = "")
    })
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_parent",
           label = "Parents' education",
           choices = unique(ts_Q3()$Dimension[
           ts_Q3()$Dimension_type == 'Parent Education']), selected = "")
    })
    #Time series Q4----
    ts_Q4 <- reactive({
        Q4() %>% 
        filter(state == input$inputvar_state) %>% 
        mutate(Dimension =  recode(Dimension,
                                   `High Income (more than $100,000)`='High income',
                                   `Low Income (less than $50,000)`='Low income')) %>%
        mutate(variable = 'Schools/districts provided') 
    })
    observe({
      updateSelectInput(session, "inputvar_state",
                        choices = unique(Q3()$state)
      )})
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_total",
                               label = "Total",
                               choices = "Total", selected = "Total")
    })
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_race",
               label = "Race",
               choices = unique(ts_Q3()$Dimension[
                 ts_Q3()$Dimension_type == 'Race']), selected = "")
    })
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_income",
                               label = "Household income",
                               choices = unique(ts_Q3()$Dimension[
                                 ts_Q3()$Dimension_type == 'Income']), selected = "")
    })
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_parent",
                               label = "Parents' education",
                               choices = unique(ts_Q3()$Dimension[
                                 ts_Q3()$Dimension_type == 'Parent Education']), selected = "")
    })
    #Time series Q1----
    ts_Q1 <- reactive({
      Q1() %>% 
        filter(Total != 0) %>%
        filter(state == input$inputvar_state) %>% 
        arrange(Week_end) %>%
        mutate(Dimension =  recode(Dimension,
                                   `High Income (more than $100,000)`='High income',
                                   `Low Income (less than $50,000)`='Low income')) 
    })
    observe({
      updateSelectInput(session, "inputvar_state",
                        choices = unique(Q3()$state)
      )})
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_total",
                               label = "Total",
                               choices = "Total", selected = "Total")
    })
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_race",
                               label = "Race",
                               choices = unique(ts_Q3()$Dimension[
                                 ts_Q3()$Dimension_type == 'Race']), selected = "")
    })
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_income",
                               label = "Household income",
                               choices = unique(ts_Q3()$Dimension[
                                 ts_Q3()$Dimension_type == 'Income']), selected = "")
    })
    #
    observe({
      updateCheckboxGroupInput(session, "checkbox_parent",
                               label = "Parents' education",
                               choices = unique(ts_Q3()$Dimension[
                                 ts_Q3()$Dimension_type == 'Parent Education']), selected = "")
    })
    ###Learning-access ts plot----
    output$ts_plot <- renderPlotly({
      data_access <- bind_rows(ts_Q3(), ts_Q4()) %>% 
        filter(Dimension %in% input$checkbox_total | 
                 Dimension %in% input$checkbox_race |
                 Dimension %in% input$checkbox_income |
                 Dimension %in% input$checkbox_parent) %>%
        tidyr::pivot_longer(!c(1:5,8,9), 
                            names_to = "type", values_to = "Percent") %>% 
        arrange(Dimension) %>% group_by(Dimension, variable, type) %>%
        mutate(across(where(is.numeric), 
                      ~ zoo::rollapply(.x, 
                                       as.numeric(input$inputvar_roll), mean, 
                                       align = 'right', fill = NA))) %>%
        drop_na() %>%
        mutate(Percent = round(Percent,1)) %>%
        mutate(type = recode(type, `PC` = 'Digital devices'))
      #
      ggp <- ggplot(
          data = data_access, 
                 aes(x = Week_end, y = Percent)) + 
          geom_line(aes(color = Dimension), 
                 linetype = 1, size = 0.5) + 
        scale_x_date(labels = date_format("%m-%Y"),
            date_breaks = "2 month") + 
            #scale_y_continuous(limits = c(0,100)) + 
        facet_grid(variable ~ type, scales="free_y",
           labeller = label_wrap_gen(width = 28)) +
          xlab('') + ylab('') + 
          ggtitle(paste('% Students with Learning-Facility Access in ', 
                        input$inputvar_state, sep = '')) +
          theme_bw() +
          theme(legend.position = "none",
                legend.title = element_blank(),
                legend.key.height = unit(2, "cm"),
                legend.text = element_text(size = 11),
                plot.title = element_text(hjust = 0.5), 
                axis.text.x = element_text(size = 11,
                      angle = 45, vjust = 1, hjust = 1),
                axis.text.y = element_text(size = 11),
                strip.text.x = element_text(size = 9, face = 'bold'),
                strip.text.y = element_text(size = 9, face = 'bold'),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) #end of ggplot
      ##Get the default ylim and build a rectangle coordinate matrix: Not working
      #ylim_top <- ggplot_build(ggp)$layout$panel_scales_y[[1]]$range$range;
      #ylim_bottom <- ggplot_build(ggp)$layout$panel_scales_y[[2]]$range$range
      #rect_mat <- rbind.data.frame(
      #  cbind(dates_sem[,-3],rbind(ylim_top, ylim_top)));
      #colnames(rect_mat) <- c('xmin','xmax','ymin','ymax')
      ##Add shaded rectangles
      ggp <- ggp + geom_rect(
                data =  dates_sem,
                aes(xmin = from1-1, xmax = to1, 
                ymin = 0, ymax = 100, fill = col), 
                inherit.aes = FALSE, alpha = 0.15, show.legend=FALSE) + 
        scale_fill_manual(values = c('#619CFF','#00BA38'), guide=FALSE) + 
        annotate(geom = 'text', label = 'Spring',
                 color = '#619CFF', x = as.Date(c("2020-05-20")), 
                 y= 95, size = 3, hjust = 0) + 
        annotate(geom = 'text', label = 'Summer',
                 color = 'black', x = as.Date(c("2020-07-20")), 
                 y= 95, size = 3, hjust = 0) + 
        annotate(geom = 'text', label = 'Fall',
                 color = '#00BA38', x = as.Date(c("2020-11-4")), 
                 y= 95, size = 3, hjust = 0) + 
        annotate(geom = 'text', label = 'Winter',
                 color = 'black', x = as.Date(c("2021-01-17")), 
                 y= 95, size = 3, hjust = 0) 
      ##convert to ggplotly
      gp <- ggplotly(ggp)
      gp
    })
    
    ##Average days of live contact plot----
    output$ts_plot_day <- renderPlotly({
      data_day <- ts_Q1() %>% 
        mutate(`Average day` = 
                 (0*None + 2*`1-3 days` + 4.5*`4 or more days`)/100) %>%
        filter(Dimension %in% input$checkbox_total | 
                 Dimension %in% input$checkbox_race |
                 Dimension %in% input$checkbox_income |
                 Dimension %in% input$checkbox_parent) %>%
        arrange(Dimension) %>% group_by(Dimension) %>%
        mutate(across(where(is.numeric), 
                      ~ zoo::rollapply(.x, 
                                       as.numeric(input$inputvar_roll), mean, 
                                       align = 'right', fill = NA))) %>%
        drop_na() %>%
        mutate(`Average day` = round(`Average day`,1))
      #
      ggp <- ggplot(
          data = data_day, 
          aes(x = Week_end, y = `Average day`)) + 
      geom_line(aes(color = Dimension), 
           linetype = 1, size = 0.5) + 
        scale_x_date(labels = date_format("%m-%Y"),
                     date_breaks = "1 month") + 
        xlab('') + ylab('') +
        ggtitle(paste('Average Days of Live Contact with Teachers in ', 
                      input$inputvar_state, sep = '')) +
        theme_bw() +
        #guides(fill=FALSE) + 
        theme(legend.position = "right",
              legend.title = element_blank(),
              legend.key.height = unit(2, "cm"),
              legend.text = element_text(size = 11),
              plot.title = element_text(hjust = 0.5), 
              axis.text.x = element_text(size = 11,
                                         angle = 45, vjust = 1, hjust = 1),
              axis.text.y = element_text(size = 11),
              strip.text.x = element_text(size = 9, face = 'bold'),
              strip.text.y = element_text(size = 9, face = 'bold'),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()
          ) 
      ##get the default ylim
      ylim_ggp <- layer_scales(ggp)$y$get_limits()
      ##Add shaded rectangles
      ggp <- ggp + 
        geom_rect(data = dates_sem2,
               aes(xmin = from1-1, xmax = to1, 
               ymin = ylim_ggp[1], ymax = ylim_ggp[2]+0.25, fill = col), 
               inherit.aes = FALSE, alpha = 0.15, show.legend = FALSE) +
        scale_fill_manual(values = c('#00BA38'), guide=FALSE) +
        annotate(geom = 'text', label = 'Fall',
                 color = '#00BA38', x = as.Date(c("2020-11-4")), 
                 y= ylim_ggp[2]+0.5, size = 5, hjust = 0) + 
        annotate(geom = 'text', label = 'Winter',
                 color = 'black', x = as.Date(c("2021-01-17")), 
                 y= ylim_ggp[2]+0.5, size = 5, hjust = 0) 
      ##convert to ggplotly
      gp <- ggplotly(ggp) 
      gp <- plotly::style(gp, showlegend = FALSE, 
                   traces = (length(gp[['x']][['data']])-2):
                   length(gp[['x']][['data']]));
      for (i in 1:(length(gp[['x']][['data']])-3)) {
        gp[['x']][['data']][[i]][['name']] <- 
          substr(gsub("[()]", "", 
          gp[['x']][['data']][[i]][['name']]),1,
            nchar(gsub("[()]", "", 
            gp[['x']][['data']][[i]][['name']]))-2)
      }
      #gp<- gp %>% plotly::style(hoverinfo = "none", traces = c(2:4)) 
      gp
    })
    
    ####Spatial maps----
    
    ##Use CRS map projection
    epsg2163 <- leafletCRS(
      crsClass = "L.Proj.CRS",
      code = "EPSG:2163",
      proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
      resolutions = 2^(16:7))
    ##coloring
    pal_Q3 <- colorNumeric(palette = "viridis", domain = NULL)
    pal_Q3_na <- colorNumeric(palette = "viridis", 
                              domain = NULL, na.color=rgb(0,0,0,0),
                              reverse = TRUE)
    ###Map Q3----
    ##Reactive filter for map data 
    df_Q3 <- reactive({sp::merge(states, Q3() %>% 
                    mutate(Week_end = as.Date(Week_end, format = "%Y-%m-%d")) %>%              
                    filter(Week_end == input$inputvar_Round) %>% 
                    filter(Dimension == input$inputvar_Dimension),
                    by.x = "name", by.y = "state",
                    duplicateGeoms = TRUE) 
                    })
    observe({
      updateSelectInput(session, "inputvar_Dimension",
                        choices = unique(Q3()$Dimension)
         )})
    observe({
      updateSelectInput(session, "inputvar_Round",
                        choices = sort(unique(Q3()$Week_end),
                                       decreasing = TRUE)
         )})
    ##Reactive variable selector
    var_Q3 <- reactive({switch(input$inputvar_Q3, 
                        "Digital devices" = df_Q3()$PC, 
                        "Internet" = df_Q3()$Internet)
                        })
    ##Reactive labels and popups
    labels_Q3 <- reactive({switch(input$inputvar_Q3, 
              "Digital devices" = sprintf(
                "<strong>%s</strong><br/>%g percent<br/>%g thousands surveyed",
                df_Q3()$name, df_Q3()$PC,
                round(df_Q3()$Total/1000,1)) %>% lapply(htmltools::HTML), 
              "Internet" = sprintf(
                "<strong>%s</strong><br/>%g percent<br/>%g thousands surveyed",
                df_Q3()$name, df_Q3()$Internet,
                round(df_Q3()$Total/1000,1)) %>% lapply(htmltools::HTML)
                   )
              })
    #
    popups_Q3 <- reactive({switch(input$inputvar_Q3, 
              "Digital devices" = paste0("<strong> State: </strong>", 
                                         df_Q3()$name,"<br/>", "<strong> Percent: </strong>",
                                         df_Q3()$PC,"<br/>", "<strong> Survey sample size (in thousands): </strong>",
                                         round(df_Q3()$Total/1000,1)),
              "Internet" = paste0("<strong> State: </strong>", 
                                  df_Q3()$name,"<br/>", "<strong> Percent: </strong>",
                                  df_Q3()$Internet,"<br/>", "<strong> Survey sample size (in thousands): </strong>",
                                  round(df_Q3()$Total/1000,1))
                     )
              })
    ##Create heatmap of selected variables
    output$map_Q3 <- renderLeaflet({
      leaflet(df_Q3(), options = leafletOptions(
              crs = epsg2163,
              zoomControl = FALSE,
              minZoom = 1, maxZoom = 6)) %>%
        addControl(tags$div(
          HTML('<img border="0" 
               alt = "% With Access to Digital Devices and Internet" 
               width="300" height="100"> 
               </a>')
          ), position = "bottomright") %>%
        setView(-90, 35, zoom = 2) %>%
        #addProviderTiles(providers$Stamen.TonerLite) %>% 
        addPolygons(data = df_Q3(),
          fillColor = ~pal_Q3(var_Q3()),
          weight = 1, opacity = 1, color = "white",
          dashArray = "1", fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 1, color = "#666", dashArray = "",
            fillOpacity = 0.7, bringToFront = TRUE),
          label = labels_Q3(),
          popup = popups_Q3()) %>%
         addLegend(pal = pal_Q3_na, values = ~var_Q3(), 
                   labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                   opacity = 0.7, title = NULL,
                   na.label = "", position = "bottomright") 
      })
    
    ###Map Q1----
    ##Reactive filter for map data 
    df_Q1 <- reactive({sp::merge(states, Q1() %>% 
             mutate(Week_end = as.Date(Week_end, format = "%Y-%m-%d")) %>% 
             filter(Week_end == input$inputvar_Round) %>% 
             filter(Dimension == input$inputvar_Dimension),
                                 by.x = "name", by.y = "state",
                                 duplicateGeoms = TRUE) 
    })
    observe({
      updateSelectInput(session, "inputvar_Dimension",
                        choices = unique(Q3()$Dimension)
      )})
    observe({
      updateSelectInput(session, "inputvar_Round",
          choices = sort(unique(Q3()$Week_end),
          decreasing = TRUE)
      )})
    
    ##Reactive variable selector
    var_Q1 <- reactive({switch(input$inputvar_Q1, 
                               "None" = df_Q1()$`None`, 
                               "1-3 days" = df_Q1()$`1-3 days`,
                               "4 or more days" = df_Q1()$`4 or more days`)
    })
    labels_Q1 <- reactive({switch(input$inputvar_Q1, 
                                  "None" = sprintf(
                                    "<strong>%s</strong><br/>%g percent<br/>%g thousands surveyed",
                                    df_Q1()$name, df_Q1()$`None`,
                                    round(df_Q1()$Total/1000,1)) %>% lapply(htmltools::HTML), 
                                  "1-3 days" = sprintf(
                                    "<strong>%s</strong><br/>%g percent<br/>%g thousands surveyed",
                                    df_Q1()$name, df_Q1()$`1-3 days`,
                                    round(df_Q1()$Total/1000,1)) %>% lapply(htmltools::HTML),
                                  "4 or more days" = sprintf(
                                    "<strong>%s</strong><br/>%g percent<br/>%g thousands surveyed",
                                    df_Q1()$name, df_Q1()$`4 or more days`,
                                    round(df_Q1()$Total/1000,1)) %>% lapply(htmltools::HTML)
    )
    })
    #
    popups_Q1 <- reactive({switch(input$inputvar_Q1, 
                                  "None" = paste0("<strong> State: </strong>", 
                                           df_Q1()$name,"<br/>", "<strong> Percent: </strong>",
                                           df_Q1()$`None`,"<br/>", "<strong> Sample size (in thousands): </strong>",
                                           round(df_Q1()$Total/1000,1)),
                                  "1-3 days" = paste0("<strong> State: </strong>", 
                                                      df_Q1()$name,"<br/>", "<strong> Percent: </strong>",
                                                      df_Q1()$`1-3 days`,"<br/>", "<strong> Sample size (in thousands): </strong>",
                                                      round(df_Q1()$Total/1000,1)),
                                  "4 or more days" = paste0("<strong> State: </strong>", 
                                                            df_Q1()$name,"<br/>", "<strong> Percent: </strong>",
                                                            df_Q1()$`4 or more days`,"<br/>", "<strong> Sample size (in thousands): </strong>",
                                                            round(df_Q1()$Total/1000,1))
    )
    })
    ##Create heatmap of selected variables
    output$map_Q1 <- renderLeaflet({
      leaflet(df_Q1(), options = leafletOptions(
        crs = epsg2163,
        zoomControl = FALSE,
        minZoom = 1, maxZoom = 6)) %>%
        addControl(tags$div(
          HTML('<img border="0" 
               alt = "% With Frequent Live Contact With Teachers" 
               width="100" height="100"> 
               </a>')
        ), position = "bottomright") %>%
        setView(-90, 35, zoom = 2) %>%
        #addProviderTiles(providers$Stamen.TonerLite) %>% 
        addPolygons(data = df_Q1(),
                    fillColor = ~pal_Q3(var_Q1()),
                    weight = 1, opacity = 1, color = "white",
                    dashArray = "1", fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 1, color = "#666", dashArray = "",
                      fillOpacity = 0.7, bringToFront = TRUE),
                    label = labels_Q1(),
                    popup = popups_Q1()) %>%
        addLegend(pal = pal_Q3_na, values = ~var_Q1(), 
                  opacity = 0.7, title = NULL,
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                  na.label = "", position = "bottomright") 
    })
}

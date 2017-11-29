server <- function(input, output, session) {
  
  ################ Set Up Reactives ###########################
  # used to filter companies by industry types
  industry_filt <- reactive({
    industry_data[industry_data$IndGroup %in% input[["sic"]], ]
  })
  
  # reactive to toggle raster
  raster_tog <- reactive({input[["raster_toggle"]]})
  
  # used to choose which column measure to display
  col_number <- reactive({
    which(names(landings_to_port) == input[["landings_measure"]])
  })
  
  
  ############# initial leaflet render #########################
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        minZoom = 6,
        maxZoom = 13
      )
    ) %>%
      setView(lat = 55, lng = -3.8, zoom = 6)
  })
  
  
  ############## change background map using radio buttons #################
  observeEvent({
    input[["raster_toggle"]]
  }, {
    proxy <- 
      leafletProxy("map") %>% 
      clearTiles() %>% 
      addProviderTiles("Stamen.TonerLite")
    
    proxy <- withProgress({
      if (raster_tog() == 1) {
        proxy
      } else if (raster_tog() == 2) {
        proxy %>% 
          addRasterImage(indu_empprop, opacity = 0.5)
      } else if (raster_tog() == 3) {
        proxy %>% 
          addRasterImage(aqua_empprop, opacity = 0.5)
      } else if (raster_tog() == 4) {
        proxy %>% 
          addRasterImage(mari_empprop, opacity = 0.5)
      } else if (raster_tog() == 5) {
        proxy %>% 
          addRasterImage(prod_empprop, opacity = 0.5)
      } else if (raster_tog() == 6) {
        proxy %>% 
          addRasterImage(proc_empprop, opacity = 0.5)
      } else if (raster_tog() == 7) {
        proxy %>% 
          addRasterImage(reta_empprop, opacity = 0.5)
      } else if (raster_tog() == 8) {
        proxy %>% 
          addRasterImage(whol_empprop, opacity = 0.5)
      }},
      message = "Rendering raster..."
    )
  }, 
  once = FALSE
  )
  
  ########### toggle SIC markers from checkbox group controls
  map_zoom <- reactive({
    input$map_zoom
  })
  
  observeEvent({
    input[["sic"]]
    input[["landings_measure"]]
    industry_filt()
    map_zoom()
  }, {
    
    labels <- sprintf(
      paste0("<strong>%s</strong><br/>Landed Weight (tonnes): ",
             "%s<br/>Total Value (£): %s"),
      landings_to_port$Port, landings_to_port[, "Landed.Weight"],
      landings_to_port$Value
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorFactor(
      RColorBrewer::brewer.pal(
        n = length(levels(industry_data$IndGroup)), "Dark2"
      ), 
      industry_data$IndGroup
    )
    
    proxy <- 
      leafletProxy("map") %>% 
      clearMarkers() %>%
      addCircleMarkers(
        lng = na.omit(industry_filt()$longitude), 
        lat = na.omit(industry_filt()$latitude),
        radius = 3, 
        fillOpacity = 0.8, 
        weight = 0.5,
        color = pal(industry_filt()$IndGroup),
        options = pathOptions(clickable = FALSE)
      ) %>%
      addCircles(
        lng = na.omit(landings_to_port$Longitude), 
        lat = na.omit(landings_to_port$Latitude),
        radius = if (input[["landings_measure"]] == "Landed.Weight") {
          (13 - map_zoom()) * 300 * 
            (1 + (log(landings_to_port[, col_number()] + 1)))
        } else {
          ((13 - map_zoom()) / 2) * 300 * 
            (1 + log(landings_to_port[, col_number()]))
        },
        fillOpacity = 0.01,
        weight = 1.5,
        stroke = TRUE,
        layerId = landings_to_port$Port, 
        color = "cornflowerblue",
        label = labels,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.01,
          bringToFront = TRUE
        )
      )
    proxy
  },
  ignoreInit = FALSE, 
  once = FALSE
  )
  
  ############# create a list called clicked_markers when to store clicked ports
  clicked_markers <- reactiveValues(ports = list())
  
  # observer event for circle marker clicks
  observeEvent(
    input[["map_shape_click"]],
    {
      # handle NULL clicks
      if (is.null(input[["map_shape_click"]]["id"])) {
        return(NULL)
      } else if (input[["map_shape_click"]]["id"] == "company") { 
      } else {
        click <- input[["map_shape_click"]]["id"]
        # if a port was clicked get the id then...
        # either add or remove from clicked_marker list (which subsequently adds 
        # or removes from sankey diagram)
        if (click$id %in% clicked_markers$ports) {
          if (length(clicked_markers$ports) > 1) {
            clicked_markers$ports <- 
              clicked_markers$ports[clicked_markers$ports != click$id]
          } else {
            clicked_markers$ports <- clicked_markers$ports
          }
        } else {
          clicked_markers$ports <- unique(c(clicked_markers$ports, click$id))
        }
        
      }
      # update port selection box from map click
      updateSelectizeInput(
        session,
        "port_sel",
        choices = c(Choose = "", unique(landings_data$Port)),
        selected = c(input[["port_sel"]], click))
    }
  )
  
  filtered_ports <- reactive({
    c(input[["port_sel"]], click)
  })
  
  ######### build sankey based on port clicks - group lower populated spiecies 
  # into "Other" category controlled by the slider
  observeEvent({
    filtered_ports()
    input[["landings_measure"]]
    input[["sankey_to"]]
    input[["slider"]]
  }, {
    # get landings data for selected ports
    select_ports <- landings_data[landings_data$Port %in% filtered_ports(), ]
    
    # get the measure name from the measure drop down
    measure_name <- input[["landings_measure"]]
    to_name <- input[["sankey_to"]]
    
    # aggregate data by sankey_to_name for measure_name sums
    select_ports$To <- select_ports[, to_name]
    select_ports$measure <- select_ports[, measure_name]
    aggs <- 
      select_ports %>% 
      group_by(Port, To) %>% 
      summarise(Sum = sum(measure))
    
    # sum of the whole measure - used to group small values into other category
    total_measure <- sum(aggs$Sum)
    
    # proportion below which to_name is grouped into the "Other" category
    qualifying_prop <- total_measure * (input[["slider"]] / 100)
    
    aggs_lookup <- 
      aggs %>% 
      group_by(To) %>%
      summarise(grouped_sum = sum(Sum))
    
    aggs_lookup$to_new <- aggs_lookup$To
    aggs_lookup$to_new[aggs_lookup$grouped_sum < qualifying_prop] <- "Other"
    aggs <- left_join(aggs, aggs_lookup, by = "To")
    
    aggs <- 
      aggs %>% 
      group_by(Port, to_new) %>% 
      summarise(measure = sum(Sum))
    
    # Render the Sankey chart
    output$sankey_cap <- renderUI({
      if (input[["landings_measure"]] == "Landed.Weight") {
        h5("Figure 2: Sankey Diagram of Landed Weight (tonnes)")
      } else {
        h5("Figure 2: Sankey Diagram of Value (£)")
      }
    })
    output$sankey <- renderGvis(
      gvisSankey(
        data = aggs[, c("Port", "to_new", "measure")],
        from = "Port",
        to = "to_new",
        weight = "measure",
        options = list(
          width = "100%",
          sankey = "node: {label: { colour: '#000000'}}"
        )
      )
    )
    
    landings_data_agg <-landings_data %>% group_by(Port) %>%
      summarise(Landed_Weight = sum(Landed.Weight),
                Landed_Value = sum(Value))
    
    output$dt <- DT::renderDataTable({
      dt_data <- landings_data_agg[
        landings_data_agg$Port %in% filtered_ports(),
        c("Port", "Landed_Weight", "Landed_Value")
        ] %>%
        rename(
          "Landed Weight (tonnes)" = "Landed_Weight",
          "Value (£)" = "Landed_Value"
        ) 
      datatable(
        dt_data, 
        rownames = FALSE,
        options = list(
          filter = "none",
          pageLength = 5,
          dom = "ltip"
        )
      )
    })
  }, 
  ignoreInit = TRUE
  )
}
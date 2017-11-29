header <- dashboardHeader(
  title = "The UK Fishing Industry"
)

sidebar <- dashboardSidebar(
  h4("Map Controls:"),
  selectInput(
    "sic", 
    label = "Company (SIC):",
    choices = c(
      "Marine Fishing" = "A",
      "Freshwater Fishing" = "B",
      "Marine Aquaculture" = "C",
      "Freshwater Aquaculture" = "D"
    )
  ),
  selectInput(
    "raster_toggle", 
    label = "Raster Layer:",
    choices = list(
      "None" = 1,
      "ALL" = 2,
      "Aquaculture" = 3,
      "Marine Fishing" = 4,
      "All Production" = 5,
      "Processing" = 6,
      "Retail" = 7,
      "Wholesale" = 8
    ),
    selected = 1
  ),
  selectInput(
    "landings_measure", 
    label = "Landings Measure:",
    list("Landed Weight" = "Landed.Weight", "Value" = "Value")
  ),
  h4("Sankey Controls:"),
  selectizeInput(
    "port_sel",
    label = "Select up to 5 Ports:",
    choices = c(Choose = "", unique(landings_data[3])),
    multiple = TRUE,
    options = list(
      maxItems = 5,
      plugins = list('remove_button')
    )
  ),
  conditionalPanel(
    condition = "input[['port_sel']] != null",
    selectInput(
      "sankey_to", 
      label = "Breakdown Sankey By:",
      list(
        "Species Name" = "Species_Name",
        "Species Group" = "Species_Group",
        "Vessel Nationality" = "Vessel_Nationality",
        "Month"
      )
    ),
    sliderInput(
      "slider", 
      label = "Proportion (%) to be grouped into 'Other'",
      min = 0.01, 
      max = 10, 
      value = 5
    )
  )
)

body <- dashboardBody(
  fluidRow(
    column(
      width = 6,
      h5(paste0("Figure 1: Fleet Landings Data (sourced from the Marine ",
                "Management Organisation)")),
      leafletOutput("map", height = "800px")
    ),
    column(
      width = 6,
      # Decide whether to a message about selecting ports to display the 
      # sankey or display a figure caption if the sankey is drawn
      conditionalPanel(
        condition = "input[['port_sel']] == null",
        h4("Select a port(s) to view the sankey plot")
      ),
      conditionalPanel(
        condition = "input[['port_sel']] != null",
        uiOutput("sankey_cap")
      ),
      htmlOutput("sankey"),
      conditionalPanel(
        condition = "input[['port_sel']] != null",
        h5(
          paste0(
            "Table 1: Landed Weight (tonnes) and Value (£) by Port, Year, ",
            "Month, Species Group and Species Name"
          )
        ),
        DT::dataTableOutput("dt")
      )
    )
  )
)

dashboardPage(header, sidebar, body)
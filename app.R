r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
install.packages("tidyverse")
install.packages("tigris")
install.packages("shiny")
install.packages("plotly")
library(tigris)
library(tidyverse)
library(mapdata)
library(stringr)
library(sf)
library(shiny)
library(plotly)
library(scales)
library(viridis)

Sys.setlocale("LC_ALL", "C")

store_data <- as.data.frame(read.csv("superstore_final_dataset (1).csv"))

store_by_state <- store_data |>
  group_by(State) |>
  summarize(total_state_sales = sum(Sales))

store_by_zip <- store_data |>
  group_by(Postal_Code) |>
  summarize(total_zip_sales = sum(Sales))

store_data_cat <- store_data |>
  group_by(Category) |>
  summarise(cat_sales = sum(Sales))

store_data_subcat <- store_data |>
  group_by(Sub_Category) |>
  summarise(subcat_sales = sum(Sales))


store_data_region <- store_data |>
  group_by(Region) |>
  summarise(region_sales = sum(Sales))

store_data$Order_Date <- format(as.Date(store_data$Order_Date, format = '%d/%m/%Y'),'%Y-%m-%d')

store_data_order_date <- store_data |>
  group_by(Order_Date) |>
  summarize(sales_by_OD = sum(Sales)) |>
  mutate(sum_sales_OD = cumsum(sales_by_OD))

reg_cat_sales <- store_data |>
  group_by(Region, Category) |>
  summarize(region_cat_sales = sum(Sales))

reg_subcat_sales <- store_data |>
  group_by(Region, Category, Sub_Category) |>
  summarize(regional_subcat_sales = sum(Sales))

od_cat <- store_data |>
  group_by(Order_Date, Category) |>
  summarize(cat_date_sales = sum(Sales))


store_data <- left_join(store_data, store_by_state, by = "State")
store_data <- left_join(store_data, store_by_zip, by = "Postal_Code")
store_data <- left_join(store_data, store_data_cat, by = "Category")
store_data <- left_join(store_data, store_data_subcat, by = "Sub_Category")
store_data <- left_join(store_data, store_data_region, by = "Region")
store_data <- left_join(store_data, store_data_order_date, by = "Order_Date")
store_data <- left_join(store_data, reg_cat_sales, by = c("Region", "Category"))
store_data <- left_join(store_data, reg_subcat_sales, by = c("Region", "Category", "Sub_Category"))
store_data <- left_join(store_data, od_cat, by = c("Order_Date", "Category"))

us_states <- states(cb = TRUE, resolution = "20m")

us_states_shifted <- shift_geometry(us_states) %>%
  mutate(lon = st_coordinates(st_centroid(.))[,1],
         lat = st_coordinates(st_centroid(.))[,2],
         lat = case_when(NAME == "Mississippi" ~ lat -0.5e5,
                         NAME == "Alabama" ~ lat + 0.5e5,
                         NAME == "Illinois" ~ lat -0.5e5,
                         NAME == "Indiana" ~ lat + 0.5e5,
                         .default = lat)) |>
  rename(State = NAME)

move_labels <- c("Connecticut", "Delaware",
                 "Maryland", "Massachusetts", "New Hampshire", "New Jersey",
                 "Rhode Island", "Vermont")

move_states <- us_states_shifted %>%
  filter(State %in% move_labels) %>%
  arrange(lat) %>%
  mutate(xend = 2.2e6,
         yend = seq(min(lat)-2.5e5, max(lat), length.out = n()))

store_data <- left_join(store_data, us_states_shifted, by = "State")


uszips <- zctas(cb = TRUE, year = 2020)

uszips <- uszips |>
  rename(zip_geometry = geometry)

store_data$Postal_Code <- as.character(store_data$Postal_Code)

store_data <- left_join(store_data, uszips, by = c("Postal_Code" = "ZCTA5CE20"))


sd_plotting <- as.data.frame(store_data)
sd_plotting$geometry <- NULL
sd_plotting$zip_geometry <- NULL



ui <- fluidPage(
  tabsetPanel(
    tabPanel("Welcome",
             tags$h1("Analyze A Sales Dataset"),
             tags$p("Hello and thank you for taking interest in this Shiny App I have created
                     to analyze some sample sales data!  The data used in this app can be found here:"),
             tags$a(href="https://www.kaggle.com/datasets/bhanupratapbiswas/superstore-sales/data",
                    "Superstore Sales Data"),
             tags$br(),
             tags$b(),
             tags$p("The main purpose of this app is to demonstrate general functionality of the Shiny App environment,
                     and the capability of myself to build something useful within it.  The superstore sales data
                     is a fairly simple dataset, however the functionality presented should be translatable to
                     more complex data from your organization."),
             tags$p("The data is broken down in two ways: location and product category.  Each observation is a product sale 
                     which includes a state and zip code, as well as a product category, sub category, and the product's 
                     name and sale price. The app utilizes the following logic:  Categories determine sub categories -> Sub categories
                     determine states -> States determine zip codes."),
             tags$p("Begin by selecting a category, clicking the 'Set Category' button
                     and then selecting a sub category. After making a category selection, you set your inputs for the next selection 
                     (sub category, state, zip code) by a simple click of the appropriate button.  For example, selecting the category 
                     'Furniture' and clicking the 'Set Category' button, will yield only the Furniture sub categories to select from.
                     National data requires only a category 
                     and sub category selection.  State data requires a state selection.  Zip code data requires a state and zip code 
                     selection. Inputs are reset by selecting new categories and clicking the 'Set Category' button. You can select 
                     multiple inputs of each selection. 
                     When you have made your input selections, Visualizations are produced by clicking the 'Fetch Data' button."),
             tags$p("This app is part 1 of a three part module titled", tags$strong("'Practical Applications of the R language for 
                    Sales Management'."), "You can view the entirety of the module here at:"),
             tags$a(href="https://github.com/kylestone225", "My GitHub Repository"),
             tags$b(),
             tags$p("I hope you enjoy the app and find it useful!  I really enjoy coding and analyzing data using R.
                    Utilizing analytical models and programming concepts is a powerful way to protect sales strategy from subjectivity.
                    I look forward to discussing how I can help apply these methods within your organization, and make your 
                    sales department achieve more productive successes.")
    ),
    tabPanel("National Data",
             fluidRow(
               column(6,
                      selectInput("natcat", "Category", unique(store_data$Category), multiple = TRUE)
               ),
               column(6,
                      dateRangeInput("natorder", "Select Date Range", start = min(store_data$Order_Date), end = max(store_data$Order_Date))
               )
             ),
             actionButton("natcatset", "Set Category", class = "btn-sm btn-primary"),
             fluidRow(
               column(6,
                      selectInput("natsubcat", "Sub Category", unique(store_data$Sub_Category), multiple = TRUE)
               ),
               column(6,
                      selectInput("streg", "Select Map View", c("State", "Region", "Zip Code"), selected = "State")
               )
             ),
             fluidRow(
               column(6,
                      actionButton("natfetch", tags$strong("Fetch Data"), class = "btn-sm btn-success")
               ),
               column(6,
                      conditionalPanel(
                        "input.streg === 'Zip Code'",
                        style = "display: none;",
                        tags$h4("Set Coordinates To Zoom"),
                        tags$hr(),
                        sliderInput("lon", "Select Longitude",value = c(-123, -73), min = -123, max = -73),
                        sliderInput("lat", "Select Latitude", value = c(25, 50), min = 25, max = 50))
               )
             ),
             plotOutput("natplot", width = "1400px", height = "600px"),
             fluidRow(
               column(12,
                      plotlyOutput("natcol")
               )
             ),
             fluidRow(
               column(12,
                      plotOutput("natscat", brush = "natscat_brush")
               )
             ),
             column(12,
                    tableOutput("nattable")
             )
             
    ),
    tabPanel("State Data",
             fluidRow(
               column(6,
                      selectInput("stcat", "Category", unique(store_data$Category), multiple = TRUE)
                      ),
               column(6,
                      dateRangeInput("storder", "Select Date Range", start = min(store_data$Order_Date), end = max(store_data$Order_Date))
               )
             ),
             actionButton("stcatset", "Set Category", class = "btn-sm btn-primary"),
             fluidRow(
               column(6,
                      selectInput("stsubcat", "Sub Category", unique(store_data$Sub_Category), multiple = TRUE)
                      ),
               column(6,
                      selectInput("state", "Select State", unique(store_data$State), multiple = TRUE)
                      )
             ),
             fluidRow(
               column(6,
                      actionButton("stset", "Set States", class = "btn-sm btn-primary")
                      ),
               column(6,
                      actionButton("stfetch", tags$strong("Fetch State Data"), class = "btn-sm btn-success")
                      )
             ),
             plotOutput("stplot"),
             fluidRow(
               column(12,
                      plotlyOutput("sttcol")
               )
             ),
             fluidRow(
               column(12,
                      plotOutput("stscat", brush = "stscat_brush")
               )
             ),
             column(12,
                    tableOutput("sttable")
                    )
        
    ),
    tabPanel("Zip Code Data",
             fluidRow(
               column(6,
                      selectInput("zipcat", "Category", unique(store_data$Category), multiple = TRUE)
                      ),
               column(6,
                      dateRangeInput("ziporder", "Select Date Range", start = min(store_data$Order_Date), end = max(store_data$Order_Date))
                      )
               ),
               actionButton("zipcatset", "Set Category", class = "btn-sm btn-primary"),
             fluidRow(column(6,
                             selectInput("zipsubcat", "Sub Category", unique(store_data$Sub_Category), multiple = TRUE)
                             ),
                      column(6,
                             selectInput("statezip", "Select State", unique(store_data$State), multiple = TRUE)
                      )
             ),
             fluidRow(
               column(6,
                      actionButton("stzipset", "Set States", class = "btn-sm btn-primary")
                      ),
               column(6,
                      actionButton("zipset", "Set Zip Codes", class = "btn-sm btn-primary"))
             ),
             fluidRow(
               column(6,
                      selectInput("zips", "Select Zip Codes", unique(store_data$Postal_Code), multiple = TRUE),
                      actionButton("zipfetch", tags$strong("Fetch Zip Code Data"), class = "btn-sm btn-success")
                      )
             ),
             plotOutput("zipplot"),
             fluidRow(
               column(12,
                      plotlyOutput("zipcol")
               )
             ),
             fluidRow(
               column(12,
                      plotOutput("zipscat", brush = "zipscat_brush")
               )
             ),
             column(12,
                    tableOutput("ziptable")
             )
    )
  )
)

server <- function(input, output, session){
  #create reactive data from main data
  store_values <- reactiveValues(
    df = data.frame()
  )
  
  store_values$df <- as.data.frame(store_data)
  
  #Filter the category selection
  nat_cat <- eventReactive(input$natcatset,{
    filter(store_values$df, Category %in% input$natcat)
  })
  #Update the Subcategory selection with available sub categories from above
  observeEvent(input$natcatset,{
    updateSelectInput(session, "natsubcat", "Sub Category", unique(nat_cat()$Sub_Category))
  })
  #Final filter collecting subcat and date range info and filtering.
  nat_sub_cat <- eventReactive(input$natfetch,{
    filter(nat_cat(), Sub_Category %in% input$natsubcat & Order_Date > input$natorder[1] & Order_Date < input$natorder[2])
  })
  #toggle your map view
  streg <- reactive(input$streg)
  #map plots
  output$natplot <- renderPlot({
    if(streg() == "State"){
      nat_sub_cat() |>
        group_by(State, geometry) |>
        summarize(tot_st_sales = sum(Sales)) |>
        ggplot() +
        geom_sf(aes(fill = tot_st_sales, color = "grey", geometry = geometry), show.legend = FALSE) +
        scale_fill_distiller("tot_st_sales", palette="Spectral") +
        geom_label(data = nat_sub_cat() |>
                     group_by(State, geometry, lon, lat) |>
                     summarize(tot_st_sales = sum(Sales)) |>
                     filter(!State %in% move_labels),
                   aes(x = lon, y = lat,
                       label = dollar(tot_st_sales)),
                   fill = "white",
                   size = 4) +
        geom_label(data = nat_sub_cat() |>
                     group_by(State, geometry) |>
                     summarize(tot_st_sales = sum(Sales)) |>
                     merge(move_states, by = c("State", "geometry")),
                   aes(x = xend, y = yend,
                       label = dollar(tot_st_sales)),
                   fill = "white",
                   size = 4,
                   hjust = 0) +
        geom_segment(data = move_states, 
                     aes(lon, lat, xend = xend, yend = yend),
                     colour = "grey60",
                     linewidth = 0.3) +
        coord_sf(clip = "off") +
        labs(title = "Total Sales By State Based On Your Selection") +
        scale_color_identity(guide = "legend") +
        scale_size_identity(guide = "legend") +
        theme_classic()
    }else{
      if(streg() == "Region"){
        nat_sub_cat() |>
          ggplot() +
          geom_sf(aes(fill = region_sales, geometry = geometry)) +
          labs(title = "Total Sales By Region") +
          scale_fill_distiller("region_sales", palette="Spectral", label = label_comma()) +
          scale_color_identity(guide = "legend") +
          theme_classic()
      }else{
        if(streg() == "Zip Code"){
          nat_sub_cat() |>
            group_by(Postal_Code, zip_geometry) |>
            summarize(tot_z_sales = sum(Sales)) |>
            ggplot() +
            geom_sf(aes(fill = tot_z_sales, geometry = zip_geometry)) +
            labs(title = "Zip Codes Containing Sales of Your Selections") +
            scale_fill_distiller("Sales By Zip", palette="Spectral", label = label_comma()) +
            coord_sf(xlim = c(input$lon[1], input$lon[2]), ylim = c(input$lat[1], input$lat[2])) +
            theme_classic()
        }
      }
    }
  })
  #SF geoms are lists and not conducive to the interactive features of shiny. Below an identical
  #set of data without the SF geoms is filtered for plotting the blow charts and the brush table.
  plot_values <- reactiveValues(
    df = data.frame()
  )
  
  plot_values$df <- as.data.frame(sd_plotting)
  
  plot_cat <- eventReactive(input$natcatset,{
    filter(plot_values$df, Category %in% input$natcat)
  })
  
  plot_sub_cat <- eventReactive(input$natfetch,{
    filter(plot_cat(), Sub_Category %in% input$natsubcat & Order_Date > input$natorder[1] & Order_Date < input$natorder[2])
  })
  
  #Scatter chart used for your brushed points
  output$natscat <- renderPlot({
    plot_sub_cat() |>
      ggplot(aes(x = Order_Date, y = Sales, colour = Category)) +
      geom_point() +
      labs(title = "Quickly Visualize Your Largest Sales",
           subtitle = "Click and Drag Your Cursor Around a Group of Points to View Data") +
      theme_classic() +
      theme(axis.text.x = element_blank())
    
  })
  
  #Plotly geom_col chart
  output$natcol <- renderPlotly({
    plot_sub_cat() |>
      ggplot(aes(Category, Sales, fill = Sub_Category)) +
      geom_col(aes(label = Product_Name), show.legend = FALSE) +
      labs(title = "Total Sales By Category: Hover To View") +
      xlab("") + ylab("") +
      scale_y_continuous(labels = label_comma()) +
      theme_classic()
  })
  # Brushed points
  output$nattable <- renderTable({
    brushedPoints(plot_sub_cat()[c(3,7,10,11,15:18)], brush = input$natscat_brush)
  })    
  
  #State level data
  #Filter the category selection
  st_cat <- eventReactive(input$stcatset,{
    filter(store_values$df, Category %in% input$stcat)
  })
  #Update the Subcategory selection with available sub categorys from above
  observeEvent(input$stcatset,{
    updateSelectInput(session, "stsubcat", "Sub Category", unique(st_cat()$Sub_Category))
  })
  
  st_subcat <- eventReactive(input$stset,{
    filter(st_cat(), Sub_Category %in% input$stsubcat)
  })
  
  observeEvent(input$stset,{
    updateSelectInput(session, "state", "Select State", unique(st_subcat()$State))
  })
  #Final filter collecting subcat and date range info and filtering.
  st_sub_final <- eventReactive(input$stfetch,{
    filter(st_subcat(), State %in% input$state & Order_Date > input$storder[1] & Order_Date < input$storder[2])
  })
  
  # Plot of selected state
  output$stplot <- renderPlot({
    st_sub_final() |>
      group_by(State, geometry) |>
      summarize(tot_st_sales = sum(Sales)) |>
      ggplot() +
      geom_sf(aes(fill = tot_st_sales, color = "grey", geometry = geometry), show.legend = FALSE) +
      geom_sf_text(aes(label = dollar(tot_st_sales), geometry = geometry, size = 4),
                   show.legend = FALSE) +
      labs(title = "Total Sales By State Based On Your Selection") +
      scale_fill_distiller("tot_st_sales", palette="Spectral") +
      scale_color_identity(guide = "legend") +
      scale_size_identity(guide = "legend") +
      theme_void()
  })
  
  #SF geoms are lists and not conducive to the interactive features of shiny. Below an identical
  #set of data without the SF geoms is filtered for plotting the blow charts and the brush table.
  
  
  st_plot_cat <- eventReactive(input$stcatset,{
    filter(plot_values$df, Category %in% input$stcat)
  })
  
  st_plot_sub_cat <- eventReactive(input$stfetch,{
    filter(st_plot_cat(), Sub_Category %in% input$stsubcat &
             Order_Date > input$storder[1] &
             Order_Date < input$storder[2] &
             State %in% input$state)
  })
  
  
  # Plotly geom_col
  output$sttcol <- renderPlotly({
    st_plot_sub_cat() |>
      ggplot(aes(Category, Sales, fill = Sub_Category)) +
      geom_col(aes(label = Product_Name), show.legend = FALSE) +
      labs(title = "Total Sales By Category: Hover To View") +
      xlab("") + ylab("") +
      scale_y_continuous(labels = label_comma()) +
      theme_classic()
  })
  
  # brushed points
  output$stscat <- renderPlot({
    st_plot_sub_cat() |>
      ggplot(aes(x = Order_Date, y = Sales, colour = Category)) +
      geom_point() +
      labs(title = "Quickly Visualize Your Largest Sales",
           subtitle = "Click and Drag Your Cursor Around a Group of Points to View Data") +
      theme_classic() +
      theme(axis.text.x = element_blank())
  })
  
  # brushed points table
  output$sttable <- renderTable({
    brushedPoints(st_plot_sub_cat()[c(3,7,10,11,15:18)], brush = input$stscat_brush)
  })
  
  # OK lets do our final page for zip code data
  #Filter the category selection
  zip_cat <- eventReactive(input$zipcatset,{
    filter(store_values$df, Category %in% input$zipcat)
  })
  #Update the Subcategory selection with available sub categorys from above
  observeEvent(input$zipcatset,{
    updateSelectInput(session, "zipsubcat", "Sub Category", unique(zip_cat()$Sub_Category))
  })
  
  zip_subcat <- eventReactive(input$stzipset,{
    filter(zip_cat(), Sub_Category %in% input$zipsubcat)
  })
  
  observeEvent(input$stzipset,{
    updateSelectInput(session, "statezip", "Select State", unique(zip_subcat()$State))
  })
  
  zip_subcat_state <- eventReactive(input$zipset,{
    filter(zip_subcat(), State %in% input$statezip)
  })
  
  observeEvent(input$zipset,{
    updateSelectInput(session, "zips", "Select Zip Codes", unique(zip_subcat_state()$Postal_Code))
  })
  
  
  #Final filter collecting subcat and date range info and filtering.
  zip_sub_final <- eventReactive(input$zipfetch,{
    filter(zip_subcat_state(), Order_Date > input$storder[1] &
             Order_Date < input$storder[2] &
             Postal_Code %in% input$zips)
  })
  
  #plot zip codes
  output$zipplot <- renderPlot({
    zip_sub_final() |>
      group_by(Postal_Code, zip_geometry) |>
      summarize(tot_z_sales = sum(Sales)) |>
      ggplot() +
      geom_sf(aes(fill = tot_z_sales, geometry = zip_geometry), show.legend = FALSE) +
      geom_sf_text(aes(label = dollar(tot_z_sales), geometry = zip_geometry, size = 4),
                   show.legend = FALSE) +
      labs(title = "Zip Codes Containing Sales of Your Selections") +
      scale_fill_distiller("tot_z_sales", palette="Spectral") +
      scale_color_identity(guide = "legend") +
      theme_void()
  }) 
  
  # zip code plotting data without the geom lists
  zip_plot_cat <- eventReactive(input$zipcatset,{
    filter(plot_values$df, Category %in% input$zipcat)
  })
  
  zip_plot_sub_cat <- eventReactive(input$zipfetch,{
    filter(zip_plot_cat(), Sub_Category %in% input$zipsubcat &
             Order_Date > input$ziporder[1] &
             Order_Date < input$ziporder[2] &
             Postal_Code %in% input$zips)
  })
  
  
  # PLotly columns
  output$zipcol <- renderPlotly({
    zip_plot_sub_cat() |>
      ggplot(aes(Category, Sales, fill = Sub_Category)) +
      geom_col(aes(label = Product_Name), show.legend = FALSE) +
      labs(title = "Total Sales By Category: Hover To View") +
      xlab("") + ylab("") +
      scale_y_continuous(labels = label_comma()) +
      theme_classic()
  })
  
  # Brushed zip points
  output$zipscat <- renderPlot({
    zip_plot_sub_cat() |>
      ggplot(aes(x = Order_Date, y = Sales, colour = Category)) +
      geom_point() +
      labs(title = "Quickly Visualize Your Largest Sales",
           subtitle = "Click and Drag Your Cursor Around a Group of Points to View Data") +
      theme_classic() +
      theme(axis.text.x = element_blank())
  })
  
  # Brushed table
  output$ziptable <- renderTable({
    brushedPoints(zip_plot_sub_cat()[c(3,7,10,11,15:18)], brush = input$zipscat_brush)
  })
  
  
}

shinyApp(ui, server)



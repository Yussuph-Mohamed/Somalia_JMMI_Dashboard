### PROJECT:  Somalia JMMI
### PURPOSE:  Analyse JMMI data
### INPUT:    jmmi_data.csv
### OUTPUT:   objects for Shiny
### AUTHOR:   Tie Franco Brotto, Ahmed Mahamoud
### LAST UPDATED: 26 October, 2020

# clean memory
rm(list=ls())

# load required packages
library(shiny)
library(tidyverse)
library(data.table)
library(openxlsx)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(plotly)
library(highcharter)
library(lubridate) #for some reason sometimes this has to be called manually
library(formattable)
library(reshape2)
library(ggthemes)
library(extrafont)
library(ggrepel)
library(kableExtra)
library(gridExtra)
library(shinyBS)

# Set REACH colors
cc_red <- "#ee5859"
cc_lightred <- "#fbdada"
cc_grey <- "#58595a"
cc_midgrey <- "#acacad"
cc_lightgrey <- "#d1d3d4"
cc_bg <- "#e4e6e8"
cc_gr1 <- "#e6ced0"
cc_gr2 <- "#e7b7b8"
cc_gr3 <- "#e99fa1"
cc_gr4 <- "#eb8789"
cc_green <- "#37a76f"
cc_lightgreen <- "#a5c9a1"

# load somalia admin2 shapefile (with only targeted locations!)
som_02 <- st_read("data/som_adm2.shp")

# Load JMMI data
df_base <- read.csv("www/jmmi_data.csv", stringsAsFactors = FALSE, header = T, na.strings = c("", " ", "NA"))

# merging tabular and spatial data
jmmi_df <- merge(x=som_02, y=df_base, by.x="admin2Name", by.y="call_location")
names(jmmi_df)[names(jmmi_df) == 'admin2Name'] <- "call_location"

# Make tibble copy
df <- as_tibble(df_base)

# List of select multiple questions
ll_locations0 <- df %>%
    distinct(call_location) %>%
    pull(call_location)
ll_locations <- c("all", ll_locations0)

# UI ---------------------------------------------
ui <- fluidPage(
    tags$head(includeCSS("www/style.css")),
    
    wellPanel(
        style = "background: #FFFFFF;",
        titlePanel(title = fluidRow(column(
            12,
            HTML(
                "<h1 align = 'left'>SOMALIA &#149; JOINT MARKET MONITORING INITIATIVE</h1>"
            )
        )),
        
        windowTitle = "SOMALIA JMMI test 2020"),
        fluidRow(
            column(
                4,
                h2("ABOUT"),
                p(
                    "The Joint Market Monitoring Initiative (JMMI) is a joint initiative from the Somalia Water, Sanitation and Hygiene (WASH) and Shelter clusters and REACH. It aims to address an information gap in Somalia in terms of regular and updated monitoring of market functionality and a broad range of non-food items (NFIs), while contributing to existing supply chain and price monitoring of the main minimum expenditure basket (MEB) items."
                ),
                p(
                    "Refer to the bottom of this page for more information about the methodology, partners, target item specifications, and other resources"
                )
            ),
            column(
                4,
                h3("Co-leads"),
                imageOutput("lg_colead", height = "auto"),
                h3("Partners"),
                imageOutput("lg_partners", height = "auto"),
                h3("Donor"),
                imageOutput("lg_donor", height = "auto", width = "33%")
            ),
            column(
                4,
                leafletOutput("Map", height = 400),
                p(
                    "The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the JMMI partners"
                )
            )
        ),
        
        fluidRow(column(12,
                        # Horizontal line
                        hr())),
        
        fluidRow(column(
            3,
            h2("LOCATIONS"),
            # Select location
            radioButtons(
                "select_location",
                "Select location",
                choices = ll_locations,
                selected = NULL,
                inline = FALSE
            )
        ),
        
        column(9,
               h2("PRICES"),
               # Price table
               DTOutput("table_price"))),
        
        fluidRow(column(
            12,
            h2("DISTRIBUTION OF PRICES"),
            p(
                "Refer to 'How to read a boxplot' (below) for an explanation about how to read this graph. The targetted item specifications can also be found below. This graph is interactive, you can zoom in and mouse over to get more information, among other tools available at the upper right corner of this graph."
            ),
            # Price boxplot
            plotlyOutput("boxplot")
        )),
        
        fluidRow(column(12,
                        # Horizontal line
                        hr())),
        
        fluidRow(column(9,
                        h2("STOCK"),
                        # Stock table
                        DTOutput("table_stock")),
                 column(
                     3,
                     h3("Key"),
                     HTML(
                         "<p>Stock duration: Median number of days reported by vendors, that current stock of each assessed item is expected to last, assuming that the rate of purchase remains consistent. <em>In the graph below it is represented by the left bar.</em></p>"
                     ),
                     HTML(
                         "<p>Restocking time: Median number of days reported by vendors, that it would take to restock each item assessed, from ordering to delivery in the shop. <em>In the graph below it is represented by the right bar.</em></p>"
                     ),
                     HTML(
                         "<p>Stocking difficulty: Percentage of vendors reporting having experienced difficulty to restock each item assessed, in the 3 months prior to data collection. <em>In the graph below it is represented by the color.</em></p>"
                     )
                 )),
        
        fluidRow(column(
            12,
            h2("STOCK GRAPH"),
            p(
                "Refer to the key (above) for an explanation about how to read this graph. The targetted item specifications can be found below. For stock conditions, items with more than one specification are combined. This graph is interactive, you can zoom in and mouse over to get more information, among other tools available at the upper right corner of this graph."
            ),
            # Stock graph
            plotlyOutput("graph_stock")
        )),
        
        fluidRow(
            
            column(
                8,
                
                fluidRow(column(
                    6,
                    h2("METHODOLOGY"),
                    p(
                        "Primary data is collected quarterly through interviews with market vendors selected purposively from the targeted markets. The clusters' partners are responsible for data collection. In this first round, 491 interviews were collected from 12 locations between August 9-12, 2020. As vendors were selected purposively, findings are not statistically representative. To prevent spread and contraction of COVID-19, data was collected remotely. This situation limited the capacity of enumerators to target specific vendors. In addition, it limited possibilities of follow up with vendors. All findings are indicative only, and only apply to the period within which data was collected. Moreover, item specifications may vary slightly between locations according to different brands available, and comparability between the locations assessed is limited."
                    )
                ),
                column(
                    6,
                    h2("PARTNERS"),
                    p(
                        "The WASH and Shelter clusters are responsible for the identification of partners, among cluster members, willing to contribute to the JMMI. The clusters also lead external coordination with the Humanitarian Country Team (HCT) stakeholders and government actors. Cluster members identified as partners provide data collection capacity according to their access and availability, and support the study with sector-specific expertise. REACH is responsible for leading the tools and analysis framework design, training of partners and technical support for data collection, supporting focal points in managing the field data collection, leads on technical data management and data cleaning, data analysis, and output production. The geographic coverage area is determined by the access and capacity of partners. In order to maximize efficacy, certain markets are prioritized to reflect the areas in which cash transfer programs, particularly focused on NFIs, are planned or ongoing, as well as key supply chains for the main NFIs assessed."
                    )
                )
                ),
                
                fluidRow(column(
                    12,
                    h2("HOW TO READ A BOXPLOT"),
                    imageOutput("img_boxplot", height = "auto")
                )),
                
                fluidRow(column(
                    12,
                    h2("RESOURCES"),
                    HTML("REACH, <a href='https://bit.ly/som-jmmi-20q3'>Joint Market Monitoring Initiative (factsheets)</a><br/>
          REACH, <a href='https://www.reachresourcecentre.info/country/somalia/cycle/29743/#cycle-29743'>Joint Market Monitoring Initiative</a><br/>
          REACH, <a href='https://bit.ly/3eBwBX6'>Market Feasibility Study - Mogadishu</a><br/>
REACH, <a href='https://www.reachresourcecentre.info/country/somalia/cycle/705#cycle-705'>Market Feasibility Studies</a><br/>
REACH, <a href='https://www.reachresourcecentre.info/country/somalia/'>Somalia</a><br/>
CWG, <a href='https://data.humdata.org/visualization/somalia-cash-programing-v3/'>Cash Based Programming in Somalia</a><br/>
CWG, <a href='https://www.humanitarianresponse.info/en/operations/somalia/cash-activities'>Website</a><br/>
FSNAU, <a href='https://www.fsnau.org/sectors/markets'>Markets</a><br/>
WFP VAM, <a href='https://dataviz.vam.wfp.org/Reports_Explorer'>Reports Explorer</a> 
")
                ))
                
            ),
            
            column(
                4,
                h2("TARGET ITEM SPECIFICATIONS"),
                HTML(
                    "<b>Blanket 1 </b>-> 1.5m x 2.0m, polyester/acrylic <br/>
<b>Bowl 1 </b>-> medium, 1 litre, stainless steel <br/>
<b>Brick 1 </b>-> 1 piece, 20cm x 20cm <br/>
<b>Bucket 1 </b>-> 10 litres, plastic, with lid <br/>
<b>Cement 1 </b>-> 1 bag, 50kg <br/>
<b>Chlorine 1 </b>-> 1 box, to clear 10 litres of water, e.g. Aqua Tabs (67m/g) <br/>
<b>Cooking pot 1 </b>-> 5 litres, stainless steel/aluminium, with lid <br/>
<b>Cooking pot 2 </b>-> 7 litres, stainless steel/aluminium, with lid <br/>
<b>Cup 1 </b>-> small, 250 ml, stainless steel <br/>
<b>Gravel 1 </b>-> 1 cubic meter (m3) <br/>
<b>Gumboots 1 </b>-> 1 pair <br/>
<b>Hammer 1 </b>-> 1 unit, also used to remove nails, 0.5kg <br/>
<b>Hinges 1 </b>-> 1 piece, 4 inches long <br/>
<b>Iron sheet 1 </b>-> 1 piece, 0.9m x 1.5m <br/>
<b>Jerry can 1 </b>-> 20 litres, plastic <br/>
<b>Jerry can 2 </b>-> collapsible, 10 litres <br/>
<b>Jerry can 3 </b>-> non-collapsible, 10 litres <br/>
<b>Kettle 1 </b>-> 2 litres <br/>
<b>Knife 1 </b>-> medium size <br/>
<b>Lock 1 </b>-> 1 unit <br/>
<b>Metal bar 1 </b>-> 1 quintol, 6mm, 6m long (reinforced) <br/>
<b>Metal bar 2 </b>-> 1 quintol, 8mm, 6m long (reinforced) <br/>
<b>MHM 1 </b>-> Menstrual Hygiene Management (MHM), Disposible sanitary pads (pack of 10-14 un) e.g. Always <br/>
<b>MHM 2 </b>-> Menstrual Hygiene Management (MHM), Reusable pads (5 units) e.g. Afrikapads <br/>
<b>Mosquito net 1 </b>-> 1.8m x 1.6m x 1.5m, polyester, treated with insecticides <br/>
<b>Mug 1 </b>-> 1 unit <br/>
<b>Nails 1 </b>-> 1 box, No.5 (1.5 inch) <br/>
<b>Nails 2 </b>-> 1 box, No.6 (2.5 inches) <br/>
<b>Nose mask 1 </b>-> 1 box (50 units), same as face mask <br/>
<b>Plastic glove 1 </b>-> 1 box (100 units) <br/>
<b>Plastic sheet 1 </b>-> 4m x 5m <br/>
<b>Plastic sheet 2 </b>-> 6m x 7.5m <br/>
<b>Plate 1 </b>-> medium, 25cm diameter, stainless steel <br/>
<b>Rake 1 </b>-> 1 unit <br/>
<b>Sand 1 </b>-> 1 cubic meter (m3) <br/>
<b>Serving spoon 1 </b>-> 125 ml, stainless steel <br/>
<b>Sleeping mat 1 </b>-> 1.8m x 0.9m, synthetic yarn <br/>
<b>Soap 1 </b>-> 3 small bars (150g) <br/>
<b>Solar lamp 1 </b>-> 1 unit, Solar Powered LED Lamp <br/>
<b>Spade 1 </b>-> 1 unit <br/>
<b>Spoon 1 </b>-> 1 unit, stainless steel <br/>
<b>Timber 1 </b>-> 1 piece, 5cm x 2.5cm, 4 metres long <br/>
<b>Timber 2 </b>-> 1 piece, 8cm x 4cm, 4 metres long <br/>
<b>Timber 3 </b>-> 1 piece, 10cm x 2.5cm, 4 metres long <br/>
<b>Vent Pipe 1 </b>-> 1 piece, 4 metres long <br/>
<b>Washing powder 1 </b>-> 100 grams, e.g. OMO <br/>
<b>Water 1 </b>-> 1 litre bottle <br/>
<b>Water communal 1 </b>-> fill 20 litres jerrycan <br/>
<b>Water piped 1 </b>-> 1m3 or 1000 litres <br/>
<b>Water truck 1 </b>-> 1m3 or 1000 litres <br/>
<b>Wheel barrow 1 </b>-> 1 unit <br/>
<b>Wood saw 1 </b>-> 1 unit, 10 inches long <br/>
<b>Wooden pole 1 </b>-> 1 unit, 6 metres long <br/>
"
                )
            )
        )
        
    )
   
) #

# Server -----------------------------------------
server <- function(input, output) {
    # REACTIVE LOCATION FILTER
    rex_location <- reactive({
        if (input$select_location != "all") {
            df <- df %>%
                filter(str_detect(call_location, input$select_location))
        } else {
            df
        }
    })
    
    # Interactive Map
    output$Map <- renderLeaflet({
        leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            setView(42.2, 5.15, zoom = 5) %>% 
            
            addPolygons(data = som_02,    # Governorates
                        # label = oblasts$adm1NameLa,
                        options = pathOptions(clickable = FALSE),
                        
                        
                        color = "#333333",
                        weight= 0.6,
                        fill=FALSE,
                        fillOpacity = 0.8,
                        opacity = 0.9 ) %>% 
            
            addPolygons(data= som_02,    # Districts
                        color = "#6D6D70",
                        weight = 1.5,
                        opacity = 1,
                        smoothFactor = 0.5,
                        fill = TRUE,
                        fillColor = "#6D6D70",
                        fillOpacity = 0.1,
                        layerId = ~admin2Name,
                        highlightOptions = highlightOptions(color = "#e36159", weight = 3,
                                                            bringToFront = TRUE, sendToBack = FALSE),
                        label = som_02$admin2Name,
                        labelOptions= labelOptions(
                            interactive= FALSE,
                            permanent= FALSE,
                            opacity= 1,
                            textOnly = TRUE,
                            style = list(
                                "color"= "#333333",
                                "font-weight" = "normal",
                                "font-size" = "15px",
                                "font-family"= "Calibri")
                        )
            ) 
        
    }) 
    
    observeEvent(input$Map_shape_click, { #When shape is clicked add highlight poly and remove any exsisting
        p <- input$Map_shape_click
        if(p$id=="select_location"){
            leafletProxy("Map") %>% removeShape(layerId="select_location")
        } else {
            leafletProxy("Map")  %>% addPolygons(data=(subset(som_02, admin2Name==p$id)), 
                                                 fillColor="#e36159", fillOpacity=1, opacity=1, 
                                                 stroke=FALSE, layerId="select_location",
                                                 color = "#e36159",
                                                 weight = 3.5)
        }
    })
    
    observeEvent(input$Map_shape_click, {
        p <- input$Map_shape_click
        if(!is.null(p$id)){
            if(is.null(input$select_location)) updateSelectInput(session, "select_location", selected=p$id)
            if(!is.null(input$select_location) && input$select_location!=p$id) updateSelectInput(session, "select_location", selected=p$id)
        }
        
        
        observeEvent(input$select_location, {
            p <- input$Map_shape_click
            p2 <- subset(som_02, admin2Name==input$select_location)
            if(nrow(p2)==0){
                leafletProxy("Map") %>% removeMarker(layerId="select_location")
            } else if(is.null(p$id) || input$ll_locations!=p$id){
                leafletProxy("Map")  %>% addPolygons(data=p2, fillColor="#e36159", fillOpacity=1, opacity=1, 
                                                     stroke=FALSE, layerId="select_location",
                                                     color = "#e36159",
                                                     weight = 3.5)
            }
        }) %>% 
            
            #Create map title
            addControl(tags$div(
                HTML(
                    '<p style="text-align: center; padding-left: 3px; padding-right: 10px; font-family: Arial Narrow; color:#333333; font-size: 15px; font-weight: bold;">JMMI Assessed Districts</p>'
                )
            ),
            position = "topleft",
            className = "fieldset { border: 0; }")
        
    }) 
    
    
    # REACTIVE PRICE DATA
    rex_price <- reactive({
        rex_location() %>%
            # Adds all columns starting with price_usd_ and
            # Excludes the _other prices (which should have been manually moved to price_usd_ during checks)
            select(starts_with("price_usd_"), vendor_type, call_location) %>%
            select(!ends_with("_other"))
    })
    
    # REACTIVE ITEM LIST WITH TYPES
    rex_items <- reactive({
        rex_price() %>%
            select(!call_location) %>%
            # Pivot so that we can create an items column
            pivot_longer(starts_with("price_usd_"),
                         names_to = "item",
                         values_to = "value") %>%
            filter(!is.na(value)) %>%
            select(!value) %>%
            distinct()
    })
    
    # REACTIVE PRICE TABLE - pt 1
    rex_table_price1 <- reactive({
        if (input$select_location != "all") {
            temp <- rex_price() %>%
                select(!call_location)
        } else {
            temp <- rex_price() %>%
                group_by(call_location)
        }
        
        temp %>%
            select(!vendor_type) %>%
            # Get number of vendors reporting each item (nv), median, and quartiles
            summarise(across(
                everything(),
                list(
                    nv = ~ sum(!is.na(.)),
                    price = ~ median(.x, na.rm = TRUE),
                    q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
                    q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
                )
            )) %>%
            # Pivot so that we can create an items column
            pivot_longer(!any_of("call_location"),
                         names_to = "key",
                         values_to = "value") %>%
            # obs. the regex "_(?!.*_)" means the last _ in the string
            separate(key, c("item", "info"), sep = "_(?!.*_)") %>%
            pivot_wider(names_from = "info", values_from = "value") %>%
            # Add a check column for all items with less than three prices (OR two prices for water suppliers)
            mutate(check = case_when(
                str_detect(item, "truck") ~ nv > 1,
                str_detect(item, "communal") ~ nv > 1,
                str_detect(item, "piped") ~ nv > 1,
                TRUE ~ nv > 2
            ))
    })
    
    # REACTIVE PRICE TABLE - pt 2
    rex_table_price <- reactive({
        temp <- rex_table_price1() %>%
            # Delete items with less than three prices
            filter(check == TRUE) %>%
            select(!check)
        
        # Draw median of medians
        if (input$select_location == "all") {
            temp <- temp %>%
                select(!call_location) %>%
                group_by(item) %>%
                summarise(
                    nv = sum(nv),
                    price = median(price),
                    q1 = median(q1),
                    q3 = median(q3)
                )
        }
        
        temp %>%
            # Add vendor type
            left_join(rex_items(), by = "item") %>%
            # Rename items
            mutate(item = str_to_title(gsub('_', " ", gsub(
                'price_usd_', "", item
            )))) %>%
            # Change number of vendors to integer
            mutate(nv = as.integer(nv)) %>%
            # Order by vendor type and then item
            arrange(vendor_type, item) %>%
            # Add USD sign and round to two decimal
            #mutate(price = paste('$', format(round(price, 2), nsmall = 2))) %>%
            # Finalize table
            select(
                Item = item,
                "# Vendors" = nv,
                "Price (USD)" = price,
                "1st Quartile" = q1,
                "3rd Quartile" = q3,
                Type = vendor_type
            )
        
    })
    
    # REACTIVE NUMBER OF ROWS IN THE PRICE TABLE
    rex_nrow_table_price <- reactive({
        nrow(rex_table_price())
    })
    
    # CREATE TABLE PRICE
    output$table_price <- renderDT({
        rex_table_price () %>%
            datatable() %>%
            formatStyle(c("1st Quartile", "3rd Quartile"),
                        color = cc_grey,
                        backgroundColor = cc_midgrey) %>%
            formatCurrency(
                c("Price (USD)", "1st Quartile", "3rd Quartile"),
                currency = "$",
                interval = 3,
                mark = ",",
                digits = 2,
                dec.mark = getOption("OutDec"),
                before = TRUE
            )
    })
    
    # CREATE PRICE BOXPLOTS
    output$boxplot <- renderPlotly({
        #># Boxplot styling
        f <- list(family = '"Arial Narrow", sans-serif',
                  size = 18,
                  color = cc_grey)
        x <- list(titlefont = f,
                  tickangle = 90)
        
        # List items with insufficient quotations
        ll_insuf_items <- rex_table_price1() %>%
            # Delete items with less than three prices
            filter(check == TRUE) %>%
            distinct(item) %>%
            pull(item)
        
        #># Item prices (boxplot)
        bx_g <- rex_price() %>%
            select(!vendor_type) %>%
            pivot_longer(contains("price_usd"),
                         names_to = "item",
                         values_to = "price") %>%
            # Remove items with insufficient quotations
            filter(item %in% ll_insuf_items) %>%
            # Remove empty prices
            filter(!is.na(price)) %>%
            mutate(item = str_to_title(gsub("_", " ", gsub(
                "price_usd_", "", item
            ))))
        
        plot_ly(
            bx_g,
            y = ~ price,
            color = ~ item,
            colors = cc_red,
            type = "box"
        ) %>%
            layout(
                showlegend = FALSE,
                xaxis = x,
                yaxis = list(
                    title = "Reported price (USD)",
                    titlefont = f,
                    type = "log"
                )
            )
        
    })
    
    
    # REACTIVE START OF STOCK TABLE
    rex_table_stock <- reactive({
        if (input$select_location != "all") {
            temp <- rex_location() %>%
                select(!call_location)
            # Get number of interviews for location specific query
            rex_nn <- nrow(rex_location())
        } else {
            temp <- rex_location() %>%
                group_by(call_location)
            # Get number of interviews for aggregate query
            rex_nn <- rex_location() %>%
                select(call_location) %>%
                group_by(call_location) %>%
                summarise(nn = n())
        }
        
        st_i <- temp %>%
            # Adds all columns related to stock
            select(starts_with(c("stock_len_", "stock_time_"))) %>%
            # Get median and quartiles
            summarise(across(
                everything(),
                list(
                    nv = ~ sum(!is.na(.)),
                    median = ~ median(.x, na.rm = TRUE),
                    q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
                    q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
                )
            )) %>%
            pivot_longer(!any_of("call_location"),
                         names_to = "key",
                         values_to = "value") %>%
            # obs. the regex "_(?!.*_)" means the last _ in the string
            separate(key, c("item", "info"), sep = "_(?!.*_)") %>%
            pivot_wider(names_from = "info", values_from = "value")
        
        if (input$select_location == "all") {
            st_i <- st_i %>%
                select(!call_location) %>%
                group_by(item) %>%
                summarise(
                    nv = sum(nv, na.rm = TRUE),
                    median = median(median, na.rm = TRUE),
                    q1 = median(q1, na.rm = TRUE),
                    q3 = median(q3, na.rm = TRUE)
                )
        }
        
        #># Stock length table
        
        st_l <- st_i %>%
            rename(len_m = median,
                   len_q1 = q1,
                   len_q3 = q3) %>%
            filter(str_detect(item, "stock_len_")) %>%
            mutate(item = gsub('stock_len_', "", item))
        
        #># Stock time table
        
        st_t <- st_i %>%
            select(!nv) %>%
            rename(tim_m = median,
                   tim_q1 = q1,
                   tim_q3 = q3) %>%
            filter(str_detect(item, "stock_time_")) %>%
            mutate(item = gsub('stock_time_', "", item))
        
        #># Stock difficult table
        
        st_d <- temp %>%
            # Adds all columns related to stock
            select(starts_with("stock_dif_")) %>%
            summarize(across(everything(), ~ list(count(
                as_tibble_col(.), across(everything())
            )))) %>%
            pivot_longer(!any_of("call_location"),
                         names_to = "name",
                         values_to = "value") %>%
            unnest(value)
        
        if (input$select_location == "all") {
            st_d <- st_d %>%
                full_join(rex_nn, by = "call_location") %>%
                unite("name", call_location, name, sep = "___")
        } else {
            st_d <- st_d %>%
                mutate(nn = rex_nn)
        }
        
        st_dx <- st_d %>%
            filter(is.na(value)) %>%
            mutate(nn = nn - n) %>%
            select(name, nn)
        
        st_d <- st_d %>%
            filter(!is.na(value)) %>%
            select(!nn) %>%
            left_join(st_dx, by = "name") %>%
            mutate(pc = n / nn)
        
        # Make a list of empty location-item pairs, in the cases of DIF = 0% that will not have a value = YES
        st_dx <- st_d %>%
            filter(!str_detect(value, "yes")) %>%
            filter(pc == 1) %>%
            mutate(pc = 0) %>%
            select(name, n, pc)
        
        st_d <- st_d %>%
            filter(str_detect(value, "yes")) %>%
            bind_rows(st_dx) %>%
            select(item = name,
                   dif = n,
                   dif_pc = pc) %>%
            mutate(item = gsub('stock_dif_', "", gsub('__yes', "", item)))
        
        if (input$select_location == "all") {
            st_d <- st_d %>%
                separate(item, c("call_location", "item"), sep = "___") %>%
                select(!call_location) %>%
                group_by(item) %>%
                summarise(dif = sum(dif, na.rm = TRUE),
                          dif_pc = median(dif_pc, na.rm = TRUE))
        }
        
        # Fix item names for the non-price indicators
        ve_t <- rex_items() %>%
            # obs. the regex "_(?!.*_)" means the last _ in the string
            separate(item, c("item", "extra"), sep = "_(?!.*_)") %>%
            mutate(item = gsub('price_usd_', "", item)) %>%
            select(!extra) %>%
            distinct()
        
        #># Finalize main stock table
        st_l %>%
            full_join(st_t, by = "item") %>%
            full_join(st_d, by = "item") %>%
            # Add vendor type
            left_join(ve_t, by = "item") %>%
            # Clear NAs
            filter(!is.na(len_m)) %>%
            # Order by vendor type and then item
            arrange(vendor_type, item) %>%
            # Add colours that will be used in the graph
            mutate(
                color = case_when(
                    is.na(dif_pc) ~ cc_grey,
                    len_m < tim_m ~ cc_red,
                    # This is for imminent shortage, rest is reported difficulty to stock
                    dif_pc < 0.2 ~ cc_midgrey,
                    dif_pc < 0.4 ~ cc_gr1,
                    dif_pc < 0.6 ~ cc_gr2,
                    dif_pc < 0.8 ~ cc_gr3,
                    TRUE ~ cc_gr4
                )
            ) %>%
            mutate(item = str_to_title(gsub('_', " ", gsub(
                'price_usd_', "", item
            )))) %>%
            mutate(check = case_when(
                str_detect(item, "truck") ~ nv > 1,
                str_detect(item, "communal") ~ nv > 1,
                str_detect(item, "piped") ~ nv > 1,
                TRUE ~ nv > 2
            )) %>%
            # Delete items with less than three prices
            filter(check == TRUE)
    })
    
    ### Stock analysis
    output$table_stock <- renderDT({
        temp <- rex_table_stock() %>%
            select(
                Item = item,
                "# Vendors" = nv,
                "Stock duration (days)" = len_m,
                "Restocking time (days)" = tim_m,
                "Stocking difficulty" = dif_pc,
                Type = vendor_type
            ) %>%
            datatable() %>%
            formatStyle("Stocking difficulty",
                        backgroundColor = styleInterval(
                            c(0.2, 0.4, 0.6, 0.8),
                            c(cc_bg, cc_gr1, cc_gr2, cc_gr3, cc_gr4)
                        )) %>%
            formatPercentage(
                "Stocking difficulty",
                digits = 0,
                interval = 3,
                mark = ",",
                dec.mark = getOption("OutDec")
            )
        
    })
    
    
    ### Stock analysis
    output$graph_stock <- renderPlotly({
        #># Boxplot styling
        
        f <- list(family = '"Arial Narrow", sans-serif',
                  size = 18,
                  color = cc_grey)
        x <- list(titlefont = f,
                  tickangle = 90)
        
        # List of select multiple questions
        st_color <- rex_table_stock() %>%
            pull(color)
        
        #># General stock data (vertical bars)
        
        st_g <- rex_table_stock() %>%
            filter(str_detect(vendor_type, "general"))
        
        st_g <-
            plot_ly(
                rex_table_stock(),
                x = ~ item,
                y = ~ len_m,
                type = 'bar',
                name = 'Reported length of current stock',
                marker = list(color = st_color)
            )
        st_g <-
            st_g %>% add_trace(y = ~ tim_m, name = 'Reported time to restock, from order to item in shop')
        st_g <- st_g %>% layout(
            yaxis = list(title = 'Days',
                         titlefont = f),
            xaxis = list(title = ''),
            barmode = 'group',
            showlegend = FALSE
        )
        
    })
    
    ### Logo coleads
    output$lg_colead <- renderImage({
        list(src = file.path("www", paste0("jmmi_colead.svg")),
             contentType = "image/svg+xml")
    }, deleteFile = FALSE)
    ### Logo partners
    output$lg_partners <- renderImage({
        list(src = file.path("www", paste0("jmmi_partners.svg")),
             contentType = "image/svg+xml")
    }, deleteFile = FALSE)
    ### Logo donor
    output$lg_donor <- renderImage({
        list(
            src = file.path("www", paste0("usaid.png")),
            contentType = "image/png",
            width = "100%"
        )
    }, deleteFile = FALSE)
    ### How to read a boxplot
    output$img_boxplot <- renderImage({
        list(src = file.path("www", paste0("how_boxplot.svg")),
             contentType = "image/svg+xml")
    }, deleteFile = FALSE)
    
} #


# Run the application 
shinyApp(ui = ui, server = server)

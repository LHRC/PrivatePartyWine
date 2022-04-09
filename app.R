#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(DBI)
library(RPostgreSQL)

query <- "select pi.product_instance_id,
   pi.wine_list_bin_number as plu, 
    pi.wine_list_name as wine_name, v.vintage_year as vintage,
    bs.bottle_size as btl_ml, sale_price as price, c.country_name as country, 
    r.region_name as region, wlc.wine_list_category_name as category, 
    wt.wine_type_name as type, length(pos_story) > 0 and pos_story is not null as has_desc, 
    label_image_oid is not null as has_image
   from product_instances pi left outer join vintages v 
   on pi.vintage_id = v.vintage_id, 
   products p,  
   countries c, regions r, wine_types wt, 
   wine_list_categories wlc, bottle_sizes bs
   where p.product_id = pi.product_id 
   and p.country_id = c.country_id 
   and p.region_id = r.region_id 
   and p.wine_type_id = wt.wine_type_id 
   and pi.wine_list_category_id = wlc.wine_list_category_id
   and pi.bottle_size_id = bs.bottle_size_id
   and pi.active = true"    
query2 <- "select product_instance_id, round(sum(theoretical_on_hand),1) 
    as on_hand from get_theoreticals_active_product_instances_only( $1 ) 
    group by product_instance_id"
db <- 'brg_wine'
db_host <- 'blueridgegrill.cxond9zlkmhk.us-east-1.rds.amazonaws.com'
db_port <- '5432'
#db_user <- 'inventory'
#db_pass <- '1261#Lhrc!3423.Brg'
db_user <- 'read_only'
db_pass <- '?1261!Lhrc'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=db_host, port=db_port, user=db_user, password=db_pass)
#rs <- dbSendQuery(con, query)
wineListData <- dbGetQuery(con, query)
#all<-dbGetQuery(con, "select * from product_instances limit 1")
#print(all)
theoData <- dbGetQuery(con, query2, list(Sys.Date()))
wineListData <- merge(wineListData, theoData, by="product_instance_id")

str(wineListData)
wineListData <- wineListData %>% 
    select(plu, wine_name, vintage, price, on_hand, btl_ml, 
           country, region, category, type, has_desc, has_image) %>%
    mutate(category = as.factor(category)) %>%
    mutate(on_list = ! (is.na(plu) | is.na(price) | is.na(wine_name) ))  %>%
    #arrange( ! is.na(plu), plu)
    arrange( on_list, plu )
str(wineListData)
    

#wineListData <- wineListData[,-1]
#str(wineListData)
#wineListData <- wineListData[order(wineListData$plu),]
#wineListData$category <- as.factor(wineListData$category)
#wineListData <- wineListData[,c(1,2,3,5,10,4,6,7,8,9)]

ui <- dashboardPage(
    dashboardHeader(title = "Wine List"),
    dashboardSidebar(
        selectInput("types", "Types", 
                    unique(wineListData$type[order(wineListData$type)]), 
                    multiple = TRUE, selectize = F),     
        selectInput("categories", "Categories", 
                    unique(wineListData$category[order(wineListData$category)]), 
                    multiple = TRUE, selectize = F),        
        selectInput("countries", "Countries", 
                    unique(wineListData$country[order(wineListData$country)]), 
                    multiple = TRUE, selectize = F),
        selectInput("btlSize", "Bottle Size", unique(wineListData$btl_ml[order((wineListData$btl_ml))]), 
                    selectize = F, multiple = TRUE),
        numericInput("minPrice", "Min Price", 0, min = 0),
        numericInput("maxPrice", "Max Price", max(wineListData$price, na.rm = TRUE) ),
        numericInput("minOnHand", "Min On Hand", 1)
    ),
    dashboardBody(
        fluidPage(
            column(12,
                   DT::dataTableOutput('salesTable')
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observe(
        {
            filteredWLD <- wineListData %>%
                filter( if (! is.null(input$categories)) category %in% input$categories else TRUE) %>%
                filter( if (! is.null(input$countries)) country %in% input$countries else TRUE) %>%
                filter( if (! is.null(input$btlSize)) btl_ml == input$btlSize else TRUE) %>%
                filter( if (! is.null(input$types)) type %in% input$types else TRUE) %>%
                filter( if (! is.null(input$minPrice)) price >= input$minPrice else (price > 0)) %>% 
                filter( if (! is.null(input$maxPrice)) price <= input$maxPrice else (price <= max(price) )) %>%
                filter( if (! is.null(input$minOnHand)) on_hand >= input$minOnHand else (on_hand > 0 ))
            wineListDataTable <- datatable(filteredWLD, 
                                        rownames = FALSE,
                                        extensions = 'Buttons', filter = 'top', options = list(
                                            pageLength = -1,
                                            columnDefs = list(list(width = '40px', targets = c(0,2,3,4,5))),
                                            lengthMenu = list( c(20,50,100, -1), c('20', '50','100', 'All')),
                                            lengthChange = TRUE,
                                            dom = 'Bfrtip',
                                            buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print')))
            output$salesTable <- DT::renderDataTable(wineListDataTable )
        }
    )
    

}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(bslib)
library(dplyr)

# Initialize reactive values
strx <- reactiveVal("Period, Demand, Shipment, Costs, SL,\n")
period <- reactiveVal(0)
demand <- reactiveVal(0)
inventory_ret <- reactiveVal(0)
holdingrate_ret <- reactiveVal(4)
lostsalesrate_ret <- reactiveVal(25)
sl_ret <- reactiveVal(1.0)
lostsales_ret <- reactiveVal(0)
lostsalescount_ret <- reactiveVal(0)
costs_ret <- reactiveVal(0)
inventorycosts_ret <- reactiveVal(0)
lostsalescosts_ret <- reactiveVal(0)

# UI definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .card-header-customer {
        color: white !important;
        background-color: #797979 !important;
      }
      .card-header-manufacturer {
        color: white !important;
        background-color: #ca615f !important;
      }
      .period-btn {
        color: white !important;
        background-color: #797979 !important;
      }
      .export-btn {
        color: black !important;
        background-color: #FFFFFF !important;
        border: 1px solid #ddd;
      }
    "))
  ),
  
  h1("Newsvendor model", style = "font-size: 300%;"),
  
  # Customer card
  card(
    h3(card_header("Customer", class = "card-header-customer")),
    fluidRow(
      column(6, h4(textOutput("txt_demand"))),
      column(6, h4(textOutput("txt_period")))
    )
  ),
  
  br(),
  
  # Manufacturer card
  card(
    h3(card_header("Manufacturer", class = "card-header-manufacturer")),
    h4(textOutput("out_ret_rates")),
    fluidRow(
      column(6, 
             card(
               h4(numericInput("shipment_distr", "Shipment size", value = 100, min = 0))
             )
      ),
      column(6,
             card(
               h4(textOutput("txt_costs_ret")),
               h4(textOutput("txt_sl_ret"))
             )
      )
    )
  ),
  
  br(),
  
  # Buttons row
  fluidRow(
    column(6,
           card(
             card_header("Next period"),
             actionButton("period_button", "Next period", class = "period-btn")
           )
    ),
    column(6,
           card(
             card_header("Export statistics"),
             downloadButton("stats_button", "Statistics", class = "export-btn")
           )
    )
  ),
  
  br(),
  
  # Footer
  tags$div(
    style = "margin-top: 30px;",
    tags$a(
      href = "https://github.com/max-over",
      target = "_blank",
      "https://github.com/max-over"
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Text outputs
  output$txt_demand <- renderText({
    paste("Demand:", demand())
  })
  
  output$txt_period <- renderText({
    paste("Period:", period() + 1)
  })
  
  output$out_ret_rates <- renderText({
    paste("Wastage rate:", holdingrate_ret(), ", Lost sales rate:", lostsalesrate_ret())
  })
  
  output$txt_costs_ret <- renderText({
    paste("Manufacturer costs:", costs_ret())
  })
  
  output$txt_sl_ret <- renderText({
    paste("SL level:", round(sl_ret(), 2))
  })
  
  # Download handler for statistics
  output$stats_button <- downloadHandler(
    filename = function() {
      paste0("Inv_stat-", Sys.Date(), "-", sample(100:999, 1), ".csv")
    },
    content = function(file) {
      # Small delay to simulate async behavior
      Sys.sleep(0.25)
      writeLines(strx(), file)
    }
  )
  
  # Period button action
  observeEvent(input$period_button, {
    # Update inventory and period
    inventory_ret(input$shipment_distr)
    period(period() + 1)
    
    # Generate random demand
    rnd_num <- max(round(rnorm(1, 100, 30)), 0)
    demand(rnd_num)
    
    # Calculate costs based on inventory vs demand
    if (inventory_ret() >= demand()) {
      # Excess inventory costs
      inventorycosts_ret(
        inventorycosts_ret() + (inventory_ret() - demand()) * holdingrate_ret()
      )
    } else {
      # Lost sales costs
      lostsalescosts_ret(
        lostsalescosts_ret() + (demand() - inventory_ret()) * lostsalesrate_ret()
      )
      lostsalescount_ret(lostsalescount_ret() + 1)
    }
    
    # Update total costs and service level
    costs_ret(round(inventorycosts_ret() + lostsalescosts_ret(), 0))
    sl_ret(round(1 - lostsalescount_ret() / period(), 2))
    
    # Update CSV string
    new_str <- paste0(
      strx(),
      period(), ",", 
      as.integer(demand()), ",",
      as.integer(input$shipment_distr), ",",
      as.integer(costs_ret()), ",",
      sl_ret(), ",\n"
    )
    strx(new_str)
    
    # Reset inventory
    inventory_ret(0)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
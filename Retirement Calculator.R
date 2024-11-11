# Title: Retirement Contribution Calculator
# Description: This calculates how much money you could accumulate in your employer retirement plan (or other similar retirement account) over time by making periodic contributions from each paycheck.
# Author: Joel Yamada
# Date: 10/31/2023


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse) # for data manipulation and graphics
library(plotly)    # for web-interactive graphics
library(DT)        # to work with HTML table widgets


# ======================================================
# Auxiliary objects/functions 
# (that don't depend on input widgets)
# ======================================================
# You may want to add one or more auxiliary objects for your analysis
# (by "auxiliary" we mean anything that doesn't depend on input widgets)





# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Retirement Contribution Calculator"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      numericInput(inputId = "annualSalary",
                  label = "Annual salary:",
                  value = 80000),
      numericInput(inputId = "rateOfgrowth",
                   label = "Rate of growth (in decimal):",
                   value = 0.02),
      numericInput(inputId = "contributionPercentage",
                   label = "Contribution percentage (in decimal):",
                   value = 0.15),
      selectInput(inputId = "numberOfperiods",
                  label = "Number of periods:", 
                  choices = list(1, 2, 4, 6, 12, 52),
                  selected = 12), 
      numericInput(inputId = "yearsInvested",
                   label = "Years invested:",
                   value = 5),
      numericInput(inputId = "annualRateofReturn",
                   label = "Annual rate of return (in decimal):",
                   value = 0.08),
      numericInput(inputId = "targetAmount",
                   label = "(Optional) Target amount:",
                   value = 35000),
      checkboxInput(inputId = "showTarget",
                   label = "Show target amount:",
                   value = FALSE),
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      h3("Balance Timeline"),
      plotlyOutput(outputId = "plot1"),
      hr(),
      h3("Composition of Own Contributions vs. Investment Growth"),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3("Data Table"),
      dataTableOutput(outputId = "table"),
    )
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive Balance table
  # (adapt code to get the appropriate Balance data table)
  # ------------------------------------------------------------
  tbl = reactive({
    balance_tbl = data.frame(
      year = 1:input$yearsInvested,
      salary = input$annualSalary,
      annual_contrib = input$annualSalary * input$contributionPercentage)|>
      mutate(
        salary = salary * (1 + input$rateOfgrowth) ^ (year - 1),
        annual_contrib = salary * input$contributionPercentage,
        period_contrib = annual_contrib / as.numeric(input$numberOfperiods),
        balance = 0,
        own = cumsum(annual_contrib),
        growth = balance - own,
        own_pct = own / balance * 100,
        growth_pct = growth / balance * 100,
        hit_target = ifelse(balance >= input$targetAmount, "yes", "no")
      )
    
    for (y in 1:input$yearsInvested) {
      if (y == 1) {
        for (i in 0:(as.numeric(input$numberOfperiods) - 1)) {
          balance_tbl$balance[y] = balance_tbl$balance[y] + balance_tbl$period_contrib[y]*(1 + input$annualRateofReturn/as.numeric(input$numberOfperiods)) ** i
        } # 12449.93
      }
    }
    
    for (y in 2:input$yearsInvested) {
      balance_tbl$balance[y] = balance_tbl$balance[y-1] 
      
      for (i in 0:(as.numeric(input$numberOfperiods) - 1)) {
        interest_factor = 1 + input$annualRateofReturn / as.numeric(input$numberOfperiods)
        balance_tbl$balance[y] = balance_tbl$balance[y] * interest_factor + balance_tbl$period_contrib[y]
      }
    }
      
    balance_tbl$growth = balance_tbl$balance - balance_tbl$own
    balance_tbl$own_pct = balance_tbl$own / balance_tbl$balance * 100
    balance_tbl$growth_pct = balance_tbl$growth / balance_tbl$balance * 100
    balance_tbl$hit_target = ifelse(balance_tbl$balance >= input$targetAmount, "yes", "no")
    
    balance_tbl
  })

  
  # ------------------------------------------------------------
  # Plot of balance timeline
  # (adapt code to make a timeline according to your analysis)
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    # the following code is for demo purposes only; adapt it!!!
    p <- ggplot(data = tbl(), aes(x = year, y = balance)) +
      geom_line(color = "black") +
      geom_point(color = "black") + 
      geom_area(fill = "blue",
                alpha = 0.3)
    
    if (input$showTarget) {
      p <- p + 
        geom_hline(yintercept = input$targetAmount, 
                          color = "orange", 
                          linetype = "solid")
    }
    
    # Return the plot
    p
  })
  
  # ------------------------------------------------------------
  # Plot of balance decomposition
  # (adapt code to make a graphic according to your analysis)
  # ------------------------------------------------------------``
  output$plot2 <- renderPlotly({
    
    tbl = pivot_longer(
      data = tbl(),
      cols = own:growth,
      names_to = "type")
    
    ggplot(data = tbl,
           aes(x = year,
               y = value,
               fill = type)) +
      geom_col()
  })

  
  # ------------------------------------------------------------
  # Table with Retirement Balance data
  # (adapt code to display appropriate table)
  # ------------------------------------------------------------
  output$table <- renderDataTable({
    # to limit the number of decimal digits in the rendered table
    # you can convert it into a "DataTable" object, and then use
    # formatRound() to limit the displayed decimals.
    tbl() |>
      datatable() |> # convert into "DataTable" object
      formatRound(columns = c("annual_contrib", 
                              "period_contrib", 
                              "balance", 
                              "salary", 
                              "own", 
                              "own_pct", 
                              "growth", 
                              "growth_pct"), 
                  digits = 2) # round to 2-digits
  })
  
 
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)


# ui.R

shinyUI(fluidPage(
    
    titlePanel("Demo"),
    
    mainPanel(
        tabsetPanel(
            # Data
            tabPanel("Data",
                     dataTableOutput("view_data")),
            
            # Descriptive
            tabPanel("Descriptive",
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("var1",
                                         label = "Which variable?",
                                         choices = colnames(Xtt)),
                             selectInput("var2",
                                         label = "And which variable?",
                                         choices = colnames(Xtt))
                         ),
                         mainPanel(
                             plotOutput("plot_1var"),
                             plotOutput("plot_2var")
                         )
                     )),
            
            # Predictive
            tabPanel("Predictive",
                     navlistPanel(
                         tabPanel("G1",
                                  plotOutput("plot_tr0_G1"),
                                  tableOutput("plot_tr0_G1_pred")),
                         tabPanel("G2",
                                  plotOutput("plot_tr0_G2"),
                                  plotOutput("plot_tr0_G2_pred")),
                         tabPanel("G3",
                                  plotOutput("plot_lm0_G3"),
                                  plotOutput("plot_lm0_G3_pred"))
                     )
            ),
            
            tabPanel("Prescriptive",
                     tableOutput("actionable_vars"),
                     tableOutput("static_vars"))
        )
    )
))

# ui.R

source("C:/Users/weizhong/Documents/R/Demo-Sales/codes/load.R")

shinyUI(fluidPage(
    
    titlePanel("Predicting student performances"),
    
    mainPanel(
        tabsetPanel(
            # Data
            tabPanel("Data",
                     tags$br(),
                     dataTableOutput("view_data")),
            
            # Descriptive
            tabPanel("Descriptive",
                     tags$br(),
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
                     tags$br(),
                     navlistPanel(
                         tabPanel("Predicting G1 (pass or fail)",
                                  plotOutput("plot_tr0_G1"),
                                  dataTableOutput("plot_tr0_G1_pred")),
                         tabPanel("Predicting G2 (actual scores)",
                                  plotOutput("plot_tr0_G2"),
                                  plotOutput("plot_tr0_G2_pred")),
                         tabPanel("Predicting improvements in G3",
                                  plotOutput("plot_lm0_G3_beta"),
                                  plotOutput("plot_lm0_G3_anova"),
                                  plotOutput("plot_lm0_G3_pred"))
                     )
            ),
            
            # Prescriptive
            tabPanel("Prescriptive",
                     tags$br(),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("absences",
                                         label = "Number of days absences in the previous year",
                                         choices = sort(unique(Xtt$absences))),
                             selectInput("failures",
                                         label = "Past failures",
                                         choices = sort(unique(Xtt$failures))),
#                              selectInput("G1",
#                                          label = "G1 score",
#                                          choices = sort(unique(Xtt$G1))),
#                              selectInput("G2",
#                                          label = "G2 score",
#                                          choices = sort(unique(Xtt$G2))),
                             selectInput("traveltime",
                                         label = "Travel time to school",
                                         choices = sort(unique(Xtt$traveltime))),
                             selectInput("studytime",
                                         label = "Amount of studying time",
                                         choices = sort(unique(Xtt$studytime)))
                         ),
                         mainPanel(
                             plotOutput("prescriptive")
                         )
                     )
            )
        )
    )
))
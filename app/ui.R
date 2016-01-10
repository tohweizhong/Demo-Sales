
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
                         tabPanel("Predicting improvements in G3 (students who failed either G1 or G2 or both)",
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
                             h4("Static variables"),
                             selectInput("failures",
                                         label = "Past failures",
                                         choices = sort(unique(Xtt$failures))),
                             selectInput("G1",
                                         label = "G1 score",
                                         choices = 1:9),
                             selectInput("G2",
                                         label = "G2 score",
                                         choices = 1:9),
                             selectInput("studytime",
                                         label = "Amount of studying time",
                                         choices = sort(unique(Xtt$studytime))),
                             tags$h4("Actionable variables"),
                             selectInput("absences",
                                         label = "Number of days absences in the previous year",
                                         choices = sort(unique(Xtt$absences))),
                             selectInput("paid",
                                         label = "Is student having extra tuition lessons?",
                                         choices = c("yes", "no")),
                             selectInput("schoolsup",
                                         label = "Is the school subsidizes the student's education?",
                                         choices = c("yes", "no")),
                             selectInput("activities",
                                         label = "Is the student participating in CCAs?",
                                         choices = c("yes", "no"))
                             #                              selectInput("traveltime",
                             #                                          label = "Travel time to school",
                             #                                          choices = sort(unique(Xtt$traveltime))),
                             #                              selectInput("freetime",
                             #                                          label = "Amount of free time",
                             
                             
                         ),
                         mainPanel(
                             verbatimTextOutput("prescriptive_text"),
                             plotOutput("prescriptive_plot")
                         )
                     )
            )
        )
    )
))
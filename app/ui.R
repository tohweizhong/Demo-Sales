
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
                                         choices = c("G1", "G2", "G3")),
                             selectInput("var2",
                                         label = "And which variable?",
                                         choices = colnames(subset(Xtt, select = -c(G1, G2, G3))))
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
                                         choices = sort(unique(Xtt$failures)),
                                         selected = "3"),
                             selectInput("G1",
                                         label = "G1 score",
                                         choices = seq(10, 50, by = 10),
                                         selected = "50"),
                             selectInput("G2",
                                         label = "G2 score",
                                         choices = seq(10, 50, by = 10),
                                         selected = "10"),
                             selectInput("studytime",
                                         label = "Amount of studying time",
                                         choices = sort(unique(Xtt$studytime)),
                                         selected = "4"),
                             tags$h4("Actionable variables"),
                             selectInput("absences",
                                         label = "Number of days absences in the previous year",
                                         choices = sort(unique(Xtt$absences)),
                                         selected = "10"),
                             selectInput("Dalc",
                                         label = "Level of alcohol consumption",
                                         choices = c(1, 2, 3, 4, 5),
                                         selected = "5"),
                             selectInput("famsup",
                                         label = "Is the school subsidizes the student's education?",
                                         choices = c("yes", "no"),
                                         selected = "no"),
                             selectInput("activities",
                                         label = "Is the student participating in CCAs?",
                                         choices = c("yes", "no"),
                                         selected = "yes")
                             #                              selectInput("traveltime",
                             #                                          label = "Travel time to school",
                             #                                          choices = sort(unique(Xtt$traveltime))),
                             #                              selectInput("freetime",
                             #                                          label = "Amount of free time",
                             
                             
                         ),
                         mainPanel(
                             verbatimTextOutput("prescriptive_text"),
                             selectInput("absences_improv",
                                         label = "What if number of days absent is changed to:",
                                         choices = sort(unique(Xtt$absences)),
                                         selected = "5"),
                             selectInput("Dalc_improv",
                                         label = "What if level of alcohol consumption is changed to:",
                                         choices = c(1, 2, 3, 4, 5),
                                         selected = "1"),
                             selectInput("famsup_improv",
                                         label = "What if the school changes the student's education subsidy to:",
                                         choices = c("yes", "no"),
                                         selected = "yes"),
                             selectInput("activities_improv",
                                         label = "What if the student CCA participation is changed to:",
                                         choices = c("yes", "no"),
                                         selected = "no"),
                             plotOutput("prescriptive_plot", width = "100%", height = "500px")
                         )
                     )
            )
        )
    )
))
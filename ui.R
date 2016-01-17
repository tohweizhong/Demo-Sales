
# ui.R

source("codes/load.R")

shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
                  titlePanel("Predicting students' academic performances"),
                  
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
                                           plotOutput("plot_scores"),
                                           plotOutput("plot_othervars"),
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
                                           selectInput("age",
                                                       label = "Age of sudent",
                                                       choices = sort(unique(Xtt$age)),
                                                       selected = "15"),
                                           selectInput("failures",
                                                       label = "Past failures",
                                                       choices = sort(unique(Xtt$failures)),
                                                       selected = "0"),
                                           selectInput("G1_cate",
                                                       label = "Passed G1?",
                                                       choices = c("Pass", "Fail"),
                                                       selected = "Pass"),
                                           selectInput("G2",
                                                       label = "G2 score",
                                                       choices = seq(10, 100, by = 10),
                                                       selected = "60"),
                                           selectInput("famrel",
                                                       label = "Quality of family relationships",
                                                       choices = sort(unique(Xtt$famrel)),
                                                       selected = "5"),
                                           selectInput("famsup",
                                                       label = "Is the family recieving financial aid from the govt?",
                                                       choices = c("yes", "no"),
                                                       selected = "yes"),
                                           tags$h4("Actionable variables"),
                                           selectInput("romantic",
                                                       label = "Is the student in a romantic relationship?",
                                                       choices = c("yes", "no"),
                                                       selected = "yes"),
                                           selectInput("activities",
                                                       label = "Is the student participating in CCAs?",
                                                       choices = c("yes", "no"),
                                                       selected = "yes"),
                                           selectInput("paid",
                                                       label = "Is the student going for private tuition classes?",
                                                       choices = c("yes", "no"),
                                                       selected = "no"),
                                           selectInput("schoolsup",
                                                       label = "Is the school subsidizes the student's education?",
                                                       choices = c("yes", "no"),
                                                       selected = "no")),
                                       mainPanel(
                                           verbatimTextOutput("prescriptive_text"),
                                           tags$br(),
                                           tags$h4("Acting on the actionable:"),
                                           selectInput("romantic_improv",
                                                       label = "What if the student's romantic relationship status is changed to:",
                                                       choices = c("yes", "no"),
                                                       selected = "yes"),
                                           selectInput("activities_improv",
                                                       label = "What if the student CCA participation is changed to:",
                                                       choices = c("yes", "no"),
                                                       selected = "yes"),
                                           selectInput("paid_improv",
                                                       label = "What if the school provides/don't provide additional supplementary classes:",
                                                       choices = c("yes", "no"),
                                                       selected = "no"),
                                           selectInput("schoolsup_improv",
                                                       label = "What if the school changes the student's education subsidy to:",
                                                       choices = c("yes", "no"),
                                                       selected = "no"),
                                           
                                           plotOutput("prescriptive_plot", width = "100%", height = "500px")
                                       )
                                   )
                          )
                      )
                  )
))

# server.R

source("C:/Users/weizhong/Documents/R/Demo-Sales/codes/load.R")

shinyServer(function(input, output, session){
    
    # Data
    output$view_data <- renderDataTable({
        (data.frame(Xtt[1:20,]))
    })
    
    # ====
    
    # Descriptive
    output$plot_1var <- renderPlot({
        par(mfrow = c(1,2))
        plot(Xtt[,input$var1],
             main = input$var1)
        plot(Xtt[,input$var2],
             main = input$var2)
    })
    output$plot_2var <- renderPlot({
        plot(Xtt[,input$var1] ~ Xtt[,input$var2],
             main = paste0("Relationship between ", input$var1, " and ", input$var2))
    })

    # ====
    
    # Predictive
    output$plot_tr0_G1 <- renderPlot({
        rpart.plot(tr0_G1, main = "Predicting G1 (pass or fail)")
    })
    output$plot_tr0_G1_pred <- renderTable({
        tr0_G1_pred_class <- predict(tr0_G1, newdata = Xtest_G1, type = "class")
        print(tr0_G1_tb <- table(tr0_G1_pred_class, ytest_G1))
        print(tr0_G1_acc <- sum(diag(tr0_G1_tb)) / sum(tr0_G1_tb))
        
        tr0_G1_pred_prob <- predict(tr0_G1, newdata = Xtest_G1, type = "prob")
        print(tr0_G1_auc <- pROC::auc(response = ytest_G1, predictor = tr0_G1_pred_prob[,2]))
        
        tr0_G1_tb
    })
    output$plot_tr0_G2 <- renderPlot({
        rpart.plot(tr0_G2, main = "Predicting G2")
    })
    output$plot_tr0_G2_pred <- renderPlot({
        tr0_G2_pred <- predict(tr0_G2, newdata = Xtest_G2)
        rmse(actual = ytest_G2, predicted = tr0_G2_pred)
        plot(ytest_G2 ~ tr0_G2_pred, main = "Actual G2 vs. predicted G2")
        abline(a = 0, b = 1)
    })
    output$plot_lm0_G3 <- renderPlot({
        an <- anova(lm0_G3)
        barplot(an$"Sum Sq"[-nrow(an)], main = "Variable importances for predicting G3")
    })
    output$plot_lm0_G3_pred <- renderPlot({
        lm0_G3_pred <- predict(lm0_G3, newdata = Xtest_G3)
        rmse(actual = ytest_G3, predicted = lm0_G3_pred)
        
        print(anova(lm0_G3))
        plot(ytest_G3 ~ lm0_G3_pred)
        abline(a = 0, b = 1)
    })
    
    # Prescriptive
    output$actionable_vars <- renderTable({
        df <- data.frame(t(actionable_vars))
        colnames(df) <- NULL
        df
    })
    output$static_vars <- renderTable({
        data.frame(static_vars)
    })
})
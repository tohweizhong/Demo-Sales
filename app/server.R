
# server.R

shinyServer(function(input, output, session){
    
    # Data
    output$view_data <- renderDataTable({
        datatable(Xtt[1:20,], options = list(pageLength = 20, searching = FALSE, paging = FALSE))
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
    output$plot_tr0_G1_pred <- renderDataTable({
        tr0_G1_pred_class <- predict(tr0_G1, newdata = Xtest_G1, type = "class")
        print(tr0_G1_tb <- table(tr0_G1_pred_class, ytest_G1))
        print(tr0_G1_acc <- sum(diag(tr0_G1_tb)) / sum(tr0_G1_tb))
        
        tr0_G1_pred_prob <- predict(tr0_G1, newdata = Xtest_G1, type = "prob")
        print(tr0_G1_auc <- pROC::auc(response = ytest_G1, predictor = tr0_G1_pred_prob[,2]))
        
        class(tr0_G1_tb) <- "matrix"
        colnames(tr0_G1_tb) <- c("Fail", "Pass")
        rownames(tr0_G1_tb) <- c("Predicted as fail", "Predicted as pass")
        datatable(tr0_G1_tb, options = list(paging = FALSE, searching = FALSE))
        
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
    output$plot_lm0_G3_beta <- renderPlot({
        barplot(summary(lm0_G3)$coefficients[-1,1], main = "Model coefficients")
    })
    output$plot_lm0_G3_anova <- renderPlot({
        an <- anova(lm0_G3)
        barplot(an$"Sum Sq"[-nrow(an)], main = "Variable importances for predicting G3 improvements",
                names.arg = rownames(an)[-nrow(an)])
    })
    output$plot_lm0_G3_pred <- renderPlot({
        lm0_G3_pred <- predict(lm0_G3, newdata = Xtest_G3)
        rmse(actual = ytest_G3, predicted = lm0_G3_pred)
        
        print(anova(lm0_G3))
        plot(ytest_G3 ~ lm0_G3_pred, main = "Actual improvements in G3 vs. predicted improvements")
        abline(a = 0, b = 1)
    })
    
    # Prescriptive
    output$prescriptive_text <- renderText({
        newdata <- data.frame(as.numeric(input$absences),
                              as.numeric(input$failures),
                              as.numeric(input$G1),
                              as.numeric(input$G2),
                              as.numeric(input$studytime),
                              as.factor(input$paid),
                              as.factor(input$schoolsup),
                              as.factor(input$activities))
        colnames(newdata) <- c("absences", "failures", "G1", "G2",
                               "studytime", "paid", "schoolsup", "activities")
        
        lm0_G3_pred <- predict(lm0_G3, newdata = newdata)
        return(paste0("Student is predicted to make an improvement of ",
                      round(as.numeric(lm0_G3_pred), 1),
                      " in G3 relative to G2"))
    })
    
    output$prescriptive_plot <- renderPlot({
        
        newdata <- data.frame(as.numeric(input$absences),
                              as.numeric(input$failures),
                              as.numeric(input$G1),
                              as.numeric(input$G2),
                              as.numeric(input$studytime),
                              as.factor(input$paid),
                              as.factor(input$schoolsup),
                              as.factor(input$activities))
        colnames(newdata) <- c("absences", "failures", "G1", "G2",
                               "studytime", "paid", "schoolsup", "activities")
        
        lm0_G3_pred <- as.numeric(predict(lm0_G3, newdata = newdata))
        
        betas <- summary(lm0_G3)$coefficients[c("absences", "paidyes", "schoolsupyes", "activitiesyes"),1]
        
        after_action <- rep(lm0_G3_pred, 4) + betas
        
        betas
    })
})
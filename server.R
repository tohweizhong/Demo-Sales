
# server.R

shinyServer(function(input, output, session){
    
    # Data
    output$view_data <- renderDataTable({
        datatable(Xtt[1:20,], options = list(pageLength = 20, searching = FALSE, paging = FALSE))
    })
    
    # ====
    
    # Descriptive
    output$plot_scores <- renderPlot({
        qplot(Xtt[,input$var1], geom = "histogram", xlab = input$var1, main = input$var1) + theme_bw()
    })
    output$plot_othervars <- renderPlot({
        if(input$var2 %in% num_vars)
            qplot(Xtt[,input$var2], geom = "histogram", xlab = input$var2, main = input$var2) + theme_bw()
        
        else if(input$var2 %in% cate_vars)
            qplot(Xtt[,input$var2], geom = "bar", xlab = input$var2, main = input$var2) + theme_bw()
    })
    output$plot_2var <- renderPlot({
        qplot(y = Xtt[,input$var1], x = factor(Xtt[,input$var2]),
              main = paste0("Relationship between ", input$var1, " and ", input$var2),
              geom = "boxplot", xlab = input$var2, ylab = input$var1) + theme_bw()
    })
    
    # ====
    
    # Predictive
    output$plot_tr0_G1 <- renderPlot({
        rpart.plot(tr0_G1, main = "Predicting G1 (pass or fail)")
    })
    output$plot_tr0_G1_pred <- renderDataTable({
        tr0_G1_pred_class <- predict(tr0_G1, newdata = Xtest_G1, type = "class")
        tr0_G1_tb <- table(tr0_G1_pred_class, ytest_G1)
        tr0_G1_acc <- sum(diag(tr0_G1_tb)) / sum(tr0_G1_tb)
        
        tr0_G1_pred_prob <- predict(tr0_G1, newdata = Xtest_G1, type = "prob")
        tr0_G1_auc <- pROC::auc(response = ytest_G1, predictor = tr0_G1_pred_prob[,2])
        
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
        tr0_G2_pred_rmse <- rmse(actual = ytest_G2, predicted = tr0_G2_pred)
        
        plot_df <- data.frame(cbind(ytest_G2, tr0_G2_pred))
        colnames(plot_df) <- c("Actual", "Predicted")
        qplot(plot_df, x = plot_df$Predicted, y = plot_df$Actual, 
              main = paste0("Actual G2 vs. Predicted G2, error = ", round(tr0_G2_pred_rmse, 2)),
              xlab = "Predicted G2", ylab = "Actual G2") + geom_abline(slope = 1, intercept = 0) + theme_bw()
        
    })
    output$plot_lm0_G3_beta <- renderPlot({
        coef <- summary(lm0_G3)$coefficients[-1,1]
        qplot(coef, x = factor(names(coef)), main = "Predicting G3 improvements: model coefficients", geom = "bar", stat = "identity", xlab = "Variables", ylab = "Model coefficients") + theme_bw()
    })
    output$plot_lm0_G3_anova <- renderPlot({
        an <- anova(lm0_G3)
        impt <- an$"Sum Sq"[-nrow(an)]
        names(impt) <- rownames(an)[-nrow(an)]
        qplot(impt, x = factor(names(impt)), main = "Variable importances for predicting G3 improvements",
              geom = "bar", stat = "identity", xlab = "Variables", ylab = "Variable importance") + theme_bw()
    })
    output$plot_lm0_G3_pred <- renderPlot({
        lm0_G3_pred <- predict(lm0_G3, newdata = Xtest_G3)
        lm0_G3_pred_rmse <- rmse(actual = ytest_G3, predicted = lm0_G3_pred)
        
        plot_df <- data.frame(cbind(ytest_G3, lm0_G3_pred))
        colnames(plot_df) <- c("Actual", "Predicted")
        qplot(plot_df, x = plot_df$Predicted, y = plot_df$Actual,
              main = paste0("Actual improvements in G3 vs. predicted improvements, error = ", round(lm0_G3_pred_rmse, 2)),
              xlab = "Predicted improvements", ylab = "Actual improvements") + geom_abline(slope = 1, intercept = 0) + theme_bw()
    })
    
    # Prescriptive
    output$prescriptive_text <- renderText({
        newdata <- data.frame(as.numeric(input$age),
                              as.numeric(input$failures),
                              as.factor(input$activities),
                              as.numeric(input$famrel),
                              as.factor(input$G1_cate),
                              as.numeric(input$G2),
                              as.factor(input$schoolsup),
                              as.factor(input$paid),
                              as.factor(input$famsup),
                              as.factor(input$romantic)
                              #as.numeric(input$Dalc)
                              )
        colnames(newdata) <- lm0_G3_vars[-1]
#         colnames(newdata) <- c("absences", "failures", "G1", "G2",
#                                "studytime", "Dalc", "famsup", "activities")
        
        lm0_G3_pred <- predict(lm0_G3, newdata = newdata)
        return(paste0("Student is predicted to make an improvement of ",
                      round(as.numeric(lm0_G3_pred), 2),
                      " in G3 relative to G2"))
    })
    
    output$prescriptive_plot <- renderPlot({
        
        newdata <- data.frame(as.numeric(input$age),
                              as.numeric(input$failures),
                              as.factor(input$activities),
                              as.numeric(input$famrel),
                              as.factor(input$G1_cate),
                              as.numeric(input$G2),
                              as.factor(input$schoolsup),
                              as.factor(input$paid),
                              as.factor(input$famsup),
                              as.factor(input$romantic)
                              #as.numeric(input$Dalc)
        )
        colnames(newdata) <- lm0_G3_vars[-1]
        
        #str(newdata)
        
        lm0_G3_pred <- as.numeric(predict(lm0_G3, newdata = newdata))
        betas <- summary(lm0_G3)$coefficients[c("romanticyes","activitiesyes", "paidyes" ,"schoolsupyes"),1]
        
        improv <- c(input$romantic_improv, input$activities_improv, input$paid_improv, input$schoolsup_improv)
        #diff <- as.numeric(as.numeric(improv[c(1,2)]) - newdata[,c(5,11)])
        
        diff <- NULL
        if(input$romantic == input$romantic_improv)
            diff <- c(diff, 0)
        else if(input$romantic != input$romantic_improv && input$romantic == "yes")
            diff <- c(diff, -1)
        else if(input$romantic != input$romantic_improv && input$romantic == "no")
            diff <- c(diff, 1)
        
        if(input$activities == input$activities_improv)
            diff <- c(diff, 0)
        else if(input$activities != input$activities_improv && input$activities == "yes")
            diff <- c(diff, -1)
        else if(input$activities != input$activities_improv && input$activities == "no")
            diff <- c(diff, 1)
        
        if(input$paid == input$paid_improv)
            diff <- c(diff, 0)
        else if(input$paid != input$paid_improv && input$paid == "yes")
            diff <- c(diff, -1)
        else if(input$paid != input$paid_improv && input$paid == "no")
            diff <- c(diff, 1)
        
        if(input$schoolsup == input$schoolsup_improv)
            diff <- c(diff, 0)
        else if(input$schoolsup != input$schoolsup_improv && input$schoolsup == "yes")
            diff <- c(diff, -1)
        else if(input$schoolsup != input$schoolsup_improv && input$schoolsup == "no")
            diff <- c(diff, 1)
        
        #str(diff); print(diff)
        
        print("betas")
        print(betas)
        print("diff*betas")
        print(diff*betas)
        print("lm0_G3_pred")
        print(lm0_G3_pred)
        
        
        after_one_action <- lm0_G3_pred + diff*betas
        after_all_actions <- lm0_G3_pred + sum(diff*betas)
        
        print(after_one_action)
        print(after_all_actions)
        
        all_tgt <- c(lm0_G3_pred, after_one_action, after_all_actions)
        names(all_tgt) <- c("Predicted", "Romantic", "CCA", "Supplementary", "Subsidies", "All")
        print(all_tgt)
        #qplot(all_tgt, x = factor(c("Predicted", "Absences", "Alcohol", "CCA", "Supplementary", "Subsidies", "All"),
        #                          levels = c("Predicted", "Absences", "Alcohol", "CCA", "Supplementary", "Subsidies", "All")),
        #      main = "Predicted G3 improvements; acting on actionable variables", geom = "bar", stat = "identity", xlab = "Acting on ...", ylab = "Predicted improvements") + theme_bw()
        qplot(y = all_tgt, x = factor(names(all_tgt), levels = c("Predicted","Romantic", "CCA", "Supplementary", "Subsidies", "All")), main = "Predicted G3 improvements; acting on actionable variables",
              geom = "bar", stat = "identity", xlab = "Acting on...", ylab = "Predicted improvements") + theme_bw()
    })
})
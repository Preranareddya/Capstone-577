#Regression model
      full.model.train <- lm(V8480~., data=data.train)
      full.model.test <- lm(V8480~., data=data.test)
      
      #full.model <- lm(y=data.train[,1], x=data.train[,-1])
      
      #significance
      #https://stat.ethz.ch/pipermail/r-help/2005-December/084308.html
      
      #http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
      
      # Fit the full model 
      
      # Stepwise regression model
      
      #stepwise regression
      #step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
      step.model.train <- stepAIC(full.model.train, direction = "both", 
                            trace = FALSE)
      step.model.test <- stepAIC(full.model.test, direction = "both", 
                                  trace = FALSE)
      summary(step.model.train)
      #lm(formula = V8480 ~ V8502 + V8505 + V8509 + V8512 + V8514 + V8536 +
      #V7501 + V8565, data = data.train)
      
      summary(step.model.test) 
      #lm(formula = V8480 ~ V8502 + V8505 + V8509 + V8514 + V8536 + 
      #    V7501 + V8565, data = data.test)
      
       #Calculating MSE for training data
      mse.train<- mean(residuals(step.model.train)^2)
      mse.train
      
      #Calculating RMSE for training data
      rmse.train <- sqrt(mse.train)
      rmse.train
      
      #Calculating MSE for testing data
      mse.test <- mean(residuals(step.model.test)^2)
      mse.test
      
      #Calculating RMSE for testing data
      rmse.test <- sqrt(mse.test)
      rmse.test
      
      #For Training data
      V8480= -1.680696+0.136087*V8502+0.183306*V8505-0.125640*V8509-0.031838*V8512
              -0.143121*V8514+0.256743*V8536-0.022045*V7501+0.506726*V8565
      
      #For testing data
      V8480= -1.71595+0.14148*V8502+0.15657*V8505-0.14001*V8509-0.14515*V8514
              +0.22792*V8536-0.02297*V7501+0.533686*V8565
      

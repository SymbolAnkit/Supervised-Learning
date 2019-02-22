  ### Required Libraries

          library(caret)
          library(glmnet)
          library(mlbench)
          library(psych)

          data("BostonHousing")
 
          pairs.panels(BostonHousing , cex.cor = 5)

          colnames(BostonHousing)

### linear regression

          fit <- lm(medv~. , data = BostonHousing)

          summary(fit)

### cross validation

          set.seed(111)

          control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 10,
                        verboseIter = T)

          cvfit <- train(medv~. , BostonHousing, method = "lm" , trControl = control)

          cvfit$results
          plot(cvfit$finalModel)

          dev.off()
### Ridge

          ridge <- train(medv~. , data = BostonHousing, 
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = seq(0.1, 5, length = 5)),
               trControl = control)
          summary(ridge)

          plot(ridge)
          ridge

          plot(ridge$finalModel, xvar = "lambda",label = T)

          plot(ridge$finalModel, xvar = "dev" , label = T)

          plot(varImp(ridge , scale = F))

### Lasso

          set.seed(191)

          lasso <- train(medv~. , data = BostonHousing,method = "glmnet" ,
               tuneGrid = expand.grid(alpha =1 ,
                                      lambda = seq(0.1,5,length = 10)),
               trControl = control)

          plot(lasso)

          lasso

          plot(lasso$finalModel, xvar = "lambda" , label = T)

          plot(lasso$finalModel , xvar = "dev" , label = T)

          plot(varImp(lasso,scale = F))

### Elastic Net

          elnet <- train(medv~. , data = BostonHousing ,
               method = "glmnet" , 
           
                   tuneGrid = expand.grid(alpha = seq(0,1,length = 15),
                                      lambda = seq(0,0.2,length = 10)),
               trControl = control)

          elnet

          plot(elnet)

          plot(elnet$finalModel , xvar = "lambda" , label = T)

          plot(elnet$finalModel , xvar = "dev" , label = T)

          plot(varImp(elnet))

## compare models

          model_list <- list( Ridge = ridge , Lasso = lasso ,
                   ElasticNet = elnet)

          res <- resamples(model_list)
          
          summary(res)

          bwplot(res)

## Best model

          elnet$bestTune
          ridge$bestTune
          lasso$bestTune

          x <- elnet$finalModel

          coef(x , s = elnet$bestTune$lambda)

_save the model_

          saveRDS(elnet , "finalmodel.rds")

          fm <- readRDS("finalmodel.rds")

          print(fm)

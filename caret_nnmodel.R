#
library('NeuralNetTools')
library(tidyverse)
library(ggplot2)
library(caret)
library(metR)
library(ggthemes)
library(grDevices) 
library(RColorBrewer)
library(ggspatial)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggsci)
library(patchwork)
#install.packages("DALEX")
library(DALEX)
#install.packages("ingredients")
library(ingredients)
library(lattice)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model_WCPO_LLCPUE_BET <- train(WCPO_LLCPUE_BET ~ Month + Lon + Lat + SST + SSS + SSH + O2 + Nppv + ONI, 
                               data = Dataset, 
                               method = "nnet", 
                               linout = TRUE, 
                               trace = FALSE,
                               tuneGrid = expand.grid(size = 4:13, decay = 0.01), 
                               trControl = ctrl)

explainer_WCPO_LL <- explain(model = model_WCPO_LLCPUE_BET, data = Dataset[,-c(1:4)], y = Dataset$WCPO_LLCPUE_BET,label = "WCPO_LL")
mp_WCPO_LL <- model_performance(explainer_WCPO_LL)
vi_WCPO_LL <- variable_importance(explainer_WCPO_LL)
plot_WCPO_LL <- plot(predict_parts(explainer_WCPO_LL, new_observation = Dataset[1, ]))
cp_WCPO_LL <- ceteris_paribus(explainer_WCPO_LL, new_observation = new_observation)

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)
print(args)


# Importing the Dataset
dataset = read.csv(args[1], header=TRUE)

# Modeling the Data
model = lm(formula = y ~ x, data = dataset)

library(ggplot2)

png (paste0(args[1],"_scatter",".png"))
ggplot()+
    geom_point(aes(x = dataset$x, y = dataset$y), colour = 'red') +
    ggtitle('Y over X') +
    xlab('X') +
    ylab('Y')
dev.off ()

png (paste0(args[1],"_line",".png"))
ggplot()+
    geom_point(aes(x = dataset$x, y = dataset$y), colour = 'red') +
    geom_line(aes(x = dataset$x, y = predict(model, newdata = dataset)), colour = 'blue')+
    ggtitle('Y over X') +
    xlab('X') +
    ylab('Y')
dev.off ()

summary(model)
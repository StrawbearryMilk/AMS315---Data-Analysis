library(stochvol) #contains built-in function to compute logreturn
library(plotly) #used to make plot

amazon <- read.csv("AMZN.csv", header = TRUE, stringsAsFactors = FALSE)
netflix <- read.csv("NFLX.csv", header = TRUE, stringsAsFactors = FALSE)

#training data set to be used
training.log <- data.frame(logret(amazon$Adj.Close[1:252]),
                           logret(netflix$Adj.Close[1:252]))
colnames(training.log)<- c("Amazon", "Netflix")
#a) test to see if the expected means equal zero
t.test(training.log$Amazon,alternative = "two.sided") 
t.test(training.log$Netflix,alternative = "two.sided") 

#b) test to see if the expected means are significantly different,
#we first test to see if the variances are equal
var.test(training.log$Amazon,training.log$Netflix) 
#p << .05, so we reject H0, conclude variances are not equal
#Perform unpooled t-test
t.test(training.log$Amazon,training.log$Netflix,alternative = "two.sided", 
       var.equal = FALSE) 

#c) calculate correlation and then determine significance of this test
cor(training.log)
cor.test(training.log$Amazon,training.log$Netflix) 

#d) find regression equation, R^2, and confidence interval
y <- training.log$Netflix; x <- training.log$Amazon
fit <- lm(y ~ x, data= training.log)
summary(fit)
confint(fit, 'x', .95)

#e) Use training and testing data to make a plot for Netflix's stocks in December
#2017 with predicted data, actual data, and a prediction interval
x.test <- logret(amazon$Adj.Close) #logreturn vector of all Amazon data

#the following array uses the training data from the first four parts, 
#computes a predicted y value, then goes to the next value of the sequence 
#containing untrained data points, and computes a new predicted value. 
#This process is then repeated until the end value is reached, and every point 
#is added to a new vector, y.pred
y.pred <- sapply(252:length(x.test), 
       function(x) {sum(as.numeric(lm(logret(netflix$Adj.Close)[1:x-1] 
                          ~ x.test[1:x-1])$coefficients)*c(1,x.test[x]))})

#Current prediction is based on noting that exp(logreturn + logY_i) = Y_(i+1)
adjClose.pred <- exp(y.pred + log(netflix$Adj.Close)[251:270])
adjClose.true <- netflix$Adj.Close[253:272]
dates <- as.Date(netflix$Date[253:272], format = "%m/%d/%Y")

#find prediction interval bands
new.x <- data.frame(x = x.test[252:length(x.test)])
pred.int <- predict(fit, newdata = new.x, interval = "prediction")

#Convert elements of the prediction interval into price values
pred.int <- exp(pred.int)*netflix$Adj.Close[252:length(x.test)]
plot.data <- data.frame(dates, adjClose.pred, adjClose.true,
                        pred.int[,2],pred.int[,3])
names(plot.data) <- c("Dates","Pred.price","True.price","lwr","upr")

plot_ly(plot.data, x = ~Dates, y = ~Pred.price,type = "scatter", 
             mode = "lines+markers",name = "Predicted Price", 
             line = list(color = 'rgb(61, 119, 99)', width = 3)) %>%
  add_trace(y = ~True.price, name = "True Price", opacity = .5, 
            line = list(color = 'rgb(221, 172, 66)', 
                        width = 3, dash = "dot"))%>%
  add_trace(y = ~lwr, name = "Lower Prediction Band", line = list(
    color = rgb(0, 0, 0), width = 5)) %>%
  add_trace(y = ~upr, name = "Upper Prediction Band", line = list(
    color = rgb(0, 0, 0), width = 5)) %>%
  layout(title = "Price of Netflix Stocks in December 2017",
         xaxis = list(title = "December 2017"),
         yaxis = list(title = "Netflix Stock Prices"))
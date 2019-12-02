## Here is the code that I (Bjorn) used for the Adaboost question:

## Load data
library(mboost)
sp = read.csv2("spambase_lab1_block2.csv")
sp$Spam = as.factor(sp$Spam)

n=dim(sp)[1]
set.seed(12345)
id=sample(1:n, floor(n*(2/3)))
train=sp[id,]
test=sp[-id,]

## Adaboost ##
ada_model = blackboost(Spam ~., data = train, control = boost_control(mstop = 100, nu=0.6),
                       family = AdaExp())

## Loop through different numbers of trees

n = seq(10, 100, by = 10)
for(i in n){
  ada_model = blackboost(Spam ~., data = train, control = boost_control(mstop = i, nu = 0.6),
                         family = AdaExp())
  fitted.results_test= predict(ada_model,newdata=test)
  fitted.results_test = ifelse(fitted.results_test > 0.1,1,0)
  misClasificError_test = mean(fitted.results_test != test$Spam)
  ada_confmat_test = table("Y"=test$Spam,"Y hat"=fitted.results_test)
  acc[i] = sum(diag(ada_confmat_test)) / sum(ada_confmat_test)
} 
acc

## Clean up the accuracy vector and then plot it
y_error <- na.omit(1-acc)
y_error = y_error[-1]
x_trees <- n
ada_m <- na.omit(data.frame(x_trees, y_error))

ggplot()+
  ggtitle(" Error for Adaboost ")+
  geom_point(data=ada_m, aes(x=x_trees, y=y_error), size=2)+
  geom_line(data=ada_m, aes(x=x_trees, y=y_error), size=1)

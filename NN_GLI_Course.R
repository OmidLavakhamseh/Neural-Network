# conceptual example:
set.seed(1234567890)
x1 <- runif(1000,-1,1)
x2 <- runif(1000,-1,1)
tr <- data.frame(x1,x2,y=x1+x2)
head(tr)
winit <- runif(9,-1,1)
nn <- neuralnet(y~.,data=tr,hidden = 1,startweights = winit,
                act.fct = 'tanh')
plot(nn)
nn$weights
f <- seq(-2,2,0.1)
g <- tanh(f)
plot(f,g)
v <- tanh(-0.14*tr$x1-0.14*tr$x2)
points(0.13*tr$x1+0.13*tr$x2,v)

#1/-0.14 almost -6.97
# NN has set weights to use linear part of tanh to predict x1+x2 


### Next example.
set.seed(1234567890)
x <- runif(500,0,10)
df <- data.frame(x=x,sin=sin(x))
tr <- df[1:25,]
te <- df[26:500,]
num_hid_unit <- 10
plot(tr,col='red',cex=2)
points(te,col='green')
library(neuralnet)
winit <- runif(10,-1,1)
#regression problem
nn_model <- neuralnet(sin~.,tr,hidden = 10,startweights = winit)#default act func: logistic
pred <- predict(nn_model,newdata=te)
points(te[,1],pred,col=blues9)
####
Linear <- function(x){
  x
}
Relu <- function(x){
  ifelse(x>0,x,0)
}
softplus <- function(x){
  
  log(1+exp(x))
}
model_linear <- neuralnet(sin~.,data=tr,hidden = 10,startweights = winit,
                          act.fct = Linear)
plot(model_linear)
pred_l <- predict(model_linear,newdata=te)
points(te[,1],pred_l)
#
model_Relu <- neuralnet(sin~.,data=tr,hidden = 10,startweights = winit,
                        act.fct = Relu)

plot(model_Relu)
pred_R <- predict(model_Relu,newdata=te)
points(te[,1],pred_R,col='blue')
#
model_S <- neuralnet(sin~.,data=tr,hidden=10,startweights = winit,
                     act.fct = softplus)
pred_S <- predict(model_S,newdata=te)
points(te[,1],pred_S,col='grey')

#
mean((te[,2]-pred)**2)
mean((te[,2]-pred_l)**2)
mean((te[,2]-pred_R)**2)
mean((te[,2]-pred_S)**2)
#####newdata
newdata <- runif(500,0,50)
newdf <- data.frame(x=newdata,sin=sin(newdata))
pred_newdata <- predict(nn_model,newdata=newdf)
plot(newdf,ylim=c(-12,1))
points(newdf[,1],pred_newdata,col='green')
nn_model$weights#bias and weights
plot(nn_model)
nn_model$weights[[1]][[2]]#weight for out put of actfunction
###

head(df)
nn_model_vice <- neuralnet(x~sin,df,startweights =winit,hidden = 10,threshold = 0.1)
plot(df[,2],predict(nn_model_vice,df),col='green')


#test saturation:
newdata <- runif(500,0,20)
newdf <- data.frame(x=newdata,sin=sin(newdata))
mode_test <- neuralnet(sin~.,data=tr,hidden=1,startweights = winit)
plot(mode_test)
pred_newdata <- predict(mode_test,newdata=newdf)
plot(newdf[,1],pred_newdata)
sum(mode_test$weights[[1]][[2]])#it is saturating to the -0.4+0.6(1)=
#because the output of sigmoid is 1 when saturates.


#####




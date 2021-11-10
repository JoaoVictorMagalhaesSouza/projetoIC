library("neuralnet")
# creating training data set
TKS=c(20,10,30,20,80,30)
CSS=c(90,20,40,50,50,80)
Placed=c(1,0,0,0,1,1)
# Here, you will combine multiple columns or features into a single set of data
df=data.frame(TKS,CSS,Placed)

# load library
require(neuralnet)

# fit neural network
nn=neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
             linear.output = FALSE)

plot(nn)

for(i in 1:nrow(myData))
{if(myData$Area[i]>0.04){tmp[i]='XL'}
else if(myData$Area[i]>0.03){tmp[i]='L'}
else if(myData$Area[i]>0.02){tmp[i]='M'}
else if(myData$Area[i]>0.1){tmp[i]='S'}
else {tmp[i]='XS'} }


"Question 1"
myData=read.csv(file='myData.csv');
np = ceiling(0.1*nrow(myData));
test.index = sample(1:nrow(myData), np);
myData.testData = myData[test.index,];
myData.trainData = myData[-test.index,];
myData.tree = rpart(Label ~ Circ.+Feret+FeretX+FeretY+FeretAngle+MinFeret+AR+Round, 
    method = 'class', data=myData.trainData);
summary(myData.tree);
plot(myData.tree,margin = 0.1);text(myData.tree);

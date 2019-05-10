
#Preprocessing
for(i in 1:nrow(myData))
{if(myData$Area[i]>0.04){tmp[i]='XL'}
else if(myData$Area[i]>0.03){tmp[i]='L'}
else if(myData$Area[i]>0.02){tmp[i]='M'}
else if(myData$Area[i]>0.01){tmp[i]='S'}
else {tmp[i]='XS'} }


#Question 1
myData=read.csv(file='myData.csv');
np = ceiling(0.1*nrow(myData));
test.index = sample(1:nrow(myData), np);
myData.testData = myData[test.index,];
myData.testData = myData[-test.index,];
myData.tree = rpart(Label ~ Circ.+Feret+FeretX+FeretY+FeretAngle+MinFeret+AR+Round, 
    method = 'class', data=myData.testData);
summary(myData.tree);
plot(myData.tree,margin = 0.1);text(myData.tree);

size.testdata = myData$Label[-test.index];
test.predict=factor(
    predict(myData.tree, myData.testData,type = 'class', 
        levels=levels(size.testdata)));
table.testdata = table(size.testdata, test.predict);
table.testdata
#               test.predict
# size.testdata   M   S  XL  XS
#             L    4   0   7   0
#             M   32   6   1   0
#             S    8  97   0   9
#             XL   0   0   8   0
#             XS   0   8   0 395

table.testdata=table.testdata[2:5,];
# Because model didn't predict any 'L' symbol, I removed the row represent 'L'
# for calculating correctness easily

#               test.predict
# size.testdata   M   S  XL  XS
#             M   32   6   1   0
#             S    8  97   0   9
#             XL   0   0   8   0
#             XS   0   8   0 395

correct.testdata = correct.testdata = sum(diag(table.testdata))/sum(table.testdata)*100
correct.testdata
# [1] 94.32624


size.testdata = myData$Label[-test.index];
test.predict=factor(
    predict(myData.tree, myData.testData,type = 'class', 
        levels=levels(size.testdata)));
table.testdata = table(size.testdata, test.predict);
table.testdata
#              test.predict
# size.testdata  M  S XL XS
#            L   0  0  2  0
#            M   2  0  0  0
#            S   1 11  0  3
#            XL  0  0  1  0
#            XS  0  1  0 43

table.testdata=table.testdata[2:5,];
# Because model didn't predict any 'L' symbol, I removed the row represent 'L'
# for calculating correctness easily
#              test.predict
# size.testdata  M  S XL XS
#            M   2  0  0  0
#            S   1 11  0  3
#            XL  0  0  1  0
#            XS  0  1  0 43


correct.testdata = correct.testdata = sum(diag(table.testdata))/sum(table.testdata)*100
correct.testdata
# [1] 91.93548

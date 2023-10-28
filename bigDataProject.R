dataframe = read.csv("healthcare-dataset-stroke-data.csv")
is.null(dataframe)
dataframe = dataframe[!duplicated(dataframe), ]
dataframe = dataframe[, -1]
dataframe = subset(dataframe, bmi != "N/A")
dataframe$bmi = as.numeric(dataframe$bmi)
library(DescTools)
dataframe$smoking_status[dataframe$smoking_status == "Unknown"] = Mode(dataframe$smoking_status)
library(caret)
dmy <- dummyVars(" ~ .", data = dataframe)
dataframe = data.frame(predict(dmy, newdata = dataframe))
library(corrplot)
corrplot(cor(dataframe))
hist(dataframe$hypertension, main = "Hyper-tension", xlab = "hyper-tension")
ind = sample(2, nrow(dataframe), prob = c(0.8, 0.2), replace = TRUE)
train.data = dataframe[ind==1, ]
test.data = dataframe[ind==2, ]
data.pre = preProcess(train.data, method = "range")
train.data = predict(data.pre, train.data)
test.data = predict(data.pre, test.data)
library(party)
stroke.tree = ctree(stroke~., data = train.data)
plot(stroke.tree, type = "simple")
testpred = predict(stroke.tree, newdata = test.data)
testpred[testpred>=0.5] = 1
testpred[testpred<0.5] = 0
library(randomForest)
rf = randomForest(x = train.data[, -21], y = train.data[, 21])
testpred2 = predict(rf, newdata = test.data[, -21])
testpred2[testpred2 >= 0.5] = 1
testpred2[testpred2 < 0.5] = 0
glm = glm(formula = stroke ~ ., data = train.data)
testpred3 = predict(glm, newdata = test.data)
testpred3[testpred3 >= 0.5] = 1
testpred3[testpred3< 0.5] = 0
library(e1071)
library(class)
classifier_knn <- knn(train = train.data, test = test.data, cl = train.data$stroke, k = 2)
classifier_knn
unique(classifier_knn)
table(test.data$stroke)
table(classifier_knn, test.data$stroke)
accuracy <- mean(classifier_knn == test.data$stroke)
library(ipred)
bagger = bagging(formula = stroke ~ ., data = train.data)
testpred4 = predict(bagger, newdata = test.data)
testpred4[testpred4 >= 0.5] = 1
testpred4[testpred4 < 0.5] = 0
boxplot(dataframe$age, ylab = "Age boxplot", col = "red")
rug(dataframe$age,side=2)
boxplot(dataframe$avg_glucose_level, ylab = "Average glucose level boxplot", col = "green")
rug(dataframe$avg_glucose_level,side=2)
boxplot(dataframe$bmi, ylab = "bmi boxplot", col = "blue")
rug(dataframe$bmi,side=2)
strokewithage = aggregate(dataframe$stroke, list(dataframe$age), mean)
barplot(strokewithage$x, names.arg = strokewithage$Group.1, main = "Age and stroke")
strokewithglucose = aggregate(dataframe$stroke, list(dataframe$avg_glucose_level), mean)
barplot(strokewithglucose$x, names.arg = strokewithglucose$Group.1, main = "Average glucose level and stroke")
strokewithbmi = aggregate(dataframe$stroke, list(dataframe$bmi), mean)
barplot(strokewithbmi$x, names.arg = strokewithbmi$Group.1, main = "bmi and stroke")
numstrokemales = nrow(dataframe[dataframe$stroke == 1 & dataframe$genderMale == 1, ])
numstrokefemales = nrow(dataframe[dataframe$stroke == 1 & dataframe$genderFemale == 1, ])
strokepeoplesum = numstrokemales + numstrokefemales
pie(c(numstrokemales/strokepeoplesum*100, numstrokefemales/strokepeoplesum*100), c(paste("Percentage of males having stroke = ", numstrokemales/strokepeoplesum*100, "%"), paste("Percentage of females having stroke = ", numstrokefemales/strokepeoplesum*100, "%")), main = "Percenatges of males and females having stroke", col = c("red", "green"))
numestrokeurban = nrow(dataframe[dataframe$stroke == 1 & dataframe$Residence_typeUrban == 1, ])
numestrokerural = nrow(dataframe[dataframe$stroke == 1 & dataframe$Residence_typeRural == 1, ])
strokeresidencesum = numestrokerural + numestrokeurban
pie(c(numestrokeurban/strokeresidencesum*100, numestrokerural/strokeresidencesum*100), c(paste("Percentage of urban having stroke = ", numestrokeurban/strokeresidencesum*100, "%"), paste("Percentage of rural having stroke = ", numestrokerural/strokeresidencesum*100, "%")), main = "Percenatges of urban and rural having stroke", col = c("purple", "yellow"))
numestrokeurban = nrow(dataframe[dataframe$stroke == 1 & dataframe$Residence_typeUrban == 1, ])
numestrokerural = nrow(dataframe[dataframe$stroke == 1 & dataframe$Residence_typeRural == 1, ])
strokeresidencesum = numestrokerural + numestrokeurban
pie(c(numestrokeurban/strokeresidencesum*100, numestrokerural/strokeresidencesum*100), c(paste("Percentage of urban having stroke = ", numestrokeurban/strokeresidencesum*100, "%"), paste("Percentage of rural having stroke = ", numestrokerural/strokeresidencesum*100, "%")), main = "Percenatges of urban and rural having stroke", col = c("purple", "yellow"))
numstrokesmokes = nrow(dataframe[dataframe$stroke == 1 & dataframe$smoking_statussmokes == 1, ])
numstrokeneversmoked = nrow(dataframe[dataframe$stroke == 1 & dataframe$smoking_statusnever.smoked == 1, ])
numstrokeFS = nrow(dataframe[dataframe$stroke == 1 & dataframe$smoking_statusformerly.smoked == 1, ])
sumstrokesmokingstatus = numstrokesmokes + numstrokeneversmoked +numstrokeFS
pie(c(numstrokesmokes/sumstrokesmokingstatus*100, numstrokeneversmoked/sumstrokesmokingstatus*100, numstrokeFS/sumstrokesmokingstatus*100), c(paste("Percentage of smokers having stroke = ", numstrokesmokes/sumstrokesmokingstatus*100, "%"), paste("Percentage of non-smokers having stroke = ", numstrokeneversmoked/sumstrokesmokingstatus*100, "%"), paste("Percentage of non-smokers having stroke = ", numstrokeFS/sumstrokesmokingstatus*100, "%")), main = "Percenatges of formerly-smoked/never-smoked/smoking people having stroke", col = rainbow(3))
numestrokeHD1 = nrow(dataframe[dataframe$stroke == 1 & dataframe$heart_disease == 1, ])
numestrokeHD0 = nrow(dataframe[dataframe$stroke == 1 & dataframe$heart_disease == 0, ])
strokeHDsum = numestrokeHD1 + numestrokeHD0
pie(c(numestrokeHD1/strokeHDsum*100, numestrokeHD0/strokeHDsum*100), c(paste("Percentage of heart-diseased people having stroke = ", numestrokeHD1/strokeHDsum*100, "%"), paste("Percentage of non-heart-diseased people having stroke = ", numestrokeHD0/strokeHDsum*100, "%")), main = "Percenatges of non-heart-diseased/heart-diseased people having stroke", col = c("blue", "pink"))
numestrokemarried = nrow(dataframe[dataframe$stroke == 1 & dataframe$ever_marriedYes == 1, ])
numestrokesingle = nrow(dataframe[dataframe$stroke == 1 & dataframe$ever_marriedNo == 1, ])
strokemarriagesum = numestrokemarried + numestrokesingle
pie(c(numestrokemarried/strokemarriagesum*100, numestrokesingle/strokemarriagesum*100), c(paste("Percentage of married people having stroke = ", numestrokemarried/strokemarriagesum*100, "%"), paste("Percentage of single people having stroke = ", numestrokesingle/strokemarriagesum*100, "%")), main = "Percenatges of married and single people having stroke", col = c("red", "yellow"))
numestrokeHT1 = nrow(dataframe[dataframe$stroke == 1 & dataframe$hypertension == 1, ])
numestrokeHT0 = nrow(dataframe[dataframe$stroke == 1 & dataframe$hypertension == 0, ])
strokeHTsum = numestrokeHT1 + numestrokeHT0
pie(c(numestrokeHT1/strokeHTsum*100, numestrokeHT0/strokeHTsum*100), c(paste("Percentage of hyper-tensioned people having stroke = ", numestrokeHT1/strokeHTsum*100, "%"), paste("Percentage of non-hyper-tensioned people having stroke = ", numestrokeHT0/strokeHTsum*100, "%")), main = "Percenatges of non-hyper-tensioned/hyper-tensioned people having stroke", col = c("purple", "yellow"))

numstrokechild = nrow(dataframe[dataframe$stroke == 1 & dataframe$work_typechildren == 1, ])
numstrokegovt = nrow(dataframe[dataframe$stroke == 1 & dataframe$work_typeGovt_job == 1, ])
numstrokeneverworked = nrow(dataframe[dataframe$stroke == 1 & dataframe$work_typeNever_worked == 1, ])
numstrokeprivate = nrow(dataframe[dataframe$stroke == 1 & dataframe$work_typePrivate == 1, ])
numstrokeself = nrow(dataframe[dataframe$stroke == 1 & dataframe$work_typeSelf.employed == 1, ])
sumstrokeworktype = numstrokechild + numstrokegovt + numstrokeneverworked + numstrokeprivate + numstrokeself
pie(c(numstrokechild/sumstrokeworktype*100, numstrokegovt/sumstrokeworktype*100, numstrokeneverworked/sumstrokeworktype*100, numstrokeprivate/sumstrokeworktype*100, numstrokeself/sumstrokeworktype*100), c(paste("Percentage of children having stroke = ", numstrokechild/sumstrokeworktype*100, "%"), paste("Percentage of government workers having stroke = ", numstrokegovt/sumstrokeworktype*100, "%"), paste("Percentage of never worked people having stroke = ", numstrokeneverworked/sumstrokeworktype*100, "%"), paste("Percentage of private working people  having stroke = ", numstrokeprivate/sumstrokeworktype*100, "%"), paste("Percentage of self-employed people having stroke = ", numstrokeself/sumstrokeworktype*100, "%")), main = "Percenatges of people with different work type having stroke", col = rainbow(5))
plot(dataframe$age, dataframe$avg_glucose_level, xlab = "age", ylab = "avg_glucose_level")
plot(dataframe$age, dataframe$bmi, xlab = "age", ylab = "bmi")
plot(dataframe$bmi, dataframe$avg_glucose_level, xlab = "bmi", ylab = "avg_glucose_level")
plot(dataframe$bmi, dataframe$age, xlab = "bmi", ylab = "age")
plot(dataframe$avg_glucose_level, dataframe$bmi, xlab = "avg_glucose_level", ylab = "bmi")
plot(dataframe$avg_glucose_level, dataframe$age, xlab = "avg_glucose_level", ylab = "age")

print(paste('Logistic regression accuracy = ', mean(testpred3 == test.data$stroke)))
print(paste('Decision tree accuracy = ', mean(testpred == test.data$stroke)))
print(paste('random forest accuracy = ', mean(testpred2 == test.data$stroke)))
print(paste('KNN accuracy =', accuracy))
print(paste('Bagger accuracy = ', mean(testpred4 == test.data$stroke)))
model = aov(stroke ~ factor(genderFemale) + factor(genderMale) + genderOther + factor(age) + factor(hypertension) + factor(heart_disease) + factor(ever_marriedNo) + ever_marriedYes + factor(work_typechildren) + factor(work_typeGovt_job) + factor(work_typeNever_worked) + factor(work_typePrivate) + work_typeSelf.employed + factor(Residence_typeRural) +  Residence_typeUrban + factor(avg_glucose_level) + factor(bmi) + factor(smoking_statusformerly.smoked) + factor(smoking_statusnever.smoked) + smoking_statussmokes, data = dataframe)
summary(model)
TukeyHSD(model)
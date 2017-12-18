#R-Code
#data cleaning
DiabeticData<-read.csv("E:/KDD/10kDiabetes.csv",header = TRUE,sep=",")
DiabeticData
summary(DiabeticData)
install.packages("editrules")
library(editrules)
rule<-editfile("E:/KDD/rules.txt")
View(rule)
violate<-violatedEdits(rule,DiabeticData)
violate
summary(violate)
#checking pecentage of data vilating the rules
plot(violate)
#Replacing blanks with NA's
DiabeticData$admission_type_id<-replace(DiabeticData$admission_type_id,DiabeticData$admission_type_id=='',NA)
#PREDICTING RE-ADMITTANCE OF DIABETIC PATIENTS
DiabeticData$discharge_disposition_id<-replace(DiabeticData$discharge_disposition_id,DiabeticData$discharge_disposition_id=='',NA)
DiabeticData$admission_source_id<-replace(DiabeticData$admission_source_id,DiabeticData$admission_source_id=='',NA)
#Replacing ? with NA's
DiabeticData$race<-replace(DiabeticData$race,DiabeticData$race=='?',NA)
DiabeticData$weight<-replace(DiabeticData$weight,DiabeticData$weight=='?',NA)
DiabeticData$payer_code<-replace(DiabeticData$weight,DiabeticData$payer_code=='?',NA)
DiabeticData$diag_1<-replace(DiabeticData$weight,DiabeticData$diag_1=='?',NA)
DiabeticData$diag_2<-replace(DiabeticData$weight,DiabeticData$diag_2=='?',NA)
DiabeticData$diag_3<-replace(DiabeticData$weight,DiabeticData$diag_3=='?',NA)
DiabeticData$medical_specialty<-replace(DiabeticData$medical_specialty,DiabeticData$medical_specialty=='?',NA)
DiabeticData$medical_specialty<-replace(DiabeticData$medical_specialty,DiabeticData$medical_specialty=='?',NA)
summary(DiabeticData)
#Replacing NA with nearest neighbour using KNN function.
library(VIM)
DiabeticData1<-kNN(DiabeticData,variable=c("race","admission_type_id","medical_specialty","payer_code","weight","diag_1","diag_2","diag_3"),k=10)
summary(DiabeticData1)
# checking for violations again after replacing missing values
library(editrules)
rule<-editfile("E:/KDD/rules.txt")
View(rule)
violate1<-violatedEdits(rule,DiabeticData1)
violate1
summary(violate1)
#PREDICTING RE-ADMITTANCE OF DIABETIC PATIENTS 26
plot(violate1)
#removing weight column
DiabeticData2<-DiabeticData1[c(1:4,6:52)]
summary(DiabeticData2)
DiabeticData2$readmitted[DiabeticData2$readmitted=="TRUE"]<-1
DiabeticData2$readmitted[DiabeticData2$readmitted=="FALSE"]<-0
DiabeticData2$readmitted<-as.factor(DiabeticData2$readmitted)
DiabeticData2$readmitted<-as.numeric(DiabeticData2$readmitted)
DiabeticData2$gender1[DiabeticData2$gender=="Male"]<-1
DiabeticData2$gender1[DiabeticData2$gender=="Female"]<-0
DiabeticData2$gender1<-as.factor(DiabeticData2$gender1)
DiabeticData2$gender1<-as.numeric(DiabeticData2$gender1)
DiabeticData2$race1<- as.numeric(factor(DiabeticData2$race,levels=c("?","AfricanAmerican","Asian","Caucasian","Hispanic","Other")))
summary(DiabeticData2)
DiabeticData2$race1<- as.numeric(factor(DiabeticData2$race,levels=c("?","AfricanAmerican","Asian","Caucasian","Hispanic","Other")))
DiabeticData2$admission_type_id1<- as.numeric(factor(DiabeticData2$admission_type_id,levels=c("","Elective","Emergency","Newborn","Not Available","Not Mapped","Urgent")))
summary(DiabeticData2$admission_type_id1)
levels(DiabeticData2$admission_source_id)
write.csv(DiabeticData2, "E:/KDD/filename1.csv")
DiabeticData2$admission_source_id1<- as.numeric(factor(DiabeticData2$admission_source_id,levels=c("","Clinic Referral", "Court/Law Enforcement","Emergency Room","HMO Referral","Not Available","Not Mapped","Physician Referral","Transfer from a hospital","Transfer from a Skilled Nursing Facility (SNF)","Transfer from another health care facility")))
DiabeticData2$A1Cresult1<- as.numeric(factor(DiabeticData2$A1Cresult,levels=c(">7",">8","None","Norm")))
DiabeticData2$metformin1<- as.numeric(factor(DiabeticData2$metformin,levels=c("Down","No","Steady","Up")))
DiabeticData2$repaglinide1<- as.numeric(factor(DiabeticData2$repaglinide,levels=c("Down","No","Steady","Up")))
DiabeticData2$nateglinide1<- as.numeric(factor(DiabeticData2$nateglinide,levels=c("Down","No","Steady","Up")))
DiabeticData2$chlorpropamide1<- as.numeric(factor(DiabeticData2$chlorpropamide,levels=c("No","Steady","Up")))
DiabeticData2$chlorpropamide1<-as.numeric(DiabeticData2$chlorpropamide1)
DiabeticData2$glimepiride1<- as.numeric(factor(DiabeticData2$glimepiride,levels=c("Down","No","Steady","Up")))
DiabeticData2$glipizide1<- as.numeric(factor(DiabeticData2$glipizide,levels=c("Down","No","Steady","Up")))
DiabeticData2$glyburide1<- as.numeric(factor(DiabeticData2$glyburide,levels=c("Down","No","Steady","Up")))
DiabeticData2$pioglitazone1<- as.numeric(factor(DiabeticData2$pioglitazone,levels=c("Down","No","Steady","Up")))
DiabeticData2$rosiglitazone1<- as.numeric(factor(DiabeticData2$rosiglitazone,levels=c("Down","No","Steady","Up")))
DiabeticData2$miglitol1<- as.numeric(factor(DiabeticData2$miglitol,levels=c("Down","No","Steady","Up")))
DiabeticData2$insulin1<- as.numeric(factor(DiabeticData2$insulin,levels=c("Down","No","Steady","Up")))
DiabeticData2$glyburide.metformin1<- as.numeric(factor(DiabeticData2$glyburide.metformin,levels=c("Down","No","Steady","Up")))
DiabeticData2$glimepiride.pioglitazone1<- as.numeric(factor(DiabeticData2$glimepiride.pioglitazone,levels=c("No")))
DiabeticData2$citoglipton1<- as.numeric(factor(DiabeticData2$citoglipton,levels=c("No")))
DiabeticData2$glipizide.metformin1<- as.numeric(factor(DiabeticData2$glipizide.metformin,levels=c("No","Steady")))
DiabeticData2$metformin.rosiglitazone1<- as.numeric(factor(DiabeticData2$metformin.rosiglitazone,levels=c("No")))
DiabeticData2$metformin.pioglitazone1<- as.numeric(factor(DiabeticData2$metformin.pioglitazone,levels=c("No")))
DiabeticData2$glipizide1<- as.numeric(factor(DiabeticData2$glipizide,levels=c("No","Steady")))
DiabeticData2$examide1<- as.numeric(factor(DiabeticData2$examide,levels=c("No")))
DiabeticData2$tolazamide1<- as.numeric(factor(DiabeticData2$tolazamide,levels=c("No","Steady")))
DiabeticData2$troglitazone1<- as.numeric(factor(DiabeticData2$troglitazone,levels=c("No")))
DiabeticData2$acarbose1<- as.numeric(factor(DiabeticData2$acarbose,levels=c("No","Steady","Up")))
DiabeticData2$tolbutamide1<- as.numeric(factor(DiabeticData2$tolbutamide,levels=c("No","Steady")))
DiabeticData2$acetohexamide1<- as.numeric(factor(DiabeticData2$acetohexamide,levels=c("No")))
DiabeticData2$medical_specialty1<- as.numeric(factor(DiabeticData2$acetohexamide,levels=c("No")))
levels(DiabeticData2$medical_specialty)
summary(DiabeticData2)
DiabeticData2$max_glu_serum1<- as.numeric(factor(DiabeticData2$max_glu_serum,levels=c(">200",">300","None","Norm")))
levels(DiabeticData2$max_glu_serum)
DiabeticData2$diabetesMed1[DiabeticData2$diabetesMed=="Yes"]<-1
DiabeticData2$diabetesMed1[DiabeticData2$diabetesMed=="No"]<-0
DiabeticData2$diabetesMed1<-as.factor(DiabeticData2$diabetesMed1)
DiabeticData2$diabetesMed1<-as.numeric(DiabeticData2$diabetesMed1)
DiabeticData2$change1[DiabeticData2$change=="Ch"]<-1
DiabeticData2$change1[DiabeticData2$change=="No"]<-0
DiabeticData2$change1<-as.factor(DiabeticData2$change1)
DiabeticData2$change1<-as.numeric(DiabeticData2$change1)
DiabeticData2$time_in_hospital<-as.numeric(DiabeticData2$time_in_hospital)
DiabeticData2$num_lab_procedures<-as.numeric(DiabeticData2$num_lab_procedures)
DiabeticData2$num_procedures<-as.numeric(DiabeticData2$num_procedures)
DiabeticData2$num_medications<-as.numeric(DiabeticData2$num_medications)
DiabeticData2$number_outpatient<-as.numeric(DiabeticData2$number_outpatient)
DiabeticData2$number_emergency<-as.numeric(DiabeticData2$number_emergency)
DiabeticData2$number_inpatient<-as.numeric(DiabeticData2$number_inpatient)
DiabeticData2$number_diagnoses<-as.numeric(DiabeticData2$number_diagnoses)
DiabeticData2$diag_1<-as.numeric(DiabeticData2$diag_1)
DiabeticData2$diag_2<-as.numeric(DiabeticData2$diag_2)
DiabeticData2$diag_3<-as.numeric(DiabeticData2$diag_3)
DiabeticData2$A1Cresult1<-as.numeric(DiabeticData2$A1Cresult1)
DiabeticData2$metformin1<-as.numeric(DiabeticData2$metformin1)
DiabeticData2$repaglinide1<-as.numeric(DiabeticData2$repaglinide1)
DiabeticData2$A1Cresult1<-as.numeric(DiabeticData2$A1Cresult1)
DiabeticData2$metformin1<-as.numeric(DiabeticData2$metformin1)
DiabeticData2$repaglinide1<-as.numeric(DiabeticData2$repaglinide1)
DiabeticData2$nateglinide1<-as.numeric(DiabeticData2$nateglinide1)
DiabeticData2$glimepiride1<-as.numeric(DiabeticData2$glimepiride1)
DiabeticData2$acetohexamide1<-as.numeric(DiabeticData2$acetohexamide1)
DiabeticData2$glipizide1<-as.numeric(DiabeticData2$glipizide1)
DiabeticData2$glyburide1<-as.numeric(DiabeticData2$glyburide1)
DiabeticData2$tolbutamide1<-as.numeric(DiabeticData2$tolbutamide1)
DiabeticData2$pioglitazone1<-as.numeric(DiabeticData2$pioglitazone1)
DiabeticData2$rosiglitazone1<-as.numeric(DiabeticData2$rosiglitazone1)
DiabeticData2$troglitazone1<-as.numeric(DiabeticData2$troglitazone1)
DiabeticData2$tolazamide1<-as.numeric(DiabeticData2$tolazamide1)
DiabeticData2$examide1<-as.numeric(DiabeticData2$examide1)
DiabeticData2$citoglipton1<-as.numeric(DiabeticData2$citoglipton1)
DiabeticData2$insulin1<-as.numeric(DiabeticData2$insulin1)
DiabeticData2$glyburide.metformin1<-as.numeric(DiabeticData2$glyburide.metformin1)
DiabeticData2$glipizide.metformin1<-as.numeric(DiabeticData2$glipizide.metformin1)
DiabeticData2$glimepiride.pioglitazone1<-as.numeric(DiabeticData2$glimepiride.pioglitazone1)
DiabeticData2$metformin.rosiglitazone1<-as.numeric(DiabeticData2$metformin.rosiglitazone1)
DiabeticData2$metformin.pioglitazone1<-as.numeric(DiabeticData2$metformin.pioglitazone1)
levels(DiabeticData2$medical_specialty)
DiabeticData2$age1<- as.numeric(factor(DiabeticData2$age,levels=c("[0-10)","[10-20)","[20-30)","[30-40)","[40-50)","[50-60)","[60-70)","[70-80)","[80-90)","[90-100)")))
DiabeticData2$age1<-as.numeric(DiabeticData2$age1)
DiabeticData2$medical_specialty1<- as.numeric(factor(DiabeticData2$medical_specialty,levels=c("?","Anesthesiology-Pediatric","Cardiology","Cardiology-Pediatric","Emergency/Trauma","Endocrinology","Family/GeneralPractice","Gastroenterology","Gynecology","Hematology","Hematology/Oncology","Hospitalist","InfectiousDiseases","InternalMedicine","Nephrology","Neurology","Obsterics&Gynecology-GynecologicOnco","Obstetrics","ObstetricsandGynecology","Oncology","Ophthalmology","Orthopedics","Orthopedics-Reconstructive","Osteopath","Otolaryngology","OutreachServices","Pathology","Pediatrics","Pediatrics-CriticalCare","Pediatrics-EmergencyMedicine","Pediatrics-Endocrinology","Pediatrics-Hematology-Oncology","Pediatrics-Pulmonology","PhysicalMedicineandRehabilitation","PhysicianNotFound","Podiatry","Psychiatry","Psychology","Pulmonology","Radiologist","Radiology","Surgeon","Surgery-Cardiovascular","Surgery-Cardiovascular/Thoracic","Surgery-Colon&Rectal","Surgery-General","Surgery-Neuro","Surgery-Pediatric","Surgery-Plastic","Surgery-PlasticwithinHeadandNeck","Surgery-Thoracic","Surgery-Vascular","Urology")))
DiabeticData2$medical_specialty1<-as.numeric(DiabeticData2$medical_specialty1)
#################################################################################################
##correlation Analysis
write.csv(DiabeticData2, "E:/kdd/final.csv")
install.packages("Hmisc")
library(Hmisc)
sliceData<-DiabeticData2[c("readmitted","race1","glipizide1","medical_specialty1","admission_type_id1","gender1","max_glu_serum1","diabetesMed1","change1","time_in_hospital","num_lab_procedures","num_medications","num_procedures","number_diagnoses","number_emergency","number_inpatient","diag_1","diag_2","diag_3","number_diagnoses","A1Cresult1","metformin1","repaglinide1","nateglinide1","chlorpropamide1","glimepiride1","acetohexamide1","glyburide1","tolbutamide1","pioglitazone1","rosiglitazone1","acarbose1","miglitol1","troglitazone1","tolazamide1","examide1","citoglipton1","insulin1","glyburide.metformin1","glipizide.metformin1","glimepiride.pioglitazone1","metformin.rosiglitazone1","metformin.pioglitazone1")]
sliceData2<-DiabeticData2[c("readmitted","race1","medical_specialty1","admission_type_id1","gender1","max_glu_serum1","diabetesMed1","change1","time_in_hospital","num_lab_procedures","num_medications","num_procedures","number_diagnoses","number_emergency","number_inpatient","diag_1","diag_2","diag_3","number_diagnoses","A1Cresult1","metformin1","repaglinide1","nateglinide1","chlorpropamide1","glimepiride1","acetohexamide1","glyburide1","tolbutamide1","pioglitazone1","rosiglitazone1","acarbose1","miglitol1","troglitazone1","tolazamide1","examide1","citoglipton1","insulin1","glyburide.metformin1","glipizide.metformin1","glimepiride.pioglitazone1","metformin.rosiglitazone1","metformin.pioglitazone1")]
sliceData0<-DiabeticData2[c("readmitted","race1","admission_type_id1","gender1","change1","time_in_hospital","num_medications","number_diagnoses","number_emergency","number_inpatient","diag_1","diag_2","diag_3","A1Cresult1","acetohexamide1","glyburide1","tolbutamide1","pioglitazone1","rosiglitazone1","acarbose1","miglitol1","troglitazone1","tolazamide1","examide1","citoglipton1","insulin1","metformin.pioglitazone1")]
sliceData2
dataMatrix<-as.matrix(sliceData)
correlationMatrix<-rcorr(dataMatrix,type="pearson")
correlationMatrix
correlationMatrix1<-cor(dataMatrix)
install.packages("corrplot")
library(corrplot)
corrplot(correlationMatrix1,method = "circle")
str(sliceData)
#################################################################################################
#regression analysis
model1<-lm(readmitted~.,data=sliceData0)
summary(model1)
model2<-lm(readmitted~race1+admission_type_id1+diabetesMed1+change1+num_lab_procedures+num_procedures+number_diagnoses+number_emergency+number_inpatient+pioglitazone1+insulin1,data=sliceData2)
summary(model2)
anova(model1,model2)
#################################################################################################
#pca analysis(error)
sliceData2<-DiabeticData2[c("readmitted","race1","medical_specialty1","admission_type_id1","gender1","max_glu_serum1","diabetesMed1","change1","time_in_hospital","num_lab_procedures","num_medications","num_procedures","number_diagnoses","number_emergency","number_inpatient","diag_1","diag_2","diag_3","number_diagnoses","A1Cresult1","metformin1","repaglinide1","nateglinide1","chlorpropamide1","glimepiride1","acetohexamide1","glyburide1","tolbutamide1","pioglitazone1","rosiglitazone1","acarbose1","miglitol1","troglitazone1","tolazamide1","examide1","citoglipton1","insulin1","glyburide.metformin1","glipizide.metformin1","glimepiride.pioglitazone1","metformin.rosiglitazone1","metformin.pioglitazone1")]
dataMatrix2<-as.matrix(sliceData2)
install.packages("gdata")
library(gdata)
install.packages("pastecs")
library("pastecs")
cormatrix<-cor(dataMatrix2,method="pearson")
round(cormatrix,digits=3)
round(stat.desc(cormatrix),3)
install.packages("psych")
library(psych)
cortest.bartlett(dataMatrix2)
KMO(dataMatrix2)
det(dataMatrix2)
pa1<-principal(dataMatrix2,nfactors=31,rotate="none")
pa1
pa2<-principal(dataMatrix2,nfactors=31,rotate="none")
pa2
plot(pa1$values,type-"b")
print.psych(pa2,cut=0.3,sort=TRUE)
#################################################################################################
# logistic regression
dataMatrix2<-as.matrix(sliceData2)
dataMatrix2
dataMatrix2<-as.data.frame(dataMatrix2)
dataMatrix2$readmitted
dataMatrix2$readmitted[dataMatrix2$readmitted=="1"]<-0
dataMatrix2$readmitted[dataMatrix2$readmitted=="2"]<-1
dataMatrix2$readmitted
set.seed(100)
dataMatrix2
train <- sample (1:nrow(dataMatrix2), .75*nrow(dataMatrix2))
inputData <- dataMatrix2[train, ]
testData <- dataMatrix2[-train, ]
#creating the model
model <- glm(readmitted ~.,family=binomial(link='logit'),data=inputData)
summary(model)
#model using test data set
output<- predict(model,testData,type="response")
output
output<- ifelse(output > 0.5,1,0)
misClasificError <- mean(output != testData$readmitted)
print(paste('Accuracy',1-misClasificError))
#evaluating the accuracy
install.packages("ROCR")
library(ROCR)
library(ROCR)
p <- predict(model, testData, type="response")
pr <- prediction(p, testData$readmitted)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
###logistic 2
slicedata7<- dataMatrix2[c("readmitted","race1","admission_type_id1","gender1","diabetesMed1","num_lab_procedures","number_diagnoses","num_procedures","number_emergency","number_inpatient","pioglitazone1")]
slicedata7
slicedata7<-as.data.frame(slicedata7)
train <- sample (1:nrow(slicedata7), .75*nrow(slicedata7))
inputData_l1 <- slicedata7[train, ]
testData_l1 <- slicedata7[-train, ]
#creating the model
model1 <- glm(readmitted ~.,family=binomial(link='logit'),data=inputData_l1)
summary(model1)
#model using test data set
output<- predict(model1,testData_l1,type="response")
output
output<- ifelse(output > 0.5,1,0)
misClasificError <- mean(output != testData_l1$readmitted)
print(paste('Accuracy',1-misClasificError))
#evaluating the accuracy
install.packages("ROCR")
library(ROCR)
library(ROCR)
p <- predict(model1, testData, type="response")
pr <- prediction(p, testData$readmitted)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize=T)
abline(a=0 ,b=1)
#Area Under Curve
auc<-performance(pr,"auc")
auc<-unlist(slot(auc,"y.values"))
auc
auc<-round(auc,4)
legend(.6, .2, auc, title="AUC")
##logistic 3
reg3<-DiabeticData2[c("readmitted","race1","medical_specialty1","admission_type_id1","gender1","max_glu_serum1","diabetesMed1","change1","A1Cresult1","metformin1","repaglinide1","nateglinide1","chlorpropamide1","glimepiride1","acetohexamide1","glyburide1","tolbutamide1","pioglitazone1","rosiglitazone1","acarbose1","miglitol1","troglitazone1","tolazamide1","examide1","citoglipton1","insulin1","glyburide.metformin1","glipizide.metformin1","glimepiride.pioglitazone1","metformin.rosiglitazone1","metformin.pioglitazone1")]
reg3
reg3<-as.data.frame(reg3)
reg3$readmitted[reg3$readmitted=="1"]<-0
reg3$readmitted[reg3$readmitted=="2"]<-1
reg3$readmitted
set.seed(100)
train <- sample (1:nrow(reg3), .75*nrow(reg3))
inputData_l1 <- reg3[train, ]
testData_l1 <- reg3[-train, ]
#creating the model
model2 <- glm(readmitted ~.,family=binomial(link='logit'),data=inputData_l1)
summary(model2)
#model using test data set
output<- predict(model2,testData_l1,type="response")
output
output<- ifelse(output > 0.5,1,0)
misClasificError <- mean(output != testData_l1$readmitted)
print(paste('Accuracy',1-misClasificError))
#evaluating the accuracy
install.packages("ROCR")
library(ROCR)
library(ROCR)
p <- predict(model2, testData, type="response")
pr <- prediction(p, testData$readmitted)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize=T)
abline(a=0 ,b=1)
#Area Under Curve
auc<-performance(pr,"auc")
auc<-unlist(slot(auc,"y.values"))
auc
auc<-round(auc,4)
legend(.6, .2, auc, title="AUC")
##logistic 4
slicedata7<- dataMatrix2[c("readmitted","insulin1","race1","admission_type_id1","gender1","diabetesMed1",
                           "num_lab_procedures","number_diagnoses","num_procedures","number_emergency","number_inpatient","pioglitazone1")]
slicedata7
slicedata7<-as.data.frame(slicedata7)
train <- sample (1:nrow(slicedata7), .75*nrow(slicedata7))
inputData_l1 <- slicedata7[train, ]
testData_l1 <- slicedata7[-train, ]
#creating the model
model3 <- glm(readmitted ~.,family=binomial(link='logit'),data=inputData_l1)
summary(model3)
#model using test data set
output<- predict(model3,testData_l1,type="response")
output
output<- ifelse(output > 0.5,1,0)
misClasificError <- mean(output != testData_l1$readmitted)
print(paste('Accuracy',1-misClasificError))
#evaluating the accuracy
install.packages("ROCR")
library(ROCR)
library(ROCR)
p <- predict(model3, testData, type="response")
pr <- prediction(p, testData$readmitted)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize=T)
abline(a=0 ,b=1)
#Area Under Curve
auc<-performance(pr,"auc")
auc<-unlist(slot(auc,"y.values"))
auc
auc<-round(auc,4)
legend(.6, .2, auc, title="AUC")
#comparison of logistic regrestion models.
anova(model,model1,model2,model3)
#################################################################################################
#DECISION TRESS
tree1<- dataMatrix2[c("readmitted","insulin1","race1","admission_type_id1","gender1","diabetesMed1","num_lab_procedures","number_diagnoses","num_procedures","number_emergency","number_inpatient","pioglitazone1")]
library(data.table)
library(gdata)
library(dplyr)
dim(tree1)
data.frame(tree1)
typeof(tree1$readmitted)
tree1$readmitted<-factor(tree1$readmitted)
str(tree1)
testdata<-sample_frac(f, 0.2)
sid <-as.numeric(rownames(testdata))
traindata <- tree1[-sid,]
install.packages("party")
library(party)
fit1<- ctree(traindata$readmitted~., data =traindata, controls = ctree_control(mincriterion = .95, minsplit = 100))
fit1
summary(fit1)
#Predict Output
predicted= predict(fit1,testdata)
tab<-table(predicted,testdata$readmitted)
tab
predicted
error = mean(predicted != testdata$readmitted)
error
plot(fit1)
#Decision tree 2
tree2<-DiabeticData2[c("readmitted","race1","medical_specialty1","admission_type_id1","gender1","max_glu_serum1","diabetesMed1","change1","A1Cresult1","metformin1","repaglinide1","nateglinide1","chlorpropamide1","glimepiride1","acetohexamide1","glyburide1","tolbutamide1","pioglitazone1","rosiglitazone1","acarbose1","miglitol1","troglitazone1","tolazamide1","examide1","citoglipton1","insulin1","glyburide.metformin1","glipizide.metformin1","glimepiride.pioglitazone1","metformin.rosiglitazone1","metformin.pioglitazone1")]
library(data.table)
library(gdata)
library(dplyr)
dim(tree2)
data.frame(tree2)
typeof(tree2$readmitted)
tree2$readmitted<-factor(tree2$readmitted)
str(tree2)
testdata<-sample_frac(f, 0.2)
sid <-as.numeric(rownames(testdata))
traindata <- tree2[-sid,]
install.packages("party")
library(party)
fit2 <- ctree(traindata$readmitted~., data =traindata, controls = ctree_control(mincriterion = .95, minsplit = 100))
fit2
summary(fit2)
#Predict Output
predicted= predict(fit2,testdata)
tab<-table(predicted,testdata$readmitted)
tab
predicted
error = mean(predicted != testdata$readmitted)
error
plot(fit2)
#Decision tree 3
write.csv(dataMatrix2, "E:/KDD/filename2.csv")
library(data.table)
library(gdata)
library(dplyr)
f<-read.csv("E:/KDD/filename2.csv", header=TRUE, strip.white = TRUE,na.strings = c(""," ",".","NA"))
dim(f)
data.frame(f)
typeof(f$readmitted)
f$readmitted<-factor(f$readmitted)
str(f)
testdata<-sample_frac(f, 0.2)
sid <-as.numeric(rownames(testdata))
traindata <- f[-sid,]
install.packages("party")
library(party)
fit3 <- ctree(traindata$readmitted~race1+medical_specialty1+admission_type_id1+gender1+max_glu_serum1+diabetesMed1+change1+time_in_hospital+num_lab_procedures+num_medications+num_procedures+number_diagnoses+number_emergency+number_inpatient+diag_1+diag_2+diag_3+number_diagnoses.1+A1Cresult1+metformin1+repaglinide1+nateglinide1+chlorpropamide1+glimepiride1+acetohexamide1+glyburide1+tolbutamide1+pioglitazone1+rosiglitazone1+acarbose
              1+miglitol1+troglitazone1+tolazamide1+examide1+citoglipton1+insulin1+glyburide.metformin1+glipizide.metformin1+glimepiride.pioglitazone1+metformin.rosiglitazone1+metformin.pioglitazone1, data =traindata, controls = ctree_control(mincriterion = .95, minsplit = 100))
fit3
summary(fit3)
#Predict Output
predicted= predict(fit3,testdata)
tab<-table(predicted,testdata$readmitted)
tab
predicted
error = mean(predicted != testdata$readmitted)
error
plot(fit3)
#comparison of decision tree models
anova(fit1,fit2,fit3)
################################################################################################ Text Analysis########
install.packages("tm")
library(tm)
txt1<-Corpus(VectorSource(DiabeticData1$diag_1_desc)) #converting diag_1_desc into doc
inspect(txt1)
txt2<-Corpus(VectorSource(DiabeticData1$diag_2_desc)) #converting diag_2_desc into doc
inspect(txt2)
txt3<-Corpus(VectorSource(DiabeticData1$diag_3_desc)) #converting diag_3_desc into doc
inspect(txt3)
punCleanedTxt1<-tm_map(txt1,removePunctuation) #removing punctuations
inspect(punCleanedTxt1)
punCleanedTxt2<-tm_map(txt2,removePunctuation) #removing punctuations
inspect(punCleanedTxt2)
punCleanedTxt3<-tm_map(txt3,removePunctuation) #removing punctuations
inspect(punCleanedTxt3)
lowCleanedTxt1<-tm_map(punCleanedTxt1,tolower) #converting to lower case
inspect(lowCleanedTxt1)
lowCleanedTxt2<-tm_map(punCleanedTxt2,tolower) #converting to lower case
inspect(lowCleanedTxt2)
lowCleanedTxt3<-tm_map(punCleanedTxt3,tolower) #converting to lower case
inspect(lowCleanedTxt3)
stripCleanedText1<-tm_map(lowCleanedTxt1,removeWords,c("unspecified","type","without")) #removing some words
stripCleanedText2<-tm_map(lowCleanedTxt2,removeWords,c("unspecified","type","stated","mention","without")) #removing some words
stripCleanedText3<-tm_map(lowCleanedTxt3,removeWords,c("unspecified","type","stated","mention","without")) #removing some words
cleaned_diag_1_desc<-tm_map(stripCleanedText1, removeWords, stopwords("english")) #removing stop words
inspect(cleaned_diag_1_desc)
cleaned_diag_2_desc<-tm_map(stripCleanedText2, removeWords, stopwords("english")) #removing stop words
inspect(cleaned_diag_2_desc)
cleaned_diag_3_desc<-tm_map(stripCleanedText3, removeWords, stopwords("english")) #removing stop words
inspect(cleaned_diag_3_desc)
diag1DescMatrix<-DocumentTermMatrix(cleaned_diag_1_desc)
diag2DescMatrix<-DocumentTermMatrix(cleaned_diag_2_desc)
diag3DescMatrix<-DocumentTermMatrix(cleaned_diag_3_desc)
freq1<-colSums(as.matrix(diag1DescMatrix))
freq2<-colSums(as.matrix(diag2DescMatrix))
freq3<-colSums(as.matrix(diag3DescMatrix))
ord1 <- order(freq1,decreasing=TRUE)
ord2 <- order(freq2,decreasing=TRUE)
ord3 <- order(freq3,decreasing=TRUE)
freq1[head(ord1)]
freq2[head(ord2)]
freq3[head(ord3)]
freq1[tail(ord1)]
freq2[tail(ord2)]
freq3[tail(ord3)]
findAssocs(diag1DescMatrix,"diabetes",0.8)
findAssocs(diag1DescMatrix,"chronic",0.6)
findAssocs(diag1DescMatrix,"mellitus",0.6)
findAssocs(diag1DescMatrix,"malignant",0.6)
fft1<-findFreqTerms(diag1DescMatrix, lowfreq = 500)
fft2<-findFreqTerms(diag2DescMatrix, lowfreq = 500)
fft3<-findFreqTerms(diag3DescMatrix, lowfreq = 500)
install.packages("wordcloud")
library(wordcloud)
d1<-as.matrix(diag1DescMatrix)
v1<-sort(colSums(d1),decreasing = TRUE)
df1<-data.frame(word=names(v1),freq=v1)
head(df1,100)
set.seed(34)
wordcloud(words = df1$word, freq = df1$freq, min.freq = 50,
          max.words=1000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
d2<-as.matrix(diag2DescMatrix)
v2<-sort(colSums(d1),decreasing = TRUE)
df2<-data.frame(word=names(v2),freq=v2)
head(df2,100)
set.seed(34)
wordcloud(words = df2$word, freq = df2$freq, min.freq = 50,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
d3<-as.matrix(diag2DescMatrix)
v3<-sort(colSums(d1),decreasing = TRUE)
df3<-data.frame(word=names(v2),freq=v2)
head(df3,100)
set.seed(134)
wordcloud(words = df3$word, freq = df3$freq, min.freq = 50,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
tdmat <- as.matrix(removeSparseTerms(diag1DescMatrix, sparse=0.3))
distMatrix <- dist(scale(tdmat))
fit <- hclust(distMatrix, method="ward.D2")
kmeans1<- kmeans(diag1DescMatrix, 10)
#Merge cluster assignment back to keywords
kw_with_cluster <- as.data.frame(cbind(searchkeywords$'Natural Search Keyword', kmeans1$cluster))
names(kw_with_cluster) <- c("keyword", "kmeans5")
#####################Task 2####################
library(data.table)
library(gdata)
library(dplyr)
f<-read.csv("final.csv", header=TRUE, strip.white = TRUE,na.strings = c(""," ",".","NA"))
dim(f)
View(f)
data.frame(f)
typeof(f$readmitted)
typeof(f$time_in_hospital)
f$rowID<-NULL
f$readmitted<-factor(f$readmitted)
f$time_in_hospital<-factor(f$time_in_hospital)
str(f)
testdata<-sample_frac(f, 0.2)
sid <-as.numeric(rownames(testdata))
traindata <- f[-sid,]
View(traindata)
install.packages("party")
library(party)
#x <- cbind(x_train,y_train)
# grow tree
#model <- glm(f$readmitted ~ (diabetesMed + admission_type_id + gender + num_procedures + number_emergency + number_inpatient),family = binomial(link = 'logit'),data = traindata )
fit <- ctree(traindata$time_in_hospital ~(traindata$num_lab_procedures +traindata$num_medications + traindata$num_procedures + traindata$number_diagnoses + traindata$number_emergenc +traindata$number_inpatient + traindata$number_outpatient), data = traindata, controls = ctree_control(mincriterion = .99, minsplit = 1500))
#$num_lab_procedures + d$num_medications +d$num_procedures + d$number_diagnoses + d$number_emergenc +d$number_inpatient + d$number_outpatient
fit
summary(fit)
#Predict Output
predicted= predict(fit,testdata)
tab<-table(predicted,testdata$time_in_hospital)
tab
predicted
accuracy = mean(predicted != testdata$time_in_hospital)
accuracy
plot(fit)
#############logistic regression############
d<-read.csv("ram.csv", header=TRUE, strip.white = TRUE,na.strings = c(""," ",".","NA"))
d1<-d$num_medications
d1<-cbind(d$num_procedures,d$number_diagnoses, d$number_emergency, d$number_inpatient, d$number_outpatient)
d1[,-3]
d2<-d1[,1]
as.matrix(d1)
install.packages("mvoutlier")
library(mvoutlier)
uni.plot(d)
boxplot(d)
View(d)
install.packages("mlogit")
library(mlogit)
g<-lm(d$time_in_hospital~d$num_lab_procedures+d$num_medications+d$num_procedures+d$number_diagnoses+ d$number_emergenc+d$number_inpatient+d$number_outpatient, family="binomial")
summary(g)
################naive bayes code############
#######################################################################################63 percent accuracy
data <-read.csv("E:/KDD/10kDiabetes.csv",na.strings = c("?","","na"))
install.packages("e1071")
library(e1071)
test_sample = sample(1:nrow(data), size=0.2*nrow(data))
test_data <- data[test_sample,]
training_data <- data[-test_sample,]
Naivesmodel <- naiveBayes(readmitted ~ (insulin + race + diabetesMed + admission_type_id + gender + num_procedures + number_emergency + number_inpatient +
                                          PREDICTING RE-ADMITTANCE OF DIABETIC PATIENTS 47
                                        metformin.pioglitazone),data = training_data,method="knn",metric='Accuracy',tuneGrid=expand.grid(.k=5:100))
naivespredict = predict(Naivesmodel, test_data, type = "class")
table(naivespredict)
errornaives = sum(naivespredict != test_data$readmitted)/nrow(test_data)
errornaives
#-----------------------------------------------------------------------------60 accuracy
data <-read.csv("E:/KDD/10kDiabetes.csv",na.strings = c("?","","na"))
install.packages("e1071")
library(e1071)
test_sample = sample(1:nrow(data), size=0.2*nrow(data))
test_data <- data[test_sample,]
training_data <- data[-test_sample,]
Naivesmodel <- naiveBayes(readmitted ~ ((admission_type_id + race + medical_specialty + max_glu_serum + gender + change + diabetesMed + A1Cresult + metformin + repaglinide + nateglinide + chlorpropamide + glimepiride + acetohexamide + glipizide + glyburide + tolbutamide + pioglitazone + rosiglitazone + acarbose + miglitol + troglitazone + tolazamide + examide + citoglipton + insulin + glyburide.metformin + glipizide.metformin + glimepiride.pioglitazone + metformin.rosiglitazone + metformin.pioglitazone)),data = training_data,method="knn",metric='Accuracy',tuneGrid=expand.grid(.k=5:100))
naivespredict = predict(Naivesmodel, test_data, type = "class")
table(naivespredict)
errornaives = sum(naivespredict != test_data$readmitted)/nrow(test_data)
errornaives
#------------------------------------------------------------------------------64.3 accuracy
data <-read.csv("E:/KDD/10kDiabetes.csv",na.strings = c("?","","na"))
install.packages("e1071")
library(e1071)
test_sample = sample(1:nrow(data), size=0.2*nrow(data))
test_data <- data[test_sample,]
training_data <- data[-test_sample,]
Naivesmodel <- naiveBayes(readmitted ~ ((admission_type_id + number_emergency + number_inpatient + race + num_procedures + num_lab_procedures + num_medications + number_diagnoses + medical_specialty + time_in_hospital + diag_1_desc + diag_2_desc + diag_3_desc + max_glu_serum + gender + change + diabetesMed + A1Cresult + metformin + repaglinide + nateglinide + chlorpropamide + glimepiride + acetohexamide + glipizide + glyburide + tolbutamide + pioglitazone + rosiglitazone + acarbose + miglitol + troglitazone + tolazamide + examide + citoglipton + insulin + glyburide.metformin + glipizide.metformin + glimepiride.pioglitazone + metformin.rosiglitazone + metformin.pioglitazone)),data = training_data,method="knn",metric='Accuracy',tuneGrid=expand.grid(.k=5:100))
naivespredict = predict(Naivesmodel, test_data, type = "class")
table(naivespredict)
errornaives = sum(naivespredict != test_data$readmitted)/nrow(test_data)
errornaives
###########################################################################################################
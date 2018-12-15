rm(list=ls(all=T))
setwd("C:/Users/DELL/Desktop/edwisor/project1")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"Metrics","psych","party","rpart.plot")

lapply(x, require, character.only = TRUE)
rm(x)

## Read the data
Absenteeism_at_work = read.csv("Absenteeism_at_work_Project.csv", header = T, na.strings = c(" ", "", "NA"))

###########################################Explore the data##########################################
str(Absenteeism_at_work)

##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(Absenteeism_at_work,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(Absenteeism_at_work)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Missing_perc.csv", row.names = F)

ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()

####Convert work load avg from factor to numeric######
#Relace comma by blank
Absenteeism_at_work$Work.load.Average.day=gsub(",","",Absenteeism_at_work$Work.load.Average.day)
Absenteeism_at_work$Work.load.Average.day=as.numeric(as.character(Absenteeism_at_work$Work.load.Average.day))
#check datatype
str(Absenteeism_at_work)

#Remove all 0s and NAs from target variable
str(Absenteeism_at_work)
Absenteeism_at_work=Absenteeism_at_work[!is.na(Absenteeism_at_work$Absenteeism.time.in.hours) & !(Absenteeism_at_work$Absenteeism.time.in.hours)==0,]


# kNN Imputation
Absenteeism_at_work = knnImputation(Absenteeism_at_work, k = 3)
sum(is.na(Absenteeism_at_work))

write.csv(Absenteeism_at_work, 'Absenteeism_at_work_missing.csv', row.names = F)
multi.hist(Absenteeism_at_work[,c(1:4)], main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(Absenteeism_at_work[,c(12:13,15:16)], main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(Absenteeism_at_work[,c(5,21)], main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")


### BoxPlots - Distribution and Outlier Check
#print(colnames(Absenteeism_at_work))
cnames = colnames(Absenteeism_at_work[,-c(1:5,12:13,15:16)])
#print(cnames)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(Absenteeism_at_work))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of Absenteeism time in hours for",cnames[i])))
}

# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn10,gn11,ncol=3)
gridExtra::grid.arrange(gn2,gn3,gn4,ncol=3)
gridExtra::grid.arrange(gn5,gn6,gn7,ncol=3)
gridExtra::grid.arrange(gn8,gn9,ncol=2)

# #Replace all outliers with NA and impute
# #create NA on "custAge
for(i in cnames){
  val = Absenteeism_at_work[,i][Absenteeism_at_work[,i] %in% boxplot.stats(Absenteeism_at_work[,i])$out]
  #print(length(val))
  Absenteeism_at_work[,i][Absenteeism_at_work[,i] %in% val] = NA
}

Absenteeism_at_work = knnImputation(Absenteeism_at_work, k = 3)

cor(Absenteeism_at_work[,-c(1:5,12:13,15:16,21)])
##################################Feature Selection################################################
## Correlation Plot 
corrgram(Absenteeism_at_work[,-c(1:5,12:13,15:16,21)], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

cnames=colnames(Absenteeism_at_work[,-c(1:5,12:13,15:16,21)])

# #Standardisation
for(i in cnames){
  print(i)
  Absenteeism_at_work[,i] = (Absenteeism_at_work[,i] - mean(Absenteeism_at_work[,i]))/
    sd(Absenteeism_at_work[,i])
}

#Drop ID and correlated column as it wont be of any requirement

Absenteeism_at_work=Absenteeism_at_work[,-c(1,9,18,19)]
str(Absenteeism_at_work)

#Divide data into train and test using stratified sampling method
set.seed(123)
train.index = createDataPartition(Absenteeism_at_work$Absenteeism.time.in.hours, p = .80, list = FALSE)
train = Absenteeism_at_work[ train.index,]
test  = Absenteeism_at_work[-train.index,]


##Decision tree for classification
#Develop Model on training data
C50_model = rpart(Absenteeism.time.in.hours ~., data=train)

#Summary of DT model
summary(C50_model)
prp(C50_model)
C50_Predictions = predict(C50_model, test[,-17])
rmse(test[,17],C50_Predictions)

###Random Forest
RF_model = randomForest(Absenteeism.time.in.hours ~., train, importance = TRUE, ntree = 500)
#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-17])
rmse(test[,17],RF_Predictions)

#Develop Linear Regression model
LM_model = lm(Absenteeism.time.in.hours ~., train)

#predict on test cases #raw
LM_Predictions = predict(LM_model, test[,1:17])
rmse(test[,17],LM_Predictions)
summary(LM_model)
anova(LM_model)
LM_model2 = update(LM_model,. ~ . - Month.of.absence-Seasons-Hit.target-Disciplinary.failure-Education-Pet-Body.Mass.Index)
summary(LM_model2)
f=as.data.frame(table(Absenteeism_at_work$Reason.for.absence))

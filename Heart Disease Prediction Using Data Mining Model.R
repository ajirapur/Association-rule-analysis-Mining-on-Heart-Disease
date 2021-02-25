

install.packages("dplyr")
library(arules)
# association visulization
library(arulesViz)
install.packages("arulesViz", dependencies = TRUE)

# lebaling the colunm name 
h.data <-read.csv("C:/Users/jirap/Desktop/R-Projects/FinalProject-AIT664/heart(version1).csv")

View(h.data)


name_new <- c("Age","sex","chest_pain_type","rest_bp","chol","fasting_bloodsugar",
              "rest_ecg","max_heartrate","exercise_angina",
              "ST_depression","slope","n_major_vessel","thal","target")

names(h.data) <- name_new

# changed Age into range 
h.data$Age <- cut(h.data$Age,breaks =c(18, 35,45,65,Inf),labels=c("18-34", "35-45","46-55", "65+"))

# lebaling to variable  data

h.data$sex[h.data$sex == "1"]= "male"
h.data$sex[h.data$sex == "0"] = "female"
h.data$n_major_vessel[h.data$n_major_vessel == "0"]= "A"
h.data$n_major_vessel[h.data$n_major_vessel == "1"]= "b"
h.data$n_major_vessel[h.data$n_major_vessel == "2"]= "c"
h.data$n_major_vessel[h.data$n_major_vessel == "3"]= "d"
h.data$chest_pain_type[h.data$chest_pain_type == "0"]= "typical angina"
h.data$chest_pain_type[h.data$chest_pain_type == "1"]= "atypical angina"
h.data$chest_pain_type[h.data$chest_pain_type == "2"]= "non-anginal pain"
h.data$chest_pain_type[h.data$chest_pain_type == "3"]= "asymptomatic"
h.data$fasting_bloodsugar[h.data$fasting_bloodsugar == 1]= "True"
h.data$fasting_bloodsugar[h.data$fasting_bloodsugar == 0] = "False"
h.data$rest_ecg[h.data$rest_ecg == 0] = "Normal"
h.data$rest_ecg[h.data$rest_ecg == 1] = "Abnormality"
h.data$rest_ecg[h.data$rest_ecg == 2] = "Probable or definite"
h.data$exercise_angina[h.data$exercise_angina == "1"]= "yes"
h.data$exercise_angina[h.data$exercise_angina == "0"] = "no"
h.data$slope[h.data$slope == "0"]= "Upsloping"
h.data$slope[h.data$slope == "1"] = "Flat"
h.data$slope[h.data$slope == "2"] = "Downsloping"
h.data$target[h.data$target == "0"]= "Presnce"
h.data$target[h.data$target == "1"]= "Absence"


#discretize
h.data$ST_depression <- discretize(h.data$ST_depression)
h.data$chol <- discretize(h.data$chol)
h.data$rest_bp <- discretize(h.data$rest_bp)


#changed variables into factor

h.data$slope=as.factor(h.data$slope)
h.data$thal=as.factor(h.data$thal)
h.data$target=as.factor(h.data$target)
h.data$sex=as.factor(h.data$sex)
h.data$fasting_bloodsugar=as.factor(h.data$fasting_bloodsugar)
h.data$exercise_angina=as.factor(h.data$exercise_angina)
h.data$Age=as.factor(h.data$Age)

#APPLY R-RULES , WHERE Target=Absence
View(h.data)

h.rules<-apriori(h.data,parameter =list(supp=0.5,conf=1,maxlen=10),
                 appearance = list(rhs=c("target=Absence")))



inspect(subset(h.rules,subset = lhs %pin% "slope" )) 
inspect(subset(h.rules,subset = lhs %pin% "Age" ))


head(quality(h.rules)) 
new_subset<-inspect(head(h.rules,n=10,by="lift")) 
str(h.data)

plot(new_subset)


# find The  Redundant rules
subset.matrix <- is.subset(h.rules, h.rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# Remove redundant rules
h.rules <- h.rules[!redundant]




inspect(head(h.rules,n=10))




#View(trans)
#4/29/2019
#8.21 pm
inspect(subset(h.rules,subset = lhs %pin% "sex" ))
new <-inspect(subset(h.rules,subset = lhs %pin% "slope",n=10)) 
 # the code generating output

#newone<-subset(h.rules,subset = lhs %ain%  c("Age=", "sex="))




plot(h.rules,measure = c("support"),shading = "confidence")


plot(new_subset)


plot(h.rules,method="paracoord")

# 8.Double Decker plots

onerule <- sample(rules,1)
inspect(onerule)

plot(onerule, method= "doubledecker",data= Groceries)

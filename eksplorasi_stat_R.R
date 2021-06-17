NewBWF_players = read.csv(file="/Users/amefedoraignaciaginting/Documents/Semester6 /StatMul/Tugas Besar/BWF_players19.csv", header = TRUE, sep=",", dec=".", na.strings="NA")
is.na(BWF_players)

na_dataNew = BWF_players[!complete.cases(BWF_players),]

#NA Height Change
#Asia
BWF_players$Height[NewBWF_players$Name == "Adnan Maulana"] <- 175
BWF_players$Height[NewBWF_players$Name == "Bagas Maulana"] <- 175
BWF_players$Height[NewBWF_players$Name == "Cheah Yee See"] <- 164
BWF_players$Height[NewBWF_players$Name == "Di Zi Jian"] <- 183
BWF_players$Height[NewBWF_players$Name == "Lu Guang Zu"] <- 183
BWF_players$Height[NewBWF_players$Name == "Muhammad Shohibul Fikri"] <- 175
BWF_players$Height[NewBWF_players$Name == "Nguyen Thuy Linh"] <- 165
BWF_players$Height[NewBWF_players$Name == "Pearly Tan"] <- 164
BWF_players$Height[NewBWF_players$Name == "Pitha Haningtyas Mentari"] <- 165
BWF_players$Height[NewBWF_players$Name == "Sim Yu-jin"] <- 170
BWF_players$Height[NewBWF_players$Name == "Siti Fadia Silva Ramadhanti"] <- 165
BWF_players$Height[NewBWF_players$Name == "Taichi Saito"] <- 170
BWF_players$Height[NewBWF_players$Name == "Wang Chang"] <- 183
BWF_players$Height[BWF_players$Name == "Yeremia Erich Yoche Yacob Rambitan"] <- 175


#Europe
NewBWF_players$Height[NewBWF_players$Name == "Nhat Nguyen"] <- 183
NewBWF_players$Height[NewBWF_players$Name == "Qi Xuefei"] <- 186

#Pan Am
NewBWF_players$Height[NewBWF_players$Name == "Ryan Chew"] <- 172

#NA Hand Change
BWF_players$Hand[BWF_players$Name == "Nguyen Thuy Linh"] <- "Right"
BWF_players$Hand[NewBWF_players$Name == "Ryan Chew"] <- "Right"
BWF_players$Hand[NewBWF_players$Name == "Sim Yu-jin"] <- "Right"
BWF_players$Hand[NewBWF_players$Name == "Wang ZhiYi"] <- "Right"

#NA Medal Change
BWF_players$Medals[BWF_players$Name == "Chang Tak Ching"] <- 4
BWF_players$Medals[BWF_players$Name == "Ekaterina Malkova"] <- 4
BWF_players$Medals[BWF_players$Name == "Kang Min-hyuk"] <- 4
BWF_players$Medals[BWF_players$Name == "Kim Jae-hwan"] <- 4
BWF_players$Medals[BWF_players$Name == "Ng Wing Yung"] <- 4
BWF_players$Medals[BWF_players$Name == "Ryan Chew"] <- 4

#change multiple
BWF_players$Multiple[NewBWF_players$Name == "Di Zi Jian"] <- 1

#change Age
BWF_players$Age[NewBWF_players$Name == "Vu Thi Trang"] <- 29
BWF_players$Age[NewBWF_players$Name == "Ayako Sakuramoto"] <- 25
BWF_players$Age[NewBWF_players$Name == "Cheung Ngan Yi"] <- 28
BWF_players$Age[BWF_players$Name == "Wakana Nagahara"] <- 25

#change Height
BWF_players$Height[BWF_players$Name == "Kang Min-hyuk"] <- 183

#change career
BWF_players$Career[BWF_players$Name == "Cheah Yee See"] <- 124
BWF_players$Career[BWF_players$Name == "Huang Yu Xiang"] <- 102
BWF_players$Career[BWF_players$Name == "Akira Koga"] <- 90
BWF_players$Career[BWF_players$Name == "Qi Xuefei"] <- 114
BWF_players$Career[BWF_players$Name == "Lee Yong-dae"] <- 810
BWF_players$Career[BWF_players$Name == "Fitriani Fitriani"] <- 112
BWF_players$Career[BWF_players$Name == "Zhang Yi Man"] <- 66

#change medals
BWF_players$Medals[BWF_players$Name == "Lin Dan"] <- 21
#-------
  
BWF_players$Gender = as.factor(NewBWF_players$Gender)
BWF_players$Hand = as.factor(NewBWF_players$Hand)
BWF_players$Category = as.factor(NewBWF_players$Category)
BWF_players$Continent = as.factor(NewBWF_players$Continent)
BWF_players$Multiple = as.factor(NewBWF_players$Multiple)
BWF_players$Country = as.factor(NewBWF_players$Country)

BWF_players$Age = as.integer(BWF_players$Age)
BWF_players$Medals= as.integer(BWF_players$Medals)
BWF_players$Height = as.integer(BWF_players$Height)
BWF_players$Medals = as.integer(BWF_players$Medals)
BWF_players$Career = as.integer(BWF_players$Career)

#---------

BWF_players <- NewBWF_players

#Bivariat Kategorik Vs Kategorik 
#Gender x Hand
counts <- table(BWF_players$Gender, BWF_players$Hand)
barplot(counts, main="Distribusi Tangan Pemain Badminton Berdasarkan Gender",
        xlab="Class", col=c("yellow", "pink"), beside=T,
        legend.text = rownames(counts),
        args.legend = list(x = "top"))


#Bivariat Kategorik Vs Numerik
#Career x Continent
boxplot(BWF_players$Career~NewBWF_players$Continent,
        xlab="Continent", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Continent")
boxplot(BWF_players$Career~NewBWF_players$Gender,
        xlab="Gender", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Gender")
boxplot(BWF_players$Career~NewBWF_players$Category,
        xlab="Category", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Category")
boxplot(BWF_players$Career~NewBWF_players$Hand,
        xlab="Hand", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Hand")
boxplot(BWF_players$Career~NewBWF_players$Multiple,
        xlab="Multiple", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Multiple")


boxplot(BWF_players$Age~BWF_players$Continent,
        xlab="Continent", ylab="Athlete Age",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Continent")

boxplot(BWF_players$Age~BWF_players$Hand,
        xlab="Continent", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Continent")

boxplot(BWF_players$Age~BWF_players$Multiple,
        xlab="Continent", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Continent")

boxplot(BWF_players$Age~BWF_players$Gender,
        xlab="Continent", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Continent")
boxplot(BWF_players$HRank~BWF_players$Gender,
        xlab="Continent", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Continent")
boxplot(BWF_players$HRank~BWF_players$Continent,
        xlab="Continent", ylab="Athlete Total Wins",
        col=topo.colors(3), main="Boxplot Athlete Total Wins by Continent")


#Numerik Vs Numerik
xx <- BWF_players$Age
yy <- BWF_players$Career
plot(xx, yy, type="p", xlab="Age (days)", ylab="Circumference (mm)",
     pch=2, cex=1.5,col="purple",lwd=2)

xx <- BWF_players$HRank
yy <- BWF_players$Age
plot(xx, yy, type="p", xlab="Age (days)", ylab="Circumference (mm)",
     pch=2, cex=1.5,col="purple",lwd=2)

xx <- BWF_players$Medals
yy <- BWF_players$Career
plot(xx, yy, type="p", xlab="Age (days)", ylab="Circumference (mm)",
     pch=2, cex=1.5,col="purple",lwd=2)

xx <- BWF_players$Medals
yy <- BWF_players$Height
plot(xx, yy, type="p", xlab="Age (days)", ylab="Circumference (mm)",
     pch=2, cex=1.5,col="purple",lwd=2)

xx <- BWF_players$Height
yy <- BWF_players$Career
plot(xx, yy, type="p", xlab="Age (days)", ylab="Circumference (mm)",
     pch=2, cex=1.5,col="purple",lwd=2)

xx <- BWF_players$Age
yy <- BWF_players$Medals
plot(xx, yy, type="p", xlab="Age (days)", ylab="Circumference (mm)",
     pch=2, cex=1.5,col="purple",lwd=2)

#----- grafik regresi linear

hrank <- BWF_players$HRank
age <- BWF_players$Age
height <- BWF_players$Height
career <- BWF_players$Career
medal <- BWF_players$Medals


#regresi medal dan career
(ols <- lm(career ~ medal))
#hrank adalah variabel tak bebas
#age adalah variabel bebas. Age adalah x atau beta0 (intercept)
#sebelah kanan akan jadi sumbu x

plot(medal, career, pch = 16, cex = 1.3, col = "blue",main = "",
     xlab = "medal", ylab = "career")
abline(ols, col="red",lwd=2) #ini adalah garis regresinya
segments(bodymass, height, bodymass, predict(ols))
text(60, 190, labels =
       substitute(paste(y[i],"=","98,0054+0,9528",x[i],"+",epsilon[i])))
text(60, 200, labels =
       substitute(paste(tilde(y)[i],"=","98,0054+0,9528",x[i])))

#regresi
(ols <- lm(age ~ medal))
plot(medal, age, pch = 16, cex = 1.3, col = "blue",main = "",
     xlab = "medal", ylab = "age")
abline(ols, col="red",lwd=2) #ini adalah garis regresinya
segments(bodymass, height, bodymass, predict(ols))

#regresi
(ols <- lm(career ~ age))
plot(age, career, pch = 16, cex = 1.3, col = "blue",main = "",
     xlab = "age", ylab = "career")
abline(ols, col="red",lwd=2) #ini adalah garis regresinya
segments(bodymass, height, bodymass, predict(ols))

#regresi
(ols <- lm(age ~ height))
plot(height, age, pch = 16, cex = 1.3, col = "blue",main = "",
     xlab = "hrank", ylab = "career")
abline(ols, col="red",lwd=2) #ini adalah garis regresinya
segments(bodymass, height, bodymass, predict(ols))

#regresi
(ols <- lm(age ~ height))
plot(height, age, pch = 16, cex = 1.3, col = "blue",main = "",
     xlab = "hrank", ylab = "career")
abline(ols, col="red",lwd=2) #ini adalah garis regresinya
segments(bodymass, height, bodymass, predict(ols))

#regresi banyak variable
ols1 <- lm(Career~ Age + Height + Gender + Country + Hand + Medals + HRank + Multiple,data=BWF_players)

ols1 <- lm(Career ~ Age*Multiple + Gender + Height + Gender + relevel(Country,ref="Indonesia") + Hand + Medals*HRank, data = NewBWF_players1)
summary(ols1)

#interaction plot 
gender <- BWF_players$Gender
hand <- BWF_players$Hand
multiple <- BWF_players$Multiple
category <- BWF_players$Category
continent <- BWF_players$Continent
category <- BWF_players$Category
career <- BWF_players$Career

interaction.plot(gender, multiple, medal, main="Interaction Plot of
career by Gender*Multiple", xlab = "Gender", ylab = "Medal",
                 col=c("red","blue"),lty=c(1,2),pch=3,legend = F)
legend("topleft",inset = .1, c("Rangkap","Tidak"),col=c("red","blue"), lty=c(1,2), pch=3, title="Sport Intensity")
#ada interaksi

interaction.plot(continent, hand, career, main="Interaction Plot of
career by Continent*Hand", xlab = "Continent", ylab = "career",
                 col=c("red","blue"),lty=c(1,2),pch=3,legend = F)
legend("topright",inset = .1, c("Left","Right"),col=c("red","blue"), lty=c(1,2), pch=3, title="Hand")
#ada interaksi

interaction.plot(category, hand, career, main="Interaction Plot of
career by Category*Hand", xlab = "Continent", ylab = "career",
                 col=c("red","blue"),lty=c(1,2,3,4),pch=3,legend = F)
legend("topright",inset = .1, c("Left","Right"),col=c("red","blue"), lty=c(1,2), pch=3, title="Hand")
#ada interaksi

interaction.plot(category, continent, career, main="Interaction Plot of
career by Category*Continent", xlab = "Continent", ylab = "career",
                 col=c("red","blue"),lty=c(1,2,3,4),pch=3,legend = F)
legend("topright",inset = .1, c("Left","Right"),col=c("red","blue"), lty=c(1,2), pch=3, title="Hand")
#ada interaksi

interaction.plot(category, multiple, career, main="Interaction Plot of
career by Continent*Hand", xlab = "Continent", ylab = "career",
                 col=c("red","blue"),lty=c(1,2,3,4),pch=3,legend = F)
legend("topright",inset = .1, c("Left","Right"),col=c("red","blue"), lty=c(1,2), pch=3, title="Hand")
#ada interaksi

#--pemilihan parameter pemodelan dengan corrplot
shapiro.test(BWF_players$Age)
shapiro.test(BWF_players$Height)
shapiro.test(BWF_players$Medals)
shapiro.test(BWF_players$HRank)
shapiro.test(BWF_players$Career)

library(corrplot)
#career.num <- BWF_players[-which(colnames(BWF_players)==c("Category","Gender"))]
career.num <- data.frame(BWF_players$Age, BWF_players$Height, BWF_players$Medals, BWF_players$HRank, BWF_players$Career)
mat <- cor(career.num) #matriks korelasi
corrplot(mat, method="circle")
plot(waist$Age, waist$Waistline,pch=20,col="red")
plot(waist$Mass, waist$Waistline, pch=8, col="blue")
plot(waist$Height, waist$Waistline, pch=15, col="darkgreen")


#---Regresi

#regresi banyak variable
ols <- lm(Career~Age + Height + Gender + Country + Hand + Medals + HRank + Multiple,data=BWF_players)
summary(ols)
AIC(ols)

#ols1 <- lm(Career~Age*Multiple + Gender + Height + Gender + relevel(Country,ref="Indonesia") + Hand + Medals*HRank, data = NewBWF_players1)
#summary(ols1)
#AIC(ols1)

ols2 <- lm(Career~Age + Height + Gender + Multiple + Country + Hand + Medals + HRank, data=BWF_players)
summary(ols2)
AIC(ols2)

ols3 <-lm(Career~ Age*Medals*Gender + Multiple + Height*Hand + relevel(Country,ref="China") + HRank, data=BWF_players)
summary(ols3)
AIC(ols3)
#AIC 3531

ols4 <-lm(Career~ Age*Medals*Height*Gender + Category*Multiple + Country*HRank*Medals + Hand*Height, data=BWF_players)
summary(ols4)
AIC(ols4)
#AIC 3504

ols5 <-lm(Career~ Age*Medals*Height*Gender + Category + Multiple + Country*HRank*Medals + Hand*Height, data=BWF_players)
summary(ols5)
AIC(ols5)

ols6 <-lm(Career~ Age*Medals*Height*Gender + Category+ Country*HRank*Medals*Multiple + Hand*Height, data=BWF_players)
summary(ols6)
AIC(ols6)
#3450

ols7 <-lm(Career~ Age*Medals*Height*Gender + Category+ Country*HRank*Medals*Multiple + Hand + Height, data=BWF_players)
summary(ols7)
AIC(ols7)

ols8 <-lm(Career~ Age*Medals*Height*Gender + Category*Age + Country*HRank*Medals*Multiple + Hand + Height, data=BWF_players)
summary(ols8)
AIC(ols8)
#AIC 3428.779


ols9 <-lm(Career~ Age*Medals*Height*Gender + Category*Age*Height + Continent*Height + Country*HRank*Medals*Multiple + Hand, data=BWF_players)
summary(ols9)
AIC(ols9)
#AIC 3429

ols9 <-lm(Career~Age*Medals*Height*Gender + Category*Age*Height + Continent*Hand + Country*HRank*Medals*Multiple + Hand*Multiple, data=BWF_players)
AIC(ols9)
#3437
stepwise1 <- step(ols9,direction="both")


ols10 <- lm(Career ~ Age*HRank*Medals*Height*Gender + Multiple + Category*Age*Height + relevel(Country,ref="Indonesia") + Continent*Category*Hand, data = BWF_players)
summary(ols10)
AIC(ols10)
#3459


ols11 <-lm(Career~ Age*Medals*Height*Gender + Category*Age*Height + Continent*Height + Country*HRank*Medals*Multiple + Hand*Medals, data=BWF_players)
summary(ols11)
AIC(ols11)
# AIC 3428.753
stepwise1 <- step(ols11,direction="both")

ols12 <-lm(Career~ Age*HRank + Height + Hand + Category + Multiple + Continent + Country + Medals*HRank*Age + Gender, data=BWF_players)
summary(ols12)
AIC(ols12)
#AIC 3428.091

ols13 <-lm(Career~ Age*HRank + Category + Hand + Medals*HRank + Multiple*Height + Continent + Gender + Country*Multiple , data=BWF_players)
summary(ols13)
AIC(ols13)
#3441.939

ols <-lm(Career ~ Age + Country + Hand + Medals + Continent + Multiple+ Category + HRank + Hand*Continent + HRank*Medals + Age*HRank, data =BWF_players)
AIC(ols)
#3439


#--------


ols11 <-lm(Career~ Age*Medals*Height*Gender + Category*Age*Height + Continent*Height + Country*HRank*Medals*Multiple + Hand*Medals, data=BWF_players)
summary(ols11)
AIC(ols11)
# AIC 3428.753
stepwise1 <- step(ols11,direction="both")
summary(stepwise1)
extractAIC(stepwise1)

ols12 <-lm(Career~ Age + Medals + Gender + Age*Medals + Medals*Gender, data=BWF_players)
summary(ols12)
AIC(ols12)
stepwise2 <- step(ols12,direction="both")
summary(stepwise2)
extractAIC(stepwise2)
# AIC
x

#-----
ols <- lm(Career~Age + Height + Country + Hand + Medals + HRank + Multiple + Category + Gender + Continent,data=BWF_players)
summary(ols)
AIC(ols)

#no continent
ols <- lm(Career~Age + Height + Country + Medals + HRank + Multiple + Category + Gender + Hand,data=BWF_players)
summary(ols)
AIC(ols)

#no hand
ols <- lm(Career~Age + Height + Country + Medals + HRank + Multiple + Category + Gender,data=BWF_players)
summary(ols)
AIC(ols)

#no category
ols <- lm(Career~Age + Height + Country + Medals + HRank + Multiple + Gender,data=BWF_players)
summary(ols)
AIC(ols)

#no gender
ols <- lm(Career~Age + Height + Country + Medals + HRank + Multiple,data=BWF_players)
summary(ols)
AIC(ols)


#no height
ols <- lm(Career~Age + Country + Medals + HRank + Multiple,data=BWF_players)
summary(ols)
AIC(ols)

ols <- lm(Career~Age + Country + Medals + HRank + Multiple + ,data=BWF_players)
summary(ols)
AIC(ols)

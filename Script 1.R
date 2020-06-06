#------------------------------------------------------
# Importing data

dset <- read.csv("wbcd0.csv", header = TRUE, sep="\t"); View(dset)
head(dset)

#------------------------------------------------------
# Part 1 
# Summary for each variable 
summary(dset)


#------------------------------------------------------
# Part 2 
# Correlation between radius, area and perimeter
pairs(dset[c("perimeter","radius","area")])
cor(dset$perimeter, dset$radius)
cor(dset$area, dset$radius)
cor(dset$perimeter, dset$area)

#------------------------------------------------------
# Part 3
beni <- ifelse(dset$diagnosis == "B",0,1); 
plot(dset$perimeter,beni, col = "red")
modelperi <- glm(beni ~ dset$perimeter, data = dset, family=binomial); summary(modelperi)

plot(dset$area,beni, col = "red")
modelarea <- glm(beni ~ dset$area, data = dset, family=binomial); summary(modelarea)

plot(dset$radius,beni, col = "red")
modelradius <- glm(beni ~ dset$radius, data = dset, family=binomial); summary(modelradius)

plot(dset$perimeter*dset$radius*dset$area,beni, col = "green",main = "Mal/Bei vs radius*area*perimeter")
dset$geo <- dset$perimeter*dset$radius*dset$area
modelgeo <- glm(beni ~ dset$geo, data = dset, family=binomial); summary(modelgeo)

plot(dset$texture,beni, col = "green",main = "Mal/Bei vs Texture")
modeltexture <- glm(beni ~ dset$texture, data = dset, family=binomial); summary(modeltexture)

plot(dset$smoothness,beni, col = "red",main = "Mal/Bei vs Smoothness")
modelsmoothness <- glm(beni ~ dset$smoothness, data = dset, family=binomial); summary(modelsmoothness)

plot(dset$compactness,beni, col = "green",main = "Mal/Bei vs Compactness")
modelcompactness <- glm(beni ~ dset$compactness, data = dset, family=binomial); summary(modelcompactness)

plot(dset$concavity,beni, col = "green",main = "Mal/Bei vs Concavity")
modelconcavity <- glm(beni ~ dset$concavity, data = dset, family=binomial); summary(modelconcavity)

plot(dset$points,beni, col = "green",main = "Mal/Bei vs Points")
modelpoints <- glm(beni ~ dset$points, data = dset, family=binomial); summary(modelpoints)

plot(dset$symmetry,beni, col = "red",main = "Mal/Bei vs Symmetry")
modelsymmetry <- glm(beni ~ dset$symmetry, data = dset, family=binomial); summary(modelsymmetry)

plot(dset$dimension,beni, col = "red",main = "Mal/Bei vs Dimension")   
modeldimension <- glm(beni ~ dset$dimension, data = dset, family=binomial); summary(modeldimension)


#------------------------------------------------------
# Part 4

area <- dset$area
perimeter <- dset$perimeter
radius <- dset$perimeter

geo <- dset$geo
texture <- dset$texture
smoothness <- dset$smoothness
compactness <- dset$compactness
concavity <- dset$concavity
points <- dset$points
symmetry <- dset$symmetry
dimensions <- dset$dimension

maindset <- data.frame(beni, area, perimeter, radius, geo, texture, smoothness, compactness, concavity, points, symmetry, dimensions)
#train_sample<-sample(1,469)
cred_train<-maindset[1:469, ]; tail(cred_train)
cred_test<- maindset[470:569, ]; head(cred_test)

model1 <- glm(beni ~ geo + texture + smoothness + compactness + concavity + points + symmetry + dimensions, data = cred_train, family=binomial); summary(model1)
anova(model1, test = "Chisq")

model2 <- glm(beni ~ geo + texture + compactness + points , data = cred_train, family=binomial); summary(model2)
anova(model2, test = "Chisq")


library(gmodels)
credpreds1 <- predict(model2, newdata = cred_test, type = "response")
pre <- ifelse(credpreds1>.4,1,0); 
CrossTable(beni[470:569], pre[1:100])
print("accuracy is 94%" )





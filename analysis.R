#!/usr/bin/R
library(lattice)                                                                                                       
library(plot3D)

raw_data <- read.csv("data.csv", sep=',', header=TRUE)

ratio_1 <- raw_data$Ratio_TA
ratio_2 <- raw_data$Ratio_LP
ratio_1[ratio_1 == -Inf] = 0
ratio_2[ratio_2 == -Inf] = 0
biomass_1 <- raw_data$Biomass_TA
biomass_2 <- raw_data$Biomass_LP
gardener <- as.factor(raw_data$Gardener)

#---------------------------------------------------------------------
#results, displaying shit

biomass1_means = aggregate(biomass_1 ~ ratio_1 + ratio_2, raw_data, mean, na.action=na.exclude)
biomass2_means = aggregate(biomass_2 ~ ratio_1 + ratio_2, raw_data, mean, na.action=na.exclude)

biomass1_matrix = xtabs(biomass_1 ~ ratio_1 + ratio_2 , data=biomass1_means)
biomass2_matrix = xtabs(biomass_2 ~ ratio_1 + ratio_2 , data=biomass2_means)

biomass1_matrix[biomass1_matrix == 0 ] = NA
biomass2_matrix[biomass2_matrix == 0 ] = NA

biomass2_matrix = t(biomass2_matrix)

#how to color shit
new.palette=colorRampPalette(c("khaki", "yellow", "darkgoldenrod1", "darkorange", "firebrick1", "brown4"),space="rgb", bias=2, interpolate="linear")

#when only 1 individual vs shit
png(filename="Biomass_1_intra.png")
plot(ratio_1[ratio_2 == "1"], biomass_1[ratio_2 == "1"], main="Intraspecies - TA", xlab = "ratio TA (no LP)", ylab="biomass species TA", na.color="gray")

png(filename="Biomass_2_intra.png")
plot(ratio_2[ratio_1 == "1"], biomass_2[ratio_1 == "1"], main="Intraspecies - LP", xlab = "ratio LP (no TA)", ylab = "biomass species LP", na.color="gray")

png(filename="Biomass_1_inter.png")
plot(ratio_2[ratio_1 == "1"], biomass_1[ratio_1 == "1"], main="Interspecies - TA", xlab="ratio LP (TA fixed to one individual)", ylab="biomass TA",na.color="gray")

png(filename="Biomass_2_inter.png")
plot(ratio_1[ratio_2 == "1"], biomass_2[ratio_2 == "1"], main="Interspecies - LP", xlab="ratio TA (LP fixed to one individual)", ylab="biomass LP",na.color="grey")

#all together and shit
#here goes all the labeling and shit
png(filename="Biomass_1.png")
levelplot(biomass1_matrix, col.regions = new.palette, xlab = "TA [seeds/pot]", ylab = "LP [seeds/pot]", zlab="TA Biomass [mg]", main="Biomass TA [mg]", na.color="gray")

png(filename="Biomass_2.png")
levelplot(biomass2_matrix, col.regions = new.palette, xlab = "LP [seeds/pot]", ylab = "TA [seeds/pot]", main="Biomass LP [mg]", na.color="gray")

#---------------------------------------------------------------------
#results, analysing shit

model1 <- lm(biomass_1 ~ ratio_1*ratio_2)
summary(model1)

model2 <- lm(biomass_2 ~ ratio_1*ratio_2)
summary(model2)

model2a <- lm(biomass_2[ratio_1 < 36]~ ratio_1[ratio_1 < 36]*ratio_2[ratio_1 < 36])
summary(model2a)
model2b <- lm(biomass_2[ratio_1 >= 36]~ ratio_1[ratio_1 >= 36]*ratio_2[ratio_1 >= 36])
summary(model2b)

#CHECK AND CHANGE HERE - interaction ? etc.

#getting conditions shit
png(filename="conditions_linreg_model1.png")
par(mfrow=c(2,2))
plot(model1)
png(filename="conditions_linreg_model2.png")
par(mfrow=c(2,2))
plot(model2)

png(filename="conditions_linreg_model2a.png")
par(mfrow=c(2,2))
plot(model2a)

png(filename="conditions_linreg_model2b.png")
par(mfrow=c(2,2))
plot(model2b)


#if you want to text file the shit
sink("model1.txt")
print(summary(model1))
sink() 

sink("model2.txt")
print(summary(model2))
sink() 

sink("model2<36.txt")
print(summary(model2a))
sink() 

sink("model2>36.txt")
print(summary(model2b))
sink() 

#Plotting inter/intra and shit

png(filename="model2-inter-updated.png")
x = seq(0,30,1)
y = model2a$coefficients[1] + x*model2a$coefficients[2]
plot(ratio_1, biomass_2, col="black", main="Interspecies - LP", xlim=c(0,60), ylim=c(0,16), xlab="ratio TA", ylab="biomass LP")
lines(x,y, type="l", col="blue", lwd=3)
x = seq(36,60,1)
y = model2b$coefficients[1] + x*model2b$coefficients[2]
lines(x,y, type="l", col="blueviolet", lwd=3)
legend("topright",c("TA < 36", "TA >= 36"),col=c("blue", "blueviolet"), lty=c(1,1))

#the special case of LP - Multi color plot

png(filename="Biomass_2_inter_full.png")
model3 <- lm(biomass_2[ratio_2 == '1'] ~ ratio_1[ratio_2 == '1']+ratio_2[ratio_2 == '1'])
summary(model3)
#plot(ratio_1[ratio_2 == "1"], biomass_2[ratio_2 == "1"], main="Interspecies - LP", xlab="ratio TA", ylab="biomass LP", col="black")
x = seq(0,60,1)
y = model3$coefficients[1] + x*model3$coefficients[2]
plot(x,y, type="l", main="LP Biomass - Linear Regressions", xlab="LP density [seeds/pot]", ylab="LP Biomass [mg]", col="black")
colors = c("darkred", "red", "darkorange", "gold", "yellow", "green", "cyan", "deepskyblue", "darkorchid", "deeppink")
for(i in 1:10)
{
    a = 6*i
    #points(ratio_1[ratio_2 == a], biomass_2[ratio_2 == a], col=paste("grey",8*i))

    model3 <- lm(biomass_2[ratio_2 == a] ~ ratio_1[ratio_2 == a]+ratio_2[ratio_2 == a])

    y = model3$coefficients[1] + x*model3$coefficients[2]
    lines(x,y, col=colors[i])
}
legend("topright",c("TA Density = 1","TA Density = 6","TA Density = 12","TA Density = 18","TA Density = 24","TA Density = 30","TA Density = 36","TA Density = 42","TA Density = 48","TA Density = 54","TA Density = 60"),col=c("black","darkred", "red", "darkorange", "gold", "yellow", "green", "cyan", "deepskyblue", "darkorchid", "deeppink"), lty=c(1,1))

#Multicolor shit is down here

png(filename="Biomass_2_inter_fullest.png")
model3 <- lm(biomass_2[ratio_2 == '1'] ~ ratio_1[ratio_2 == '1']+ratio_2[ratio_2 == '1'])
summary(model3)
plot(ratio_1[ratio_2 == "1"], biomass_2[ratio_2 == "1"], main="Interspecies - LP", xlab="ratio TA", ylab="biomass LP", col="black")
x = seq(0,60,1)
y = model3$coefficients[1] + x*model3$coefficients[2]
plot(x,y, type="l", main="Interspecies - LP", xlab="ratio LP", ylab="biomass LP", ylim=c(0,16), col="black")
colors = c("darkred", "red", "darkorange", "gold", "yellow", "green", "cyan", "deepskyblue", "darkorchid", "deeppink")
for(i in 1:10)
{
    a = 6*i
    points(ratio_1[ratio_2 == a], biomass_2[ratio_2 == a], col=colors[i])

    model3 <- lm(biomass_2[ratio_2 == a] ~ ratio_1[ratio_2 == a]+ratio_2[ratio_2 == a])

    y = model3$coefficients[1] + x*model3$coefficients[2]
    lines(x,y, col=colors[i])
}
legend("topright",c("TA Density = 1","TA Density = 6","TA Density = 12","TA Density = 18","TA Density = 24","TA Density = 30","TA Density = 36","TA Density = 42","TA Density = 48","TA Density = 54","TA Density = 60"),col=c("black","darkred", "red", "darkorange", "gold", "yellow", "green", "cyan", "deepskyblue", "darkorchid", "deeppink"), lty=c(1,1))
#
#---------------------------------------------------------------------
#results, displaying the analysed shit

fitpoints <- predict.lm(model1)

png(filename="surface_model1.png")
x = seq(0,60,1)
y = seq(0,60,1)
M = mesh(x,y)
z = model1$coefficients[1] + M$x*model1$coefficients[2] + M$y*model1$coefficients[3] + M$x*M$y*model1$coefficients[4]

scatter3D(ratio_1, ratio_2, biomass_1, xlim=c(0,60), ylim=c(0,60), xlab="TA density [seeds/pot]", ylab="LP density [seeds/pot]", zlab="TA biomass [mg]", clab="[mg]", main="TA Biomass - Linear Regression", ticktype="detailed", surf=list(x = M$x, y = M$y, z = z, shade=0.1, facets = NA, border = NA, col= "grey"), theta = 0, phi = 0)

#change theta and phi for various angles

model2 <- lm(biomass_2 ~ ratio_1*ratio_2)
png(filename="surface_model2.png")
x = seq(0,60,1)
y = seq(0,60,1)
M = mesh(x,y)
z = model2$coefficients[1] + M$x*model2$coefficients[2] + M$y*model2$coefficients[3] + M$x*M$y*model2$coefficients[4]

scatter3D(ratio_1, ratio_2, biomass_2, xlim=c(0,60), ylim=c(0,60), xlab="TA density [seeds/pot]", ylab="LP density [seeds/pot]", zlab="LP biomass [mg]", main = "LP Biomass - Linear Regression", clab="[mg]", ticktype="detailed", surf=list(x = M$x, y = M$y, z = z, shade=0.1, facets = NA, border = NA, col= "grey"), theta = 0, phi = 0)
#change theta and phi for various angles

#testing------------------
png(filename="testing-conditions.png")

log_ratio_1 = log(ratio_1)
log_ratio_1[log_ratio_1 == -Inf] = 0
log_ratio_2 = log(ratio_2)
log_ratio_2[log_ratio_2 == -Inf] = 0

modeltest <- lm(biomass_2 ~ log_ratio_1*log_ratio_2)
par(mfrow=c(2,2))
plot(modeltest)
summary(modeltest)

biomass_2t = log(biomass_2)
modeltest <- lm(biomass_2t ~ ratio_1*ratio_2)
par(mfrow=c(2,2))
plot(modeltest)
summary(modeltest)

z = model2$coefficients[1] + M$x*model2$coefficients[2] + M$y*model2$coefficients[3] + M$x*M$y*model2$coefficients[4]

png(filename="testing.png")
scatter3D(ratio_1, ratio_2, biomass_2t, xlim=c(0,5), ylim=c(0,5), xlab="TA density [seeds/pot]", ylab="LP density [seeds/pot]", zlab="LP biomass [mg]", main = "LP Biomass - Linear Regression", clab="[mg]", ticktype="detailed", surf=list(x = M$x, y = M$y, z = z, shade=0.1, facets = NA, border = NA, col= "grey"), theta = 0, phi = 0)
#testing------------------

model2a <- lm(biomass_2[ratio_1 < 36]~ ratio_1[ratio_1 < 36]*ratio_2[ratio_1 < 36])
model2b <- lm(biomass_2[ratio_1 >= 36]~ ratio_1[ratio_1 >= 36]*ratio_2[ratio_1 >= 36])
png(filename="surface_model2_updated.png")
xa = seq(0,30,1)
ya = seq(0,60,1)
Ma = mesh(xa,ya)
xb = seq(31,60,1)
yb = seq(0,60,1)
Mb = mesh(xb,yb)
za = model2a$coefficients[1] + Ma$x*model2a$coefficients[2] + Ma$y*model2a$coefficients[3] + Ma$x*Ma$y*model2a$coefficients[4]
zb = model2b$coefficients[1] + Mb$x*model2b$coefficients[2] + Mb$y*model2b$coefficients[3] + Mb$x*Mb$y*model2b$coefficients[4]

z = rbind(za,zb)

sa = list(x = Ma$x, y = Ma$y, z = za, shade=0.1, facets = NA, border = NA, col= "grey")
sb = list(x = Mb$x, y = Mb$y, z = zb, shade=0.1, facets = NA, border = NA, col= "grey")

scatter3D(ratio_1, ratio_2, biomass_2, xlim=c(0,60), ylim=c(0,60), xlab="TA density [seeds/pot]", ylab="LP density [seeds/pot]", zlab="LP biomass [mg]", clab="[mg]", main="LP Biomass - Linear Regressions", ticktype="detailed", surf=list(x = M$x, y = M$y, z = z, shade=0.1, facets = NA, border = NA, col= "grey"), theta = 140, phi = 20)
#change theta and phi for various angles


#trying heatmap of variance
biomass1_var = aggregate(biomass_1 ~ ratio_1 + ratio_2, raw_data, var, na.action=na.exclude)
biomass2_var = aggregate(biomass_2 ~ ratio_1 + ratio_2, raw_data, var, na.action=na.exclude)

biomass1_matrix = xtabs(biomass_1 ~ ratio_1 + ratio_2 , data=biomass1_var)
biomass2_matrix = xtabs(biomass_2 ~ ratio_1 + ratio_2 , data=biomass2_var)

biomass1_matrix[biomass1_matrix == 0 ] = NA
biomass2_matrix[biomass2_matrix == 0 ] = NA

biomass2_matrix = t(biomass2_matrix)

#when only 1 individual vs shit
png(filename="Biomass_var_TA.png")
levelplot(biomass1_matrix, col.regions = new.palette, xlab = "TA [seeds/pot]", ylab = "LP [seeds/pot]", clab = "TA Variance [mg]", main="Variance of Biomass TA", na.color="gray")

png(filename="Biomass_var_LP.png")
levelplot(biomass2_matrix, col.regions = new.palette, xlab = "Seeds/pot - LP", ylab = "Seeds/pot - TA", main="Variance of Biomass LP", na.color="gray")

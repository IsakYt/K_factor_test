library(FrF2)
library(MASS)
library(leaps)

experiment = FrF2(nruns=16, nfactors=4, randomize=FALSE)

#Our responses
y = c(337,226,211,266,313,226,284,430,
       377,251,238,277,253,262, 226, 285)

quantile(y,0.25)

#The variable combinations
A = c(-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1) #morgen/kveld (1,-1)
B = c(-1,1,1,-1,1,-1,1,-1,-1,1,1,-1,1,-1,1,-1) #steep/steeper (1,-1)
C = c(-1,-1,1,1,-1,1,1,-1,-1,-1,1,1,-1,1,1,-1) #asphalt/gravel (1,-1)
D = c(-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1) #warmup/coldstart (1,-1)
df = data.frame(y,A,B,C,D)

#Linear regression using all combinations of variables
lm4 = lm(y~.^4, data=df)
summary(lm4)

##main effect plot
MEPlot(lm4)
IAPlot(lm4)

#Now we must analyze the estimators, using Lenth's method from Tyssedal
effects = 2*lm4$coefficients[-1] # omit intercept
abseffects=abs(effects)
medabseffects = median(abseffects)

tauest = 1.5*medabseffects
keepeffects = abseffects[abseffects < 2.5*tauest]
taustar = 1.5*median(keepeffects)

alpha=0.05 # significance level
cutoff=qt(1-alpha/2,length(abseffects)/3)*taustar
cutoff

# Pareto-type plot for the effects (Lenth plot)
oldmar = par()$marq
par(xpd = FALSE)
barplot(sort(abseffects,decreasing=FALSE), las=1, horiz=TRUE,xlim = c(0,80))
abline(v=cutoff, col=2, lwd=2)

#reducing the model to relevant variables
redMod = lm(y ~ B + C + A*D + B*C, data = df)
summary(redMod)

print(redMod$fitted.values)
print(resid(redMod))

#study the residuals for the reduced model
residuals = rstudent(redMod)
residuals = resid(redMod)
avvik = sd(residuals)

mean(residuals)
median(residuals)
plot(redMod$fitted.values, residuals)
abline(h=c(0,avvik,-avvik,-2*avvik,-3*avvik), col = c(2,4,4,4,4), lwd = 2)

#checking normal distribution of residuals
residuals = rstudent(redMod)
qqnorm(residuals)
qqline(residuals, col = "blue")

check4 = regsubsets(y~.^4,data=df)
bob = summary(check4)
names(bob)
which.max(bob$adjr2)


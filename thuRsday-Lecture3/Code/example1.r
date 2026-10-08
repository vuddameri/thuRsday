library(vioplot)

# Set the path
path <- '/home/vuddameri/MathWorkshop/thuRsday-Lecture3/Data/'
setwd(path)
# Read in the file
fname <- "SanAntonioACS.csv"
a <- read.csv(fname)
head(a)
nrow(a)  # Total number of rows
# Cleanup the data; remove rows with missing values
a <- na.omit(a)
nrow(a) # Number of rows retained

# compute Education Attainment
a["EduAttain"] = (a$Bachelors+a$Masters+a$Doctorate+a$Professional)*100/
                  a$Education25Plus 

# Make a plot using base R
par(mgp=c(2,1,0),cex=0.8)
plot(a$MedIncome~a$EduAttain,
     pch=20, col='blue',
     xlab='Education Attainment (%)',
     ylab='Median Houshold Income ($)',
     main='Median Household Income vs Educational Attainment (%)')
grid()

# Fit a continuous segmented regression
library(segmented)
model <- lm(MedIncome ~ EduAttain, data = a)
segmodel <- segmented(model,
                      seg.Z = ~ EduAttain,
                      psi = 55)
summary(segmodel)
# Extract coefficients and breakpoint
b <- coef(segmodel)
bp <- segmodel$psi[1, "Est."]

# Construct the two regression equations
eq1 <- sprintf("X <= %.1f: Y = %.0f + %.1f X",
               bp, b[1], b[2])

eq2 <- sprintf("X > %.1f: Y = %.0f + %.1f X + %.1f(X - %.1f)",
               bp, b[1], b[2], b[3], bp)

# Plot the segmented model
plot(segmodel, add = TRUE, col = "red", lwd = 3,lty=2)
# Add equations to the existing graph
legend("topleft",
       legend = c(eq1,eq2),
       bty = "n", cex = 0.9)
#################################################################
#svg('hist.svg',width=5,height=5)
# Make a histogram of median income
hist(a$MedIncome,breaks='scott',
     xlab='Median Income',
     ylab='Frequency', col='tomato4',
     main='')
rug(a$MedIncome)
box()
#dev.off

# List all named colors available in R
colors()

# Plotting symbols
# Display the 26 standard plotting symbols
plot(0:25, rep(1, 26),
     pch = 0:25,
     col = "blue",
     cex = 2,
     xlim = c(-1, 26),
     ylim = c(0.5, 1.5),
     axes = FALSE,
     xlab = "", ylab = "")

text(0:25, rep(0.8, 26), labels = 0:25)

# Arrange a 2 x 2 plot matrix
par(mfrow=c(2,2))
plot(a$Poverty,a$MedAge,pch=1)
plot(a$Poverty,a$EduAttain,pch=3)
plot(a$Poverty,a$MedHomePrice,pch=10,col='blue')
hist(a$MedAge,main="")
box()

# Make a violinplot
par(mfrow=c(1,1))
vioplot::vioplot(a$MedIncome)




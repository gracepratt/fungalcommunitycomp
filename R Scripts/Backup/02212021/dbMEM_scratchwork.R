## dbMEM

source('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/plot.links.R')
source('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/scalog.R')
source('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/quickMEM.R')
source('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/sr.value.R')


# Transform the data
amf.h <- decostand (amf_otu, "hellinger")
amf.xy <- geo
amf.xy.c <- scale(amf.xy, center = TRUE, scale = FALSE)
amf.env <- df[envi_factors]

## Univariate spatial correlogram (based on Moran's I)
# Search for neighbours of all points within a radius of 0.7 m
# and multiples (i.e., 0 to 0.7 m, 0.7 to 1.4 m and so on).
plot.links(geo, thresh = 0.7)
nb1 <- dnearneigh(as.matrix(geo), 0, 0.7)
summary(nb1)


# The species data are first detrended; see Sect. 7.3
amf.h.det <- resid(lm(as.matrix(amf.h) ~ ., data = amf.xy))

## Step 1. Construct the matrix of dbMEM variables
amf.dbmem.tmp <- dbmem(amf.xy, silent = FALSE)
amf.dbmem <- as.data.frame(amf.dbmem.tmp)

# Truncation distance used above:
(thr <- give.thresh(dist(amf.xy)))

# Display and count the eigenvalues
attributes(amf.dbmem.tmp)$values
length(attributes(amf.dbmem.tmp)$values)


## Step 2. Run the global dbMEM analysis on the detrended
## Hellinger-transformed amf data
(amf.dbmem.rda <- rda(amf.h.det ~., amf.dbmem))
anova(amf.dbmem.rda)


## Step 3. Since the R-square is significant, compute the adjusted
## R2 and run a forward selection of the dbmem variables
(amf.R2a <- RsquareAdj(amf.dbmem.rda)$adj.r.squared)
(amf.dbmem.fwd <- forward.sel(amf.h.det, as.matrix(amf.dbmem),
                               adjR2thresh = amf.R2a))
(nb.sig.dbmem <- nrow(amf.dbmem.fwd)) # Number of signif. dbMEM
# Identity of the significant dbMEM in increasing order
(dbmem.sign <- sort(amf.dbmem.fwd[ ,2]))
# Write the significant dbMEM to a new object
dbmem.red <- amf.dbmem[ ,c(dbmem.sign)]


## Step 4. New dbMEM analysis with 5 significant dbMEM variables
## Adjusted R-square after forward selection: R2adj = 0.1206
(amf.dbmem.rda2 <- rda(amf.h.det ~ ., data = dbmem.red))
(amf.fwd.R2a <- RsquareAdj(amf.dbmem.rda2)$adj.r.squared)
anova(amf.dbmem.rda2)
(axes.test <- anova(amf.dbmem.rda2, by = "axis"))
# Number of significant axes
(nb.ax <- length(which(axes.test[ , ncol(axes.test)] <= 0.05)))


## Step 5. Plot the significant canonical axes
amf.rda2.axes <-
  scores(amf.dbmem.rda2,
         choices = c(1:nb.ax),
         display = "lc",
         scaling = 1)
par(mfrow = c(1,nb.ax))
for(i in 1:nb.ax){
  sr.value(amf.xy, amf.rda2.axes[ ,i],
           sub = paste("RDA",i),
           csub = 2)
}




# Interpreting the spatial variation: regression of the significant
# canonical axes on the environmental variables, with Shapiro-Wilk
# normality tests of residuals
amf.rda2.axis1.env <- lm(amf.rda2.axes[ ,1] ~ ., data = amf.env)
shapiro.test(resid(amf.rda2.axis1.env))
summary(amf.rda2.axis1.env)
amf.rda2.axis2.env <- lm(amf.rda2.axes[ ,2] ~ ., data = amf.env)
shapiro.test(resid(amf.rda2.axis2.env))
summary(amf.rda2.axis2.env)
amf.rda2.axis3.env <- lm(amf.rda2.axes[ ,3] ~ ., data = amf.env)
shapiro.test(resid(amf.rda2.axis3.env))
summary(amf.rda2.axis3.env)
amf.rda2.axis4.env <- lm(amf.rda2.axes[ ,4] ~ ., data = amf.env)
shapiro.test(resid(amf.rda2.axis4.env))
summary(amf.rda2.axis4.env)



# Scalogram of the variance explained by all dbMEM eigenfunctions,
# computed with our homemade function scalog()
par(mfrow = c(1,1))
scalog(amf.dbmem.rda)


sr.value(amf.xy,
         dbmem.red[ ,1],
         sub = paste("dbMEM", dbmem.sign[1]),
         csub = 2)

s.value(amf.xy, dbmem.red[ ,2])

testDF <- as.data.frame(cbind(MEM = dbmem.red$MEM2,Long_point=amf.xy$Long_point, Lat_point=amf.xy$Lat_point ))
memDF <- testDF %>% 
  mutate(MEM.abs = abs(MEM),
         direction = case_when(MEM > 0 ~ "+",
                               MEM <0 ~ "-"))
head(memDF)


ggplot(memDF, mapping=aes(x=Long_point, y=Lat_point, size=MEM.abs, shape=direction)) + 
  geom_jitter(width=0.01, height=0.01) +
  scale_shape_manual(values=c(16,21))




## dbMEM analysis of the mite data - broad scale
(amf.dbmem.broad <- rda(amf.h.det ~ ., data=amf.dbmem[,c(2,3)]) )
anova(amf.dbmem.broad)
(axes.broad <- anova(amf.dbmem.broad, by = "axis"))
# Number of significant axes
(nb.ax.broad <-
    length(which(axes.broad[ , ncol(axes.broad)] <= 0.05)))
# Plot of the two significant canonical axes
amf.dbmembroad.axes <-
  scores(amf.dbmem.broad,
         choices = c(1,2),
         display = "lc",
         scaling = 1)
par(mfrow = c(1, 2))
sr.value(amf.xy, amf.dbmembroad.axes[ ,1])
sr.value(amf.xy, amf.dbmembroad.axes[ ,2])
# Interpreting the broad-scaled spatial variation: regression of
# the two significant spatial canonical axes on the environmental
# variables
amf.dbmembroad.ax1.env <-
  lm(amf.dbmembroad.axes[ ,1] ~ ., data = amf.env)
summary(amf.dbmembroad.ax1.env)
amf.dbmembroad.ax2.env <-
  lm(amf.dbmembroad.axes[ ,2] ~ ., data = amf.env)
summary(amf.dbmembroad.ax2.env)

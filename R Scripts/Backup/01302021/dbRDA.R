
df <- all_wa$df

df <- df %>% mutate(Key = as.integer(Key)) 
row.names(df) <- df$Key
amf_otu <- df%>% dplyr::select(contains("OTU"))
dist <- vegdist(decostand(amf_otu,"hellinger"), "bray")

geo <- df %>% dplyr::select("Long_point", "Lat_point")
dist.geo <- as.dist(distm(geo, fun = distHaversine) )
geo.pcnm <- pcnm(dist.geo, threshold=60)
# geo.pcnm <- pcnm(dist.geo)
pcnmDF <- as.data.frame(geo.pcnm$vectors)
# ordisurf(geo, scores(geo.pcnm, choi=1), bubble = 4, main = "PCNM 1")
# ordisplom(geo.pcnm, choices=1:4)
dbmem.tmp <- dbmem(dist(geo), silent = FALSE)
dbmemDF <- as.data.frame(amf.dbmem.tmp)

amf.env <- df[c("pH","P","N")]
cd <- df[c("FarmBi")]

vp <- varpart(dist, dbmemDF,amf.env,cd )

plot(vp)

mrmTest <- MRM(dist~  dist(scale(FarmBi)) + dist(scale(pH)) + dist(scale(N)) + dist(scale(P)) + dist(scale(TOC)) + dist(dist.geo), data=df, nperm=999)
round(mrmTest$coef,3)
calc.relimp(mrmTest, type = c("lmg"), rela = TRUE) 


lmTest <- glm(observed ~ FarmBi + scale(pH) + scale(N) + scale(P) + scale(TOC) + scale(pcnmDF$PCNM1), data=df)
summary(lmTest)
calc.relimp(lmTest, type = c("lmg"), rela = TRUE)@lmg


lmTest <- glm.nb(observed ~ scale(pH)*FarmType + scale(N)*FarmType + scale(P)*FarmType + scale(TOC)*FarmType, data=all_wa$df)
summary(lmTest)
calc.relimp(lmTest, type = c("lmg"), rela = TRUE)@lmg

lmTest <- glmer.nb(observed ~ scale(pH)*FarmType + scale(N)*FarmType + scale(P)*FarmType + scale(TOC)*FarmType + (1|FarmKey:Year), data=all_wa$df)
summary(lmTest)
calc.relip.mm(lmTest)


lmTest <- glmer.nb(observed ~ scale(pH) + scale(N) + scale(P) + scale(TOC) + FarmType + (1|FarmKey:Year), data=all_wa$df)
summary(lmTest)
calc.relip.mm(lmTest)

lmTest <- lm(shannon ~ scale(pH) + scale(N) + scale(P) + scale(TOC) + FarmBi, data=all_wa$df)
summary(lmTest)
calc.relimp(lmTest)



lmTest01 <- lmer(observed ~ FarmType + scale(pH) + scale(N) + scale(P) + scale(TOC) + scale(pcnmDF$PCNM1) + (1|farmCode), data=df)
summary(lmTest01)
lmTest02 <- lmer(observed ~ scale(pH) + scale(N) + scale(P) + scale(TOC) + scale(pcnmDF$PCNM1) + (1|farmCode), data=df)
summary(lmTest02)



ggplot(mapping=aes(x=df$pH, y = df$observed, color = df$FarmType)) + geom_point() 

ggplot(mapping=aes(x=df$Long_point, y = df$Lat_point, size = df$observed, color = df$observed, shape=df$FarmType)) + geom_jitter(width = 0.015, height = 0.015) 

# For defining permutations based on blocking design: https://fromthebottomoftheheap.net/2014/11/03/randomized-complete-block-designs-and-vegan/
dbrda.test <- capscale(amf_otu ~ FarmBi +  N+ P+ pH+ TOC+ pcnmDF$PCNM1 , data=df, distance = "bray", sqrt.dist = TRUE)

# ggord(dbrda.test, df_w$df$FarmType,ext=1.2, vec_ext=0.4) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
# looking at the raw code, this is plotting the 'wa scores', the blue dots are different species

# dataframe
site_scores <- scores(dbrda.test, choices = c(1,2), display = "sites")
dbRDA_df <- as.data.frame(cbind(site_scores, Key = df$Key)) %>%
  left_join(df, by = "Key")

ggplot(dbRDA_df, aes(x=CAP1, y=CAP2, color=FarmBi)) +
  geom_point() +
  # scale_shape_manual(values = c(16,1))+
  themeBorder


h <- how(blocks = df$FarmKey, nperm = 999)

anova.cca(dbrda.test, by="terms", permutations = h)

RsquareAdj(dbrda.test)

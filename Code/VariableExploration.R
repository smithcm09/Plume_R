#PlumeDF.RData
attach(PlumeDerivedStatistics)
# load car, leaps, effects, lattic packages
#scatterplotMatrixs ---- 

#All the variables
scatterplotMatrix(~PV+MW+PF+CumVol+NpT+NpR+PPH+IPV+NLS+ltgAll+VASR_AB+OverallABmedInfra+OverallABmedSeis+maxseisDur+maxinfraDur+
                    CRF_factor+Factor_ltgAll+start_ElectricActivity)

#use only one Electric variable vs just the plume variables first
#NLS
scatterplotMatrix(~NLS+PV+MW+PF+CumVol+PPH+IPV, transform=TRUE)
#ltgAll
scatterplotMatrix(~ltgAll+PV+MW+PF+CumVol+PPH+IPV)
scatterplotMatrix(~sqrt(ltgAll+0.1)+log10(PV)+log10(MW)+log10(PF)+log10(CumVol)+PPH+log10(IPV))
#start_Electrical Duration
scatterplotMatrix(~start_ElectricActivity+PV+MW+PF+CumVol+PPH+IPV, transform=TRUE)

#Factor CRF
scatterplotMatrix(~CRF_factor+PV+MW+PF+CumVol+PPH+IPV)
scatterplotMatrix(~CRF_factor+log10(PV)+log10(MW)+log10(PF)+log10(CumVol)+PPH+log10(IPV))
#Factor Lightning
scatterplotMatrix(~Factor_ltgAll+PV+MW+PF+CumVol+PPH+IPV)


#Determine best transformations for data ----
#PV (Peak Velocity)----
summary(PV)
plot(PV)
hist(PV, breaks = 10)
boxplot(PV)
symbox(PV)
powerTransform(PV)
#factored boxplot
Boxplot(log10(PV)~CRF_factor, notch=TRUE, id.n=3)
Boxplot(log10(PV)~Factor_ltgAll, notch=TRUE, id.n=3)

Boxplot((PV^-0.5)~CRF_factor, notch=TRUE, id.n=3)
Boxplot((PV^-0.5)~Factor_ltgAll, notch=TRUE, id.n=3)

Boxplot((PV)~CRF_factor, notch=TRUE, id.n=3)
Boxplot((PV)~Factor_ltgAll, notch=TRUE, id.n=3)

histogram(PV^-0.5, breaks = 10)
histogram(log10(PV), breaks = 10)

qqPlot(PV)
qqPlot(log10(PV))
qqPlot((PV^-0.5))

t.test(PV^-0.5~CRF_factor)
t.test(PV^-0.5~Factor_ltgAll)

symbox(PV)

#MW (Max Width)----
summary(MW)
plot(MW)
hist(MW, breaks = 10)
boxplot(MW)
symbox(MW)
powerTransform(MW)
#factored boxplot
Boxplot(log10(MW)~CRF_factor, notch=TRUE, id.n=3)
Boxplot(log10(MW)~Factor_ltgAll, notch=TRUE, id.n=3)

qqPlot(MW)
qqPlot(log10(MW))

t.test(log10(MW)~CRF_factor)
t.test(log10(MW)~Factor_ltgAll)

#PF (Peak Flux)----
summary(PF)
plot(PF)
hist(PF, breaks = 10)
boxplot(PF)
symbox(PF)
powerTransform(PF)
#factored boxplot
Boxplot(log10(PF)~CRF_factor, notch=TRUE, id.n=3)
Boxplot(log10(PF)~Factor_ltgAll, notch=TRUE, id.n=3)

histogram(log10(PF), breaks = 10)

qqPlot(PF)
qqPlot(log10(PF))

t.test(log10(PF)~CRF_factor)
t.test(log10(PF)~Factor_ltgAll)

#CumVol (Cumulative Volume)----
summary(CumVol)
plot(CumVol)
hist(CumVol, breaks = 10)
boxplot(CumVol)
symbox(CumVol)
powerTransform(CumVol)
#factored boxplot
Boxplot(log10(CumVol)~CRF_factor, notch=TRUE, id.n=3)
Boxplot(log10(CumVol)~Factor_ltgAll, notch=TRUE, id.n=3)

histogram(log10(CumVol), breaks = 10)

qqPlot(CumVol)
qqPlot(log10(CumVol))

t.test(log10(CumVol)~CRF_factor)
t.test(log10(CumVol)~Factor_ltgAll)

#PPH (Peak Plume Height)----
summary(PPH)
plot(PPH)
hist(PPH, breaks = 10)
boxplot(PPH)
symbox(PPH)
powerTransform(PPH)
#factored boxplot
Boxplot(PPH~CRF_factor, notch=TRUE, id.n=3)
Boxplot(PPH~Factor_ltgAll, notch=TRUE, id.n=3)

histogram(PPH, breaks = 10)

qqPlot(PPH)
#PPH_altered (Peak Plume Height)----
summary(PPH_altered)
plot(PPH_altered)
hist(PPH_altered, breaks = 10)
boxplot(PPH_altered)
symbox(PPH_altered)
powerTransform(PPH_altered)
#factored boxplot
Boxplot(PPH_altered~CRF_factor, notch=TRUE, id.n=3)
Boxplot(PPH_altered~Factor_ltgAll, notch=TRUE, id.n=3)

histogram(PPH_altered, breaks = 10)

qqPlot(PPH_altered)

t.test(PPH_altered~CRF_factor)
t.test(PPH_altered~Factor_ltgAll)
# PPH vs PPH_altered ----
plot(PPH, PPH_altered)
plot(log10(NLS),PPH)
plot(log10(NLS),PPH_altered)

#IPV (Initial Plume Velocity)----
summary(IPV)
plot(IPV)
hist(IPV, breaks = 10)
boxplot(IPV)
symbox(IPV)
powerTransform(IPV)
#factored boxplot
Boxplot(log10(IPV)~CRF_factor, notch=TRUE, id.n=3)
Boxplot(log10(IPV)~Factor_ltgAll, notch=TRUE, id.n=3)

histogram(log10(IPV), breaks = 10)

qqPlot(IPV)
qqPlot(log10(IPV))

t.test(log10(IPV)~CRF_factor)
t.test(log10(IPV)~Factor_ltgAll)


#NLS ----
symbox(NLS)
powerTransform(NLS)
#for 1 x 1 plots put NLS into log10(NLS)

#start_ElectricActivity ----
symbox(start_ElectricActivity)
powerTransform(start_ElectricActivity)
#for 1 x 1 plots put sEA into log10(sEA)
#ltgAll----
symbox(ltgAll+0.1)
powerTransform(ltgAll+0.1)
#for 1 x 1 plots put ltgAll into log10(ltgAll+0.1)

#Creation of new variables based on determined transformations ----
neg05_PV <- (PV^(-0.5))
L_MW <- log10(MW)
L_PF <- log10(PF)
L_CumVol <- log10(CumVol)
#no transform for PPH or PPH_altered
L_IPV <- log10(IPV)



#preliminary 1x1 linear models----
#NLS vs PV
singleVar1 <- lm(log10(NLS)~neg05_PV)
summary(singleVar1)
scatterplot(neg05_PV,log10(NLS))
plot(singleVar1)

#NLS vs CumVol
singleVar2 <- lm(log10(NLS)~L_CumVol)
summary(singleVar2)
scatterplot(L_CumVol,log10(NLS))
plot(singleVar2)

#NLS vs PPH
singleVar3 <- lm(log10(NLS)~PPH)
summary(singleVar3)
scatterplot(PPH,log10(NLS))
plot(singleVar3)

#NLS vs PPH_altered
singleVar3.1 <- lm(log10(NLS)~PPH_altered)
summary(singleVar3.1)
scatterplot(PPH_altered,log10(NLS))
plot(singleVar3.1)

#NLS vs IPV
singleVar20 <- lm(log10(NLS)~L_IPV)
summary(singleVar20)
scatterplot(L_IPV,log10(NLS))
plot(singleVar20)

#NLS vs PF
singleVar21 <- lm(log10(NLS)~log10(PF))
summary(singleVar21)
scatterplot(log10(PF),log10(NLS))#,ellipse=TRUE) #ellipse shows 0.5 and 0.95 concentrations of data
plot(singleVar21)


#start_ElectricActivity vs PV
singleVar4 <- lm(log10(start_ElectricActivity)~neg05_PV)
summary(singleVar4)
scatterplot(neg05_PV,log10(start_ElectricActivity))
plot(singleVar4)

#start_ElectricActivity vs CumVol
singleVar5 <- lm(log10(start_ElectricActivity)~L_CumVol)
summary(singleVar5)
scatterplot(L_CumVol,log10(start_ElectricActivity))
plot(singleVar5)

#start_ElectricActivity vs PPH
singleVar6 <- lm(log10(start_ElectricActivity)~PPH)
summary(singleVar6)
scatterplot(PPH,log10(start_ElectricActivity))
plot(singleVar6)

#start_ElectricActivity vs PPH_altered
singleVar6.1 <- lm(log10(start_ElectricActivity)~PPH_altered)
summary(singleVar6.1)
scatterplot(PPH_altered,log10(start_ElectricActivity))
plot(singleVar6.1)

#start_ElectricActivity vs IPV
singleVar22 <- lm(log10(start_ElectricActivity)~L_IPV)
summary(singleVar22)
scatterplot(L_IPV,log10(start_ElectricActivity))
plot(singleVar22)

#start_ElectricActivity vs PF
singleVar23 <- lm(log10(start_ElectricActivity)~L_PF)
summary(singleVar23)
scatterplot(L_PF,log10(start_ElectricActivity))
plot(singleVar23)

#ltgAll+0.1 vs PV
singleVar7 <- lm(log10(ltgAll+0.1)~neg05_PV)
summary(singleVar7)
scatterplot(neg05_PV,log10(ltgAll+0.1))
plot(singleVar7)

#ltgAll+0.1 vs CumVol
singleVar8 <- lm(log10(ltgAll+0.1)~L_CumVol)
summary(singleVar8)
scatterplot(L_CumVol,log10(ltgAll+0.1))
plot(singleVar8)

#ltgAll+0.1 vs PPH
singleVar9 <- lm(log10(ltgAll+0.1)~PPH)
summary(singleVar9)
scatterplot(PPH,log10(ltgAll+0.1))
plot(singleVar9)

#ltgAll+0.1 vs PPH_altered
singleVar9.1 <- lm(log10(ltgAll+0.1)~PPH_altered)
summary(singleVar9.1)
scatterplot(PPH_altered,log10(ltgAll+0.1))
plot(singleVar9.1)

#ltgAll+0.1 vs IPV
singleVar24 <- lm(log10(ltgAll+0.1)~L_IPV)
summary(singleVar24)
scatterplot(L_IPV,log10(ltgAll+0.1))
plot(singleVar24)

#ltgAll+0.1 vs PF
singleVar25 <- lm(log10(ltgAll+0.1)~L_PF)
summary(singleVar25)
scatterplot(L_PF,log10(ltgAll+0.1))
plot(singleVar25)

#preliminary 1x1 logistic models ----
#CRF
#**
singleVar10 <- glm(CRF_factor~neg05_PV, family = binomial(link = "logit"))
summary(singleVar10)
scatterplot(neg05_PV,CRF_factor)
plot(allEffects(singleVar10))
#pseduo R2 = 1-(residul/null)
#0.079

#***
singleVar11 <- glm(CRF_factor~log10(CumVol), family = binomial(link = "logit"))
summary(singleVar11)
scatterplot(L_CumVol,CRF_factor)
plot(allEffects(singleVar11))
#pseduo = 0.133

singleVar12 <- glm(CRF_factor~PPH, family = binomial(link = "logit"))
summary(singleVar12)
scatterplot(PPH,CRF_factor)
plot(allEffects(singleVar12))
#pseduo = 0.036

#*
singleVar12.1 <- glm(CRF_factor~PPH_altered, family = binomial(link = "logit"))
summary(singleVar12.1)
scatterplot(PPH_altered,CRF_factor)
plot(allEffects(singleVar12.1))
#pseduo = 1-(102.79/108.54) = 0.05297586

#***
singleVar18 <- glm(CRF_factor~log10(PF), family = binomial(link = "logit"))
summary(singleVar18)
scatterplot(log10(PF),CRF_factor)
plot(allEffects(singleVar18))
#pseduo = 0.144

#**
singleVar19 <- glm(CRF_factor~log10(IPV), family = binomial(link = "logit"))
summary(singleVar19)
scatterplot(log10(IPV),CRF_factor)
plot(allEffects(singleVar19))
#pseduo = 0.134

#LTG
#nothing
singleVar13 <- glm(Factor_ltgAll~neg05_PV, family = binomial(link = "logit"))
summary(singleVar13)
scatterplot(neg05_PV,Factor_ltgAll)
plot(allEffects(singleVar13))
#pseduo R2 = 1-(residul/null)
# 0.032

#*
singleVar14 <- glm(Factor_ltgAll~log10(CumVol), family = binomial(link = "logit"))
summary(singleVar14)
scatterplot(L_CumVol,Factor_ltgAll)
plot(allEffects(singleVar14))
#pseduo = 0.057

singleVar15 <- glm(Factor_ltgAll~PPH, family = binomial(link = "logit"))
summary(singleVar15)
scatterplot(PPH,Factor_ltgAll)
plot(allEffects(singleVar15))
#pseduo = 0.005

#nothing
singleVar15.1 <- glm(Factor_ltgAll~PPH_altered, family = binomial(link = "logit"))
summary(singleVar15.1)
scatterplot(PPH_altered,Factor_ltgAll)
plot(allEffects(singleVar15.1))
#pseduo = 0.0038

#*
singleVar16 <- glm(Factor_ltgAll~log10(PF), family = binomial(link = "logit"))
summary(singleVar16)
scatterplot(log10(PF),Factor_ltgAll)
plot(allEffects(singleVar16))
#pseduo = 0.06

#*
singleVar17 <- glm(Factor_ltgAll~log10(IPV), family = binomial(link = "logit"))
summary(singleVar17)
scatterplot(log10(IPV),Factor_ltgAll)
plot(allEffects(singleVar17))
#pseduo = 0.0567
plot(singleVar17)

#preliminary multiple linear models ----
#NLS
Multi_NLS1 <- lm((NLS)~neg05_PV+L_CumVol+PPH_altered+L_IPV+L_PF)
summary(Multi_NLS1)
plot(allEffects(Multi_NLS1))

NLSnothing_plumeonly <- lm(NLS~1)
NLSfullmod_plumeonly <- lm(NLS~neg05_PV+L_CumVol+PPH_altered+L_IPV+L_PF)
NLS_bothways_raw_plumeonly = step(NLSnothing_plumeonly, list(lower=formula(NLSnothing_plumeonly),upper=formula(NLSfullmod_plumeonly)), direction="both")

summary(NLS_bothways_raw_plumeonly)
plot(allEffects(NLS_bothways_raw_plumeonly))
#same as NLS_bothways_raw_plumeonly
PlumeOnlyMod <- lm(NLS~log10(PF)+log10(IPV))
summary(PlumeOnlyMod)
plot(allEffects((PlumeOnlyMod)))

#transform response

symbox(NLS)
powerTransform(NLS)

PlumeOnlyMod_transResp <- lm(log10(NLS)~log10(PF)+log10(IPV))
summary(PlumeOnlyMod_transResp)
plot(allEffects((PlumeOnlyMod_transResp)))


tRNLSnothing_plumeonly <- lm(log10(NLS)~1)
tRNLSfullmod_plumeonly <- lm(log10(NLS)~neg05_PV+L_CumVol+PPH_altered+L_IPV+L_PF)
tRNLS_bothways_raw_plumeonly = step(tRNLSnothing_plumeonly, list(lower=formula(tRNLSnothing_plumeonly),upper=formula(tRNLSfullmod_plumeonly)), direction="both")

tRNLS_bothways_raw_plumeonly_realvar <- lm(log10(NLS)~log10(PF)+log10(IPV)+PPH_altered)
summary(tRNLS_bothways_raw_plumeonly_realvar)
plot(allEffects(tRNLS_bothways_raw_plumeonly_realvar))
plot(allEffects(tRNLS_bothways_raw_plumeonly))

m.Plume <- tRNLS_bothways_raw_plumeonly

summary(m.Plume)

#test model
outlierTest(m.Plume)
dfbetasPlots(m.Plume)
influenceIndexPlot(m.Plume, id.n=3)
influencePlot(m.Plume, id.n=3) #one I like
residualPlots(m.Plume)
vif(m.Plume)
avPlots(m.Plume)
qqPlot(m.Plume)
spreadLevelPlot(m.Plume)
crPlots(m.Plume)
boxCox(m.Plume)
inverseResponsePlot(m.Plume)
Anova(m.Plume)

#Check interactions
Anova(lm(log10(NLS)~log10(PF)*log10(IPV)*PPH_altered))
# no significant interactions #ThankGod

#adding in the seimic/infrasound variables from that NLS model----
L_SE <- log10(OverallABmedSeis)
L_IE <- log10(OverallABmedInfra)
Sq_SD <- sqrt(maxseisDur)

#no interactions
Multi_NLS2 <- lm((NLS)~neg05_PV+L_CumVol+PPH_altered+L_IPV+L_PF+L_IE+L_SE+Sq_SD)
summary(Multi_NLS2)
plot(allEffects(Multi_NLS2))

#seis paper models interaction only
Multi_NLS3 <- lm((NLS)~neg05_PV+L_CumVol+PPH_altered+L_IPV+L_PF+L_IE:L_SE+Sq_SD)
summary(Multi_NLS3)
plot(allEffects(Multi_NLS3))

#seis paper model interaction plus parts
Multi_NLS4 <- lm(NLS~neg05_PV+L_CumVol+PPH_altered+L_IPV+L_PF+L_IE*L_SE+Sq_SD)
summary(Multi_NLS4)
plot(allEffects(Multi_NLS4))

#seis paper model interaction plus parts plus y-transform
Multi_NLS5 <- lm(log10(NLS)~neg05_PV+L_CumVol+PPH_altered+L_IPV+L_PF+L_IE*L_SE+Sq_SD)
summary(Multi_NLS5)
plot(allEffects(Multi_NLS5))
#R = 57%

#seis paper model plus only sig Plume parts
Multi_NLS6 <- lm(log10(NLS)~PPH_altered+L_IPV+L_PF+L_IE+L_SE+Sq_SD)
summary(Multi_NLS6)
plot(allEffects(Multi_NLS6))
#R = 54.5%

#seis paper model plus only sig Plume parts
Multi_NLS7 <- lm(log10(NLS)~PPH_altered+L_IPV+L_PF+L_IE*L_SE+Sq_SD)
summary(Multi_NLS7)
plot(allEffects(Multi_NLS7))
#R = 56.5%

#Anova - checking all possible interactions
Anova(lm(log10(NLS)~L_IE*L_SE*Sq_SD*PPH_altered*L_PF*L_IPV))

#Stepwise regression
NLSnothing <- lm(log10(NLS)~1)
NLSfullmod <- lm(log10(NLS)~PPH_altered+L_IPV+L_PF+L_IE+L_SE+Sq_SD)
NLS_bothways_raw = step(NLSnothing, list(lower=formula(NLSnothing),upper=formula(NLSfullmod)), direction="both")
#summary(NLSfullmod)

# NLS_bothways <- lm(formula = log10(NLS) ~ L_IE + Sq_SD + neg05_PV + L_IPV)
# NLS_bothways <- lm(formula = log10(NLS) ~ log10(OverallABmedInfra) + sqrt(maxseisDur) + neg05_PV + log10(IPV))
# summary(NLS_bothways)
summary(NLS_bothways_raw)
plot(allEffects(NLS_bothways_raw))

Anova(lm(log10(NLS) ~ L_IE * L_IPV * L_PF * PPH_altered * Sq_SD))


#plot just one of many effect plots ----

#what I want but with PV untransformed
NLS_bothways <- lm(formula = log10(NLS) ~ log10(OverallABmedInfra) + sqrt(maxseisDur) + neg05_PV + log10(IPV))
summary(NLS_bothways)
plot(allEffects(NLS_bothways))

#trying to do that
NLS_bothways1 <- lm(formula = log10(NLS) ~ log10(OverallABmedInfra) + sqrt(maxseisDur) + I(sqrt(PV)^-1) + log10(IPV))
summary(NLS_bothways1)
plot(allEffects(NLS_bothways1))

#make object of all effect plots
effNLS <- allEffects(NLS_bothways1)
#plot just the one interested in
plot(effNLS,(I(sqrt(PV)^-1)))
#transform x axis
plot(effNLS,'neg05_PV', transformation=list(link=function(x) (x^(-0.5)) ,inverse=function(x) (1/x)^2))




#choosen model thus far = Chosen_Model_NLS----
Chosen_Model_NLS <- lm(log10(NLS) ~ log10(OverallABmedInfra) + sqrt(maxseisDur) + log10(PF) + log10(IPV) + PPH_altered)
plot(allEffects(Chosen_Model_NLS))
summary(Chosen_Model_NLS)

Anova(Chosen_Model_NLS)

#Now do Outlier Detection and Model Validation
#Outlier Detection
outlierTest(Chosen_Model_NLS)
influencePlot(Chosen_Model_NLS)

#Check Collinearity
vif(Chosen_Model_NLS)
#no vif problems :)

#normality
qqPlot(Chosen_Model_NLS)
plot(density(rstudent(Chosen_Model_NLS)),xlim=c(-3,3))
curve(dt(x,97), -3,3, col= 3, add=T)
#very close to normal :)

crPlots(Chosen_Model_NLS) 
ceresPlots(Chosen_Model_NLS)
#no curves so linear assumption holds

residualPlots(Chosen_Model_NLS,type="rstudent")
#Tukey test is non-sig so model is a good fit

summary(covratio(Chosen_Model_NLS))
avPlots(Chosen_Model_NLS)
dfbetaPlots(Chosen_Model_NLS)
influenceIndexPlot(Chosen_Model_NLS)
marginalModelPlots(Chosen_Model_NLS)
ncvTest(Chosen_Model_NLS)

#chosen model passes all tests 
summary(Chosen_Model_NLS)
plot(allEffects(Chosen_Model_NLS))

# Final NLS Plot Choices ----
#Plume characteristics only
summary(m.Plume)
plot(allEffects(m.Plume), ylim=c(0,3))

#Plume and Seismic/Infrasound Characteristics
summary(Chosen_Model_NLS)
plot(allEffects(Chosen_Model_NLS))

Chosen_Model_NLS <- lm(log10(NLS) ~ log10(OverallABmedInfra) + sqrt(maxseisDur) + log10(PF) + log10(IPV) + PPH_altered)
Chosen_Model_NLS <- lm(log10(NLS) ~ L_IE + sqrt(maxseisDur) + L_PF + L_IPV + PPH_altered)
plot(allEffects(Chosen_Model_NLS), ylim=c(0,3))

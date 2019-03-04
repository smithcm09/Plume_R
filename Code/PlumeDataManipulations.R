#Determining High Medium Low sets for plume/infra/ltg values

attach(PlumeDerivedStatistics)
V <- maxinfraDur
summary(V)
boxplot(V)
histogram(V,breaks = 50)

#Subsetting
test <- PlumeDerivedStatistics
#Subsetting values are determined by the median and 3rd Quantile values of the summary statistics of the variable in question
#all subsets number of observations for a specific variable must equal 97 when added togeather

#Lightning Subsetting
highLtg <- subset(test, ltgAll > 5)
lowLtg <- subset(test, ltgAll < 2 )
medLtg <- subset(test, ltgAll < 6 & ltgAll > 1)

#NLS Subsetting
highNLS <- subset(test, NLS > 72)
lowNLS <- subset(test, NLS < 25)
medNLS <- subset(test, NLS < 73 & NLS > 24)

#Infrasound Energy Subsetting
highIE <- subset(test, OverallABmedInfra > 2100000)
lowIE <- subset(test, OverallABmedInfra < 900000)
medIE <- subset(test, OverallABmedInfra > 899999 & OverallABmedInfra < 2099999)

#Infrasound Duration Subsetting
highID <- subset(test, maxinfraDur > 90)
lowID <- subset(test, maxinfraDur < 53)
medID <- subset(test, maxinfraDur > 53 & maxinfraDur < 90)

#VASR Subsetting
highVASR <- subset(test, VASR_AB > 4.6)
lowVASR <- subset(test, VASR_AB < 2.9)
medVASR <- subset(test, VASR_AB < 4.6 & VASR_AB >2.9)

#PV Subsetting
highPV <- subset(test, PV > 19)
lowPV <- subset(test, PV < 9)
medPV <- subset(test, PV > 9 & PV < 19)

#IPV Subsetting
highIPV <- subset(test, IPV > 50)
lowIPV <- subset(test, IPV < 35)
medIPV <- subset(test, IPV > 35 & IPV < 50)

#PF Subsetting
highPF <- subset(test, PF > 68270)
lowPF <- subset(test, PF < 33080)
medPF <- subset(test, PF < 68271 & PF > 33079)

#Cumulative Volume Subsetting
highCumVol <- subset(test, CumVol > 2054000)
lowCumVol <- subset(test, CumVol < 72660)
medCumVol <- subset(test, CumVol < 2054001 & CumVol > 72659)

#PPH_altered Subsetting
highPPHa <- subset(test, PPH_altered > 1150)
lowPPHa <- subset(test, PPH_altered < 930)
medPPHa <- subset(test, PPH_altered < 1150 & PPH_altered > 930)

#Pulling Matlab indexes into a new DF

MatlabIndexDF <- data.frame(highCumVol$MatLabCatNum, highID$MatLabCatNum)

write.table(highVASR, "highVASR", sep="\t")


write.table(lowCumVol, "lowCumVol", sep="\t")
write.table(lowID, "lowID", sep="\t")
write.table(lowIE, "lowIE", sep="\t")
write.table(lowIPV, "lowIPV", sep="\t")
write.table(lowLtg, "lowLtg", sep="\t")
write.table(lowNLS, "lowNLS", sep="\t")
write.table(lowPF, "lowPF", sep="\t")
write.table(lowPPHa, "lowPPHa", sep="\t")
write.table(lowPV, "lowPV", sep="\t")
write.table(lowVASR, "lowVASR", sep="\t")
write.table(medCumVol, "medCumVol", sep="\t")
write.table(medID, "medID", sep="\t")
write.table(medIE, "medIE", sep="\t")
write.table(medIPV, "medIPV", sep="\t")
write.table(medLtg, "medLtg", sep="\t")
write.table(medNLS, "medNLS", sep="\t")
write.table(medPF, "medPF", sep="\t")
write.table(medPPHa, "medPPHa", sep="\t")
write.table(medPV, "medPV", sep="\t")
write.table(medVASR, "medVASR", sep="\t")
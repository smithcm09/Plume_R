RedoubtH <- (RedoubtData2$PH*1000)

NLS_combined <- c(RedoubtData2$NLS, PlumeDerivedStatistics$NLS)
Heights <- c(RedoubtH, PPH_altered)

scatterplot(log10(Heights),log10(NLS_combined), col='blue')
min(RedoubtH)
max(RedoubtH)
log10(18900)
log10(5200)

mod.RC <- lm(log10(NLS_combined[c(-4,-16)])~log10(Heights[c(-4,-16)]))
abline(mod.RC)
summary(mod.RC)

plot(log10(RedoubtH),log10(RedoubtData2$NLS))

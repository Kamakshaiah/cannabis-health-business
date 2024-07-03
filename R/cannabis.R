# D:\Research\PAPERS\WORKING\marketing\cannabis

cnbfile <- read.csv(file.choose())
names(cnbfile)
cnbabs <- cnbfile["Abstract"]
length(t(cnbfile)) # 351

library(tm)

cnbcorp <- VCorpus((VectorSource(t(cnbabs))))

abstracts_corp <- tm_map(cnbcorp, stripWhitespace)
abstracts_corp <- tm_map(abstracts_corp, removePunctuation)
abstracts_corp <- tm_map(abstracts_corp, content_transformer(tolower))                          
abstracts_corp <- tm_map(abstracts_corp, removeWords, stopwords("english"))
abstracts_corp <- tm_map(abstracts_corp, removeNumbers)

path <- readline()
pathch <- gsub('\\\\', '//', path)
pathout <- file.path(pathch, '/outputs')
setwd(pathout)
getwd()

# write.csv(summary(abstracts_corp), 'corpus-summary.csv')
# data.frame(text = sapply(myCorpus, as.character), stringsAsFactors = FALSE)

adtm <- DocumentTermMatrix(abstracts_corp)
tm::inspect(adtm[10:16, ] )
dim(adtm) # [1]  117 3869
adtm$dimnames

adtm_ <- adtm[, -c(1:13)]
dim(adtm_) # [1]  117 3856

# cnbdense <- data.frame(as.matrix(removeSparseTerms(adtm_, 0.99)))
# dim(cnbdense)

cnbdf <- data.frame(as.matrix(adtm_), stringsAsFactors=FALSE)
dim(cnbdf)

grep('recre', names(cnbdf))

names(cnbdf)[c(2130, 2882, 2883)]

cnbdfr <- subset(cnbdf, select = c(agribusiness, business, businesseconomics, businesses, 
                                   health, healthcare, healthrelated, healthy, unhealthy, 
                                   cannabidiol, cannabinoid, cannabinoids, cannabis, cannabisbranded, 
                                   cannabisinfused, cannabiss, cbdcannabis, noncannabis, δtetrahydrocannabinol, 
                                   governance, policies, frameworks, recreation, recreational))
dim(cnbdfr)
names(cnbdfr)
head(cnbdfr)

library(FactoMineR)

fit <- CA(cnbdfr, 3)
plot.CA(fit, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5))
plot.CA(fit, xlim = c(15, 17), ylim = c(-1, 1))

path <- readline()
pathca <- gsub('\\\\', '/', path)
setwd(pathca)
getwd()

write.csv(fit$row$coord, 'row-coord.csv')
write.csv(fit$row$cos2, 'row-cos2.csv')
write.csv(fit$row$contrib, 'row-contrib.csv')
write.csv(fit$row$inertia, 'row-inertia.csv')

write.csv(fit$col$coord, 'col-coord.csv')
write.csv(fit$col$cos2, 'col-cos2.csv')
write.csv(fit$col$contrib, 'col-contrib.csv')
write.csv(fit$col$inertia, 'col-inertia.csv')

# factor analysis

library(psych)

rel <- alpha(cnbdfr)

fitg <- fa(cnbdfr)
fitfa <- fa(cnbdfr, 3)
structure.diagram(fitfa)

loadings(fitfa)
write.csv(loadings(fitfa), 'cnb-loadings.csv')
write.csv(fitfa$r.scores, 'cnb-rscores.csv')

write.csv(loadings(fitg), 'cnb-gf-loadings.csv')
write.csv(fitg$r.scores, 'cnb-gf-rscores.csv')

# sem

library(lavaan)

grep('cannab', names(cnbdf))

names(cnbdf)[c(441, 442, 443, 444, 445, 446, 447, 448, 449, 450, 451, 486, 1829, 2305, 3856)]

cnbdfr <- subset(cnbdf, select = c(
  agribusiness, business, businesseconomics, businessending, businesses, businesseshow, 
  sciencebusiness, health, healthcare, healthrelated, healthy, unhealthy,
  cannabidiol, cannabinoid, cannabinoids, cannabis, cannabisbranded, cannabisimpaired, cannabisinfused, cannabisrelated, cannabiss, cbdcannabis,          
  noncannabis, δtetrahydrocannabinol, governance, policies, frameworks))

rel <- alpha(cnbdfr)

fitg <- fa(cnbdfr)
fitfa <- fa(cnbdfr, 3)
structure.diagram(fitfa)

loadings(fitfa)
write.csv(loadings(fitfa), 'cnb-loadings.csv')
write.csv(fitfa$r.scores, 'cnb-rscores.csv')

library(lavaan)

# model <- '
# 
#         # latent
#         business_ =~ business + agribusiness + businesseconomics
#         health_ =~ health + healthcare + unhealthy
#         cbm =~ cannabidiol + cannabinoid + cannabis + δtetrahydrocannabinol
#         gpf =~ governance + policies + frameworks
#         recr =~ recreation + recreational
# 
#         # direct effect
#         
#         business_ ~ c1 * health_ + c2* recr
#         health_ ~ c3 * cbm + c4 * recr
#         
#         # mediation  
#         
#         gpf ~ a * business_
#         health_ ~ b * gpf 
#         
#         # indirect 
#         ind := a * b
#         total := (c1 + c2 + c3 + c4) + (a*b) 
# '
# model <- '
# 
#         # latent
#         business_ =~ business + agribusiness + businesseconomics
#         health_ =~ health + healthcare + unhealthy
#         cbm =~ cannabidiol + cannabinoid + cannabis + δtetrahydrocannabinol
#         gpf =~ governance + policies + frameworks
#         recr =~ recreation + recreational
# 
#         # direct effect
#         
#         business_ ~ c1 * health_ + c2 * cbm + c3 * recr
#         
#         # mediation  
#         
#         gpf ~ a * business_
#         health_ ~ b * gpf 
#         
#         # indirect 
#         ind := a * b
#         total := (c1 + c2 + c3) + (a*b) 
# '

model <- '
        business + agribusiness + businesseconomics ~ c1 * cannabidiol + c2 * cannabinoid + c3 * cannabis + c4 * δtetrahydrocannabinol
        health + healthcare + unhealthy ~ c5 * cannabidiol + c6 * cannabinoid + c7 * cannabis + c8 * δtetrahydrocannabinol
        business + agribusiness + businesseconomics ~ c9 * health + c10 * healthcare + c11 * unhealthy
        business + agribusiness + businesseconomics ~ c12 * governance + c13 * policies + c14 * frameworks
        cannabidiol + cannabinoid + cannabis + δtetrahydrocannabinol ~ c15 * governance + c16 * policies + c17 * frameworks
        business + agribusiness + businesseconomics ~ c18 * recreation + c19 * recreational

'

fitsem <- sem(model, scale(cnbdfr))
summary(fitsem)

modindices(fitsem)

library(semPlot)
semPaths(fitsem, whatLabels = "est", layout = "circle2")

path <- readline()
pathsemnew <- gsub('\\\\', '/', path)
setwd(pathsemnew)
getwd()

write.csv(parameterEstimates(fitsem), 'cnb_fit.csv')
write.csv(fitmeasures(fitsem), 'cnb_fit_measures.csv')

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
semPaths(fitsem, layout = "circle2")
semPaths(fitsem, whatLabels = "std", layout = "circle2")

# library(pls)
# 
# cnbfit <- plsr(business ~ (cannabidiol + cannabinoid + cannabis + δtetrahydrocannabinol)* health, ncomp = 3, data = cnbdfr, validation = "LOO")
# cnbfit1 <- pcr(business ~ (cannabidiol + cannabinoid + cannabis + δtetrahydrocannabinol)* health, ncomp = 3, data = cnbdfr, validation = "LOO")
# 
# summary(cnbfit1)
# # path <- readline()
# # pathplsr <- gsub('\\\\', '/', path)
# # setwd(pathplsr)
# # getwd()
# 
# write.csv(data.frame(cnbfit$loadings[, c(1, 2, 3)]), 'loadings.csv')
# write.csv(data.frame(cnbfit$coefficients), 'coefficients.csv')
# 
# summary(cnbfit)
# 
# plot(RMSEP(cnbfit), legendpos = "topright")
# plot(cnbfit, plottype = "scores", comps = 1:3)
# 
# plot(cnbfit, "loadings", comps = 1:2, legendpos = "topleft")
# abline(h = 0)
# 
# plot(cnbfit, plottype = "coef", ncomp=1:3, legendpos = "bottomleft")
# 
# corrplot(cnbfit1, plottype = "correlation", ncomp=1:3, legendpos = "bottomleft")
# # number of comps
# 
# selectNcomp(cnbfit, method = "onesigma", plot = TRUE)
# selectNcomp(cnbfit, method = "randomization", plot = TRUE)


# library(seminr)
# 
# meas_mod <- constructs(
#   composite("business_", c("business", "agribusiness", "businesseconomics")), 
#   composite("health_", c("health", "healthcare", "unhealthy")), 
#   composite("cnb", c("cannabidiol", "cannabinoid", "cannabis", "δtetrahydrocannabinol")), 
#   composite("gpf", c("governance", "policies", "frameworks")), 
#   composite("recr", c("recreation", "recreational"))
# )
# 
# struc_mod <- relationships(
#   paths(from = c("cnb", "health_", "recr"), to = c("business_")), 
#   paths(from = c("gpf"), to = c("cnb", "business_", "health_")),
#   paths(from = c("recr"), to = c("gpf", "business_", "health_"))
#   
# )
# 
# fit <- estimate_pls(cnbdfr, 
#                     measurement_model = meas_mod, 
#                     structural_model = struc_mod, 
#                     missing = mean_replacement,
#                     missing_value = "-99"
# )
# plot(fit)
# 
# sumfit <- summary(fit)
# sumfit$loadings^2
# setwd(file.path(getwd(), 'plssem'))
# # class(sumfit$loadings)
# write.csv(sumfit$paths, 'plssem-path-coef.csv')
# write.csv(sumfit$reliability, 'plssem-realiability.csv')
# write.csv(sumfit$loadings, 'plssem-loadings.csv')
# 
# bootstrmod <- bootstrap_model(fit, nboot=10)
# sumbootstrmod <- summary(bootstmod, apha = 0.05)
# write.csv(sumfit$loadings^2, 'indicator-validity.csv') # indicator validity
# write.csv(as.matrix(sumfit$validity$vif_items), 'indicator-collinearity.csv') # collinearity
# write.csv(sumfit$reliability, 'model-realiability.csv')
# write.csv(sumfit$validity$fl_criteria, 'disc-val-flcri.csv')
# write.csv(sumbootstrmod$bootstrapped_HTMT, 'disc-val-htmt.csv')
# 
# plot(bootstmod)
# 
# write.csv(sumbootstrmod$bootstrapped_total_paths, 'bootstr-paths.csv')
# write.csv(sumbootstrmod$bootstrapped_loadings, 'bootstr-loadings.csv')
source("../R/parKml.r")

cat("\n####################################################################
########################### Test ParKml ############################
####################################################################\n")

cleanProg(.ParKml.validity)
cleanProg(parKml,,,2) # DISTANCE_METHODS meanNA
cleanProg(.ParKml.show)
new("ParKml")
parKml()
parKml(saveFreq=300)
pk <- parKml(distanceName="maximum")
pk2 <- parKml(distanceName="manhattan",saveFreq=30,maxIt=50,imputationMethod="copyMean")
pk3 <- parKml(distanceName="minkowski",power=3,saveFreq=30,maxIt=50,imputationMethod=c(method="copyMean",boundInf="min",boundSup="max"),centerMethod=medianNA,startingCond="randomAll")
pk['distance']
pk['distanceName']<-"manhattan"
pk['distance'](1:3,2:4)
pk["distance"] <- function(x,y)cor(x,y)
pk['distance'](c(1:3,3),2:5)


cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++ Fin Test ParKml +++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

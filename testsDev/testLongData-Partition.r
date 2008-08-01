source("../R/longData-Partition.R")


cat("###################################################################
#################### Test  LongData & Partition ###################
############################### plot ##############################
###################################################################\n")

par(mfrow=c(3,4))
plot(ld3)
plot(ld3,c3a)
plot(ld3,c3b,colorTraj="no")
plot(ld3,c3c,colorTraj="black")
plot(ld3,c3a,colorTraj="color")
plot(ld3,c3a,colorMean="no")
plot(ld3,c3a,colorMean="black",point="point")
plot(ld3,c3a,colorMean="both")
plot(ld3,c3a,colorMean="black",point="symbols")
plot(ld3,c3a,colorTraj="color",colorMean="no",point="letters")
plot(ld3,c3a,colorTraj="no",colorMean="no",point="no")
plot(ld3,c3a,colorTraj="color",colorMean="no",point="letters",size=3)

par(mfrow=c(3,4))
plot(ld4)
plot(ld4,c4a)
plot(ld4,c4b,colorTraj="no")
plot(ld4,c4c,colorTraj="black")
plot(ld4,c4a,colorTraj="color")
plot(ld4,c4a,colorMean="no")
plot(ld4,c4a,colorMean="black")
plot(ld4,c4a,colorMean="both")
plot(ld4,c4a,colorMean="black",point="symbols")
plot(ld4,c4a,colorTraj="color",colorMean="no",point="letters")
plot(ld4,c4a,colorTraj="no",colorMean="no",point="no")
plot(ld4,c4a,colorTraj="color",colorMean="no",point="letters",size=3)

par(mfrow=c(3,3))
plot(ld3,c3a,colorTraj="color",subGroups="A",colorMean="both")
plot(ld3,c3a,colorTraj="color",subGroups="B",colorMean="both",point="letters")
plot(ld3,c3a,colorTraj="color",subGroups="C",colorMean="both")
plot(ld3,c3a,colorTraj="color",subGroups=c("A","C"),colorMean="both")
plot(ld3,c3a,colorTraj="no",subGroups=c("A","C"),colorMean="no")
plot(ld3,c3a,colorTraj="no",subGroups=c("A","C"),colorMean="both")
plot(ld3,c3a,colorTraj="no",subGroups=c("A","C"),colorMean="no",point="letters")
plot(ld3,c3a,colorMean="no",subGroups=c("A","C","B"))
plot(ld3,c3a,colorMean="no")

par(mfrow=c(3,4))
plot(ld1)
plot(ld1,colorTraj="black")
plot(ld2)
plot(ld2n)
plot(ld3)
plot(ld3n,colorTraj="black",colorMean="black")
plot(ld4,colorTraj="black",colorMean="both")
plot(ld4n,colorTraj="black",point="symbols")
plot(ld4n,colorTraj="black")
plot(ld5,colorTraj="black")
plot(ld5,point="point")
plot(ld5n)

dev.off()
plotSubGroups(ld3,c3a,colorMean="both")
plotSubGroups(ld3,c3b,colorMean="no")
plotSubGroups(ld4,c4a,colorMean="both")
plotSubGroups(ld3,c3b,colorMean="no",colorTraj="no",point="letters")


#electTrajNoNA(ld1n)
#electTrajNoNA(ld2n)
#electTrajNoNA(ld3n)
#selectTrajNoNA(ld3n)
#selectTrajNoNA(ld3n)
#selectTrajNoNA(ld3n)
#try(selectTrajNoNA(ld4n))
#selectTrajNoNA(ld4n)
#selectTrajNoNA(ld5n)
#selectTrajNoNA(ld5n)

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++ Fin Test  LongData-Partition +++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")





#choicePlot(ld4,p4a)

dev.off()
trajKmlSlow(ld3["traj"],partitionInitialise(3,243),screenPlot=1)
trajKmlSlow(ld3n["traj"],partitionInitialise(4,243),screenPlot=1)
trajKmlSlow(ld3["traj"],partitionInitialise(4,243),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))
trajKmlSlow(ld3n["traj"],partitionInitialise(4,243),screenPlot=1,centerMethod=medianNA)
trajKmlSlow(ld3["traj"],partitionInitialise(4,243),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)
trajKmlSlow(ld3n["traj"],partitionInitialise(5,243),screenPlot=1)

trajKmlSlow(ld4n["traj"],partitionInitialise(2,180),screenPlot=1)
trajKmlSlow(ld4n["traj"],partitionInitialise(3,180),screenPlot=1)
trajKmlSlow(ld4n["traj"],partitionInitialise(4,180),screenPlot=1)
trajKmlSlow(ld4n["traj"],partitionInitialise(5,180),screenPlot=1)

trajKmlSlow(ld4n["traj"],partitionInitialise(2,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))
trajKmlSlow(ld4n["traj"],partitionInitialise(3,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))
trajKmlSlow(ld4n["traj"],partitionInitialise(4,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))
trajKmlSlow(ld4n["traj"],partitionInitialise(5,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"))

trajKmlSlow(ld4n["traj"],partitionInitialise(2,180),screenPlot=1,centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(3,180),screenPlot=1,centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(4,180),screenPlot=1,centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(5,180),screenPlot=1,centerMethod=medianNA)

trajKmlSlow(ld4n["traj"],partitionInitialise(2,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(3,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(4,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)
trajKmlSlow(ld4n["traj"],partitionInitialise(5,180),screenPlot=1,distance=function(x,y)dist(rbind(x,y),method="manhattan"),centerMethod=medianNA)


# include "kml.h"
void kml1(double *mTraj, int *pNbId, int *pNbTime,
	  double *mClustersCenter, int *pNbClusters, int *pConvergenceTime,
	  int *vClusterAffectation){
    int nbId = *pNbId, nbClusters = *pNbClusters, nbTime = *pNbTime, convergenceTime = *pConvergenceTime;

    int iter, iNbCenters, iNbId, iNbClusters, newNbClusters=0, iNbTime, indiceClusterAffect, nbNA = 0;
    double best, dist, tmp;
    int *nbIndivEachClusters=malloc(nbClusters*sizeof(int));
    Rboolean updated;

    for(iNbId = 0; iNbId < nbId; iNbId++) vClusterAffectation[iNbId] = 1;
//    Rprintf("\n\nDDD mClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
    //  Rprintf("\n DDD mAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}
    for(iter = 0; iter < convergenceTime; iter++) {
	updated = FALSE;
	for(iNbId = 0; iNbId < nbId; iNbId++) {
 	    // find nearest centre for each point
	    best = R_PosInf;
	    for(iNbClusters = 0; iNbClusters < nbClusters; iNbClusters++) {

		// Distance between mClustersCenter j and individual i
		dist = 0.0; nbNA = 0; tmp=0;
		for(iNbTime=0;iNbTime<nbTime;iNbTime++){
//		    Rprintf("\nAvant distance i=%i nbId=%i c=%i j=%i nbClust=%i mTraj=%f mClus=%f",i,nbId,c,j,nbClusters,mTraj[i+nbId*c],mClustersCenter[j+nbClusters*c]);
		    if(R_FINITE(mTraj[iNbId+iNbTime*nbId]) && R_FINITE(mClustersCenter[iNbClusters+iNbTime*nbClusters])){
//			Rprintf("\nXXXX Avant distance i=%i nbId=%i c=%i j=%i nbClust=%i mTraj=%f mClus=%f",i,nbId,c,j,nbClusters,mTraj[i+nbId*c],mClustersCenter[j+nbClusters*c]);
			//		Rprintf("\nDistA=%f Tmp=%f",dist,tmp);
			tmp = mTraj[iNbId+iNbTime*nbId] - mClustersCenter[iNbClusters+iNbTime*nbClusters];
//			Rprintf("\nDistB=%f Tmp=%f",dist,tmp);
			dist += (tmp * tmp);
			//		Rprintf("\nDistC=%f Tmp=%f",dist,tmp);
		    }else{
			nbNA +=1;
		    }
		}
		dist = (dist)*nbTime/(nbTime-nbNA);

		if(dist < best) {
		    best = dist;
		    newNbClusters = iNbClusters+1;
		}
	    }
	    if(vClusterAffectation[iNbId] != newNbClusters) {
		updated = TRUE;
		vClusterAffectation[iNbId] = newNbClusters;
	    }else{}

//	    Rprintf("\n\nMMM mClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
	    //    Rprintf("\n MMM mAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}
//	    Rprintf("\n\nmClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
	    //    Rprintf("\nmAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}
//	    Rprintf("\niter= %i max=%i i=%i",iter,convergenceTime,i);
	}

	if(!updated) break;
	// update each centre
	for(iNbCenters = 0; iNbCenters < nbClusters*nbTime; iNbCenters++) mClustersCenter[iNbCenters] = 0.0;

	for(iNbTime = 0; iNbTime < nbTime; iNbTime++) {
	    for(iNbClusters = 0; iNbClusters < nbClusters; iNbClusters++) nbIndivEachClusters[iNbClusters] = 0;
	    for(iNbId = 0; iNbId < nbId; iNbId++) {
		indiceClusterAffect = vClusterAffectation[iNbId] - 1;
		if(R_FINITE(mTraj[iNbId+iNbTime*nbId])){
		    mClustersCenter[indiceClusterAffect+iNbTime*nbClusters] += mTraj[iNbId+iNbTime*nbId];
		    nbIndivEachClusters[indiceClusterAffect]++;
		}else{}
	    }
	    for(iNbClusters = 0; iNbClusters < nbClusters; iNbClusters++){
//		Rprintf("\nTOTO");
//		Rprintf("j=%i Center=%f nbEach=%i",j,mClustersCenter[j+c*nbClusters],nbIndivEachClusters[j]);
		if(nbIndivEachClusters[iNbClusters]==0){
		    mClustersCenter[iNbClusters+iNbTime*nbClusters]=NA_REAL;
		}else{
		    mClustersCenter[iNbClusters+iNbTime*nbClusters]/=nbIndivEachClusters[iNbClusters];
		}
//		Rprintf("\n    j=%i Center=%f nbEach=%i",j,mClustersCenter[iNbClusters+iNbTime*nbClusters],nbIndivEachClusters[j]);
	    }
	}
//	Rprintf("\n\nFFF mClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
	//Rprintf("\n FFF mAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}

    }
//    Rprintf("\n\nmClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
    //  Rprintf("\nmAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}

    *pConvergenceTime = iter + 1;
}

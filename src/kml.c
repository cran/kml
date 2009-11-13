# include "kml.h"

#define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
#define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))

/* ****************************************************
********************** DISTANCES **********************
**************************************************** */

static double R_manhattan2(double *x,double *y,int taille){
    double dev, dist;
    int count, i;

    count = 0;
    dist = 0;
    for(i = 0 ; i < taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    dev = fabs(x[i] - y[i]);
	    if(!ISNAN(dev)) {
		dist += dev;
		count++;
	    }
	}
    }
    if(count == 0) return NA_REAL;
    if(count != taille) dist /= ((double)count/taille);
    return dist;
}




static double R_euclidean2(double *x, double *y, int taille)
{
    double dev, dist;
    int count, i;

    count= 0;
    dist = 0;
    for(i = 0 ; i < taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    dev = (x[i] - y[i]);
	    if(!ISNAN(dev)) {
		dist += dev * dev;
		count++;
	    }
	}
    }
    if(count == 0)return NA_REAL;
    if(count != taille) dist /= ((double)count/taille);
    return sqrt(dist);
}


static double R_minkowski2(double *x,double *y,int taille,double power){
    double dev, dist;
    int count, i;

    count= 0;
    dist = 0;
    for(i = 0 ; i < taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    dev = (x[i] - y[i]);
	    if(!ISNAN(dev)) {
		dist += R_pow(fabs(dev), power);
		count++;
	    }
	}
    }
    if(count == 0) return NA_REAL;
    if(count != taille) dist /= ((double)count/taille);
    return R_pow(dist, 1.0/power);
}


static double R_maximum2(double *x,double *y,int taille){
    double dev, dist;
    int count, i;

    count = 0;
    dist = -DBL_MAX;
    for(i = 0 ; i < taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    dev = fabs(x[i] - y[i]);
	    if(!ISNAN(dev)) {
		if(dev > dist)
		    dist = dev;
		count++;
	    }
	}
    }
    if(count == 0) return NA_REAL;
    return dist;
}


static double R_canberra2(double *x,double *y,int taille){
    double dev, dist, sum, diff;
    int count, i;

    count = 0;
    dist = 0;
    for(i = 0 ; i < taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    sum = fabs(x[i] + y[i]);
	    diff = fabs(x[i] - y[i]);
	    if (sum > DBL_MIN || diff > DBL_MIN) {
		dev = diff/sum;
		if(!ISNAN(dev) ||
		   (!R_FINITE(diff) && diff == sum &&
		    /* use Inf = lim x -> oo */ (dev = 1.))) {
		    dist += dev;
		    count++;
		}
	    }
	}
    }
    if(count == 0) return NA_REAL;
    if(count != taille) dist /= ((double)count/taille);
    return dist;
}


static double R_dist_binary2(double *x,double *y,int taille){
    int total, count, dist;
    int i;

    total = 0;
    count = 0;
    dist = 0;

    for(i = 0 ; i < taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    if(!both_FINITE(x[i], y[i])) {
		warning(("treating non-finite values as NA"));
	    }
	    else {
		if(x[i] || y[i]) {
		    count++;
		    if( ! (x[i] && y[i]) ) dist++;
		}
		total++;
	    }
	}
    }

    if(total == 0) return NA_REAL;
    if(count == 0) return 0;
    return (double) dist / count;
}



enum { MANHATTAN=1, EUCLIDEAN, MINKOWSKI, MAXIMUM,  CANBERRA, BINARY };
void R_distance2(double *x, double *y, int *taille, int *method, double *power, double *dist)
{
    switch(*method) {
    case MANHATTAN:
	*dist = R_manhattan2(x,y,*taille);
	break;
    case EUCLIDEAN:
	*dist = R_euclidean2(x,y,*taille);
	break;
    case MINKOWSKI:
	*dist = R_minkowski2(x,y,*taille,*power);
	break;
    case MAXIMUM:
	*dist = R_maximum2(x,y,*taille);
	break;
    case CANBERRA:
	*dist = R_canberra2(x,y,*taille);
	break;
    case BINARY:
	*dist = R_dist_binary2(x,y,*taille);
	break;
	}
}



void printMatrix(double *mTraj,int *nbCol, int *nbLigne){
    int i=0,j=0;
    for(i=0 ;  i < *nbLigne ; i++){
	for(j=0 ; j < *nbCol ; j++){
	    Rprintf(" %f",mTraj[i * *nbCol + j]);
	}
	Rprintf("\n");
    }
}


/* ****************************************************
*********************** K means ***********************
**************************************************** */

void calculMean(double *mTraj, int *iNbInd, int *iNbTime, int *vClusterAffectation, int *iNbClusters,
		double *mTrajMean){

    int i=0,j=0;
    int nbEachCluster[*iNbClusters * *iNbTime];

    /*    Rprintf("Traj =\n");
    printMatrix(mTraj,iNbTime,iNbInd);
    Rprintf("\n\niNbInd=%i iNbTime=%i iNbClusters=%i",*iNbInd,*iNbTime,*iNbClusters);
    Rprintf("\nvClusterAffectation=%i %i %i",vClusterAffectation[0],vClusterAffectation[1],vClusterAffectation[2]);


    Rprintf("\n\nTrajMean =\n");
    printMatrix(mTrajMean,iNbTime,iNbClusters);
    */
    // L'initialisation de mTrajMean est IMPERATIVEMENT à faire sous R
    // for (i=0 ; i < *iNbClusters * *iNbTime ; i++){mTrajMean[i] = 0;};

    for (i=0 ; i < *iNbClusters * *iNbTime ; i++){nbEachCluster[i] = 0;};

    for (i=0 ; i < *iNbInd ; i++){
        for(j=0 ; j < *iNbTime ; j++){
	    if(R_FINITE(mTraj[i * *iNbTime + j]) && vClusterAffectation[i]!=NA_INTEGER ){
		mTrajMean[(vClusterAffectation[i]-1) * *iNbTime + j] += mTraj[i * *iNbTime + j];
		nbEachCluster[(vClusterAffectation[i]-1) * *iNbTime + j] += 1;
		/*          Rprintf("\n\nXXXXXXXX\ni=%i j=%i indice=%i trajMean=%f nbEach=%i",i,j,(vClusterAffectation[i]-1) * *iNbInd + j,
		    mTrajMean[(vClusterAffectation[i]-1) * *iNbTime + j],
		    nbEachCluster[(vClusterAffectation[i]-1) * *iNbTime + j]
		);
            Rprintf("\n\nTrajMean =\n");
            printMatrix(mTrajMean,iNbTime,iNbClusters);*/
	    }else{};
	};
    }
    for (i=0 ; i < *iNbClusters ; i++){
        for(j=0 ; j < *iNbTime ; j++){
	    mTrajMean[i * *iNbTime + j] /= nbEachCluster[i * *iNbTime + j];
	}
    }
    /*    Rprintf("\n\nTrajMean =\n");
	  printMatrix(mTrajMean,iNbTime,iNbClusters);*/
}



void affecteIndiv(double *mTraj, int *iNbInd, int *iNbTime,
		  double *mTrajMean, int *iNbClusters, int *distance, double *power,
		  int *vClusterAffectation){

    int i=0,j=0 ;
    double best=0,dist=0;
    /*    Rprintf("\nTraj =\n");
    printMatrix(mTraj,iNbTime,iNbInd);

    Rprintf("\n\nTrajMean =\n");
    printMatrix(mTrajMean,iNbTime,iNbClusters);
    */
    for (i=0 ; i<*iNbInd ; i++){
      //Rprintf("%f %f %i %i %f ",mTraj[i * *iNbTime], mTrajMean[0], *iNbTime, *distance, *power);
	R_distance2(&mTraj[i * *iNbTime], &mTrajMean[0], iNbTime, distance, power, &best);
	//Rprintf("\nBest=%f \n",best);
	vClusterAffectation[i] = 1 ;

	for(j=2 ; j<*iNbClusters+1 ; j++){
	    R_distance2(&mTraj[i * *iNbTime], &mTrajMean[(j-1) * *iNbTime], iNbTime, distance, power, &dist);
	    // Rprintf("\nDist=%f \n",dist);
	    if(dist<best){
		best = dist;
		vClusterAffectation[i] = j ;
	    }else{};
	    // Rprintf("\nBest2=%f \n",best);
	}
    }
}



/*void pour(int *t){
    Rprintf("\nT0=%i T1=%i T2=%i T3=%i",t[0],t[1],t[2],t[3]);
    if(t[0]==NA_INTEGER){Rprintf("CA MARCHE");}else{Rprintf("Ben non");};
}
*/

/*void essai(double *traj, int *ncol, int *nlig){
    pour(&traj[0]);
    pour(&traj[4]);
    }
*/
//void R_distance2(double *x, double *y, int *taille, int *method, double *power, double *dist)




// void calculMean(double *mTraj, int *iNbInd, int *iNbTime, int *vClusterAffectation, int *iNbClusters,double *mTrajMean);






void kml1(double *mTraj, int *iNbInd, int *iNbTime, int *iNbClusters, int *maxIt,
	  int *distance, double *power, int *vClusterAffectation1, int *convergenceTime){

/*    int nbId = *pNbId, nbClusters = *pNbClusters, nbTime = *pNbTime, convergenceTime = *pConvergenceTime;

    int iter, iNbCenters, iNbId, iNbClusters, newNbClusters=0, iNbTime, indiceClusterAffect, nbNA = 0;
    double best, dist, tmp;
    int *nbIndivEachClusters=malloc(nbClusters*sizeof(int));
    Rboolean updated;
*/
    int i=0,iter=0;
    int *vClusterAffectation2=malloc(*iNbInd * sizeof(int));
    double *mTrajMean=malloc(*iNbClusters * *iNbTime * sizeof(double));

    for(i = 0; i < *iNbClusters * *iNbTime; i++){mTrajMean[i] = 0.0;};
    for(i = 0; i < *iNbInd; i++){vClusterAffectation2[i] = 0;};

    //    printMatrix(mTrajMean,iNbTime,iNbClusters);

//    for(iNbId = 0; iNbId < nbId; iNbId++) vClusterAffectation[iNbId] = 1;
//    Rprintf("\n\nDDD mClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
    //  Rprintf("\n DDD mAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}
    for(iter = 0; iter < *maxIt; iter+=2){
	calculMean(mTraj,iNbInd,iNbTime,vClusterAffectation1,iNbClusters,mTrajMean);
	affecteIndiv(mTraj,iNbInd,iNbTime,mTrajMean,iNbClusters,distance,power,vClusterAffectation2);

	i = 0;
	while(vClusterAffectation1[i]==vClusterAffectation2[i] && i<*iNbInd){i++;};
	if(i == *iNbInd){
	    *convergenceTime = iter + 1;
	    break;
	}else{};

	calculMean(mTraj,iNbInd,iNbTime,vClusterAffectation2,iNbClusters,mTrajMean);
	affecteIndiv(mTraj,iNbInd,iNbTime,mTrajMean,iNbClusters,distance,power,vClusterAffectation1);

	i = 0;
	while(vClusterAffectation1[i]==vClusterAffectation2[i] && i<*iNbInd){i++;};
	if(i == *iNbInd){
	    *convergenceTime = iter + 2;
	    break;
	}else{};
    }

}



//	    Rprintf("\n\nMMM mClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
	    //    Rprintf("\n MMM mAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}
//	    Rprintf("\n\nmClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
	    //    Rprintf("\nmAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}
//	    Rprintf("\niter= %i max=%i i=%i",iter,convergenceTime,i);
/*	}

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




double euclideanNA(double *x,double *y,int *taille){
  double dist = 0.0;
  int nbNA = 0, i=0;
  double tmp=0;

  Rprintf("\n DIST 2");

  for(i=0;i<*taille;i++){
    if(R_FINITE(x[i]) && R_FINITE(y[i])){
      Rprintf("\nDistA=%f Tmp=%f",dist,tmp);
      tmp = x[i]-y[i];
      Rprintf("\nDistB=%f Tmp=%f",dist,tmp);
      dist += (tmp * tmp);
      Rprintf("\nDistC=%f Tmp=%f",dist,tmp);
    }else{
      nbNA +=1;
    }
  }
  dist = pow((dist) * *taille/(*taille-nbNA),0.5);
  return(dist);
}


double manhattanNA(double *x,double *y,int *taille){
  double dist = 0.0;
  int nbNA = 0, i=0;

  for(i=0;i<*taille;i++){
    if(R_FINITE(x[i]) && R_FINITE(y[i])){
      dist += abs(x[i]-y[i]);
    }else{
      nbNA +=1;
    }
  }
  dist = (dist)* *taille/(*taille-nbNA);
  return(dist);
}

double maximumNA(double *x,double *y,int *taille){
  double dist = 0.0,tmp = 0.0 ;
  Rboolean onlyNA = TRUE;
  int i=0 ;

  for(i=0;i<*taille;i++){
    if(R_FINITE(x[i]) && R_FINITE(y[i])){
      //		Rprintf("\nDistA=%f Tmp=%f",dist,tmp);
      tmp = abs(x[i]-y[i]);
      if(dist<tmp){dist=tmp;}else{};
      onlyNA = FALSE;
      //			Rprintf("\nDistB=%f Tmp=%f",dist,tmp);
    }else{};
  }
  if(onlyNA){
    dist = NA_REAL;
  }else{};
  return(dist);
}

double minkowskiNA(double *x,double *y,int *taille, double *power){
  double dist = 0.0;
  int nbNA = 0, i=0;

  for(i=0;i<*taille;i++){
    if(R_FINITE(x[i]) && R_FINITE(y[i])){
      dist += pow(abs(x[i]-y[i]),*power);
    }else{
      nbNA +=1;
    }
  };
  dist = pow( (dist) * *taille/(*taille-nbNA), 1/ *power);
  return(dist);
}


void distance(double *x,double *y,int *taille, double *power, double *dist){
  if(*power==2.0){
    *dist = euclideanNA(x,y,taille);
  }else{
    if(*power==1.0){
      *dist = manhattanNA(x,y,taille);
    }else{
      if(*power==R_PosInf){
	*dist = maximumNA(x,y,taille);
      }else{
        *dist = minkowskiNA(x,y,taille,power);
      }
    }
  }
}

*/

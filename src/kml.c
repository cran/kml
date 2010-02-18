# include "kml.h"

#define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
#define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))

/*void R_init_kml(DllInfo *info){
    printMatrix = R_GetCCallable("longitudinalData","printMatrix");
    printMatrixInt = R_GetCCallable("longitudinalData","printMatrixInt");
    }*/

/* ****************************************************
********************** DISTANCES **********************
**************************************************** */



/*void printMatrix(double *mTraj,int *nbCol, int *nbLigne){
    int i=0,j=0;
    for(i=0 ;  i < *nbLigne ; i++){
	for(j=0 ; j < *nbCol ; j++){
	    Rprintf(" %f",mTraj[i * *nbCol + j]);
	}
	Rprintf("\n");
    }
}
*/

/*
void printMatrixInt(int *mTraj,int *nbCol, int *nbLigne){
    int i=0,j=0;
    for(i=0 ;  i < *nbLigne ; i++){
	for(j=0 ; j < *nbCol ; j++){
	    Rprintf(" %i",mTraj[i * *nbCol + j]);
	}
	Rprintf("\n");
    }
}
*/



/* *****************************************************
****************** Distances for traj ******************
***************************************************** */

static double manhattanTraj(double *x,double *y,int *taille){
    double dist=0.0;
    int nbNA=0, i=0;
    double difference=0.0;

    for(i=0 ; i<*taille ; i++){
	if(R_FINITE(x[i]) && R_FINITE(y[i])){
	    difference = fabs(x[i]-y[i]);
	    if(!ISNAN(difference)){
		dist += difference;
	    }else{}
	}else{
	    nbNA +=1;
	}
    }
    if(nbNA==*taille){return NA_REAL;}else{};
    if(nbNA!=0){dist = (dist)* *taille/(*taille-nbNA);}else{};
    return(dist);
}



static double euclideanTraj(double *x,double *y,int *taille){
    double dist=0.0;
    int nbNA=0, i=0;
    double difference=0.0;

    for(i=0 ; i<*taille ; i++){
	if(R_FINITE(x[i]) && R_FINITE(y[i])){
	    difference = x[i]-y[i];
	    if(!ISNAN(difference)){
		dist += difference * difference;
	    }else{}
	}else{
	    nbNA +=1;
	}
    }
    if(nbNA==*taille){return NA_REAL;}else{};
    if(nbNA!=0){dist = (dist)* *taille/(*taille-nbNA);}else{};
    dist = pow(dist,0.5);
    return(dist);
}

static double minkowskiTraj(double *x,double *y,int *taille, double *power){
    double dist = 0.0;
    int nbNA = 0, i=0;
    double difference=0.0;

    for(i=0;i<*taille;i++){
	if(R_FINITE(x[i]) && R_FINITE(y[i])){
	    difference = fabs(x[i]-y[i]);
	    if(!ISNAN(difference)){
		dist += pow(difference,*power);
	    }else{}
	}else{
	    nbNA +=1;
	}
    };
    if(nbNA==*taille){return NA_REAL;}else{};
    if(nbNA!=0){dist = (dist)* *taille/(*taille-nbNA);}else{};
    dist = pow( dist, 1/ *power);
    return(dist);
}


static double maximumTraj(double *x,double *y,int *taille){
    double dist=0.0;
    int i=0;
    Rboolean onlyNA = TRUE;
    double difference=0.0;

    dist = -DBL_MAX;
    for(i = 0 ; i < *taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    difference = fabs(x[i] - y[i]);
	    if(!ISNAN(difference)) {
		if(difference > dist){
		    dist = difference;
		    onlyNA = FALSE;
		}else{};
	    }else{
	    };
	};
    };
    if(onlyNA){return(NA_REAL);}else{}
    return(dist);
}


static double canberraTraj(double *x, double *y, int *taille){//int nr, int nc, int i1, int i2)
    double dist=0.0;
    int nbNA=0,i=0;
    double fraction=0.0,sum=0.0;

    for(i = 0 ; i < *taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    sum = fabs(x[i]) + fabs(y[i]);
	    if(sum==0){
		fraction = 1;
	    }else{
		fraction = fabs(x[i] - y[i])/sum;
	    };
	    if(!ISNAN(fraction)){
		dist += fraction;
	    }else{};
	}else{
	    nbNA +=1;
	}
    }
    if(nbNA == *taille){return NA_REAL;}else{};
    if(nbNA!=0){dist = dist * *taille/(*taille-nbNA);}else{};
    return dist;
}


static double binaryTraj(double *x, double *y, int *taille){
    double dist=0.0;
    int nbNA=0,i=0;
    int numerator=0,denumerator=0;

    for(i = 0 ; i < *taille ; i++) {
	if(both_non_NA(x[i], y[i])) {
	    if(!both_FINITE(x[i], y[i])) {
		nbNA++;
	    }else{
		if(x[i] || y[i]) {
		    denumerator++;
		    if( ! (x[i] && y[i]) ){numerator++;}else{};
		}else{};
	    }
	}else{
	    nbNA++;
	}
    }

    if(denumerator==0){return NA_REAL;}else{};
    dist = (double)numerator/(double)denumerator;
    return(dist);
}





void distanceTraj(double *x,double *y,int *taille, double *power, double *dist){
    *dist = 0.0;
    if(*power==2.0){
	*dist = euclideanTraj(x,y,taille);
    }else{
	if(*power==1.0){
	    *dist = manhattanTraj(x,y,taille);
	}else{
	    if(*power==R_PosInf){
		*dist = maximumTraj(x,y,taille);
	    }else{
		if(*power==-1){ // -1 code canberra
		    *dist = canberraTraj(x,y,taille);
		}else{
		    if(*power==-2){ // -2 code binary
			*dist = binaryTraj(x,y,taille);
		    }else{
			*dist = minkowskiTraj(x,y,taille,power);
		    }
		}
	    }
	}
    }
}



/* ****************************************************
*********************** K means ***********************
**************************************************** */

void calculMean(double *mTraj, int *iNbInd, int *iNbTime, int *vClusterAffectation, int *iNbClusters,
		double *mTrajMean){

    int i=0,j=0;
    int nbEachCluster[*iNbClusters * *iNbTime];

/*        Rprintf("Traj =\n");
    printMatrix(mTraj,iNbTime,iNbInd);
    Rprintf("\n\niNbInd=%i iNbTime=%i iNbClusters=%i",*iNbInd,*iNbTime,*iNbClusters);
    Rprintf("\nvClusterAffectation=%i %i %i",vClusterAffectation[0],vClusterAffectation[1],vClusterAffectation[2]);


    Rprintf("\n\nTrajMean =\n");
    printMatrix(mTrajMean,iNbTime,iNbClusters);*/

    // Initialisation de mTrajMean
    for (i=0 ; i < *iNbClusters * *iNbTime ; i++){
	mTrajMean[i] = 0;
	nbEachCluster[i] = 0;
    };

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
/*        Rprintf("\n\nTrajMean (from calculMean)=\n");
	  printMatrix(mTrajMean,iNbTime,iNbClusters);*/
}



void affecteIndiv(double *mTraj, int *iNbInd, int *iNbTime,
		  double *mTrajMean, int *iNbClusters, double *power,
		  int *vClusterAffectation){

    int i=0,j=0 ;
    double best=0;
    double *dist=malloc(sizeof(double));
    *dist=0;
/*    Rprintf("Pre=%p,P=%p, Dist=%f\n",&dist,dist,*dist);
    Rprintf("\nTraj =\n");
    printMatrix(mTraj,iNbTime,iNbInd);

    Rprintf("\n\nTrajMean =\n");
    printMatrix(mTrajMean,iNbTime,iNbClusters);
*/
    for (i=0 ; i<*iNbInd ; i++){
	*dist=0;
	distanceTraj(&mTraj[i * *iNbTime], &mTrajMean[0], iNbTime, power, dist);
//	Rprintf("-- 0 -- %f %f %i %f %f\n",mTraj[i * *iNbTime], mTrajMean[0], *iNbTime, *power, *dist);
	best = *dist;
	//distance(double *x,double *y,int *taille, double *power, double *dist)
	//Rprintf("\nBest=%f \n",best);
	vClusterAffectation[i] = 1 ;

	for(j=2 ; j<*iNbClusters+1 ; j++){
	    distanceTraj(&mTraj[i * *iNbTime], &mTrajMean[(j-1) * *iNbTime], iNbTime, power, dist);
//	    Rprintf("-- j -- %f %f %i %f %f\n",mTraj[i * *iNbTime], mTrajMean[0], *iNbTime, *power, *dist);

	    // R_distance2(&mTraj[i * *iNbTime], &mTrajMean[(j-1) * *iNbTime], iNbTime, distance, power, &dist);
	    // Rprintf("\nDist=%f \n",dist);
	    if(*dist<best){
		best = *dist;
		vClusterAffectation[i] = j ;
	    }else{};
	    // Rprintf("\nBest2=%f \n",best);
	}
    }
}

void kml1(double *mTraj, int *iNbInd, int *iNbTime, int *iNbClusters, int *maxIt,
	  int *distance, double *power, int *vClusterAffectation1, int *convergenceTime){

    int i=0,iter=0;
    int *vClusterAffectation2=malloc(*iNbInd * sizeof(int));
    double *mTrajMean=malloc(*iNbClusters * *iNbTime * sizeof(double));

    for(i = 0; i < *iNbClusters * *iNbTime; i++){mTrajMean[i] = 0.0;};
    for(i = 0; i < *iNbInd; i++){vClusterAffectation2[i] = 0;};

/*    Rprintf("Traj =\n");
    printMatrix(mTraj,iNbTime,iNbInd);
    Rprintf("\n\niNbInd=%i iNbTime=%i iNbClusters=%i maxIt=%i distance=%i",*iNbInd,*iNbTime,*iNbClusters,*maxIt,*distance);
    Rprintf("\nvClusterAffectation1=%i %i %i",vClusterAffectation1[0],vClusterAffectation1[1],vClusterAffectation1[2]);
    Rprintf("\nvClusterAffectation2=%i %i %i",vClusterAffectation2[34],vClusterAffectation2[35],vClusterAffectation2[36]);
    Rprintf("\npower=%f ConvergenceTime=%i \n **************************",*power,*convergenceTime);
*/


    //    printMatrix(mTrajMean,iNbTime,iNbClusters);

//    for(iNbId = 0; iNbId < nbId; iNbId++) vClusterAffectation[iNbId] = 1;
//    Rprintf("\n\nDDD mClustersCenter=");for(int g = 0; g < nbTime*nbClusters; g++){Rprintf("%f ",mClustersCenter[g]);}
    //  Rprintf("\n DDD mAffectation=");for(int g = 0; g < nbId; g++){Rprintf("%i ",vClusterAffectation[g]);}
    for(iter = 0; iter < *maxIt; iter+=2){
//	Rprintf("+++++++++++++ ITER1=%i +++++++++++++\n",iter);
//	printMatrix(mTrajMean,iNbTime,iNbClusters);
	calculMean(mTraj,iNbInd,iNbTime,vClusterAffectation1,iNbClusters,mTrajMean);
//	Rprintf("traj mean\n");
	//printMatrix(mTrajMean,iNbTime,iNbClusters);
	affecteIndiv(mTraj,iNbInd,iNbTime,mTrajMean,iNbClusters,power,vClusterAffectation2);
//	for(int j = 0; j < *iNbInd; j++){Rprintf("%i",vClusterAffectation1[j]);};	Rprintf("\n");
	//for(int j = 0; j < *iNbInd; j++){Rprintf("%i",vClusterAffectation2[j]);};	Rprintf("\n");

	i = 0;
	while(vClusterAffectation1[i]==vClusterAffectation2[i] && i<*iNbInd){i++;};
	if(i == *iNbInd){
	    *convergenceTime = iter + 1;
	    break;
	}else{};


//	Rprintf("+++++++++++++ ITER2=%i +++++++++++++\n",iter);
	//printMatrix(mTrajMean,iNbTime,iNbClusters);
	calculMean(mTraj,iNbInd,iNbTime,vClusterAffectation2,iNbClusters,mTrajMean);
//	Rprintf("traj mean\n");
	//printMatrix(mTrajMean,iNbTime,iNbClusters);
	affecteIndiv(mTraj,iNbInd,iNbTime,mTrajMean,iNbClusters,power,vClusterAffectation1);
//	for(int j = 0; j < *iNbInd; j++){Rprintf("%i",vClusterAffectation1[j]);};	Rprintf("\n");
	//for(int j = 0; j < *iNbInd; j++){Rprintf("%i",vClusterAffectation2[j]);};	Rprintf("\n");

	i = 0;
	while(vClusterAffectation1[i]==vClusterAffectation2[i] && i<*iNbInd){i++;};
	if(i == *iNbInd){
	    *convergenceTime = iter + 2;
	    break;
	}else{};
    }
}




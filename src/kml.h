# include <R.h>
# include <Rdefines.h>
# include <Rmath.h>
# include <stdio.h>
# include <stdlib.h>
# include <float.h>


double euclideanNA(double *x,double *y,int *taille);
static double R_euclidean2(double *x,double *y,int taille);
static double R_manhattan2(double *x,double *y,int taille);
static double R_maximum2(double *x,double *y,int taille);
static double R_canberra2(double *x,double *y,int taille);
static double R_dist_binary2(double *x,double *y,int taille);
static double R_minkowski2(double *x,double *y,int taille,double power);
double manathanNA(double *x,double *y,int *taille);
double manathanNA(double *x,double *y,int *taille);
double maximumNA(double *x,double *y,int *taille);
double minkowskiNA(double *x,double *y,int *taille, double *power);
void distance(double *x, double *y, int *taille, double *power, double *dist);
void R_distance2(double *x, double *y, int *taille, int *method, double *power, double *dist);

void pour(int *t);
void printMatrix(double *mTraj,int *nbCol, int *nbLigne);
//void essai(double *traj, int *ncol, int *nlig);


double meanNA(double *x,int taille);
//double medianNA(double *x,int taille);

void calculMean(double *mTraj, int *iNbInd, int *iNbTime, int *vClusterAffectation, int *iNbClusters,
		double *mTrajMean);
void affecteIndiv(double *mTraj, int *iNbInd, int *iNbTime, double *mTrajMean, int *iNbClusters, int *distance, double *power,
		  int *vClusterAffectation);

/*void kml1(double *mTraj, int *pNbId, int *pNbTime, double *mClustersCenter, int *pNbClusters,
  int *pConvergenceTime, int *vClusterAffectation);*/


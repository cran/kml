# include <R.h>
# include <Rdefines.h>
# include <Rmath.h>
# include <stdio.h>
# include <stdlib.h>
# include <float.h>
//#include <R_ext/Rdynload.h>

//void R_init_kml(DllInfo *info);

// all distance 'Traj' are optimized to work with trajectories
static double manhattanTraj(double *x,double *y,int *taille);
static double euclideanTraj(double *x,double *y,int *taille);
static double minkowskiTraj(double *x,double *y,int *taille,double *power);
static double maximumTraj(double *x,double *y,int *taille);
static double canberraTraj(double *x,double *y,int *taille);
static double binaryTraj(double *x,double *y,int *taille);

// distance that switch to one of the distance'Traj'
void distanceTraj(double *x, double *y, int *taille, double *power, double *dist);

// k-means
void calculMean(double *mTraj, int *iNbInd, int *iNbTime, int *vClusterAffectation, int *iNbClusters,
		double *mTrajMean);

void affecteIndiv(double *mTraj, int *iNbInd, int *iNbTime, double *mTrajMean, int *iNbClusters, double *power,
		  int *vClusterAffectation);

// kml
void kml1(double *mTraj, int *pNbInd, int *pNbTime, int *pNbClusters, int *maxIt,
	  int *distance, double *power, int *vClusterAffectation1, int *convergenceTime);


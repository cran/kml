# include <R.h>
# include <Rdefines.h>
# include <Rmath.h>
# include <stdio.h>
# include <stdlib.h>

double distanceNA(double *x,double *y,int taille);
double manathanNA(double *x,double *y,int taille);
double minkowskiNA(double *x,double *y,int taille, double power);
void kml1(double *mTraj, int *pNbId, int *pNbTime,
	  double *mClustersCenter, int *pNbClusters, int *pConvergenceTime,
	  int *vClusterAffectation);


/* 
 affpar.h: 
*/

#ifndef AFFPAR_H
#define AFFPAR_H

#define    FREE_PTR          (void *)
#define    EPS               1E-15
#define    EPS_SCHWARZ       0.0000001
#define    EPS_X             1E-20
#define    MCLERR           -3201
#define    EPS_DET           0.0000001

#define    OK                1
#define    MA_SCHWARZ        2
#define    DET_0             3

#define    if_EOF_return     if (status < 0) return(status);

#ifdef __cplusplus
extern "C" {
#endif

int affinpar (double, double, int, double *, double *, double *, double *,
              double *, double *, double *);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* AFFPAR_H */

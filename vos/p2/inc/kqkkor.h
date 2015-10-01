/*
 kqkkor.h:  Subroutine kqkkor carries out calculus associated with tiepoint 
 "correlation" in the match mode.  This routine accepts a number of parameters 
 needed for the matching, such as matching patch sizes, the search area, and 
 the requested point accuracy and returns information on the results of 
 matching, such as correlation coefficients before and after LSM fitting, 
 point accuracy, as well as pixel position of the matched point.  
 */

#ifndef KQKKOR_H
#define KQKKOR_H

#define    U                 8     /* ------ Anz unbek d KQK-Ausgl */
#define    LSMACU            0.08

#define    KQK_RAND          2
#define    MAX_SUCH          20
#define    PMK_DIM           5
#define    KQK_DIM           7
#define    PMK_MAX           31
#define    KQK_MAX           39

#define    ITERMAX           12

#define    FEHLER            (-1)
#define    ZUVIEL_ITER       4
#define    AUSERHALB         5
#define    GRMP              7
#define    MINPMK            8
#define    MINKQK            9
#define    MINTXT            10

#define    OK_KQK            21
#define    OK_PMK            22
#define    RAND              2

#define    AFF_MAX           0.05

#define    DEF_SUCHB         10
#define    DEF_PMKDIM         9
#define    DEF_KQKDIM        17
#define    DEF_MINPMK        0.5

#define  if_EOF_return  if (status < 0) return(status);
#define  OK             1
#define  MCLERR1       -2001
#define  EPS1           0.000001
/* ----------------------------------- 
                                   Parameter fuer die Ausgleichung */
#define    EPS_SCHWARZ1      0.00001
#define    EPS_DET1          0.00001
#define    EPS_X1            1E-8

#define    OK                1
#define    MA_SCHWARZ        2
#define    DET_0             3


typedef struct int_matrix
   {
   int     dimz;
   int     dims;
   int     *ptr_m;
   } INT_MATRIX;      

typedef struct korpar
   {
   int      suchfns;
   int      pmkdim;
   int      kqkdim;
   double   affin[6];
   float    lsmacu;
   float    minpmk;
   } KORPAR;      

typedef struct erg_0
   {
   double   dz;
   double   ds;
   double   pmk;
   double   kqk;
   double   mp;
   } ERG_0;      


typedef struct erg
   {
   float    dz;
   float    ds;
   float    pmk;
   float    kqk;
   float    mp;
   } ERG;      

#ifdef __cplusplus
extern "C" {
#endif

void dim_muster_such (KORPAR *, INT_MATRIX *, INT_MATRIX *);
void kqkkor          (KORPAR *, INT_MATRIX *, INT_MATRIX *, ERG *);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* KQKKOR_H */

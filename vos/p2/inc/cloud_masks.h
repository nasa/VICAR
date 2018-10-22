#ifndef CLOUD_MASK
#define CLOUD_MASK

#define CM_CLOUD      1
#define CM_AMBIG      1
#define CM_SNOW       1
#define CM_DESERT     1
#define CM_WARM_CLOUD 1
#define CM_COLD_CLOUD 1
#define CM_ICE        1
#define CM_CIRRUS     1
#define CM_VALID      1

#define CM_NONCLOUD       0
#define CM_NONAMBIG       0
#define CM_NONSNOW        0
#define CM_NONDESERT      0
#define CM_NONWARM_CLOUD  0
#define CM_NONCOLD_CLOUD  0
#define CM_NONICE         0
#define CM_NONCIRRUS      0
#define CM_NONVALID       0

#define CM_GREEN_REF (cm->band_files->green_band->buffer)
#define CM_RED_REF   (cm->band_files->red_band->buffer)
#define CM_NIR_REF   (cm->band_files->nir_band->buffer)
#define CM_SWIR1_REF (cm->band_files->swir1_band->buffer)
#define CM_TIR1_REF  (cm->band_files->tir_band1->buffer)

#define CM_CLOUDMASK     0
#define CM_SNOWMASK      1
#define CM_DESERTMASK    2
#define CM_WARMCLOUDMASK 3
#define CM_COLDCLOUDMASK 4
#define CM_ICEMASK       5
#define CM_CIRRUSMASK    6
#define CM_AMBIGMASK     7
#define CM_VALIDMASK     8

#define CM_FILTER1       10
#define CM_FILTER2       11
#define CM_FILTER3       12
#define CM_FILTER4       13
#define CM_FILTER5       14
#define CM_FILTER6       15
#define CM_FILTER7       16
#define CM_FILTER8       17
#define CM_FILTER9       18
#define CM_FILTER10      19

#define NEIGHBORSIZE 5

#include "ImageUtils.h"

typedef struct
{
   long long int b3_noncloud;
   long long int b6_noncloud;
   long long int snowCnt;
   long long int ndsi_noncloud;
   long long int pass2_revisit;
   long long int b5_noncloud;
   long long int b42ratio_tally;
   long long int b45ratio_tally;
   long long int warmcloud;
   long long int coldcloud;
   long long int scenePixels;
   long long int enterdesert;
   long long int exitdesert;
   long long int snowpresent;
   long long int cloudCnt;

   double filter4Thresh;
   double filter8Thresh;

   int nl, ns;
}ALGORITHM_VARS;

typedef struct
{
   double *ndsi;
   double *bTempComp;
   double *gv;
   double *sv;
   double *rs;
}CM_WORKSPACE;

typedef struct
{
   VICAR_IMAGE *green_band;
   VICAR_IMAGE *red_band;
   VICAR_IMAGE *nir_band;
   VICAR_IMAGE *swir1_band;
   VICAR_IMAGE *tir_band1;
}BAND_FILES;

typedef struct
{
   VICAR_IMAGE *ndsi_file;
   VICAR_IMAGE *bTempComp_file;
   VICAR_IMAGE *gv_file;
   VICAR_IMAGE *sv_file;
   VICAR_IMAGE *rs_file;
}WORKSPACE_FILES;

typedef struct
{
   ALGORITHM_VARS *vars;
   CM_WORKSPACE *ws;
   WORKSPACE_FILES *ws_files;
   BAND_FILES *band_files;

   unsigned char **CMcloud;
   unsigned char **CMsnow;
   unsigned char **CMdesert;
   unsigned char **CMcloud_warm;
   unsigned char **CMcloud_cold;
   unsigned char **ice;
   unsigned char **filter_cirrus;
   unsigned char **ambig;
   unsigned char **valid;

   unsigned char **filter1;
   unsigned char **filter2;
   unsigned char **filter3;
   unsigned char **filter4;
   unsigned char **filter5;
   unsigned char **filter6;
   unsigned char **filter7;
   unsigned char **filter8;
   unsigned char **filter9;
   unsigned char **filter10;

   unsigned char **tambig_warm_mask;
   unsigned char **tambig_cold_mask;

}CLOUD_MASKS;

/******************************************************************************/
// get_Cloud_Masks: creates a CLOUD_MASKS and initializes the buffers inside them
//
// input:
// ======
// + nl
//    - number of lines
// + ns
//    - number of samples
//
// output:
// =======
// + masks
//    - CLOUD_MASKS pointer
//
/******************************************************************************/
CLOUD_MASKS* get_CLOUD_MASKS(int nl, int ns);

/******************************************************************************/
// delete_Cloud_Masks: deletes a CLOUD_MASKS
// 
// input:
// ======
// + masks
//    - CLOUD_MASKS
/******************************************************************************/
void delete_CLOUD_MASKS(CLOUD_MASKS **masks);

/******************************************************************************/
// init_CM_WORKSPACE: creates a CLOUD_MASK workspace
//
// input:
// ======
// + nl
//    - number of lines
// + ns
//    - number of samples
//
// output:
// =======
// + work
//    - CM_WORKSPACE pointer
//
/******************************************************************************/
void init_CM_WORKSPACE(CLOUD_MASKS **masks);

/******************************************************************************/
// delete_CM_WORKSPACE: deletes a CLOUD_MASKS
// 
// input:
// ======
// + work
//    - CM_WORKSPACE
/******************************************************************************/
void delete_CM_WORKSPACE(CLOUD_MASKS **masks);

/******************************************************************************/
// filter1: brightness threshold
//
// inputs:
// =======
// + red_ref (0.6-0.7 micron) reflectance
//    - ETM+ (BAND 3)
//    - ASTER (BAND 2)
// + ns
//    - number of samples
// + valid
//    - valid pixels of the scene
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + ambig
//    - ambig mask
/******************************************************************************/
void filter1(double *red_ref, unsigned char *finalmask, unsigned char* ambig,
             unsigned char *valid, int ns);

/******************************************************************************/
// filter2: snow threshold
//
// inputs:
// =======
// + ndsi
//    - normalized difference snow index
//    - see getNDSI
// + near_ref (0.7-0.9 micron) reflectance
//    - ETM+ (BAND 4)
//    - ASTER (BAND 3)
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + snow
//    - flag to represent if snow is in the image
/******************************************************************************/
void filter2(double* ndsi, double *near_ref, unsigned char *finalmask,
             unsigned char *snow, int ns);

/******************************************************************************/
// filter3: temperature threshold
//
// inputs:
// =======
// + b_temp
//    - brightness temperature (! NOT AT SATELLITE TEMPERATURE !)
//    - ETM+ (BAND 6)
//    - Aster (BAND 13)
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
/******************************************************************************/
void filter3(double *b_temp, unsigned char *finalmask, int ns);

/******************************************************************************/
// filter4: snow and tundra threshold
//
// inputs:
// =======
// + bTempComp
//    - see getBTempComp
// + shortwave_ref (0.7-0.9 micron) reflectance
//    - ETM+ (BAND 5)
//    - ASTER (BAND 4)
// + b_temp
//    - brightness temperature (! NOT AT SATELLITE TEMPERATURE !)
//    - see getBTemp
// + ns
//    - number of samples
// + thresh
//    - threshold for brightness comp filter
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + ambig
//    - ambiguous mask
// + snow
//    - flag to represent if snow is in the image
// + ice
//    - ice mask
/******************************************************************************/
void filter4(double *bTempComp, double *shortwave_ref, double *b_temp, unsigned char *finalmask,
             unsigned char *ambig, unsigned char *ice, int ns, double thresh);

/******************************************************************************/
// filter5: growing vegetation
//
// inputs:
// =======
// + gv
//    - see getGV
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + ambig
//    - ambiguous mask
/******************************************************************************/
void filter5(double *gv, unsigned char *finalmask, unsigned char *ambig, int ns);

/******************************************************************************/
// filter6: senescing vegetation
//
// inputs:
// =======
// + sv
//    - see getSV
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + ambig
//    - ambiguous mask
// + enterdesert
//    - count of pixels entering "desert"
//    - cumulative count - does not start at 0 but at the value passed in
/******************************************************************************/
void filter6(double *sv, unsigned char *finalmask, unsigned char *ambig,
             long long int *enterdesert, int ns);

/******************************************************************************/
// filter7: reflective rocks and soil
//
// inputs:
// =======
// + rs
//    - see getRS
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + CMdesert
//    - desert mask
// + exitdesert
//    - count of desert pixels exiting "desert"
//    - cumulative count - does not start at 0 but at the value passed in
/******************************************************************************/
void filter7(double *rs, unsigned char *finalmask, unsigned char *CMdesert,
             unsigned char *ambig, long long int *exitdesert, int ns);

/******************************************************************************/
// filter8: identifying warm and cold clouds
//
// inputs:
// =======
// + bTempComp
//    - see getBTempComp
// + ns
//    - number of samples
// + thresh
//    - threshold for this filter
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + CMcloud_cold
//    - cold cloud mask
// + CMcloud_warm
//    - warm cloud mask
// + enterdesert
//    - count of desert pixels
//    - cumulative count - does not start at 0 but at the value passed in
/******************************************************************************/
void filter8(double *bTempComp, unsigned char *finalmask, unsigned char *CMcloud_cold,
             unsigned char *CMcloud_warm, int ns, double thresh,
             long long int *coldcloud, long long int *warmcloud);

/******************************************************************************/
// getNDSI: gets the normalized difference snow index
//
// inputs:
// =======
// + green_ref reflectance
//    - ETM+ (BAND 2)
//    - Aster (BAND 1)
// + shortwave_ref reflectance
//    - ETM+ (BAND 5)
//    - Aster (BAND 4)
// + ns
//    - number of samples
//
// outputs:
// ========
// + ndsi
//    - buffer containing ndsi data
/******************************************************************************/
void getNDSI(double *green_ref, double *shortwave_ref, double *ndsi, int ns);

/******************************************************************************/
// getBTempComp: gets the brightness temperature
//
// inputs:
// =======
// + b_temp
//    - brightness temperature
//    - ETM+ (BAND 6)
//    - Aster (BAND 13)
// + shortwave_ref reflectance
//    - ETM+ (BAND 5)
//    - Aster (BAND 4)
// + ns
//    - number of samples
//
// outputs:
// ========
// + bTempComp
//    - buffer containing bTempComp
/******************************************************************************/
void getBTempComp(double *b_temp, double *shortwave_ref, double *bTempComp, int ns);

/******************************************************************************/
// getGV: gets the growing vegetation indicator
//
// inputs:
// =======
// + near_ref reflectance
//    - ETM+ (BAND 4)
//    - Aster (BAND 3)
// + red_ref reflectance
//    - ETM+ (BAND 3)
//    - Aster (BAND 2)
// + ns
//    - number of samples
//
// outputs:
// ========
// + gv
//    - buffer containing growing vegetation data
/******************************************************************************/
void getGV(double *near_ref, double *red_ref, double *gv, int ns);

/******************************************************************************/
// getSV: gets the senescing vegetation indicator
//
// inputs:
// =======
// + near_ref reflectance
//    - ETM+ (BAND 4)
//    - Aster (BAND 3)
// + green_ref reflectance
//    - ETM+ (BAND 2)
//    - Aster (BAND 1)
// + ns
//    - number of samples
//
// outputs:
// ========
// + sv
//    - buffer containing senescing vegetation data
/******************************************************************************/
void getSV(double *near_ref, double *green_ref, double *sv, int ns);

/******************************************************************************/
// getRS: gets the reflective soil indicator
//
// inputs:
// =======
// + near_ref reflectance
//    - ETM+ (BAND 4)
//    - Aster (BAND 3)
// + shortwave_ref reflectance
//    - ETM+ (BAND 5)
//    - Aster (BAND 4)
// + ns
//    - number of samples
//
// outputs:
// ========
// + rs
//    - buffer containing reflective soil data
/******************************************************************************/
void getRS(double *near_ref, double *shortwave_ref, double *rs, int ns);

/******************************************************************************/
// setBandImages: assign images to corresponding bands
//
// inputs:
// =======
// + cm
//    - CLOUD_MASKS struct to assign
// + green
//    - ETM+ (BAND 2)
//    - Aster (BAND 1)
// + red
//    - ETM+ (BAND 3)
//    - Aster (BAND 2)
// + nir
//    - ETM+ (BAND 4)
//    - Aster (BAND 3)
// + swir1
//    - ETM+ (BAND 5)
//    - Aster (BAND 4)
// + tir1
//    - ETM+ (BAND 6)
//    - Aster (BAND 5)
//
// outputs:
// ========
// + cm
//    - BAND_FILES struct in cm is initialized
/******************************************************************************/
void setBandImages(CLOUD_MASKS **cm, VICAR_IMAGE *green, VICAR_IMAGE *red, VICAR_IMAGE *nir,
                   VICAR_IMAGE *swir1, VICAR_IMAGE *tir1);

/******************************************************************************/
// setWorkspaceImages: assign images to corresponding workspace
//
// inputs:
// =======
// + cm
//    - CLOUD_MASKS struct to assign
// + ndsi
//    - Normalized Difference Snow Index image
// + bTempComp
//    - bTempComp image
// + gv
//    - growing vegetation image
// + sv
//    - scenesing vegetation image
// + rs
//    - reflective soil image
//
// outputs:
// ========
// + cm
//    - WORKSPACE_FILES struct in cm is initialized
/******************************************************************************/
void setWorkspaceImages(CLOUD_MASKS **cm, VICAR_IMAGE *ndsi, VICAR_IMAGE *bTempComp,
                        VICAR_IMAGE *gv, VICAR_IMAGE *sv, VICAR_IMAGE *rs);

/******************************************************************************/
// doPass1: perform all the filters of pass1 (setBandImages and setWorkspaceImages
//                                            should be called before performing
//                                            this function)
//
// inputs:
// =======
// + masks
//    - CLOUD_MASKS stuct to perform the pass1 on
//
// outputs:
// ========
// + masks
//    - filter buffers will be set
/******************************************************************************/
void doPass1(CLOUD_MASKS *masks);

/******************************************************************************/
// doPass2: perform all the filters of pass2 (pass1 should be called before performing
//                                            this function)
//
// inputs:
// =======
// + masks
//    - CLOUD_MASKS stuct to perform the pass1 on
//
// outputs:
// ========
// + masks
//    - CMcloud buffers will be set
/******************************************************************************/
void doPass2(CLOUD_MASKS *cm);

#endif

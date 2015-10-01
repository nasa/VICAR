	/*
	 * Public Defines for Photometric Function Package
	 * 
	 * History:
	 *   05jan94 -lwk- initial version
	 *   march '94 	F. Oschuetz : completed
	 */

#ifndef pho_defines
#define pho_defines

typedef void *PHO;	/* ensure that PHO object is private */



  /* general parameters : */

#define phoMAX_FUNC_NAME_LENGTH	22
#define phoMAX_KEYWD_LENGTH 	11
#define phoMAX_PARAM_PER_FUNC   8       /* Maximum number of parameters in
                                           a function */ 

#define phoMAX_ARGUMENTS_PER_FUNC   5   /* Maximum number of arguments in
                                           a function call */



  /* The following parameter is the maximum number of columns allocated
     to parameters in the photometric parameter file -- this is intended
     to be the maximum value that phoMAX_PARAM_PER_FUNC might attain
     in future extensions */

#define phoMAX_PARAM_COLS	15


  /* photometric function names : */

#define phoLAMBERT			"LAMBERT"
#define phoMINNAERT			"MINNAERT"
#define phoIRVINE			"IRVINE"
#define phoSQUE_VEV			"SQUE_VEV"
#define phoVEVERKA			"VEVERKA"
#define phoBURATTI1			"BURATTI1"
#define phoBURATTI2			"BURATTI2"
#define phoBURATTI3			"BURATTI3"
#define phoMOSHER			"MOSHER"
#define	phoLUMME_BOWEL_HG1		"LUMME_BOWEL_HG1"
#define	phoHAPKE_81_LE2			"HAPKE_81_LE2"
#define	phoHAPKE_81_COOK		"HAPKE_81_COOK"
#define	phoHAPKE_86_HG1			"HAPKE_86_HG1"
#define	phoHAPKE_86_HG2			"HAPKE_86_HG2"
#define	phoHAPKE_86_LE2			"HAPKE_86_LE2"
#define	phoHAPKE_HG1_DOM		"HAPKE_HG1_DOM"
#define	phoREGNER_HAPKE_HG1		"REGNER_HAPKE_HG1"
#define phoATMO_CORR_REGNER		"ATMO_CORR_REGNER"

  /* keywords for photometric function parameters : */

#define phoALBEDO	"ALBEDO"
#define phoEXPONENT	"EXPONENT"
#define phoA_VEVERKA	"A_VEVERKA"
#define phoB_VEVERKA	"B_VEVERKA"
#define phoC_VEVERKA	"C_VEVERKA"
#define phoD_VEVERKA	"D_VEVERKA"
#define phoMO_EXP1	"MO_EXP1"
#define phoMO_EXP2	"MO_EXP2"
#define phoE_BURATTI	"E_BURATTI"
#define phoDEN_SOIL	"DEN_SOIL"
#define phoW_SOIL	"W_SOIL"
#define phoHG1_SOIL	"HG1_SOIL"
#define phoHG2_SOIL	"HG2_SOIL"
#define phoHG_ASY_SOIL	"HG_ASY_SOIL"
#define phoLE1_SOIL	"LE1_SOIL"
#define phoLE2_SOIL	"LE2_SOIL"
#define phoH_SHOE	"H_SHOE"
#define	phoB_SHOE	"B_SHOE"
#define	phoH_CBOE	"H_CBOE"
#define	phoB_CBOE	"B_CBOE"
#define	phoTHETA	"THETA"
#define	phoCOOK		"COOK"
#define	phoTAU_ATM	"TAU_ATM"
#define	phoW_ATM	"W_ATM"
#define	phoHG1_ATM	"HG1_ATM"
#define phoIRV_EXP1	"IRV_EXP1"
#define phoIRV_EXP2	"IRV_EXP2"

  /* keywords for compute mode in phoBidiRef: */

#define phoMax_MODE_NAME_LENGTH 6

#define phoVALUE   "value"  /* phoFuncVal[0] = function value    default mode */
#define phoGRAD	   "grad"   /* phoFuncVal[0] = function value 		      */
			    /* phoFuncVal[1] = d phoFuncVal[0] / d cos(i)     */
			    /* phoFuncVal[2] = d phoFuncVal[0] / d cos(e)     */
#define phoDIR_DEV "dirDev" /* phoFuncVal[0] = function value 		      */
			    /* phoFuncVal[1] = d phoFuncVal[0] / d cos(i)     */
			    /* phoFuncVal[2] = d phoFuncVal[0] / d cos(e)     */
			    /* phoFuncVal[3] = d phoFuncVal[0] / d phasang    */
#define phoPAR_DEV "parDev" /* phoFuncVal[0] = function value 		      */
			    /* phoFuncVal[3+i] = d phoFuncVal[0] / d param[i] */
#define phoALL	    "all"   /* phoFuncVal[0] = function value 		      */
			    /* phoFuncVal[1] = d phoFuncVal[0] / d cos(i)     */
			    /* phoFuncVal[2] = d phoFuncVal[0] / d cos(e)     */
			    /* phoFuncVal[3] = d phoFuncVal[0] / d phasang    */
			    /* phoFuncVal[3+i] = d phoFuncVal[0] / d param[i] */



	/* Union and the contained structurs 				     */
	/* for input of the the illumination conditions :		     */


typedef struct 	/* Items valid for any illumination type --> type             */
        {
	int  mode;	 /* Illumination mode (see below )  		      */
			 /* default is false !!and error message will occur!! */

	int  sunshadow;  /* true if the pixel is in the sunshadow, and        */
			 /* false if the pixel is illuminated (default)	      */
	int  viewshadow; /* true if the pixel is in the viewshadow, and       */
			 /* false if the pixel is visible (default)	      */

	}
	PHO_ILLUMTyps;


typedef struct 	/* Cosines of the of illumin. angles  -->cos	   	      */
        {
	PHO_ILLUMTyps  	type;   /* Items that are the same for any            */
				/* illumination type   			      */

       	double inc;	/* Meassured cosine of incident/ellips.-direct. angle */
       	double em;	/* Meassured cosine of emission/ellips.-direct. angle */
       	double phas;	/* Meassured cosine of phase angle		      */

       	double inc_surf;/* Meassured cosine of incident/surface-direct. angle */
       	double em_surf;/* Meassured cosine of emission/surface-direct. angle*/

	}
	PHO_ILLUMDTMCosines;


typedef struct 	/* Illumination and surfaces directions --> direction	      */
        {
	PHO_ILLUMTyps  	type;   /* Items that are the same for any            */
				/* illumination type   			      */

	double inc[3];     /* Meassured incident direction in the body fixed  */
			   /* frame (prime meridian / eqator system)	      */
	double em[3];    /* Meassured emission direction in the body fixed  */
		           /* frame (prime meridian / eqator system)          */
	double ellips[3];  /* Ellipsoid normal direction in the body fixed    */
			   /* frame (prime meridian / eqator system)	      */
	double surf[3];    /* Surface normal direction in the body fixed      */
			   /* frame (prime meridian / eqator system)	      */
	}
	PHO_ILLUMDTMDirects;


typedef union
	{
	int  		mode;	/* Illumination mode (see below )  	      */
				/* default is false !!and error message will  */
				/* occur!!				      */

	PHO_ILLUMTyps  	type;   /* Items that are the same for any            */
				/* illumination type   			      */

	PHO_ILLUMDTMCosines cos;	/* Structure containes the cosine of the      */
				/* incident, emissision and phase angles in   */
				/* relation to the ellipsoid and surface      */
				/* normales  (additional to the Mode and the  */
				/* Shadow).         			      */

	PHO_ILLUMDTMDirects direction; /* Structure containes the Incidence,   */
				/* Emission, Ellipsoid and Suface directions  */
	}
	PHO_ILLUM;





	/* is the pixel in the shadow ? : */

#define illShadow	  1  /* the pixel is in the sun or visibility shadow  */
#define illNoShadow	  0  /* the pixel is illuminated or visible           */





  	/* illMode for input of the the illumination conditions : */
  	/* default is false(=0) !!and error message will occur!!  */


/* for phoBidiRef and phoFunc for an ellipsoid : */

#define illEllCos	   1 	/* illumination conditions given by           */
			  	/* cosine of the incidence, emission and      */
				/* phase angles with respect to the ellipsoid */
				/* (or surface) normale 		      */

#define illEllDir	   3	/* illumination conditions given by the       */
			  	/* incidence, emission and the ellipsoid      */
				/* (or surface) normales.  		      */


/* for phoBidiRef and phoFunc with respect to a DTM : */

#define illDTMDir	   4	/* illumination conditions given by the      */
			  	/* incidence, emission, ellipsoid and        */
				/* surface normales.  			     */

#define illDTMCos 	   2    /* illumination conditions given by          */
			  	/* cosine of the incidence, emission angles  */
				/* with respect to the ellipsoid and surface */
				/* normales and cosine of the phase angle    */




	/* error codes : */


#define phoSUCCESS			1	/* Default success 	    */

#define phoFUNC_CHANGED			2	/* Success:		    */
						/* Photometric function     */
						/* set in the pho_object    */
						/* but function had been    */
						/* set previously and has   */
						/* now been changed         */

#define phoKEYWD_CHANGED		3	/* Success:		    */
						/* Parameter set in the     */
						/* pho_object but parameter */
						/* had been set previously  */
						/* and now has been changed */

#define phoFAILURE			0	/* Failure:		    */
						/* Without a specification  */

#define phoFUNC_NOT_SET			-1001   /* Failure:		    */
						/* Photometric function not */
						/* set in the pho_object    */

#define phoKEYWD_NOT_SET		-1002   /* Failure:		    */
						/* Parameter keyword not    */
						/* set in the pho_object    */

#define phoINVALID_FUNCTION		-1003	/* Failure:		    */
						/* Invalid photometric      */
						/* function name has been   */
						/* passed */

#define phoINVALID_KEYWD		-1004   /* Failure:		    */
						/* Invalid keyword has been */
						/* passed		    */

#define phoFUNC_NOT_SPECIFIED		-1005   /* Failure:		    */
						/* No photometric function  */
						/* specified in the PDF	    */

#define phoKEYWD_NOT_SPECIFIED		-1006   /* Failure:		    */
						/* A parameter of the       */
						/* photometric function was */
						/* not specified by the PDF */

#define phoINV_ILLU_FUNCTION_SET	-1007   /* Failure:		    */
						/* The sets of illumination */
						/* and viewing conditions   */
						/* are invalid for the      */
						/* current photometric      */
						/* function		    */

#define phoILLU_SET_MIX			-1008   /* Failure:		    */
						/* This is an invalid       */
						/* representation of the    */
						/* illumination and viewing */
						/* conditions		    */

#define phoINVALID_ILL_MODE		-1009	/* Failure :		    */
						/* The illMode is invalid   */

#define phoARGUMENT_OUT_OF_RANGE	-1010	/* Failure :		    */
						/* One of the arguments is  */
						/* out of the valid range   */

#define phoCLASS_DOUBLING		-1011   /* Failure:		    */
						/* This class-Id exists in  */
						/* the photometric	    */
						/* parameter file jet	    */

#define phoROW_OUT_OF_RANGE		-1012   /* Failure:		    */
						/* This row number is not   */
						/* in the current interface */
						/* file (parameter or	    */
						/* catalog file) 	    */


  /* miscellaneous : */

#ifndef MAX
#define MAX(a, b)         ((a) > (b) ?  (a) : (b))
#endif

#ifndef MIN
#define MIN(a, b)         ((a) < (b) ?  (a) : (b))
#endif


#define ANG_EPS           1.0e-10

#define EQUAL(x,y) (strcmp(x,y)==0)

#ifndef M_PI
#define M_PI  ((double) 3.14159265358979323846)
#endif

#ifndef PI
#define PI  ((double) 3.14159265358979323846)
#endif

#define COEF_FAC      0.07957747154594767 	/* COEF_FAC = 1.0/(4.0 * M_PI)*/

#define RETURN_SIGN(x)    ( ((x)>0) ? 1 : -1 )

#define RETURN_RADIANS(x) ((double) M_PI * (double) ((x) / 180.0) )
#define RETURN_DEGREES(x) ((double) 180.0 * (x) / (double) M_PI )

#define RADDIR2(dir) ( (double) dir[0] * (double) dir[0] +  (double) dir[1] * (double) dir[1] +  (double) dir[2] * (double) dir[2] )

#define COSPHAS(IncAng,EmAng,AziAng) (cos(RETURN_RADIANS(IncAng)) * cos(RETURN_RADIANS(EmAng)) + sin(RETURN_RADIANS(IncAng)) * sin(RETURN_RADIANS(EmAng)) * cos(RETURN_RADIANS(AziAng))) 

# define DIRCOS(dir1,dir2) ( ( (double) dir1[0] * (double) dir2[0] +  (double) dir1[1] * (double) dir2[1] +  (double) dir1[2] * (double) dir2[2] ) / sqrt ( RADDIR2(dir1) * RADDIR2(dir2) ) )


#endif

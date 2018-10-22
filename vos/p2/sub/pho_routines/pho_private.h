	/*
	 *   PHO_PRIVATE.H
	 *
	 * Photometric Data Object Include File
	 * 
	 * Purpose:
	 *
	 * This is an include file that is used by all routines of the 
	 * 'phoX...' set of photometric function correction software. 
	 * It includes the definition of the data object proper.
	 * The pho_init.h includes the definition of the codes needed 
	 * to process it .
	 * 
	 * These are private include files which are *not* to be included 
	 * in programs or subroutines which themselves call the 'phoX...' 
	 * functions.
	 * 
	 * To add a new function:
	 *  1. Add the function definition to PHO_ROUTINES.COM 
	 *    corresponding to the call in phoFunctions (in PHO_ROUTINES.COM).
	 *  2. Increase parameter phoFUNCTION_COUNT by 1.
	 *  3. Find all arrays with dimension phoFUNCTION_COUNT and
	 *    add an element to each array.
	 *  4. Add the function to the list of '#define's in PHO.H.
	 *  5. Check if parameter phoMAX_PARAM_COUNT increases, and if
	 *    so repeat steps 2 (may need to be increased by more than 1)
	 *    and 3 for this parameter.
	 *  6. Check if parameter phoMAX_PARAM_PER_FUNC (in pho.h and
	 *    pho.fin) increases,
	 *    and if so repeat steps 2 (may need to be increased by more
	 *    than 1) and 3 for this parameter.
	 *
	 * History:
	 *   05jan94 -lwk- initial version
	 */

#include <math.h>
#include "xvmaininc.h"		/* Standard VICAR Include File		    */
#include "pho.h"

		/*
		 * DEFINES of overall structure properties:
		 * Note:  phoMAX_FUNC_NAME_LENGTH and phoMAX_PARAM_PER_FUNC
		 * and phoMAX_KEYWD_LENGTH are defined in pho.h
		 */
#define	phoFUNCTION_COUNT	18	/* Number of phot.functions */
#define phoMAX_PARAM_COUNT	29	/* Number of distinct parameters */


/*************************************************************************/
  /* PHO data object: */

typedef struct  {
	char func_name[phoMAX_FUNC_NAME_LENGTH+1];
	double func_params[phoMAX_PARAM_PER_FUNC];
	char flag_set[phoMAX_PARAM_PER_FUNC];
	char flag_chg[phoMAX_PARAM_PER_FUNC];
	char flag_func_set[1];
	char flag_func_chg[1];
	char flag_mode[phoMax_MODE_NAME_LENGTH+1];
	}
PHO_STRUCT;


/*************************************************************************/
  /*
   * Second, the declarations of subroutine calls to the above-
   * listed supported photometric functions:
   */
void LAMBERT(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
void MINNAERT(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
void VEVERKA(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
void IRVINE(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
/*void LOM_SEEL();*/
void SQUE_VEV();
void BURATTI1();
void BURATTI2();
void BURATTI3();
void MOSHER();
void LUMME_BOWEL_HG1();
void HAPKE_81_LE2();
void HAPKE_81_COOK();
void HAPKE_86_HG1();
void HAPKE_86_HG2();
void HAPKE_86_LE2(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
void HAPKE_HG1_DOM();
void REGNER_HAPKE_HG1();

  /*
   * helping functions : 
   */
void pho_HAPKE86 (double *x, double *a, double *res, int flag);
double pho_hp_rough (double *cosi, double *cose, double cosg, double tbar);
double pho_dg_rough (double *ci, double *ce, double cg, double tbar, 
		     double *di, double *de, double *dg, double *da, int flag);
double pho_d_h_fct (double a, double sqw, double *da, double *dw);




 typedef void (*FTyp)();


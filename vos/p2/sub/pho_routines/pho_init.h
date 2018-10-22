	/*
	 * PHO_INIT.H
	 *
	 * Photometric  Functions Include File
	 *
	 * Purpose:
	 *
	 * This is an include file that is used only by the routines
	 * which have a direct access to the pho_object.
	 * It includes the definitionof coodes needed to get a direct
	 * access to the pho_object.
	 * To add a new function see the description in "pho_private.h
	 * to.
	 * The following comments describe the changings in this file
	 * to add a new function :

/*************************************************************************/
  /*
   * HERE FOLLOW THREE LISTS OF VALID PHOTOMETRIC FUNCTIONS, WHICH
   * MUST BE MAINTAINED IN PARALLEL.  FURTHERMORE, THE DEFINITIONS
   * OF THE FUNCTION NAMES AS '#DEFINE'S ARE FOUND IN PHO.H AND MUST
   * ALSO BE MAINTAINED IN PARALLEL.
   *   
   * First, all the valid, currently supported photometric functions:
   */

char phoFUNC_NAMES[phoFUNCTION_COUNT][phoMAX_FUNC_NAME_LENGTH+1] = {
	phoLAMBERT,
	phoMINNAERT,
	phoIRVINE,
	phoSQUE_VEV,
	phoVEVERKA,
	phoBURATTI1,
	phoBURATTI2,
	phoBURATTI3,
	phoMOSHER,
	phoLUMME_BOWEL_HG1,
	phoHAPKE_81_LE2,
	phoHAPKE_81_COOK,
	phoHAPKE_86_HG1,
	phoHAPKE_86_HG2,
	phoHAPKE_86_LE2,
	phoHAPKE_HG1_DOM,
	phoREGNER_HAPKE_HG1,
	phoATMO_CORR_REGNER
};
/*************************************************************************/
  /*
   * Third, the names of subroutine calls to the above-listed 
   * supported photometric functions:
   */


FTyp	phoFUNC_CALL_NAME[phoFUNCTION_COUNT-1] = {
	LAMBERT,
	MINNAERT,
	IRVINE,
	SQUE_VEV,
	VEVERKA,
	BURATTI1,
	BURATTI2,
	BURATTI3,
	MOSHER,
	LUMME_BOWEL_HG1,
	HAPKE_81_LE2,
	HAPKE_81_COOK,
	HAPKE_86_HG1,
	HAPKE_86_HG2,
	HAPKE_86_LE2,
	HAPKE_HG1_DOM,
	REGNER_HAPKE_HG1
};


/*************************************************************************/
  /* Array specifying number of parameters for each function: */

int phoPARAM_COUNT[phoFUNCTION_COUNT] = { 1, 2, 3, 4, 4, 3, 4, 5, 6, 5, 4, 5, 5, 
					7, 6, 7, 8, 8 };


/*************************************************************************/
  /* 
   * Array of valid parameter codes for the functions:
   * in this array, each function has a separate line, in which the
   * entries refer to the phoPARAM_KEYWORDS array (below), e.g., a 
   * value of 2 denotes the 3rd entry in that array.
   */

int phoFUNC_PARAM_CODES[phoFUNCTION_COUNT][phoMAX_PARAM_PER_FUNC] = {
  0, 0, 0, 0, 0, 0, 0, 0,		/* func.0 = Lambert  		*/
  0, 1, 0, 0, 0, 0, 0, 0,		/* func.1 = Minnaert 		*/
  1,27,28, 0, 0, 0, 0, 0,		/* func.2 = Irvine		*/
  2, 3, 4, 5, 0, 0, 0, 0,		/* func.3 = Sque_Vev		*/ 
  2, 3, 4, 5, 0, 0, 0, 0,		/* func.4 = Veverka  		*/
  0, 3, 8, 0, 0, 0, 0, 0,		/* func.5 = Buratti1		*/
  0, 3, 4, 8, 0, 0, 0, 0,		/* func.6 = Buratti2		*/
  0, 3, 4, 5, 8, 0, 0, 0,		/* func.7 = Buratti3		*/
  2, 3, 4, 5, 6, 7, 0, 0,		/* func.8 = Mosher   		*/
  12,18,11,22,13,0, 0, 0,		/* func.9 = Lumme-Bowel-HG1 	*/
  12,18,16,17,0, 0, 0, 0,		/* func.10= Hapke-81-Le2 	*/
  12,18,16,17,23,0, 0, 0,		/* func.11= Hapke-81-Cook	*/
  12,18,19,22,13,0, 0, 0,		/* func.12= Hapke-86-HG1	*/
  12,18,19,22,13,14,15,0,		/* func.13= Hapke-86-HG2	*/
  12,18,19,22,16,17,0, 0,		/* func.14= Hapke-86-Le2	*/
  12,18,19,22,13,20,21,0,		/* func.15= Hapke-HG1-Dom	*/
  12,18,19,22,13,25,24,26,		/* func.16= Regner-Hapke-HG1	*/
  12,18,19,22,13,25,24,26             /* func.17= Atmospheric_Correct_Regner */
};


/*************************************************************************/
  /*
   * all the valid photometric parameter keywords:
   * For clarity, the sequence number is repeated in a comment on
   * each line, since this index is used by the phoFUNC_PARAM_CODES
   * array (see above)
   */

char phoPARAM_KEYWORDS[phoMAX_PARAM_COUNT][phoMAX_KEYWD_LENGTH+1] = {
	phoALBEDO,	/* keyword #0 */
	phoEXPONENT,	/* keyword #1 */
	phoA_VEVERKA,	/* keyword #2 */
	phoB_VEVERKA,	/* keyword #3 */
	phoC_VEVERKA,	/* keyword #4 */
	phoD_VEVERKA,	/* keyword #5 */
	phoMO_EXP1,	/* keyword #6 */
	phoMO_EXP2,	/* keyword #7 */
	phoE_BURATTI,	/* keyword #8 */
	phoMO_EXP1,	/* keyword #9 */
	phoMO_EXP2,	/* keyword #10 */
	phoDEN_SOIL,	/* keyword #11 */
	phoW_SOIL,	/* keyword #12 */
	phoHG1_SOIL,	/* keyword #13 */
	phoHG2_SOIL,	/* keyword #14 */
	phoHG_ASY_SOIL,	/* keyword #15 */
	phoLE1_SOIL,	/* keyword #16 */
	phoLE2_SOIL,	/* keyword #17 */
	phoH_SHOE,	/* keyword #18 */
	phoB_SHOE,	/* keyword #19 */
	phoH_CBOE,	/* keyword #20 */
	phoB_CBOE,	/* keyword #21 */
	phoTHETA,	/* keyword #22 */
	phoCOOK,	/* keyword #23 */
	phoTAU_ATM,	/* keyword #24 */
	phoW_ATM,	/* keyword #25 */
	phoHG1_ATM,	/* keyword #26 */
	phoIRV_EXP1,	/* keyword #27 */
	phoIRV_EXP2	/* keyword #28 */
};


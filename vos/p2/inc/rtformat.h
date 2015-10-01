/******************************************************************************/
/*                                                                            */
/*  This include file contains parameters which describe the format of the    */
/*  realtime image display.                                                   */
/*                                                                            */
/*  Cognizant Programmer:  Paul Bartholomew                                   */
/*                                                                            */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  03/11/93   N/A   PDB - Officially ported to Unix (didn't actually change  */
/*                      anything, though).                                    */
/*                                                                            */
/******************************************************************************/

#define LUT_SECT 	1			/*LUT section*/
#define PHYSICAL 	1			/*XDDNAME argument*/
#define TARCHARS 	9			/*target characters + 1*/
#define LOC	 	1			/*specify lower left of char*/
#define FONT	 	0			/*font number*/
#define VERT    	0			/*vertical*/
#define HORIZ   	1			/*horizontal*/

#define SCREEN_512	1			/*screen size*/
#define SCREEN_1024	2
#define SCREEN_480	3

extern int FRWID;
extern int FRHIT;
extern int GRTOP;
extern int GRBOT;
extern int GRLEFT;
extern int GRRIT;
extern int GRORT;
extern int IMLEFT;
extern int HILEFT;
extern int HITOP;
extern int CAP_LIN1;
extern int CAP_LIN2;
extern int CAP_LIN3;
extern int MIPL_LIN;
extern int MIPL_COL;
extern int SC_LIN;
extern int SC_COL;
extern int RAW_LIN;
extern int RAW_COL;
extern int PGM_LIN;
extern int PGM_COL;
extern int TGT_LIN;
extern int TGT_COL;
extern int PNO_LIN;
extern int PNO_COL;
extern int ST1_LIN;
extern int ST1_COL;
extern int ST2_LIN;
extern int ST2_COL;
extern int ST3_LIN;
extern int ST3_COL;
extern int ZOOM_LIN;
extern int ZOOM_COL;
extern int DCS_LIN;
extern int DCS_COL;
extern int DATE_LIN;
extern int DATE_COL;
extern int FLT_LIN;
extern int FLT_COL;
extern int PWS_LIN;
extern int PWS_COL;

extern int IMTOP;
extern int GRSPAC;
extern int GRSQR;
extern int IMBORD;
extern int PCSIZ;
extern int PCLEFT;
extern int PCTOP;
extern int PCRIT;
extern int PCBOT;
extern int PCSTIK;
extern int PCLTIK;
extern int IMWID;
extern int IMRIT;
extern int IMTIK;
extern int HIBORD;
extern int HITIK;
extern int HISEP;
extern int HIHIT;
extern int SCALE;
extern int OFFSET;
extern int HITOP2;
extern int HIX2;
extern int HIX3;
extern int HIX4;
extern int HIX5;
extern int CHAR_HT1;
extern int CHAR_HT2;
extern int CAP_COL1;
extern int CAP_COL2;
extern int CAP_COL2A;
extern int CAP_COL3;
extern int CAP_COL3A;
extern int CAP_COL4;
extern int CAP_COL4A;
extern int HLAB_DCOL;
extern int HLAB_DLIN;

extern float CHAR_SC1, CHAR_SC2;

extern int HDR_ERS_LIN1;
extern int HDR_ERS_LIN2;
extern int HDR_ERS_COL1;
extern int HDR_ERS_COL2;
extern int SHD_ERS_LIN1;
extern int SHD_ERS_LIN2;
extern int SHD_ERS_COL1;
extern int SHD_ERS_COL2;
extern int CAP_ERS_LIN1;
extern int CAP_ERS_LIN2;
extern int CAP_ERS_COL1;
extern int CAP_ERS_COL2;
extern int ENH_ERS_LIN1;
extern int ENH_ERS_LIN2;
extern int ENH_ERS_COL1;
extern int ENH_ERS_COL2;
extern int STR_ERS_LIN1;
extern int STR_ERS_LIN2;
extern int STR_ERS_COL1;
extern int STR_ERS_COL2;
extern int ZM_ERS_LIN1;
extern int ZM_ERS_LIN2;
extern int ZM_ERS_COL1;
extern int ZM_ERS_COL2;

extern int IMAGE_PLN;			/*image plane*/
extern int GRAPH_PLN;			/*graphics plane*/
extern int VECTOR_PLN;			/*image plane with vector capability*/
extern int screen;			/*screen size*/
extern char zoom_mode[7];		/*permanent hw zoom*/

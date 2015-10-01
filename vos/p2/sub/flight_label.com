$!****************************************************************************
$!
$! Build proc for MIPL module flight_label
$! VPACK Version 1.9, Monday, December 07, 2009, 16:17:55
$!
$! Execute by entering:		$ @flight_label
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module flight_label ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to flight_label.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("flight_label.imake") .nes. ""
$   then
$      vimake flight_label
$      purge flight_label.bld
$   else
$      if F$SEARCH("flight_label.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake flight_label
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @flight_label.bld "STD"
$   else
$      @flight_label.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create flight_label.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack flight_label.com -mixed -
	-s flight_label.c -
	-i flight_label.imake -
	-t tflight_label.c tflight_label.imake tflight_label.pdf -
	   tstflight_label.pdf -
	-o flight_label.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create flight_label.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xvmaininc.h"   
#include <zvproto.h>
#define OPEN 0x0001			/* File open? */
#define MAX  50				/* Maximum number of tasks in label */
#define ITC_COMPRSSD 1
#define LOS_COMPRSSD 2
#define NON_COMPRSSD 3
#define OLD_STYLE_LABEL    4
#define PHASEII 2
/*******************************************************************************

Purpose:	This subroutine lists label information of flight images
		in the standard format proposed by Gary Yagi on June 6,
		1989 (MSD: 384-89-165).
	
Written:		June 1, 1990

Revisions:		August 20, 1991		Searches for REDRTAPE and 
						REDRFILE if file is of BYTE
						format.
			March 21, 1991 SP       Allowing up to 12 chars for
						EDRTAPE.
			December 7, 1990  	"ENTRPY" changed to "ENTROPY"
						in the output display.
			November 6, 1990	Task name selection made 
						dependent on SENSOR label item
						value.
			January  10, 1990	Added ITC Compression type display
Language:		C
Written by:		Justin McNeill
Cognizant programmer: 	Justin McNeill

Ported  to UNIX:        Wen-Piao  Lee
                        May 6, 1992

*******************************************************************************/
struct  node { char * str;  /* LINK list for output buffer */
	       struct node * next;
	     };
typedef struct node NODE;
struct queue {NODE* head;
              NODE* tail;};
typedef struct queue QUEUE;
void enqueue(char* str, QUEUE l);

NODE *flight_label( unit )
int unit;		/* Unit number of file whose label is listed	      */
{
 static char  asterisks[]="*******************";    /* Array of asterisks   */
 static char  lat_rad[5];
 static int   n[60] ;   /* June, 1992  by W.P. Lee                      */
 char    fltr[8],	/* Array of possible filters			*/
	format[12],	/* Input file data format			*/
	gain[8],	/* Array of possible gains			*/
	ir[35][35],	/* 2D array of strings to store real values     */
/*	lat_rad[5], */	/* String to hold respective keywords 		*/
	line[100],	/* Display string				*/
	lineb[70],	/* Display string				*/
	nv[60][35],	/* 2D array of strings to store integer values  */
	rate[15],	/* Rate code string for output			*/
	task[15],	/* Task name for which flight_label works	*/
	tasks[MAX][15],	/* Array of task names				*/
	v[35][35],	/* 2D array of strings to store label items	*/
        STARn[6];
int	flags,		/* Flag to determine if file is open		*/
	instances[MAX],	/* Array of instances				*/
	nhist,		/* Number of tasks in VICAR label		*/
	sta[60],	/* Array of VICAR command error messages	*/
	status,		/* VICAR command error flag			*/
	x,y,		/* Loop count variables				*/
        galsosdone,     /* 1 if input is a galsosed image.              */
	i,		/* Index 					*/
  	i_sensor,	/* Index to sensor (in array)			*/
   	i_picno,
  	i_tlmfmt,
  	i_target,
    	i_fibe,
     	i_tca , 
     	i_pa,
   	i_boom,
     	i_cal,
     	i_ubwc,
    	i_dc,
     	i_blm ,
      	i_so,
  	i_comp,
	i_qm ,
 	i_zz,
 	i_huff,
   	i_edrtape,
  	i_filter,
 	i_partition,i_rim,i_mod91,i_mod10,i_mod8,
	i_scetyear,i_scetday, i_scethour, i_scetmin, i_scetsec,i_scetmsec,
    	i_gain,
    	i_rate,
        i_tpplne,
 	i_qstep,
	i_roi,
	i_tw,
    	i_edrfile,
   	i_exp, 
	i_tbppxl, 
	i_entropy,
   	i_ina,
 	i_twist,
 	i_sunaz,
  	i_hscl,
   	i_ema,
  	i_cone,
 	i_smraz,
 	i_smear,
  	i_vscl,
   	i_pha,
   	i_ra,
  	i_scaz,
   	i_latrad,
 	i_plrange,
     	i_hra,
    	i_dec,
   	i_noraz,
    	i_lon,
 	i_slrange,
     	i_iof,
     	i_solrange,
    	i_cnv,
 	i_compratio,i_max_comp,i_min_comp,
        i_nstars,i_star1, 
        i_seqno,
        i_ict_despike_t,
        i_sub_solar_lat, i_sub_solar_long, 
        i_sub_spacecraft_lat,i_sub_spacecraft_long,	
	type,		/* type = LOS_COMPRSSD, ITC_COMPRSSD, NON_CMPRSSD, OLD_STYLE_LABEL*/
        phase;          /*if phaseII then phase == 2*/
float	r[35];	 	/* Array of real values to store label items  	*/
QUEUE list;
list.head = (NODE *) malloc(sizeof(NODE));
list.tail = (NODE *) malloc(sizeof(NODE));
list.head->next =NULL;
list.tail->next =NULL;
phase = 0;
galsosdone=0;
sta[0] = zvget(unit,"FLAGS",&flags, NULL);
if((flags & OPEN)==0)
  sta[0] = zvopen(unit,"OP","READ","OPEN_ACT","SA", NULL);

sta[0] = zvget(unit,"FORMAT",format, NULL);  /* Get input file data format  */

strcpy(gain,"(****)");
strcpy(fltr,"(****)");
strncpy(rate,asterisks,13);

nhist = MAX;
strcpy( task,"NONE" );
status = zlhinfo(unit,tasks[0],instances,&nhist,"ULEN",15,"ERR_ACT","SA", NULL);
for(x=0;x<nhist;x++)
 {
   if (!strncmp(tasks[x],"GALSOS",6)) galsosdone=1;
   sta[0] = zlget(unit,"HISTORY","SENSOR",v[0],"HIST",tasks[x], NULL);
   if( sta[0]==1 && strcmp(v[0],"SSI")==0 ) strcpy( task, tasks[x] );
   }

if( strcmp(task,"NONE")==0 )
 {
   zvmessage("NON-SSI IMAGE","");
   zvmessage(" - SSI label display aborted","");
   return(NULL);
 }
/* 	Get compression type .. to determine label layout later */
type = NON_COMPRSSD;
sta[0] = zlget(unit,"HISTORY","ENCODING_TYPE",v[0], "HIST", task, NULL);
if ( sta[0] == 1 )  { 
	if ( strcmp( v[0], "HUFFMAN ") == 0)
		type = LOS_COMPRSSD;
	if ( strcmp( v[0], "INTEGER COSINE TRANSFORM ") == 0 )
		type = ITC_COMPRSSD;
}
else {
	type = OLD_STYLE_LABEL;
}


/*	Get all string values for label list		*/
i=0;
sta[i] = zlget(unit,"HISTORY","SENSOR", v[i],"HIST",task, NULL); i_sensor = i; i++;
sta[i] = zlget(unit,"HISTORY","PICNO",  v[i],"HIST",task, NULL); i_picno = i; i++;
sta[i] = zlget(unit,"HISTORY","TLMFMT", v[i],"HIST",task, NULL); i_tlmfmt = i; i++;
sta[i] = zlget(unit,"HISTORY","TARGET", v[i],"HIST",task, NULL); i_target = i; i++;
sta[i] = zlget(unit,"HISTORY","TCA",    v[i],"HIST",task, NULL); i_tca  = i; i++; 
sta[i] = zlget(unit,"HISTORY","PA",     v[i],"HIST",task, NULL); i_pa = i; i++;
sta[i] = zlget(unit,"HISTORY","BOOM",   v[i],"HIST",task, NULL); i_boom = i; i++;
sta[i] = zlget(unit,"HISTORY","CAL",    v[i],"HIST",task, NULL);
if (sta[i] != 1 && galsosdone) sta[i] = zlget(unit,"HISTORY","CAL",    v[i],"HIST","GALSOS", NULL);
i_cal = i; i++;
sta[i] = zlget(unit,"HISTORY","UBWC",   v[i],"HIST",task, NULL); i_ubwc  = i; i++;
sta[i] = zlget(unit,"HISTORY","DC",     v[i],"HIST",task, NULL);
if (sta[i] != 1 && galsosdone) sta[i] = zlget(unit,"HISTORY","DC",     v[i],"HIST","GALSOS", NULL);
i_dc = i; i++;
sta[i] = zlget(unit,"HISTORY","BLM",    v[i],"HIST",task, NULL);
if (sta[i] != 1 && galsosdone) sta[i] = zlget(unit,"HISTORY","BLM",    v[i],"HIST","GALSOS", NULL);
i_blm  = i; i++;
sta[i] = zlget(unit,"HISTORY","SO",     v[i],"HIST",task, NULL);
if (sta[i] != 1 && galsosdone) sta[i] = zlget(unit,"HISTORY","SO",     v[i],"HIST","GALSOS", NULL);
i_so = i; i++;
sta[i] = zlget(unit,"HISTORY","QUANTIZATION_MATRIX_NAME", v[i],"HIST",task,NULL); i_qm =i; i++;
sta[i] = zlget(unit,"HISTORY","ZIGZAG_PATTERN", v[i],"HIST",task,NULL); i_zz = i; i++;
sta[i] = zlget(unit,"HISTORY","HUFFMAN_TABLE_NAME", v[i],"HIST",task,NULL); i_huff = i; i++;
if ( type == OLD_STYLE_LABEL ) {
	sta[i] = zlget(unit,"HISTORY","BARC",   v[i],"HIST",task, NULL); i_comp = i; i++;
	sta[i] = zlget(unit,"HISTORY","FIBE", v[i],"HIST",task, NULL); i_fibe = i; i++; 
	}
else {
	sta[i] = zlget(unit,"HISTORY","ENCODING_TYPE", v[i],"HIST",task, NULL); i_comp = i; i++;
	sta[i] = zlget(unit,"HISTORY","MOFIBE", v[i],"HIST",task, NULL); i_fibe = i; i++;	
	}
if( strcmp(format,"HALF")==0 ) {
  	sta[i] = zlget(unit,"HISTORY","EDRTAPE",v[i],"HIST",task, NULL); i_edrtape = i; i++;
  	}
else {
  	sta[i] = zlget(unit,"HISTORY","REDRTAPE",v[i],"HIST",task, NULL); i_edrtape = i; i++;
  	}


for(x=0;x<i;x++) {	/* Check status values to insure error free zlget's   */
	if(sta[x]!=1) { /* if keyword is not found, fill value with asterisks */

		if(sta[x]==(-38)) 
			strcpy(v[x],asterisks);
		else	/* If other error exists, print "ERR'error number'"   */
			sprintf(v[x],"ERR%d",sta[x]); 
	}
	else {
	    if ((x == i_comp)) {
		if ( strcmp( v[x], "INTEGER COSINE TRANSFORM ") == 0)
			strcpy(v[x], "ICT" );
		if ( strcmp( v[x], "HUFFMAN ") == 0)
			strcpy( v[x], "HUF" );
		if ( strcmp( v[x], "BARC RATE CONTROL ") == 0)
			strcpy( v[x], "RC" );
		if ( strcmp( v[x], "BARC INFORMATION PRESERVING ") == 0)
			strcpy( v[x], "IC" );
	    }
	}
}

			
/*	Get all integer values for label listing	*/
i = 0;
 sta[i] = zlget(unit,"HISTORY","FILTER", 	(char*)&n[i],"HIST",task, NULL); i_filter  = i; i++;
 sta[i] = zlget(unit,"HISTORY","PARTITION", 	(char*)&n[i],"HIST",task, NULL); i_partition = i; if (sta[i]==1) phase = PHASEII; i++;
sta[i] = zlget(unit,"HISTORY","RIM",    	(char*)&n[i],"HIST",task, NULL); i_rim = i; i++;
sta[i] = zlget(unit,"HISTORY","MOD91",  	(char*)&n[i],"HIST",task, NULL); i_mod91 = i; i++;
sta[i] = zlget(unit,"HISTORY","MOD10",  	(char*)&n[i],"HIST",task, NULL); i_mod10 = i; i++;
sta[i] = zlget(unit,"HISTORY","MOD8",   	(char*)&n[i],"HIST",task, NULL); i_mod8 = i; i++;
sta[i] = zlget(unit,"HISTORY","SCETYEAR",	(char*)&n[i],"HIST",task, NULL); i_scetyear  = i; i++;
sta[i] = zlget(unit,"HISTORY","SCETDAY", 	(char*)&n[i],"HIST",task, NULL); i_scetday = i; i++; 
sta[i] = zlget(unit,"HISTORY","SCETHOUR",	(char*)&n[i],"HIST",task, NULL); i_scethour = i; i++;
sta[i] = zlget(unit,"HISTORY","SCETMIN", 	(char*)&n[i],"HIST",task, NULL); i_scetmin= i; i++;
sta[i] = zlget(unit,"HISTORY","SCETSEC", 	(char*)&n[i],"HIST",task, NULL); i_scetsec = i; i++;
sta[i] = zlget(unit,"HISTORY","SCETMSEC", 	(char*)&n[i],"HIST",task, NULL); i_scetmsec = i; i++;
sta[i] = zlget(unit,"HISTORY","GAIN",     	(char*)&n[i],"HIST",task, NULL); i_gain = i; i++;
sta[i] = zlget(unit,"HISTORY","RATE",     	(char*)&n[i],"HIST",task, NULL); i_rate = i; i++;
sta[i] = zlget(unit,"HISTORY","QUANTIZATION_STEP_SIZE", (char*)&n[i],"HIST",task, NULL); i_qstep = i; i++;
sta[i] = zlget(unit,"HISTORY","CUT_OUT_WINDOW", (char*)&n[i],"HIST", task, 
		"ELEMENT", 1, NULL);i_roi = i; i++;
sta[i] = zlget(unit,"HISTORY","CUT_OUT_WINDOW", (char*)&n[i],"HIST", task, 
		"ELEMENT", 2, NULL); i++;
sta[i] = zlget(unit,"HISTORY","CUT_OUT_WINDOW", (char*)&n[i],"HIST", task, 
		"ELEMENT", 3, NULL); i++;
sta[i] = zlget(unit,"HISTORY","CUT_OUT_WINDOW", (char*)&n[i],"HIST", task, 
		"ELEMENT", 4, NULL); i++;
sta[i] = zlget(unit,"HISTORY","TRUTH_WINDOW",   (char*)&n[i],"HIST", task, 
		"ELEMENT", 1, NULL);i_tw = i; i++;
sta[i] = zlget(unit,"HISTORY","TRUTH_WINDOW",   (char*)&n[i],"HIST", task, 
		"ELEMENT", 2, NULL); i++;
sta[i] = zlget(unit,"HISTORY","TRUTH_WINDOW",   (char*)&n[i],"HIST", task, 
		"ELEMENT", 3, NULL); i++;
sta[i] = zlget(unit,"HISTORY","TRUTH_WINDOW",   (char*)&n[i],"HIST", task, 
		"ELEMENT", 4, NULL); i++;
if( strcmp(format,"HALF")==0 ) {
   sta[i] = zlget(unit,"HISTORY","EDRFILE", (char*)&n[i],"HIST",task, NULL); i_edrfile = i; i++;
}
else {
   sta[i] = zlget(unit,"HISTORY","REDRFILE",(char*)&n[i],"HIST",task, NULL); i_edrfile = i; i++;
}
sta[i] = zlget(unit,"HISTORY","SEQNO",(char*)&n[i],"HIST",task,NULL);i_seqno=i; i++;
sta[i] = zlget(unit,"HISTORY","ICT_DESPIKE_THRESHOLD",(char*)&n[i],"HIST",task,NULL);i_ict_despike_t=i; i++;

sta[i] = zlget(unit,"HISTORY","NSTARS",(char*)&n[i],"HIST",task,NULL);i_nstars=i; i++;
i_star1=i;
strcpy(STARn,"STAR ");
for (x=1; x<= n[i_nstars]; x++)
  {
  STARn[4]=(char) (48 + x);
   for ( y=1; y<=4; y++){         

           sta[i] = zlget(unit,"HISTORY",STARn,(char*)&n[i],"HIST",task,"ELEMENT",y,NULL);
           i++;
			}
}
                           
/*n[i_scetyear] %= 100;*/ 		/* Get year of millenium */

for(x=0;x<i;x++) {	/* Check status values to insure error free zlget     */
	if(sta[x]!=1) 	{/* If keyword is not found, fill value with asterisks */
		if(sta[x]==(-38))
			strcpy(nv[x],asterisks);
		else   	/* If other error exists, print "ERR'error number'"   */
			sprintf(nv[x],"ERR%d",sta[x]);
		if ( x==i_partition)
			sprintf(nv[x], "*" );
	}
	else {	/* Perform special formatting for scet data and tpplne        */
		if((x>=i_scetyear && x<=i_scetmsec))
			if((x==i_scetday) || (x==i_scetmsec))
				sprintf(nv[x],"%03d",n[x]);
			else if (x==i_scetyear)
				sprintf(nv[x],"%04d",n[x]);
                        else
                                sprintf(nv[x],"%02d",n[x]);

		else
			sprintf(nv[x],"%d",n[x]);
	}
}
					
/* Get all real values for label listing 		*/
if(strcmp(v[i_target],"RING")==0)	/* If TARGET is ring plane, radians is used */
	strcpy(lat_rad,"RAD");	/* as a measure instead of latitude.	    */
else
	strcpy(lat_rad,"LAT");

i=0;
sta[i] = zlget(unit,"HISTORY","EXP",    (char*)&r[i],"HIST",task, NULL);  i_exp = i; i++; 
sta[i] = zlget(unit,"HISTORY","TBPPXL", (char*)&r[i],"HIST",task, NULL);  i_tbppxl = i; i++; 
sta[i] = zlget(unit,"HISTORY","INA",    (char*)&r[i], "HIST",task, NULL); i_ina = i; i++;
sta[i] = zlget(unit,"HISTORY","TWIST",  (char*)&r[i], "HIST",task, NULL); i_twist = i; i++;
sta[i] = zlget(unit,"HISTORY","SUNAZ",  (char*)&r[i], "HIST",task, NULL); i_sunaz = i; i++;
sta[i] = zlget(unit,"HISTORY","HSCL",   (char*)&r[i], "HIST",task, NULL); i_hscl = i; i++;
sta[i] = zlget(unit,"HISTORY","EMA",    (char*)&r[i], "HIST",task, NULL); i_ema = i; i++;
sta[i] = zlget(unit,"HISTORY","CONE",   (char*)&r[i], "HIST",task, NULL); i_cone = i; i++;
sta[i] = zlget(unit,"HISTORY","SMRAZ",  (char*)&r[i], "HIST",task, NULL); i_smraz = i; i++;
sta[i] = zlget(unit,"HISTORY","SMEAR",  (char*)&r[i],"HIST",task, NULL); i_smear = i; i++;
sta[i] = zlget(unit,"HISTORY","VSCL",   (char*)&r[i],"HIST",task, NULL); i_vscl = i; i++;
sta[i] = zlget(unit,"HISTORY","PHA",    (char*)&r[i],"HIST",task, NULL); i_pha = i; i++;
sta[i] = zlget(unit,"HISTORY","RA",     (char*)&r[i],"HIST",task, NULL); i_ra = i; i++;
sta[i] = zlget(unit,"HISTORY","SCAZ",   (char*)&r[i],"HIST",task, NULL); i_scaz = i; i++;
sta[i] = zlget(unit,"HISTORY",lat_rad,  (char*)&r[i],"HIST",task, NULL); i_latrad = i; i++;
sta[i] = zlget(unit,"HISTORY","PLRANGE",(char*)&r[i],"HIST",task, NULL); i_plrange = i; i++;
sta[i] = zlget(unit,"HISTORY","HRA",    (char*)&r[i],"HIST",task, NULL); i_hra = i; i++;
sta[i] = zlget(unit,"HISTORY","DEC",    (char*)&r[i],"HIST",task, NULL); i_dec = i; i++;
sta[i] = zlget(unit,"HISTORY","NORAZ",  (char*)&r[i],"HIST",task, NULL); i_noraz = i; i++;
sta[i] = zlget(unit,"HISTORY","LON",    (char*)&r[i],"HIST",task, NULL);
if (sta[i] != 1) sta[i] = zlget(unit,"HISTORY","LOG",    (char*)&r[i],"HIST",task, NULL); /*try old style */
i_lon = i; i++;
sta[i] = zlget(unit,"HISTORY","SLRANGE",(char*)&r[i],"HIST",task, NULL); i_slrange = i; i++;
sta[i] = zlget(unit,"HISTORY","IOF",    (char*)&r[i],"HIST",task, NULL); i_iof = i; i++;
sta[i] = zlget(unit,"HISTORY","SOLRANGE",(char*)&r[i],"HIST",task, NULL);i_solrange = i; i++;
sta[i] = zlget(unit,"HISTORY","CNV",    (char*)&r[i],"HIST",task, NULL); i_cnv = i; i++;
sta[i] = zlget(unit,"HISTORY","COMPRESSION_RATIO", (char*)&r[i],"HIST",task, NULL); i_compratio = i; i++;
sta[i] = zlget(unit,"HISTORY","MAXIMUM_COMPRESSION_RATIO", (char*)&r[i],"HIST",task, NULL); i_max_comp = i; i++;
sta[i] = zlget(unit,"HISTORY","MINIMUM_COMPRESSION_RATIO", (char*)&r[i],"HIST",task, NULL); i_min_comp = i; i++;
sta[i] = zlget(unit,"HISTORY","TPPLNE", (char*)&r[i],"HIST",task, NULL); i_tpplne = i; i++;

sta[i] = zlget(unit,"HISTORY","ENTRPY",(char*)&r[i],"HIST",task, NULL); /*try old style */
if (sta[i] != 1) /* new style */
	sta[i] = zlget(unit,"HISTORY","ENTROPY",(char*)&r[i],"HIST",task, NULL);  i_entropy = i; i++;

sta[i]=zlget(unit,"HISTORY","SUB_SOLAR_LATITUDE",(char*)&r[i],"HIST",task,NULL); i_sub_solar_lat=i; i++;
sta[i]=zlget(unit,"HISTORY","SUB_SOLAR_LONGITUDE",(char*)&r[i],"HIST",task,NULL); i_sub_solar_long=i; i++;
sta[i]=zlget(unit,"HISTORY","SUB_SPACECRAFT_LATITUDE",(char*)&r[i],"HIST",task,NULL); i_sub_spacecraft_lat=i; i++;
sta[i]=zlget(unit,"HISTORY","SUB_SPACECRAFT_LONGITUDE",(char*)&r[i],"HIST",task,NULL); i_sub_spacecraft_long=i; i++;


for(x=0;x<i;x++) {	/* Test for error in retrieving keywords	      */
	if(sta[x]!=1)	/* If error, determine if keyword cannot be found.    */
		if(sta[x]==(-38))		 /* If keyword is not found,  */
			strcpy(ir[x],asterisks); /* load asterisks into value */
		else	/* Otherwise, load error number into value	      */
			sprintf(ir[x],"ERR%d",sta[x]); 
	else	/* If no error exists, convert real values to strings */
	
		if (x == i_exp) 	
			sprintf(ir[x],"%8.2f",r[x]);
			
		else if ((x == i_tbppxl) || (x == i_entropy))
			sprintf(ir[x],"%f",r[x]);
		else if ( x == i_tpplne )
			sprintf(ir[x], "%3.0f", r[x]);	
		else if (x == i_smear)
			sprintf(ir[x],"%5.2f",r[x]);
			
		else if ((x == i_hscl)
		 	|| (x == i_vscl )
		 	|| (x == i_plrange )
		 	|| (x == i_slrange )
		 	|| (x == i_iof )
		 	|| (x == i_solrange )
		 	|| (x == i_cnv))
			sprintf(ir[x],"%7.4e",r[x]);
			
		else 
			sprintf(ir[x],"%6.2f",r[x]);
}

if(strcmp(nv[i_filter],asterisks)!=0) 			/* Check filter color */
	switch(n[i_filter]){
	case 0:	strcpy(fltr,"(CLR)");
		break;
	case 1:	strcpy(fltr,"(GRN)");
		break;
	case 2:	strcpy(fltr,"(RED)");
		break;
	case 3:	strcpy(fltr,"(VLT)");
		break;
	case 4:	strcpy(fltr,"(756)");
		break;
	case 5:	strcpy(fltr,"(968)");
		break;
	case 6:	strcpy(fltr,"(727)");
		break;
	case 7:	strcpy(fltr,"(889)");
		break;}

if(strcmp(nv[i_gain],asterisks)!=0) 		/* Determine value of gain */
	switch(n[i_gain]){
	case 1:	strcpy(gain,"(400K)");
		break;
	case 2:	strcpy(gain,"(100K)");
		break;
	case 3: strcpy(gain,"(40K)");
		break;
	case 4:	strcpy(gain,"(10K)");
		break;}

if(strcmp(nv[i_rate],asterisks)!=0) 		/* Determine frame rate code */
	switch(n[i_rate]){
	case 1:	strcpy(rate,"2 1/3");
		break;
	case 2:	strcpy(rate,"8 2/3");
		break;
	case 3:	strcpy(rate,"30 1/3");
		break;
	case 4:	strcpy(rate,"60 2/3");
		break;
        case 5: strcpy(rate,"15 1/6");
                break;}
     
if(strcmp(v[i_boom],asterisks)!=0) {			/* Determine boom presence */
	if(strcmp(v[i_boom],"P")==0)
		strcpy(v[i_boom],"POSSIBLE");
	else
		if(strcmp(v[i_boom],"N")==0)
			strcpy(v[i_boom],"NO");
		else
			if(strcmp(v[i_boom],"Y")==0)
				strcpy(v[i_boom],"YES");
}
				
				

/*	FIRST LINE			*/
sprintf(line,
        "GLL/%-5.3sPICNO=%-12.7sFILTER=%.1s%-7.6sTLMFMT=%-8.3sTARGET=%-10.8s",
        v[i_sensor],v[i_picno],nv[i_filter],fltr,v[i_tlmfmt],v[i_target]);

zvmessage(line, "");
enqueue(line,list);

/*	SECOND LINE			*/
if ( phase == PHASEII ){ 
if(strcmp(nv[i_rim],asterisks)!=0)
	sprintf(line,"RIM=%1s/%08d:%02d:%d:%-1d      EXP=%-11.8sMOFIBE=%-8.6sTCA=%.13s",
		nv[i_partition],n[i_rim],n[i_mod91],n[i_mod10],
		n[i_mod8],ir[i_exp],v[i_fibe],v[i_tca]);
else
	sprintf(line,"RIM=*/********:**:*:*      EXP=%-11.8sMOFIBE=%-8.6sTCA=%.13s",
		ir[i_exp],v[i_fibe],v[i_tca]);
}
else
  {if(strcmp(nv[i_rim],asterisks)!=0)
	sprintf(line,"RIM=%1s/%08d:%02d:%d:%-1d      EXP=%-11.8sFIBE=%-10.6sTCA=%.13s",
		nv[i_partition],n[i_rim],n[i_mod91],n[i_mod10],
		n[i_mod8],ir[i_exp],v[i_fibe],v[i_tca]);
else
	sprintf(line,"RIM=*/********:**:*:*      EXP=%-11.8sFIBE=%-10.6sTCA=%.13s",
		ir[i_exp],v[i_fibe],v[i_tca]);
 }

zvmessage(line, "");
enqueue(line,list);

/*	THIRD LINE			*/
if (phase == PHASEII) 
sprintf(line,
        "SCET=%.4s.%.3s %.2s:%.2s:%.2s:%-4.3sGAIN=%.1s%-9.6sETYPE=%-9.4s",
        nv[i_scetyear],nv[i_scetday],nv[i_scethour],nv[i_scetmin],
        nv[i_scetsec],nv[i_scetmsec],nv[i_gain],gain,v[i_comp]);

else
sprintf(line,
        "SCET=%.4s.%.3s %.2s:%.2s:%.2s:%-4.3sGAIN=%.1s%-9.6sCOMP=%-10.4s",
        nv[i_scetyear],nv[i_scetday],nv[i_scethour],nv[i_scetmin],
        nv[i_scetsec],nv[i_scetmsec],nv[i_gain],gain,v[i_comp]);

if (( type == NON_COMPRSSD ) || ( type == OLD_STYLE_LABEL )){
  sprintf(lineb,"TRUNC BITS/PXL=%-.4s",ir[i_tbppxl]);
}
else {
  sprintf(lineb,"COMP-RATIO=%-.8s",ir[i_compratio]);
}    
strcat(line,lineb);
zvmessage(line, "");
enqueue(line,list);

/*	FOURTH LINE			*/
if (( type == NON_COMPRSSD ) || ( type == OLD_STYLE_LABEL )) {
  sprintf(line,"PA=%-24.20sRATE=%-10.7sENTROPY=%-7.4sTRUNC PXLS/LNE=%.4s",
	v[i_pa],rate,ir[i_entropy],ir[i_tpplne]);
  zvmessage(line,"");
  enqueue(line,list);     
}
else {
  sprintf(line,"PA=%-24.20sRATE=%-10.7sENTROPY=%-7.4sHUF=%-.7s",
	v[i_pa],rate,ir[i_entropy],v[i_huff]);
  zvmessage(line, "");
  enqueue(line,list);
  sprintf(line,"MAX_C RATIO=%-15.8sMIN_C RATIO=%-18.8s",
       ir[i_max_comp],ir[i_min_comp]);
  if (type == ITC_COMPRSSD){
     sprintf(lineb,"THW=(%3.3s,%3.3s,%3.3s,%3.3s)",
         nv[i_tw],nv[(i_tw+1)],nv[i_tw+2],nv[i_tw+3]);
     strcat(line,lineb);
   }
  zvmessage(line,"");
  enqueue(line,list);
}
  
/*	EXTRA LINE FOR ITC_COMPRSSD   */
if ( type == ITC_COMPRSSD ) { 
  sprintf(line,"QSTEP=%-6.6sQM=%-12.11sZZ=%-27.7sROI=(%3.3s,%3.3s,%3.3s,%3.3s)",
	nv[i_qstep],v[i_qm],v[i_zz],
	nv[i_roi],nv[(i_roi+1)],nv[(i_roi+2)],nv[(i_roi+3)]);
  zvmessage(line,"");		
  enqueue(line,list);
} 
  
/* 	FIFTH LINE			*/
sprintf(line,"INA=%-8.6sTWST=%-10.6sSUNAZ=%-9.6sBOOM=%-10.8sHSCL=%.10s M/PXL",
	ir[i_ina],ir[i_twist],ir[i_sunaz],v[i_boom],ir[i_hscl]);
zvmessage(line, "");
enqueue(line,list);
/*	SIXTH LINE			*/
 sprintf(line,"EMA=%-8.6sCONE=%-10.6sSMRAZ=%-9.6sSMEAR=%-9.6sVSCL=%.10s M/PXL",
	ir[i_ema],ir[i_cone],ir[i_smraz],ir[i_smear],ir[i_vscl]);
zvmessage(line, "");
enqueue(line,list);

/*	SEVENTH LINE			*/
 sprintf(line,"PHA=%-8.6sRA=%-12.6sS/CAZ=%-9.6s%s=%-11.6sPLANETRNG=%.10s",
	ir[i_pha],ir[i_ra],ir[i_scaz],lat_rad,ir[i_latrad],ir[i_plrange]);
zvmessage(line, "");
enqueue(line,list);

/* 	EIGHTH LINE			*/
 sprintf(line,"HRA=%-8.6sDEC=%-11.6sNORAZ=%-9.6sLON=%-11.6sSLANT RNG=%.10s",
	ir[i_hra],ir[i_dec],ir[i_noraz],ir[i_lon],ir[i_slrange]);
zvmessage(line, "");
enqueue(line,list);

/* 	NINTH LINE			*/
sprintf(line,"CAL=%-23.20sIOF=%-11.10sUBWC=%-10.3sSOLAR RNG=%.10s",
	v[i_cal],ir[i_iof],v[i_ubwc],ir[i_solrange]);
zvmessage(line, "");
enqueue(line,list);

/*	TENTH LINE			*/
 sprintf(line,"DC=%-24.20sCNV=%-11.10s",v[i_dc],ir[i_cnv]);
if(phase ==  PHASEII){ 
       sprintf(lineb,"SEQNO=%-9.6sDSPK_THRESH=%-1.8s",
               nv[i_seqno],nv[i_ict_despike_t]);
       strcat(line,lineb);
     }
zvmessage(line, "");
enqueue(line,list);

line[0] = '\0';

/*	ELEVENTH LINE			*/
 if( strcmp(format,"HALF")==0 )
	sprintf(line,"BLM=%-23.20sSO=%-27.20sEDR=%12.12s/%3.3s",
		v[i_blm],v[i_so],v[i_edrtape],nv[i_edrfile]);
else
	sprintf(line,"BLM=%-23.20sSO=%-27.20sREDR=%12.12s/%3.3s", 
		v[i_blm],v[i_so],v[i_edrtape],nv[i_edrfile]); 
zvmessage(line, "");
enqueue(line,list);
line[0]='\0';
if(strcmp(ir[i_sub_solar_lat],asterisks)){
  sprintf(line,"SUB_SOLAR_LAT=%-13.10sSUB_SOLAR_LONG=%-15.10sSUB_SPCFT_LAT=%-7.7s",
                   ir[i_sub_solar_lat],ir[i_sub_solar_long],ir[i_sub_spacecraft_lat]);
  zvmessage(line,"");
  enqueue(line,list);
  sprintf(line,"SUB_SPCFT_LONG=%-12.10s",ir[i_sub_spacecraft_long]);
}

if (strcmp(nv[i_nstars],asterisks) != 0){
           sprintf(lineb,"NSTARS=%-20.10sSTAR1=(%3.3s,%3.3s,%3.3s,%3.3s)",
                  nv[i_nstars],nv[i_star1],nv[i_star1+1],nv[i_star1+2],
                  nv[i_star1+3]);
           i_star1 += 3;
           strcat(line,lineb);
           zvmessage(line,"");
           enqueue(line,list);
           line[0] = '\0';
    if(n[i_nstars] > 1){
           for (x=2; x <= n[ i_nstars ] && x <= 4; x ++){
           sprintf(lineb,"STAR%c=(%3.3s,%3.3s,%3.3s,%3.3s)    ",(char)(48+x),
                  nv[i_star1+1],nv[i_star1+2],nv[i_star1+3],nv[i_star1+4]);
           i_star1 += 4;
           strcat(line,lineb);
	 }
           /* terminate line at position 78 */
           if ((int)strlen(line) >= 78) line[78] = '\0';
           zvmessage(line,"");
           enqueue(line,list);
	 }
           if (n[i_nstars] ==5){
              sprintf(line,"STAR5=(%3.3s,%3.3s,%3.3s,%3.3s)   ",
                  nv[i_star1+1],nv[i_star1+2],nv[i_star1+3],nv[i_star1+4]);
                 /* terminate line at position 78 */
                 if ((int)strlen(line) >= 78) line[78] = '\0';
                 zvmessage(line,"");
                 enqueue(line,list);
	   }}
else if (line[0] !='\0') {zvmessage(line," ");
                          enqueue(line,list);
                          }	 
 
/* If file was opened within FLIGHT_LABEL, close it */
if((flags & OPEN)==0)
   sta[0] = zvclose(unit, NULL);
   return list.tail->next;
}


void enqueue(char* str, QUEUE l)
{NODE * temp;
      temp = (NODE *) malloc (sizeof(NODE));
      temp->next ='\0';
      temp->str = (char *) malloc (100);  /*size of line*/
      strcpy(temp->str,str);
      if (l.tail->next == '\0'){ l.head->next = temp;
				 l.tail->next = l.head->next;
			       }
      else {l.head->next->next = temp;
            l.head->next = l.head->next->next;
	  }
} 

















$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create flight_label.imake
/* Imake file for VICAR subroutine FLIGHT_LABEL   */

#define SUBROUTINE  flight_label

#define MODULE_LIST  flight_label.c  

#define P2_SUBLIB

#define USES_ANSI_C

$ Return
$!#############################################################################
$Test_File:
$ create tflight_label.c

#include   "vicmain_c"
/*
#include   <stdio.h>
#include   <stdlib.h>
*/
struct  node { char * str;  /* LINK list for output buffer */
               struct node * next;
             };
typedef struct node NODE;
void main44()
{NODE * list;
int   count, unit[10], sta, x, y;
char  inputs[800];
char  outbuf[100];
sta = zvp("INP",inputs, &count);
for( x=0,y=1; x<count; x++,y++ ) /* Open files before calling flight_label*/
 {
   sta = zvunit(&unit[x],"INP", y, 0);
   sta = zvopen(unit[x],"OP","READ","OPEN_ACT","SA", 0);
   list =(NODE*) flight_label(unit[x]);        /* Display flight label  */
   zvmessage("",""); 
   zvmessage("PRINTING FROM OUTPUT BUFFER"," ");
   zvmessage("","");
   if (list == NULL)  zvmessage("Nothing to print.  Buffer is empty.","");
   else  while (list != NULL){
             strcpy (outbuf, list->str);
             zvmessage(outbuf,"");
	     list = list->next;
   }
     
   sta = zvclose(unit[x], 0);
   zvmessage("FINISHED", "");
   zvmessage("", "");
  }
}

$!-----------------------------------------------------------------------------
$ create tflight_label.imake
/* IMAKE file for Test of VICAR subroutine  FLIGHT_LABEL   */

#define PROGRAM tflight_label

#define MODULE_LIST tflight_label.c

#define MAIN_LANG_C
#define TEST

#define USES_ANSI_C

#define   LIB_RTL
#define   LIB_TAE

#define   LIB_P2SUB          /* Enable during delivery  */

/*
#define LIB_LOCAL
*/

$!-----------------------------------------------------------------------------
$ create tflight_label.pdf
Process
Parm  INP (String,80)  Count=1:10
End-Proc
$!-----------------------------------------------------------------------------
$ create tstflight_label.pdf
procedure
refgbl $echo
refgbl $syschar
body
local PATH1 TYPE=STRING init="wms_test_work:[testdata.sitod1.test_data.images]"
local PATH2 TYPE=STRING init="wms_test_work:[testdata.mipl.gll]"
local PATH3 TYPE=STRING init="wms_test_work:[testdata.gll]"
local PATH4 TYPE=STRING init="wms_test_work:[testdata.mipl.vgr]"
local PATH5 TYPE=STRING init="wms_test_worK:[ssi.udr]"
local PATH6 TYPE=STRING init="wms_test_work:[stream.udr]"
local PATH7 TYPE=STRING Init="wms_test_work:[testdata.y2k]"
if ($syschar(1) = "UNIX")
    let PATH1 ="/project/test_work/testdata/sitod1/test_data/images/"
    let PATH2 ="/project/test_work/testdata/mipl/gll/"
    let PATH3 ="/project/test_work/testdata/gll/"
    let PATH4 ="/project/test_work/testdata/mipl/vgr/"
    let PATH5 ="/project/test_work/ssi/udr/"
    let PATH6 ="/project/test_work/stream/udr/"
    let PATH7 ="/project/test_work/testdata/y2k/"
end-if
let _onfail="continue"
let $echo="yes"
!
Write " The Test Data are handled for both VMS and UNIX in this PDF. "

tflight_label  &"path2"venus.img
LABEL-LIST  &"path2"venus.img     
tflight_label  &"path2"venus2.img    
LABEL-LIST  &"path2"venus2.img   
tflight_label  &"path2"venus3.img    
LABEL-LIST  &"path2"venus3.img    
tflight_label  &"path1"s0061498500.1    
LABEL-LIST  &"path1"s0061498500.1   
tflight_label  &"path1"s0061510200.1    
LABEL-LIST  &"path1"s0061510200.1    
tflight_label  &"path1"s0061512000.1    
LABEL-LIST  &"path1"s0061512000.1    
tflight_label  &"path3"test_image_ict.udr	   
LABEL-LIST  &"path3"test_image_ict.udr	   
tflight_label  &"path3"test_image_barc.udr	   
LABEL-LIST  &"path3"test_image_barc.udr	   
tflight_label  &"path3"test_image_lossless.udr   
LABEL-LIST  &"path3"test_image_lossless.udr  

!(FR 89354)
label-list &"path5"s0165200300.20
tflight_label  &"path5"s0165200300.20

!placing a valid value for filter
label-list &"path3"s1677721400.3
label-replace &"path3"s1677721400.3 star.out "FILTER=3" TASK="TASK"
tflight_label star.out

!adding stars
label-add star.out  stars.out "NSTARS = 5,+
STAR1=(231,165,12,894),STAR2=(21,32,187,23),STAR3=(5,10,15,20),+
STAR4=(1,2,3,5),STAR5=(9,4,8,3)" TASK="TASK"
tflight_label stars.out
label-list stars.out

label-replace stars.out star.out "NSTARS = 4" TASK="TASK"
tflight_label star.out 

label-replace stars.out star.out "NSTARS = 3" TASK="TASK"
tflight_label star.out 

label-replace stars.out star.out "NSTARS = 2" TASK="TASK"
tflight_label star.out 

label-replace stars.out star.out "NSTARS = 1" TASK="TASK"
tflight_label star.out 

label-add     stars.out star.out "RAD = 10.22" TASK="TASK"
tflight_label star.out 

label-delete stars.out star.out /all task="TASK"
label-add    star.out  stars.out "MISSION=GLL PARTITION=1,+
               ENCODING_TYPE='INTEGER COSINE TRANSFORM ', +
               NSTARS=2 SENSOR=SSI SUB_SOLAR_LATITUDE=9.0" TASK="TASK"
tflight_label stars.out
Write  "The file should not have an output: NON-SSI image "
tflight_label INP= &"path4"mirandab.vio

label-list &"path6"s0351305813.galsos
tflight_label &"path6"s0351305813.galsos

! Y2K tests.  Check SCET value
label-list &"PATH7"scet_jan_01_2000.img
tflight_label &"PATH7"scet_jan_01_2000.img

label-list &"PATH7"scet_mar_01_2000.img
tflight_label &"PATH7"scet_mar_01_2000.img

label-list &"PATH7"scet_dec_31_2000.img
tflight_label &"PATH7"scet_dec_31_2000.img

label-list &"PATH7"scet_jan_01_2001.img
tflight_label &"PATH7"scet_jan_01_2001.img

label-list &"PATH7"scet_mar_01_2001.img
tflight_label &"PATH7"scet_mar_01_2001.img

label-list &"PATH7"scet_dec_31_2001.img
tflight_label &"PATH7"scet_dec_31_2001.img

if ($syschar(1)="UNIX")
	ush rm stars.out
        ush rm star.out
else
        dcl del stars.out;*
        dcl del star.out;*
end-if
end-proc


$ Return
$!#############################################################################
$Other_File:
$ create flight_label.hlp
1 FLIGHT_LABEL

  FLIGHT_LABEL outputs to the display the flight label of a Galileo SSI image.
  Flight_label also returns a pointer to a link list that contains the 
  information printed on the display.  If the input file is not a Galileo SSI 
  image, FLIGHT_LABEL returns NULL.

  Calling Sequence:

  FLIGHT_LABEL ( UNIT )

  where	UNIT is the unit number of the file of interest.  The file may or
  may not be open before this subroutine is called.  If the file is closed
  this subroutine will open it and then close it once the flight label has
  been displayed.

2 Display Format

  This display of SSI flight label items is identical to that of Vicar program
  LABLIST (see also MSD:384-89-165).  There are three possible formats for
  displaying the SSI labels
    
  (1) If the input image is ICT compressed, the label items will be organized and
  printed in the following format:

  GLL/SSI  PICNO=12A0001     FILTER=3(VIO)  TLMFMT=XXX    TARGET=CALLISTO
  RIM=1/16777215:90:9:7      EXP=51200.00   FIBE=1001     TCA=-003 23:13:00  
  SCET=1995.123 12:23:56     GAIN=1(400K)   COMP=ICT      COMP RATIO=24.23
  PA=NNIOOOOOO#MMSSSSXXXX    RATE=60 2/3    ENTROPY=2.23  HUF=ABCDEFG
  QSTEP=20      QM=ABCDEFG   ZZ=ABCDEFG     TW=(353,353)  ROI=(101,101,600,500)
  INA= 89.12  TWST=359.99    SUNAZ=359.99   BOOM=NO       HSCL=1.2345E5 M/PXL   
  EMA=180.00  CONE=179.99    SMRAZ=359.99   SMEAR=99.99   VSCL=1.2345E5 M/PXL   
  PHA=179.33  RA=359.99      S/CAZ=359.99   LAT=-90.00    PLANETRNG=123456789
  HRA=130.31  DEC=-90.00     NORAZ=359.99   LON=359.99    SLANT RNG=123456789
  CAL=RADIOMETRIC-FILENAME   IOF=1.0000E-3  UBWC=YES      SOLAR RNG=123456789
  DC=DARKCURRENT-FILENAME    CNV=3.5135E-2
  BLM=BLEMISH-FILENAME       SO=SHUT-OFFSET-FILENAME      EDR=GES006622/066
  
  (2) If the input image was losslessly compressed via the Huffman compressor,
  the output format is the same as above except that the line containing
  label items QSTEP, QM, ZZ, TW, and ROI is not printed. 
  
  (3) If the input image is not ICT compressed or losslessly compressed using the
  Huffman compressor, the output format is as follows:

  GLL/SSI  PICNO=12A0001     FILTER=3(VIO)  TLMFMT=***    TARGET=CALLISTO
  RIM=1/16777215:90:9:7      EXP=51200.00   FIBE=1001     TCA=-003 23:13:00  
  SCET=1995.123 12:23:56     GAIN=1(400K)   COMP=RC       TRUNC BITS/PXL=2.34
  PA=NNIOOOOOO#MMSSSSXXXX    RATE=60 2/3    ENTROPY=2.23  TRUNC PXLS/LNE=123
  INA= 89.12  TWST=359.99    SUNAZ=359.99   BOOM=NO       HSCL=1.2345E5 M/PXL   
  EMA=180.00  CONE=179.99    SMRAZ=359.99   SMEAR=99.99   VSCL=1.2345E5 M/PXL   
  PHA=179.33  RA=359.99      S/CAZ=359.99   LAT=-90.00    PLANETRNG=123456789
  HRA=130.31  DEC=-90.00     NORAZ=359.99   LON=359.99    SLANT RNG=123456789
  CAL=RADIOMETRIC-FILENAME   IOF=1.0000E-3  UBWC=YES      SOLAR RNG=123456789
  DC=DARKCURRENT-FILENAME    CNV=3.5135E-2
  BLM=BLEMISH-FILENAME       SO=SHUT-OFFSET-FILENAME      EDR=GES006622/066
  
  If any of the label items are not found in the input file specified by the
  user, asterisks are printed in place of the value.  If a vicar command error 
  is encountered, "ERRxxx" is printed after the keyword.  xxx is the VICAR 
  error message number found in appendix B of the VICAR Run-Time Library  
  Reference Manual.

3 History

  Original Programmer: Justin McNeill, Jr, June 1, 1990
  Cognizant Programmer: Justin McNeill, Jr.
  Source Language: C
  Revisions: 	December 1990  	"ENTRPY" changed to "ENTROPY" in output display.
		November 1990	Task name determined by SENSOR label item.

  Ported to UNIX by:  W.P. Lee,       May-1992
  Revisions on Ported Version:

  June-1992... W.P. Lee  1. Enabled #include "xvmaininc.h" statement in the 
                            source code
                         2. Made n[15] array to be static so that it would print
                            out sensible MOD91 value for image of old label for-
                            mat
                    
                            
  Galileo Phase II additions and changes: G. Connor,   January 1994
  July-1996   O.A.Montoya  Added capability to return a link list that contains
                           the information printed on the display.  This 
                           information can be used by programs such as MASKV. 
                           Modified to display correctly PHASEII data(FR89392).
  August-1996 O.A.Montoya  Modified code to search for DC, CAL, BLM and SO in 
                           both the first and Galsos tasks (FR89811).

  June 30, 1998 T.Huang    Modified to be Year 2000 compilance. 
                           Converted to ANSI C.

$ Return
$!#############################################################################

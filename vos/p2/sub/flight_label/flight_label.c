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


















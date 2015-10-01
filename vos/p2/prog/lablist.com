$!****************************************************************************
$!
$! Build proc for MIPL module lablist
$! VPACK Version 1.9, Monday, December 07, 2009, 16:34:33
$!
$! Execute by entering:		$ @lablist
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module lablist ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to lablist.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("lablist.imake") .nes. ""
$   then
$      vimake lablist
$      purge lablist.bld
$   else
$      if F$SEARCH("lablist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lablist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lablist.bld "STD"
$   else
$      @lablist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lablist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lablist.com -mixed -
	-s lablist.c -
	-i lablist.imake -
	-p lablist.pdf -
	-t tstlablist.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lablist.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h> 
#include  "vicmain_c"
#include  "defines.h"
#include <string.h>
#include <stdlib.h>

/*	DEFINE GLOBAL CONSTANTS						*/
#define MAXTASKS         60
#define WIDTH             80
#define MAXLINEWIDTH    1000
#define TRUE               1

/*	ERROR HANDLING MACROS						*/
#define return_if_error(A)	zvsignal(A,stay,0); if(stay<=0) return
#define continue_if_error(A)	zvsignal(A,stay,0); if(stay<=0) continue
#define break_if_error(A)	zvsignal(A,stay,0); if(stay<=0) break

/************************************************************************

Subroutines in order of appearance:

determine_label()		Determines project label type (i.e. Galileo 
				flight label versus IBM label format of 
				"LABxx" for Viking, Voyager and Galileo ground
				calibration labels).

print_header_systemlabel()	Prints header displaying image filename and
				the system label containing data format,
				number of bands, number of lines, number of
				samples, and organization of image data.

general_label()			Displays labels of IBM format "LABxx" without
				reformatting.  Removes "LABxx" and line 
				continuation characters from labels.

history_tasks()			Displays history of processing (i.e. all tasks
				are listed).

print_history_labels()		Prints all history labels, with proper 
				formatting of keyword/value pairs and no screen
				scrolling.

print_keyword_value()		Prints keyword/value pairs for respective
				task/history labels.

************************************************************************/

/*	DECLARE GLOBAL VARIABLES					 */

int 	stay  ;	    /* VICAR command error message variable    */
int     ii,jj ;     /* for conveinence use  */
char	frmat[10], /* Format of image data (BYTE,HALF,etc.)   */
	bufline[MAXLINEWIDTH],/* Character buf for history label output  */
	line[WIDTH];          /* General character buf for screen output */

/* Structure to simulate task name array   */
/*  struct  char8 {char name[8];};    */
/*  char   tasks[MAXTASKS][MAX_LABEL_KEY_SIZE + 1] ;    */

struct  multival {	/* Structure to hold pertinent information for   */
	int nelements;	/* zlget's of subroutine history_tasks.		 */
	int maxlength;
	char *data  ;
	int allocsize;
                } info;

/*	MAIN PROGRAM							  */
void main44(void)
{
int		count,		/* Required variable of ZVPARM, not applic.  */
		instances[MAXTASKS], /*Array to hold instance values of tasks*/
		numoftasks,	/* Number of tasks retrieved with Zlhinfo    */
		type,		/* Type of label(s) found		     */	
		unit;		/* Unit number identifying flight image      */
char		extent[8],	/* String to hold user parameter	     */
		file[70];	/* Flight image name of label to be listed   */
/* struct char8	tasks[MAXTASKS]; */	/* Array of structures char8 for task names  */
char    tasks[MAXTASKS][MAX_LABEL_KEY_SIZE + 1];
/*  =======================================================  */

zvmessage("LABLIST version:  JULY-1996 ","");

numoftasks=MAXTASKS;			/* Initialize maximum # of tasks      */

stay = zvp("INP", file, &count);      /* Obtain input file name  */
stay = zvp("EXTENT",extent,&count);   /* Obtain user parameter	      */
ii = 1;
stay = zvunit(&unit,"INP",ii, NULL);    /* Determine unit number of file name */
stay = zvopen(unit,"OPEN_ACT","SA", NULL);	/* Open file; if error, abort LABLIST */

print_header_systemlabel(unit,file);	/* Print header and system label      */
/* Get all task names of file         */
stay = zlhinfo(unit,(char *)tasks,instances,&numoftasks,
		"ULEN",MAX_LABEL_KEY_SIZE+1, "ERR_ACT","SA", NULL);
determine_label(unit,&type,tasks[0]); /* Determine label types */

if(type==0)				/* Print appropriate label format     */
	general_label(unit,tasks[0]);
else
	if (type==1) 
              flight_label(unit) ;  
              		
if(numoftasks>1 || type==2)		/* Print processing history	     */
	history_tasks(unit,tasks,instances,numoftasks,extent,type);
}



/******************************************************************************

	Purpose:	To print the output header and the system label
			information.					     */

print_header_systemlabel(u,s)
int u;				/* Unit number of file		             */
char s[];			/* Input file name		             */
{
int	nb,			/* Number of bands in image	             */
	nl,			/* Number of lines in image	             */
	ns;			/* Number of samples in image                */
char	org[10];		/* Image data organization (BSQ,BIP,or BIL)  */

sprintf(line,"\nLABEL FOR FILE:");
zvmessage(line, "");
sprintf(line,"%s",s);
zvmessage(line,"");

stay = zvget(u,"FORMAT",frmat,"NL",&nl,"NS",&ns,"NB",&nb,"ORG",org, NULL);
return_if_error(u);

sprintf(line,"%s FORMAT  NB=%2d  NL=%5d  NS=%5d  FILE ORGANIZATION=%s",
	frmat,nb,nl,ns,org);
zvmessage(line, "");
}



/******************************************************************************

	Purpose:	Determine the type of label for a file with a 
			given unit number.				     */

determine_label(u,type,task)
int 	u,					/* Unit number of file	     */
	*type;					/* Pointer to type of file   */
						/*	TYPE:		     */
						/*	0 = old IBM format   */
						/*	1 = Galileo/SSI	     */
						/*          (not ICT cmprssd)*/
						/*	2 = Galileo/SSI	     */
						/*	    (ICT compressed) */
						/*      3 = Unrecognizable   */
char	task[];					/* Task name		     */
{
char 	value[90];				/* String to accept values   */
						/*	from ZLGET call      */ 

stay = zlget(u,"HISTORY","LAB01",value,"HIST"   /* Determine label type	     */
	,task, NULL);
if(stay != 1) {
	stay = zlget(u,"HISTORY","MISSION",value,"HIST",task, NULL);
	if(stay != 1) 
		*type = 2;
	
	else 
		*type = 1;
	
}
else	/* If there is an error, unknown label message will be printed. */
	*type = 0;
}



/******************************************************************************

	Purpose:	To print all labels identified by LABxx label item.
									     */
general_label(u, task)
int u;			/* unit number of file whose label will be listed    */
char task[];		/* task/program name				     */
{
int	n,		/* loop control variable			     */
	numoflabels;	/* number of labels in project label		     */
char	data[80],	/* array used to hold a string value		     */
	label[6],	/* string used to match LABxx label items	     */
	numlabels[3];	/* character representation of label number	     */
	
numoflabels=10;		/* set default value of number of labels	     */

  /* Determine the number of labels   */

stay = zlget(u,"HISTORY","NLABS",&numoflabels, "HIST",task, NULL);

for(n=2;n<=numoflabels;n++)     /* Print all labels  */
	{
	strcpy(label,"LAB");
	sprintf(numlabels,"%d",n);
	if(n<10)
		strcat(label,"0");
	strcat(label,numlabels);  /* Produce label keyword  */
	stay = zlget(u,"HISTORY",label,data,"HIST",task, NULL);
	break_if_error(u);      /* Exit loop if Zlget error exists  */
	sprintf(line,"%.71s",data);	 /* Print labels		     */
	zvmessage(line, "");
	}
}



/*****************************************************************************

	Purpose:	Based on the presence of user parameter FULL - if FULL 
			is specified, a complete listing of all history labels 
			is displayed.  If no user parameter is specified, a 
			listing of just program names is displayed.
									      */
history_tasks(unit,tasks,instances,numoftasks,extent,type)
int 	unit,			/* Unit number of file			      */
	type,			/* Label type				      */
	instances[MAXTASKS],  	/* Array of instances of task/program names   */
	numoftasks;		/* Total number of task keywords	      */
char    extent[];               /* User parameter (f,F,full,FULL or NULL)     */
char    tasks[MAXTASKS][MAX_LABEL_KEY_SIZE + 1];
/*  struct char8 tasks[MAXTASKS];*/	/* Structure of task/program names    */
{
int	count,			/* Loop control variable		      */
	length,			/* Output buffer length			      */
	start;			/* History task at which to start listing     */

zvmessage(" ", "");
line[0]='\0';					/* Clear line buffer	      */

if(extent[0]=='F' || extent[0]=='f')		/* Check for FULL option      */
	print_history_labels(unit,tasks,instances,numoftasks,type);
else	
	{
	if(type!=2)	 /* Check type of project label to print appropriate  */
		start=1; /*	first program name of processing history.     */
	else
		start=0;

	for(count=start;count<numoftasks;count++)   /* Print all programs in  */
		{			            /* processing history.    */
		if(count==start)			
			sprintf(bufline,"%.8s",&tasks[count][0]);
		else
			sprintf(line," - %.8s",&tasks[count][0]);
		length=strlen(bufline)+strlen(line);	/* Calculate output   */
		if(length>75)				/* string length.     */
			{
			zvmessage(bufline, "");	/* Print line if length of    */
			strcpy(bufline," ");	/* bufline and line is >75.   */
			}
		strcat(bufline,line);
		}
	zvmessage(bufline, "");		/* Print remaining contents of buffer */
	}
}



/******************************************************************************

	Purpose:	Print all history labels, with proper formatting
			of keyword/value pairs.
									      */
print_history_labels(unit,tasks,instances,numoftasks,type)
int	instances[MAXTASKS],   /* Array of instances for tasks/programs      */
	numoftasks,		/* Number of task names			      */
	type,			/* Project label type number(=2 label unknown)*/
	unit;			/* Unit number of file			      */
/* struct char8 task[MAXTASKS]; */ /* Array of structures of task names      */
char    tasks[MAXTASKS][MAX_LABEL_KEY_SIZE+1] ;
{
int	count,		/* Loop control variable	  		      */
	dummy,		/* Dummy variable for use in VICAR command	      */
	start;		/* Starting point of label printing		      */
int     jj ;
char	forma[32],	/* Data format of returned value for respective keyword
				value returned for respective keyword	      */
	key[MAX_LABEL_KEY_SIZE + 1],    /*    Keyword   */
	time[28],	/* Data & time at which respective task was performed */
	user[12];	/* Username for respective task			      */
struct multival	info;	/* Structure used to store information about zlinfo() */

info.allocsize = 0;     /* Initialize variables */
info.data = NULL; 
jj =  MAX_LABEL_KEY_SIZE + 1;

if(type==2)		/* If no project label was printed,		      */
	start=0;	/* start history label printing at first task.        */
else
	start=1;	/* Otherwise, start label printing at second task.    */

for(count=start;count<numoftasks;count++) 		/* Print all tasks    */
	{
	if(strlen(bufline)!=0)		/* Check for proper screen display    */
		{
		zvmessage(bufline, "");
		bufline[0]='\0';
		}

/* Clear user and time variable	      */
        memset(user, 0, 12) ;
        memset(time, 0, 28) ; 
/*        jj = 3;
        zia(user, &jj);			
        jj = 7;
	zia(time, &jj);
*/
  /* Get username			      */
   stay = zlget(unit,"HISTORY","USER",&user[0], "HIST",&tasks[count][0],
                    "INSTANCE",instances[count], NULL);
	if(stay<=0 && info.data!=NULL)   free(info.data);  
	return_if_error(unit);   

/* Get date and time		      */
      stay = zlget(unit,"HISTORY","DAT_TIM",&time[0],"HIST",
                       &tasks[count][0],
                     "INSTANCE",instances[count], NULL);
 	if(stay<=0 && info.data!=NULL) 	free(info.data);  
	return_if_error(unit);
	
	sprintf(line, 			 /* Print task header		      */
		"---- TASK: %-14.8sUSER: %s\t%s ----",&tasks[count][0],
                    user,time);
	zvmessage(line, "");

/* Set keyword to current task       */
	stay = zlinfo(unit,"HISTORY","TASK",&forma[0],&dummy,&info.nelements,
                    "HIST",tasks[count],"INSTANCE",instances[count],
                      "STRLEN",&info.maxlength, NULL);
	continue_if_error(unit);	
	
	while(TRUE)		/* Cycle until no more keywords are found     */
		{
/* Clear key and format buffers		      */
         jj = MAX_LABEL_KEY_SIZE + 1 ;
         memset(key, 0, jj);
         memset(forma, 0, 32);
/*
         jj = 3;
         zia(key, &jj);	
         jj = 8 ; 
	 zia(forma, &jj);
*/
/* Find next keyword 	      */
        stay = zlninfo(unit, &key[0], &forma[0], &dummy, &info.nelements,
               "STRLEN", &info.maxlength, NULL);
        if((stay == END_OF_LABEL) || (strcmp(key,"TASK")==0))  break;
        break_if_error(unit);
        info.maxlength++;
        if((strcmp(key,"DAT_TIM")==0)||(strcmp(key,"USER")==0)) continue;
/*  Insure memory space    */
        if(info.maxlength*info.nelements>info.allocsize)
        {   
         info.data = (char *) malloc(info.maxlength*info.nelements);
	 info.allocsize = info.maxlength*info.nelements;
	 if(info.data==NULL)
         {
	  zvmessage("INSUFFICIENT MEMORY", "");
	  zabend();  
	  }
	}

/* Get next value  */
        stay = zlget(unit,"HISTORY",key, info.data,"HIST",&tasks[count][0],
               "INSTANCE",instances[count],"FORMAT","STRING",
               "ULEN",info.maxlength,"NELEMENT",info.nelements, NULL);
		continue_if_error(unit);
          print_keyword_value(key,&info,forma);/* Print keyword/value */
		}
	}
  if(info.data != NULL)
	free(info.data);

if(strlen(bufline) != 0)
	zvmessage(bufline, "");
}



/*******************************************************************************

	Purpose:	Print keyword/value(s) pairs for respective task labels.
									      */
print_keyword_value(key,info,form)
char 	key[MAX_LABEL_KEY_SIZE + 1] ,   /* Keyword	*/
	form[32];			/* Pointer to format of label item    */
struct multival *info;			/* Structure of type multival	      */
{
int	i,				/* Loop control variable	      */
	length;				/* Output string buffer size	      */
int     l1, l2;

l2 = strlen(key) ;
l1= strlen(info->data);
length = strlen(key)	/* Calculate length of keyword and data + one element */
 	+ strlen(info->data) + 8;

if((strlen(bufline)!=0) && (strlen(bufline)+length >= WIDTH))
	{ 	/* If LENGTH is longer than screen width, print bufline       */
	zvmessage(bufline, "");
	bufline[0]='\0';
	}

if (strlen(bufline)!=0)			/* Load buffer with next keyword      */
	strcat(bufline,"  ");
strcat(bufline,key);
strcat(bufline,"=");
	
if(info->nelements > 1)		/* If the data is multivalued, check length   */
	strcat(bufline,"(");	/* after every addition of an element.	      */

for(i=0;i<info->nelements;i++)  /* Loop through all values		      */
	{
	length = strlen(info->data + (i*info->maxlength))+4;
	if((strlen(bufline)!=0)&&(strlen(bufline)+length >= WIDTH))
		{
		zvmessage(bufline, "");
		bufline[0]='\0';
		}

	if(form[0] == 'S') /* If string value, print value with apostrophes   */
		strcat(bufline,"'");
	strcat(bufline,info->data + (i*info->maxlength));
	if(form[0] == 'S')
		strcat(bufline,"'");
	if(i != info->nelements-1)			
		strcat(bufline,", ");
	}
if(info->nelements > 1)
	strcat(bufline,")");
if(strlen(bufline)!=0)
	{
	zvmessage(bufline, "");
	bufline[0]='\0';
	}
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lablist.imake
#define PROGRAM  lablist

#define MODULE_LIST lablist.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB


$ Return
$!#############################################################################
$PDF_File:
$ create lablist.pdf
process help=*
PARM INP    STRING
PARM EXTENT KEYWORD VALID=(F,f,full,FULL) COUNT=(0:1) DEFAULT=--
END-PROC
!#annot function=(display/listing,labels)
!#project=(Galileo,Viking,Voyager,LABxx)
!#keywords=(listing,display,labels)
.title
VICAR PROGRAM LABLIST
.help
PURPOSE:

LABLIST will print the project label of an image followed by either a simple 
listing of history label program/task names or a complete listing of all 
history labels.

Projects supported by LABLIST are Galileo, Voyager, Viking Orbiter, and all 
previous missions using the IBM "LABxx" label convention (e.g. LAB01, LABO2, 
LAB03,... ).  For reference of a similar program, see program LABEL-LIST.

REFERENCE:
  MSD:384-89-165, "Galileo SSI Picture Label", Revision 2, Gary Yagi,
    6 June 1989.

.page
EXECUTION:

	LABLIST  INP  user-parameter

where INP is a flight image in standard VICAR format.  User-parameter specifies
whether or not the user desires a full display of the history labels of an 
image.  The default for user-parameter is NULL.

OPERATION:

LABLIST will first identify the input file organization, data format, and
image size:

  Ex:  BYTE FORMAT  NB=XX  NL=XXXXX  NS=XXXXX  FILE ORGANIZATION=BSQ

LABLIST will then read the input image label and identify the project and label
type.  Keywords "LABxx" and "MISSION" are used to identify IBM format and 
Galileo SSI format labels, respectively.
.page
LABELS OF IBM FORMAT

If the project label is in the old IBM format (LAB01, LAB02, etc.), the label is
printed with the LABxx identifier deleted (this permits the label to fit on an 
80 character terminal screen).  An example is shown below.

   VGR-2   FDS 11412.51   PICNO 1347N2+000   SCET 89.237 21:55:09         
   WA CAMERA  EXP  111360.0 MSEC FILT 2(CLEAR )  LO GAIN  SCAN RATE  5:1  
   ERT 89.238 02:05:23   1/ 1 FULL    RES   VIDICON TEMP  -80.00 DEG C    
   IN/xxxxxx/xx OUT/xxxxxx/xx                 DSS #14   BIT SNR   26.383  
    60000 A/E10005EF B/907F C/917F D/007F0000 ETLM/C5C6D58BA0C4E1DE1910S A
   NA OPCAL xx(111360.0*MSEC)PIXAVG 048/0 OPERATIONAL MODE 7(BOTSIM)     A
   CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  A
   NA   NO   PREP  NO    YES   NO    NO    NO    NO    0 P  * NORMAL     A
   WA   NO   READ  YES   NO    NO    NO    NO    NO    2 P  6 NORMAL     A
   LSB_TRUNC=OFF  TLM_MODE=IM-O  COMPRESSION=ON                           

.page
FLIGHT LABELS FOR GALILEO SSI

If the input image is ICT compressed, the label items will be organized and
printed in the following format:

  GLL/SSI  PICNO=12A0001    FILTER=3(VIO)  TLMFMT=XXX    TARGET=CALLISTO
  RIM=1/16777215:90:9:7     EXP=51200.00   FIBE=1001     TCA=-003 23:13:00  
  SCET=95.123 12:23:56      GAIN=1(400K)   COMP=ICT      COMP RATIO=24.23
  PA=NNIOOOOOO#MMSSSSXXXX   RATE=60 2/3    ENTROPY=2.23  HUF=ABCDEFG
  QSTEP=20      QM=ABCDEFG  ZZ=ABCDEFG     TW=(353,353)  ROI=(101,101,600,500)
  INA= 89.12  TWST=359.99   SUNAZ=359.99   BOOM=NO       HSCL=1.2345E5 M/PXL   
  EMA=180.00  CONE=179.99   SMRAZ=359.99   SMEAR=99.99   VSCL=1.2345E5 M/PXL   
  PHA=179.33  RA=359.99     S/CAZ=359.99   LAT=-90.00    PLANETRNG=123456789
  HRA=130.31  DEC=-90.00    NORAZ=359.99   LON=359.99    SLANT RNG=123456789
  CAL=RADIOMETRIC-FILENAME  IOF=1.0000E-3  UBWC=YES      SOLAR RNG=123456789
  DC=DARKCURRENT-FILENAME   CNV=3.5135E-2
  BLM=BLEMISH-FILENAME      SO=SHUT-OFFSET-FILENAME      EDR=GES006622/066

If the input image was losslessly compressed via the Huffman compressor,
the output format is the same as above except that the line containing
label items QSTEP, QM, ZZ, TW, and ROI is not printed. 

If the input image is not ICT compressed or losslessly compressed using the
Huffman compressor, the output format is as follows:

  GLL/SSI  PICNO=12A0001    FILTER=3(VIO)  TLMFMT=XXX    TARGET=CALLISTO
  RIM=1/16777215:90:9:7     EXP=51200.00   FIBE=1001     TCA=-003 23:13:00  
  SCET=95.123 12:23:56      GAIN=1(400K)   COMP=RC       TRUNC BITS/PXL=2.34
  PA=NNIOOOOOO#MMSSSSXXXX   RATE=60 2/3    ENTROPY=2.23  TRUNC PXLS/LNE=123
  INA= 89.12  TWST=359.99   SUNAZ=359.99   BOOM=NO       HSCL=1.2345E5 M/PXL   
  EMA=180.00  CONE=179.99   SMRAZ=359.99   SMEAR=99.99   VSCL=1.2345E5 M/PXL   
  PHA=179.33  RA=359.99     S/CAZ=359.99   LAT=-90.00    PLANETRNG=123456789
  HRA=130.31  DEC=-90.00    NORAZ=359.99   LON=359.99    SLANT RNG=123456789
  CAL=RADIOMETRIC-FILENAME  IOF=1.0000E-3  UBWC=YES      SOLAR RNG=123456789
  DC=DARKCURRENT-FILENAME   CNV=3.5135E-2
  BLM=BLEMISH-FILENAME      SO=SHUT-OFFSET-FILENAME      EDR=GES006622/066

.page
ERROR HANDLING FOR GALILEO SSI FLIGHT LABELS

If any of the label items are not found in the input file specified by the
user, asterisks are printed in place of the value.  If a vicar command error 
is encountered, "ERRxxx" is printed after the keyword.  xxx is the VICAR error 
message number found in appendix B of the VICAR Run-Time Library Reference 
Manual.


LABELS FOR GALILEO NIMS  (TBD, as of June-12, 1992)

.page
UNRECOGNIZABLE LABELS

If the label does not possess a "LABxx" or "MISSION" keyword, LABLIST prints a 
message stating that the program cannot properly identify the label.  LABLIST
continues with the processing of history labels.


PROGRAMS IN PROCESSING HISTORY

After the flight project labels have been printed, a list of every program in 
the processing history is printed:

	EX:  FICOR77 - RESSAR77 - PHOTFUNC - LGEOM - COPY
 
If the keyword FULL is specified as a user-parameter, the program list is 
replaced by a printout of all the history labels, including task name, user
name, and date for each label.

.page
PROGRAM HISTORY:

Written by: Justin McNeill, September 27, 1989
Current cognizant programmer: Justin McNeill
Revisions:
  Dec 7, 1990 JFM  "ENTRPY" changed to "ENTROPY" in output display
				 and frame rate 2 1/2 changed to 2 1/3.
  Mar 21,1991 SXP  Allow EDRTAPE name to be up to 12 chars.

  Jun    1992 W.P. Lee  1. Eliminated the original subroutine FLIGHTLABEL.
                                Instead, interfaced with FLIGHT_LABEL that was
                                already ported
                        2. Shortened NIMS comments in PDF
                        3. Ported to UNIX
  Feb 22 1995 G. Connor  Add SSI Phase 2 capability.
  Jul  8 1996 O. Montoya Modified FLIGHT_LABEL to display phaseII labels
                         correctly (FR89392).

.LEVEL1
.VARI INP
STRING - input image
.VARI FULL
KEYWORD--OPTIONAL
.LEVEL 2
.VARI INP
A VICAR labelled image file for which history labels will be printed.
.VARI FULL
An optional user specified parameter that causes all the history labels to be 
printed in page-by-page manner.  F, FULL,f, and full are all acceptable 
parameter inputs. 
.end







$ Return
$!#############################################################################
$Test_File:
$ create tstlablist.pdf
procedure
refgbl $echo
refgbl $syschar
body
local PATH1 TYPE=STRING init="wms_test_work:[testdata.sitod1.test_data.images]"
local PATH2 TYPE=STRING init="wms_test_work:[testdata.mipl.gll]"
local PATH3 TYPE=STRING init="wms_test_work:[testdata.gll]"
local PATH4 TYPE=STRING init="wms_test_work:[testdata.mipl.vgr]"
if ($syschar(1) = "UNIX")
    let PATH1 ="/project/test_work/testdata/sitod1/test_data/images/"
    let PATH2 ="/project/test_work/testdata/mipl/gll/"
    let PATH3 ="/project/test_work/testdata/gll/"
    let PATH4 ="/project/test_work/testdata/mipl/vgr/"
end-if
let _onfail="continue"
let $echo="yes"
!
Write " The Test Data are handled for both VMS and UNIX in this PDF. "

LABLIST  &"path1"gll.edr
LABLIST  &"path1"gll.edr   'full
LABEL-LIST  &"path1"gll.edr   
LABLIST  &"path4"mirandab.vio
LABLIST  &"path4"mirandab.vio  'full
LABEL-LIST  &"path4"mirandab.vio  
LABLIST  &"path2"venus.img
LABLIST  &"path2"venus.img     'full
LABEL-LIST  &"path2"venus.img     
LABLIST  &"path2"venus2.img
LABLIST  &"path2"venus2.img    'full
LABEL-LIST  &"path2"venus2.img   
LABLIST  &"path2"venus3.img
LABLIST  &"path2"venus3.img    'full
LABEL-LIST  &"path2"venus3.img    
LABLIST  &"path1"s0061498500.1    'FULL
LABEL-LIST  &"path1"s0061498500.1   
LABLIST  &"path1"s0061510200.1    'FULL
LABEL-LIST  &"path1"s0061510200.1    
LABLIST  &"path1"s0061512000.1    'FULL
LABEL-LIST  &"path1"s0061512000.1    
LABLIST  &"path3"test_image_ict.udr	   'FULL
LABEL-LIST  &"path3"test_image_ict.udr	   
LABLIST  &"path3"test_image_barc.udr	   'FULL
LABEL-LIST  &"path3"test_image_barc.udr	   
LABLIST  &"path3"test_image_lossless.udr   'FULL
LABEL-LIST  &"path3"test_image_lossless.udr  

!placing a valid value for filter
label-list &"path3"s1677721400.3
label-replace &"path3"s1677721400.3 star.out "FILTER=3" TASK="TASK"
lablist star.out

!adding stars
label-add star.out  stars.out "NSTARS = 5,+
STAR1=(231,165,12,894),STAR2=(21,32,187,23),STAR3=(5,10,15,20),+
STAR4=(1,2,3,5),STAR5=(9,4,8,3)" TASK="TASK"
lablist stars.out
label-list stars.out

label-replace stars.out star.out "NSTARS = 4" TASK="TASK"
lablist star.out 'full

label-replace stars.out star.out "NSTARS = 3" TASK="TASK"
lablist star.out 'full

label-replace stars.out star.out "NSTARS = 2" TASK="TASK"
lablist star.out 'full

label-replace stars.out star.out "NSTARS = 1" TASK="TASK"
lablist star.out 'full

label-add     stars.out star.out "RAD = 10.22" TASK="TASK"
label-replace star.out stars.out "TARGET = RING" TASK="TASK"
lablist stars.out 'full

if ($syschar(1) = "UNIX")
	ush rm star.out
        ush rm stars.out
else 
	dcl del star.out;*
	dcl del stars.out;*
end-if
end-proc




$ Return
$!#############################################################################

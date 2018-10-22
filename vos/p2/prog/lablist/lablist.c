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

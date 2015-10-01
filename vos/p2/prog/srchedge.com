$!****************************************************************************
$!
$! Build proc for MIPL module srchedge
$! VPACK Version 1.9, Monday, December 07, 2009, 17:04:04
$!
$! Execute by entering:		$ @srchedge
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
$ write sys$output "*** module srchedge ***"
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
$ write sys$output "Invalid argument given to srchedge.com file -- ", primary
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
$   if F$SEARCH("srchedge.imake") .nes. ""
$   then
$      vimake srchedge
$      purge srchedge.bld
$   else
$      if F$SEARCH("srchedge.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake srchedge
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @srchedge.bld "STD"
$   else
$      @srchedge.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create srchedge.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack srchedge.com -mixed -
	-s srchedge.c -
	-i srchedge.imake -
	-p srchedge.pdf -
	-t tstsrchedge.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create srchedge.c
$ DECK/DOLLARS="$ VOKAGLEVE"

/**********************************************************************************************

Program  srchedge.c
Written by Young K. Kwon
For Charlie Avis,  JPL
Date complete:  March 12, 1996

srchedge.c:

INPUT IMAGE:
Only black and white halfword(2 bytes) vicar images are valid.  The input
image only has a dark and light dn value - that is one side of the image is
dark and the other side of the image is light.  A clear distinctive straight
line created by the transition edges(from dark to light) separates the two
values. e.g.


	ddddddddddddddddddddddddll
	dddddddddddddddddddddlllll
	ddddddddddddddddddllllllll
	dddddddddddddddlllllllllll
	ddddddddddddllllllllllllll	d=dark dn value
	ddddddddllllllllllllllllll
	ddddllllllllllllllllllllll	l=light dn value
	dlllllllllllllllllllllllll
	lllllllllllllllllllllllll|
	llllllllllllllllllllllllll
	llllllllllllllllllllllllll
	llllllllllllllllllllllllll


PROCESSING:
srchedge.c does the following:

	1) finds the transition edges(the point where dark dn 
		value and light dn value touch)) 
	2) creates an output image file(vicar image file out) 
		containing only the transition edges
	3) does a least squares fit on the transition edges(done 
		by function fitpts()) 	
	4) and gets the angle of the line create by the least fit square


***********************************************************************************************
***********************************************************************************************
***********************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "vicmain_c"


#define PTDN 20000		/* Dn value of edge in output image out */

/********
#define SMALL_SL 400
#define SMALL_SS 400
#define SMALL_NL 250
#define SMALL_NS 250
**********/

typedef unsigned short  image_element_type; 	 /* input image  element type */

/* Globals */
image_element_type *image;	/* input image array */
unsigned int ptcnt=0;		/* edge count	*/


/*** Finds one transition edge in the row of the input image ***/
void oneedge_row(image_element_type *,unsigned int *, unsigned int *,
		unsigned int, unsigned int, unsigned int, unsigned int);


/*** Finds one transition edge in the column of the input image ***/
void oneedge_col(image_element_type *,unsigned int *, unsigned int *,
		unsigned int, unsigned int, unsigned int, unsigned int, unsigned int);

/*** Fits a set of data points to a line ***/
void fitpts(unsigned int *, unsigned int *, unsigned int *, 
            unsigned int *, unsigned int, unsigned int *, double *, 
            double *, double *, double *, double *);

void main44()
{ 
	int i, j, k, inunit, outunit, status, size, nl, ns, cut;
	int true_ns,true_nl, small_sl, small_ss, small_nl, small_ns;
	char inp[100], out[100], small[11]; 
	unsigned int skipns, skipnl, dark, darkskip;
	image_element_type  *imageptr, *imagetemp, *smallimag;
	int count, def;
	double angle;
	unsigned int *x, *y;
	unsigned int *nx, *ny;
	unsigned int ntos=0, ston=0, wtoe=0, etow=0;


	/* fitpts, a function in dave3.h, output parameters */
	unsigned int fitpts_nndata=0;
	double fitpts_mean,fitpts_sigma, 
		fitpts_angle, fitpts_maxdiff, fitpts_b;


	/* use to find averages of dark and light side of image */
	double ave1=0, ave2=0, slope=0, yint=0, ave=0;
	long unsigned int total1=0,total2=0;
	long unsigned int cnt1=0, cnt2=0, xbeg, xend, ybeg, yend,
			  ibeg,iend;
	unsigned short transitioned=0;



	double parb[500]; /* output angle to vicar pdf parmameter */

	double ratio1, ratio2; /* output ratios to vicar pdf parameters */


        status=zveaction("AS"," ");
	
        status=zvparm("INP",inp,&count,&def,1,0);
        status=zvparm("OUT",out,&count,&def,1,0);
        status=zvparm("SKIPNS",&skipns,&count,&def,1,0);
        status=zvparm("SKIPNL",&skipnl,&count,&def,1,0);
/**        status=zvparm("DIFF",&diff,&count,&def,1,0);**/
	status=zvparm("DARKSKIP",&darkskip,&count,&def,1,0);
/**	status=zvparm("LIGHT",&light,&count,&def,1,0);**/
	status=zvparm("CUT",&cut,&count,&def,1,0);
	status=zvparm("SMALL",small,&count,&def,1,0);
	status=zvpone("SMALLSIZE",&small_sl,1,0);
        status=zvpone("SMALLSIZE",&small_ss,2,0);
        status=zvpone("SMALLSIZE",&small_nl,3,0);
        status=zvpone("SMALLSIZE",&small_ns,4,0);


	status=zvunit(&inunit, "INP", 1, NULL);
        zvsignal(inunit,status,1);
	status=zvunit(&outunit, "OUT", 1, NULL);
        zvsignal(outunit,status,1);
	status=zvopen(inunit, "OP", "READ", "U_FORMAT", "HALF", NULL);
        zvsignal(inunit,status,1);
	status=zvopen(outunit, "OP", "WRITE", "O_FORMAT", "HALF", NULL);
        zvsignal(inunit,status,1);
	status=zvget(inunit, "NL", &nl, "NS" ,&ns, "RECSIZE", &size, NULL);
        zvsignal(inunit,status,1);

	if (skipnl >= nl-2*cut) {
		fprintf(stderr,"\n\nERROR: No of lines in image < SKIPNL\n");
		zabend();
		/***
		exit(10);
		****/
	}
	else if (skipns >= ns-2*cut) {
		fprintf(stderr,"\n\nERROR: No of samples in image < SKIPNS\n"); 
		zabend();
		/****
		exit(10);
		****/
	}
	
	if (nl < 3*cut || ns < 3*cut) {
		fprintf(stderr,"\n\nERROR: Image is to small.  Image size ");
		fprintf(stderr,"must be greater than or equal to %dX%d.\n",
								3*cut,3*cut);
		zabend();
	}
	size = ns*nl;

	if ( 	((image=(unsigned short *)calloc(nl*ns,sizeof(image_element_type))) == NULL ) || 
		((x=(unsigned int *)calloc(nl+ns,sizeof(unsigned int))) == NULL) ||
		((y=(unsigned int *)calloc(nl+ns,sizeof(unsigned int))) == NULL)   ) {
		fprintf(stderr,"\n\nOut of memory(image file might be to big)\n\n");
		zabend();
		/********
		exit(11);
		*********/
	}

	imageptr=image;
	for(i=0; i < nl; i++) {
		status=zvread(inunit,imageptr,"LINE",i+1, NULL);
		zvsignal(inunit,status,1);
		imageptr+=ns;
	}

	imageptr=image;

	if (strcmp(small,"small")==0)  {
		if((smallimag=(unsigned short *)calloc(small_nl*small_ns,
			sizeof(image_element_type))) == NULL) {
	                fprintf(stderr,"\n\nOut of memory(for WACFM)\n");
        	        zabend();
		}

		for(k=0,i=small_sl; i < small_sl+small_nl; i++)
			for(j=small_ss; j <small_ss+small_ns; j++)
				smallimag[k++]=image[i*ns+j];
		image=smallimag;
                true_nl=nl; true_ns=ns;
                nl=small_nl; ns=small_ns;
                skipnl=1; skipns=1; cut=0;

	}
	total1=cnt1=0;
	for(i=cut; i < nl-cut; i+=darkskip)
		for(j=cut; j < ns-cut; j+=darkskip,cnt1++)
			total1+=image[ns*i+j];
			
	dark=1.0*total1/cnt1;

/**fprintf(stderr,"\n\ndark=%d\n\n",dark);**/

	for(j=1+cut; j < ns-1-cut; j+=skipns) {
		if (image[j] > image[ns*(nl-2)+j]) {
			for(i=nl-2-cut; i > 0+cut; i -= skipnl) {
				if (image[ns*i+j] > dark) {
					if (i==nl-2-cut) break;
					oneedge_col(image,x,y,i,j,nl,ns,skipnl);
 					ston++; break;
				}
			}
		}
		else {
                        for(i=1+cut; i < nl-1-cut; i += skipnl) {
				if (image[ns*i+j] > dark) {
                                        if (i==1+cut) break;
                                       	oneedge_col(image,x,y,i,j,nl,ns,skipnl); 
					ntos++; break;
				}
			}
		}
	}


	for(i=1+cut; i < nl-1-cut; i+=skipnl) {
		if (image[i*ns] > image[i*ns+ns-2])  {
			for(j=ns-2-cut; j > 0+cut; j -= skipns) {
				if (image[ns*i+j] > dark) {
					if (j==ns-2-cut) break;
					oneedge_row(image,x,y,i,j,ns,skipns); 
					etow++; break;
				}
			}
		}
		else {
			for(j=1+cut; j < ns-1-cut; j += skipns) {
				if (image[ns*i+j] > dark) {
					if (j==1+cut) break;
					oneedge_row(image,x,y,i,j,ns,skipns);
					 wtoe++; break;
				}
			}
		}
	}
	
	if (strcmp(small,"small") == 0) {
	        image=imageptr;
        	nl=true_nl; ns=true_ns;
		for (i=0; i<ptcnt; i++) {
/*fprintf(stderr,"\n  %d		%d", x[i], y[i]);*/
			x[i]=x[i]+small_ss;
			y[i]=y[i]+small_sl;
		}
	}

/*******
        for(i=0; i < ptcnt; i++) {
                image[ns*y[i]+x[i]-1] = PTDN;
                image[ns*y[i]+x[i]] = PTDN;
                image[ns*y[i]+x[i]+1] = PTDN;
        }
        for(i=0, imageptr=image; i < nl; i++)   {
                status=zvwrit(outunit,imageptr,"LINE",i+1, NULL);
                zvsignal(outunit,status,1);
                imageptr+=nl;
        }
        zabend();

**********/

/********************************************************************/
/***********************dave3.h**************************************/
/********************************************************************/
        if ( ((nx=(unsigned int *)calloc(ptcnt,sizeof(double))) == NULL)  ||
	     ((ny=(unsigned int *)calloc(ptcnt,sizeof(double))) == NULL) ) {
                fprintf(stderr,"\n\nOut of memory\n");
                zabend();
		/********
		exit(11);
		********/
		
        }


	if (ptcnt > 2) fitpts(x,y,nx,ny,ptcnt,&fitpts_nndata,&fitpts_mean,
			&fitpts_sigma,&fitpts_angle,&fitpts_maxdiff,&fitpts_b);
	else if (ptcnt==2) {
		fprintf(stderr,"\nWARNING: only two transition edges discovered\n");
		nx[0]=x[0];
		nx[1]=x[1];
		ny[0]=y[0];
		ny[1]=y[1];
	}
	else  {
		fprintf(stderr,"\n\nWARNING.  %d transition edges found ", ptcnt);
		fprintf(stderr,"(check parameters and/or input image %c)\n\n", inp);
		zabend();
		/*****
		exit(9);
		******/
	}

/****/	angle=fitpts_angle; /******/

	if (wtoe > etow && angle > 0) angle=360-angle;
	else if (wtoe < etow && angle > 0) angle=180-angle;
	else if (wtoe > etow && angle < 0) angle= 180-angle; 
	else if (wtoe < etow && angle < 0) angle = -angle;
	else if (wtoe == 0 && etow==0) 
		if (ntos > ston) angle=180;
		else angle=0;
	
	if (angle < 0) {
		fprintf(stderr,"\n\nERROR: Processing Error\n");
		fprintf(stderr,"Angle is less then zero when it ");
		fprintf(stderr,"should always be zero or greater then zeo.\n");
		zabend();
		/********
		exit(4);
		******/
	}


/**********/	if(angle >= 0 && angle <= 180)  angle=180-angle;
/**********/	else if(angle >=180 && angle <= 360) angle=540-angle;
/**********/

/********************************************************************/
/*************  mark transition edges and output to output file *******/
/**********************************************************************/

        for(i=0; i < fitpts_nndata; i++) {
                image[ns*ny[i]+nx[i]-1] = PTDN;
                image[ns*ny[i]+nx[i]] = PTDN;
                image[ns*ny[i]+nx[i]+1] = PTDN;
        }

       for(i=0, imageptr=image; i < nl; i++)   {
                status=zvwrit(outunit,imageptr,"LINE",i+1, NULL);
                zvsignal(outunit,status,1);
                imageptr+=nl;
        }

/********************************************************************/
/********  outputs to vicar pdf parameter  ***************************/
/********************************************************************/

q_init(&parb,500, P_ABORT);
q_real(&parb, "ANGLE", 1, &angle, P_ADD);
zvq_out(&parb);
	
/********************************************************************/
/********************************************************************/
	return;
} /*end of main*/


void oneedge_col(image_element_type *array, unsigned int *x, 
		unsigned int *y, unsigned int i, unsigned int j, 
		unsigned int nl,unsigned int ns,unsigned int skip)
{
        
        unsigned int beg=0, end=0, mid=0, m=0, *edgearray;
        unsigned int c=0,a=0,b=0,d=0, max=0;
	int something;



        if ((edgearray=(unsigned int *)calloc(skip*2,sizeof(unsigned int))) == NULL) {
                fprintf(stderr,"\n\nOut of memory(SKIPNS might be too big)\n");
                zabend();
		/*********8
		exit(11);
		***********/
        }
        if(i < skip) { beg=j; mid=i; } 
        else    { beg=i*ns+j-(skip*ns); mid=skip; }
        
        if((nl-i-1) < skip) end=i*ns+j+(ns-i-1)*ns;
        else    end=i*ns+j+(ns*skip);
 
        for (c=0,a=beg; a < end; c++, a+=ns)  {
                edgearray[c]=abs(array[a]-array[a+ns]);

	}        

        for (d=0, max=0; d < c-1; d++)  {
                if(edgearray[max] < edgearray[d+1]) 
                        max=d+1;
	}

        for (b=0; b < ptcnt; b++) 
		if (y[b] == i + (max-mid) && x[b] == j) break;  

	if ( b == ptcnt ) { y[ptcnt]=i+(max-mid); x[ptcnt++]=j; }

 }

void oneedge_row(image_element_type *array, unsigned int *x, 
			unsigned int *y, unsigned int i, unsigned int j, 
				unsigned int ns,unsigned int skip)
{
	
	unsigned int beg=0, end=0, mid=0, m=0, *edgearray;
	unsigned int c=0,a=0,b=0,d=0, max=0;


	if ((edgearray=(unsigned int *)calloc(skip*2,sizeof(unsigned int))) == NULL) {
                fprintf(stderr,"\n\nOut of memory(SKIPNS might be too big)\n");
                zabend();
		/******
		exit(11);
		*********/
	}

	if(j < skip) { beg=(i*ns); mid=j; } 
	else	{ beg=i*ns+j-skip; mid=skip; }
 	
	if(ns-1-j < skip) end=(i+1)*ns-1;
	else 	end=i*ns+j+skip;

	for (c=0, a=beg; a < end; c++, a++) 
		edgearray[c]=abs(array[a]-array[a+1]);
	
	for (d=0, max=0; d < c-1; d++) 
		if(edgearray[max] < edgearray[d+1]) 
			max=d+1;

	for (b=0; b < ptcnt; b++) 
/*****/		if (y[b] == i && x[b] == j + (max-mid)) break;	

/*****/	if ( b == ptcnt ) { y[ptcnt]=i; x[ptcnt++]=j+(max-mid); }
}



/********************************************************************
*    Given a set of ndata points x(), y(), fit them to a straight line 
* y= ax + b and angle of the slope angle = atan(,). Return are a,b,angle,
* mean and sigma, nx[] and ny[](good data points)*/


                
        void fitpts(unsigned int *x, unsigned int *y, unsigned int *nx, 
                        unsigned int *ny, unsigned int ndata, 
                        unsigned int *outnndata, double *outmean, 
                        double *outsigma, double *outangle, double *outmaxdiff,
			double *outb)

	{
	double mean,sigma,/**x[1500],y[1500],absres,**/res[1500];
	long int sx,sy,sxy,sxx,sumres,sres,absres;
/**	double sumres,sres;*/
	double maxdiff,newy,a,b/**,ny[1500],nx[1500]**/;
/*	long int maxdiff;
	long int*/double  delta_y;
	double angle,n,d;
	int i,nnd, nndata;

/* input test data points. To be remove after the program have work properly*/
/***
		for (i=0;i<ndata;i++){
fprintf(stderr"x[%d]:%d	y[%d]:%d\n", i, x[i], i, y[i]);
		}
***/
/* copy data into new location*/
		for (i=1;i<=ndata;i++){
			nx[i]=x[i-1];
			ny[i]=y[i-1];
		}
		nndata=ndata;

	recalc_linefit:;

		sx=0; sy=0; sxy=0; sxx=0; sumres=0; sres=0; maxdiff=0; nnd=0;
		for (i=1;i<=nndata;i++){
			sx=sx+nx[i];
			sy=sy+ny[i];
			sxy=sxy+nx[i]*ny[i];
			sxx=sxx+nx[i]*nx[i];
		}
		n=(1.0)*sxy-(1.0)*sx*sy/nndata;
		d=(1.0)*sxx-(1.0)*sx*sx/nndata;
		if(d==0.00000){	goto find_angle;}
		a=n/d;
/****		a=((1.0)*sxy-(1.0)*sx*sy/nndata)/((1.0)*sxx-(1.0)*sx*sx/nndata);*****/
		b=(1.0)*sy/nndata-a*sx/nndata;
		for(i=1;i<=nndata;i++){
				newy=a*nx[i]+b;
				delta_y=sqrt((ny[i]-newy)*(ny[i]-newy));
				res[i]=delta_y*cos(atan2(n,d));
			absres=sqrt(res[i]*res[i]);
			if (absres > maxdiff) { maxdiff=absres;}
			sres = sres + absres;
			sumres = sumres + res[i]*res[i];

		}
		mean=(1.0)*sres/nndata;
		/* sigma=sqrt(sumres/nndata - mean*mean); */

/* test to remove bad data points*/
/******fprintf(stderr,"maxdiff:%f\n2*sigma:%f	sigma:%f\n",
maxdiff, 2*sigma, sigma);
********/
		/* if (maxdiff <= 2*sigma){ goto find_angle;} */
		for (i=1;i<=nndata;i++){
			if (maxdiff > sqrt(res[i]*res[i])) {
				nnd = nnd + 1;
				nx[nnd]=nx[i];
				ny[nnd]=ny[i];}
		}
		if (nnd > 2) nndata=nnd;
		else goto find_angle;
		goto recalc_linefit;

	find_angle:;
		if (d==0.00000){ angle = 90;}
		else {angle=atan2(n,d)*180/3.14159265359;}
/**        	printf("\n angle = %f",angle);
		printf("  deg."); **/

	
	for (i=0; i<nndata; i++) {  
		nx[i]=nx[i+1];
		ny[i]=ny[i+1];
	}

/*fprintf(stderr,"\nnndata: %d\n",nndata);*/

	*outnndata=nndata; *outmean=mean; *outsigma=sigma;
        *outangle=angle; *outmaxdiff=maxdiff; *outb=b;

		}			

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create srchedge.imake
#define  PROGRAM   srchedge

#define MODULE_LIST srchedge.c

#define  MAIN_LANG_C
#define R2LIB

#define  USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create srchedge.pdf
process help=*
LOCAL (DUM1)    TYPE=REAL
PARM INP 	TYPE=STRING  COUNT=1
PARM OUT 	TYPE=STRING  COUNT=1 DEFAULT="srchedge0.img"
PARM SKIPNS 	TYPE=INTEGER COUNT=1 DEFAULT=10
PARM SKIPNL 	TYPE=INTEGER COUNT=1 DEFAULT=10
PARM DARKSKIP 	TYPE=INTEGER COUNT=1 DEFAULT=10
PARM SMALL	TYPE=STRING  COUNT=1 DEFAULT="normal" +
		VALID=("normal", "small")
PARM SMALLSIZE	TYPE=INTEGER COUNT=4 DEFAULT=(400,400,250,250)
PARM CUT	TYPE=INTEGER COUNT=1 DEFAULT=100
PARM ANGLE	TYPE=NAME    DEFAULT=DUM1
END-PROC


.TITLE
VICAR program srchedge


.HELP
PURPOSE:
	To find the angle in degrees of the transition edge of an input image.  
	The transition edge is the line at which a vast difference in DN
	values is found.

EXECUTION:
        
	srchedge inp=in.img out=out.img angle=angle [optional-parameters]

.PAGE

METHOD:

        INPUT IMAGE:

        Only black and white halfword VICAR images are valid.	The input
	image has only dark and light DN values with one side of the image
	dark and the other light.  A distinctive straight line created by
	the transition edge from dark to light or light to dark separates
	the two values.


        ddddddddddddddddddddddddll
        dddddddddddddddddddddlllll
        ddddddddddddddddddllllllll
        dddddddddddddddlllllllllll
        ddddddddddddllllllllllllll      d=dark DN value
        ddddddddllllllllllllllllll
        ddddllllllllllllllllllllll      l=light DN value
        dlllllllllllllllllllllllll
        lllllllllllllllllllllllll|
        llllllllllllllllllllllllll
        llllllllllllllllllllllllll
        llllllllllllllllllllllllll

.PAGE

        PROCESSING:

        Srchedge does the following:

                1) Finds the transition edge where the dark DN value
                   and the light DN value come together.
                2) Creates an output image file that is the same as the
		   input file except the transition edge is marked with DN
		   values of 20,000.
                3) Does a least squares fit on the points making up the
	           transition edge.
                4) Gets the angle of the line created by the least squares
		   fit and returns it in parameter angle.

	The angle is measured as follows:

		1) Preliminary angle A is obtained such that -90.0< A <=90.0
		2) Then there are 6 cases:
			A. Dark side on left  and A>0   =>  angle = 360.0 - A
			B. Dark side on right and A>0   =>  angle = 180.0 - A
			C. Dark side on left  and A<0   =>  angle = 180.0 - A
			D. Dark side on right and A<0   =>  angle = -A
			E. Horizontal w/ dark on top    =>  angle = 180.0
			F. Horizontal w/ dark on bottom =>  angle = 0.0

	Note:  A local variable of type real for parameter angle must be
	       declared by the user.

HISTORY:
Oct 24, 1997	RRD	Improved help section.  Fixed bug on Solaris by
			changing variable cut from unsigned int to int.
			Commented out line in fitpts that calculates sigma
			because it generates DOMAIN error. 
Jul 22, 1997	RRD	Ported to UNIX.
Mar 28, 1996  Y Kwon	Wrote original version.

COGNIZANT PROGRAMMER:  Young Kwon

.LEVEL1
.VARI INP
1 input VICAR image.

.VARI OUT
1 output VICAR image.

.VARI SKIPNS
Number of samples to skip in
searching for transition edge.

.VARI SKIPNL
Number of lines to skip in 
searching for transition edge.

.VARI DARKSKIP
Number of lines and samples to 
skip in calculating the average
DN of the image.

.VARI SMALL
Indicates whether the entire 
image will be searched.

.VARI SMALLSIZE
Indicates the portion of the 
image which will be searched.

.VARI CUT
Shortens the search area of 
the image.

.VARI ANGLE
Output angle in degrees of 
transition edge.  Parameter
is of type real.

.LEVEL2
.VARI INP
The input VICAR halfword image must be black and white and have a darker and
a lighter side.

.VARI OUT
The output image will the same as the input image except that the
transition edge points will be highlighted with a DN of 20,000.

.VARI SKIPNS
This variable designates the number of samples to skip while incrementing
through each input image line to find the transition point form dark to
light or light to dark.

.VARI SKIPNL
This variable designates the number of lines to skip while incrementing
through each input image column to find the transition point form dark to
light or light to dark.

.VARI DARKSKIP
This variable designates the number of lines and samples to skip while
incrementing through the input image to calculate its average DN value.

.VARI SMALL
Indicates whether the entire image or a portion of the image
(indicated by SMALLSIZE) will be searched.  When SMALL is set to 
'SMALL', SMALLSIZE is used.  When SMALL is set to 'NORMAL', 
SMALLSIZE is ignored.

.VARI SMALLSIZE
Indicates the portion of the image which will be searched.
SMALLSIZE must be smaller than the input image size.

.VARI CUT
Another means to shorten the search area of the image.  For example, when
CUT is set to 100 the top-most and bottom-most 100 lines, as well as the
left-most and right-most 100 samples, are ignored in the search for the
transition edge.

.VARI ANGLE
The angle in degrees of the transition edge is output to this parameter.
The user must declare a variable of type real to receive this value.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstsrchedge.pdf
procedure help=*

refgbl $echo
local angle	type=real

body
let _onfail="continue"
let $echo=("yes","no","no")	! echo top level only

gen out=A nl=300 ns=150 format=half linc=0 sinc=0 ival=50
gen out=B nl=300 ns=150 format=half linc=0 sinc=0 ival=150
mss inp=(A,B)  out=C
srchedge inp=C out="tstsrchedge.img" angle=angle
list tstsrchedge.img sl=1 ss=146 nl=10 ns=10
display angle

gen out=A nl=300 ns=300 format=half linc=0 sinc=0 ival=50
f2 inp=A out=B function="(in1 + (100 * (line <= samp)))"
srchedge inp=B out="tstsrchedge.img" angle=angle
list tstsrchedge.img sl=1 ss=1 nl=10 ns=10
display angle

end-proc

.help

	This is the test procedure for srchedge.  An angle of
	270.0 degrees should be output after the first call
	to srchedge and an angle of 224.989259055 degrees 
	should be ouptut the second time.

.end
$ Return
$!#############################################################################

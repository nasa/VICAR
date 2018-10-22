
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


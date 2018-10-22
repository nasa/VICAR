/*************************************************************************/
/* Bridge for FORTRAN access                                             */
/*************************************************************************/
int splot_(x,y,n,xlo,xhi,ylo,yhi,xtic,ytic,mode,grid1)
{
   splot(x,y,n,xlo,xhi,ylo,yhi,xtic,ytic,mode,grid1);
}

/**************************************************************************/


#include <stdio.h>
#define	 XMAX  780
#define  YMAX  370
#define	 XMIN  100
#define  YMIN   10
#define  TICLEN	5
#define	 BUFSIZE 132


char *splotlabelr(r) float r;
   {
   static char buf[20];
   sprintf(buf,"%9.5g",r);

   return buf;
   }

int splot(x,y,n,xlo,xhi,ylo,yhi,xtic,ytic,mode,grid1)
   float *x,	/* array of x data points */
	 *y,	/* array of y data plints */
	 *xlo,	/* min x value		  */
	 *xhi,	/* max x value		  */
	 *ylo,	/* min y value		  */
	 *yhi;	/* max y value		  */
   int	 *n,	/* number of points	  */
	 *xtic,	/* number of x tickmarks  */
	 *ytic,	/* number of y tickmarks  */
	 *mode, /* mode of plotting 	  */
         *grid1; /* 0=nogrid 1=grid	  */
/*
 *	Plots data on screen of VT240 terminal 
 *
 *		Programmer Stan Schultz
 *		Startdate  ?/?/84
 *		modified   8/20/85	SDS to check if last string = string
 *		modified   1/14/86	SDS to add grid option
 *              modified   1/26/87      REA adapt to be a VICAR subroutine
 *		modified   4/8/87	REA initialize character size
 *
 *		Mode is an integer that is defined as follows
 *
 *			mode =-1	draw continuious line
 *			mode = 0	draw continuious line
 *			mode = 1	break line when y=ylo
 *			mode = 2	point plot
 */
{
   int	i,j,ix,iy,nx,ny,out,grid,lastx,lasty;
   float rangex,rangey,offx,offy,t;
   char esc=27;
   char s[BUFSIZE],c;

   rangey = (*yhi-*ylo);
   offy   = (*ylo);
   rangex = (*xhi-*xlo);
   offx   = (*xlo);
   if(*grid1!=0)
      grid = 1;
   else
      grid = 0;

   if(*xtic>0 && *ytic>0)
      printf("\033[2J\033[0;0H",esc,esc);		/* clear screen */
   printf("%cP1pS(A[0,0][799,479])W(P1)T(S1);",esc);	/* enter regis */
   if(*xtic>0 && *ytic>0)
      {
      printf("P[%d,%d]V[%d,%d]V[%d,%d]",XMIN,YMIN,XMAX,YMIN,XMAX,YMAX);/* draw*/
      printf("V[%d,%d]V[%d,%d]P[%d,%d]",XMIN,YMAX,XMIN,YMIN,XMIN,YMAX);/* box */
      for(j=grid;j<2;j++)			/*  X tickmarks */
         {
         int iy2;
         iy = j*(YMAX-YMIN-TICLEN*2) + YMIN;
         if(grid)
            {
            iy  = YMAX;
            iy2 = YMIN;
            }
         else
            iy2 = iy +TICLEN*2;
         for(i=0;i<=*xtic;i++)
            {
            ix = XMIN + i*(XMAX-XMIN)/(*xtic);
            printf("\n;P[%d,%d]V[%d,%d]",ix,iy,ix,iy2);
            if(j==1)
               {
	       t = offx + i * rangex/(*xtic);
	       printf("\n;P[%d,%d]T\"%s\"",ix-80,iy+20,splotlabelr(t));
               }
            }
         }

      for(j=grid;j<2;j++)			/* Y tickmarks */
         {
         int ix2;
         ix = j*(XMAX-XMIN-TICLEN) + XMIN;
         if(grid)
            ix2 = XMIN;
         else
            ix2 = ix+TICLEN;
         for(i=0;i<=*ytic;i++)
            {
            iy = YMIN + i*(YMAX-YMIN)/(*ytic);
            printf("\n;P[%d,%d]V[%d,%d]",ix,iy,ix2,iy);
            if(j==1)
               {
               t = offy + (*ytic-i) * rangey/(*ytic);
               printf("\n;P[%d,%d]T\"%s\"",5,iy-7,splotlabelr(t));
               }
            }
         }
      }

   s[0]=0;
   out=0;/* not out of bounds */
   for(i=0;i<*n;i++)				/* plot data at given scale */
      {
      if(c=='Q')
          break;
      iy = YMAX - (y[i]-offy) * (YMAX-YMIN) /rangey;
      ix = XMIN + (x[i]-offx) * (XMAX-XMIN) /rangex;
      if(i!=0 && ix==lastx && iy==lasty)
         continue;
      else
         {
         lastx=ix;
         lasty=iy;
         }
      if(iy<YMIN || iy>YMAX || ix<XMIN || ix>XMAX)/* point is out of bounds */
         out++;
      if(out)
         {
         if(iy < YMIN) iy = YMIN;
         if(iy > YMAX) iy = YMAX;
         if(ix < XMIN) ix = XMIN;
         if(ix > XMAX) ix = XMAX;
         if(out>1 && i<*n-1)   /* if not first out of bounds point */
            {
            ny = YMAX - (y[i+1]-offy) * (YMAX-YMIN) /rangey;
            nx = XMIN + (x[i+1]-offx) * (XMAX-XMIN) /rangex;
            if(nx<YMIN || ny>YMAX || nx<XMIN || nx>XMAX) /* if next point is */
               continue;                                 /* also off screen  */
            out=0;                             /* if next point is on screen */
            }
         }
      if(i==0)
         printf("P[%d,%d]",ix,iy);
      if(out<=1)
         {
         s[0]=0;j=0;
         switch (*mode)
            {
            case 2 : sprintf(s,"P[%d,%d]V[%d,%d]",ix,iy,ix,iy);
	             break;
	    case 1 : if((y[i]==offy))	/* break line */
	                break;
	             if((i!=0) && (y[i-1]==offy))
		        j=sprintf(s,"P[%d,%d]",ix,iy);
                     if(y[i]==offy)
                        break;
            case 0 : 
	    case -1: sprintf(s+j,"V[%d,%d]",ix,iy);
	             break;
            }
         puts(s);	/* send command */
         }
      }

   printf("%c\\%c[22;1H",esc,esc);		/* exit regis */
   return;
}

$!****************************************************************************
$!
$! Build proc for MIPL module metropolis
$! VPACK Version 1.9, Monday, December 07, 2009, 16:27:27
$!
$! Execute by entering:		$ @metropolis
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
$ write sys$output "*** module metropolis ***"
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
$ write sys$output "Invalid argument given to metropolis.com file -- ", primary
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
$   if F$SEARCH("metropolis.imake") .nes. ""
$   then
$      vimake metropolis
$      purge metropolis.bld
$   else
$      if F$SEARCH("metropolis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake metropolis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @metropolis.bld "STD"
$   else
$      @metropolis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create metropolis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack metropolis.com -mixed -
	-s metropolis.f zmetropolis.c -
	-i metropolis.imake -
	-t tmetropolis.f tzmetropolis.c tmetropolis.imake tmetropolis.pdf -
	   tstmetropolis.pdf -
	-o metropolis.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create metropolis.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      subroutine metropolis(cost,narg,array,array2,range,numten,answer,
     +                       limits,norm,npts,prnt,ind)

C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C	VICAR SUBROUTINE                                       METROPOLIS
C       ----------------                                       ----------
C	Anneals the cost function by iterative guessing of ANSWER using
C       a monte carlo generator with continually decreasing bounds.
C
C	Fortran format of call:
C
C	CALL metropolis(cost,narg,array,array2,range,numten,answer,
C                       limits,norm,npts,prnt,ind)
C
C	"C" format of call:
C
C	zmetropolis(cost,narg,array,array2,range,numten,answer,
C                  limits,norm,npts,prnt,ind)
C
C	Parameters:-
C
C          COST  A function provided by the user (see test examples).
C          NARG  The number of variables. (Integer)
C          ARRAY An array of data. (Float)
C          ARRAY2 Another array of data. (Float)
C          RANGE Bounds of solution vector elements. (Integer) 
C          NUMTEN The number of iterations to reduce the error by 10. (Integer)
C          ANSWER The solution vector. (Float could be array)
C          LIMITS The total number of iterations permitted. (Integer)
C          NORM The number of iterations between normalizations. (Integer)
C          NPTS The number of points in ARRAY & ARRAY2. (Integer)
C          PRNT The number of iterations between printouts. (Integer)
C          IND  Status indicator. (Integer)
C
C   REVISION HISTORY
C
C      14-05-94   CRI  MSTP S/W Conversion (VICAR Porting)
C      19-05-95   CRI  Correction per FR 85782
C                      See help file for "C" bridge support
C      14-07-95   CRI  Correction per FR 87256 Removed hard coded seed line
C
C   REQUIRED SUBROUTINE cost (provided by the user):
C
C     Returns the cost function resulting from guessing x (answer).
C     This could be any function or logical operation.
C     The user supplies this subroutine. The purpose of COST is
C     to return a penalty measure which METROPOLIS uses as it tries
C     to feel it's way to the solution by guessing at values for
C     the solution vector X. METROPOLIS will select that solution vector
C     which results in the smallest cost.
C
C	Fortran format of subroutine:
C
C          subroutine cost(x,array,array2,n,error,ind)
C
C	"C" format of subroutine:
C
C          zcost(x,array,array2,n,error,ind)
C               The name zcost may be arbritary in "C", but must be 
C               defined as subroutine name when passed to the 
C               zmetropolis bridge. (See Test Cases).
C   
C	Parameters:-
C
C          X     is the solution vector. REAL (length known by COST).
C          ARRAY is an array of N data points (if needed). REAL
C          ARRAY2 is another array of N elements if needed. REAL
C          N      is the number of elements/data points in ARRAY & ARRAY2.
C          ERROR is the returned cost. REAL scalar.
C          IND   is 0 for normal return, 1 for abnormal return.
C          If METROPOLIS senses an indicator of 1 it will generate another
C                guess somewhere else & try again.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      integer ind,npts,prnt,narg,limits,numten,norm
      integer fail,limit,j,loop,loop1,loop2,loop3,numreset,k
      real pi,pi2,scale,costsum,c1,c2,c3,mincost,boltzman,energy
      real*4 range(narg),answer(narg),prob
      real*4 array(1),array2(1)
      real*4 temp(10),x(10),minx(10)
      character*80 msg  
      integer*4 SEED
      REAL*4 randnum

      external cost

      pi=3.14159
      pi2=pi/2.0
      limit=limits

c  Compute a random number seed based on the time of day

       call get_seconds(SEED)
c  Compute the cost at position ANSWER and assign to variable C1.

      call cost(answer,array,array2,npts,c1,ind)
      if(ind.ne.0)then
         call xvmessage('Failure in COST function at initial guess',' ')
         return
      endif

c  Save the cost in case the user had good reason to inspect this
c  solution position.
      mincost=c1
      do j=1,narg
         minx(j)=answer(j)
      enddo

c  Set initial temperatures to the range estimates.

      do j=1,narg
         temp(j)=range(j)
      enddo

      fail=0
      loop=1
      loop1=0
      loop2=0
      loop3=0
      numreset=numten/10
      scale=exp((log(0.1))/real(numten))

      if(prnt.gt.0)then
          call xvmessage(' ',' ')
          write (msg, 900) scale
900       format ('scale=', f15.7)
          call xvmessage(msg,' ')
          msg(1:49)='Solution  Temperature     Cost  #downhill #uphill'
          msg(50:72)=' #rejected #outofbounds'
          call xvmessage(msg,' ')
          call xvmessage(' ',' ')
      endif

c   MAIN LOOP: loop on number of successful changes in solution space. 
      do while(loop.lt.limit)

c       Compute the delta_cost/temperature ratio for
c       normalization of probabilities.
c       Note that this is the Boltzmann constant for this 'system'.

        if(mod(loop,norm).eq.1)then
           costsum=0.0
           k=0
           do j=1,narg
              x(j)=answer(j)
           enddo
           do j=1,narg
              x(j)=answer(j)-temp(j)
              call cost(x,array,array2,npts,c2,ind)
              if(ind.eq.0)then
                k=k+1
                costsum=costsum+abs(c1-c2)
              endif
              x(j)=answer(j)+temp(j)
              call cost(x,array,array2,npts,c2,ind)
              if(ind.eq.0)then
                k=k+1
                costsum=costsum+abs(c1-c2)
              endif
              x(j)=answer(j)
           enddo
           if(k.eq.0) then
              call xvmessage(' ',' ')
              call xvmessage('Failure in normalization procedure',' ')
              call xvmessage(' ',' ')
              call xvmessage('solution + - range outofbounds',' ')
              call xvmessage(' ',' ')
              return
           endif              
           boltzman=5.0*(costsum/k)/temp(1)
           if(prnt.gt.0) then
              write (msg,930) boltzman
930           format ('Boltzmann = ',f15.7)
              call xvmessage(msg,' ')
           endif
        endif
                     
c       Decrement the temperature according to the multiplicative
c       cooling schedule.

        do j=1,narg
           temp(j)=temp(j)*scale
        enddo
        energy=boltzman*temp(1)

c       Compute a solution space guess using a Cauchy-Lorentzian
c       random probability distribution function.

91      do j=1,narg
           call rangen(SEED,randnum)
           x(j)=temp(j)*tan(pi*randnum+pi2)+answer(j)
        enddo
        call cost(x,array,array2,npts,c2,ind)
        if(ind.ne.0)then
           loop3=loop3+1
           goto 91
        endif

        if(c2.lt.c1)then

c           Accept lower cost position.
c           We always accept a downhill cost route if offered.

            c1=c2
            do j=1,narg
               answer(j)=x(j)
            enddo
            loop1=loop1+1

        else
c           Compute probability of accepting higher cost position.
c           This comes from the Boltzmann probability of our system 
c           transitioning from energy state c1 to energy state c2.

            c3=(c2-c1)/energy
            if(c3.gt.50.)then
               fail=fail+1
               goto 91
            endif
c           prob=1.0/(1.0+exp(c3))
            prob=1.0/exp(c3)

c           Evaluate the probability by comparing it against chance.
            call rangen(SEED,randnum) 
            if(prob.gt.randnum) then
c               Accept higher cost position.
                c1=c2
                do j=1,narg
                   answer(j)=x(j)
                enddo
                loop2=loop2+1
            else
c               Reject higher cost position.
                fail=fail+1
                goto 91
            endif
        endif

c       Save the minimum cost and associated solution as we go.

        if(c1.lt.mincost)then
            mincost=c1
            do j=1,narg
               minx(j)=answer(j)
            enddo
        endif

c       Reset the solution pointer to the minimum cost
c       location every numreset successful iterations.

        if(mod(loop,numreset).eq.0)then
            c1=mincost
            do j=1,narg
               answer(j)=minx(j)
            enddo
        endif

        loop=loop+1

c       Print out a status every PRNT iterations.

        if(prnt.gt.0)then
           if(mod(loop,prnt).eq.0)then
               write(msg,100)answer(1),temp(1),c1,loop1,loop2,fail,loop3
               call xvmessage(msg,' ')
               if(narg.gt.1)then
                  do j=2,narg
                      write(msg,100) answer(j),temp(j)
                      call xvmessage(msg,' ')
                  enddo
               endif
100            format(3(1x,g11.5),4(i5,3x))
               loop1=0
               loop2=0
               loop3=0
               fail=0
           endif
        endif

      enddo
      call xvmessage(' ',' ')
c     END of MAIN LOOP

c     Put minimum solution into ANSWER & it's cost into
c     RANGE.

      do j=1,narg
         answer(j)=minx(j)
      enddo
      range(1)=mincost

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zmetropolis.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

void (* cost)();   /* the cost function to be called */
/************************************************************************/
/* FORTRAN-Callable Version: cost  -                                    */
/************************************************************************/

void FTN_NAME2(zzcost, ZZCOST) (answer, array, array2, npts, retcost, ind)

float *answer;       /* The solution vector */
float *array;     /* An array of data */
float *array2;    /* Another array of data */
int *npts;         /* The number of points in array and array2 */
float *retcost;	  /* The returned cost */
int *ind;         /* Status indicator */

{

(* cost)(answer, array, array2, *npts, retcost, ind);

}


/************************************************************************/
/* C-Callable Version: zmetropolis -                                    */
/************************************************************************/

void zmetropolis(costz,narg, array, array2, range, nten, ans, lim, 
                   norm, npts, prnt, ind)
void (* costz)();   /* the cost function to be called */
int narg;         /* The number of variables */  
void *array;      /* An array of data */
float *array2;    /* Another array of data */
float *range;     /* Bounds of solution vector elements */
int nten;         /* The number of iteration to reduce the error by 10 */
float *ans;       /* The solution vector */
int lim;          /* The total number of iterations permitted */
int norm;         /* The number of iterations between normalizations */
int npts;         /* The number of points in array and array2 */
int prnt;	  /* The number of iterations between printouts */
int *ind;         /* Status indicator */

{
void (* costp)();      /* The bridge cost function to be called */
costp = FTN_NAME2(zzcost, ZZCOST);
cost = costz;        /* The cost function to be called */
FTN_NAME2(metropolis,METROPOLIS)(costp, &narg, array, array2, range, &nten, ans,
                   &lim, &norm, &npts, &prnt, ind); /* invoke metropolis */

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create metropolis.imake
/***********************************************************************

                IMAKE FILE FOR VICAR SUBROUTINE metropolis

   To Create the build file give the command:

		$ vimake metropolis			(VMS)
   or
		% vimake metropolis			(Unix)


************************************************************************/

#define SUBROUTINE metropolis

#define MODULE_LIST metropolis.f zmetropolis.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tmetropolis.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

c EXAMPLE # 1
c Purpose: To test the Metropolis algorithm in three dimensions.
c The example below is to fit a circle to 6 data points, two
c of which do not lie on the circle.
c Notice the cost function rewards for including lots of data
c points and is allowed to reject points which fall far from the
c fitted function.
      real*4 range(3),answer(3),array(2,6),array2(1)
      character*80 msg
      external cost
      numten=1000
      limit=3000
      iprint=200
      narg=3
      norm=330
      npts=6
c sample,line pairs...
      data array/10.,50.,30.,50.,10.,70.,30.,70.,80.,30.,80.,40./
c (1)=radius (2)=x_center (3)=y_center
      data answer/30.,60.,50./
      data range/15.,20.,20./
      call xvmessage('Fortran test case 1',' ')

      call metropolis(cost,narg,array,array2,range,numten,answer,
     +                 limit,norm,npts,iprint,ind)
      if(ind.eq.0)then
         write(msg,*)'answer=',(answer(j),j=1,narg)
         call xvmessage(msg,' ')
      else
         call xvmessage('no solution',' ')
      endif
      call xvmessage('solution should be about 14.1 20. 60.',' ')
      call xvmessage(' ',' ')
C
C     The "C" is not portable on all machines and therefore the
C     following:' CALL TZMETROPOLIS ' is commented our of the delivered
C     test procedure 
C
C      CALL TZMETROPOLIS 
C
      call example2
      end


      subroutine  cost(x,array,array2,n,error,ind)
c Returns the cost function resulting from guessing x (answer).
c This could be any function or logical operation.

c X     is the solution vector.
c ARRAY is an array of N data points if needed.
c ARRAY2 is another array of N elements if needed.
c N      is the number of elements/data points in ARRAY & ARRAY2.
c ERROR is the returned cost.
c IND   is 0 for normal return, 1 for abnormal return.
c        If METROPOLIS senses an indicator of 1 it will generate another
c        guess somewhere else & try again.

c (1)=radius (2)=x_center (3)=y_center
      real*4 array(2,n),array2(n),x(4),dr(10)

      ind=1
      if(x(1).lt.5.)return
      if(x(1).gt.50.)return
      if(x(2).gt.100.)return
      if(x(3).gt.100.)return
      if(x(2).lt.1.)return
      if(x(3).lt.1.)return
      sumdr=0.
      sum=0.
      range=20.
      m=0
      do j=1,n
        dr(j)=abs(x(1)-sqrt((array(1,j)-x(2))**2+(array(2,j)-x(3))**2))
        sum=sum+dr(j)
        if(dr(j).lt.range)then
           m=m+1
           sumdr=sumdr+dr(j)
        endif
      enddo
      if(m.eq.0)then
         error=sum/n + range/n
      else
         error=sumdr/m + range/m
      endif
      ind=0
      return
      end

      subroutine example2
c EXAMPLE # 2
c purpose to test the Metropolis algorithm in one dimension.
c The example below is to find the LOWEST minimum of the
c polynomial expressed by the COST function found at the
c end of this example.
      real*4 range(1),answer(1),array(1),array2(1)
      character*80 msg
      external e2cost
      range(1)=2.0
      numten=500
      answer(1)=3.
      limit=1500
      iprint=200
      narg=1
      norm=200
      npts=0
      call xvmessage('Fortran test case 2',' ')
      call metropolis(e2cost,narg,array,array2,range,numten,answer,
     +                 limit,norm,npts,iprint,ind)
      write(msg,*)'answer=',answer(1)
      call xvmessage(msg,' ')
      call xvmessage('solution should be about -2.9',' ')
      return
      end


      subroutine e2cost(x,array,array2,n,error,ind)
      real*4 array(1),array2(1),x(1)
      ind=0
      error=x(1)**4-16*x(1)**2+5*x(1)
      return
      end
$!-----------------------------------------------------------------------------
$ create tzmetropolis.c
#include "xvmaininc.h"
#include <math.h>
#include "ftnbridge.h"
/**************************************************************************/
/* Example 2 Cost routine                                                 */
/* Returns the cost function resulting from guessing x (answer).          */
/* This could be any function or logical operation.                       */
/*                                                                        */
/* X     is the solution vector.                                          */
/* ARRAY is an array of N data points if needed.                          */
/* ARRAY2 is another array of N elements if needed.                       */
/* N      is the number of elements/data points in ARRAY & ARRAY2.        */
/* ERROR is the returned cost.                                            */
/* IND   is 0 for normal return, 1 for abnormal return.                   */
/*        If METROPOLIS senses an indicator of 1 it will generate another */
/*        guess somewhere else & try again.                               */
/*                                                                        */
/**************************************************************************/

void zcost2 (x, array, array2, n, error, ind) 

float *x;         /* The solution vector */
float *array;     /* An array of n data points, if needed */
float *array2;    /* Another array of n data points, if needed */

int n;              /* The number of data points in array and array2 */  
float *error;       /* The returned cost */
int *ind;           /* Status indicator */
{
  float xvp;
  double err,dxvp;
  dxvp = *(x);
  *ind=0;
  err=pow(dxvp,4.) - 16. * pow(dxvp,2.) + 5. * (dxvp);
  xvp = err;
  *error = xvp;
}

/**************************************************************************/
/* Example 1 Cost routine                                                 */
/* Returns the cost function resulting from guessing x (answer).          */
/* This could be any function or logical operation.                       */
/*                                                                        */
/* X     is the solution vector.                                          */
/* ARRAY is an array of N data points if needed.                          */
/* ARRAY2 is another array of N elements if needed.                       */
/* N      is the number of elements/data points in ARRAY & ARRAY2.        */
/* ERROR is the returned cost.                                            */
/* IND   is 0 for normal return, 1 for abnormal return.                   */
/*        If METROPOLIS senses an indicator of 1 it will generate another */
/*        guess somewhere else & try again.                               */
/*                                                                        */
/* (1)=radius (2)=x_center (3)=y_center                                   */
/**************************************************************************/

void zcost (x, array, array2, n, error, ind) 

float *x;         /* The solution vector */
float *array;     /* An array of n data points, if needed */
float *array2;    /* Another array of n data points, if needed */

int n;              /* The number of data points in array and array2 */  
float *error;       /* The returned cost */
int *ind;           /* Status indicator */

{
   double dr[10], d1, d2, d3;
   float *mpa, *xp, ferr;
   double sumdr, sum, range, m, derr;
   int j, nc; 
   *ind = 1;
   xp = x;
   if (*xp >= 5 && *xp <= 50)
      {
      ++xp;
      if (*xp >= 1 && *xp <= 100)
         {
         ++xp;
         if (*xp >= 1 && *xp <= 100)
            {
            sumdr = 0;
            sum = 0;
            range = 20;
            m = 0;
            nc = n;
            xp = x;
            mpa = array;
            for (j=0;j<nc;j++)
               {
               d1 = *xp;
               d2 = *(mpa + j) - *(xp + 1);
               d3 = *(mpa + (j + nc)) - *(xp + 2);
               dr[j] = fabs(d1 - sqrt(pow(d2,2.) + pow(d3,2.)));
               sum=sum+dr[j];
               if(dr[j]<range)
                  {
                  m++;
                  sumdr=sumdr+dr[j];
                  }
               }
            if(m==0) derr = sum/nc + range/nc;
            else derr = sumdr/m + range/m;
            ferr = derr;
            *error = ferr;
            *ind=0;
            }
         }
      }
}

/********************************************************************/
/* EXAMPLE # 2                                                      */
/* purpose to test the Metropolis algorithm in one dimension.       */
/* The example below is to find the LOWEST minimum of the           */
/* polynomial expressed by the COST function found at the           */
/* end of this example.                                             */
/********************************************************************/
zexample2 () 
{

void (*costp)();     /* The cost function to be called */
int narg;           /* The number of variables */  
float array[1];     /* An array of data */
float array2[1];    /* Another array of data */
float range[1];     /* Bounds of solution vector elements */
int numten;         /* The number of iteration to reduce the error by 10 */
float answer[1];    /* The solution vector */
int limit;          /* The total number of iterations permitted */
int norm;           /* The number of iterations between normalizations */
int npts;           /* The number of points in array and array2 */
int iprint;	    /* The number of iterations between printouts */
int ind;            /* Status indicator */
int *indp;
char msg[80], *mp;
void *ap;

numten=500;
limit=1500;
iprint=200;
narg=1;
norm=200;
npts=0;
mp=msg;
answer[0] = 3;
range[0] = 2;
indp = &ind;
costp = *zcost2;
ap = array;
zvmessage("C test case 2","");
zmetropolis(costp,narg,ap,array2,range,numten,answer,
                       limit,norm,npts,iprint,indp);

(void) sprintf(mp,"answer= %f\n", *answer); 
zvmessage(msg,""); 
zvmessage("solution should be about -2.9","");
}


/************************************************************************/
/*  Main Test routine for the "C" call to metropolis. This routine      */
/*  builds an array of two dimensions and invokes the "C" bridge        */
/*  zmetropolis.  The bridge will set up to reverse bridge to the zcost */
/*  routine associated with this invocation.                            */
/************************************************************************/

/********************************************************************/
/*  EXAMPLE # 1                                                     */
/*  Purpose: To test the Metropolis algorithm in three dimensions.  */
/*  The example below is to fit a circle to 6 data points, two      */
/*  of which do not lie on the circle.                              */
/*  Notice the cost function rewards for including lots of data     */
/*  points and is allowed to reject points which fall far from the  */
/*  fitted function.                                                */
/********************************************************************/

void FTN_NAME(tzmetropolis)(void)
{

void (*costp)();     /* The cost function to be called */
int narg;           /* The number of variables */  
float array[2][6];  /* An array of data */
float array2[1];    /* Another array of data */
float range[3];     /* Bounds of solution vector elements */
int numten;         /* The number of iteration to reduce the error by 10 */
float answer[3];    /* The solution vector */
int limit;          /* The total number of iterations permitted */
int norm;           /* The number of iterations between normalizations */
int npts;           /* The number of points in array and array2 */
int iprint;	    /* The number of iterations between printouts */
int ind;            /* Status indicator */
int *indp, j;
char msg[80], *mp;
float *answerp;
void *ap;

numten=1000;
limit=3000;
iprint=200;
narg=3;
norm=330;
npts=6;
mp=msg;
answerp=answer; 
ap=array;
/* sample,line pairs... */

array[0][0] = 10;
array[0][1] = 30;
array[0][2] = 10;
array[0][3] = 30;
array[0][4] = 80;
array[0][5] = 80;

array[1][0] = 50;
array[1][1] = 50;
array[1][2] = 70;
array[1][3] = 70;
array[1][4] = 30;
array[1][5] = 40;

/* (1)=radius (2)=x_center (3)=y_center */

answer[0]=30;
answer[1]=60;
answer[2]=50;

range[0]=15;
range[1]=20;
range[2]=20;

indp = &ind;
costp = *zcost;
zvmessage("C test case 1","");

zmetropolis(costp,narg,ap,array2,range,numten,answer,
                       limit,norm,npts,iprint,indp);

if (ind==0)
    {
        (void) sprintf(mp,"answer= ");
        mp=(mp+8); 
        for (j=0; j<narg; j++)
        {
           (void) sprintf(mp," %14.5f",*(answerp+j));
           mp=(mp+14);
        }
        zvmessage(msg,"");
    }
else zvmessage("answer = no solution","");
zvmessage(" solution should be about 14.1 20. 60.","");

zexample2();  /* try example 2 */

}


$!-----------------------------------------------------------------------------
$ create tmetropolis.imake
/* Imake file for Test of VICAR subroutine METROPOLIS */

#define PROGRAM tmetropolis

#define MODULE_LIST tmetropolis.f tzmetropolis.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL


#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
$!-----------------------------------------------------------------------------
$ create tmetropolis.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstmetropolis.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
tmetropolis
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create metropolis.hlp

1  METROPOLIS

      subroutine metropolis(cost,narg,array,array2,range,numten,answer,
     +                       limits,norm,npts,prnt,ind)
       May be called from Fortran or "C". The "C" bridge is zmetropolis
       and uses the same arguments.  

       NOTE: The "C" bridge has been tested on the following systems:
		MIPL3, MIPL5, WASATCH, ANDES and ARSIA
	     Assume that the "C" bridge is NOT a supported feature on 
             ALL other systems.


  COST  A function provided by the user.
  NARG  The number of variables.
  ARRAY An array of data.
  ARRAY2 Another array of data.
  RANGE Bounds of solution vector elements.
  NUMTEN The number of iterations to reduce the error by 10.
  ANSWER The solution vector.
  LIMITS The total number of iterations permitted.
  NORM The number of iterations between normalizations.
  NPTS The number of points in ARRAY & ARRAY2.
  PRNT The number of iterations between printouts.
  IND  Status indicator. 


  PURPOSE:

 Anneals the cost function by iterative guessing of ANSWER using
 a monte carlo generator with continually decreasing bounds.


 This is an iterative numerical method for finding the coefficients of
 a function which minimize that function. There are two versions of this
 routine:
    (1)   In Numerical Recipes page 326, entitled: Simulated Annealing.
          To be used for combinatoric minimization of "goals".
    (2)   This case.
          To be used with mathematical expressions.

 There are all kinds of ways to use this subroutine. Two of them
 are:
    (1)   When you are trying to find the minimum of an N dimensional
          function whose coefficients you already know.
    (2)   When you are trying to determine the coefficients of a
          function when you know the form of the function and
          you have a bunch of data points, some of which, fit the
          function.

 In either event the process you must follow is to generate a FORTRAN
 subroutine called COST which returns a cost or penalty. This cost
 reflects the penalty imposed by assuming certain (incorrect) values
 for the coefficients (or the minimum position of the function).
 METROPOLIS will solve for the coefficients (or minimum) by guessing
 the solution vector and using the generated cost as a guide. It
 will return the solution vector which produces the lowest cost.

 These are the advantages of this scheme:

 1. You need not solve the inverse problem.
 2. The function need not be continuous or even have derivatives.
 3. You can generate a cost function which is as complicated 
    as required to express what you really want, ie: any constrained
    least squares operation is possible.
 4. The function can be N dimensional and wildly non linear.
 5. If you 'cool' slowly enough the solution will be for the
    LOWEST existing minimum possible, not the nearest to
    where you began.
 6. It doesent care if the solution is exact, ie: if some points
    have nothing to do with the function being minimized.

 These are the disadvantages:

 1. It requires many iterations, usually a couple of thousand
    in order to generate a model of the solution space.
    This is NOT an efficient least squares routine. If other
    (linear) methods exist use them instead.
 2. There is no guarantee that the solution will be correct
    if you are greedy and 'cool' too fast.
 
2  METHOD:

 The method interprets your problem as though it were a problem
 in statistical mechanics. You begin with a guess at the 'solution'
 and with a range about the solution where you expect the real
 solution to be found. The initial ranges are interpreted as
 temperatures. A random number generator produces guesses for the
 solution vector using a Cauchy Lorentzian pdf of the form:
    guess=temperature * tan (pi * randomnumber + pi/2) + lastguess
 A new cost is computed at the new guess location. If that cost
 is less than the previous one then we adopt the new solution.
 If the cost is higher we consult the Boltzmann probability
 distribution to determine the probability of an uphill excursion.
 This is computed from:
    probability= 1/ exp(uphillcost/(Boltzmann*temperature))
 If the probability > randomnumber then we adopt the new solution
 otherwise we abandon it. 
 At regular intervals we force the solution to return to the point
 where the smallest cost was discovered.
 Each successful iteration we decrease the temperature by:
   newtemp=oldtemp*scale
 where scale is the solution to :
   scale**n=1/10 where n is the number of iterations required to
   reduce the search range by one order of magnitude.

 PROPERTIES OF THIS METHOD:

 Initially the solution guesses fall over a large area of the solution
 space. As the temperature falls the solution generating function
 constricts solutions to ever decreasing regions. The Cauchy Lorentzian
 pdf allows occasional long jumps which provide an escape route out
 of local minima ,ie: the analogy of tunneling in physics. 
 Most guesses form a gaussian distribution and as the range of a guess
 decreases the precision increases. A memory of the lowest cost is
 maintained and occasionally the solution pointer is forced to revisit
 this location. This is equivalent to saying that in a statistical 
 situation where ANY molecule can find the hole in the bucket
 we, as serial computing machines, reserve the right to know about it.


2  ARGUMENTS:

      subroutine metropolis(cost,narg,array,array2,range,numten,answer,
     +                       limits,norm,npts,prnt,ind)

 COST is an input subroutine which returns a cost or
      penalty resulting from guessing the solution vector ANSWER.
      Cost is a function of vector ANSWER and of any data in ARRAY
      and ARRAY2 which it feels like making use of.
      See COST writeup following this writeup.

 NARG is the number of arguments in the ANSWER and RANGE vectors.
      INTEGER*4.

 ARRAY is an array of data points for COST to use in computing
       the cost. It is of length NPTS.

 ARRAY2 is another array of data points for COST to use in computing
       the cost. It is also of length NPTS.

 RANGE is the expected range about each initial guess ANSWER.
       It need not be exact. It provides a general feel for how
       broad the solution space is for elements of ANSWER.
       It is a vector of length NARG. On return range(1) contains
       the cost of the solution vector returned in ANSWER.
       REAL*4.

 NUMTEN is the number of iterations required to reduce the search
        range by a factor of 10.
        INTEGER*4.
        
 ANSWER is the initial guess position. On return answer holds the
        solution which provided the smallest cost.
        Answer is a vector of length NARG.
        REAL*4.

 LIMITS is the maximum number of successful iterations permitted.
       Should be at least several hundred. If LIMITS/NUMTEN=3 for
       example the solution precision will be about RANGE/10**3.
       INTEGER*4.

 NORM is the number of iterations which pass between recomputation
      of the Boltzmann normalization constant. A normalization
      is always performed before the first iteration.
      Certain types of minimization problems require one to renormalize
      at regular intervals.
      INTEGER*4.

 NPTS is the number of points entered in ARRAY. INTEGER*4.

 PRNT prints convergence status each PRNT iterations. A zero
      means no printing at all. INTEGER*4.

 IND  returned indicator. 0 = normal, 1 = abnormal return.
      INTEGER*4.


  REQUIRED SUBROUTINE COST (provided by the user):

    Fortran Version
      subroutine cost(x,array,array2,n,error,ind)
    C Version
      zcost(x,array,array2,n,error,ind)
        zcost is an arbritary name for the "C" routine, but the
        name must be passed as a routine name to the bridge
        zmetropolis. 


 Returns the cost function resulting from guessing x (answer).
 This could be any function or logical operation.
 The user supplies this subroutine. The purpose of COST is
 to return a penalty measure which METROPOLIS uses as it tries
 to feel it's way to the solution by guessing at values for
 the solution vector X. METROPOLIS will select that solution vector
 which results in the smallest cost.

  ARGUMENTS OF COST:

 X     is the solution vector. REAL (length known by COST).
 ARRAY is an array of N data points (if needed). REAL
 ARRAY2 is another array of N elements if needed. REAL
 N      is the number of elements/data points in ARRAY & ARRAY2.
 ERROR is the returned cost. REAL scalar.
 IND   is 0 for normal return, 1 for abnormal return.
        If METROPOLIS senses an indicator of 1 it will generate another
        guess somewhere else & try again.


     
2  HISTORY

 The technique was conceived of by Edward Teller and Nocholas
 Metropolis in 1953 and in my humble opinion is the most
 powerful numerical minimization method in existence.
 I have taken the liberty to modify it as i saw fit by incorporating
 ideas from several authors including a few of my own.

     METROPOLIS
     Original Programmer: 		J. J. Lorre
     Current Cognizant Programmer:	J. J. Lorre
     Documentation Author: 		J. J. Lorre
     Reference:     Szu H. H., SPIE 698 Real Time Signal Processing
                    #9 (1986) 59 "Non-Convex Optimization"
     Revisions:
     5-94   RNR(CRI)  MSTP S/W Conversion (VICAR Porting) - Provided a
                      "C" interface and test routines.  May be invoked
                      from "C" or Fortran, however, the "COST" 
                      subroutine must be in the same language as the 
                      invoking program.
     3-95   CRI     Removed LIB_LOCAL as per FR85757
     3-95   CRI     Fixed xvmessage lines for alphas as per FR85782
     7-95   CRI     Removed hard coded seed line as per FR87256

2 Examples

     FORTRAN  EXAMPLE
      purpose to test the Metropolis algorithm in one dimension.
      The example below is to find the LOWEST minimum of the
      polynomial expressed by the COST function found at the
      end of this example.
     
      real*4 range(1),answer(1),array(1),array2(1)
      external e2cost
      range(1)=2.0
      numten=500
      answer(1)=3.
      limit=1500
      iprint=200
      narg=1
      norm=200
      npts=0
      call metropolis(e2cost,narg,array,array2,range,numten,answer,
     +                 limit,norm,npts,iprint,ind)
      return
      end


      subroutine e2cost(x,array,array2,n,error,ind)
      real*4 array(1),array2(1),x(1)
      ind=0
      error=x(1)**4-16*x(1)**2+5*x(1)
      return
      end


     C  EXAMPLE
      void zcost2 (x, array, array2, n, error, ind) /* cost function*/

      float *x;         /* The solution vector */
      float *array;     /* An array of n data points, if needed */
      float *array2;    /* Another array of n data points, if needed */

      int n;              /* The number of data points in array and array2 */  
      float *error;       /* The returned cost */
      int *ind;           /* Status indicator */
      {
        float *xp,xvp;
        double err,dxvp,quad;
        dxvp = *(x);
        *ind=0;
        err=pow(dxvp,4.) - 16. * pow(dxvp,2.) + 5. * (dxvp);
        xvp = err;
        *error = xvp;
      }


      void main() /* main Routine*/
      {

        void (*costp)();     /* The cost function to be called */
        int narg;           /* The number of variables */  
        float array[1];     /* An array of data */
        float array2[1];    /* Another array of data */
        float range[1];     /* Bounds of solution vector elements */
        int numten;         /* The itrations to reduce the error by 10 */
        float answer[1];    /* The solution vector */
        int limit;          /* The total number of iterations permitted */
        int norm;           /* The iterations between normalizations */
        int npts;           /* The number of points in array and array2 */
        int iprint;	    /* The number of iterations between printouts */
        int ind;            /* Status indicator */
        int *indp;
        void *ap;

        numten=500;
        limit=1500;
        iprint=200;
        narg=1;
        norm=200;
        npts=0;
        answer[0] = 3;
        range[0] = 2;
        indp = &ind;
        costp = *zcost2;
        zmetropolis(costp,narg,array,array2,range,numten,answer,
                       limit,norm,npts,iprint,indp);

        }
$ Return
$!#############################################################################

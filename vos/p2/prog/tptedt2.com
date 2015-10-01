$!****************************************************************************
$!
$! Build proc for MIPL module tptedt2
$! VPACK Version 1.8, Monday, February 19, 1996, 10:26:05
$!
$! Execute by entering:		$ @tptedt2
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
$ write sys$output "*** module tptedt2 ***"
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
$ write sys$output "Invalid argument given to tptedt2.com file -- ", primary
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
$   if F$SEARCH("tptedt2.imake") .nes. ""
$   then
$      vimake tptedt2
$      purge tptedt2.bld
$   else
$      if F$SEARCH("tptedt2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tptedt2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tptedt2.bld "STD"
$   else
$      @tptedt2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tptedt2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tptedt2.com -
	-s tptedt2.f -
	-i tptedt2.imake -
	-p tptedt2.pdf -
	-t tsttptedt2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tptedt2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c program tptedt2
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (ntable=100000)
c ntable = max # tiepoints
      integer*4 status,count,def,located(20),point,pass
      real*4 left_line(ntable),left_samp(ntable)
      real*4 right_line(ntable),right_samp(ntable)
      real*4 buf(4,32)
      real*8 coefx(4),coefy(4)
      real*8 a(4,4),aa(4,4)
      real*8 c(20,4),cl(20),weights(20)
      byte flag(ntable)
      character*80 msg
      logical xvptst,exceed_length,exceed_angle,both

      radeg=180./3.141592653

      call xvparm('NPTS',nptsfit,count,def,1)
      call xvparm('DISTANCE',distance,count,def,1)
      call xvparm('LENGTH',size,count,def,1)
      size=size*size
      call xvparm('ANGLE',angle_limit,count,def,1)
      call xvparm('RANGE',range_limit,count,def,1)
      call xvparm('BIAS',bias,count,def,1)
      both=xvptst('BOTH')

c load the tiepoint table from the mark file.
      call xvunit(inmark,'INP',1,status,' ')
      call xvopen(inmark,status,'IO_ACT','AS','U_FORMAT','REAL'
     +            ,'OPEN_ACT','AS',' ')
      call xvget(inmark,status,'NL',nl,'NS',ns,' ')
      npts=0
      nptsgood=0
      nptsbad=0
      do i=1,nl
         call xvread(inmark,buf,status,'LINE',i,' ')
         do j=1,32
            npts=npts+1
            flag(npts)=1
            if(buf(1,j).eq.0.0) flag(npts)=0
            if(buf(2,j).eq.0.0) flag(npts)=0
            if(buf(3,j).eq.0.0) flag(npts)=0
            if(buf(4,j).eq.0.0) flag(npts)=0
            if(flag(npts).eq.1)then
               nptsgood=nptsgood+1
            else
               nptsbad=nptsbad+1
            endif
            if(npts.gt.ntable)then
               write(msg,5) ntable
5              format(' Too many input points, limit= ',i6)
               call xvmessage(msg,' ')
               call abend
            endif
            left_line(npts)=buf(1,j)            
            left_samp(npts)=buf(2,j)            
            right_line(npts)=buf(3,j)            
            right_samp(npts)=buf(4,j)            

c           Delete vectors that are too long
            if(((right_line(npts)-left_line(npts))**2+
     +         (right_samp(npts)-left_samp(npts))**2).gt.size)
     +         flag(npts)=0

         enddo
      enddo

      call xvmessage('Input tiepoints file contains:',' ')
      write(msg,4) nptsgood,nptsbad
4     format(1x,i6,' Good points and ',i6,' empty spaces')
      call xvmessage(msg,' ')
      call xvclose(inmark,status,'CLOS_ACT','FREE',' ')

      pass=1
      number_deleted=0               ! true number for any reason
1000  number_deleted_range=0
      number_deleted_angle=0

c Loop on tiepoints
      do 100 point=1,npts
            if(flag(point).eq.0) goto 100
            gridl=left_line(point)
            grids=left_samp(point)

c           determine the 4 nearest points, one in each quadrant.
            d_topleft=1.0e+10
            d_topright=1.0e+10
            d_botleft=1.0e+10
            d_botright=1.0e+10
            i_topleft=0
            i_topright=0
            i_botleft=0
            i_botright=0
            do 101 i=1,npts
               if(flag(i).eq.0) goto 101
               if(i.eq.point)   goto 101
               if(left_line(i).lt.gridl)then
                  if(left_samp(i).lt.grids)then  ! upper left
                     d=gridl-left_line(i) + grids-left_samp(i)
                     if(d.lt.d_topleft)then
                        d_topleft=d
                        i_topleft=i
                     endif
                  else                            ! upper right
                     d=gridl-left_line(i) + left_samp(i)-grids
                     if(d.lt.d_topright)then
                        d_topright=d
                        i_topright=i
                     endif
                  endif
               else                               
                  if(left_samp(i).lt.grids)then  ! lower left
                     d=left_line(i)-gridl + grids-left_samp(i)
                     if(d.lt.d_botleft)then
                        d_botleft=d
                        i_botleft=i
                     endif
                  else                            ! lower right
                     d=left_line(i)-gridl + left_samp(i)-grids
                     if(d.lt.d_botright)then
                        d_botright=d
                        i_botright=i
                     endif
                  endif
               endif                     
101         continue

c           PERFORM FITTING STEP. 

            if(nptsfit.gt.4) goto 200

c           perform exact fit using 4 points only, no weighting.

c           skip analysis if all 4 quadrants dont have a point
            if(i_topleft.eq.0.or.i_topright.eq.0.or.
     +         i_botleft.eq.0.or.i_botright.eq.0)then
               goto 100
            endif

            a(1,1)=dble(left_line(i_topleft))*
     +             dble(left_samp(i_topleft))
            a(1,2)=dble(left_line(i_topleft))
            a(1,3)=dble(left_samp(i_topleft))
            a(1,4)=1.d0
            a(2,1)=dble(left_line(i_topright))*
     +             dble(left_samp(i_topright))
            a(2,2)=dble(left_line(i_topright))
            a(2,3)=dble(left_samp(i_topright))
            a(2,4)=1.d0
            a(3,1)=dble(left_line(i_botleft))*
     +             dble(left_samp(i_botleft))
            a(3,2)=dble(left_line(i_botleft))
            a(3,3)=dble(left_samp(i_botleft))
            a(3,4)=1.d0
            a(4,1)=dble(left_line(i_botright))*
     +             dble(left_samp(i_botright))
            a(4,2)=dble(left_line(i_botright))
            a(4,3)=dble(left_samp(i_botright))
            a(4,4)=1.d0
            call mve(8,16,a,aa,1,1) ! save a

c           to right image line
            coefy(1)=right_line(i_topleft)
            coefy(2)=right_line(i_topright)
            coefy(3)=right_line(i_botleft)
            coefy(4)=right_line(i_botright)
            call dsimq(a,coefy,4,kstat)
            if(kstat.ne.0)then
               call xvmessage('DSIMQ singular solution',' ')
               goto 100
            endif

c           to right image sample
            call mve(8,16,aa,a,1,1) ! restore a
            coefx(1)=right_samp(i_topleft)
            coefx(2)=right_samp(i_topright)
            coefx(3)=right_samp(i_botleft)
            coefx(4)=right_samp(i_botright)
            call dsimq(a,coefx,4,kstat)
            if(kstat.ne.0)then
               call xvmessage('DSIMQ singular solution',' ')
               goto 100
            endif
            goto 300            

c           Least squares fit using from 5 to 20 nearest points.
200         continue

c           move 4 quadrant points into located() buffer
            n=0
            if(i_topleft.gt.0)then
              n=n+1
              located(n)=i_topleft
            endif
            if(i_topright.gt.0)then
              n=n+1
              located(n)=i_topright
            endif
            if(i_botleft.gt.0)then
              n=n+1
              located(n)=i_botleft
            endif
            if(i_botright.gt.0)then
              n=n+1
              located(n)=i_botright
            endif

c           locate additional points as the nearest ones remaining
            do i=n+1,nptsfit
              d=1.0e+20
              jj=0
              do 102 j=1,npts
                if(flag(j).eq.0) goto 102
                if(j.eq.point)   goto 102
                do k=1,i-1   ! avoid points already located
                  if(located(k).eq.j) goto 210
                enddo
                dist=(gridl-left_line(j))**2+(grids-left_samp(j))**2
                if(dist.lt.d) then
                   d=dist
                   jj=j
                endif
210             continue
102           continue
              if(jj.eq.0)then
                 call xvmessage(' Insufficient points remain',' ')
                 call abend
              endif
              located(i)=jj
            enddo

c           compute weights for points inversely with distance
            do i=1,nptsfit
              dist=sqrt((gridl-left_line(located(i)))**2 +
     +                  (grids-left_samp(located(i)))**2 )
              weights(i)=distance/(dist+1.0)
            enddo

c           fit for right image line direction.
            do i=1,nptsfit
              c(i,1)=dble(left_line(located(i))) *
     +               dble(left_samp(located(i)))
              c(i,2)=dble(left_line(located(i)))
              c(i,3)=dble(left_samp(located(i)))
              c(i,4)=1.d0
              cl(i)=right_line(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,coefy)
            if(kstat.ne.0)then
               call xvmessage('LSQP singular solution',' ')
               goto 100
            endif

c           fit for right image sample direction.
            do i=1,nptsfit
              cl(i)=right_samp(located(i))
            enddo
            call lsqp(kstat,nptsfit,4,c,cl,weights,coefx)
            if(kstat.ne.0)then
               call xvmessage('LSQP singular solution',' ')
               goto 100
            endif

c END OF FITTING

300         continue
c           compute the x,y position in the right image.
            y=coefy(1)*dble(gridl)*grids+coefy(2)*gridl+
     +                coefy(3)*grids+coefy(4)
            x=coefx(1)*dble(gridl)*grids+coefx(2)*gridl+
     +                coefx(3)*grids+coefx(4)

c           Compute range constraint
            exceed_length=.false.
            p_range=sqrt((y-gridl)**2+(x-grids)**2)  ! predicted range
            t_range=sqrt((right_line(point)-gridl)**2+
     +                   (right_samp(point)-grids)**2) ! actual range
            if(abs(t_range-p_range)/(t_range+p_range+bias).gt.
     +         range_limit)then
               number_deleted_range=number_deleted_range+1
               exceed_length=.true.
            endif

c           Compute angle constraint
            exceed_angle=.false.
            p_angle=radeg*atan2(y-gridl,x-grids)    ! predicted angle
            t_angle=radeg*atan2(right_line(point)-gridl,
     +                    right_samp(point)-grids)  ! actual angle
            if(p_angle.lt.0.) p_angle=p_angle+360.  ! assure 0 to 360
            if(t_angle.lt.0.) t_angle=t_angle+360.
            dangle=abs(t_angle - p_angle)
            if(dangle.gt.180.) dangle=360.-dangle ! on both sides of 0
            if(dangle*t_range/(t_range+bias).gt.angle_limit)then
               number_deleted_angle=number_deleted_angle+1
               exceed_angle=.true.
            endif

c           Decide on point
            if(both)then
               if(exceed_length.and.exceed_angle)then
                  flag(point)=0
                  number_deleted=number_deleted+1
               endif
            else
               if(exceed_length.or.exceed_angle)then
                  flag(point)=0
                  number_deleted=number_deleted+1
               endif
            endif
            
100   continue              ! end point loop

      write(msg,9) pass
9     format(' Pass number ',i4)
      call xvmessage(msg,' ')
      write(msg,7) number_deleted_range
7     format(' Number of points failing range check =',i6)
      call xvmessage(msg,' ')
      write(msg,8) number_deleted_angle
8     format(' Number of points failing angle check =',i6)
      call xvmessage(msg,' ')

c return for a second pass
      pass=pass+1
      if(pass.lt.3) goto 1000

      write(msg,6) number_deleted
6     format(' Actual number of points deleted =',i6)
      call xvmessage(msg,' ')
      call xvmessage('   ',' ')

c re-load the mark file.
      call xvunit(outmark,'OUT',1,status,' ')
      call xvopen(outmark,status,'IO_ACT','AS','U_FORMAT','REAL',
     +            'OP','WRITE','O_FORMAT','REAL','OPEN_ACT','AS',' ')
      n=0
      do i=1,nl
         do j=1,32
            n=n+1
            if(flag(n).eq.0)then
               buf(1,j)=0.  ! deleted tiepoint
               buf(2,j)=0.
               buf(3,j)=0.
               buf(4,j)=0.
            else
               buf(1,j)=left_line(n)
               buf(2,j)=left_samp(n)
               buf(3,j)=right_line(n)
               buf(4,j)=right_samp(n)
            endif
         enddo
         call xvwrit(outmark,buf,status,'LINE',i,' ')
      enddo
      call xvclose(outmark,status,'CLOS_ACT','FREE',' ')

      return
      end

c**************************************************************
      SUBROUTINE LSQP(ind,NE,NU,C,CL,wts,X1)
C
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C
C2    THE INFORMATION FROM THE MAIN PROGRAM IS:
C          C(I,J) = COEFFICIENT MATRIX
C          CL(I) = ARRAY OF FREE TERMS
C          NE = NUMBER OF EQUATIONS
C          NU=NUMBER OF UNKNOWNS
c          wts=weight for each input point
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      REAL*8  A(4,4),AL(4),R(4,4),RL(4),Q(4,4),X(4),SUM
      REAL*8 C(20,4),CL(20),X1(4),wts(20)
      
      ind=0
      DO 57 J = 1,NU
      DO 57 I=1,NU
      A(I,J)=0.
      R(I,J)=0.
57    Q(I,J)=0.
      DO 100 I=1,NU
      DO 100 J=1,NU
      DO 100 K=1,NE
100   A(I,J)=A(I,J)+C(K,I)*C(K,J)*wts(k)
      DO 102 I=1,NU
      AL(I)=0.
      DO 102 K=1,NE
102   AL(I)=AL(I)+C(K,I)*CL(K)*wts(k)
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      if(a(i,i).eq.0.d0)goto 999
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      if(a(1,1).eq.0.d0)goto 999
      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
      if(a(i,i).eq.0.d0)goto 999
125   RL(I)=AL(I)/A(I,I)
       X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      RETURN
999   ind=1
      return
      END


C*********************************************************************
      SUBROUTINE DSIMQ(A,B,N,KS)
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
      real*8 A(1),B(1),biga,save,tol
C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
C
      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
      if(biga.eq.0.d0)then
        ks=1
        return
      endif
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tptedt2.imake
#define  PROGRAM   tptedt2

#define MODULE_LIST tptedt2.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create tptedt2.pdf
process help=*
PARM INP	TYPE = STRING   COUNT=1
PARM OUT	TYPE = STRING   COUNT=1
PARM NPTS       TYPE = INTEGER  COUNT=1	  DEFAULT = 6  VALID=(4:20)
PARM DISTANCE   TYPE = REAL     COUNT=1	  DEFAULT = 20. VALID=(1.:1000.)
PARM LENGTH     TYPE = REAL     COUNT=1	  DEFAULT = 1000. VALID=(.001:1001.)
PARM ANGLE      TYPE = REAL     COUNT=1	  DEFAULT = 30. VALID=(0.:180.)
PARM RANGE      TYPE = REAL     COUNT=1	  DEFAULT = 0.3 VALID=(0.:1.)
PARM BIAS       TYPE = REAL     COUNT=1	  DEFAULT = 3.0 VALID=(.001:1000.)
PARM BOTH       TYPE = KEYWORD  	  DEFAULT = EITHER +
               VALID=(BOTH,EITHER)
END-PROC
.TITLE
VICAR program TPTEDT2.

.HELP
PURPOSE:
Batch editor for automated removal of erroneous vectors (tiepoints)
from a mark file produced by TRACKER3.
The equivalent interactive program is TPTEDT.

.PAGE
EXECUTION:

TPTEDT2 may be executed in the following manner:

                TRACKER3 INP=(LEFT,RIGHT) OUT=A
		TPTEDT2 INP=A OUT=B PARAMS

where INP, OUT, SIZE, AND PARAMS are parameters and are explained in their
respective parameter section.

PROPER USAGE:

This algorithm uses neighborhood vectors to predict behavior at a
point. For this to make any sense the vector spacing in TRACKER
must be LESS than the correlation size or less than the eddy size
in the medium being analyzed. Too coarse a spacing will just 
produce junk. Good results require hundreds of vectors.

The keyword BIAS is important. Make certain you understand it.

.PAGE
OPERATION:

Tptedt2 edits mark file tiepoints.  The input tiepoints are stored in
memory. Each tiepoint (reference) is analyzed in the following manner:

1. 4 points are selected. These are the nearest points in each quadrant
   surrounding the reference tiepoint (in the left image).

2. Optionally additional points are selected from the closest points
   in any direction until the total neighborhood points is NPTS.

3. A least squares fit is made relating the left and right images
   using the NPTS points (exluding the reference tiepoint).

4. The right position of the reference tiepoint is computed using the
   left base position and the polynomial coefficients.

5. The vector length is computed for the reference and predicted
   reference tiepoint.

6. The vector angle is computed for the reference and predicted
   reference tiepoint.

7. The reference tiepoint is deleted if:
   A)  It's length departs excessively from the predicted value.
       If Lr is the  vector length and Lp is the predicted
       length then the tiepoint is deleted if:
       abs(Lr-Lp)/(Lr+Lp+BIAS) > RANGE
   B)  It's angle deviates excessively from the predicted value.
       If Ar is the vector angle and Ap is the predicted
       angle then the tiepoint is deleted if:
       abs(Ar-Ap)*Lr/(Lr+BIAS) > ANGLE

   NOTE: The default case deletes a tiepoint if either of the above
         criteria are violated. The BOTH keyword requires both criteria
         to be violated for the point to be deleted.


Notes: If NPTS=4 then only the quadrant points are used. Border points
       may not have 4 quadrant points and will never be deleted.

       If NPTS=4 equal weight is given to each point in the polynomial
       fit.

       If NPTS > 4 the fit will be least squares using weighting. The
       weight = DISTANCE/(d+1)   where d=the distance of each point
       from the reference point in pixels.

       The polynomial is of the form:
       Y=Ax+By+Cxy+D
       X=Ex+Fy+Gxy+H

EXAMPLE:

	 TPTEDT2 INP=A OUT=B NPTS=7 DISTANCE=10. ANGLE=15. RANGE=.5

.PAGE
HISTORY:

ORIGINALLY WRITTEN BY: J Lorre 3/30/93
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
STRING-input dataset.
.VARI OUT
STRING-output dataset.
.VARI NPTS
Number of fitted
tiepoints.
.VARI DISTANCE
Weighting distance
.VARI LENGTH
Max vector length
.VARI ANGLE
Permitted angle
deviation
.VARI RANGE
Permitted length
deviation.
.VARI BIAS
Permitted length 
deviation bias.
.VARI BOTH
Both range and
angle must be
violated.

.LEVEL2

.VARI INP
Input mark format tiepoints file created by program TRACKER.
Contains 4 word groups leftline,leftsample,rightline,rightsample.
real format 512 byte records. Bad points are zeroed out.

.VARI OUT
Identical to INP except deleted tiepoints are zeroed out.

.VARI NPTS
Number of tiepoints in the near vicinity of each tiepoint to use in
a least squares fit to predict that tiepoint from it's neighbors.
Must be between 4 and 20.

.VARI DISTANCE
Weighting distance used in weighting points according to their
distance.  Weight=DISTANCE/(d+1)  where d is the distance to each 
tiepoint in pixels.

.VARI LENGTH
The maximum vector length permitted. Any vector longer than this is rejected.

.VARI ANGLE
Maximum permitted angle deviation between the tiepoint vector and
the vector predicted from the neighboring tiepoint vectors.
In degrees.
       If Ar is the vector angle and Ap is the predicted
       angle then the tiepoint is deleted if:
       abs(Ar-Ap)*Lr/(Lr+BIAS) > ANGLE
See the BIAS keyword.

.VARI RANGE
Maximum permitted length deviation between the tiepoint vector and
the vector predicted from the neighboring tiepoint vectors.
This is a ratio between 0 and 1.
       If Lr is the  vector length and Lp is the predicted
       length then the tiepoint is deleted if:
       abs(Lr-Lp)/(Lr+Lp+BIAS) > RANGE
See the BIAS keyword.

.VARI BIAS
BIAS protects short vectors from being deleted. It should be set to
about the length of the shortest vectors which one considers to begin
to be immune from testing. 
For purposes of geoms or morphing a vector of nearly zero length does no harm.
Such a short vector really points nowhere and it's angle is meaningless. You
generally want to keep it.

.VARI BOTH
Both range and angle checks must be violated for a point to be deleted.
The default is to delete the tiepoint if either check fails.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttptedt2.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
gausnois out=a.img nl=220 ns=220
copy inp=a.img out=b.img nl=200 ns=200
copy inp=a.img out=c.img sl=5 ss=5 nl=200 ns=200
tracker3 inp=(b.img,c.img) +
 out=d.img grid=25 nlw=21 nsw=21
tptedt2 inp=d.img out=d2.img npts=5 distance=25. angle=50. +
   range=0.7 bias=5.
end-proc
$ Return
$!#############################################################################

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

	External GSFIT
        Parameter (NV=4,NP=3)
	Dimension P(NV,NP),X(NP),Y(NV),RMIN(NP),RMAX(NP)
	Dimension OBS(6),WVCENT(6),OUTBUF(10000,NP)
	Dimension FILTER(158,6),WVLN(158)
	Integer*2 INBUF(10000,6)
	Logical IPROG,XVPTST
	Common/BOX1/RMIN,RMAX
	Common/RAWFIL/FILTER,WVLN,ICALDATE
	Common/TIMS/WVCENT
	Common/DATA1/OBS
	Data RMIN/0.0,0.0,0.0/
	Data RMAX/1.0,0.0,4.0/

C------------------------------------------------------------------------
C                           Format Block
C------------------------------------------------------------------------
100	Format(/,' Progress Report for MINMAP ',/)
101	Format('+ Current Output Line: ',I10,
     +         '   Elapsed Time: ',F9.2,'   seconds')
102	Format(I2,1X,I2,1X,I2)

C--------------------------------------------------------------------------
C			Open datasets
C--------------------------------------------------------------------------
	Call XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	Call XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','HALF',' ')
	Call XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	Call XVUNIT(IOUTUNIT1,'OUT',1,ISTAT,' ')
	Call XVOPEN(IOUTUNIT1,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL','O_FORMAT','REAL','OP','WRITE',
     +		    'U_NL',NL,'U_NS',NS,'U_NB',3,'U_ORG','BIL',' ')

C--------------------------------------------------------------------------
C	            Get the User's Parameters
C--------------------------------------------------------------------------
	Call XVPARM('SCALE',USCAL,ICNT,IDEF,0)
	Call XVPARM('RND_SEED',ISEED,ICNT,IDEF,0)
	ISEED=-ISEED
	IPROG=XVPTST('PROGRESS')

C---------------------------------------------------------------------------
C                  Calculate the TIMS band centers
C---------------------------------------------------------------------------
	Call BDCENT(IUNIT1,WVCENT)

C---------------------------------------------------------------------------
C 			Start Line Loop
C---------------------------------------------------------------------------
	If(IPROG) Write(6,100)
	ICOUNT=1
	IEL = ISL+NL-1
	Do ILIN=ISL,IEL	
				
C---------------------------------------------------------------------------
C            Read in a line of imagery
C---------------------------------------------------------------------------
	    Do IBND=1,6
		Call XVREAD(INUNIT,INBUF(1,IBND),ISTAT,'LINE',ILIN,'BAND',
     +			    IBND,'SAMP',ISS,'NSAMPS',NS,' ')
	    EndDo

C----------------------------------------------------------------------------
C                        Start Sample Loop
C
C	Read in emissivities, convert to "reflectance"
C----------------------------------------------------------------------------
	    JCOUNT=1
	    Do IPIX=1,NS
	       Do IBND=1,6
	          OBS(IBND) = 0.0
		  OBS(IBND) = FLOAT(INBUF(IPIX,IBND))/USCAL
	          OBS(IBND) = 1.0 - OBS(IBND)
	       EndDo

C------------------------------------------------------------------------------
C                      Bracket the position of the maximum
C------------------------------------------------------------------------------
            INDXHI=0
            RTEST=-1e6
            Do IBND=1,6
	       If(OBS(IBND).GT.RTEST) Then
                  RTEST=OBS(IBND)
                  INDXHI=IBND
               EndIf
            EndDo
            If(INDXHI.LE.3) Then	
	       RMIN(2)=8.0
	       RMAX(2)=10.0
            EndIf
	    If(INDXHI.GE.4) Then
	       RMIN(2)=9.0
	       RMAX(2)=12.0
	    EndIf

C-------------------------------------------------------------------------------
C	         Solve for the three model parameters
C 
C                Initialize the vertex matrix
C-------------------------------------------------------------------------------
	     Do IVRT=1,NV		 
	        Do IPRM=1,NP		 
                   P(IVRT,IPRM)=0.0 		
                   P(IVRT,IPRM)=RAN1(ISEED)		
                EndDo
             EndDo

C-------------------------------------------------------------------------------
C            Fill the model vector
C------------------------------------------------------------------------------
	     Do IVRT=1,NV
	        Do IPRM=1,NP
		   X(IPRM)=0.0
	           X(IPRM)=P(IVRT,IPRM)
                EndDo
		Y(IVRT)=0.0
                Y(IVRT)=GSFIT(X)
	     EndDo

C-------------------------------------------------------------------------------
C            Perform the inversion
C-------------------------------------------------------------------------------
	     NDIM=NP
	     Call AMOEBA(P,Y,NV,NP,NDIM,GSFIT) 

C------------------------------------------------------------------------
C	Decode the model parameter estimates returned from AMOEBA
C-----------------------------------------------------------------------
             Do IPRM=1,NP
	        X(IPRM)=0.0					
	        X(IPRM)=P(1,IPRM)
                X(IPRM)=X(IPRM)*(RMAX(IPRM)-RMIN(IPRM))+RMIN(IPRM)
                IF(IPRM.EQ.1) X(IPRM)=1.0-X(IPRM)
                OUTBUF(IPIX,IPRM)=ABS(X(IPRM))
              EndDo  
            EndDo


C------------------------------------------------------------------------------
C                    Write to the Output File
C-------------------------------------------------------------------------------
	     Do IPRM=1,NP
	     Call XVWRIT(IOUTUNIT1,OUTBUF(1,IPRM),ISTAT,' ')
	     EndDo
	        If(IPROG) Then
	           Write(6,101) ICOUNT
	           JCOUNT=JCOUNT+1
	        EndIf
	     If(IPROG) ICOUNT=ICOUNT+1
	EndDo

C-------------------------------------------------------------------------------
C                           Closing Business
C-------------------------------------------------------------------------------
	Call XLADD(IUNIT2,'HISTORY','LBL1','MINIMUM FEATURE MAP',
     +             ISTAT,'FORMAT','STRING','ULEN',19,' ')
	Call XVCLOSE(IUNIT1,ISTAT,' ')
	Call XVCLOSE(IUNIT2,ISTAT,' ')
	Return
	End




C------------------------------------------------------------------------------
C		           ----- TOOLKIT -----
C------------------------------------------------------------------------------
      SUBROUTINE AMOEBA(P,Y,MP,NP,NDIM,FUNK)
      PARAMETER (NMAX=20,ALPHA=1.0,BETA=0.5,GAMMA=2.0)
      PARAMETER (ITMAX=1000,FTOL=1.0E-6)
      DIMENSION P(MP,NP),Y(MP),PR(NMAX),PRR(NMAX),PBAR(NMAX)

      MPTS=NDIM+1
      ITER=0
1     ILO=1


      IF(Y(1).GT.Y(2))THEN
        IHI=1
        INHI=2
      ELSE
        IHI=2
        INHI=1
      ENDIF
      DO 11 I=1,MPTS
        IF(Y(I).LT.Y(ILO)) ILO=I
        IF(Y(I).GT.Y(IHI))THEN
          INHI=IHI
          IHI=I
        ELSE IF(Y(I).GT.Y(INHI))THEN
          IF(I.NE.IHI) INHI=I
        ENDIF
11    CONTINUE

C------------------------------------------------------------
C	Test the range between low and high vertices
C------------------------------------------------------------
	RTOL=2.*ABS(Y(IHI)-Y(ILO))/(ABS(Y(IHI))+ABS(Y(ILO)))
	If(RTOL.LT.FTOL.OR.ITER.EQ.ITMAX) Then
            Return
      	ENDIF

      ITER=ITER+1
      DO 12 J=1,NDIM
        PBAR(J)=0.
12    CONTINUE
      DO 14 I=1,MPTS
        IF(I.NE.IHI)THEN
          DO 13 J=1,NDIM
            PBAR(J)=PBAR(J)+P(I,J)
13        CONTINUE
        ENDIF
14    CONTINUE
      DO 15 J=1,NDIM
        PBAR(J)=PBAR(J)/NDIM
        PR(J)=(1.+ALPHA)*PBAR(J)-ALPHA*P(IHI,J)
15    CONTINUE
      YPR=FUNK(PR)
      IF(YPR.LE.Y(ILO))THEN
        DO 16 J=1,NDIM
          PRR(J)=GAMMA*PR(J)+(1.-GAMMA)*PBAR(J)
16      CONTINUE
        YPRR=FUNK(PRR) 
        IF(YPRR.LT.Y(ILO))THEN
          DO 17 J=1,NDIM
            P(IHI,J)=PRR(J)
17        CONTINUE
          Y(IHI)=YPRR
        ELSE 
          DO 18 J=1,NDIM
            P(IHI,J)=PR(J)
18        CONTINUE
          Y(IHI)=YPR
        ENDIF
      ELSE IF(YPR.GE.Y(INHI))THEN
        IF(YPR.LT.Y(IHI))THEN
          DO 19 J=1,NDIM
            P(IHI,J)=PR(J)
19        CONTINUE
          Y(IHI)=YPR
        ENDIF
        DO 21 J=1,NDIM
          PRR(J)=BETA*P(IHI,J)+(1.-BETA)*PBAR(J)
21      CONTINUE
        YPRR=FUNK(PRR)
        IF(YPRR.LT.Y(IHI))THEN
          DO 22 J=1,NDIM
            P(IHI,J)=PRR(J)
22        CONTINUE
          Y(IHI)=YPRR
        ELSE
          DO 24 I=1,MPTS
            IF(I.NE.ILO)THEN
              DO 23 J=1,NDIM
                PR(J)=0.5*(P(I,J)+P(ILO,J))
                P(I,J)=PR(J)
23            CONTINUE
              Y(I)=FUNK(PR)
            ENDIF
24        CONTINUE
        ENDIF
      ELSE
        DO 25 J=1,NDIM
          P(IHI,J)=PR(J)
25      CONTINUE
        Y(IHI)=YPR
      ENDIF
      GO TO 1
      END




        FUNCTION GSFIT(X)
	Parameter (PI=3.141592654,NP=3,NV=6)
	Dimension X(NP),Y(NP),RMIN(NP),RMAX(NP),OBS(NV),WAVE(NV)
	Common/DATA1/OBS
	Common/BOX1/RMIN,RMAX
	Common/TIMS/WAVE

C----------------------------------------------------------------------
C	Decode the normalized model parameters
C----------------------------------------------------------------------
	Do i=1,NP
	   Y(i)=X(i)*(RMAX(i)-RMIN(i))+RMIN(i)
        EndDo
C-----------------------------------------------------------------------
C	Calculate the sum of the squares of the residuals
C-----------------------------------------------------------------------
        SUMSQ=0.
	Do i=1,NV
           REFL=GAUSS3(Y,WAVE(i))
	   RSD=OBS(i)-REFL
	   SUMSQ=SUMSQ+(RSD*RSD)
        EndDo
       
C-----------------------------------------------------------------------
C	Calculate the fit parameter
C----------------------------------------------------------------------- 
	GSFIT=SQRT(SUMSQ)
	Return
	End	
      


	FUNCTION GAUSS3(RMOD,WV)
C-------------------------------------------------------------------
C	Calculate the Gaussian Emittance, given the 
C	three model parameters and a wavelength
C--------------------------------------------------------------------
	Parameter(PI=3.14159265,N=3,FACT=1.0E-6)
	Dimension RMOD(N)
	A1=(WV-RMOD(2))/(RMOD(3)+FACT)	
	A2=PI*(A1**2.0)
	GAUSS3=RMOD(1)*EXP(-A2)
	Return
	End	



      FUNCTION RAN1(IDUM)
C-----------------------------------------------------------------
C	Improved random number generator, lifted from
C	Numerical Recipes. Set IDUM to any negative
C	number to initialize the sequence.
C-----------------------------------------------------------------
      DIMENSION R(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      DATA IFF /0/
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
        IFF=1
        IX1=MOD(IC1-IDUM,M1)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX2=MOD(IX1,M2)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX3=MOD(IX1,M3)
        DO 11 J=1,97
          IX1=MOD(IA1*IX1+IC1,M1)
          IX2=MOD(IA2*IX2+IC2,M2)
          R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11      CONTINUE
        IDUM=1
      ENDIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      IF(J.GT.97.OR.J.LT.1)PAUSE
      RAN1=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      RETURN
      END


	Subroutine BDCENT(IPIX,WAVE)
C---------------------------------------------------------------------
C	Calculates the centers of the appropriate TIMS bands
C---------------------------------------------------------------------
	Real WAVE(6),FILTER(158,6),WVLN(158)
	CHARACTER*100 LABEL
	Common/RAWFIL/FILTER,WVLN,ICALDATE

        Call XVPARM('DATE',IDATE,ICNT,IDEF,0)
        If(IDATE.EQ.-1) Then
C
C		Check to see if the date is already in the label
C
	CALL XLGET(IPIX,'HISTORY','INFO1',LABEL,ISTAT,
     &		   'HIST','TIMSLOG','FORMAT','STRING',0)
	IF (ISTAT.LT.0) CALL XLGET(IPIX,'HISTORY','INFO1',LABEL,ISTAT,
     &			'HIST','VTIMSLOG','FORMAT','STRING',0)
	IF (ISTAT.LT.0) CALL XLGET(IPIX,'HISTORY','LAB1',LABEL,ISTAT,
     &			'HIST','VTIMSLOG','FORMAT','STRING',0)
	IF (LABEL(6:6).EQ.'D') THEN
	    READ (LABEL,80,err=95) MONTH,IDAY,IYEAR
   80	    FORMAT (14X,I2,1X,I2,1X,I2)
	ELSE
	    READ (LABEL,90,err=95) MONTH,IDAY,IYEAR
   90	    FORMAT (17X,I2,1X,I2,1X,I2)
	END IF
	IDATE = 10000*IYEAR+100*MONTH+IDAY
   95	CONTINUE
	EndIf
        Call GETFIL(IDATE,0)
	Do IBND=1,6
	   PSUM=0.
	   FSUM=0.
	   WVLEN=7.488
	   Do INDX=1,158
              PSUM=PSUM+(FILTER(INDX,IBND)*WVLEN)
	      FSUM=FSUM+FILTER(INDX,IBND)
	      WVLEN=WVLEN+0.032
	   EndDo
	   WAVE(IBND)=PSUM/FSUM
	EndDo
	Return
	End

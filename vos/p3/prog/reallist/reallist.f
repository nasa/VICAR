	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C     23 SEPT 93   ...REA...    INITIAL RELEASE
C     11 APRIL 02  ...REA...    add SB, NB parameters
C     16 APRIL 02  ...REA...    add DECIMAL parameter
C      7 MAY 03    ...REA...    PRECISE keyword added
C
	REAL*8 BUF(10)
	CHARACTER*140 PR, fstrng
	CHARACTER*3 ORG
	LOGICAL XVPTST
C								open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','DOUB',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVBANDS(ISB,NB,NBIN)
C							set width parameters
	IF (XVPTST('WIDE')) THEN
	    INCS = 10
	ELSE
	    INCS = 6
	END IF
C							set precision
	IF (XVPTST('PRECISE')) THEN
	    IWIDE = 20
	    INCS = 6
	ELSE
	    IWIDE = 11
	END IF
	CALL XVPARM('DECIMAL',IDEC,NUM,IDEF,0)
C						  	set ends of loops
	IEL = ISL+NL-1   
	IES = ISS+NS-1
	IEB = ISB+NB-1
C							get file organization
	CALL XVGET(INUNIT,ISTAT,'ORG',ORG,' ')
	IF (ORG .EQ. 'BIP') THEN
	    CALL XVMESSAGE('REALLIST does not support BIP format',' ')
	    CALL ABEND
	ELSE
	    DO IB=ISB,IEB
		CALL XVMESSAGE(' ',' ')
		IF (NBIN .NE. 1) THEN
		    WRITE (PR,400) IB
  400		    FORMAT('Band',I3)
		    CALL XVMESSAGE(PR,' ')
		END IF
C
		DO I=ISS,IES,INCS
		    IF (I .NE. ISS) CALL XVMESSAGE(' ',' ')
		    NSS = MIN(IES-I+1,INCS)
c		    WRITE (PR,500) (J,J=I,I+NSS-1)
c 500		    FORMAT('  Line',I<IWIDE>,9I<IWIDE+1>)
c  above is not std.Fortran, replace with:
		    fstrng = ' '
		    write(fstrng,500) iwide,iwide+1
500		    format('(''  Line'',I',i2,',9I',i2,')')
		    write(pr,fstrng) (j,j=i,i+nss-1)
		    CALL XVMESSAGE(PR,' ')
C							loop thru all lines	
		    DO J=ISL,IEL
			CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',J,'SAMP',I,
     +				'NSAMPS',NSS,'BAND',IB,' ')
c			WRITE (PR,600) J,(BUF(K),K=1,NSS)
c 600			FORMAT(I6,10(1X,F<IWIDE>.<IDEC>))
		        fstrng = ' '
		        write(fstrng,600) iwide,idec
600		        format('(I6,10(1X,F',i2,'.',i2,'))')
		        write(pr,fstrng) j,(buf(k),k=1,nss)
		 	CALL XVMESSAGE(PR,' ')
		    END DO
		END DO
	   END DO
	END IF
C
	RETURN
	END

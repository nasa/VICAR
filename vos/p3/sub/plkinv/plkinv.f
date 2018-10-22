	FUNCTION PLKINV(WAVE,RAD)
C
C	4/27/99    rea	Update radiation constants.
C
C------------------------------------------------------------
C	Given a wavelength and a radiance, return the
C       temperature by the inversion of Planck's Law
C-------------------------------------------------------------
	Parameter(C1=3.741775E-16,C2=0.01438769,PI=3.14159265,
     +            FACT=1.0E-6)
	IF (RAD.GT.0.0) THEN
	  WV=WAVE*FACT			!Convert um to m
	  RAD2=RAD/FACT
	  A=(C1/(WV**5.))/(PI*RAD2)
 	  PLKINV=C2/(WV*LOG(A+1.0))
	ELSE
	  PLKINV=0.0
	END IF
	Return
	End

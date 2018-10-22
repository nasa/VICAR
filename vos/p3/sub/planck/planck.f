	FUNCTION PLANCK(WAVE,TEMP)
C
C       4/27/99    rea  Update radiation constants.
C
C---------------------------------------------------------
C    Calculate the Spectral Radiance with Planck's Law
C----------------------------------------------------------
	Parameter(C1=3.741775E-16,C2=0.01438769,PI=3.14159265,
     +            FACT=1.0E-6)
C
	IF (TEMP.GT.0.0) THEN
	  WV=WAVE*FACT			!Convert um to m
 	  A=PI*(WV**5.)
	  B=EXP(C2/(WV*TEMP))
	  P=C1/(A*(B-1.0))
	  PLANCK=P*FACT			!Convert 1/m to 1/um
	ELSE
	  PLANCK=0.0
	END IF
	Return
	End

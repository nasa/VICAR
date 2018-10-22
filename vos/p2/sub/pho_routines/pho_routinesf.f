c******************************************************************************
c
c			Mosher Function 
c		(called by the C bridge in pho_routines.c)
c	
c ******************************************************************************
	SUBROUTINE XMOSHER( arg, con, ref)

	INCLUDE 'pho'

	double precision arg(3), con(6), ref

        CI = COS(con(1)*rad_deg)
        CE = COS(con(2)*rad_deg)
	xg = con(3)
        REF = (CON(1)+CON(2)*XG+CON(3)*EXP(-CON(4)*XG)) *
     &   (CI*CE)**(CON(5)+CON(6)*XG)/CE

	return
	end

c ******************************************************************************
  

      INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	DOUBLE PRECISION XC,ZC,TH,TH1,TH2,LAM,F,CAS,PSI,RP,RE
	REAL LINE,SAMP,LAT,LON

        DO 1000 I= 1,15
         IF( I .EQ. 7 .OR. I .EQ. 8) GOTO 1000
        ITYPE =I
C
	M = 2
	LINE = 500.
	SAMP = 500.
	IF(ITYPE .EQ. 1) THEN 
           CALL XVMESSAGE('POLAR ORTH',' ')
	   XC = 400.0000       
      	ZC = 400.0000       ! This swarm of hard constants comes from the log
   	TH = 90.00000       ! of tsttranv before it was ported to UNIX, when
   	TH1 = 0.0000000D+00 ! it called MAP3 and searcv2.
   	TH2 = 0.0000000D+00
   	LAM = 150.0000       
   	F = 7.000000       
   	CAS = 1.000000      
   	PSI = 0.0000000D+00   
   	RP = 1815.300    
   	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 2) THEN 
           CALL XVMESSAGE('OBLIQ ORTH',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000       
	PSI = 15.23100       
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 3) THEN 
           CALL XVMESSAGE('POLAR STER',' ')
	XC = 400.0000       
	ZC = 400.0000       
	TH = 90.00000      
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 4) THEN 
           CALL XVMESSAGE('OBLIQ STER',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000       
	PSI = 15.23100       
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 5) THEN 
           CALL XVMESSAGE('LAMBERT   ',' ')
	XC = 400.0000       
	ZC = 400.0000       
	TH = 90.00000       
	TH1 = 59.17000       
	TH2 = 35.83000    
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 6) THEN 
           CALL XVMESSAGE('MERCATOR  ',' ')
	XC = 1.000000       
	ZC = 1.000000       
	TH = 65.58600      
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 237.7170       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 9) THEN 
           CALL XVMESSAGE('NORMAL CYL',' ')
	XC = 1082.000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 236.9170       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 10) THEN 
           CALL XVMESSAGE('SIMPLE CYL',' ')
	XC = 1.000000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 237.6500       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 11) THEN 
           CALL XVMESSAGE('OBLIQUE SIMPLE CYL',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 12) THEN 
           CALL XVMESSAGE('SINUSOIDAL',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 13) THEN 
           CALL XVMESSAGE('OBLIQUE SINUSOIDAL',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00   
	TH1 = 40.00000      
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 14) THEN 
           CALL XVMESSAGE('MOLLWEIDE',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
	ELSE IF(ITYPE .EQ. 15) THEN 
           CALL XVMESSAGE('TRANS MERC',' ')
	XC = 400.0000       
	ZC = 400.0000      
	TH = 0.0000000D+00  
	TH1 = 0.0000000D+00  
	TH2 = 0.0000000D+00
	LAM = 150.0000       
	F = 7.000000       
	CAS = 1.000000      
	PSI = 0.0000000D+00   
	RP = 1815.300    
	RE = 1830.000    
        END IF
	CALL TRANV(IND,ITYPE,m,XC,ZC,TH,TH1,th2,LAM,F,CAS,LINE,SAMP,
     1             LAT,LON,RP,RE,PSI)
	IF(IND .NE. 0) GO TO 998
	CALL PRNT(7,2,LINE,'FROM L,S.')
	CALL PRNT(7,2,LAT,'TO LT,LN.')
	M = 1
	CALL TRANV(IND,ITYPE,m,XC,ZC,TH,TH1,th2,LAM,F,CAS,LINE,SAMP,
     1             LAT,LON,RP,RE,PSI)
	IF(IND .NE. 0) GO TO 998
	CALL PRNT(7,2,LINE,'AND BACK TO L,S.')

c  special check for LAT=90 in Mercator:
	if (itype.eq.6) then
	  lat = 90.
	  lon = 0.
	  call tranv(ind,itype,m,xc,zc,th,th1,th2,lam,f,cas,line,samp,
     1             lat,lon,rp,re,psi)
	  call prnt(7,2,line,'Mercator (90,0) gives L,S=.')
	endif

1000    CONTINUE

      CALL XVMESSAGE(
     . 'Repeat a test case in C to test C interface: ztranv', ' ')

      call tztranv

	RETURN
998	CALL PRNT(4,1,IND,'TRANV IND = .')
	RETURN
	END

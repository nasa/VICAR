C
C Image test program for the GMASK routines
C Tests Fortran interface.
C
C 	Author :	Justin McNeill
C	Date :		April 1990
C

	INCLUDE 'VICMAIN_FOR'		! MIPL main program

	SUBROUTINE MAIN44
	implicit integer (A-Z)
C
	integer i, j, l, count
	logical*1 Input(180), buffer(40000)
	integer Err, maxdn, numpoints, min, max
        character*1  typec, types, typel, typer, typea, typef, typen
        integer*4 halfhist(-32768:32767)
        integer*4 temphist(1:256),temphist1(1:256),temphist2(1:256)
C	integer halfhist(65536),temphist2(256),temphist1(256),temphist(256)
        integer len(10), thick(10), rgb(0:2), inunit(0:30)
	integer black(3)/0,0,0/
	integer white(3)/255,255,255/
	integer red(3)/255,0,0/
	integer green(3)/0,255,0/
	integer blue(3)/0,0,255/
	integer orange(3)/255,128,0/
	integer yellow(3)/255,255,130/
	integer violet(3)/160,80,255/
	integer purple(3)/100,0,150/
	integer brown(3)/100,60,40/
        real*8  period(2,10)
C       real*8  dmy(80)
C       real*8  lsat, hsat, iscale, oscale
        real*4  lsat, hsat, iscale, oscale
C	real*8  mean, sigma, maxi, scale 
	real*4  mean, sigma, maxi, scale 
	logical*1 Anotation_Position, Anotation_Orientation 
        logical*1 Anotation_Justification
        real*8    Anotation_Start_Value, Anotation_Increment
        real*8    Anotation_Modulus
        integer   Anotation_Significant_Digits,Anotation_Size
	character file(90)

        typec = 'C'
        types = 'S'
        typel = 'L'
        typer = 'R'
        typea = 'A'
        typef = 'F'
        typen = 'N'


	rgb(0) = 90
	rgb(1) = 90
	rgb(2) = 90

	Err = XASetDim( 3 )				! Initialize
	Err = XAInitialize( 1024,3584,rgb )		! Initialize

  	call xvp('INP',file,count)
	Do i = 1,count
		call xvunit(inunit(i-1),'INP',i,Err,' ')
	EndDo

C	Test XASTOREBUFFER at (512, 1)
	call xvopen( inunit(8), Err, 'OP','READ','OPEN_ACT','SA',
     &		'IO_ACT','SA','U_FORMAT','BYTE','O_FORMAT','BYTE',' ')
	call xvget( inunit(8), Err, 'NL', NL, 'NS', NS,' ')
	Do x=0,NL-1
	   	call xvread( inunit(8), Input, Err,' ')   
		Do y=1,NS
		 buffer(x*NS + y) = Input(y)
		EndDo
	EndDo
	call xvclose( inunit(8), Err,' ')

	Err = XaStoreBuffer( buffer, 1, 1, 180, 180, 512, 1, 1, 'BW')	
	Err = XaStoreBuffer( buffer, 1, 1, 180, 180, 692, 1, -2, 'BW')	
	Err = XaStoreBuffer( buffer, 1, 1, 180, 180, 512, 512, 2, 'BW')	
	Err = XaStoreBuffer( buffer, 1, 1, 180, 180, 512, 190, -1, 'BW')	

	Err = XaBand( inunit(0), 1 )
	Err = XaBand( inunit(1), 2 )
	Err = XaBand( inunit(2), 3 )
	Err = XaBand( inunit(5), 1 )
	Err = XaBand( inunit(6), 2 )
	Err = XaBand( inunit(7), 3 )
C
C OutLine a box
C
	Err = XAStoreVector( 1, 1, 480, 1, 1, orange )
	Err = XAStoreVector( 1, 1, 1, 480, 1, orange )
	Err = XAStoreVector( 480, 1, 480, 480, 1, orange )
	Err = XAStoreVector( 1, 480, 480, 480, 1, orange )

	Err = XAStorellipse( 400, 400, 30, 30, 30, blue )
	Err = XAStorellipse( 400, 400, 30, 30, 1, white )
   	Err = XAStorellipse( 400, 400, 30, 7, 1, white )
   	Err = XAStorellipse( 400, 400, 30, 14, 1, white )
   	Err = XAStorellipse( 400, 400, 30, 22, 1, white )
   	Err = XAStorellipse( 400, 400, 7, 30, 1, white )
   	Err = XAStorellipse( 400, 400, 14, 30, 1, white )
   	Err = XAStorellipse( 400, 400, 22, 30, 1, white )

   	Err = XAStorevector( 400, 370, 400, 429, 1, white )
   	Err = XAStorevector( 370, 400, 429, 400, 1, white )
   
C
C Start line, Start sample, Width, Length, Dn
C
	rgb(0) = 69
	rgb(1) = 160
	rgb(2) = 160
	Err = XAStoreBox( 1, 1, 480, 480, rgb )	! Background

      	Do 20 j = 0,255,32
	   If ( j .Eq. 256 ) then
		 Do 10 l=0,2
			rgb(l) = 255
   10	  	 Continue
	   else
		Do 15 l=0,2
			rgb(l) = j
   15	   	Continue
	   EndIf
	   Err = XAStoreBox( 200+j, 10, 32, 32, rgb)
	   Err = XAStoreBox( 300, 150+j, 32, 32, rgb )

	   Do 18 l=0,2
		rgb(l) = 255 - j
   18	   Continue
	   Err = XAStoreBox( 200+j, 50, 32, 32, rgb)
	   Err = XAStoreBox( 350, 150+j, 32, 32, rgb)
   20	   Continue
C
C Start line, Start sample, End line, End sample,
C Width, Starting Dn, Ending Dn
C
	Err = XAStoreGray( 10, 1, 512, 40, 0, 255, TYPEC ) 
 	Err = XAStoreGray( 60, 1, 512, 40, 255, 0, TYPEC ) 
	Err = XAStoreGray( 110, 70, 350, 40, 0, 255, TYPES ) 
   	Err = XAStoreGray( 160, 70, 350, 40, 255, 0, TYPES ) 

C
C Start line, Start sample, End line, End sample,
C Width, Length, Spacing, Dn
C
	rgb(0) = 96
	rgb(1) = 0
	rgb(2) = 192
	Err = XAStoreTick( 1, 1, 1, 480, 1, 5, 5, rgb )
	Err = XAStoreTick( 1, 1, 480, 1, 1, 5, 5, rgb )
	Err = XAStoreTick( 480-5, 1, 480-5, 480, 1, 5, 5, rgb )
	Err = XAStoreTick( 1, 480-5, 480, 480-5, 1, 5, 5, rgb )
C
C   Start line, Start sample, Size, Dn, Character String
C
	rgb(0) = 192
	rgb(1) = 255
	rgb(2) = 96
	Err = XASetFontAngle( 90 )
	Err = XAStoreString( 50, 25, 2, rgb, 'This is the regular font.',1 )
	Err = XASetFontAngle( 0 )
	Err = XAStoreString( 200, 50, 4, rgb, 'This is the regular font.',1 )
	Err = XASetFont( 'HERSHEY', 3 )
        call xastringlength ( 'THIS IS A TEST', 3, 3, length )
        call xvmessage(' THIS IS A TEST', ' ')
        call prnt(4,1,length,
     &		'The string length of the above Hershey 3 Font =.')

	Err = XASetCharSpacing( 5 )
	Err = XAStoreString( 230, 50, 2, red, 'Hershey #3 font (5sp)',2 )
C	Err = XAStoreString( 50, 50, 2, red, 'USING FORTRAN',2 )
	Err = XASetFont( 'DEFAULT', 0 )
        call xastringlength ( 'THIS IS A TEST', 3, 1, length )
        call xvmessage(' THIS IS A TEST', ' ')
        call prnt(4,1,length,
     & 		'The string length of the above Block Font =.')

	Err = XASetCharSpacing( 1 )
	Err = XAStoreString( 260, 50, 4, orange,
     *                      'This is a change back to regular.',1 )
	Err = XASetFont( 'HERSHEY', 5 )

	Err = XASetCharSpacing( -1 )
	Err = XAStoreString( 300, 50, 2, white, 'This is Hershey #5 font.',2 )

	Err = XASetFont( 'DEFAULT', 0 )

C	LIME GREEN
	rgb(0) = 25
	rgb(1) = 224
	rgb(2) = 196
C
C Store string at location (50,850)
C
	Err = XAStoreString( 50, 850, 3, rgb, 'ZOOMS & SHRINKS',1)
C 
C Store images and zoom or shrink images.
C
	Err = XaZoomImage( inunit(0), 100, 200, 200, 300, 1, 512, 1, 'In',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(1), 100, 200, 200, 300, 1, 512, -1, 'Re',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(2), 100, 200, 200, 300, 1, 512, -1, 'in',
     &			TYPEN, 0, 255, 0, 0)

	Err = XaZoomImage( inunit(0), 100, 200, 200, 300, 102, 512, 2, 'Re',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(1), 100, 200, 200, 300, 102, 512, 2, 'Re',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(2), 100, 200, 200, 300, 102, 512, 2, 'Re',
     &			TYPEN, 0, 255, 0, 0)
 
C	Store monochrome image at location ( 1, 713 )
	Err = XaStoreImage( inunit(0), 100, 200, 200, 300, 1, 
     &			612, TYPEN, 0. , 255., 0, 0) 	
   	Err = XaZoomImage( inunit(0), 100, 200, 200, 300, 1, 
     &  		713, 3,'Interpolation', TYPEN, 0, 255,0, 0)
   	Err = XaZoomImage( inunit(0), 100, 200, 200, 300, 1, 
     &  		713, 3,'Replication', TYPEN, 0, 255, 0, 0)

C	Store monochrome image at location ( 310, 512 )
	Err = XaStoreImage( inunit(1), 100, 200, 200, 300, 310, 
     &   		512,TYPEN, 0. , 255., 0, 0)
   	Err = XaZoomImage( inunit(1), 100, 200, 200, 300, 310 , 
     & 			612, -2,'Interpolation', TYPEN, 0, 255, 0, 0)

C	Store a color image at location ( 310, 712 )
	Err = XaStoreImage( inunit(0), 1, 200, 200, 400, 310, 712, 
     &	TYPEN, 0, 255, 0, 0)
	Err = XaStoreImage( inunit(1), 1, 200, 200, 400, 310, 712, 
     &	TYPEN, 0, 255, 0, 0)
	Err = XaStoreImage( inunit(2), 1, 200, 200, 400, 310, 712, 
     &	TYPEN, 0, 255, 0, 0)

C	Store zoomed by interpolation color image at location ( 310, 912 )
	Err = XaZoomImage( inunit(0), 1, 200, 200, 400, 310, 912, -4, 'Re',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(1), 1, 200, 200, 400, 310, 912, -4, 'Re',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(2), 1, 200, 200, 400, 310, 912, -4, 'Re',
     &			TYPEN, 0, 255, 0, 0)

C	Store zoomed by replication color image at location ( 360, 912 )
	Err = XaZoomImage( inunit(0), 1, 200, 200, 400, 360, 912, -3, 'Re',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(1), 1, 200, 200, 400, 360, 912, -3, 'Re',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(2), 1, 200, 200, 400, 360, 912, -3, 'Re',
     &			TYPEN, 0, 255, 0, 0)

C	Store a color image at location ( 1, 1024 )
	Err = XaStoreImage( inunit(3), 1, 1, 512, 512, 1, 1024, 
     &	TYPEN, 0, 255, 0, 0)

C	HOT PINK
	rgb(0) = 255
	rgb(1) = 89
	rgb(2) = 167

C	Calculate histogram of fourth input file
 	Err = XaCalculateHist( inunit(3), 1, 1, 512, 512, 1, 
     &  1, temphist, maxdn, numpoints,TYPEN, 0, 0, 0, 0 )

C	Print heading for image
	Err = XaStoreString( 20, 1050, 5, rgb, 'B52:',1)

C	Store vertically oriented histogram at location (50, 1050)
	Err = XaStoreHistogram( temphist, 50, 1050, 305, 1250, 
     &  maxdn, 'V', rgb )

C	BLUE
	rgb(2) = 255
	rgb(0) = 89
	rgb(1) = 167

C	Store horizontally oriented histogram at location (300, 1050)
	Err = XaStoreHistogram( temphist, 300, 1050, 500, 1305, maxdn, 
     &	'H', rgb )

C	Store shrunk by interpolation color image at location ( 1, 1536 )
	Err = XaZoomImage( inunit(0), 1, 1, 512, 512, 1, 1536, -4, 'In',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(1), 1, 1, 512, 512, 1, 1536, -4, 'In',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(2), 1, 1, 512, 512, 1, 1536, -4, 'In',
     &			TYPEN, 0, 255, 0, 0)

C	Store zoomed by replication color image at location ( 1, 2048 )
	Err = XaZoomImage( inunit(0), 1, 1, 512, 512, 1, 2048, -4, 'In',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(1), 1, 1, 512, 512, 1, 2048, -4, 'In',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(2), 1, 1, 512, 512, 1, 2048, -4, 'In',
     &			TYPEN, 0, 255, 0, 0)

C	Calculate histogram of three color bands of first image file
 	Err = XaCalculateHist( inunit(0), 1, 1, 512, 512, 1, 
     &  1, temphist, maxdn, numpoints,TYPEN, 0, 0, 0, 0 )
	maximum = maxdn
 	Err = XaCalculateHist( inunit(1), 1, 1, 512, 512, 1, 
     &  1, temphist1, maxdn, numpoints,TYPEN, 0, 0, 0, 0 )
	If ( maxdn.Gt.maximum ) maximum = maxdn
 	Err = XaCalculateHist( inunit(2), 1, 1, 512, 512, 1, 
     &  1, temphist2, maxdn, numpoints,TYPEN, 0, 0, 0, 0 )
	If ( maxdn.Lt.maximum ) maxdn = maximum

C	Red band histograms
	Err = XaStoreHistogram(temphist, 50, 1640, 130, 1895, 
     &  maxdn,'H',red)
	Err = XaStoreHistogram(temphist, 320, 1640, 400, 1895, 
     &  maxdn,'Hm',red)
	Err = XaStoreHistogram(temphist, 410, 1640, 490, 1895, 
     &  maxdn,'Hb',red)
	Err = XaStoreHistogram(temphist, 150, 2060, 405, 2140, 
     &  maxdn,'V',red)
	Err = XaStoreHistogram(temphist, 150, 2330, 405, 2410, 
     &  maxdn,'Vm',red)
	Err = XaStoreHistogram(temphist, 150, 2420, 405, 2500, 
     &  maxdn,'Vb',red)

C	Green band histograms
	Err = XaStoreHistogram(temphist1,140, 1640, 220, 1895, 
     &  maxdn,'H',green)
	Err = XaStoreHistogram(temphist1,320, 1640, 400, 1895,
     &  maxdn,'Hm',green)
	Err = XaStoreHistogram(temphist1,410, 1640, 490, 1895,
     &  maxdn,'Hb',green)
	Err = XaStoreHistogram(temphist1,150, 2150, 405, 2230,
     &  maxdn,'V',green)
	Err = XaStoreHistogram(temphist1,150, 2330, 405, 2410,
     &  maxdn,'Vm',green)
	Err = XaStoreHistogram(temphist1,150, 2420, 405, 2500,
     &  maxdn,'VB',green)

C	Blue band histograms
	Err = XaStoreHistogram(temphist2, 230, 1640, 310, 1895,
     &  maxdn,'H',blue)
	Err = XaStoreHistogram(temphist2, 320, 1640, 400, 1895,
     &  maxdn,'Hm',blue)
	Err = XaStoreHistogram(temphist2, 410, 1640, 490, 1895, 
     &  maxdn,'Hb',blue)
	Err = XaStoreHistogram(temphist2, 150, 2240, 405, 2320, 
     &  maxdn,'V',blue)
	Err = XaStoreHistogram(temphist2, 150, 2330, 405, 2410, 
     &  maxdn,'Vm',blue)
	Err = XaStoreHistogram(temphist2, 150, 2420, 405, 2500, 
     &  maxdn,'Vb',blue)


C	Store shrunk by interpolation color image at location ( 1, 2560 )
	Err = XaZoomImage( inunit(0), 1, 1, 512, 512, 1, 2560, -2, 'In',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(1), 1, 1, 512, 512, 1, 2560, -2, 'In',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(2), 1, 1, 512, 512, 1, 2560, -2, 'In',
     &			TYPEN, 0, 255, 0, 0)

C	Store zoomed by replication color image at location ( 1, 2816 )
	Err = XaZoomImage( inunit(0), 1, 1, 512, 512, 1, 2816, -2, 'In',
     &			TYPEF, 60.,200., 0, 0)
	Err = XaZoomImage( inunit(1), 1, 1, 512, 512, 1, 2816, -2, 'In',
     &			TYPEF, 60., 200., 0, 0)
	Err = XaZoomImage( inunit(2), 1, 1, 512, 512, 1, 2816, -2, 'In',
     &			TYPEF, 60., 200., 0, 0)

C	Calculate histogram of three color bands of stretched image 
 	Err = XaCalculateHist( inunit(0), 1, 1, 512, 512, 1, 
     &  1, temphist, maxdn, numpoints, TYPEN, 60., 200., 0, 0 )
	maximum = maxdn
 	Err = XaCalculateHist( inunit(1), 1, 1, 512, 512, 1, 
     &  1, temphist1, maxdn, numpoints, TYPEN, 60., 200., 0, 0 )
	If(maxdn.GT.maximum) maximum=maxdn
 	Err = XaCalculateHist( inunit(2), 1, 1, 512, 512, 1, 
     &  1, temphist2, maxdn, numpoints,TYPEN, 60., 200., 0, 0 )
	If(maxdn.GT.maximum) maximum=maxdn

	Err = XaStoreHistogram(temphist, 300, 2800, 400, 3055, 
     &  maximum,'Hm',red)
	Err = XaStoreHistogram(temphist1,300, 2800, 400, 3055, 
     &  maximum,'Hm',green)
	Err = XaStoreHistogram(temphist2,300, 2800, 400, 3055, 
     &  maximum,'Hm',blue)


C	Store shrunk by interpolation color image at location ( 1, 3072 )
	Err = XaZoomImage( inunit(0), 1, 1, 512, 512, 1, 3072, -2, 'In',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(1), 1, 1, 512, 512, 1, 3072, -2, 'In',
     &			TYPEN, 0, 255, 0, 0)
	Err = XaZoomImage( inunit(2), 1, 1, 512, 512, 1, 3072, -2, 'In',
     &			TYPEN, 0, 255, 0, 0)

C	Store zoomed by replication color image at location ( 1, 3327 )
	Err = XaZoomImage( inunit(5), 1, 1, 512, 512, 1, 3327, -2, 'In',
     &			TYPEN, 60., 200., 0, 0)
	Err = XaZoomImage( inunit(6), 1, 1, 512, 512, 1, 3327, -2, 'In',
     &			TYPEN, 60., 200., 0, 0)
	Err = XaZoomImage( inunit(7), 1, 1, 512, 512, 1, 3327, -2, 'In',
     &			TYPEN, 60., 200., 0, 0)

C	Calculate histogram of three color bands of stretched image 
 	Err = XaCalculateHist( inunit(5), 1, 1, 512, 512, 1, 
     &  1, temphist, maxdn, numpoints, TYPEN, 60., 200., 0, 0 )
	maximum = maxdn
 	Err = XaCalculateHist( inunit(6), 1, 1, 512, 512, 1, 
     &  1, temphist1, maxdn, numpoints, TYPEN, 60., 200., 0, 0 )
	If(maxdn.GT.maximum) maximum=maxdn
 	Err = XaCalculateHist( inunit(7), 1, 1, 512, 512, 1, 
     &  1, temphist2, maxdn, numpoints,TYPEN, 60., 200., 0, 0 )
	If(maxdn.GT.maximum) maximum=maxdn

	Err = XaStoreHistogram(temphist, 300, 3200, 500, 3455, maximum,
     &	'Hm',violet)
	Err = XaStoreHistogram(temphist1,300, 3200, 500, 3455, maximum,
     &	'Hm',brown)
	Err = XaStoreHistogram(temphist2,300, 3200, 500, 3455, maximum,
     &	'Hm',yellow)


C	Calculate histogram of three color bands of stretched image 
 	Err = XaCalculateHist( inunit(0), 1, 1, 512, 512, 1, 
     &  1, temphist, maxdn, numpoints, TYPEN, 60., 200., 0, 0 )
	maximum = maxdn
 	Err = XaCalculateHist( inunit(1), 1, 1, 512, 512, 1, 
     &  1, temphist1, maxdn, numpoints, TYPEN, 60., 200., 0, 0 )
	If(maxdn.GT.maximum) maximum=maxdn
 	Err = XaCalculateHist( inunit(2), 1, 1, 512, 512, 1, 
     &  1, temphist2, maxdn, numpoints,TYPEN, 60., 200., 0, 0 )
	If(maxdn.GT.maximum) maximum=maxdn

	Err = XaStoreHistogram(temphist, 300, 3200, 500, 3455, 
     &  maximum,'Hm',red)
	Err = XaStoreHistogram(temphist1,300, 3200, 500, 3455, 
     &  maximum,'Hm',green)
	Err = XaStoreHistogram(temphist2,300, 3200, 500, 3455, 
     &  maximum,'Hm',blue)

C       check these routines

C	Calculate histogram of three color bands of stretched image 
 	Err = XaCalculateHist( inunit(0), 1, 1, 512, 512, 1, 
     &  1, temphist, maxdn, numpoints, TYPEF, 60., 200., 0, 0 )
	maximum = maxdn
 	Err = XaCalculateHist( inunit(1), 1, 1, 512, 512, 1, 
     &  1, temphist1, maxdn, numpoints, TYPEF, 60., 200., 0, 0 )
	If(maxdn.GT.maximum) maximum=maxdn
 	Err = XaCalculateHist( inunit(2), 1, 1, 512, 512, 1, 
     &  1, temphist2, maxdn, numpoints,TYPEF, 60., 200., 0, 0 )
	If(maxdn.GT.maximum) maximum=maxdn
  
        scale = 1.0
	maxi = maximum
        If ( maxi.GT.1.0 ) Then
		scale = 100.0 / log10( maxi )
	        Do x=1,256
			If( temphist(x).LE.1.0 ) then
				temphist(x) = 0
			Else
				maxi = temphist(x)
				temphist(x) = scale * log10( maxi ) + 0.5
			EndIf
			If( temphist1(x).LE.1.0 ) then
				temphist1(x) = 0
			Else
				maxi = temphist1(x)
				temphist1(x) = scale * log10( maxi ) + 0.5
			EndIf
			If( temphist2(x).LE.1.0 ) then
				temphist2(x) = 0
			Else
				maxi = temphist2(x)
				temphist2(x) = scale * log10( maxi ) + 0.5
			EndIf
		EndDo
	EndIf

	Err = XaStoreHistogram(temphist, 410, 2800, 510, 3055, 
     &  100,'Hm',red)
	Err = XaStoreHistogram(temphist1,410, 2800, 510, 3055, 
     &  100,'Hm',green)
	Err = XaStoreHistogram(temphist2,410, 2800, 510, 3055, 
     &  100,'Hm',blue)


C	Test of HALFWORD display, HALFWORD histograms, and HALFWORD stretched
	Err = XaStoreString( 562, 3356, 2, red, 'shrink halfword',1)
	Err = XaStoreString( 818, 3356, 2, green, 
     &	'zoom-stretch halfword (10K-30K)',1)

	Err = XaCalculateHist( inunit(4),1,1,512,512,1,1,halfhist,maxdn,
     &	numpoints, TYPEN, 60., 200., 0, 0)
        Err = XaZoomImage( inunit(4),1,1,512,512,512,3328,-2,'Re',
     &  TYPEN,0,255, 0, 0)
	Err = HiStat2( halfhist, numpoints,mean,sigma,min,max,maxdn)
C       Err = GetScale( 2, dmy, max, iscale, oscale, Err )
        iscale = 1.0
        oscale = 128.0
        iscale = iscale / oscale
        Err = HiScale( halfhist, numpoints, iscale,temphist,lsat,hsat )
	Err = XaStoreHistogram( temphist, 668, 3328, 768, 3583, maxdn, 
     &	'h', green )

	Err = XaCalculateHist( inunit(4),1,1,512,512,1,1,halfhist,maxdn,
     &  numpoints, TYPEF, 10000., 30000., 0, 0)
	Err = XaZoomImage( inunit(4),1,1,512,512,768,3328,-2,'in',TYPEF,
     & 	10000.,30000., 0, 0)
	Err = HiStat2( halfhist, numpoints,mean,sigma,min,max,maxdn)
C       Err = GetScale( 2, dmy, max, iscale, oscale, Err )
        iscale = 1.0
        oscale = 128.0
        iscale = iscale / oscale
        Err = HiScale( halfhist, numpoints, iscale,temphist,lsat,hsat )
	Err = XaStoreHistogram( temphist, 924, 3328, 1024, 3583, maxdn, 
     &	'h', green )

C	Write comments for annotation tests
	err = xastorestring( 825, 3220, 2, black, 
     &  'right_just,orient=L', 1)
	err = xastorestring( 530, 3150, 2, black, 
     &  'left_just,orient=L', 1)
	err = xastorestring( 825, 2900, 2, black, 
     &  'right_just,orient=R', 1)
	err = xastorestring( 530, 2650, 2, black, 
     &  'left_just,orient=R', 1)
	err = xastorestring( 800, 2400, 2, black, 
     &  'right_just,orient=V', 1)
	err = xastorestring( 530, 2150, 2, black, 
     &  'left_just,orient=V', 1)


        elength =  0
        ethick  =  0
        cthick  =  12
        null    =  0 

        Anotation_Position = ' '
        Anotation_Justification = ' '
        Anotation_Orientation = ' '

	Anotation_Start_Value = 0.0 
        Anotation_Increment = 0.0
        Anotation_Modulus = 0.0
        Anotation_Significant_Digits = 0
        Anotation_Size = 0

        err = xadefinescale(TYPEC, elengeh, ethick, cthick, null,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)


        len(1) = 140 
        len(2) =  84
        len(3) =  56
        len(4) =  56

        thick(1) = 12
        thick(2) = 12
        thick(3) = 12
        thick(4) = 12

        period(1,1)=  500.0
        period(1,2)=  100.0
        period(1,3)=  50.0
        period(1,4)=  10.0
        period(2,1)=  0.0
        period(2,2)=  0.0
        period(2,3)=  0.0
        period(2,4)=  0.0

        array_length = 3 

        Anotation_Start_Value = -100.0 
        Anotation_Increment = 100.0
        Anotation_Significant_Digits = 0
        Anotation_Size = 15
        Anotation_Orientation = 'V'
        Anotation_Position = 'T'
        Anotation_Justification = 'R'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 520, 1024, 520, 2048, red )

        Anotation_Position = 'B'
        Anotation_Justification = 'L'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 520, 1024, 520, 2048, red )

        Anotation_Position = 'C'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 520, 1024, 520, 2048, red )

	Err = XaClearScale()

        err = xadefinescale(TYPEC, elengeh, ethick, cthick, null,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)

        Anotation_Position = 'T'
        Anotation_Justification = 'R'
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 1000, 1024, 1000, 2048, orange )

        Anotation_Position = 'B'
        Anotation_Justification = 'L'
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 1000, 1024, 1000, 2048, orange )

        Anotation_Position = 'C'
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 1000, 1024, 1000, 2048, orange )

C	Clear all scale characteristics
	Err = XaClearScale()

	array_length = 4

        elength =  0
        ethick  =  0
        cthick  =  1
        null    =  0 

        err = xadefinescale(TYPEC, elengeh, ethick, cthick, null,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)


        len(1) = 40 
        len(2) =  6
        len(3) =  4
        len(4) =  3

        thick(1) = 2
        thick(2) = 1
        thick(3) = 1
        thick(4) = 1

	period(1,1)= 64.0
        period(1,2)= 16.0
        period(1,3)= 8.0
        period(1,4)= 4.0
        
        Anotation_Start_Value = -1.0 
        Anotation_Increment = 1.0
        Anotation_Significant_Digits = 1
        Anotation_Size = 2

C TEST OF LEFT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='V', VERT/HORIZ 
 	Anotation_Justification = 'L'
	Anotation_Orientation = 'V'
	Anotation_Position = 'C'

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 550, 2150, 750, 2150, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 700, 2250, 700, 2550, white )

	Anotation_Position = 'T'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 550, 2150, 750, 2150, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 700, 2250, 700, 2550, white )

	Anotation_Position = 'B'

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 550, 2150, 750, 2150, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 700, 2250, 700, 2550, white )


C	TEST OF RIGHT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='V', VERT/HORIZ 
 	Anotation_Justification = 'R'
	Anotation_Position = 'C'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 820, 2150, 980, 2150, white ) 

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
   	err =  xascale( 925, 2250, 925, 2550, white ) 


	Anotation_Position = 'T'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 820, 2150, 980, 2150, white ) 

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
   	err =  xascale( 925, 2250, 925, 2550, white ) 

	Anotation_Position = 'B'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 820, 2150, 980, 2150, white ) 

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
   	err =  xascale( 925, 2250, 925, 2550, white ) 


C	TEST OF LEFT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='R', VERT/HORIZ 
 	Anotation_Justification = 'L'
	Anotation_Orientation = 'R'
	Anotation_Position = 'C'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)

	err =  xascale( 550, 2650, 750, 2650, white )
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 700, 2750, 700, 3050, white )

	Anotation_Position = 'T'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 550, 2650, 750, 2650, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 700, 2750, 700, 3050, white )

	Anotation_Position = 'B'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 550, 2650, 750, 2650, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 700, 2750, 700, 3050, white )

C  TEST OF RIGHT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='R', VERT/HORIZ 
	Anotation_Justification = 'R'
	Anotation_Position = 'C'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 820, 2650, 980, 2650, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 925, 2750, 925, 3050, white )

	Anotation_Position = 'T'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 820, 2650, 980, 2650, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 925, 2750, 925, 3050, white )

	Anotation_Position = 'B'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 820, 2650, 980, 2650, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 925, 2750, 925, 3050, white )

   	Anotation_Size = 3 
   	Anotation_Significant_Digits = 0 

C TEST OF LEFT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='L', VERT/HORIZ 
	Anotation_Orientation = 'L'
 	Anotation_Justification = 'L'
	Anotation_Position = 'C'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 550, 3150, 750, 3150, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
 	err =  xascale( 700, 3225, 700, 3325, white )

	Anotation_Position = 'B'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 550, 3150, 750, 3150, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
 	err =  xascale( 700, 3225, 700, 3325, white )

	Anotation_Position = 'T'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 550, 3150, 750, 3150, white )

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
 	err =  xascale( 700, 3225, 700, 3325, white )


C	TEST OF RIGHT JUSTIFIED, POSITIONS=C,T,B, ORIENTATION='L', VERT/HORIZ 
 	Anotation_Justification = 'R'
	Anotation_Position = 'C'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 820, 3150, 980, 3150, white)

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 925, 3225, 925, 3325, white )

	Anotation_Position = 'T'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 820, 3150, 980, 3150, white)

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 925, 3225, 925, 3325, white )

	Anotation_Position = 'B'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 820, 3150, 980, 3150, white)

        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
	err =  xascale( 925, 3225, 925, 3325, white )

	Call XACopyMask(' ')
	Return
	End

C
C B/W test program for the GMASK routines
C Tests Fortran interface.
C
C 	Author :	Justin McNeill
C	Date :		April 1990
C

	INCLUDE 'VICMAIN_FOR'		! MIPL main program

	SUBROUTINE MAIN44
	implicit integer (A-Z)
C
	integer i, j, count
	integer Err, dummy(10), maxdn, numpoints
	logical*1 Input(180), buffer(40000)
        character*1  typec, types, typel, typer, typea, typef, typen
        integer*4 halfhist(-32768:32767)
        integer*4 temphist(0:255)

C	integer halfhist(65536),temphist(256)
        integer min, max
        integer len(10), thick(10), rgb, inunit(0:30)
	integer black/0/
	integer white/255/
	integer grey/128/

        real*8  period(2,10), mean, sigma
C       real*8  dmy(80)
C       real*8  lsat, hsat, iscale, oscale
        real*4  lsat, hsat, iscale, oscale
        character*1 Anotation_Position, Anotation_Orientation 
        character*1 Anotation_Justification
        real*8    Anotation_Start_Value, Anotation_Increment
        real*8    Anotation_Modulus
        integer   Anotation_Significant_Digits,Anotation_Size
	character file(90)
C

	Err = xainitialize( 1024,2560,90 )		! Initialize

  	call xvp('INP',file,count)

	Do i = 1,count
		call xvunit(inunit(i-1),'INP',i,Err,' ')
	EndDo

        typec = 'C'
        types = 'S'
        typel = 'L'
        typer = 'R'
        typea = 'A'
        typef = 'F'
        typen = 'N'

C	Test XASTOREBUFFER at (512, 1)
	call xvopen( inunit(4), Err, 'OP','READ','OPEN_ACT','SA',
     &		'IO_ACT','SA','U_FORMAT','BYTE','O_FORMAT','BYTE',' ')

	call xvget( inunit(4), Err, 'NL', NL, 'NS', NS,' ')

	Do x=0,NL-1
	   	call xvread( inunit(4), Input, Err,' ')   
		Do y=1,NS
			buffer(x*NS + y) = Input(y)
		EndDo
	EndDo
	call xvclose( inunit(4), Err, ' ')

	Err = xastorebuffer( buffer, 1, 1, 180, 180, 512, 1, 1, 'BW')	
	Err = xastorebuffer( buffer, 1, 1, 180, 180, 692, 1, -2, 'BW')	
	Err = xastorebuffer( buffer, 1, 1, 180, 180, 512, 512, 2, 'BW')	
	Err = xastorebuffer( buffer, 1, 1, 180, 180, 512, 190, -1, 'BW')	
C
C OutLine a box
C
	Err = xastorevector( 1, 1, 480, 1, 1, white)
	Err = xastorevector( 1, 1, 1, 480, 1, white )
	Err = xastorevector( 480, 1, 480, 480, 1, white)
	Err = xastorevector( 1, 480, 480, 480, 1, white)
C
C Store something like a globe.
C   
	Err = xastorellipse( 400, 400, 30, 30, 30, black )
	Err = xastorellipse( 400, 400, 30, 30, 1, white )
   	Err = xastorellipse( 400, 400, 30, 7, 1, white )
   	Err = xastorellipse( 400, 400, 30, 14, 1, white )
   	Err = xastorellipse( 400, 400, 30, 22, 1, white )
   	Err = xastorellipse( 400, 400, 7, 30, 1, white )
   	Err = xastorellipse( 400, 400, 14, 30, 1, white )
   	Err = xastorellipse( 400, 400, 22, 30, 1, white )

   	Err = xastorevector( 400, 370, 400, 429, 1, white )
   	Err = xastorevector( 370, 400, 429, 400, 1, white )
   
C
C Start line, Start sample, Width, Length, Dn
C
	Err = xastorebox( 1, 1, 480, 480, grey )	! Background

      	Do 20 j = 0,255,32
	   If ( j .Eq. 256 ) then
		rgb = 255
	   else
		rgb = j
	   EndIf
	   Err = xastorebox( 200+j, 10, 32, 32, rgb)
	   Err = xastorebox( 300, 150+j, 32, 32, rgb )

	   rgb = 255 - j
	   Err = xastorebox( 200+j, 50, 32, 32, rgb)
	   Err = xastorebox( 350, 150+j, 32, 32, rgb)
   20	   Continue
C
C Start line, Start sample, End line, End sample,
C Width, Starting Dn, Ending Dn
C
	Err = xastoregray( 10, 1, 512, 40, 0, 255, TYPEC ) 
 	Err = xastoregray( 60, 1, 512, 40, 255, 0, TYPEC ) 
	Err = xastoregray( 110, 70, 350, 40, 0, 255, TYPES ) 
   	Err = xastoregray( 160, 70, 350, 40, 255, 0, TYPES ) 

C
C Start line, Start sample, End line, End sample,
C Width, Length, Spacing, Dn
C
	Err = xastoretick( 1, 1, 1, 480, 1, 5, 5, white)
	Err = xastoretick( 1, 1, 480, 1, 1, 5, 5, white)
	Err = xastoretick( 480-5, 1, 480-5, 480, 1, 5, 5, white)
	Err = xastoretick( 1, 480-5, 480, 480-5, 1, 5, 5, white)
C
C   Start line, Start sample, Size, Dn, Character String
C
	Err = xasetfontangle( 90 )
	Err = xastorestring( 50, 25, 2, white, 'This is the regular font.',1 )
	Err = xasetfontangle( 0 )
	Err = xastorestring( 200, 50, 4, white, 'This is the regular font.',1 )
	Err = xasetfont( 'HERSHEY', 3 )
        call xastringlength ( 'THIS IS A TEST', 3, 3, length )
        call xvmessage(' ***THIS IS A TEST', ' ')
        call prnt(4,1,length,
     &		'The string length of the above Hershey 3 Font =.')

	Err = xasetcharspacing( 5 )
	Err = xastorestring( 230, 50, 2, white, 'Hershey #3 font (5sp)',2 )
C	Err = xastorestring( 50, 50, 2, white, 'USING FORTRAN',2 )
	Err = xasetfont( 'DEFAULT', 0 )
        call xastringlength ( 'THIS IS A TEST', 3, 1, length )
        call xvmessage(' ***THIS IS A TEST', ' ')
        call prnt(4,1,length,
     & 		'The string length of the above Block Font =.')

	Err = xasetcharspacing( 1 )
	Err = xastorestring( 260, 50, 4, white,
     *                      'This is a change back to regular.',1 )
	Err = xasetfont( 'HERSHEY', 5 )

	Err = xasetcharspacing( -1 )
	Err = xastorestring( 300, 50, 2, white, 'This is Hershey #5 font.',2 )

	Err = xasetfont( 'DEFAULT', 0 )
C
C Store string at location (50,850)
C
	Err = xastorestring( 50, 850, 3, white, 'ZOOMS & SHRINKS',1)
C 
C Store images and zoom or shrink images.
C
	Err = xazoomimage( inunit(0), 100, 200, 200, 300, 1, 512, 1, 'In',
     &			TYPEN, 0.0, 255.0, 0, 0)

	Err = xazoomimage( inunit(0), 100, 200, 200, 300, 102, 512, 2, 'Re',
     &			TYPEN, 0.0, 255.0, 0, 0)
 
C	Store image at location ( 1, 713 )
	Err = xastoreimage( inunit(0), 100, 200, 200, 300, 1, 
     &			612, TYPEN, 0. , 255., 0, 0 ) 	
   	Err = xazoomimage( inunit(0), 100, 200, 200, 300, 1, 
     &  		713, 3,'Interpolation', TYPEN, 0.0, 255.0, 0, 0)
   	Err = xazoomimage( inunit(0), 100, 200, 200, 300, 1, 
     &  		713, 3,'Replication', TYPEN, 0.0, 255.0, 0, 0)

C	Store image at location ( 310, 512 )
	Err = xastoreimage( inunit(0), 100, 200, 200, 300, 310, 
     &   		512, TYPEN, 0. , 255., 0, 0 )
   	Err = xazoomimage( inunit(0), 100, 200, 200, 300, 310 , 
     & 			612, -2,'Interpolation', TYPEN, 0.0, 255.0, 0, 0)

C	Store a image at location ( 310, 712 )
	Err = xastoreimage( inunit(0), 1, 200, 200, 400, 310, 712, 
     &	TYPEN, 0., 255., 0, 0)

C	Store zoomed by interpolation image at location ( 310, 912 )
	Err = xazoomimage( inunit(0), 1, 200, 200, 400, 310, 912, -4, 'Re',
     &			TYPEN, 0.0, 255.0, 0, 0)

C	Store zoomed by replication image at location ( 360, 912 )
	Err = xazoomimage( inunit(0), 1, 200, 200, 400, 360, 912, -3, 'Re',
     &			TYPEN, 0.0, 255.0, 0, 0)

C	Store a image at location ( 1, 1024 )
	Err = xastoreimage( inunit(1), 1, 1, 512, 512, 1, 1024, 
     &	TYPEN, 0., 255. , 0, 0 )

C	Calculate histogram of fourth input file
 	Err = xacalculatehist( inunit(1), 1, 1, 512, 512, 1, 
     &  1, temphist, maxdn, numpoints,TYPEN, 0, 0, dummy, 0 )

C	Print heading for image
	Err = xastorestring( 20, 1050, 5, white,'B52:',1)

C	Store vertically oriented histogram at location (50, 1050)
	Err = xastorehistogram( temphist, 50, 1050, 305, 1250, 
     &  maxdn, 'V', black)

C	Store horizontally oriented histogram at location (300, 1050)
	Err = xastorehistogram( temphist, 300, 1050, 500, 1305, maxdn, 
     &	'H', grey )


C	Store shrunk by interpolation image at location ( 1, 1536 )
	Err = xazoomimage( inunit(0), 1, 1, 512, 512, 1, 1536, -2, 'In',
     &			TYPEN, 0.0, 255.0, 0, 0)

C	Store zoomed by replication image at location ( 1, 1792 )

C       Err = xazoomimage( inunit(0), 1, 1, 512, 512, 1, 1792, -2, 'In',
C    &			TYPEA, 1., 0.05, dummy, 1)

        Err = xazoomimage( inunit(0), 1, 1, 512, 512, 1, 1792, -2, 'In',
     &			TYPEA, 1., 0.05, 0, 0)

 	Err = xacalculatehist( inunit(0), 1, 1, 512, 512, 1, 
     &  	1, temphist, maxdn, numpoints, TYPEN, 0, 0, dummy, 0 )
	Err = xastorehistogram(temphist, 300, 1700, 400, 1955, 
     &  maxdn,'H',white)


C	Test of HALFWORD display, HALFWORD histograms, and HALFWORD stretched
	Err = xastorestring( 50, 2100, 2, white, 'shrink halfword',1)
	Err = xastorestring( 50, 2400, 2, white, 'nozoom halfword',1)
	Err = xastorestring( 306, 2100, 2, white, 
     &	'zoom-stretch halfword (10K-30K)',1)


	Err = xacalculatehist( inunit(2),1,1,512,512,1,1,halfhist,maxdn,
     &	numpoints, TYPEN, 60., 200., dummy, 0)
        Err = xazoomimage( inunit(2),1,1,512,512,1,2048,-2,'Re',
     &  TYPEN,0.0,255.0,0,0)
	Err = xazoomimage( inunit(2),1,1,256,256,1,2305,1,'Re',
     &  TYPEN,0.0,255.0,0,0)
	Err = histat2( halfhist, numpoints,mean,sigma,min,max,maxdn)
C       Err = getscale( 2, dmy, max, iscale, oscale, Err )
        iscale = 1.0
        oscale = 128.0
        iscale = iscale / oscale
        Err = hiscale( halfhist, numpoints, iscale,temphist,lsat,hsat )
	Err = xastorehistogram( temphist, 156, 2048, 256, 2304, maxdn, 
     &	'h', grey )

	Err = xacalculatehist( inunit(2),1,1,512,512,1,1,halfhist,maxdn,
     &  numpoints, TYPEF, 10000., 30000., dummy, 0)
	Err = xazoomimage( inunit(2),1,1,512,512,256,2048,-2,'in',TYPEF,
     & 	10000.,30000.,0,0)
	Err = histat2( halfhist, numpoints,mean,sigma,min,max,maxdn)
C       Err = getscale( 2, dmy, max, iscale, oscale, Err )
        iscale = 1.0
        oscale = 128.0
        iscale = iscale / oscale
        Err = hiscale( halfhist, numpoints, iscale,temphist,lsat,hsat )
	Err = xastorehistogram( temphist, 412, 2048, 512, 2304, maxdn, 
     &	'h', grey )

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

        err = xascale( 520, 1024, 520, 2048, white)

        Anotation_Position = 'B'
        Anotation_Justification = 'L'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 520, 1024, 520, 2048, white)

        Anotation_Position = 'C'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 520, 1024, 520, 2048, white)

	ERR = xaclearscale()
        Anotation_Position = ' '
        Anotation_Justification = ' '
        Anotation_Orientation = ' '

        err = xadefinescale(TYPEC, elengeh, ethick, cthick, null,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)


        Anotation_Position = 'T'
        Anotation_Justification = 'R'
        Anotation_Orientation = 'V'
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 1000, 1024, 1000, 2048, white )

        Anotation_Position = 'B'
        Anotation_Justification = 'L'
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 1000, 1024, 1000, 2048, white )

        Anotation_Position = 'C'
        err = xadefinescale(TYPER, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 1000, 1024, 1000, 2048, white )


        elength =  0
        ethick  =  0
        cthick  =  1
        null    =  0 
	ERR = xaclearscale()

        Anotation_Position = ' '
        Anotation_Justification = ' '
        Anotation_Orientation = ' '

        err = xadefinescale(TYPEC, elengeh, ethick, cthick, null,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)

        len(1) = 10 
        len(2) =  8
        len(3) =  6
        len(4) =  4

        thick(1) = 1
        thick(2) = 1
        thick(3) = 1
        thick(4) = 1

        period(1,1)=  30.0
        period(1,2)=  15.0
        period(1,3)=  10.0
        period(1,4)=  5.0
        period(2,1)=  0.0
        period(2,2)=  0.0
        period(2,3)=  0.0
        period(2,4)=  0.0

        array_length = 3 

	Anotation_Start_Value = 36.0 
        Anotation_Increment = 30.0
        Anotation_Modulus = 35.0
        Anotation_Significant_Digits = 0
        Anotation_Size = 2

        Anotation_Orientation = 'V'
        Anotation_Position = 'C'
        Anotation_Justification = 'R'
        err = xadefinescale(TYPEL, len, thick, period, array_length,
     &                      Anotation_Position, Anotation_Start_Value,
     &                      Anotation_Increment, Anotation_Modulus,
     &                      Anotation_Significant_Digits,Anotation_Size, 
     &                   Anotation_Orientation, Anotation_Justification)
        err = xascale( 670, 1400, 890, 1400, white)

	Call xacopymask(' ')
	Return
	End

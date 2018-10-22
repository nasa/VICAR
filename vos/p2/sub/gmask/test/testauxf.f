C
C B/W test program for XAOUTSIZE and inverse stretches in XASTOREIMAGE
C and XAZOOMIMAGE 
C Tests Fortran interface.
C
C 	Author :	Justin McNeill
C	Date :		January 1991
C

	INCLUDE 'VICMAIN_FOR'		! MIPL main program

	SUBROUTINE MAIN44
	implicit integer (A-Z)
C
	integer count,i
	integer Err, numpoints
        character*1  typec, types, typel, typer, typea, typef, typen

	integer temphist(256),  max
        integer inunit(0:30)

	character file(90)

        typec = 'C'
        types = 'S'
        typel = 'L'
        typer = 'R'
        typea = 'A'
        typef = 'F'
        typen = 'N'

	Err = XAInitialize( 0,0,90 )		! Initialize

	Err = XAOutsize( 512,512,180 )		! Set mask size at 512x512

  	call xvp('INP',file,count)

	Do i = 1,count
		call xvunit(inunit(i-1),'INP',i,Err,' ')
	EndDo
 
C	Store inverse stretch of image at location ( 1, 1 )
	Err = XaStoreImage( inunit(0), 1, 1, 100, 100, 1, 
     &			1, TYPEF, 50. , 10., 0, 0) 	

C	Calculate and store histogram of inverse stretch 
	Err = XACalculatehist( inunit(0), 1, 1, 100, 100, 1, 1, 
     &  temphist, max, numpoints, TYPEF, 50., 10., 0, 0 )

	Err = XAStorehistogram( temphist, 100, 1, 200, 100, max, 
     &  'H', 255 )

C 	Store inverse stretch zoom of image at location ( 101, 101 )
   	Err = XaZoomImage( inunit(0), 1, 1, 50, 50, 101, 
     &  		101, 2,'Interpolation', TYPEF, 50., 10., 0, 0)

	Call XACopyMask(' ')
	Return
	End


C
C
C	FORTRAN Language Bridges for GENERAL NAIF TOOLKIT ROUTINES:
C
C		BODN2C_G
C
C

c***************************************** 
c  xbodn2c_g:
C  2nd-stage bridge to BODN2C_G, in Fortran
c*****************************************  
      subroutine xbodn2c_g(body_name, i, body_id, status)
      
      byte body_name(1)
      integer body_id
      integer i
      integer status
      character*80 text
      logical found

      text=' '

      if (i.gt.80) call xvmessage('xbodn2c_g, string too long',' ')

C     Transformation to Fortran-string
      call mvlc(body_name, text, i)

      call bodn2c_g(text, body_id, found)

      if (found) then
	status = 1
      else
	status = 0
      endif 
  
      return
      end


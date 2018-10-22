C  Bridge to PBDATA, in Fortran

      INTEGER FUNCTION xpbdata(name, i, buf)
     
      real*4 buf(20)
      byte name(1)
      integer i
      character*12 text

      text=' '

      if (i.gt.12) call xvmessage('xpbdata, string is too long',' ')

C     Transformation to Fortran-string
      call mvlc(name, text, i)
      call pbdata(text,buf,*100)
      xpbdata = 1
      goto 999

 100  xpbdata = 0

 999  return
      end

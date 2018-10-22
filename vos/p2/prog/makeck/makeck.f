      INCLUDE 'VICMAIN_FOR'
C Program to create a new C-kernel.
      SUBROUTINE MAIN44
C
      CHARACTER*(132) FILE
      INTEGER  HANDLE,ICNT,IDEF

      CALL XVPARM('OUT',file,icnt,idef,1)
C           Open with the file identifier equal to the file name
C           (it doesn't have to be this).
C
      CALL CKOPN(FILE,FILE,0,HANDLE)
C
C  We call DAFCLS rather than CKCLS because CKCLS throws an error
C  if the file contains no segments.
C
      CALL DAFCLS(HANDLE)
      END

      SUBROUTINE CHKSTAT(STATUS, MSG, ABFLAG, DATA, NDAT)
c
C  All 5 ARGS are required Input parameters 
c
C  STATUS - Return if 0 or 1, else print MSG 
C  MSG    - Error Message
C  ABFLAG - Return If 0, Abend if NON-0
C  DATA   - Additional Data  (Integer*4 Array)
C  NDAT   - # DATA Words TO PRINT 

      Implicit Integer (A-Z)
      Character*(*)  MSG
      Integer  DATA(1)
c
c   No action whatsoever if status is 0 or 1
c
      If (Status.Eq.0 .OR. Status.Eq.1) Return
c
c   Print message
c
c      Call Xvmessage(' ', ' ')
c      Call Xvmessage('Porting !! ', ' ')
      Call Xvmessage(MSG, ' ')
c
c   Process Additional DATA 
c
      N = NDAT
      If (N .GE. 1)  Call PRNT(4,N,DATA,' .')

      Call PRNT(4,1,STATUS,' Status =.')
c
c   Abend if instructed to do so
c
      If (ABFLAG .NE. 0) Call Abend

      Return
      End

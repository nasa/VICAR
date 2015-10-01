/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

#ifndef FORTRAN
static int dummy;		/* prevent ranlib from complaining */
#else
C ***** DO NOT USE TABS OF FORMFEEDS IN ANY UNIX FORTRAN FILES *****
C
C       XZINIT. FORTRAN CALLABLE ROUTINE .
C       WRITTEN IN FORTRAN TO AVOID PROBLEM OF CONVERTING A C STRING
C       TO A FORTRAN-77 STRING (FOR THE STANDARD OUTPUT FILE NAME ).
C
C
C       CHANGE LOG:
C       
C       29-MAR-83       HARDCODE PGMINC.FIN...DM
C       30-SEP-83       STORE TERMINAL TYPE, GETTERM -> XUGETT...peb
C       15-NOV-83       LIMIT VARIABLE NAMES TO 6 CHARS FOR PORTABILITY...dm
C       17-NOV-83       Add call to xzcall...palm
C       30-NOV-83       MODIFY FOR UNIX/F77 COMPATIBILITY
C       02-DEC-83       Fix bug in XRSTR calling sequence...dm
C       06-DEC-83       Save XZSAVL common block explicitely...dm
C       08-DEC-83       Do not open terminal as stdout using LUN 6...dm
C       10-MAY-84       Position file at the end in APPEND mode...dm
C       22-MAY-84       Convert file name to lower case...dm
C       21-AUG-84       Make xzinit portable, Isolate non-portable 
C                       routines to different source module...dm
C       01-APR-88       - APOLLO conversion requires all Fortran files to have
C                       ** NO TABS ** and ** NO FORMFEEDS **!!!
C                       - Fortran files now go through C pre-processor...ljn
C	23-JUL-91	PR325: Change STDLUN to LUN in PREOPN...ljn
C
C
        SUBROUTINE XZINIT(BLOCK, DIM, STDLUN, MODE, STATUS)

#include  "pgminc.fin"

        INTEGER BLOCK(1)                
        INTEGER DIM                     
        INTEGER STDLUN                  
        INTEGER MODE                    
        INTEGER STATUS                  


        CHARACTER*(xfssiz)      STDREC(2)       
        CHARACTER*(xfssiz)      TERMNM  
        INTEGER         LENGTH(2)
        INTEGER         COUNT
        LOGICAL         NEWFIL
        LOGICAL         OPENED
        INTEGER         L       
        INTEGER         LINES, COLS
        LOGICAL         TERMNL
        INTEGER         TTYPE                   

        STATUS = xsucc                                  
        CALL SETLUN(STDLUN)

        CALL XRINIM(BLOCK, DIM, MODE, STATUS)           
        IF (STATUS .NE. xsucc) RETURN
        CALL XZCALL(BLOCK)                              
        CALL XRSTR(BLOCK, '_STDOUT', 2, STDREC, LENGTH,
     +          COUNT, STATUS)
        IF (STATUS. NE. xsucc) RETURN

        NEWFIL = .TRUE.
        IF (STDREC(2) .NE. 'CREATE') NEWFIL = .FALSE.   

C
        CALL XUGETT(TERMNM)                             
        IF (TERMNM .EQ. STDREC(1)) THEN
            TERMNL = .TRUE.     
        ELSE
            TERMNL = .FALSE.  
        ENDIF
        CALL SETSTD(TERMNL)

        CALL XTINIT(TTYPE, LINES, COLS)                 
C
C       Prepare to open the standard output file, and make sure that
C       it is not already open.
C
        OPENED = .FALSE.
        CALL PREOPN(LUN, STDREC(1), OPENED)
        IF (OPENED) RETURN 
        
        CALL OPNSTD(STDREC(1), NEWFIL, OPENED)
        IF (.NOT. OPENED) THEN
c Don't fail simply because stdout couldn't be opened, so that we can
c run from something other than a terminal without triggering shell-vicar
ccccc       STATUS = xfail
            IF (MODE .EQ. xabort) THEN
              CALL XTWRIT('Could not open standard output file '
     1        / / STDREC(1)(1:L), xccstd)
              CALL XZEXIT(-1, 'TAE-STDOPN')           
            ENDIF
        ENDIF
        RETURN
        END

C
C  OPNSTD. Fortran open for an output file.
C

        SUBROUTINE OPNSTD(FILENM, NEWFIL, OPENED)
        
        CHARACTER*(*)   FILENM  
        LOGICAL         NEWFIL          
        LOGICAL         OPENED          


        CHARACTER*132   DUMMY
        CHARACTER*4     STAT 
        INTEGER         LUN
        LOGICAL         TERMNL                  

        CALL GETLUN_TAE(LUN)
        CALL GETSTD(TERMNL) 
        IF (NEWFIL) THEN
            STAT = 'NEW'
        ELSE
            STAT = 'OLD'
        ENDIF
        OPEN (FILE=FILENM, UNIT=LUN, STATUS=STAT, ERR=200)
        OPENED = .TRUE.
        IF (NEWFIL .OR. TERMNL) THEN
            RETURN
        ENDIF
C
C       IF OLD FILE, POSITION AT THE END.
C
999     CONTINUE
        READ(UNIT=LUN, FMT=101, END=150, ERR=150) DUMMY
101     FORMAT (A)
        GOTO 999
150     RETURN

200     CONTINUE
        OPENED = .FALSE.
        RETURN
        END
#endif

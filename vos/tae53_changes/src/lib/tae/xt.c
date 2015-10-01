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
C       This module provides bridges between the FORTRAN-77 user of the
C       XT routines (i.e., TAE terminal I/O) and the corresponding XU bridge
C       routines which, in turn, bridge to the C-implemented
C       routines in the TERMINAL.CNP module.  If the caller is not interactive
C       (e.g., batch) these routines do not call XU, but do FORTRAN I/O.
C       This FORTRAN I/O is to STDOUT for writes, and to LUN 5 for reads.
C       If the caller is non-interactive, XZINIT must have been called before
C       any of the XT routines.
C
C       When the caller is non-interactive:
C
C               - XT output routines write to the standard output
C                 LUN which is by default assigned to the batch
C                 log file (.LOG on VAX).
C
C               - XT input routines read from LUN 5.  (LUN 5 is assigned
C                 to INPUT.DAT on VAX.)
C
C               - cursor positioning commands are ignored on writes.
C
C               - carriage control on XTWRIT is single spacing except
C                 for "xccdbl" which is double spacing.
C
C               - the terminator is always returned as xcr or xover
C                 on reads.
C
C               - if an error or end-of-file is encountered on reads,
C                 the string is returned as "*** ERROR ***" or
C                 "*** EOF ***" respectively.
C
C       Any program that may be run in batch should follow the following
C       rules:
C
C               - Call XZINIT before making any XT calls.  This call
C                 supplies the LUN to be used for XT output routines
C                 when running in batch.  LUN 5 should not be passed
C                 to XZINIT because this would assign the input and
C                 output streams to the same file.
C
C               - FORTRAN I/O for standard output and standard input
C                 is discouraged, but WRITEs to the standard
C                 LUN (the LUN passed to XZINIT) and READs from LUN 5
C                 may be intermixed with XT calls.
C
C               - FORTRAN WRITEs to the standard LUN will not treat
C                 the 1st output character as carriage control.
C
C               - ACCEPT, TYPE, and PRINT I/O statements should not
C                 be used.  The ACCEPT does not use LUN 5 and will
C                 cause a program crash trying to open INPUT.DAT when
C                 it was already locked by any earlier XT input calls
C                 or READs from LUN 5.  The TYPE and PRINT statements use
C                 FOR$TYPE and FOR$PRINT for input.  These are not assigned
C                 to the standard output file and, in any case, are treated
C                 as separate input streams.
C
C       Note that there are no bridges for t_attn because these
C       facilities are not available to applications programs.
C
C
C       CHANGE LOG:
C
C       16-nov-83       Use TERMSTD for choice in output routines...peb
C       17-nov-83       Fix for 6-character variable names...palm
C       12-apr-84       Avoid external common block XZSAVL...dm
C       22-aug-84       Change subroutine name GSTLUN  to getlun_tae...dm
C       16-dec-86       Recode to avoid Q format in XTREAD...peb
C       01-apr-88       -Apollo conversion requires for all Fortran files
C                       ** NO TABS ** and ** NO FORMFEEDS ** !!!
C                       -Fortran files now go through C pre-processor...ljn
C
        SUBROUTINE XTINIT (TYPE, LINES, COLS)
        IMPLICIT INTEGER (A-Z)
C
C       XT INITIALIZATION.
C
        INTEGER         TYPE            
        INTEGER         LINES           
        INTEGER         COLS            

#include "pgminc.fin"

        INTEGER         LUN                     
        LOGICAL         TRMSTD                  
        INTEGER         TTYPE                   
        COMMON/XTSAV/LUN,TRMSTD,TTYPE           

        CALL XUINIT (TYPE, LINES, COLS) 
        TTYPE = TYPE                            
        CALL GETLUN_TAE(LUN)
        CALL GETSTD(TRMSTD)
        IF (LUN .LT. 0) THEN                    
            IF (TYPE .EQ. xnotrm) THEN          
                CALL XMPUT ('XTINIT called with STDOUT not defined.',
     1              'TAE-NOSTDOUT', STATUS)
                CALL XZEXIT (-1, ' ')
            ENDIF
        ENDIF
        RETURN
        END
        SUBROUTINE XTCLR
C
C       CLEARS THE SCREEN.
C       DOES NOTHING IF CALLER IS NOT INTERACTIVE.
C
#include "pgminc.fin"

        INTEGER         LUN                     
        LOGICAL         TRMSTD                  
        INTEGER         TTYPE                   
        COMMON/XTSAV/LUN,TRMSTD,TTYPE           

        IF (TRMSTD) THEN                        
            CALL XUCLR
        ENDIF
        RETURN
        END

        SUBROUTINE XTREAD (STRING, LENGTH, TERM)
        IMPLICIT INTEGER (A-Z)
C
C       READ A LINE.
C       TERM IS NEVER RETURNED AS xesc IF THE CALLER ISN'T INTERACTIVE.
C
        CHARACTER*(*)   STRING                  
        INTEGER         LENGTH                  
        INTEGER         TERM                    
                                                
                                                
                                                
#include "pgminc.fin"

        INTEGER         LUN                     
        LOGICAL         TRMSTD                  
        INTEGER         TTYPE                   
        COMMON/XTSAV/LUN,TRMSTD,TTYPE           

        CHARACTER*132   TMPSTR                  

        IF (TTYPE .EQ. xnotrm) THEN                     
            TERM = xcr
            READ (5,100, ERR=200, END=210) TMPSTR
100         FORMAT(A)

            NUMCHR = 0
            DO 110 I=LEN(TMPSTR), 1, -1
                IF (TMPSTR(I:I) .NE. ' ') THEN
                    NUMCHR = I
                    GOTO 120
                ENDIF
110         CONTINUE
120         CONTINUE

            IF (NUMCHR .GT. LEN(STRING)) THEN
                LENGTH = LEN(STRING)
                TERM = xover
            ELSE
                LENGTH = NUMCHR
                TERM = xcr
            ENDIF
            STRING = TMPSTR
        ELSE
            CALL XUREAD (STRING, LENGTH, TERM)
        ENDIF
        RETURN

200     CONTINUE                        
        STRING = '*** ERROR ***'
        RETURN

210     CONTINUE
        STRING = '*** EOF ***'
        END
        SUBROUTINE XTWRIT (STRING, CC)
        IMPLICIT INTEGER (A-Z)
C
C       WRITE A LINE TO TERMINAL.
C       IF CALLER IS NOT INTERACTIVE, LINE IS WRITTEN TO STDOUT.
C       CARRIAGE CONTROL IS HANDLED AS TO THE TERMINAL EXCEPT
C       xccnul HAS NO FORTRAN CORRESPONDENCE AND IS MAPPED TO xccpro.
C
        CHARACTER*(*)   STRING          
        INTEGER         CC              

#include "pgminc.fin"

        INTEGER         LUN                     
        LOGICAL         TRMSTD                  
        INTEGER         TTYPE                   
        COMMON/XTSAV/LUN,TRMSTD,TTYPE           

        IF (.NOT. TRMSTD) THEN                  

            IF (CC .EQ. xccdbl) THEN                    
                WRITE (LUN, 100, ERR=200) STRING
                WRITE (LUN, *)                          
            ELSE                                        
                WRITE (LUN, 100, ERR=200) STRING
            ENDIF
100         FORMAT(A)
        ELSE
            CALL XUWRIT (STRING, CC)
        ENDIF
        RETURN

200     RETURN
        END

        SUBROUTINE XTBELL
C
C       RING THE TERMINAL BELL.
C       IGNORED IF CALLER IS NOT INTERACTIVE.
C
#include "pgminc.fin"

        INTEGER         LUN                     
        LOGICAL         TRMSTD                  
        INTEGER         TTYPE                   
        COMMON/XTSAV/LUN,TRMSTD,TTYPE           

        IF (TRMSTD) THEN                        
            CALL XUBELL
        ENDIF
        RETURN
        END

        SUBROUTINE XTOUT (LINE, COLUMN, STRING)
C
C       WRITE TO A SPECIFIED POSITION ON THE SCREEN.
C       CURSOR CONTROL IS IGNORED IF THE CALLER ISN'T INTERACTIVE (I.E.,
C       A STANDARD CARRIAGE CONTROL WRITE IS DONE).
C
        INTEGER         LINE                    
        INTEGER         COLUMN                  
        CHARACTER*(*)   STRING                  

#include "pgminc.fin"

        INTEGER         LUN                     
        LOGICAL         TRMSTD                  
        INTEGER         TTYPE                   
        COMMON/XTSAV/LUN,TRMSTD,TTYPE           

        IF (.NOT. TRMSTD) THEN                  
            CALL XTWRIT (STRING, xccstd)
        ELSE
            CALL XUOUT (LINE, COLUMN, STRING)
        ENDIF
        RETURN
        END

        SUBROUTINE XTIN (LINE, COLUMN, STRING, LENGTH, TERM)
C
C       INPUT A STRING FROM A SPECIFIED POSITION ON THE SCREEN.
C       IF THE CALLER ISN'T INTERACTIVE, CURSOR CONTROL IS IGNORED
C       AND A STANDARD FORTRAN READ IS DONE.
C
        INTEGER         LINE                    
        INTEGER         COLUMN                  
        CHARACTER*(*)   STRING                  
        INTEGER         LENGTH                  
        INTEGER         TERM                    
                                                
                                                
                                                
#include "pgminc.fin"

        INTEGER         LUN                     
        LOGICAL         TRMSTD                  
        INTEGER         TTYPE                   
        COMMON/XTSAV/LUN,TRMSTD,TTYPE           

        IF (TTYPE .EQ. xnotrm) THEN             
            CALL XTREAD (STRING, LENGTH, TERM)
        ELSE
            CALL XUIN (LINE, COLUMN, STRING, LENGTH, TERM)
        ENDIF
        RETURN
        END

        SUBROUTINE XTPOS (LINE, COLUMN)
C
C       POSITION THE CUSOR ON THE SCREEN.
C       IGNORED IF THE CALLER ISN'T INTERACTIVE.
C
        INTEGER         LINE                    
        INTEGER         COLUMN                  

#include "pgminc.fin"

        INTEGER         LUN                     
        LOGICAL         TRMSTD                  
        INTEGER         TTYPE                   
        COMMON/XTSAV/LUN,TRMSTD,TTYPE           

        IF (TRMSTD) THEN                        
            CALL XUPOS (LINE, COLUMN)
        ENDIF
        RETURN
        END
#endif

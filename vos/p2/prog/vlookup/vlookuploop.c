#include "xvmaininc.h"
#include "ftnbridge.h"

#if 0
   Fortran code was commented out, left as documentation, and recoded in
   C for better performance.

c      SUBROUTINE LOOKUPLOOP (IBUF, OBUF, TABLE, NS)
C#######################################################################
C  NAME OF ROUTINE
C     LOOKUPLOOP    ( table LOOKUP LOOP)
C
C  PURPOSE
C      LOOKUPLOOP loops through the samples of a line in an input image
C      file, reassigning the data numbers according to the lookup table.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C
C  ENVIRONMENT
C      UNIX or VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     7-83  SP   CONVERTED FROM IBM VICAR VERSION: CHANGED TO FORTRAN
C                FROM IBM ASSEMBLER.
C    10-84  SP   CHANGED TO USE BYTE ARRAYS AND IZEXT.
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT PARAMETERS
C      IBUF ARRAY    - DATA NUMBERS FOR THE LINE OF THE INPUT IMAGE.
C                      ( IBUF(I) FOR I = 1 TO NS. )
C      TABLE ARRAY   - LOOKUP TABLE. ( TABLE(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).
C      NS            - NUMBER OF SAMPLES (DATA NUMBERS) IN THE LINE.
C  OUTPUT PARAMETERS
C      OBUF ARRAY    - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE.
C                      ( OBUF(I) FOR I = 1 TO NS. )
C  CALLED BY
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cc      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.
c
c      BYTE     IBUF(NS),   OBUF(NS),  TABLE(256)
c
cC=================START OF EXECUTABLE CODE===============================     
c
c      DO  I = 1, NS
c
c         OBUF(I) = TABLE(BYTE2INT( IBUF(I) ) + 1) ! CONVERT IBUF(I) TO INTEGER.
c 
c      END DO
c
c      RETURN
c      END
#endif

void FTN_NAME(lookuploop)(ibuf, obuf, table, ns)
unsigned char *ibuf;     /* input buffer   */
unsigned char *obuf;     /* output buffer   */
unsigned char table[256];/* lookup table   */
int *ns;                 /* number of samples*/
{
  unsigned char *end, *p;

   p   = obuf;
   end = (unsigned int)(*ns) + ibuf;       
   for (; ibuf < end; )
     *p++ = table[ *ibuf++ ];
}

#if 0
   Fortran code was commented out, left as documentation, and recoded in
   C for better performance.


      SUBROUTINE LOOKUPLOOP3 (IBUF, NS, OBUF, TABLE, OBUF2, TABLE2, 
     .                        OBUF3, TABLE3 )
C#######################################################################
C  NAME OF ROUTINE
C     LOOKUPLOOP3    ( table LOOKUP LOOP 3 outputs) 
C
C  PURPOSE
C      LOOKUPLOOP loops through the samples of a line in an input image
C      file, reassigning the data numbers according to the lookup table.
C      
C  PREPARED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     OCT 1984
C
C  ENVIRONMENT
C      UNIX or  VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT PARAMETERS
C      IBUF ARRAY    - DATA NUMBERS FOR THE LINE OF THE INPUT IMAGE.
C                      ( IBUF(I) FOR I = 1 TO NS. )
C      TABLE ARRAY   - LOOKUP TABLE. ( TABLE(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).  FOR OUTPUT 1.
C      TABLE2 ARRAY  - LOOKUP TABLE. ( TABLE2(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).  FOR OUTPUT 2.
C      TABLE3 ARRAY  - LOOKUP TABLE. ( TABLE3(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).  FOR OUTPUT 3.
C      NS            - NUMBER OF SAMPLES (DATA NUMBERS) IN THE LINE.
C  OUTPUT PARAMETERS
C      OBUF ARRAY    - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE 1.
C                      ( OBUF(I) FOR I = 1 TO NS. )
C      OBUF2 ARRAY   - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE 2.
C                      ( OBUF2(I) FOR I = 1 TO NS. )
C      OBUF3 ARRAY   - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE 3.
C                      ( OBUF3(I) FOR I = 1 TO NS. )
C  CALLED BY
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.
c
c      BYTE  IBUF(NS), OBUF(NS), TABLE(256), OBUF2(NS), TABLE2(256),
c     .           OBUF3(NS), TABLE3(256)
c
cC=================START OF EXECUTABLE CODE===============================     
c
c      DO  I = 1, NS
c
c         J = BYTE2INT( IBUF(I) ) + 1        ! CONVERT IBUF(I) TO INTEGER
c         OBUF(I) =  TABLE(J)
c         OBUF2(I) = TABLE2(J)
c         OBUF3(I) = TABLE3(J)
c 
c      END DO
c
c      RETURN
c      END
#endif

void FTN_NAME(lookuploop3)(ibuf, ns, obuf, table, obuf2, table2, obuf3, table3)
unsigned char *ibuf;     		/* input buffer   */
unsigned char *obuf, *obuf2, *obuf3;    /* output buffers   */
unsigned char table[256], table2[256], table3[256];  /* lookup tables   */
int *ns;                 		/* number of samples*/
{
  unsigned char *end, *p, *p2, *p3;

   p   = obuf;
   p2  = obuf2;
   p3  = obuf3;
   end = (unsigned int)(*ns) + ibuf;       
   for (; ibuf < end; ) {
     *p++ = table[ *ibuf ];
     *p2++ = table2[ *ibuf ];
     *p3++ = table3[ *ibuf++ ];
   }
}

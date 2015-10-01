$!****************************************************************************
$!
$! Build proc for MIPL module gridgen
$! VPACK Version 1.9, Monday, April 16, 2012, 11:57:24
$!
$! Execute by entering:		$ @gridgen
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module gridgen ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to gridgen.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("gridgen.imake") .nes. ""
$   then
$      vimake gridgen
$      purge gridgen.bld
$   else
$      if F$SEARCH("gridgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gridgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gridgen.bld "STD"
$   else
$      @gridgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gridgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gridgen.com -mixed -
	-s gridgen.f -
	-i gridgen.imake -
	-p gridgen.pdf -
	-t tstgridgen.pdf tstgridgen.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gridgen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'

C VICAR program GRIDGEN
C Generates a perfect grid: either a MARK format grid location file, or an
C image.
C
      SUBROUTINE MAIN44

!     Program GRIDGEN local variables 
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/OBUF(1200)

      INTEGER*4 GSL,GSS,DNG,DNB
      LOGICAL XVPTST,IMAGE

      INTEGER INC, CNT, NCOL, NROW, DUM, OUNIT, STAT

!     Initialize GRIDGEN local variables 
      DATA INC /0/, CNT /0/, GSL /0/, GSS /0/, NROW /0/, DNG /0/
     &     NCOL /0/, DNB /0/, DUM /0/, OUNIT /0/, STAT /0/
      DATA OBUF /1200*0/


!     Begin program GRIDGEN

!     14apr2012 -lwk- changed IFMESSAGE to XVMESSAGE to avoid garbage output
!     on Linux
      CALL XVMESSAGE ('GRIDGEN version 1 July 1994', ' ')

!     Set default error handling action
      CALL XVEACTION ('SA', ' ')
 
!     Determine if the keyword 'IMAGE' was set on input parameters
      IMAGE = XVPTST('IMAGE')

!     Obtain information about values for the indicated parameters
      CALL XVP('INC',INC,CNT)
      CALL XVP('GSL',GSL,CNT)
      CALL XVP('GSS',GSS,CNT)
      CALL XVP('NROW',NROW,CNT)
      CALL XVP('NCOL',NCOL,CNT)
      CALL XVP('DNGRID',DNG,CNT)
      CALL XVP('DNBACK',DNB,CNT)

!     If the keyword 'IMAGE' was set in the input parameters
      IF (IMAGE) GOTO 100
      NSO = NROW*NCOL*2
      IF (NSO.GT.1200) THEN
           CALL XVMESSAGE ('***Too many grid intersections', ' ')
           CALL XVMESSAGE ('***Reduce NROW or NCOL', ' ')
           GOTO 999
      ENDIF

      CALL GENGRID (OBUF,NROW,NCOL,GSL,GSS,INC)
      CALL PGRID   (OBUF,NROW,NCOL,DUM,0)
      CALL XVUNIT  (OUNIT,'OUT',1,STAT, ' ')
      CALL XVOPEN(OUNIT,STAT,'U_NL',1,'U_NS',NSO,
     *     'O_FORMAT','REAL','OP','WRITE', ' ')
      CALL XVWRIT(OUNIT,OBUF,STAT, ' ')
      GOTO 200

  100 NLO = GSL + NROW*INC - INC/2
      NSO = GSS + NCOL*INC - INC/2
      CALL XVUNIT (OUNIT,'OUT',1,STAT, ' ')
      CALL XVOPEN (OUNIT,STAT,'U_NL',NLO,'U_NS',NSO,
     *            'O_FORMAT','BYTE','OP','WRITE', ' ')

      CALL GENIMAGE (OUNIT,DNB,DNG,NLO,NSO,NROW,NCOL,GSL,GSS,INC)

  200 CALL XVMESSAGE ('GRIDGEN task completed', ' ')
      RETURN
  999 CALL XVMESSAGE ('***GRIDGEN task canceled', ' ')
      CALL ABEND
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                 Subroutine to generate grid locations...
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
C
      SUBROUTINE GENGRID (OBUF,NROW,NCOL,GSL,GSS,INC)

      IMPLICIT INTEGER(A-Z)

!     Subroutine GENGRID passed parameteres
      REAL*4 OBUF(1200)
      INTEGER*4 GSL,  GSS
      INTEGER   NROW, NCOL, INC

!     Subroutine GENGRID local parameteres
      INTEGER*4 GSS0
      INTEGER   I, J, K

!     Subroutine GENGRID local parameter initialization
      DATA      I /0/, J /0/, K /0/
      DATA      GSS0 /0/

!     Begin Subroutine GENGRID
      GSS0 = GSS
      K = 1

      DO J=1,NROW	!ROWS
          DO I=1,NCOL   !COLUMNS
              OBUF(K) = GSL
              OBUF(K+1) = GSS
              K = K + 2
              GSS = GSS + INC
          ENDDO
          GSS = GSS0
          GSL = GSL + INC
      ENDDO
      RETURN
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                 Subroutine GENIMAGE
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
C
      SUBROUTINE GENIMAGE(OUNIT,DNB,DNG,NLO,NSO,NROW,NCOL,GSL,GSS,INC)

      IMPLICIT INTEGER(A-Z)

!     Subroutine GENIMAGE passed parameteres
      INTEGER*4 GSL,GSS,DNG,DNB
      INTEGER   OUNIT, NLO, NSO, NROW, NCOL, INC

!     Subroutine GENIMAGE local parameteres
      COMMON/C2/PIC(1200),PIC2(1200)
      BYTE PIC,PIC2
      INTEGER   L, STAT

!     Initialize Subroutine GENIMAGE local parameters
      DATA      STAT /0/, L /0/
      DATA      PIC /1200*0/, PIC2 /1200*0/


!     Begin Subroutine GENIMAGE
      CALL ITLA(DNB,PIC,NSO)			! Start with background
      CALL MVE(-5,NCOL,DNG,PIC(GSS),0,INC)	! Add vertical grid rulings
      CALL ITLA(DNG,PIC2,NSO)			! Generate horizontal grid

      DO L=1,NLO
          IF (L.EQ.GSL) THEN
              CALL XVWRIT(OUNIT,PIC2,STAT, ' ')
              GSL = GSL + INC
          ELSE
              CALL XVWRIT(OUNIT,PIC,STAT, ' ')
          ENDIF
      ENDDO

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gridgen.imake
#define  PROGRAM   gridgen

#define MODULE_LIST gridgen.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create gridgen.pdf
process help = *
PARM OUT    TYPE=STRING  COUNT=1
PARM IMAGE  TYPE=KEYWORD COUNT=(0:1) VALID=IMAGE DEFAULT=--
PARM INC    TYPE=INTEGER COUNT=(0:1) VALID=(1:10000) DEFAULT=40
PARM GSL    TYPE=INTEGER COUNT=(0:1) VALID=(1:10000) DEFAULT=20
PARM GSS    TYPE=INTEGER COUNT=(0:1) VALID=(1:10000) DEFAULT=20
PARM NROW   TYPE=INTEGER COUNT=(0:1) VALID=(1:800) DEFAULT=20
PARM NCOL   TYPE=INTEGER COUNT=(0:1) VALID=(1:800) DEFAULT=20
PARM DNGRID TYPE=INTEGER COUNT=(0:1) VALID=(0:255) DEFAULT=5
PARM DNBACK TYPE=INTEGER COUNT=(0:1) VALID=(0:255) DEFAULT=240
END-PROC
.TITLE
	GRIDGEN
.HELP

PURPOSE:

   GRIDGEN is VICAR applications program which generates a synthetic
   grid target consisting of uniformly spaced horizontal and vertical
   grid rulings.  The program is used for geometric calibration of
   vidicon/CCD camera systems.

EXECUTION:

        GRIDGEN OUT=GRID PARAMS

Parameters are defined in the TUTOR mode. SIZE is ingored.
.PAGE
OPERATION:

   GRIDGEN has two operating modes: (1) in the default mode, the output
   consists of a grid location file in MARK format; (2) if the keyword
   IMAGE is specified, the output consists of a gridded image.

   The grid is specified by giving the line-sample coordinates of
   the upper-left-most grid intersection (see parameters GSL and GSS), the
   grid spacing (see parameter INC), and the number of horizontal and
   vertical grid rulings (see parameters NROW and NCOL).

   The grid location file contains the line-sample coordinates of each
   grid intersection is stored in a single record as ordered pairs of floating
   point numbers: L1,S1,L2,S2,L3,S3,...,Ln,Sn where n=NROW*NCOL.
   The rows are ordered from top-to-bottom, and the columns from left-to-
   right.  The intersections of the top-most row are stored first, followed
   by the second row, etc.

   If IMAGE is specified, the user may specify the DN value of the grid
   rulings and the background (see parameters DNGRID and DNBACK).

EXAMPLES:
   The following statement will generate a mark format file containing the
   intersections of a 20x20 grid of rulings spaced 40 pixels apart (all
   values represent the defaults):

         GRIDGEN OUT INC=40 GSL=20 GSS=20 NROW=20 NCOL=20

   The following statement will generate a grid image with grid rulings of
   5 DN overlayed on a background of 240 DN:

         GRIDGEN OUT INC=40 GSL=20 GSS=20 NROW=20 NCOL=20 DNGRID=5
        +       DNBACK=240 'IMAGE

RESTRICTIONS:

   NROW*NCOL must be less than 1200.

PROGRAM HISTORY:

   01 July 1994...CRI ...........Made portable for UNIX
   21 Sep  1987...G.M.Yagi.......Extensive changes
   27 MAR  1985...M.E.MORRILL....ADD OUTPUT OF GRID IMAGE
   30 OCT  1984...M.E.MORRILL....CONVERSION TO VAX-VICAR*2
   06 MAY  1983...M.E.MORRILL....INITIAL RELEASE IBM 

   Written by: Mike Morrill
   Current cognizant programmer:  Gary Yagi

.LEVEL1
.VARIABLE OUT
 STRING-REQUIRED
 Either a grid
 location file
 (default) or a
 grid image.
.VARIABLE IMAGE
 KEYWORD-OPTIONAL
 Specifies IMAGE output.
.VARIABLE INC
 INTEGER-OPTIONAL
 Spacing between 
 intersections.
.VARIABLE GSL
 INTEGER-OPTIONAL
 Grid starting line.
.VARIABLE GSS
 INTEGER-OPTIONAL
 Grid starting sample.
.VARIABLE NROW
 INTEGER-OPTIONAL
 Number of horizontal
 grid rulings.
.VARIABLE NCOL
 INTEGER-OPTIONAL
 Number of vertical
 grid rulings.
.VARIABLE DNGRID
 INTEGER-OPTIONAL
 DN value of the grid
 rulings for IMAGE mode.
.VARIABLE DNBACK
 INTEGER-OPTIONAL
 DN value of the
 background for
 IMAGE mode.
.LEVEL2
.VARIABLE OUT
 STRING-REQUIRED
 Either a grid location file in MARK format (default) or a gridded image
 (see IMAGE keyword).
.VARIABLE IMAGE
 KEYWORD-OPTIONAL
 Specifies gridded image output.
.VARIABLE INC
 INTEGER-OPTIONAL
 Pixel spacing between intersections.
.VARIABLE GSL
 INTEGER-OPTIONAL
 GSL and GSS specify the line-sample coordinates of the upper-left-most
 grid intersection.
.VARIABLE GSS
 INTEGER-OPTIONAL
 Grid starting sample.
.VARIABLE NROW
 INTEGER-OPTIONAL
 Number of horizontal grid rulings.
.VARIABLE NCOL
 INTEGER-OPTIONAL
 Number of vertical grid rulings.
.VARIABLE DNGRID
 INTEGER-OPTIONAL
 Specifies the DN value of the grid rulings for IMAGE mode.
.VARIABLE DNBACK
 INTEGER-OPTIONAL
 Specifies the DN value of the background for IMAGE mode.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgridgen.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
gridgen OUT
gridgen OUT 'IMAGE
list OUT (15,15,10,10)
list OUT (775,775,10,10)
end-proc
$!-----------------------------------------------------------------------------
$ create tstgridgen.log_solos
tstgridgen
gridgen OUT
Beginning VICAR task gridgen
GRIDGEN version 1 July 1994

       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20
     20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0  20.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

      21    22    23    24    25    26    27    28    29    30    31    32    33    34    35    36    37    38    39    40
     60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0  60.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

      41    42    43    44    45    46    47    48    49    50    51    52    53    54    55    56    57    58    59    60
    100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

      61    62    63    64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80
    140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0 140.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

      81    82    83    84    85    86    87    88    89    90    91    92    93    94    95    96    97    98    99   100
    180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0 180.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     101   102   103   104   105   106   107   108   109   110   111   112   113   114   115   116   117   118   119   120
    220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0 220.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     121   122   123   124   125   126   127   128   129   130   131   132   133   134   135   136   137   138   139   140
    260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0 260.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     141   142   143   144   145   146   147   148   149   150   151   152   153   154   155   156   157   158   159   160
    300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0 300.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177   178   179   180
    340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0 340.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     181   182   183   184   185   186   187   188   189   190   191   192   193   194   195   196   197   198   199   200
    380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0 380.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     201   202   203   204   205   206   207   208   209   210   211   212   213   214   215   216   217   218   219   220
    420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0 420.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     221   222   223   224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240
    460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0 460.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     241   242   243   244   245   246   247   248   249   250   251   252   253   254   255   256   257   258   259   260
    500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0 500.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     261   262   263   264   265   266   267   268   269   270   271   272   273   274   275   276   277   278   279   280
    540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0 540.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     281   282   283   284   285   286   287   288   289   290   291   292   293   294   295   296   297   298   299   300
    580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0 580.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     301   302   303   304   305   306   307   308   309   310   311   312   313   314   315   316   317   318   319   320
    620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0 620.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     321   322   323   324   325   326   327   328   329   330   331   332   333   334   335   336   337   338   339   340
    660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0 660.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     341   342   343   344   345   346   347   348   349   350   351   352   353   354   355   356   357   358   359   360
    700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0 700.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     361   362   363   364   365   366   367   368   369   370   371   372   373   374   375   376   377   378   379   380
    740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0 740.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0

     381   382   383   384   385   386   387   388   389   390   391   392   393   394   395   396   397   398   399   400
    780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0 780.0
     20.0  60.0 100.0 140.0 180.0 220.0 260.0 300.0 340.0 380.0 420.0 460.0 500.0 540.0 580.0 620.0 660.0 700.0 740.0 780.0
GRIDGEN task completed
gridgen OUT 'IMAGE
Beginning VICAR task gridgen
GRIDGEN version 1 July 1994
GRIDGEN task completed
list OUT (15,15,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GRIDGEN   User:lwk       Date_Time:Sat Apr 14 18:14:16 2012
     Samp    15      17      19      21      23
   Line
     15     240 240 240 240 240   5 240 240 240 240
     16     240 240 240 240 240   5 240 240 240 240
     17     240 240 240 240 240   5 240 240 240 240
     18     240 240 240 240 240   5 240 240 240 240
     19     240 240 240 240 240   5 240 240 240 240
     20       5   5   5   5   5   5   5   5   5   5
     21     240 240 240 240 240   5 240 240 240 240
     22     240 240 240 240 240   5 240 240 240 240
     23     240 240 240 240 240   5 240 240 240 240
     24     240 240 240 240 240   5 240 240 240 240
list OUT (775,775,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GRIDGEN   User:lwk       Date_Time:Sat Apr 14 18:14:16 2012
     Samp   775     777     779     781     783
   Line
    775     240 240 240 240 240   5 240 240 240 240
    776     240 240 240 240 240   5 240 240 240 240
    777     240 240 240 240 240   5 240 240 240 240
    778     240 240 240 240 240   5 240 240 240 240
    779     240 240 240 240 240   5 240 240 240 240
    780       5   5   5   5   5   5   5   5   5   5
    781     240 240 240 240 240   5 240 240 240 240
    782     240 240 240 240 240   5 240 240 240 240
    783     240 240 240 240 240   5 240 240 240 240
    784     240 240 240 240 240   5 240 240 240 240
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################

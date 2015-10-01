$!****************************************************************************
$!
$! Build proc for MIPL module pbname
$! VPACK Version 1.9, Wednesday, September 22, 2010, 19:23:15
$!
$! Execute by entering:		$ @pbname
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module pbname ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to pbname.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("pbname.imake") .nes. ""
$   then
$      vimake pbname
$      purge pbname.bld
$   else
$      if F$SEARCH("pbname.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pbname
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pbname.bld "STD"
$   else
$      @pbname.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pbname.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pbname.com -mixed -
	-s pbname.f zpbname.c -
	-i pbname.imake -
	-t tpbname.f tzpbname.c tpbname.imake tpbname.pdf tstpbname.pdf -
	   tstpbname.log -
	-o pbname.hlp pbname_old.f zpbname_old.c
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pbname.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      subroutine PBNAME(ID,PAR,*)
c
c  This is simply an interface to the SPICE routines BODN2C and BODC2N,
c  provided for compatibility with the VICAR calling sequence.
c  (The old code for PBNAME is included in the pbname.com as pbname_old.f.)
c
c  11aug10 -lwk- delivered SPICE version
c  22sep10 -lwk- XPBID was corrupting string being passed to it!  Fixed to
c               first copy this to local buffer.
c
C  ROUTINE TO RETURN PLANET NAME GIVEN ID
c
      character*12 par
      logical found

      call bodc2n( id, par, found)
      if (.not.found) return 1
      return
C
C ROUTINE TO RETURN PLANET ID GIVEN PLANET NAME (PAR)
c
      entry PBID(PAR,ID,*)
C
      call bodn2c( par, id, found)
      if (.not.found) return 1
      return

      end

c  FORTRAN bridges for above routines:

      subroutine xpbname(id,bpar,ind)
      byte bpar(13)
      character*12 par
      ind = 1
      call pbname(id,par,*100)
      go to 200
100   par = ' '
      ind = 0
200   call mvcl(par,bpar,13)
      bpar(13) = 0
      return
      end

      subroutine xpbid(bpar,id,ind)
      byte bpar(13),bpar1(13)
      character*12 par
      logical cend
      cend = .false.
      ind = 1
c  trim trailing zero and any extra stuff:
      do i = 1,13
        bpar1(i) = bpar(i)
        if (bpar1(i).eq.0) cend = .true.
        if (cend) bpar1(i) = 32		! space
      enddo
      call mvlc(bpar1,par,12)
      call pbid(par,id,*100)
      go to 200
100   ind = 0
200   return
      end

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zpbname.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* C-bridge for FORTRAN routines PBNAME, PBID */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spice89.h"
#include <string.h>

int zpbname(id, par)
int id;
char par[13];
{
  int ind;
  FTN_NAME2(xpbname,XPBNAME)(&id,par,&ind);
  return(ind);
}

int zpbid(par, id)
char par[13];
int *id;
{
  int ind;
  FTN_NAME2(xpbid,XPBID)(par,id,&ind);
  return(ind);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pbname.imake
/* Imake file for VICAR subroutine PBNAME   */

#define SUBROUTINE  pbname

#define MODULE_LIST  pbname.f  zpbname.c

#define FTN_STRING
#define USES_FORTRAN
#define USES_ANSI_C
#define P2_SUBLIB

/*#define DEBUG		/* remove on delivery */
/*#define LIB_LOCAL	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tpbname.f
c-----Program tpbname
c-----Test program for subroutine PBNAME and PBID
c
      include 'VICMAIN_FOR'
      subroutine main44
      character*12 name,name1
      character*35 pbuf1
      character*30 pbuf
      integer*2 ibuf(15)
c
      call MVCL(' id = xxx  name = xxxxxxxx',ibuf,26)
      call MVLC(ibuf, PBUF, 26)
c
      do 10 i=1,999
        call Pbname(i, name, *10)
        call Pbid(name, j, *10)
        write(Pbuf(7:9),'(I3)') j
        do 20  k=1,12
          k1 = k + 18
          PBUF(k1:k1) = Name(k:k)
20      continue          
        call Xvmessage(Pbuf, ' ')
10    continue

c   test for IDA:
      name = 'IDA'
      call pbid(name,j,*30)
      call pbname(j,name1,*30)
      call MVCL(' id = xxxxxxxx  name = xxxxxxxx',ibuf,26)
      call MVLC(ibuf, PBUF1, 26)
      write(pbuf1(7:14),'(i8)') j
      pbuf1(24:35) = name1
      call xvmessage( pbuf1, ' ')
      go to 40
30    call xvmessage(' IDA not found', ' ')
c
c   Testing the C-Bridge  
c
40    call tzpbname
c
      return
      end
$!-----------------------------------------------------------------------------
$ create tzpbname.c
/*   A C-bridge routine, called by TPBNAME.F, that tests the C-bridge version
   of PBNAME, ZPbname.c    
*/

#include "xvmaininc.h"
#include "ftnbridge.h"
void FTN_NAME(tzpbname) ()
{
  int i, jj, k1, k2;  
  char pbuf[32];
  char name[13];

  zvmessage(" ", " ");
  zvmessage(" ******  Testing C-Bridge Version  ****** ", " ");
  zvmessage(" ", " ");

  for (i=0; i < 999; i++) {
    k1 = zpbname(i, name) ;
    k2 = zpbid(name, &jj) ;
    if (k1 == 1  &&  k2 == 1) {
     snprintf( pbuf, 32, " id = %03d  name = %12s", jj, name);
     zvmessage(pbuf, " ");
    }
  }

}
$!-----------------------------------------------------------------------------
$ create tpbname.imake
/* Imake file for Fortran-Test of VICAR subroutine  PBNAME  */

#define PROGRAM tpbname

#define MODULE_LIST tpbname.f  tzpbname.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE       
#define LIB_SPICE
#define LIB_P2SUB    

/*#define DEBUG		/*  disable on delivery   */
/*#define LIB_LOCAL	/*  disable on delivery   */
$!-----------------------------------------------------------------------------
$ create tpbname.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstpbname.pdf
procedure
refgbl $echo
body
let _onfail="continue"
!let $echo="yes"
WRITE "THIS IS A TEST OF MODULE PBNAME"

TPBNAME

end-proc
$!-----------------------------------------------------------------------------
$ create tstpbname.log
tstpbname
THIS IS A TEST OF MODULE PBNAME
Beginning VICAR task TPBNAME
 id =  10  name = SUN
 id = 199  name = MERCURY
 id = 299  name = VENUS
 id = 301  name = MOON
 id = 399  name = EARTH
 id = 401  name = PHOBOS
 id = 402  name = DEIMOS
 id = 499  name = MARS
 id = 501  name = IO
 id = 502  name = EUROPA
 id = 503  name = GANYMEDE
 id = 504  name = CALLISTO
 id = 505  name = AMALTHEA
 id = 506  name = HIMALIA
 id = 507  name = ELARA
 id = 508  name = PASIPHAE
 id = 509  name = SINOPE
 id = 510  name = LYSITHEA
 id = 511  name = CARME
 id = 512  name = ANANKE
 id = 513  name = LEDA
 id = 514  name = THEBE
 id = 515  name = ADRASTEA
 id = 516  name = METIS
 id = 517  name = CALLIRRHOE
 id = 518  name = THEMISTO
 id = 519  name = MAGACLITE
 id = 520  name = TAYGETE
 id = 521  name = CHALDENE
 id = 522  name = HARPALYKE
 id = 523  name = KALYKE
 id = 524  name = IOCASTE
 id = 525  name = ERINOME
 id = 526  name = ISONOE
 id = 527  name = PRAXIDIKE
 id = 528  name = AUTONOE
 id = 529  name = THYONE
 id = 530  name = HERMIPPE
 id = 531  name = AITNE
 id = 532  name = EURYDOME
 id = 533  name = EUANTHE
 id = 534  name = EUPORIE
 id = 535  name = ORTHOSIE
 id = 536  name = SPONDE
 id = 537  name = KALE
 id = 538  name = PASITHEE
 id = 539  name = HEGEMONE
 id = 540  name = MNEME
 id = 541  name = AOEDE
 id = 542  name = THELXINOE
 id = 543  name = ARCHE
 id = 544  name = KALLICHORE
 id = 545  name = HELIKE
 id = 546  name = CARPO
 id = 547  name = EUKELADE
 id = 548  name = CYLLENE
 id = 549  name = KORE
 id = 599  name = JUPITER
 id = 601  name = MIMAS
 id = 602  name = ENCELADUS
 id = 603  name = TETHYS
 id = 604  name = DIONE
 id = 605  name = RHEA
 id = 606  name = TITAN
 id = 607  name = HYPERION
 id = 608  name = IAPETUS
 id = 609  name = PHOEBE
 id = 610  name = JANUS
 id = 611  name = EPIMETHEUS
 id = 612  name = HELENE
 id = 613  name = TELESTO
 id = 614  name = CALYPSO
 id = 615  name = ATLAS
 id = 616  name = PROMETHEUS
 id = 617  name = PANDORA
 id = 618  name = PAN
 id = 619  name = YMIR
 id = 620  name = PAALIAQ
 id = 621  name = TARVOS
 id = 622  name = IJIRAQ
 id = 623  name = SUTTUNGR
 id = 624  name = KIVIUQ
 id = 625  name = MUNDILFARI
 id = 626  name = ALBIORIX
 id = 627  name = SKATHI
 id = 628  name = ERRIAPUS
 id = 629  name = SIARNAQ
 id = 630  name = THRYMR
 id = 631  name = NARVI
 id = 632  name = METHONE
 id = 633  name = PALLENE
 id = 634  name = POLYDEUCES
 id = 635  name = DAPHNIS
 id = 636  name = AEGIR
 id = 637  name = BEBHIONN
 id = 638  name = BERGELMIR
 id = 639  name = BESTLA
 id = 640  name = FARBAUTI
 id = 641  name = FENRIR
 id = 642  name = FORNJOT
 id = 643  name = HATI
 id = 644  name = HYROKKIN
 id = 645  name = KARI
 id = 646  name = LOGE
 id = 647  name = SKOLL
 id = 648  name = SURTUR
 id = 649  name = ANTHE
 id = 650  name = JARNSAXA
 id = 651  name = GREIP
 id = 652  name = TARQEQ
 id = 699  name = SATURN
 id = 701  name = ARIEL
 id = 702  name = UMBRIEL
 id = 703  name = TITANIA
 id = 704  name = OBERON
 id = 705  name = MIRANDA
 id = 706  name = CORDELIA
 id = 707  name = OPHELIA
 id = 708  name = BIANCA
 id = 709  name = CRESSIDA
 id = 710  name = DESDEMONA
 id = 711  name = JULIET
 id = 712  name = PORTIA
 id = 713  name = ROSALIND
 id = 714  name = BELINDA
 id = 715  name = PUCK
 id = 716  name = CALIBAN
 id = 717  name = SYCORAX
 id = 718  name = PROSPERO
 id = 719  name = SETEBOS
 id = 720  name = STEPHANO
 id = 721  name = TRINCULO
 id = 722  name = FRANCISCO
 id = 723  name = MARGARET
 id = 724  name = FERDINAND
 id = 725  name = PERDITA
 id = 726  name = MAB
 id = 727  name = CUPID
 id = 799  name = URANUS
 id = 801  name = TRITON
 id = 802  name = NEREID
 id = 803  name = NAIAD
 id = 804  name = THALASSA
 id = 805  name = DESPINA
 id = 806  name = GALATEA
 id = 807  name = LARISSA
 id = 808  name = PROTEUS
 id = 809  name = HALIMEDE
 id = 810  name = PSAMATHE
 id = 811  name = SAO
 id = 812  name = LAOMEDEIA
 id = 813  name = NESO
 id = 899  name = NEPTUNE
 id = 901  name = CHARON
 id = 902  name = NIX
 id = 903  name = HYDRA
 id = 999  name = PLUTO
 id =  2431010  name = IDA
 
 ******  Testing C-Bridge Version  ****** 
 
 id = 010  name = SUN          
 id = 199  name = MERCURY      
 id = 299  name = VENUS        
 id = 301  name = MOON         
 id = 399  name = EARTH        
 id = 401  name = PHOBOS       
 id = 402  name = DEIMOS       
 id = 499  name = MARS         
 id = 501  name = IO           
 id = 502  name = EUROPA       
 id = 503  name = GANYMEDE     
 id = 504  name = CALLISTO     
 id = 505  name = AMALTHEA     
 id = 506  name = HIMALIA      
 id = 507  name = ELARA        
 id = 508  name = PASIPHAE     
 id = 509  name = SINOPE       
 id = 510  name = LYSITHEA     
 id = 511  name = CARME        
 id = 512  name = ANANKE       
 id = 513  name = LEDA         
 id = 514  name = THEBE        
 id = 515  name = ADRASTEA     
 id = 516  name = METIS        
 id = 517  name = CALLIRRHOE   
 id = 518  name = THEMISTO     
 id = 519  name = MAGACLITE    
 id = 520  name = TAYGETE      
 id = 521  name = CHALDENE     
 id = 522  name = HARPALYKE    
 id = 523  name = KALYKE       
 id = 524  name = IOCASTE      
 id = 525  name = ERINOME      
 id = 526  name = ISONOE       
 id = 527  name = PRAXIDIKE    
 id = 528  name = AUTONOE      
 id = 529  name = THYONE       
 id = 530  name = HERMIPPE     
 id = 531  name = AITNE        
 id = 532  name = EURYDOME     
 id = 533  name = EUANTHE      
 id = 534  name = EUPORIE      
 id = 535  name = ORTHOSIE     
 id = 536  name = SPONDE       
 id = 537  name = KALE         
 id = 538  name = PASITHEE     
 id = 539  name = HEGEMONE     
 id = 540  name = MNEME        
 id = 541  name = AOEDE        
 id = 542  name = THELXINOE    
 id = 543  name = ARCHE        
 id = 544  name = KALLICHORE   
 id = 545  name = HELIKE       
 id = 546  name = CARPO        
 id = 547  name = EUKELADE     
 id = 548  name = CYLLENE      
 id = 549  name = KORE         
 id = 599  name = JUPITER      
 id = 601  name = MIMAS        
 id = 602  name = ENCELADUS    
 id = 603  name = TETHYS       
 id = 604  name = DIONE        
 id = 605  name = RHEA         
 id = 606  name = TITAN        
 id = 607  name = HYPERION     
 id = 608  name = IAPETUS      
 id = 609  name = PHOEBE       
 id = 610  name = JANUS        
 id = 611  name = EPIMETHEUS   
 id = 612  name = HELENE       
 id = 613  name = TELESTO      
 id = 614  name = CALYPSO      
 id = 615  name = ATLAS        
 id = 616  name = PROMETHEUS   
 id = 617  name = PANDORA      
 id = 618  name = PAN          
 id = 619  name = YMIR         
 id = 620  name = PAALIAQ      
 id = 621  name = TARVOS       
 id = 622  name = IJIRAQ       
 id = 623  name = SUTTUNGR     
 id = 624  name = KIVIUQ       
 id = 625  name = MUNDILFARI   
 id = 626  name = ALBIORIX     
 id = 627  name = SKATHI       
 id = 628  name = ERRIAPUS     
 id = 629  name = SIARNAQ      
 id = 630  name = THRYMR       
 id = 631  name = NARVI        
 id = 632  name = METHONE      
 id = 633  name = PALLENE      
 id = 634  name = POLYDEUCES   
 id = 635  name = DAPHNIS      
 id = 636  name = AEGIR        
 id = 637  name = BEBHIONN     
 id = 638  name = BERGELMIR    
 id = 639  name = BESTLA       
 id = 640  name = FARBAUTI     
 id = 641  name = FENRIR       
 id = 642  name = FORNJOT      
 id = 643  name = HATI         
 id = 644  name = HYROKKIN     
 id = 645  name = KARI         
 id = 646  name = LOGE         
 id = 647  name = SKOLL        
 id = 648  name = SURTUR       
 id = 649  name = ANTHE        
 id = 650  name = JARNSAXA     
 id = 651  name = GREIP        
 id = 652  name = TARQEQ       
 id = 699  name = SATURN       
 id = 701  name = ARIEL        
 id = 702  name = UMBRIEL      
 id = 703  name = TITANIA      
 id = 704  name = OBERON       
 id = 705  name = MIRANDA      
 id = 706  name = CORDELIA     
 id = 707  name = OPHELIA      
 id = 708  name = BIANCA       
 id = 709  name = CRESSIDA     
 id = 710  name = DESDEMONA    
 id = 711  name = JULIET       
 id = 712  name = PORTIA       
 id = 713  name = ROSALIND     
 id = 714  name = BELINDA      
 id = 715  name = PUCK         
 id = 716  name = CALIBAN      
 id = 717  name = SYCORAX      
 id = 718  name = PROSPERO     
 id = 719  name = SETEBOS      
 id = 720  name = STEPHANO     
 id = 721  name = TRINCULO     
 id = 722  name = FRANCISCO    
 id = 723  name = MARGARET     
 id = 724  name = FERDINAND    
 id = 725  name = PERDITA      
 id = 726  name = MAB          
 id = 727  name = CUPID        
 id = 799  name = URANUS       
 id = 801  name = TRITON       
 id = 802  name = NEREID       
 id = 803  name = NAIAD        
 id = 804  name = THALASSA     
 id = 805  name = DESPINA      
 id = 806  name = GALATEA      
 id = 807  name = LARISSA      
 id = 808  name = PROTEUS      
 id = 809  name = HALIMEDE     
 id = 810  name = PSAMATHE     
 id = 811  name = SAO          
 id = 812  name = LAOMEDEIA    
 id = 813  name = NESO         
 id = 899  name = NEPTUNE      
 id = 901  name = CHARON       
 id = 902  name = NIX          
 id = 903  name = HYDRA        
exit
slogoff
$ Return
$!#############################################################################
$Other_File:
$ create pbname.hlp
1 PBNAME

2  PURPOSE

  PBNAME and PBID were originally written as Voyager-specific subroutines.
  They now simply call the corresponding SPICE routines and are supported
  only for backwards compatibility in existing VICAR programs.

  Given the target-body name, PBID will return the corresponding target-body
  ID as specified in the NAIF SPICE software. Given the target-body ID,
  PBNAME will return corresponding target-body name.

  CALLING SEQUENCE:

       CALL PBNAME(ID,name,&nnn)     or
       CALL PBID(NAME,id,&nnn)

  where 
       INTEGER*4 ID	   is the target-body ID
       CHARACTER*12 NAME   is the target-body name
       &nnn		   is the statement label of an alternate return 

  ID is input to PBNAME and output by PBID.
  NAME is input to PBID and output by PBNAME.
  NAME must be left justified and padded on the right with blanks.
  The alternate return is taken if an unidentified ID or NAME is detected.

  "C" CALLING SEQUENCE:
       int i,id;
       char par[13];
       i = zpbname(id,name)     or
       i = zpbid(name,id)
  where i=1 for success and i=0 for failure.

2 OPERATION

  Tables of target names and target numbers are maintained internal to the
  subroutine (i.e. no SPICE files are accessed).

2 HISTORY

  Original Programmer: Gary Yagi, 23 July 1980
  Current Cognizant Programmer: Lucas Kamp
  Revisions:
    24 Jan 86  GMY  Fix order of Uranian satellites
     9 Sep 88  GMY  Added new satellites and rename some Jupiter satellites
    25 oct 89  JJL  change to GLL sedr numbering & change to character*12
    12 Nov 91  GMY  Restore previous numbering to be NAIF compatible
                    Change MERCURY, VENUS, MARS to 199, 299, 499.
                    Add GASPRA to target list.  Add PCA garbage.
    28 Jan 93  WPL  Ported for UNIX Conversion
    21 Mar 94  FFM  Added ccase in PBID(FR 85159),
                    changed TETTHYS to TETHYS in ZPBNAME.C. 
    22 Aug 94  GMY  Added IDA (FR 85139)
    21 Aug 10  LWK  Rewritten to use SPICE calls.  Removed all references 
                    to PCA.
$!-----------------------------------------------------------------------------
$ create pbname_old.f
c
      Subroutine PBNAME(ID,PAR,*)
c
C  ROUTINE TO RETURN PLANET NAME GIVEN ID
c
C ID NUMBERING SCHEME IS CONSISTENT WITH GLL SPICE
C  LOOKUP TABLE FOR PLANET I.D.  FROM SEDR WORD # 9
c
      INTEGER  N/65/
       Character*12  Par
       Character*12  p8
       Character*12  pname(65) 
      INTEGER  LOOK(65)
      Data  LOOK /
c mercury
     + 199,
c venus
     + 299,
c earth
     + 399,301,
c mars
     + 499,401,402,
c jupiter
     + 599,501,502,503,504,505,506,507,508,509,510,511,512,513,
     + 514,515,516,
c saturn
     + 699,601,602,603,604,605,606,607,608,609,610,611,612,613,
     + 614,615,616,617,
c uranus
     + 799,701,702,703,704,705,706,707,708,709,710,711,712,713,
     + 714,715,
c neptune
     + 899,801,802,
c pluto
     + 999,901,
c gaspra
     + 9511010,
c ida
     + 2431010 /
c
       Data Pname /   'MERCURY     ','VENUS       ','EARTH       ',
     + 'MOON        ','MARS        ','PHOBOS      ','DEIMOS      ',
     + 'JUPITER     ','IO          ','EUROPA      ','GANYMEDE    ',
     + 'CALLISTO    ','AMALTHEA    ','HIMALIA     ','ELARA       ',
     + 'PASIPHAE    ','SINOPE      ','LYSITHEA    ','CARME       ',
     + 'ANANKE      ','LEDA        ','THEBE       ','ADRASTEA    ',
     + 'METIS       ','SATURN      ','MIMAS       ','ENCELADUS   ',
     + 'TETHYS      ','DIONE       ','RHEA        ','TITAN       ',
     + 'HYPERION    ','IAPETUS     ','PHOEBE      ','JANUS       ',
     + 'EPIMETHEUS  ','HELENE      ','TELESTO     ','CALYPSO     ',
     + 'ATLAS       ','PROMETHEUS  ','PANDORA     ','URANUS      ',
     + 'ARIEL       ','UMBRIEL     ','TITANIA     ','OBERON      ',
     + 'MIRANDA     ','CORDELIA    ','OPHELIA     ','BIANCA      ',
     + 'CRESSIDA    ','DESDEMONA   ','JULIET      ','PORTIA      ',
     + 'ROSALIND    ','BELINDA     ','PUCK        ','NEPTUNE     ',
     + 'TRITON      ','NEREID      ','PLUTO       ','CHARON      ',
     + 'GASPRA      ','IDA         ' /
c
C
      Do 5  I=1,N
       If (ID .EQ. LOOK(I)) GoTo  6
5     Continue
      RETURN1
6     Par = pname(i)      
      Return
C
C ROUTINE TO RETURN PLANET ID GIVEN PLANET NAME (PAR)
c
      ENTRY PBID(PAR,ID,*)
C
      p8=par
C
      call ccase(p8,1,-1)      ! ensure case is UPPER
      DO 10 I=1,N
      If (PNAME(I) .EQ. P8) GoTo 11
10     CONTINUE
      RETURN1
C
11    ID = LOOK(I)
c
      Return
      End
$!-----------------------------------------------------------------------------
$ create zpbname_old.c
#include "xvmaininc.h"
#include <string.h>
#define  N  65

zpbname(id, par)
int        id   ;
char       par[12]  ;
{
 /*  Routine to return PLANET name given ID#   */
 /*   char   p8[12] ;      */
 char   pname[65][12] ;
 int    look[65] ;
 int    j, stat ;

 /*   ID numbering scheme is consistent with GLL SPICE
      LOOKUP table for PLANET I.D.  FROM SEDR WORD # 9   */

   /*   MERCURY  */
  look[0] = 199 ;  

   /*  VENUS     */   
  look[1] = 299 ;

   /*  EARTH     */
  look[2] = 399 ;  look[3] = 301 ;

   /*  MARS      */
  look[4] = 499 ;  look[5] = 401 ;  look[6] = 402 ;

   /*  JUPITER   */
  look[7] = 599 ;  
  for (j=0; j < 16; j++)  look[8+j] = 501 + j;

   /*  SATURN    */
  look[24] = 699 ;
  for (j=0; j < 17; j++)  look[25+j] = 601 + j ;

   /*  URANUS    */
  look[42] = 799 ;
  for (j=0; j < 15; j++)  look[43+j] = 701 + j ;

   /*  NEPTUNE    */
  look[58] = 899 ;   look[59] = 801 ;  look[60] = 802 ;

   /*  PLUTO      */
  look[61] = 999 ;    look[62] = 901 ;

   /*  GASPRA      */
  look[63] = 9511010  ;

   /*  IDA   */
  look[64] = 2431010;

  /*   Now the PLANETARY Body name  */

  strcpy(pname[0],  "MERCURY") ; 
  strcpy(pname[1],  "VENUS")   ;
  strcpy(pname[2],  "EARTH")   ;
  strcpy(pname[3],  "MOON")   ;
  strcpy(pname[4],  "MARS")   ;
  strcpy(pname[5],  "PHOBOS")   ;
  strcpy(pname[6],  "DEIMOS")   ;
  strcpy(pname[7],  "JUPITER")   ;
  strcpy(pname[8],  "IO")   ;
  strcpy(pname[9],  "EUROPA")   ;
  strcpy(pname[10], "GANYMEDE")   ;
  strcpy(pname[11], "CALLISTO")   ;
  strcpy(pname[12], "AMALTHEA")   ;
  strcpy(pname[13], "HIMALIA")   ;
  strcpy(pname[14], "ELARA")   ;
  strcpy(pname[15], "PASIPHAE")   ;
  strcpy(pname[16], "SINOPE")   ;
  strcpy(pname[17], "LYSITHEA")   ;
  strcpy(pname[18], "CARME")   ;
  strcpy(pname[19], "ANANKE")   ;
  strcpy(pname[20], "LEDA")   ;
  strcpy(pname[21], "THEBE")   ;
  strcpy(pname[22], "ADRASTEA")   ;
  strcpy(pname[23], "METIS")   ;
  strcpy(pname[24], "SATURN")   ;
  strcpy(pname[25], "MIMAS")   ;
  strcpy(pname[26], "ENCELADUS")   ;
  strcpy(pname[27], "TETHYS")   ;
  strcpy(pname[28], "DIONE")   ;
  strcpy(pname[29], "RHEA")   ;
  strcpy(pname[30], "TITAN")   ;
  strcpy(pname[31], "HYPERION")   ;
  strcpy(pname[32], "IAPETUS")   ;
  strcpy(pname[33], "PHOEBE")   ;
  strcpy(pname[34], "JANUS")   ;
  strcpy(pname[35], "EPIMETHEUS")   ;
  strcpy(pname[36], "HELENE")   ;
  strcpy(pname[37], "TELESTO")   ;
  strcpy(pname[38], "CALYPSO")   ;
  strcpy(pname[39], "ATLAS")   ;
  strcpy(pname[40], "PROMETHEUS")   ;
  strcpy(pname[41], "PANDORA")   ;
  strcpy(pname[42], "URANUS")   ;
  strcpy(pname[43], "ARIEL")   ;
  strcpy(pname[44], "UMBRIEL")   ;
  strcpy(pname[45], "TITANIA")   ;
  strcpy(pname[46], "OBERON")   ;
  strcpy(pname[47], "MIRANDA")   ;
  strcpy(pname[48], "CORDELIA")   ;
  strcpy(pname[49], "OPHELIA")   ;
  strcpy(pname[50], "BIANCA")   ;
  strcpy(pname[51], "CRESSIDA")   ;
  strcpy(pname[52], "DESDEMONA")   ;
  strcpy(pname[53], "JULIET")   ;
  strcpy(pname[54], "PORTIA")   ;
  strcpy(pname[55], "ROSALIND")   ;
  strcpy(pname[56], "BELINDA")   ;
  strcpy(pname[57], "PUCK")   ;
  strcpy(pname[58], "NEPTUNE")   ;
  strcpy(pname[59], "TRITON")   ;
  strcpy(pname[60], "NEREID")   ;
  strcpy(pname[61], "PLUTO")   ;
  strcpy(pname[62], "CHARON")   ;
  strcpy(pname[63], "GASPRA")   ;
  strcpy(pname[64], "IDA")   ;

 /*  Constants are now loaded ....  */ 

  stat = 0;  
  for (j=0; j < N; j++)
  {
   if (id == look[j])
   {
    stat = 1; 
    strcpy(par, pname[j]) ;
    return stat ;
   }
  }

}



/*  include "xvmaininc.h"
    #define  N  64          */

int  zpbid(par, id)
char       par[12]  ;
int        *id   ;
{
 /*  Routine to return ID# given PLANET Name  */
 char   p8[12] ;  
 char   pname[65][12] ;
 int    look[65] ;
 int    j, stat ;

 /*   ID numbering scheme is consistent with GLL SPICE
      LOOKUP table for PLANET I.D.  FROM SEDR WORD # 9   */
   /*   MERCURY  */
  look[0] = 199 ;  

   /*  VENUS     */   
  look[1] = 299 ;

   /*  EARTH     */
  look[2] = 399 ;  look[3] = 301 ;

   /*  MARS      */
  look[4] = 499 ;  look[5] = 401 ;  look[6] = 402 ;

   /*  JUPITER   */
  look[7] = 599 ;  
  for (j=0; j < 16; j++)  look[8+j] = 501 + j;

   /*  SATURN    */
  look[24] = 699 ;
  for (j=0; j < 17; j++)  look[25+j] = 601 + j ;

   /*  URANUS    */
  look[42] = 799 ;
  for (j=0; j < 15; j++)  look[43+j] = 701 + j ;

   /*  NEPTUNE    */
  look[58] = 899 ;   look[59] = 801 ;  look[60] = 802 ;

   /*  PLUTO      */
  look[61] = 999 ;    look[62] = 901 ;

   /*  GASPRA      */
  look[63] = 9511010  ;

   /*  IDA        */
  look[64] = 2431010  ;

  /*   Now the PLANETARY Body name  */

  strcpy(pname[0],  "MERCURY") ; 
  strcpy(pname[1],  "VENUS")   ;
  strcpy(pname[2],  "EARTH")   ;
  strcpy(pname[3],  "MOON")   ;
  strcpy(pname[4],  "MARS")   ;
  strcpy(pname[5],  "PHOBOS")   ;
  strcpy(pname[6],  "DEIMOS")   ;
  strcpy(pname[7],  "JUPITER")   ;
  strcpy(pname[8],  "IO")   ;
  strcpy(pname[9],  "EUROPA")   ;
  strcpy(pname[10], "GANYMEDE")   ;
  strcpy(pname[11], "CALLISTO")   ;
  strcpy(pname[12], "AMALTHEA")   ;
  strcpy(pname[13], "HIMALIA")   ;
  strcpy(pname[14], "ELARA")   ;
  strcpy(pname[15], "PASIPHAE")   ;
  strcpy(pname[16], "SINOPE")   ;
  strcpy(pname[17], "LYSITHEA")   ;
  strcpy(pname[18], "CARME")   ;
  strcpy(pname[19], "ANANKE")   ;
  strcpy(pname[20], "LEDA")   ;
  strcpy(pname[21], "THEBE")   ;
  strcpy(pname[22], "ADRASTEA")   ;
  strcpy(pname[23], "METIS")   ;
  strcpy(pname[24], "SATURN")   ;
  strcpy(pname[25], "MIMAS")   ;
  strcpy(pname[26], "ENCELADUS")   ;
  strcpy(pname[27], "TETHYS")   ;
  strcpy(pname[28], "DIONE")   ;
  strcpy(pname[29], "RHEA")   ;
  strcpy(pname[30], "TITAN")   ;
  strcpy(pname[31], "HYPERION")   ;
  strcpy(pname[32], "IAPETUS")   ;
  strcpy(pname[33], "PHOEBE")   ;
  strcpy(pname[34], "JANUS")   ;
  strcpy(pname[35], "EPIMETHEUS")   ;
  strcpy(pname[36], "HELENE")   ;
  strcpy(pname[37], "TELESTO")   ;
  strcpy(pname[38], "CALYPSO")   ;
  strcpy(pname[39], "ATLAS")   ;
  strcpy(pname[40], "PROMETHEUS")   ;
  strcpy(pname[41], "PANDORA")   ;
  strcpy(pname[42], "URANUS")   ;
  strcpy(pname[43], "ARIEL")   ;
  strcpy(pname[44], "UMBRIEL")   ;
  strcpy(pname[45], "TITANIA")   ;
  strcpy(pname[46], "OBERON")   ;
  strcpy(pname[47], "MIRANDA")   ;
  strcpy(pname[48], "CORDELIA")   ;
  strcpy(pname[49], "OPHELIA")   ;
  strcpy(pname[50], "BIANCA")   ;
  strcpy(pname[51], "CRESSIDA")   ;
  strcpy(pname[52], "DESDEMONA")   ;
  strcpy(pname[53], "JULIET")   ;
  strcpy(pname[54], "PORTIA")   ;
  strcpy(pname[55], "ROSALIND")   ;
  strcpy(pname[56], "BELINDA")   ;
  strcpy(pname[57], "PUCK")   ;
  strcpy(pname[58], "NEPTUNE")   ;
  strcpy(pname[59], "TRITON")   ;
  strcpy(pname[60], "NEREID")   ;
  strcpy(pname[61], "PLUTO")   ;
  strcpy(pname[62], "CHARON")   ;
  strcpy(pname[63], "GASPRA")   ;
  strcpy(pname[64], "IDA")   ;

  strcpy(p8, par) ; 
  
  stat = 0 ;
  for (j=0; j < N; j++)
  {
   if ( strcmp(p8, pname[j]) == 0)
   {
    stat = 1 ;
    *id = look[j] ;
    return  stat ;
   }
  }
}
$ Return
$!#############################################################################

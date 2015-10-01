$!****************************************************************************
$!
$! Build proc for MIPL module librtl
$! VPACK Version 1.8, Tuesday, March 19, 1996, 18:15:10
$!
$! Execute by entering:		$ @librtl
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module librtl ***"
$!
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to librtl.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("librtl.imake") .nes. ""
$   then
$      vimake librtl
$      purge librtl.bld
$   else
$      if F$SEARCH("librtl.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake librtl
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @librtl.bld "STD"
$   else
$      @librtl.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create librtl.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack librtl.com -
	-i librtl.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create librtl.imake
/* Vimake file for creating a shared library out of the main VICAR	*/
/* run-time library.  This contains most of the RTL functions; the	*/
/* companion rtlf contains the rest (see that file for details).	*/
/* Although this file runs on VMS and should create a shareable image	*/
/* equivalent to the existing one, it is really intended for use under	*/
/* Unix.  (Under VMS, there is not rtl companion).  The VMS parts are	*/
/* mainly included as an example (it might not act exactly the same	*/
/* w.r.t. matching the shareable image for VAX->Alpha translated code).	*/
/*									*/
/* This is disabled for some platforms.  See util/imake.config for	*/
/* explanation (search for RTL_HAS_SO).  If you change the support list	*/
/* here, change it there, and in librtlf.com, as well.			*/

#if SUN4_ARCH || SUN_SOLARIS_ARCH || X86_SOLARIS_ARCH || SGI_ARCH || ALPHA_ARCH || AXP_UNIX_ARCH || ANY_LINUX_ARCH

#define PROGRAM librtl

#define LIB_RTL_NOSHR
#define LIB_TAE_NOSHR

#define SHARED_LIBRARY

#define SHARED_MAJOR_VERSION 4
#define SHARED_MINOR_VERSION 4

#if VMS_OS
#define XVZINIT FSYMBOL(xvzinit)
#define XZSINIT FSYMBOL(xzsinit)
#define TAESYM
#else		/* Keep these out of Unix links to avoid Fortran library */
#define XVZINIT
#define XZSINIT
#define TAESYM SYMBOL(q_init) SYMBOL(q_intg) SYMBOL(q_real) SYMBOL(q_string) \
 SYMBOL(q_out) 
#endif

#ifdef RTL_USE_SHELL_VIC
#define SHVIC SYMBOL(v2param_count_elements) SYMBOL(v2param_find_entry) \
 SYMBOL(v2param_get_file) SYMBOL(v2param_get_one_value) \
 SYMBOL(v2param_remove_file)
#else
#define SHVIC
#endif

#define SYMBOL_LIST XVZINIT SYMBOL(zvzinit) XZSINIT SYMBOL(zzinit) \
SYMBOL(zvpinit) SYMBOL(zv_rtl_init) SYMBOL(sc2for) SYMBOL(sc2for_array) \
SYMBOL(sfor2c) SYMBOL(sfor2c_array) SYMBOL(sfor2len) SYMBOL(sfor2ptr) \
SYMBOL(v2_sc2for) SYMBOL(v2_sc2for_array) SYMBOL(v2_sfor2c) \
SYMBOL(v2_sfor2c_array) SYMBOL(v2_sfor2len) SYMBOL(v2_sfor2ptr) \
FSYMBOL(abend) SYMBOL(zabend) FSYMBOL(qprint) SYMBOL(zqprint) \
FSYMBOL(xladd) SYMBOL(zladd) FSYMBOL(xldel) SYMBOL(zldel) \
FSYMBOL(xlget) SYMBOL(zlget) FSYMBOL(xlgetlabel) SYMBOL(zlgetlabel) \
FSYMBOL(xlhinfo) SYMBOL(zlhinfo) FSYMBOL(xlinfo) SYMBOL(zlinfo) \
FSYMBOL(xlninfo) SYMBOL(zlninfo) FSYMBOL(xmove) SYMBOL(zmove) \
FSYMBOL(xvadd) SYMBOL(zvadd) FSYMBOL(xvbands) SYMBOL(zvbands) \
FSYMBOL(xvclose) SYMBOL(zvclose) FSYMBOL(xvcmdout) SYMBOL(zvcmdout) \
FSYMBOL(xvcommand) SYMBOL(zvcommand) FSYMBOL(xveaction) SYMBOL(zveaction) \
FSYMBOL(xvend) SYMBOL(zvend) FSYMBOL(xvfilpos) SYMBOL(zvfilpos) \
FSYMBOL(xvget) SYMBOL(zvget) FSYMBOL(xvintract) SYMBOL(zvintract) \
FSYMBOL(xvip) SYMBOL(zvip) FSYMBOL(xviparm) SYMBOL(zviparm) \
FSYMBOL(xviparmd) SYMBOL(zviparmd) FSYMBOL(xvipcnt) SYMBOL(zvipcnt) \
FSYMBOL(xvipone) SYMBOL(zvipone) FSYMBOL(xvipstat) SYMBOL(zvipstat) \
FSYMBOL(xviptst) SYMBOL(zviptst) FSYMBOL(xvmessage) SYMBOL(zvmessage) \
FSYMBOL(xvopen) SYMBOL(zvopen) FSYMBOL(xvp) SYMBOL(zvp) \
FSYMBOL(xvparm) SYMBOL(zvparm) FSYMBOL(xvparmd) SYMBOL(zvparmd) \
FSYMBOL(xvpblk) SYMBOL(zvpblk) FSYMBOL(xvpclose) SYMBOL(zvpclose) \
FSYMBOL(xvpcnt) SYMBOL(zvpcnt) FSYMBOL(xvpixsizeu) SYMBOL(zvpixsizeu) \
FSYMBOL(xvpixsize) SYMBOL(zvpixsize) FSYMBOL(xvpone) SYMBOL(zvpone) \
FSYMBOL(xvpopen) SYMBOL(zvpopen) FSYMBOL(xvpout) SYMBOL(zvpout) \
FSYMBOL(xvpstat) SYMBOL(zvpstat) FSYMBOL(xvptst) SYMBOL(zvptst) \
FSYMBOL(xvread) SYMBOL(zvread) \
FSYMBOL(xvselpi) SYMBOL(zvselpi) FSYMBOL(xvselpiu) SYMBOL(zvselpiu) \
FSYMBOL(xvsfile) SYMBOL(zvsfile) FSYMBOL(xvsignal) SYMBOL(zvsignal) \
FSYMBOL(xvsize) SYMBOL(zvsize) FSYMBOL(xvsptr) SYMBOL(zvsptr) \
FSYMBOL(xvtpinfo) SYMBOL(zvtpinfo) FSYMBOL(xvtpmode) SYMBOL(zvtpmode) \
FSYMBOL(xvtpset) SYMBOL(zvtpset) FSYMBOL(xvtrans) SYMBOL(zvtrans) \
FSYMBOL(xvtrans_in) SYMBOL(zvtrans_in) \
FSYMBOL(xvtrans_inu) SYMBOL(zvtrans_inu) \
FSYMBOL(xvtrans_out) SYMBOL(zvtrans_out) \
FSYMBOL(xvtrans_set) SYMBOL(zvtrans_set) \
FSYMBOL(xvunit) SYMBOL(zvunit) FSYMBOL(xvwrit) SYMBOL(zvwrit) \
FSYMBOL(xvhost) SYMBOL(zvhost) FSYMBOL(xvpixsizeb) SYMBOL(zvpixsizeb) \
FSYMBOL(xvtrans_inb) SYMBOL(zvtrans_inb) FSYMBOL(xlpinfo) SYMBOL(zlpinfo) \
FSYMBOL(xvfilename) SYMBOL(zvfilename) \
TAESYM \
FSYMBOL(xvqout) SYMBOL(zvq_out) \
FSYMBOL(xvplabel) SYMBOL(zvplabel)\
FSYMBOL(xvplabel2) SYMBOL(zvplabel2)\
SHVIC

#else		/* Do nothing for platforms with no .so */
#define PROCEDURE librtl
#endif

$ Return
$!#############################################################################

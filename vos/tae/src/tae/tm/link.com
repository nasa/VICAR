$! Link taetm
$
$! This procedure uses:
$!	P1 for indicating TAE Classic ("classic")
$!	P2 for version number
$!
$! Change log:
$! 05-may-93 	Added definition of VAXCRTL logical to force linking with 
$!           	the DEC C RTL...ws
$! 21-may-93 	Link with multinet library if VMS_TCP is set...swd
$! 11-jan-94    PR2569: Rename DECW$MOTIF$ROOT to $TAELIB...kbs
$! 26-jan-94    PR2597: Need to link with SYS$LIBRARY:VAXC$EMPTY.EXE...rt

$!
$GET_VERS:
$ IF P2 .EQS. "" THEN -
	INQUIRE P2 "Enter version identifier (default=date/time)"
$ IF P2 .EQS. "" THEN P2 = F$TIME()
$
$ EXENAME = "TAETM"
$ VERSION = P2
$ GOTO LINK
$
$LINK:
$ @$taeTOOLS:VRSION TM "''VERSION'" 
$!
$! Check whether we should link against Multinet's TCP package.
$ warning = "$!"
$ if f$trnlnm("VMS_TCP") .nes. "" 
$ then 
$   set def $taetcp
$   @compile_all
$   set def $taetm
$   tcplib = "$taetcp:tcp/opt,"
$   vms_tcp = ",VMS_TCP"
$   vms_tcp_inc := ,sys$common:[multinet.include],-
		    sys$common:[multinet.include.vms],-
		    sys$common:[multinet.include.sys]
$   warning = "$ write sys$output ""	(Ignore multiply-defined messages for the COMM package.)"""
$ endif
$!
$! Default to linking TM with WPT stuff.
$ IF f$edit(P1, "TRIM, UPCASE") .EQS. "CLASSIC" THEN GOTO LINK_CLASSIC
$!
$ write sys$output "	Compiling DECwindows-dependent source files."
$ if f$search("tclwindows.obj") .nes. "" then delete/nolog tclwindows.obj;*
$ if (f$getsyi("cpu").eq.128)
$ then						! Alpha
$   cc/include=($taeinc,decw$include'vms_tcp_inc')/define=(XWINDOWS'vms_tcp')/standard=vaxc/tie/float=d_float/warn=(disable=missingreturn) TCLWINDOWS.c
$ else						! VAX
$   cc/include=($taeinc,decw$include'vms_tcp_inc')/define=(XWINDOWS'vms_tcp') TCLWINDOWS.c 
$ endif
$! 
$ WRITE SYS$OUTPUT "	*** LINKING THE TAE MONITOR (GRAPHICS VERSION)"
'warning'
$!
$! ** If you modify the link command, modify classic link below **
$!
$ DEFINE/USER VAXCRTL SYS$LIBRARY:VAXC$EMPTY.EXE
$link/exe=$taebin:'exename' -
		[]vrsion, -		!Tm version number
		ACTION,-
		ASYNC,-
		ASYNCPORT,-
		BATCH,-
		BLDJOB,-
		CMDFIELD,-
		COMPILE,-
		DECLARE,-
		DYNAMIC,-
		EDITOR,-
		EVALEXP,-
		EXITS,-
		FILEINS,-
		GLOBALS,-
		GREET,-
		HARDCOPY,-
		HELP,-
		HELPSUBS,-
		HOSTCMD,-
		IF,-
		INTLIN,-
		INTRINSIC,-
		LET,-
		LOG,-
		LOGSUBS,-
		LOOPS,-
		MENUPORT,-
		MISCCMD,-
		MOVEST,-
		PACKAGE,-
		PARSER,-
		PDFPRC,-
		QUALIF,-
		SAVERES,-
		SENDVAR,-
		SETCHECK,-
		SETSHOW,-
		SPECVCOPY,-
		SUBCMD,-
		SYMBOL,-
		TASKPAR,-
		TCLFUNC,-
		TCLWINDOWS,-
		TERMINAL,-
		TM,-
		TMINIT,-
		TMSUBS,-
		TMUTIL,-
		TUTCMD,-
		TUTDISP,-
		TUTGLOBS,-
		TUTHELP,-
		TUTORPORT,-
		TUTSELECT,-
		TUTSUBS,-
		UTIL,-
		VALID,-
		VCOPY,-
		WRTMSG,-
		'tcplib'-
		$taelib:tae_link/opt,-
		$taelib:libtaevicar/lib,-
!		$taelib:libwpt/lib,-
!		libddo/lib,-
!		libwmw/lib,-
!		libinterviewsx11/lib,-
!		libgraphic/lib,-
!		libtae/include=chain, - !special debug and TAE library
!		libtae/lib, -
!		libbsd/lib, -
		$taelib:dxm_link/opt, -
		sys$input/options
!
!	This makes the PSECT "$char_string_constants" 
!	protected and shareable, which is a big help (32KB/user)
!	if TM is shared.
!
	PSECT_ATTR=$CHAR_STRING_CONSTANTS,SHR,RD,NOWRT
$
$ GOTO CLEANUP
$
$LINK_CLASSIC:
$
$ write sys$output "	Compiling DECwindows-related source files without DECwindows."
$ if f$search("tclwindows.obj") .nes. "" then delete/nolog tclwindows.obj;*
$ taecc TCLWINDOWS.c 
$! 
$ WRITE SYS$OUTPUT "	*** LINKING THE TAE MONITOR (CLASSIC VERSION)"
$!
$ DEFINE/USER VAXCRTL SYS$LIBRARY:VAXC$EMPTY.EXE
$link/notraceback/exe=$taebin:'exename' -
		[]vrsion,-		!Tm version number
		ACTION,-
		ASYNC,-
		ASYNCPORT,-
		BATCH,-
		BLDJOB,-
		CMDFIELD,-
		COMPILE,-
		DECLARE,-
		DYNAMIC,-
		EDITOR,-
		EVALEXP,-
		EXITS,-
		FILEINS,-
		GLOBALS,-
		GREET,-
		HARDCOPY,-
		HELP,-
		HELPSUBS,-
		HOSTCMD,-
		IF,-
		INTLIN,-
		INTRINSIC,-
		LET,-
		LOG,-
		LOGSUBS,-
		LOOPS,-
		MENUPORT,-
		MISCCMD,-
		MOVEST,-
		PACKAGE,-
		PARSER,-
		PDFPRC,-
		QUALIF,-
		SAVERES,-
		SENDVAR,-
		SETCHECK,-
		SETSHOW,-
		SPECVCOPY,-
		SUBCMD,-
		SYMBOL,-
		TASKPAR,-
		TCLFUNC,-
		TCLWINDOWS,-
		TERMINAL,-
		TM,-
		TMINIT,-
		TMSUBS,-
		TMUTIL,-
		TUTCMD,-
		TUTDISP,-
		TUTGLOBS,-
		TUTHELP,-
		TUTORPORT,-
		TUTSELECT,-
		TUTSUBS,-
		UTIL,-
		VALID,-
		VCOPY,-
		WRTMSG,-
		$taelib:libtae/include=chain, - !special debug and TAE library
		libtae/lib, -
		libtaevicar/lib, -
		crtl/opt, -		!C shared library
	sys$input/options
!
!	This makes the PSECT "$char_string_constants" 
!	protected and shareable, which is a big help (32KB/user)
!	if TM is shared.
!
	PSECT_ATTR=$CHAR_STRING_CONSTANTS,SHR,RD,NOWRT
$
$CLEANUP:
$!
$!	***** CLEANUP *****
$!
$ DELETE/NOLOG VRSION.OBJ;*
$ DELETE/NOLOG TM.VER;*
$ purge/nolog $TAEBIN:'exename'.EXE
$ set protection=w:re $taebin:'exename'.exe
$ WRITE SYS$OUTPUT  "	*** LINK OF ''exename' COMPLETE "

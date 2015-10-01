/*
 *
 *	XDROUTINES.H 
 *
 *	Defines and character strings for all XD interface routines
 *	Used by:						
 *		xd_sys_msg - to get name of xd_current_call
 *		XD...      - to set xd_current_call
 *								
 *	If any new routines are added, they should placed at the end
 *	the list and N_ROUTINES should be updated.
 */

#define ACLEAR		0
#define AOFF		1
#define AON		2
#define ATEXT		3
#define CAUTOTRACK	4
#define CLOCATION	5
#define COFF		6
#define CON		7
#define CSET		8
#define DACTIVATE	9
#define DALLOCATE	10
#define DCONFIGURE	11
#define DFREE		12
#define DINFO		13
#define DNAME		14
#define DOPEN		15
#define GCONNECT	16
#define GLCONSTANT	17
#define GLREAD		18
#define GLWRITE		19
#define GOFF		20
#define GON		21
#define IAREAFILL	22
#define IAWLOCATION	23
#define IAWREAD		24
#define IAWSET		25
#define IAWWRITE	26
#define ICIRCLE		27
#define IDWLOCATION	28
#define IDWSET		29
#define IFILL		30
#define IHISTOGRAM	31
#define IIARITHMETIC	32
#define IICOPY		33
#define IILOGICAL	34
#define IISHIFT		35
#define ILINEREAD	36
#define ILINEWRITE	37
#define IMAWWRITE	38
#define	IMCIRCLE	39
#define IMFILL		40
#define IMLINEWRITE	41
#define IMPIXELWRITE	42
#define	IMPOLYLINE	43
#define IPIXELREAD	44
#define IPIXELWRITE	45
#define IPOLYLINE	46
#define IROTATE		47
#define	IZOOM		48
#define LCONNECT	49
#define LRAMP		50
#define LREAD		51
#define LWRITE		52
#define TCOLOR		53
#define TFONT		54
#define TLENGTH		55
#define	TMASK		56
#define TROTATE		57
#define TSIZE		58
#define TTEXT		59
#define X1D		60
#define X2D		61
#define X3D		62
#define XSWITCH		63
#define EACTION		64
#define ESIGNAL		65
#define DUNIT		66
#define DCLOSE		67
#define DOPCLS		68
#define DMOVE		69
#define DRESIZE		70
#define DNAMEDUNIT	71
#define DUNITNAMES	72
#define CSHOW		73
#define CILOCATION	74
#define CISET		75
#define GLINIT		76
#define CSIZE		77
#define CCOLOR		78
#define DBATCH		79

#define N_ROUTINES	81

#ifndef	XD_INITIALIZE
PUBLIC	char	*xd_routines[N_ROUTINES];
#else	/* XD_INITIALIZE */
PUBLIC	char	*xd_routines[N_ROUTINES] = {
	"XDACLEAR",		"XDAOFF",		"XDAON",
	"XDATEXT",		"XDCAUTOTRACK",		"XDCLOCATION",
	"XDCOFF",		"XDCON",		"XDCSET",
	"XDDACTIVATE",		"XDDALLOCATE",		"XDDCONFIGURE",
	"XDDFREE",		"XDDINFO",		"XDDNAME",
	"XDDOPEN",		"XDGCONNECT",		"XDGLCONSTANT",
	"XDGLREAD",		"XDGLWRITE",		"XDGOFF",
	"XDGON",		"XDIAREAFILL",		"XDIAWLOCATION",
	"XDIAWREAD",		"XDIAWSET",		"XDIAWWRITE",
	"XDICIRCLE",		"XDIDWLOCATION",	"XDIDWSET",
	"XDIFILL",		"XDIHISTOGRAM",		"XDIIARITHMETIC",
	"XDIICOPY",		"XDIILOGICAL",		"XDIISHIFT",
	"XDILINEREAD",		"XDILINEWRITE",		"XDIMAWWRITE",
	"XDIMCIRCLE",		"XDIMFILL",		"XDIMLINEWRITE",
	"XDIMPIXELWRITE",	"XDIMPOLYLINE",		"XDIPIXELREAD",
	"XDIPIXELWRITE",	"XDIPOLYLINE",		"XDIROTATE",
	"XDIZOOM",		"XDLCONNECT",		"XDLRAMP",
	"XDLREAD",		"XDLWRITE",		"XDTCOLOR",
	"XDTFONT",		"XDTLENGTH",		"XDTMASK",
	"XDTROTATE",		"XDTSIZE",		"XDTTEXT",
	"XDX1D",		"XDX2D",		"XDX3D",
	"XDXSWITCH",		"XDEACTION",		"XDESIGNAL",
	"XDDUNIT",		"XDDCLOSE",
	"XDDOPCLS",		"XDDMOVE",		"XDDRESIZE",
	"XDDNAMEDUNIT",		"XDDUNITNAMES",
	"XDCSHOW",		"XDCILOCATION",		"XDCISET",
	"XDGLINIT",		"XDCSIZE",		"XDCCOLOR",
	"XDDBATCH"
	};
#endif	/* XD_INITIALIZE */

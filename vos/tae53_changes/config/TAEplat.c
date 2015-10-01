/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/*
 * To add support for another platform:
 * 
 *     1.  Identify a machine-specific cpp symbol.  If your preprocessor 
 *         doesn't have any built in, you'll need to add the symbol to the
 *         cpp_argv table in imake/imake.c and rebuild imake with the
 *         BOOTSTRAPCFLAGS variable set (see the macII for an example).
 *
 *     2.  Add all machine-specific cpp symbols (either defined by you or by
 *         the preprocessor or compiler) to the predefs table in
 *         makedepend/main.c.
 *
 *     3.  Put a new #ifdef block below that defines MacroIncludeFile and
 *         MacroFile for your new platform and then #undefs the machine-
 *         specific preprocessor symbols (to avoid problems with file names).
 *
 *     4.  Create a .cf file with the name given by MacroFile.
 *
 * Modification history:
 *
 * created : 29-Aug-1990 baw (copied verbatim from InterViews)
 * 27-sep-91	Add capability to echo macro file...ljn
 * 05-oct-92	IBM RS/6000 port...rt
 * 01-apr-94	Concurrent port: change masscomp to rtu...dag
 * 04-apr-94	SCO Unix port...swd
 * 17-may-94	v5.3 IBM RS/6000 port:  undef MachineDep...dag
 * 08-jul-94	Added Intergraph (__clipper__)..dag
 */

#define POUND #

#ifdef ultrix
#undef ultrix
#define UltrixArchitecture
#ifdef vax
#undef VAX
#undef vax
#define VaxArchitecture
#define MachineDep VAX
#define machinedep vax
#endif
#ifdef mips
#undef mips
#undef MIPSEL
#undef mipsel
#define MipsArchitecture
#define MachineDep MIPSEL
#define machinedep mipsel
#endif
#define MacroIncludeFile <ultrix.cf>
#define MacroFile ultrix.cf
#endif

#if defined(vax) && !defined(UltrixArchitecture)
#undef VAX
#undef vax
#define VaxArchitecture
#define MachineDep VAX
#define machinedep vax
#define MacroIncludeFile <bsd.cf>
#define MacroFile bsd.cf
#endif

/* Added definitions for DEC ALPHA platform */
#if defined(__alpha) && !defined(__linux__)  && !defined(linux)
#define AlphaArchitecture
#define MachineDep ALPHA
#define machinedep alpha
#define MacroIncludeFile <alpha.cf>
#define MacroFile alpha.cf
#endif

#ifdef sun
#undef sun
#define SunArchitecture
#ifdef mc68020
#undef SUN3
#undef sun3
#define MachineDep SUN3
#define machinedep sun3
#endif
#ifdef sparc
#undef SUN4
#undef sun4
#define MachineDep SUN4
#define machinedep sun4
#endif
#ifdef i386
#undef SUNi386
#undef sunI386
#define MachineDep SUNi386
/*
 * modified: 15-Sep-1990 baw
 * machinedep must be all lower case, see note at end of file
 * changed "machinedep sunI386" to "machinedep suni386"
 */
#define machinedep suni386
#endif
#ifndef MachineDep
#undef SUN
#undef sun
#define MachineDep SUN
#define machinedep sun
#endif
#define MacroIncludeFile <sun.cf>
#define MacroFile sun.cf
#endif /* sun */

#ifdef hpux
#undef hpux
#define HPArchitecture
#ifdef hp9000s300
#undef HP300
#undef hp300
#define MachineDep HP300
#define machinedep hp300
#else
#ifdef hp9000s200
#undef HP200
#undef hp200
#define MachineDep HP200
#define machinedep hp200
#endif
#endif
#ifdef hp9000s500
#undef HP500
#undef hp500
#define MachineDep HP500
#define machinedep hp500
#endif
#ifdef hp9000s800
#undef HP800
#undef hp800
#define MachineDep HP800
#define machinedep hp800
#endif
#ifndef MachineDep
#undef HP
#undef hp
#define MachineDep HP
#define machinedep hp
#endif
#define MacroIncludeFile <hp.cf>
#define MacroFile hp.cf
#endif /* hpux */

#ifdef att
#undef ATT
#undef att
#define ATTArchitecture
#define MachineDep ATT
#define machinedep att
#define MacroIncludeFile <att.cf>
#define MacroFile att.cf
#endif /* att */

#ifdef apollo
#undef APOLLO
#undef apollo
#define ApolloArchitecture
#define MachineDep APOLLO
#define machinedep apollo
#define MacroIncludeFile <apollo.cf>
#define MacroFile apollo.cf
#endif /* apollo */

#ifdef sony
#undef SONY
#undef sony
#define SonyArchitecture
#define MachineDep SONY
#define machinedep sony
#define MacroIncludeFile <sony.cf>
#define MacroFile sony.cf
#endif /* sony */

#ifdef M4310
#undef M4310
#undef PEGASUS
#undef pegasus
#define PegasusArchitecture
#define MachineDep PEGASUS
#define machinedep pegasus
#define MacroIncludeFile <pegasus.cf>
#define MacroFile pegasus.cf
#endif /* M4310 */

#ifdef M4330
#undef M4330
#undef m4330
#define M4330Architecture
#define MachineDep M4330
#define machinedep m4330
#define MacroIncludeFile <m4330.cf>
#define MacroFile m4330.cf
#endif /* M4330 */

#ifdef masscomp
#undef MASSCOMP
#undef masscomp
#define RTUArchitecture
#define MachineDep RTU
#define machinedep rtu
#define MacroIncludeFile <rtu.cf>
#define MacroFile rtu.cf
#endif /* masscomp */

/* A/UX cpp has no unique symbol:  build imake with BOOTSTRAPCFLAGS=-DmacII */
#ifdef macII
#undef MACII
#undef macII
#define MacIIArchitecture
#define MachineDep MACII
/*
 * modified: 15-Sep-1990 baw
 * machinedep must be all lower case, see note at end of file
 * changed "machinedep macII" to "machinedep macii"
 */
#define machinedep macii
#define MacroIncludeFile <macII.cf>
#define MacroFile macII.cf
#endif /* macII */

#ifdef CRAY
#undef CRAY
#undef cray
#define CrayArchitecture
#define MachineDep CRAY
#define machinedep cray
#define MacroIncludeFile <cray.cf>
#define MacroFile cray.cf
#endif /* CRAY */

#ifdef sgi
#undef SGI
#undef sgi
#undef mips
#define SGIArchitecture
#define MipsArchitecture
#define MachineDep SGI
#define machinedep sgi
#define MacroIncludeFile <sgi.cf>
#define MacroFile sgi.cf
#endif

#if defined(mips) && !defined(UltrixArchitecture) && !defined(SGIArchitecture)
#undef mips
#undef umips
#undef MIPSEB
#undef mipseb
#define MipsArchitecture
#define UMipsArchitecture
#define MachineDep MIPSEB
#define machinedep mipseb
#define MacroIncludeFile <umips.cf>
#define MacroFile umips.cf
#endif

/* ----- No unique manifest defines in SCO cpp.  The imake.tmpl delivered
 * -----    in /usr/lib/X11/config assumes sco is defined.
 */
#if defined(sco)
#undef sco
#define SCOArchitecture
#define MachineDep SCO
#define machinedep sco
#define MacroIncludeFile <sco.cf>
#define MacroFile sco.cf
#endif /*sco */

#ifdef __clipper__
#undef __clipper__
#define IngrArchitecture
#define MachineDep INGR
#define machinedep ingr
#define MacroIncludeFile <ingr.cf>
#define MacroFile ingr.cf
#endif /* __clipper__ (Intergraph) */

#if defined(ibm) || defined(_IBMR2)
#undef ibm
#undef IBM
#define IBMArchitecture
#ifdef i386
#undef i386
#undef IBMi386
#undef ibmI386
#define PS2Architecture
#define MachineDep IBMi386
/*
 * modified: 15-Sep-1990 baw
 * machinedep must be all lower case, see note at end of file
 * changed "machinedep ibmI386" to "machinedep ibmi386"
 */
#define machinedep ibmi386
#endif
#ifdef ibm032
#undef ibm032
#undef RT
#undef rt
#define RtArchitecture
#define MachineDep RT
#define machinedep rt
#endif
#ifdef aix
#undef aix
#undef AIX
#define AIXArchitecture
#define MachineDep AIX
#define machinedep aix
#endif
#ifdef _IBMR2
#undef MachineDep
#undef machinedep
#define MachineDep rs6000
#define machinedep rs6000
#endif
#ifndef MachineDep
#define MachineDep IBM
#define machinedep ibm
#endif
#define MacroIncludeFile <ibm.cf>
#define MacroFile ibm.cf
#endif /* ibm */

#if defined(__linux__) || defined(linux)
#define LinuxArchitecture
#ifdef __alpha__
#define LinuxAlphaArchitecture
#define MachineDep AXP_Linux
#define machinedep axp_linux
#define MacroIncludeFile <axp_linux.cf>
#define MacroFile axp_linux.cf
#else
#ifdef __ppc__
#define LinuxPPCArchitecture
#define MachineDep PPC_Linux
#define machinedep ppc_linux
#define MacroIncludeFile <ppc_linux.cf>
#define MacroFile ppc_linux.cf
#else
#ifdef __x86_64
#define LinuxX8664Architecture
#define MachineDep X86_64_Linx
#define machinedep x86_64_linx
#define MacroIncludeFile <x86_64_linx.cf>
#define MacroFile x86_64_linx.cf
#else
#define LinuxX86Architecture
#define MachineDep X86_Linux
#define machinedep x86_linux
#define MacroIncludeFile <x86_linux.cf>
#define MacroFile x86_linux.cf
#endif
#endif
#endif
#endif

#ifdef __APPLE__
#ifdef __ppc__
#define MacOSXArchitecture
#define MachineDep Mac_OSX
#define machinedep mac_osx
#define MacroIncludeFile <mac_osx.cf>
#define MacroFile mac_osx.cf
#else
#define MacX86OSXArchitecture
#define MachineDep X86_MacOSX
#define machinedep x86_macosx
#define MacroIncludeFile <x86_macosx.cf>
#define MacroFile x86_macosx.cf
#endif
#endif

#ifndef MachineDep
POUND WARNING: Imakefile not configured; guessing at definitions!!!
POUND This might mean that BOOTSTRAPCFLAGS was not set when building imake.
#undef UNKNOWN
#undef unknown
#define MachineDep UNKNOWN
#define machinedep unknown
#define MacroIncludeFile <generic.cf>
#define MacroFile generic.cf
#endif

/*
 * Identify the architecture for scripts/cpu.sh.
 *
 * Note: You MUST use the lower-case version of the platform or tm
 * will get real ornery.
 */
POUND architecture:  machinedep

/*
 * Identify the configuration file.
 */
POUND configfile:  MacroFile

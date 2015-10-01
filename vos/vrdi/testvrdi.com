$!****************************************************************************
$!
$! Build proc for MIPL module testvrdi
$! VPACK Version 1.5, Thursday, November 05, 1992, 15:14:18
$!
$! Execute by entering:		$ @testvrdi
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
$ write sys$output "*** module testvrdi ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("testvrdi.imake") .nes. ""
$   then
$      vimake testvrdi
$      purge testvrdi.bld
$   else
$      if F$SEARCH("testvrdi.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake testvrdi
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @testvrdi.bld "STD"
$   else
$      @testvrdi.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create testvrdi.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack testvrdi.com -
	-s testvrdi.c -
	-i testvrdi.imake -
	-p testvrdi.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create testvrdi.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <ctype.h>

#include "vicmain_c"

#include "xdexterns.h"
#include "xderrors.h"

/*
  TESTVRDI

  This program will test the following VRDI functions:

    1. Ability to get a list of currently allocated devices
       XDDUnitNames

    2. Get the unit numbers for more than one device
       XDDNamedUnit

    3. Open more than one unit at a time
       XDDOpen      -- normal open
       XDDSmartOpen -- conditional initialization 

    4. Ability to manipulate more than one device
       XDIAWWrite

  Writen by: Mark S. Mann
  Date:      2/23/89

*/

#define ERRORP     printf
#define PASSED     printf
#define D1         if (TEST_LEVEL >= 1) printf
#define D2         if (TEST_LEVEL >= 2) printf

#define MAXNDEV	10
#define NDEV   2
#define STRLEN 20

int n_errors = 0;

int  TEST_LEVEL;

main44()
{
  register int i, j;                /* loop */
  int  line;
  int  status;
  int  num;

  /*   VICAR variables */
  int  vic_unit[NDEV];        /* VICAR unit number */
  int  instance;

  int  sl[NDEV],               /* starting line of image to be shown */
       ss[NDEV],               /* starting sample of image to be shown */
       nl[NDEV],               /* number of lines in image to be shown */
       ns[NDEV];               /* number of samples in image to be shown */
  int nli, nsi;		       /* dummy variables */

  int  nlimg[NDEV],            /* number of lines in image */
       nsimg[NDEV],            /* number of samples in image */
       nldev[NDEV],            /* number of lines in device */
       nsdev[NDEV];            /* number of samples in device */

  int  dev_offset[NDEV];
  char device[NDEV][STRLEN];
  char alloc_device[MAXNDEV][STRLEN];

  int count,                  /* VICAR parm count */
      deflt;                  /* VICAR parm default */
      

  /*   VRDI variables */
  int  vrdi_unit[NDEV], unit;

  int  error_return  = 1,
       error_message = 2,
       error_abort   = 3;

  int  info_offset,
       info_nitems;

  int  activate = 1;

  static int configure[4] = {0, 0, 0, 0};

  int  top,  bottom,
       left, right;

  int  imp = 1,
       lut = 1;

  char *image[NDEV],
       *ptr[NDEV];

  char buffer[NDEV][2000];         /* line buffer */
  int  size[NDEV];

  int  device_dependent_value;
  int  len;


  /* open VICAR image file */
  for (j=0; j<NDEV; j++) {
    instance = j + 1;

    zvunit (&vic_unit[j], "INP", instance, 0);
    status = zvopen (vic_unit[j], 0);

    if (status != SUCCESS) {
      printf("TESTVRDI ERROR: couldn't open file %d\n", instance);
      zabend();
    }

    zvsize(&sl[j], &ss[j], &nl[j], &ns[j], &nli, &nsi);
    zlget(vic_unit[j], "SYSTEM", "NL", &nlimg[j], "FORMAT", "INT", 0);
    zlget(vic_unit[j], "SYSTEM", "NS", &nsimg[j], "FORMAT", "INT", 0);

  }

  zvp("TEST_LVL", &TEST_LEVEL, &count);

  zvparm("DEVICES", device, &count, &deflt, NDEV, STRLEN);

  D1("Call zddunitnames\n");
  len = STRLEN;
  count = MAXNDEV;
  status = zddunitnames(&count, len, alloc_device);
  check_status(NoUnit, status, "zddunitnames", device[0]);

  if (status == SUCCESS) {
    PASSED("Currently allocated devices:\n");
    for (i=0; i<count; i++) {
      PASSED("\t%s\n", alloc_device[i]);
    }
  }

  /* set VRDI error actions */
  zdeaction(error_return, error_message, error_abort);

  /* set device unit number */
  for (j=0;j<NDEV;j++) {

    D1("\n");

    upperstring(device[j]);

    D1("Get vrdi unit[%d] for device %s\n", j, device[j]);
    status = zddnamedunit(&vrdi_unit[j], device[j]);

    check_status(vrdi_unit[j], status, "zddnamedunit", device[j]);

    /* open device */
    if (j == 0) {
      D1("Open device %s -- mandatory initialization\n", device[j]);
      status = zddopen(vrdi_unit[j]);
      check_status(vrdi_unit[j], status, "zddopen", device[j]);
    }
    else {
      D1("Open device %s -- conditional initialization\n", device[j]);
      status = zddsmartopen(vrdi_unit[j]);
      check_status(vrdi_unit[j], status, "zddsmartopen", device[j]);
    }

    if (status == SUCCESS) {
      PASSED("Successfully opened %s\n", device[j]);
    }

    /* Test shared memory */
    D1("Check DCB for device %s\n", device[j]);

    unit = vrdi_unit[j];

    if (! ZDCB_INITIALIZED) {
      ERRORP("\tERROR: DCB_INITIALIZED flag for device %s is FALSE\n",
	    device[j]);
    }
    else {
      PASSED("\tPASSED: DCB_INITIALIZED flag OK for device %s\n", device[j]);
    }

    if (! ZSHARED_MEMORY_ACTIVE) {
      ERRORP("\tERROR: SHARED_MEMORY_ACTIVE flag for device %s is FALSE\n",
		device[j]);
      ERRORP("\t       Should be TRUE\n");
    }
    else {
      PASSED("\tPASSED: SHARED_MEMORY_ACTIVE flag OK for device %s\n",
	     device[j]);
    }

    device_dependent_value = DCB[unit]->DeviceDependent[0];

    D1("This is test # %d for device %s\n", device_dependent_value + 1,
       device[j]);

    DCB[unit]->DeviceDependent[0] = device_dependent_value + 1;

    if (j == 0) {
      if (ZDEV_ACTIVE) {
	ERRORP("\tERROR: DEV_ACTIVE is TRUE for device %s\n", device[j]);
	ERRORP("\t     Should be FALSE since it should have been\n");
	ERRORP("\t     initialized.\n");
      }
      else {
	PASSED("\tPASSED: DCB has been initialized properly for device %s\n",
	       device[j]);
      }
    }
    else {
      if (device_dependent_value && !ZDEV_ACTIVE) {
	ERRORP("\tERROR: DEV_ACTIVE is FALSE for device %s\n", device[j]);
	ERRORP("\t    Should be TRUE since it already has been initialized\n");
	ERRORP("\t    and it should not have been reinitialized.\n");
      }
      else {
	PASSED("\tPASSED: DCB has been initialized properly for device %s\n",
	       device[j]);
      }
    }

    /* find device's max dimensions */
    info_offset = 5; /* nl in device */
    info_nitems = 1;
    D2("Get number of lines for device %s using zddinfo\n", device[j]);
    status = zddinfo(vrdi_unit[j], info_offset, info_nitems, &nldev[j]);
    check_status(vrdi_unit[j], status, "zddinfo", device[j]);
    
    info_offset = 6; /* ns in device */
    D2("Get number of samples for device %s using zddinfo\n", device[j]);
    status = zddinfo(vrdi_unit[j], info_offset, info_nitems, &nsdev[j]);
    check_status(vrdi_unit[j], status, "zddinfo", device[j]);
    
    /* activate device */
    D2("Activate device %s\n", device[j]);
    status = zddactivate(vrdi_unit[j], activate);
    check_status(vrdi_unit[j], status, "zddactivate", device[j]);
    
    /* configure device to default settings */
    D2("Configure device %s\n", device[j]);
    status = zddconfigure( vrdi_unit[j], configure);
    check_status(vrdi_unit[j], status, "zddconfigure", device[j]);
    
    nl[j] = (sl[j]+nl[j]-1 > nlimg[j]) ? MAX(0,((nlimg[j]-sl[j])+1)) : nl[j];
    ns[j] = (ss[j]+ns[j]-1 > nsimg[j]) ? MAX(0,((nsimg[j]-ss[j])+1)) : ns[j];
    
    nl[j] = ( nl[j] > nldev[j]) ? nldev[j] : nl[j] ;
    ns[j] = ( ns[j] > nsdev[j]) ? nsdev[j] : ns[j] ; 
    
    /* get image memory */
    D2("Get memory for image for device %s\n", device[j]);
    image[j] = (char *)malloc( ns[j]*nl[j] );
    ptr[j]   = image[j];

    /* read in image */
    D1("Read in image for device %s\n", device[j]);
    for ( line=sl[j]; line < sl[j]+nl[j]; line++ ) {
      zvread(vic_unit[j], ptr[j], "LINE", line, "SAMP", ss[j],
	     "NSAMPS", ns[j], 0);
      ptr[j] += ns[j];
    }

    /* set access window area to size of image */
    top    = 1;
    bottom = nl[j];
    left   = 1;
    right  = ns[j];
    D2("Set up access window for device %s\n", device[j]);
    status = zdiawset(vrdi_unit[j], imp, left, top, right, bottom);
    check_status(vrdi_unit[j], status, "zdiawset", device[j]);

    /* write the image */
    size[j] = nl[j] * ns[j];

    D1("Displaying image to device %s\n", device[j]);
    status = zdiawwrite(vrdi_unit[j], imp, size[j], image[j]);
    check_status(vrdi_unit[j], status, "zdiawwrite", device[j]);
  }

  D1("\n");
  for (j=0;j<NDEV;j++) {
    D1("Close device %s\n", device[j]);
    /* close the device */
    status = zddclose(vrdi_unit[j]);
    check_status(vrdi_unit[j], status, "zddclose", device[j]);
    
    /* close the VICAR file */
    zvclose(vic_unit[j], 0);
  }

  if (n_errors == 0) {
    printf("\nVRDI TEST COMPLETED SUCCESSFULLY\n");
  }
  else {
    printf("\nVRDI TEST FAILED with %d errors\n", n_errors);
  }
}

check_status(unit, status, name, device)
     int  unit;
     int  status;
     char *name, *device;
{
  if (status != SUCCESS) {
    n_errors++;
    D2("Check status = %d from the routine %s for device %s\n",
       status, name, device);
    zdesignal(status);
  }
  else {
    D2("Successful call to %s for device %s\n", name, device);
  }
}  
      

upperstring( string )
     char	*string;
{
  int	i, length;

  length = strlen( string );
  for ( i = 0; i < length; i++ ) {
    if ( islower(string[i]) ) {
      string[i] = toupper(string[i]);
    }
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create testvrdi.imake
#define PROGRAM testvrdi
#define MODULE_LIST testvrdi.c

#define MAIN_LANG_C
#define USES_C

#define TEST

#define LIB_VRDI_NOSHR
#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$PDF_File:
$ create testvrdi.pdf
process help=*
PARM INP	TYPE = STRING	COUNT = 2
PARM DEVICES	TYPE = STRING	COUNT = 2
PARM TEST_LVL	TYPE = INTEGER	COUNT = 1 VALID=(0,1,2) DEFAULT=0
END-PROC
.title
TESTVRDI
.help
TEST VRDI FUNCTIONS

	TESTVRDI will test the new functions for VRDI.  They include 
	the following capabilities:
		o Access the names of currently allocated devices
		o Get the unit number of a specific device
		o Open a device with conditional or mandatory initialization
		o Open more than one device at a time
		o Access multiple open devices
		o Shared memory
.page

	This program goes through the following procedure to test VRDI's
	features:

	It calls XDDUnitNames to get a list of the devices allocated to 
	the current process.  It will print them out in the order from 
	the most recent allocation to the first allocation.

	A call to XDDNamedUnit for each of the devices given in DEVICES,
	gets the unit number for that specific display device.

	The first device given in DEVICES is opened with XDDOpen.  This
	device will be opened and initialized.

	The other device given in DEVICES is opened with XDDSmartOpen. This
	device will be opened, then depending upon certain conditions will
	be initialized.  If XDDSmartOpen was able to attach to the shared
	memory associated with the device and the device has been already
	initialized by another application,  then the device will not be
	initialized.  Otherwise, it will be initialized as it would have been
	using XDDOpen.
.page

	Once each device is opened, its initialization is checked.  Each
	device's DCB_INITIALIZED and SHARED_MEMORY_ACTIVE flags are checked.
	They should both be set to TRUE.

	The DEV_ACTIVE flag, stored in the DCB structure, is set to FALSE
	when the DCB is initiailized.  It is set to TRUE, with XDDActivate,
	before any processing is done on the device.

	The DCB also holds an array of integers called DeviceDependent.  Each
	time this program is called, its first element is incremented. If
	shared memory is working, then the value in this location should 
	reflect the number of times this application has been called.

	If the first device has been initialized properly by XDDOpen, then
	the DEV_ACTIVE flag should be FALSE.  

	If the  other device has been  processed properly by XDDSmartOpen, 
	then the DEV_ACTIVE flag should be FALSE if the first location in
	the DeviceDependent array is zero.  Otherwise DEV_ACTIVE should be
	TRUE.
.page

	To test whether both devices can be accessed, the images specified
	in INP are displayed on the devices using a call to XDIAWWrite.

	A call to XDDClose closes each of the devices.
.page

	ERROR MESSAGES:

	After each VRDI call a status is returned.  This status is checked
	by xd_error_handler.  Any errors produced by a VRDI routine will
	be printed by zddsignal.

	Three levels of error messages are possible.  The different levels
	depend on the value of TEST_LEVEL:

	TEST_LEVEL = 0:	Print only results of the VRDI tests.
	TEST_LEVEL = 1:	Print a message before critical VRDI calls.
	TEST_LEVEL = 2:	Print a message before each VRDI call and the
			results of each call.

	The last message prints either SUCCESS or FAILURE.  If a failure
	occurred, then the number of VRDI calls that returned with a 
	non-SUCCESS code is printed.
.level1
.vari inp
Input images
.vari devices
Allocated display devices
.vari test_lvl
Error message level
.level2
.vari inp

	These are two VICAR images which will be shown in the 
	display devices.

.vari devices

	These are two display devices.  They must be allocated
	by the current process.

.vari test_lvl

	This is level of the debug messages printed by this program.
	Each debug level also prints  all the messages of lower levels.

	LEVEL 0:	Print only results of the VRDI tests.
	LEVEL 1:	Print a message before critical VRDI calls.
	LEVEL 2:	Print a message before each VRDI call and the
			results of each call.
.end

$ Return
$!#############################################################################

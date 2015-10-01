$!****************************************************************************
$!
$! Build proc for MIPL module vrditest
$! VPACK Version 1.5, Thursday, November 05, 1992, 13:29:41
$!
$! Execute by entering:		$ @vrditest
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
$ write sys$output "*** module vrditest ***"
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
$   if F$SEARCH("vrditest.imake") .nes. ""
$   then
$      vimake vrditest
$      purge vrditest.bld
$   else
$      if F$SEARCH("vrditest.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vrditest
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vrditest.bld "STD"
$   else
$      @vrditest.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vrditest.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vrditest.com -
	-s vrditest.c -
	-i vrditest.imake -
	-p vrditest.pdf -
	-t tstadage1024.pdf tstadage1024.scr tstadage512.pdf tstadage512.scr -
	   tstadage640.pdf tstadage640.scr tstadagespec.pdf tstadagespec.scr -
	   tstcursor.pdf tstcursor.scr tstdeanza.pdf tstdeanza.scr -
	   tstdeanzahi.pdf tstdeanzahi.scr tstflag.pdf tstflag.scr -
	   tstivas.pdf tstivas.scr tstoverlay.pdf tstoverlay.scr -
	   tstramtek.pdf tstramtek.scr tstramtex.pdf tstramtex.scr -
	   tsttek2d.pdf tsttek2d.scr tstxwa.pdf tstxwa.scr
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vrditest.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c"

#include "xderrors.h"
#include "errdefs.h"

#define X_SHIFT 30
#define Y_SHIFT 30
#define HALF_MEG  500000 

int nvalues,def;

unsigned char *inaddr[6], *line_ptr[6];
int linesize[6], recsize[6], imgsize[6], numlines[6], inunit[6];
int num_images=0;
int image;
char image_name[6][256];

int vstatus;
int unit[50], devno;
char message[80];

int pseudo_luts[6][256];
int readaw_size[6];
unsigned char *readaw_ptr[6];
int readline_size[6], numlines_read[6];
unsigned char *readline_ptr[6];
int can_points =  11;
int can_count =  0;
int canned_x[] = {60,72,117,82,97,60,22,37,7,47,60};
int canned_y[] = {0,37,37,62,107,80,107,62,37,37,0};
int red[256], green[256], blue[256];

int info[80];
struct color_struct {
  char color[8];
  int red;
  int green;
  int blue;
} color_chart[18] = {
  	"black",	0,0,0,
  	"red",		255,0,0,   
  	"green",	0,255,0,   
  	"blue",		0,0,255,   
  	"orange",	255,128,0,
  	"yellow",	255,255,130,   
  	"magenta",	255,0,255,  
  	"cyan",		0,255,255,   
  	"purple",	100,0,150,
  	"brown",	150,100,50,   
  	"pink",		255,80,120,   
  	"ltgreen",	120,255,120,   
  	"ltblue",	100,140,255,
  	"violet",	160,80,255,   
  	"grey",		128,128,128,   
  	"white",	255,255,255,
  	"aqua",		0,255,200,   
  	"melon",	255,150,120
};
struct pseudo_struct {
  int red;
  int green;
  int blue;
} pseudo_color[8] = {
  	0,0,255,
  	0,128,255,
  	0,255,255,
  	0,255,0,
  	84,200,0,
  	255,255,0,
  	255,128,0,
  	255,0,0
};
char info_desc[82][55] =
{ "Device type",
  "Device active  (0-no,1-yes)",
  "Number of LUTs",
  "Number of image memory planes",
  "Number of lines in image memory plane (IMP)",
  "Number of samples per line",
  "Hardware configurations available (hex)",
  "Reserved",
  "Reserved",
  "Current color configuration value ",
  "Current IMP size configuration value ",
  "Current Video Output configuration value ",
  "Current Aspect Ratio configuration value ",
  "Video Lines",
  "Video Samples",
  "Reserved",
  "Reserved",
  "Reserved",
  "Reserved",
  "Each IMP has a separate display window (0-no,1-yes)",
  "Reserved",
  "Reserved",
  "Reserved",
  "Number of sections in image LUTs",
  "LUT may be bypassed  (0-no,1-yes)",
  "Largest value in LUT",
  "Each IMP has a separate zoom (0-no,1-yes)",
  "May connect IMP to LUT (0-no,1-yes)",
  "May Zoom IMPs (0-no,1-yes)",
  "Display device has graphics overlay  (0-no,1-yes)",
  "Display graphics overlay on/off (0-off,1-on)",
  "IMPs can be connected to overlay LUT  (0-no,1-yes)",
  "Graphics overlay LUT may be bypassed (0-no,1-yes)",
  "IMP currently connected to graphics overlay LUT",
  "Graphics LUT section used (0-bypass,>0-LUT section)",
  "Graphics overlay LUT characteristics code",
  "Number of sections in graphics LUT",
  "Maximum Zoom factor",
  "Number of bits in graphics plane",
  "Graphics overlay LUT is bypassed (0-no,1-yes)",
  "Display device has an AFG  (0-no,1-yes)",
  "AFG on/off  (0-off,1-on)",
  "Number of lines of text in AFG",
  "Number of characters per line in AFG",
  "Number of AFG display types",
  "Reserved",
  "Reserved",
  "Number of cursors",
  "Number of cursor types",
  "Number of cursor blink rates",
  "Auto Track available (0-no,1-yes)",
  "Auto Track Device",
  "Auto Track Cursor",
  "Reserved",
  "Device has Processor (0-no,1-yes)",
  "Reserved",
  "Reserved",
  "Reserved",
  "Reserved",
  "Number of interactive IO devices",
  "IO device 1 - device type",
  "IO device 1 - coordinates returned",
  "IO device 1 - pen simulation",
  "IO device 1 - number of switches",
  "Reserved",
  "Reserved",
  "Reserved",
  "Reserved",
  "Reserved",
  "IO device 2 - device type",
  "IO device 2 - coordinates returned",
  "IO device 2 - pen simulation",
  "IO device 2 - number of switches",
  "Reserved",
  "Reserved",
  "Reserved",
  "Reserved",
  "Reserved",
  "Reserved",
  "Reserved" };

unsigned char *malloc();

main44()
{
  char cur_cmd[15];
  unsigned char *reserve;

  reserve = 0;
  reserve = malloc( HALF_MEG );
  if (reserve == 0) {
    zvmessage( "Couldn't allocate enough reserve memory", "VRDITEST-NOMEM" );
  }
  else {
    free( reserve );
    load_images();
    init_canned_luts();
    for (;strcmp(cur_cmd,"EXIT");) {
      zvintract("-", "VRDI");
      zvip("_SUBCMD",cur_cmd,&nvalues);

      if     (!strcmp(cur_cmd,"xddactivate"))     dactivate();
      else if(!strcmp(cur_cmd,"xddallocate"))     dallocate();
      else if(!strcmp(cur_cmd,"xddbatch"))        dbatch();
      else if(!strcmp(cur_cmd,"xddclose"))        dclose();
      else if(!strcmp(cur_cmd,"xddconfigure"))    dconfigure();
      else if(!strcmp(cur_cmd,"xddfree"))         dfree();
      else if(!strcmp(cur_cmd,"xddinfo"))         dinfo();
      else if(!strcmp(cur_cmd,"xddopen"))         dopen();
      else if(!strcmp(cur_cmd,"xddsmartopen"))    dsmartopen();
      else if(!strcmp(cur_cmd,"xddname"))         dname();
      else if(!strcmp(cur_cmd,"xddunit"))         dunit();
      else if(!strcmp(cur_cmd,"xdddnamedunit"))   dnamedunit();
      else if(!strcmp(cur_cmd,"xdddunitnames"))   dunitnames();
      else if(!strcmp(cur_cmd,"loadimages"))      load_images();

      else if(!strcmp(cur_cmd,"xdlconnect"))      lconnect();
      else if(!strcmp(cur_cmd,"xdlramp"))         lramp();
      else if(!strcmp(cur_cmd,"xdlread"))         lread();
      else if(!strcmp(cur_cmd,"xdlwrite"))        lwrite();

      else if(!strcmp(cur_cmd,"xdiawlocation"))   iawlocation();
      else if(!strcmp(cur_cmd,"xdiawread"))       iawread();
      else if(!strcmp(cur_cmd,"xdiawset"))        iawset();
      else if(!strcmp(cur_cmd,"xdiawwrite"))      iawwrite();

      else if(!strcmp(cur_cmd,"xdilineread"))     ilineread();
      else if(!strcmp(cur_cmd,"xdilinewrite"))    ilinewrite();
      else if(!strcmp(cur_cmd,"xdipixelread"))    ipixelread();
      else if(!strcmp(cur_cmd,"xdipixelwrite"))   ipixelwrite();

      else if(!strcmp(cur_cmd,"xdidwlocation"))   idwlocation();
      else if(!strcmp(cur_cmd,"xdidwset"))        idwset();

      else if(!strcmp(cur_cmd,"xdicircle"))       icircle();
      else if(!strcmp(cur_cmd,"xdifill"))         ifill();
      else if(!strcmp(cur_cmd,"xdipolyline"))     ipolyline();
      else if(!strcmp(cur_cmd,"xdizoom"))         izoom();

      else if(!strcmp(cur_cmd,"xdcautotrack"))    cautotrack();
      else if(!strcmp(cur_cmd,"xdclocation"))     clocation();
      else if(!strcmp(cur_cmd,"xdcoff"))          coff();
      else if(!strcmp(cur_cmd,"xdcon"))           con();
      else if(!strcmp(cur_cmd,"xdcset"))          cset();
      else if(!strcmp(cur_cmd,"xdcshow"))         cshow();
      else if(!strcmp(cur_cmd,"xdcsize"))         csize();
      else if(!strcmp(cur_cmd,"xdccolor"))        ccolor();

      else if(!strcmp(cur_cmd,"xdcilocation"))    cilocation();
      else if(!strcmp(cur_cmd,"xdciset"))         ciset();
      else if(!strcmp(cur_cmd,"xdcimp2raw"))      cimp2raw();
      else if(!strcmp(cur_cmd,"xdcraw2imp"))      craw2imp();

      else if(!strcmp(cur_cmd,"xdgconnect"))      gconnect();
      else if(!strcmp(cur_cmd,"xdglconstant"))    glconstant();
      else if(!strcmp(cur_cmd,"xdglread"))        glread();
      else if(!strcmp(cur_cmd,"xdglwrite"))       glwrite();
      else if(!strcmp(cur_cmd,"xdgoff"))          goff();
      else if(!strcmp(cur_cmd,"xdgon"))           gon();
      else if(!strcmp(cur_cmd,"xdglinit"))        glinit();
      else if(!strcmp(cur_cmd,"xdgcolor"))        gcolor();
      else if(!strcmp(cur_cmd,"xdgrgb"))          grgb();

      /*  Extra "if" needed because of parse stack overflow error in  */
      /*  compiling.                                                  */

      if     (!strcmp(cur_cmd,"xdaclear"))        aclear();
      else if(!strcmp(cur_cmd,"xdaoff"))          aoff();
      else if(!strcmp(cur_cmd,"xdaon"))           aon();
      else if(!strcmp(cur_cmd,"xdatext"))         atext();

      else if(!strcmp(cur_cmd,"xdx1d"))           x1d();
      else if(!strcmp(cur_cmd,"xdx2d"))           x2d();
      else if(!strcmp(cur_cmd,"xdx3d"))           x3d();
      else if(!strcmp(cur_cmd,"xdxswitch"))       xswitch();

      else if(!strcmp(cur_cmd,"xdtcolor"))        tcolor();
      else if(!strcmp(cur_cmd,"xdtfont"))         tfont();
      else if(!strcmp(cur_cmd,"xdtlength"))       tlength();
      else if(!strcmp(cur_cmd,"xdtmask"))         tmask();
      else if(!strcmp(cur_cmd,"xdtrotate"))       trotate();
      else if(!strcmp(cur_cmd,"xdtsize"))         tsize();
      else if(!strcmp(cur_cmd,"xdttext"))         ttext();

      else if(!strcmp(cur_cmd,"xdeaction"))       eaction();
      else if(!strcmp(cur_cmd,"xdelevel"))        elevel();
      else if(!strcmp(cur_cmd,"xdesignal"))       esignal();

      else if(!strcmp(cur_cmd,"xdimawwrite"))     imawwrite();
      else if(!strcmp(cur_cmd,"xdimcircle"))      imcircle();
      else if(!strcmp(cur_cmd,"xdimfill"))        imfill();
      else if(!strcmp(cur_cmd,"xdimlinewrite"))   imlinewrite();
      else if(!strcmp(cur_cmd,"xdimpixelwrite"))  impixelwrite();
      else if(!strcmp(cur_cmd,"xdimpolyline"))    impolyline();

      else if(!strcmp(cur_cmd,"xdiiarithmetic"))  iiarithmetic();
      else if(!strcmp(cur_cmd,"xdiicopy"))        iicopy();
      else if(!strcmp(cur_cmd,"xdiilogical"))     iilogical();
      else if(!strcmp(cur_cmd,"xdiishift"))       iishift();

      else if(!strcmp(cur_cmd,"xdiareafill"))     iareafill();
      else if(!strcmp(cur_cmd,"xdihistogram"))    ihistogram();
      else if(!strcmp(cur_cmd,"xdirotate"))       irotate();

      else if(!strcmp(cur_cmd,"loopgraph"))       loopgraph();
      else if(!strcmp(cur_cmd,"looploop"))        looploop();

      else if(!strcmp(cur_cmd,"xdsnl"))           snl();
      else if(!strcmp(cur_cmd,"xdsns"))           sns();
      else if(!strcmp(cur_cmd,"xdsvnl"))          svnl();
      else if(!strcmp(cur_cmd,"xdsvns"))          svns();
      else if(!strcmp(cur_cmd,"xdsmode"))         smode();
      else if(!strcmp(cur_cmd,"xdsimp"))          simp();
      else if(!strcmp(cur_cmd,"xdssection"))      ssection();
      else if(!strcmp(cur_cmd,"xdsbypass"))       sbypass();
      else if(!strcmp(cur_cmd,"xdsgraph"))        sgraph();
      else if(!strcmp(cur_cmd,"xdsgsection"))     sgsection();
      else if(!strcmp(cur_cmd,"xdsgbypass"))      sgbypass();
      else if(!strcmp(cur_cmd,"xdszoom"))         szoom();
      else if(!strcmp(cur_cmd,"xdsdwline"))       sdwline();
      else if(!strcmp(cur_cmd,"xdsdwsamp"))       sdwsamp();

      else if(!strcmp(cur_cmd,"xdfregister"))     fregister();
      else if(!strcmp(cur_cmd,"xdfconfig"))       fconfig();
      else if(!strcmp(cur_cmd,"xdflut"))          flut();
      else if(!strcmp(cur_cmd,"xdfglut"))         fglut();
      else if(!strcmp(cur_cmd,"xdfimage"))        fimage();
    }
  }
}  
dactivate()
{
  int act_flag;

  zvip("FLAG",&act_flag,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zddactivate( unit[devno], act_flag );
}

dallocate()
{
  char device[10];

  zvip("DEVICE",device,&nvalues);
  UpperString ( device );
  zddallocate( device );
}

dbatch()
{
  int flag;

  zvip("FLAG",&flag,&nvalues);
  zddbatch( unit[devno], flag );
}

dconfigure()
{
  int config[4];

  zvip("CONFIG",config,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zddconfigure( unit[devno], config );
}

dclose()
{
  zvip("DEVNO",&devno,&nvalues);
  zddclose( unit[devno] );
}

dfree()
{
  char device[10];

  zvip("DEVICE",device,&nvalues);
  UpperString ( device );
  zddfree( device );
}

dinfo()
{
  int i, start, number;

  zvip("START",&start,&nvalues);
  zvip("NUMBER",&number,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zddinfo( unit[devno], start, number, info );

  for (i = 0; i < number; i++ ) {
    if (!strcmp(info_desc[i+start-1],"Reserved")) {
      sprintf( message,"%d %s",i+start,info_desc[i+start-1] );
    } else {
      if ( (i + start) == 7 ) {
	sprintf( message,"%d %s = %x",i+start,info_desc[i+start-1],info[i] );
      } else {
	sprintf( message,"%d %s = %d",i+start,info_desc[i+start-1],info[i] );
      }
    }
    zvmessage( message, "" );
  }
}

dopen()
{
  zvip("DEVNO",&devno,&nvalues);
  zddopen( unit[devno] );
}

dsmartopen()
{
  zvip("DEVNO",&devno,&nvalues);
  zddsmartopen( unit[devno] );
}

dname()
{
  int name_flag, max_chars, ret_length;
  char device_name[30];

  zvip("FLAG",&name_flag,&nvalues);
  zvip("MAXLEN",&max_chars,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zddname( unit[devno], name_flag, device_name, max_chars, &ret_length);
  device_name[ret_length] = '\0';
  sprintf( message, "Device_name = %s, returned length = %d", 
                    device_name, ret_length );
  zvmessage( message, "" );
}

dunit()
{
  zvip("DEVNO",&devno,&nvalues);
  zddunit( &unit[devno] );
}

dnamedunit()
{
  char device[10];

  zvip("DEVICE",device,&nvalues);
  zvip("DEVNO",&devno,&nvalues);
  zddnamedunit( &unit[devno],device );
}

dunitnames()
{
  char device[50][20];
  int ndev, len, i;

  ndev = 50;
  len = 20;
  zddunitnames(&ndev, len, device);

 zvmessage("The devices currently allocated to you are:"), "";
  if (ndev == 0)
    zvmessage("   *** No devices currently allocated ***", "");
  else {
    for (i=0; i<ndev; i++) {
      sprintf(message, "   %s", device[i]);
      zvmessage(message, "");
    }
  }
}

lconnect()
{
  int imp, lut, section, bypass;

  zvip("IMP",&imp,&nvalues);
  zvip("LUT",&lut,&nvalues);
  zvip("SECTION",&section,&nvalues);
  zvip("BYPASS",&bypass,&nvalues);
  zvip("DEVNO",&devno,&nvalues);
  zdlconnect( unit[devno], imp, lut, section, bypass );
}

lramp()
{
  int lut, section;

  zvip("LUT",&lut,&nvalues);
  zvip("SECTION",&section,&nvalues);
  zvip("DEVNO",&devno,&nvalues);
  zdlramp( unit[devno], lut, section );
}
lread()
{
  int i, lut, section;
  int lut_array[256];

  zvip("LUT",&lut,&nvalues);
  zvip("SECTION",&section,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdlread( unit[devno], lut, section, lut_array );
  if (vstatus == 1) {
    for (i = 0; i < 255; i+=3 ) {
      sprintf( message, "Lut_Read[%d] = %d, Lut_Read[%d] = %d, Lut_Read[%d] = %d",
              i, lut_array[i], i+1, lut_array[i+1], i+2, lut_array[i+2] );
      zvmessage( message, "" );
    }
    sprintf( message, "Lut_Read[255] = %d", lut_array[255] );
    zvmessage( message, "" );
  }
}
lwrite()
{
  int canned, i, lut, section;

  zvip("LUT",&lut,&nvalues);
  zvip("SECTION",&section,&nvalues);
  zvip("CANNED",&canned,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdlwrite( unit[devno], lut, section, pseudo_luts[canned] );
}
iawlocation()
{
  int imp, left, top, right, bottom;

  zvip("IMP",&imp,&nvalues);
  zvip("DEVNO",&devno,&nvalues);
  vstatus = zdiawlocation( unit[devno], imp, &left, &top, &right, &bottom );
  if (vstatus == 1) {
    sprintf( message, "Left = %d, Top = %d, Right = %d, Bottom = %d", 
           left, top, right, bottom );
    zvmessage( message, "" );
  }
}

iawread()
{
  int imp, size, outimg, left, top, right, bottom;

  zvip("IMP",&imp,&nvalues);
  zvip("SIZE",&size,&nvalues);
  zvip("OUTIMG",&outimg,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = 1;
  if ( size == 0 ) {
    vstatus = zdiawlocation( unit[devno], imp, &left, &top, &right, &bottom );
    if (vstatus == 1) {
      readaw_size[outimg] = (right - left + 1) * (bottom - top + 1);
    }
  } else {
    readaw_size[outimg] = size;
  }

  if (vstatus == 1) {
    if ( readaw_ptr[outimg] == 0 ) {
      readaw_ptr[outimg] = malloc( readaw_size[outimg] );
      if ( readaw_ptr[outimg] == 0 ) {
	zvmessage( "Error allocating memory", "VRDITEST-NOMEM" );
	return;
      }
    } else {
      free( readaw_ptr[outimg] );
      readaw_ptr[outimg] = malloc( readaw_size[outimg] );
      if ( readaw_ptr[outimg] == 0 ) {
	zvmessage( "Error allocating memory", "VRDITEST-NOMEM" );
	return;
      }
    }
  }
  if (vstatus == 1) {
    zdiawread( unit[devno], imp, readaw_size[outimg], readaw_ptr[outimg] );
  }
}

iawset()
{
  int imp, left, top, right, bottom;

  zvip("IMP",&imp,&nvalues);
  zvip("LEFT",&left,&nvalues);
  zvip("TOP",&top,&nvalues);
  zvip("RIGHT",&right,&nvalues);
  zvip("BOTTOM",&bottom,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdiawset( unit[devno], imp, left, top, right, bottom );
}

iawwrite()
{
  int imp, size, image, outimg;
  char which[6], msg[80];

  zvip("IMP",&imp,&nvalues);
  zvip("SIZE",&size,&nvalues);
  zvip("IMAGE",&image,&nvalues);
  zvip("OUTIMG",&outimg,&nvalues);
  zvip("WHICH",which,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  if (!strcmp( which, "IMAGE" )) {
    if (image < num_images ) {
      if (size == 0 ) size = imgsize[image];
      zdiawwrite( unit[devno], imp, size, inaddr[image] );
    } else {
      zvmessage( "No such image input", "VRDITEST-NOIMAGE" );
    }
  } else {
    if ( readaw_ptr[outimg] == 0 ) {
	zvmessage( "No such image output from XDIAWREAD", "VRDITEST-NOIMGOUT");
    } else {
      if (size == 0 ) size = readaw_size[outimg];
      zdiawwrite( unit[devno], imp, size, readaw_ptr[outimg] );
    }
  }
}

ilineread()
{
  int i, imp, sstart, lstart, size, nlines, image, outline;
  int left, top, right, bottom;
  unsigned char *local_ptr;

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("SIZE",&size,&nvalues);
  zvip("NLINES",&nlines,&nvalues);
  zvip("OUTLINE",&outline,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdiawlocation( unit[devno], imp, &left, &top, &right, &bottom );

  if (vstatus == 1) {
    if ( sstart == 0 ) sstart = left;
    if ( lstart == 0 ) lstart = top;

    if ( size == 0 ) {
      readline_size[outline] = (right - left + 1);
    } else {
      readline_size[outline] = size;
    }
    if ( nlines == 0 ) {
      numlines_read[outline] = (bottom - top + 1);
    } else {
      numlines_read[outline] = nlines;
    }

    if ( readline_ptr[outline] == 0 ) {
      readline_ptr[outline] = 
               malloc( readline_size[outline] * numlines_read[outline] );
      if ( readline_ptr[outline] == 0 ) {
	zvmessage( "Error allocating memory", "VRDITEST-NOMEM");
	return;
      }
    } else {
      free( readline_ptr[outline] );
      readline_ptr[outline] = 
               malloc( readline_size[outline] * numlines_read[outline] );
      if ( readline_ptr[outline] == 0 ) {
	zvmessage( "Error allocating memory", "VRDITEST-NOMEM");
	return;
      }
    }
    local_ptr = readline_ptr[outline];
    for (i = lstart; i < ( numlines_read[outline] + lstart); i++ ) {
      zdilineread(unit[devno],imp,sstart,i,readline_size[outline], local_ptr);
      local_ptr += readline_size[outline];
    }
  }
}

ilinewrite()
{
  int i, imp, sstart, lstart, size, nlines, image, outline;
  unsigned char *temp_ptr;
  char which[6];
  int left, top, right, bottom;

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("SIZE",&size,&nvalues);
  zvip("NLINES",&nlines,&nvalues);
  zvip("IMAGE",&image,&nvalues);
  zvip("OUTLINE",&outline,&nvalues);
  zvip("WHICH",which,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdiawlocation( unit[devno], imp, &left, &top, &right, &bottom );

  if (vstatus == 1) {
    if ( sstart == 0 ) sstart = left;
    if ( lstart == 0 ) lstart = top;

    if (!strcmp( which, "IMAGE" )) {
      if ( image < num_images ) {
	temp_ptr = line_ptr[image];
	if (size == 0 ) size = recsize[image];
	if (nlines == 0 ) nlines = numlines[image];
	if ( size > recsize[image] ) size = recsize[image];
	if ( nlines > numlines[image] ) nlines = numlines[image];
	for (i = lstart; i < (nlines + lstart); i++ ) {
	  zdilinewrite(unit[devno],imp,sstart,i,size,temp_ptr);
	  temp_ptr += linesize[image];
	}
      } else {
	zvmessage( "No such image input", "VRDITEST-NOIMAGE" );
      }
    } 
    else {
      if ( readline_ptr[outline] == 0 ) {
	zvmessage("No such image output from XDILINEREAD","VRDITEST-NOIMGOUT");
      } 
      else {
	temp_ptr = readline_ptr[outline];
	if (size == 0 ) size = readline_size[outline];
	if (nlines == 0 ) nlines = numlines_read[outline];
	if ( nlines > numlines_read[outline] ) nlines = numlines_read[outline];
	if ( size > readline_size[outline] ) size = readline_size[outline];
	for (i = lstart; i < (nlines + lstart); i++ ) {
	  zdilinewrite(unit[devno],imp,sstart,i,size,temp_ptr);
	  temp_ptr += readline_size[outline];
	}
      }
    }
  }
}

ipixelread()
{
  int imp, sstart, lstart, value;
  unsigned char bvalue;

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdipixelread(unit[devno],imp,sstart,lstart,&bvalue );
  if (vstatus == 1 ) {
    value = bvalue;
    sprintf( message,"Value = %d", value );
    zvmessage( message, "" );
  }
}

ipixelwrite()
{
  int imp, sstart, lstart, value;
  unsigned char bvalue;

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("VALUE",&value,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  bvalue = value;
  zdipixelwrite(unit[devno],imp,sstart,lstart,bvalue );
}

idwlocation()
{
  int imp, left, top;

  zvip("IMP",&imp,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdidwlocation( unit[devno], imp, &left, &top );

  if ( vstatus == 1 ) {
    sprintf( message, "Left = %d, Top = %d", left, top );
    zvmessage( message, "" );
  }
}

idwset()
{
  int imp, left, top;

  zvip("IMP",&imp,&nvalues);
  zvip("LEFT",&left,&nvalues);
  zvip("TOP",&top,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdidwset( unit[devno], imp, left, top );
}

icircle()
{
  int imp, sstart, lstart, radius, value;

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("RADIUS",&radius,&nvalues);
  zvip("VALUE",&value,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdicircle( unit[devno], imp, sstart, lstart, radius, value );
}

ifill()
{
  int imp, value;

  zvip("IMP",&imp,&nvalues);
  zvip("VALUE",&value,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdifill( unit[devno], imp, value );
}

ipolyline()
{
  int i, imp, value, npts, xarray[2], yarray[2];
  char canned[5];

  for (i=0; i<5; i++)
    canned[i] = ' ';
  zvip("IMP",&imp,&nvalues);
  zvip("VALUE",&value,&nvalues);
  zvip("NPTS",&npts,&nvalues);
  zvip("X",xarray,&nvalues);
  zvip("Y",yarray,&nvalues);
  zvip("CANNED",canned,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  if (!strcmp( canned, "0" )) {
    zdipolyline( unit[devno], imp, value, npts, xarray, yarray );
  } else {
    for  (i = 0; i < can_points; i++) {
      canned_x[i] += (X_SHIFT + can_count * 20);
      canned_y[i] += (Y_SHIFT + can_count * 20);
    }
    can_count += 1;
    zdipolyline( unit[devno], imp, value, can_points, canned_x, canned_y);
  }
}

izoom()
{
  int imp, zoom;

  zvip("IMP",&imp,&nvalues);
  zvip("ZOOM",&zoom,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdizoom( unit[devno], imp, zoom );
}

cautotrack()
{
  int cursor, device, flag;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("DEVICE",&device,&nvalues);
  zvip("FLAG",&flag,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdcautotrack( unit[devno], cursor, device, flag );
}

clocation()
{
  int cursor, cur_x, cur_y;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdclocation( unit[devno], cursor, &cur_x, &cur_y );

  if ( vstatus == 1 ) {
    sprintf( message, "Cursor X = %d, Cursor Y = %d", cur_x, cur_y );
    zvmessage( message, "" );
  }
}

coff()
{
  int cursor;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdcoff( unit[devno], cursor );
}

con()
{
  int cursor, cform, cblink;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("FORM",&cform,&nvalues);
  zvip("BLINK",&cblink,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdcon( unit[devno], cursor, cform, cblink );
}

cset()
{
  int cursor, cur_x, cur_y;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("X",&cur_x,&nvalues);
  zvip("Y",&cur_y,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdcset( unit[devno], cursor, cur_x, cur_y );
}

cshow()
{
  int cursor;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdcshow( unit[devno], cursor );
}

csize()
{
  int cursor, xsize, ysize;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("XSIZE",&xsize,&nvalues);
  zvip("YSIZE",&ysize,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdcsize( unit[devno], cursor, xsize, ysize );
}

ccolor()
{
  int cursor, red, green, blue;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("RED",&red,&nvalues);
  zvip("GREEN",&green,&nvalues);
  zvip("BLUE",&blue,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdccolor( unit[devno], cursor, red, green, blue );
}

cilocation()
{
  int cursor, cur_x, cur_y, imp;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("IMP",&imp,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdcilocation( unit[devno], cursor, &cur_x, &cur_y, imp );

  if ( vstatus == 1 ) {
    sprintf( message, "Cursor X = %d, Cursor Y = %d", cur_x, cur_y );
    zvmessage( message, "" );
  }
}

ciset()
{
  int cursor, cur_x, cur_y, imp;

  zvip("CURSOR",&cursor,&nvalues);
  zvip("X",&cur_x,&nvalues);
  zvip("Y",&cur_y,&nvalues);
  zvip("IMP",&imp,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdciset( unit[devno], cursor, cur_x, cur_y, imp );
}

cimp2raw()
{
  int imp, ximp, yimp, xraw, yraw;

  zvip("IMP",&imp,&nvalues);
  zvip("XIMP",&ximp,&nvalues);
  zvip("YIMP",&yimp,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdcimp2raw( unit[devno], imp, ximp, yimp, &xraw, &yraw );

  if ( vstatus == 1 ) {
    sprintf( message, "Screen X = %d, Screen Y = %d", xraw, yraw );
    zvmessage( message, "" );
  }
}

craw2imp()
{
  int imp, xraw, yraw, ximp, yimp;

  zvip("IMP",&imp,&nvalues);
  zvip("XRAW",&xraw,&nvalues);
  zvip("YRAW",&yraw,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdcraw2imp( unit[devno], imp, xraw, yraw, &ximp, &yimp );

  if ( vstatus == 1 ) {
    sprintf( message, "Imp X = %d, Imp Y = %d", ximp, yimp );
    zvmessage( message, "" );
  }
}

snl()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsnl( unit[devno] );

  sprintf( message, "Number of lines in image plane = %d", vstatus );
  zvmessage( message, "" );
}

sns()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsns( unit[devno] );

  sprintf( message, "Number of samples in image plane = %d", vstatus );
  zvmessage( message, "" );
}

svnl()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsvnl( unit[devno] );

  sprintf( message, "Number of lines on video screen = %d", vstatus );
  zvmessage( message, "" );
}

svns()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsvns( unit[devno] );

  sprintf( message, "Number of samples on video screen = %d", vstatus );
  zvmessage( message, "" );
}

smode()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsmode( unit[devno] );

  if (vstatus == 1)
    zvmessage( "Video Display Mode = Color", "" );
  else if (vstatus == 2)  
    zvmessage( "Video Display Mode = Pseudo-Color", "" );
  else if (vstatus == 3)  
    zvmessage( "Video Display Mode = Monochrome", "" );
}

simp()
{
  int lut;

  zvip("LUT",&lut,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsimp( unit[devno], lut );

  sprintf( message, "Image plane %d connected to LUT %d", vstatus, lut );
  zvmessage( message, "" );
}

ssection()
{
  int lut;

  zvip("LUT",&lut,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdssection( unit[devno], lut );

  sprintf( message, "Section %d of LUT %d currently in use", vstatus, lut );
  zvmessage( message, "" );
}

sbypass()
{
  int lut;

  zvip("LUT",&lut,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsbypass( unit[devno], lut );

  if ( vstatus == 0 )
    sprintf( message, "LUT %d bypass is off (LUT in use)", lut );
  else
    sprintf( message, "LUT %d bypass is on (LUT not in use)", lut );
  zvmessage( message, "" );
}

sgraph()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsgraph( unit[devno] );

  sprintf( message, "Image plane %d connected to graphics plane", vstatus );
  zvmessage( message, "" );
}

sgsection()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsgsection( unit[devno] );

  sprintf( message, "Section %d of graphics LUT currently in use", vstatus );
  zvmessage( message, "" );
}

sgbypass()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsgbypass( unit[devno] );

  if ( vstatus == 0 )
    zvmessage( "Graphics LUT bypass is off (LUT in use)", "" );
  else
    zvmessage( "Graphics LUT bypass is on (LUT not in use)", "" );
}

szoom()
{
  int imp;

  zvip("IMP",&imp,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdszoom( unit[devno], imp );

  sprintf( message, "Zoom factor for image plane %d = %d", imp, vstatus );
  zvmessage( message, "" );
}

sdwline()
{
  int imp;

  zvip("IMP",&imp,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsdwline( unit[devno], imp );

  sprintf( message, "Upper left line of display window for IMP %d = %d",
           imp, vstatus );
  zvmessage( message, "" );
}

sdwsamp()
{
  int imp;

  zvip("IMP",&imp,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdsdwsamp( unit[devno], imp );

  sprintf( message, "Upper left sample of display window for IMP %d = %d",
           imp, vstatus );
  zvmessage( message, "" );
}

fregister()
{
  int group;

  zvip("GROUP",&group,&nvalues);

  zdfregister( group );
}

fconfig()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdfconfig( unit[devno] );

  if ( vstatus == 0 )
    zvmessage( "Device configuration has not changed.", "" );
  else
    zvmessage( "Device configuration has changed.", "" );
}

flut()
{
  int lut;

  zvip("LUT",&lut,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdflut( unit[devno], lut );

  if ( vstatus == 0 )
    sprintf( message, "LUT %d has not changed.", lut );
  else
    sprintf( message, "LUT %d has changed.", lut );
  zvmessage( message, "" );
}

fglut()
{
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdfglut( unit[devno] );

  if ( vstatus == 0 )
    zvmessage( "Graphics LUT has not changed.", "" );
  else
    zvmessage( "Graphics LUT has changed.", "" );
}

fimage()
{
  int imp;

  zvip("IMP",&imp,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdfimage( unit[devno], imp );

  if ( vstatus == 0 )
    sprintf( message, "IMP %d has not changed.", imp );
  else
    sprintf( message, "IMP %d has changed.", imp );
  zvmessage( message, "" );
}

gconnect()
{
  int imp, section, bypass;

  zvip("IMP",&imp,&nvalues);
  zvip("SECTION",&section,&nvalues);
  zvip("BYPASS",&bypass,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdgconnect( unit[devno], imp, section, bypass );
}

glconstant()
{
  int imp, section, red_value, green_value, blue_value;

  zvip("SECTION",&section,&nvalues);
  zvip("RED",&red_value,&nvalues);
  zvip("GREEN",&green_value,&nvalues);
  zvip("BLUE",&blue_value,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdglconstant( unit[devno], section, red_value, green_value, blue_value );
}

glread()
{
  int i, section;
  int red_array[256], green_array[256], blue_array[256];

  zvip("SECTION",&section,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdglread(unit[devno],section,red_array,green_array,blue_array);
  if ( vstatus == 1 ) {
    for (i = 0; i < 16; i++ ) {
      sprintf(message,"Red_Lut[%d] = %d, Green_Lut[%d] = %d, Blue_Lut[%d] = %d",
              i, red_array[i], i, green_array[i], i, blue_array[i] );
      zvmessage( message, "" );
    }
  }
}

glwrite()
{
  int section;

  zvip("SECTION",&section,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdglwrite( unit[devno], section, red, green, blue );
}

goff()
{
  zvip("DEVNO",&devno,&nvalues);

  zdgoff( unit[devno] );
}

gon()
{
  zvip("DEVNO",&devno,&nvalues);
  zdgon( unit[devno] );
}

glinit()
{
  int section;

  zvip("SECTION",&section,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdglinit( unit[devno], section );
}

gcolor()
{
  char  colorstring[80];

  zvip("TEXT",colorstring,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdgcolor( unit[devno], colorstring );

  sprintf( message, "DN value for %s = %d", colorstring, vstatus );
  zvmessage( message, "" );
}

grgb()
{
  int red, green, blue;

  zvip("RED",&red,&nvalues);
  zvip("GREEN",&green,&nvalues);
  zvip("BLUE",&blue,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdgrgb( unit[devno], red, green, blue );

  sprintf( message, "DN value = %d", vstatus );
  zvmessage( message, "" );
}

aclear()
{
  int sstart, lstart, nchars;

  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("NCHARS",&nchars,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdaclear( unit[devno], sstart, lstart, nchars );
}

aoff()
{
  zvip("DEVNO",&devno,&nvalues);

  zdaoff( unit[devno] );
}

aon()
{
  zvip("DEVNO",&devno,&nvalues);

  zdaon( unit[devno] );
}

atext()
{
  int sstart, lstart, length, blink, reverse;
  char text[80];

  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("LENGTH",&length,&nvalues);
  zvip("TEXT",text,&nvalues);
  zvip("BLINK",&blink,&nvalues);
  zvip("REVERSE",&reverse,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  if ( length > 80 ) length = 80;

  zdatext( unit[devno], sstart, lstart, length, text, blink, reverse );
}

x1d()
{
  int device, knob;
  float value;

  zvip("DEVICE",&device,&nvalues);
  zvip("KNOB",&knob,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdx1d( unit[devno], device, knob, &value );

  if ( vstatus == 1 ) {
    sprintf( message,"Value = %f", value );
    zvmessage( message, "" );
  }
}

x2d()
{
  int device, prox, pen;
  float sstart, lstart;

  zvip("DEVICE",&device,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = 1;
  for (pen = 0; (vstatus == 1 && pen == 0); ) {
    vstatus = zdx2d( unit[devno], device, &sstart, &lstart, &prox, &pen );
  }
  if ( vstatus == 1 ) {
    sprintf( message,"X = %f, Y = %f, Prox = %d, Pen = %d", 
                   sstart, lstart, prox, pen );
    zvmessage( message, "" );
  }
}

x3d()
{
  int device, prox, pen;
  float sstart, lstart, zstart;

  zvip("DEVICE",&device,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdx3d(unit[devno], device, &sstart, &lstart, &zstart, &prox, &pen);

  if ( vstatus == 1 ) {
    sprintf( message, "X = %f, Y = %f, Z = %f, Prox = %d, Pen = %d", 
                   sstart, lstart, zstart, prox, pen );
    zvmessage( message, "" );
  }
}

xswitch()
{
  int device, xswitch, value;

  zvip("DEVICE",&device,&nvalues);
  zvip("SWITCH",&xswitch,&nvalues);
  zvip("DEVNO",&devno,&nvalues);
  
  vstatus = 1;
  for ( value = 0; (vstatus == 1 && value == 0); ) {
    vstatus = zdxswitch( unit[devno], device, xswitch, &value );
  }
  if ( vstatus == 1 ) {
    sprintf( message,"Value = %d", value );
    zvmessage( message, "" );
  }
}

tcolor()
{
  int color, prec;

  zvip("COLOR",&color,&nvalues);
  zvip("PREC",&prec,&nvalues);

  zdtcolor( color, prec );
}

tfont()
{
  int font;

  zvip("FONT",&font,&nvalues);

  zdtfont( font );
}

tlength()
{
  int length, nchars;
  char text[80];

  zvip("NCHARS",&nchars,&nvalues);
  zvip("TEXT",text,&nvalues);

  zdtlength( &length, nchars, text );

  sprintf( message,"Length = %d", length );
  zvmessage( message, "" );
}

tmask()
{
  int mask;

  zvip("MASK",&mask,&nvalues);

  zdtmask( mask );
}

trotate()
{
  float angle;

  zvip("ANGLE",&angle,&nvalues);

  zdtrotate( angle );
}

tsize()
{
  int height;
  float scale;

  zvip("HEIGHT",&height,&nvalues);
  zvip("SCALE",&scale,&nvalues);

  zdtsize( height, scale );
}

ttext()
{
  int imp, sstart, lstart, loc, nchars;
  char text[80];

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("LOC",&loc,&nvalues);
  zvip("NCHARS",&nchars,&nvalues);
  zvip("TEXT",text,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  if ( nchars > 80 ) nchars = 80;

  zdttext( unit[devno], imp, sstart, lstart, loc, nchars, text );
}

eaction()
{
  int warn, error, fatal;

  zvip("WARN",&warn,&nvalues);
  zvip("ERROR",&error,&nvalues);
  zvip("FATAL",&fatal,&nvalues);

  zdeaction( warn, error, fatal );
}

elevel()
{
  int code;

  zvip("CODE",&code,&nvalues);

  vstatus = zdelevel( code );
  if (vstatus == XD_UNKNOWN)
    sprintf(message, "Invalid error code");
  else if (vstatus == XD_NO_ERROR)
    sprintf(message, "Error level == NO ERROR");
  else if (vstatus == XD_WARN)
    sprintf(message, "Error level == WARNING");
  else if (vstatus == XD_ERROR)
    sprintf(message, "Error level == ERROR");
  else if (vstatus == XD_FATAL)
    sprintf(message, "Error level == FATAL");
  zvmessage( message, "" );
}

esignal()
{
  int code;

  zvip("CODE",&code,&nvalues);

  zdesignal( code );
}

imawwrite()
{
  int imp, size, mask, image, outimg;
  char which[6];
  unsigned char bmask;

  zvip("IMP",&imp,&nvalues);
  zvip("MASK",&mask,&nvalues);
  zvip("SIZE",&size,&nvalues);
  zvip("IMAGE",&image,&nvalues);
  zvip("OUTIMG",&outimg,&nvalues);
  zvip("WHICH",which,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  bmask = mask;
  if (!strcmp( which, "IMAGE" )) {
    if (image < num_images ) {
      if (size == 0 ) size = imgsize[image];
      zdimawwrite( unit[devno], imp, bmask, size, inaddr[image] );
    } else {
      zvmessage( "No such image input", "VRDITEST-NOIMAGE" );
    }
  } else {
    if ( readaw_ptr[outimg] == 0 ) {
	zvmessage("No such image output from XDIAWREAD", "VRDITEST-NOIMGOUT");
    } else {
      if (size == 0 ) size = readaw_size[outimg];
      zdimawwrite( unit[devno], imp, bmask, size, readaw_ptr[outimg] );
    }
  }
}

imcircle()
{
  int imp, sstart, lstart, radius, mask, value;
  unsigned char bmask;

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("RADIUS",&radius,&nvalues);
  zvip("MASK",&mask,&nvalues);
  zvip("VALUE",&value,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  bmask = mask;
  zdimcircle(unit[devno], imp, sstart, lstart, radius, bmask, value );
}

imfill()
{
  int imp, mask, value;
  unsigned char bmask;

  zvip("IMP",&imp,&nvalues);
  zvip("MASK",&mask,&nvalues);
  zvip("VALUE",&value,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  bmask = mask;
  zdimfill( unit[devno], imp, bmask, value );
}

imlinewrite()
{
  int i, imp, sstart, lstart, size, nlines, image, mask, outline;
  unsigned char *temp_ptr;
  char which[6];
  unsigned char bmask;
  int left, top, right, bottom;

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("MASK",&mask,&nvalues);
  zvip("SIZE",&size,&nvalues);
  zvip("NLINES",&nlines,&nvalues);
  zvip("IMAGE",&image,&nvalues);
  zvip("OUTLINE",&outline,&nvalues);
  zvip("WHICH",which,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdiawlocation( unit[devno], imp, &left, &top, &right, &bottom );

  if (vstatus == 1) {
    if ( sstart == 0 ) sstart = left;
    if ( lstart == 0 ) lstart = top;

    bmask = mask;
    if (!strcmp( which, "IMAGE" )) {
      if (image < num_images ) {
	temp_ptr = line_ptr[image];
	if (size == 0 ) size = recsize[image];
	if (nlines == 0 ) nlines = numlines[image];
	if ( size > recsize[image] ) size = recsize[image];
	if ( nlines > numlines[image] ) nlines = numlines[image];
	for (i = lstart; i < (nlines + lstart); i++ ) {
	  zdimlinewrite(unit[devno], imp, sstart, i, bmask, size, temp_ptr);
	  temp_ptr += linesize[image];
	}
      } else {
	zvmessage( "No such image input", "VRDITEST-NOIMAGE" );
      }
    } 
    else {
      if ( readline_ptr[outline] == 0 ) {
	zvmessage("No such image output from XDILINEREAD", "VRDITEST-NOIMGOUT");
      } 
      else {
	temp_ptr = readline_ptr[outline];
	if (size == 0 ) size = readline_size[outline];
	if (nlines == 0 ) nlines = numlines_read[outline];
	if ( nlines > numlines_read[outline] ) nlines = numlines_read[outline];
	if ( size > readline_size[outline] ) size = readline_size[outline];
	for (i = lstart; i < (nlines + lstart); i++ ) {
	  zdimlinewrite(unit[devno], imp, sstart, i, bmask, size, temp_ptr);
	  temp_ptr += readline_size[outline];
	}
      }
    }
  }
}

impixelwrite()
{
  int imp, sstart, lstart, mask, value;
  unsigned char bvalue, bmask;

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("MASK",&mask,&nvalues);
  zvip("VALUE",&value,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  bvalue = value;
  bmask = mask;
  zdimpixelwrite(unit[devno],imp,sstart,lstart,bmask,bvalue);
}

impolyline()
{
  int i, imp, value, npts, xarray[2], yarray[2], mask;
  char canned[5];
  unsigned char bmask;

  zvip("IMP",&imp,&nvalues);
  zvip("MASK",&mask,&nvalues);
  zvip("VALUE",&value,&nvalues);
  zvip("NPTS",&npts,&nvalues);
  zvip("X",xarray,&nvalues);
  zvip("Y",yarray,&nvalues);
  zvip("CANNED",canned,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  bmask = mask;
  if (!strcmp( canned, "0" )) {
    zdimpolyline( unit[devno], imp, bmask, value, npts, xarray, yarray );
  } else {
    for  (i = 0; i < can_points; i++) {
      canned_x[i] += (X_SHIFT + can_count * 20);
      canned_y[i] += (Y_SHIFT + can_count * 20);
    }
    can_count += 1;
    zdimpolyline(unit[devno], imp, bmask, value, can_points,canned_x,canned_y);
  }
}

iiarithmetic()
{
  int op, imp1, imp2, imp3;

  zvip("OP",&op,&nvalues);
  zvip("IMP1",&imp1,&nvalues);
  zvip("IMP2",&imp2,&nvalues);
  zvip("IMP3",&imp3,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdiiarithmetic( unit[devno], op, imp1, imp2, imp3 );
}

iicopy()
{
  int imp1, imp2;

  zvip("IMP1",&imp1,&nvalues);
  zvip("IMP2",&imp2,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdiicopy( unit[devno], imp1, imp2 );
}

iilogical()
{
  int op, imp1, imp2, imp3;

  zvip("OP",&op,&nvalues);
  zvip("IMP1",&imp1,&nvalues);
  zvip("IMP2",&imp2,&nvalues);
  zvip("IMP3",&imp3,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdiilogical( unit[devno], op, imp1, imp2, imp3 );
}

iishift()
{
  int shift, imp1, imp2, wrap;

  zvip("SHIFT",&shift,&nvalues);
  zvip("IMP1",&imp1,&nvalues);
  zvip("IMP2",&imp2,&nvalues);
  zvip("WRAP",&wrap,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdiishift( unit[devno], shift, imp1, imp2, wrap );
}

iareafill()
{
  int imp, sstart, lstart, boundary, fill;
  unsigned char bbound, bfill;

  zvip("IMP",&imp,&nvalues);
  zvip("X",&sstart,&nvalues);
  zvip("Y",&lstart,&nvalues);
  zvip("BOUNDRY",&boundary,&nvalues);
  zvip("FILL",&fill,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  bbound = boundary;
  bfill = fill;

  zdiareafill( unit[devno], imp, sstart, lstart, bbound, bfill );
}

ihistogram()
{
  int i, imp, mask, histo[256];

  zvip("IMP",&imp,&nvalues);
  zvip("MASK",&mask,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  vstatus = zdihistogram( unit[devno], imp, mask, histo );

  if ( vstatus == 1 ) {
    for (i = 0; i < 255; i+=3 ) {
      sprintf( message, "Histo[%d] = %d, Histo[%d] = %d, Histo[%d] = %d",
                        i, histo[i], i+1, histo[i+1], i+2, histo[i+2] );
      zvmessage( message, "" );
    }
    sprintf( message, "Histo[255] = %d", histo[255] );
    zvmessage( message, "" );
  }
}

irotate()
{
  int imp, angle;

  zvip("IMP",&imp,&nvalues);
  zvip("ANGLE",&angle,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  zdirotate( unit[devno], imp, angle );
}

loopgraph()
{
  int imps;
  int i, j, k;
  int sstart, lstart;

  zvip("IMPS",&imps,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  sstart = lstart = 75;
  for ( i = 4, k = 1; i <= imps; i+=4, k++ ) {
    vstatus = zdifill( unit[devno], i, 0 );
    if (vstatus == 1) {
      vstatus = zdicircle( unit[devno], i, sstart, lstart, 50, k );
      sstart += 50;
      lstart += 50;
      if (vstatus != 1) break;
    }
    else break;
  }
}

looploop()
{
  int imps, speed;
  char color[6];
  int i, l;

  zvip("IMPS",&imps,&nvalues);
  zvip("COLOR",color,&nvalues);
  zvip("SPEED",&speed,&nvalues);
  zvip("DEVNO",&devno,&nvalues);

  if (!strcmp( color, "BW" )) {
    for ( i = 1; i <= imps; i++ ) {
      zdlconnect( unit[devno], i, 1, 1, 0 );
      zdlconnect( unit[devno], i, 2, 1, 0 );
      zdlconnect( unit[devno], i, 3, 1, 0 );
      sleep( (speed+99)/100 );		/* should be usleep??? */
    }
  } else {
    for ( i = 1,l = 1; i < imps; i++,l++ ) {
      if (  i % 4  == 0 ) i++;
      zdlconnect( unit[devno], i, l, 1, 0 );
      sleep( (speed+99)/100 );		/* should be usleep??? */
      if ( l > 2 ) l = 0;
    }
  }
}

init_canned_luts()
{
  int i, j, k;

  for ( j = 0, k = 0; j < 256, k < 8; j+=32, k++ ) {
    for ( i = j; i < (j + 32); i++ ) {
      pseudo_luts[0][i] = pseudo_color[k].red;
      pseudo_luts[1][i] = pseudo_color[k].green;
      pseudo_luts[2][i] = pseudo_color[k].blue;
    }
    pseudo_luts[0][0] = 0;
    pseudo_luts[1][0] = 0;
    pseudo_luts[2][0] = 0;
  }
  for ( i = 0; i < 256; i++ ) {
    red[i] = 0;
    green[i] = 0;
    blue[i] = 0;
  }
  for ( i = 0; i < 18; i++ ) {
    red[i] = color_chart[i].red;
    green[i] = color_chart[i].green;
    blue[i] = color_chart[i].blue;
  }
  for  (i = 0; i < 6; i++) {
    readaw_ptr[i] = 0;
    readline_ptr[i] = 0;
  }
}

load_images()
{
  int i, j, instance;
  int nl, ns, status;
  unsigned char *p;
  char msg[80];

  for ( i = 0; i < num_images; i++ ) {
    zvclose( inunit[i], 0 );
  }
  if (num_images > 0 ) {
    zviparm( "INAME", image_name, &num_images, &def, 6, 256 );
  } else {
    zvparm( "INAME", image_name, &num_images, &def, 6, 256 );
  }
  for ( i = 0; i < num_images; i++ ) {
    instance = i + 1;    
    zvunit( &inunit[i],"OTH", instance, "U_NAME", image_name[i], 0 );
    inaddr[i] = 0;

#if 0
    status = zvopen( inunit[i],"OPEN_ACT","","ADDRESS",&inaddr[i], 0 );
    if (status == SUCCESS && inaddr[i] == 0)
      zvclose(inunit[i], 0);
    if (status != SUCCESS && status != ARRAY_IO_NOT_ALLOWED)
      zvsignal(inunit[i], status, 1);
#endif

    if (inaddr[i] == 0) {		/* No array I/O */
      status=zvopen(inunit[i],"OPEN_ACT","SA","U_FORMAT","BYTE",0);
      zvget(inunit[i], "NL", &nl, "NS", &ns, 0);
      inaddr[i] = malloc(nl*ns);		/* BYTE only! */
      if (inaddr[i] == 0) {
        zvmessage("Out of memory reading file\n", "VRDITEST-NOMEM");
        zabend();
      }
      for (j=0, p=inaddr[i]; j<nl; j++,p+=ns)
        zvread(inunit[i], p, 0);
    }

    zvget( inunit[i], "NL", &numlines[i], "RECSIZE", &recsize[i], 0 );

    linesize[i] = recsize[i];
    line_ptr[i] = inaddr[i];
    imgsize[i] = numlines[i] * recsize[i];
  }
}

UpperString( String )
char	*String;
{
  int	i, Length;

  Length = strlen( String );
  for ( i = 0; i < Length; i++ ) {
    String[i] = toupper(String[i]);
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vrditest.imake
#define PROGRAM vrditest
#define MODULE_LIST vrditest.c

#define MAIN_LANG_C
#define USES_C

#define TEST

#define LIB_VRDI
#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$PDF_File:
$ create vrditest.pdf
process 
  subcmd-default loadimages
    parm	INAME	type=string	count=0:6	default=--
  end-sub
  subcmd xddactivate
    parm	flag	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddallocate
    parm	device	type=string
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddbatch
    parm	flag	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddclose
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddconfigure
    parm	config	type=integer	count=4
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddfree
    parm	device	type=string
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddinfo
    parm	start	type=integer
    parm	number	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddopen
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddsmartopen
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddname
    parm	flag	type=integer			default=2
    parm	maxlen	type=integer	valid=(1:30)	default=25
    parm	devno	type=integer	default=0
  end-sub
  subcmd xddunit
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdddnamedunit	! extra 'd' in pdf only to avoid conflict with xddname
    parm	device	type=string
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdddunitnames	! extra 'd' in pdf only to avoid conflict with xddunit
  end-sub
  subcmd xdlconnect
    parm	imp	type=integer
    parm	lut	type=integer
    parm	section	type=integer
    parm	bypass	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdlramp
    parm	lut	type=integer
    parm	section	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdlread
    parm	lut	type=integer
    parm	section	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdlwrite
    parm	lut	type=integer
    parm	section	type=integer
    parm	canned	type=integer	valid = (0:2)	default=0
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdiawlocation
    parm	imp	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdiawread
    parm	imp	type=integer
    parm	size	type=integer	valid = (0:4194304)	default=0
    parm	outimg	type=integer	valid=(0:2)		default=0
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdiawset
    parm	imp	type=integer
    parm	left	type=integer
    parm	top	type=integer
    parm	right	type=integer
    parm	bottom	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdiawwrite
    parm	imp	type=integer
    parm	size	type=integer	valid = (0:1048576)	default=0
    parm	image	type=integer	valid=(0:6)		default=0
    parm	outimg	type=integer	valid=(0:2)		default=0
    parm	which	type=keyword	valid=(IMAGE,READ)	default=IMAGE
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdilineread
    parm	imp	type=integer
    parm	x	type=integer	default=0
    parm	y	type=integer	default=0
    parm	size	type=integer	valid = (0:1024)	default=0
    parm	nlines	type=integer	valid = (0:1024)	default=0
    parm	outline	type=integer	valid=(0:2)		default=0
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdilinewrite
    parm	imp	type=integer
    parm	x	type=integer	default=0
    parm	y	type=integer	default=0
    parm	size	type=integer	default=0
    parm	nlines	type=integer	default=0
    parm	image	type=integer	valid=(0:6)		default=0
    parm	outline	type=integer	valid=(0:2)		default=0
    parm	which	type=keyword	valid=(IMAGE,READ)	default=IMAGE
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdipixelread
    parm	imp	type=integer
    parm	x	type=integer
    parm	y	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdipixelwrite
    parm	imp	type=integer
    parm	x	type=integer
    parm	y	type=integer
    parm	value	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdidwlocation
    parm	imp	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdidwset
    parm	imp	type=integer
    parm	left	type=integer	default=1
    parm	top	type=integer	default=1
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdicircle
    parm	imp	type=integer
    parm	x	type=integer
    parm	y	type=integer
    parm	radius	type=integer
    parm	value	type=integer	default=255
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdifill
    parm	imp	type=integer
    parm	value	type=integer	default=0
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdipolyline
    parm	imp	type=integer
    parm	value	type=integer
    parm	npts	type=integer	valid=(0,2)	default=0
    parm	x	type=integer	count=2		default=(0,0)
    parm	y	type=integer	count=2		default=(0,0)
    parm	canned	type=keyword	valid=(0,CAN)	default=CAN
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdizoom
    parm	imp	type=integer
    parm	zoom	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdcautotrack
    parm	cursor	type=integer
    parm	device	type=integer
    parm	flag	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdclocation
    parm	cursor	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdcoff
    parm	cursor	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdcon
    parm	cursor	type=integer
    parm	form	type=integer
    parm	blink	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdcset
    parm	cursor	type=integer
    parm	x	type=integer
    parm	y	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdcshow
    parm	cursor	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdcsize
    parm	cursor	type=integer
    parm	xsize	type=integer
    parm	ysize	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdccolor
    parm	cursor	type=integer
    parm	red	type=integer
    parm	green	type=integer
    parm	blue	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdcilocation
    parm	cursor	type=integer
    parm	imp	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdciset
    parm	cursor	type=integer
    parm	x	type=integer
    parm	y	type=integer
    parm	imp	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdcimp2raw
    parm	imp	type=integer
    parm	ximp	type=integer
    parm	yimp	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdcraw2imp
    parm	imp	type=integer
    parm	xraw	type=integer
    parm	yraw	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsnl
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsns
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsvnl
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsvns
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsmode
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsimp
    parm	lut	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdssection
    parm	lut	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsbypass
    parm	lut	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsgraph
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsgsection
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsgbypass
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdszoom
    parm	imp	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsdwline
    parm	imp	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdsdwsamp
    parm	imp	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdfregister
    parm	group	type=integer	default=0
  end-sub
  subcmd xdfconfig
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdflut
    parm	lut	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdfglut
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdfimage
    parm	imp	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdgconnect
    parm	imp	type=integer
    parm	section	type=integer
    parm	bypass	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdglconstant
    parm	section	type=integer
    parm	red	type=integer
    parm	green	type=integer
    parm	blue	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdglread
    parm	section	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdglwrite
    parm	section	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdgoff
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdgon
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdglinit
    parm	section	type=integer	default=1
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdgcolor
    parm	text	type=(string,80)
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdgrgb
    parm	red	type=integer
    parm	green	type=integer
    parm	blue	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdaclear
    parm	x	type=integer
    parm	y	type=integer
    parm	nchars	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdaoff
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdaon
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdatext
    parm	x	type=integer
    parm	y	type=integer
    parm	length	type=integer
    parm	text	type=(string,80)
    parm	blink	type=integer
    parm	reverse	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdx1d
    parm	device	type=integer
    parm	knob	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdx2d
    parm	device	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdx3d
    parm	device	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdxswitch
    parm	device	type=integer
    parm	switch	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdtcolor
    parm	color	type=integer
    parm	prec	type=integer
  end-sub
  subcmd xdtfont
    parm	font	type=integer
  end-sub
  subcmd xdtlength
    parm	nchars	type=integer
    parm	text	type=string
  end-sub
  subcmd xdtmask
    parm	mask	type=integer
  end-sub
  subcmd xdtrotate
    parm	angle	type=real
  end-sub
  subcmd xdtsize
    parm	height	type=integer
    parm	scale	type=real
  end-sub
  subcmd xdttext
    parm	imp	type=integer
    parm	x	type=integer
    parm	y	type=integer
    parm	loc	type=integer
    parm	nchars	type=integer
    parm	text	type=(string,80)
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdeaction
    parm	warn	type=integer	default=2
    parm	error	type=integer	default=2
    parm	fatal	type=integer	default=2
  end-sub
  subcmd xdelevel
    parm	code	type=integer
  end-sub
  subcmd xdesignal
    parm	code	type=integer
  end-sub
  subcmd xdimawwrite
    parm	imp	type=integer
    parm	mask	type=integer
    parm	size	type=integer	valid = (0:1048576)	default=0
    parm	image	type=integer	valid=(0:2)		default=0
    parm	outimg	type=integer	valid=(0:2)		default=0
    parm	which	type=keyword	valid=(IMAGE,READ)	default=IMAGE
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdimcircle
    parm	imp	type=integer
    parm	x	type=integer
    parm	y	type=integer
    parm	radius	type=integer
    parm	mask	type=integer
    parm	value	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdimfill
    parm	imp	type=integer
    parm	mask	type=integer
    parm	value	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdimlinewrite
    parm	imp	type=integer
    parm	x	type=integer	default=0
    parm	y	type=integer	default=0
    parm	mask	type=integer
    parm	size	type=integer	default=0
    parm	nlines	type=integer	default=0
    parm	outline	type=integer	valid=(0:2)		default=0
    parm	image	type=integer	valid=(0:2)		default=0
    parm	which	type=keyword	valid=(IMAGE,READ)	default=IMAGE
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdimpixelwrite
    parm	imp	type=integer
    parm	x	type=integer
    parm	y	type=integer
    parm	mask	type=integer
    parm	value	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdimpolyline
    parm	imp	type=integer
    parm	mask	type=integer
    parm	value	type=integer
    parm	npts	type=integer	valid=(0,2)	default=0
    parm	x	type=integer	count=2		default=(0,0)
    parm	y	type=integer	count=2		default=(0,0)
    parm	canned	type=keyword	valid=(0,CAN)	default=CAN
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdiiarithmetic
    parm	op	type=integer
    parm	imp1	type=integer
    parm	imp2	type=integer
    parm	imp3	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdiicopy
    parm	imp1	type=integer
    parm	imp2	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdiilogical
    parm	op	type=integer
    parm	imp1	type=integer
    parm	imp2	type=integer
    parm	imp3	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdiishift
    parm	shift	type=integer
    parm	imp1	type=integer
    parm	imp2	type=integer
    parm	wrap	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdiareafill
    parm	imp	type=integer
    parm	x	type=integer
    parm	y	type=integer
    parm	boundry	type=integer
    parm	fill	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdihistogram
    parm	imp	type=integer
    parm	mask	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd xdirotate
    parm	imp	type=integer
    parm	angle	type=integer
    parm	devno	type=integer	default=0
  end-sub
  subcmd looploop
    parm	imps	type=integer
    parm	speed	type=integer
    parm	color	type=keyword	valid=(COLOR,BW)	default=BW
  end-sub
  subcmd loopgraph
    parm	imps	type=integer
  end-sub
  subcmd EXIT
  end-sub
end-proc
$ Return
$!#############################################################################
$Test_File:
$ create tstadage1024.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
enable-script tstadage1024.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstadage1024.scr
vrditest (images:tm3.img,images:tm2.img,images:tm1.img)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on the Adage Ikonas for 1024x1024. The 
![7m  script will pause after executing one or 
![7m  more routines for verification. At a pause, 
![7m  check for correctness and press <return> to continue.
![7m  VAX 2 is slow, have some coffee with you.
![7m  
![7m  Also, depending on what's in the adjacent image planes,
![7m  you may see a line - about 2 pixels wide - down the right
![7m  side of the monitor
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Expect error code DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Configuration Routines
![7m  
xddunit
xddopen
xddactivate flag=1
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
xddconfigure config=(1,2,2,1)
xddinfo start=10 number=4
![7m      Configuration is 1 2 2 1
.pause
![7m      First, clean up
xdifill 1 0
xdifill 2 0
xdifill 3 0
![7m  
![7mLookup Tables Routines
![7m  
![7m      Load linear ramps and connect for color
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m  
![7mImage Memory Plane Routines
![7m  
xdiawlocation imp=2
![7m      Access Window is 1, 1, 1024, 1024
.pause
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m      Color image is displayed
.pause
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, and rewritten
![7m      (using access window read/write)
.pause
xdiawset imp=1 left=257 top=1 right=768 bottom=512
xdiawset imp=2 left=257 top=1 right=768 bottom=512
xdiawset imp=3 left=257 top=1 right=768 bottom=512
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawset imp=1 left=257 top=257 right=768 bottom=768
xdiawset imp=2 left=257 top=257 right=768 bottom=768
xdiawset imp=3 left=257 top=257 right=768 bottom=768
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      512x512 section of image appears in center
.pause
xdlconnect imp=1 lut=2 section=1 bypass=0
xdlconnect imp=1 lut=3 section=1 bypass=0
![7m      Turns BW
.pause
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m      Color again
.pause
![7m      Load pseudo color table
xdlwrite lut=2 section=1 canned=1
xdlread lut=2 section=1
![7m      Array locations:   0 -  31   value:   0
![7m                        32 -  63          128
![7m                        64 - 127          255
![7m                       128 - 159          200
![7m                       160 - 191          255
![7m                       192 - 223          128
![7m                       224 - 127            0
![7m      Read of 8 color pseudo table
.pause
![7m      Back to linear ramps 
xdlramp lut=2 section=1
xdilineread imp=1 outline=0
xdilineread imp=2 outline=1
xdilineread imp=3 outline=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdilinewrite imp=1 'READ outline=0
xdilinewrite imp=2 'READ outline=1
xdilinewrite imp=3 'READ outline=2
![7m      Planes have been erased, and rewritten
![7m      (using line read/write)
.pause
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdifill imp=2 value=0
xdifill imp=3 value=0
![7m      Green and Blue planes erased
xdidwlocation imp=1
![7m      Display window initialized at 1, 1
![7m      Ignore following 'all display windows have been set' warning.
![7m      Also, ignore the garbage you may see in the adjacent image
![7m      planes when the display window changes
.pause
xdidwset imp=1 left=1 top=257
![7m      Red plane moved up 256 pixels 
.pause
xdidwset imp=1 left=257 top=1
![7m      Red plane moved left 256 pixels 
.pause
![7m      Reinitialize Access and Display windows and Erase
xdidwset imp=1 left=1 top=1
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdifill imp=1 value=0
xdipixelwrite imp=1 x=250 y=125 value=250
xdipixelwrite imp=1 x=250 y=175 value=251
xdipixelread imp=1 x=250 y=125
xdipixelread imp=1 x=250 y=175
![7m     Verify pixels written are pixels read
.pause
xdipolyline imp=1 value=255 'CAN
xdipolyline imp=1 value=255 'CAN
xdipolyline imp=1 value=255 npts=2 x=(200,400) y=(200,200) '0
xdipolyline imp=1 value=255 npts=2 x=(400,400) y=(200,400) '0
xdipolyline imp=1 value=255 npts=2 x=(400,200) y=(400,400) '0
xdipolyline imp=1 value=255 npts=2 x=(200,200) y=(400,200) '0
xdicircle imp=1 x=300 y=300 radius=50 value=255
![7m      Canned array of vectors (imp 1, no graphics plane)
![7m      series of vectors forming a square, and a circle
.pause
xdifill imp=1 value=0
xdttext imp=1 x=100 y=100 loc=1 nchars=0 text="ZOOM"
xdizoom imp=1 zoom=2
xdizoom imp=1 zoom=3
xdizoom imp=1 zoom=1
![7m      Text "Zoom" zooms and resumes original size
![7m      Ignore 'all image planes have been zoomed' warning
.pause
xdifill imp=1 value=0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=45.
xdtsize height=14 scale=1.
xdttext imp=1 x=600 y=600 loc=1 nchars=0 text="45 DEGREES"
xdtmask mask=127
xdttext imp=1 x=700 y=700 loc=1 nchars=0 text="MASKED"
![7m      Text "45 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdtfont font=1
xdtlength nchars=4 text=four
xdtcolor color=127 prec=0
xdtrotate angle=-60.
xdtsize height=18 scale=1.
xdttext imp=2 x=340 y=340 loc=2 nchars=0 text="-60 DEGREES"
xdtmask mask=63
xdttext imp=2 x=440 y=440 loc=2 nchars=6 text="MASKED"
![7m      Text "-60 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m  
![7mCursor and IO Device Routines
![7m  
xdipixelwrite imp=1 x=256 y=256 value=255
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
![7m      Cursor is ON, Autotracking is ON,
![7m      Move cursor using pen over pixel at (256,256)
![7m      ADAGE ONLY AUTOTRACKS IN UPPER LEFT QUADRANT
![7m      AND IT'S ALMOST IMPOSSIBLE TO POSITION CURSOR
.pause
xdclocation cursor=1
![7m      Press pen
xdx2d device=1
![7m      Prox and Pen are ON
![7m      X and Y should be close to zero
.pause
![7m      Press pen
xdxswitch device=1 switch=1
![7m      Switch is ON
.pause
xdcset cursor=1 x=400 y=300
![7m      Expect error CANTPOSCUR because cannot
![7m      position cursor when autotracking is enabled
.pause
xdcautotrack cursor=1 device=1 flag=0
![7m      Autotracking is OFF,
![7m      Move pen around to verify
.pause
xdcoff cursor=1
![7m      Cursor disappears
.pause
xdcon cursor=1 form=4 blink=0
![7m      Cursor is a boxed MIPL
.pause
xdcset cursor=1 x=256 y=256
xdclocation cursor=1
![7m      Cursor is located at 256,256
.pause
xdcoff cursor=1
![7m      Cursor disappears
.pause
![7m  
![7mAdvanced Display Device Interface
![7m  
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdimfill imp=1 value=255 mask=127
xdiawset imp=1 left=1 top=513 right=512 bottom=1024
xdimfill imp=1 value=255 mask=63
xdiawset imp=1 left=513 top=1 right=1024 bottom=512
xdimawwrite imp=1 'READ outimg=0 mask=127
xdiawset imp=1 left=513 top=513 right=1024 bottom=1024
xdimlinewrite imp=1 'READ outline=0 mask=32
![7m      Top left quadrant: a masked fill
![7m      Bottom left quadrant: a further masked fill
![7m      Top right quadrant: a masked access window write 
![7m      Bottom right quadrant: a masked line write
.pause
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdifill imp=1 value=0
xdimpixelwrite imp=1 x=700 y=800 value=255 mask=127
xdimpixelwrite imp=1 x=700 y=950 value=255 mask=63
xdipixelread imp=1 x=700 y=800
xdipixelread imp=1 x=700 y=950
![7m     Verify pixels written with mask are pixels read (mask)
.pause
xdimpolyline imp=1 value=255 'CAN mask=63
xdimpolyline imp=1 value=255 'CAN mask=255
![7m     A couple masked cans (2 stars)
.pause
xdifill imp=1 value=0
xdimcircle imp=1 x=600 y=600 radius=100 value=255 mask=255
xdimcircle imp=1 x=500 y=500 radius=100 value=255 mask=127
![7m      Two circles: one unmasked, one masked 
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=600 y=600 radius=100 value=255
xdiareafill imp=1 x=600 y=600 boundry=255 fill=200
![7m      A filled circle
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=300 y=300 radius=100 value=255
xdicircle imp=2 x=600 y=600 radius=100 value=255
![7m      Verify red and green circles
.pause
xdiiarithmetic op=0 imp1=1 imp2=2 imp3=3
![7m      Image plane 1 and 2 are added and placed in 3
![7m      Verify upper left circle turns magenta and 
![7m      lower right circle turns cyan
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify both circles turn blue
.pause
xdiishift shift=3 imp1=3 imp2=1 wrap=1
xdifill imp=3 value=0
![7m      Image plane 3 is shifted into 1
![7m      Verify both turn magenta, then red
.pause
xdiicopy imp1=1 imp2=2
xdifill imp=1 value=0
![7m      Image plane 1 is copied into 2
![7m      Verify both turn yellow, then green
.pause
xdicircle imp=1 x=400 y=400 radius=100 value=255
xdiilogical op=1 imp1=1 imp2=2 imp3=3
![7m      New red circle in 1 is ORed with 2 and placed in 3
![7m      Verify old circles turn cyan, new red circle turns magenta
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify all end up blue
.pause
xdirotate imp=3 angle=2
![7m      Image plane 3 is rotated -90 degrees 
.pause
xdihistogram imp=3 mask=0
![7m      Histogram collected, 1690 in 255, rest in zero
.pause
xdifill imp=3 value=0
![7m  
![7mLimit testing - Expect ERROR MESSAGES on all calls (except xddinfo)
![7m  
xddinfo start=10 number=4
xddconfigure config=(0,3,0,0)
xddconfigure config=(0,0,0,2)
xddinfo start=4 number=1
xdiawwrite imp=13 image=0
xddinfo start=3 number=1
xdlramp lut=4 section=1
xddinfo start=24 number=1
xdlramp lut=1 section=2
xddinfo start=38 number=1
xdizoom imp=1 zoom=17
![7m  
![7mNo Graphics Overlay in 1024x1024 - Expect ERROR MESSAGES on all calls 
![7m  
xddinfo start=30 number=7
xdgon
xdgconnect imp=4 section=1 bypass=0
xdglconstant section=1 red=255 green=255 blue=255
xdglwrite section=1
xdglread section=1
![7m  
![7mNo AFG - Expect ERROR MESSAGES on all calls 
![7m  
xdaclear x=1 y=1 nchars=5
xdaoff
xdaon 
xdatext x=1 y=1 length=4 text=none blink=0 reverse=0
![7m  
![7mClose - an unsupported function on an ADAGE - Expect an ERROR MESSAGE
![7m  
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstadage512.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
enable-script tstadage512.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstadage512.scr
vrditest (images:mandrill.red,images:mandrill.grn,images:mandrill.blu)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on the ADAGE Ikonas in 512x512. The script will 
![7m  pause after executing one or more routines for verification. 
![7m  At a pause, check for correctness and press <return> to continue.
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Expect error code DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Configuration Routines
![7m  
xddunit
xddopen
xddactivate flag=1
![7m      First clean up
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdifill imp=4 value=0
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
xddconfigure config=(0,0,0,0)
xddinfo start=10 number=4
![7m      Default configuration is all 1's
.pause
![7m  
![7mLookup Tables Routines
![7m  
![7m      Load pseudo color table into section 1
xdlwrite lut=1 section=1 canned=0
xdlread lut=1 section=1
![7m      Array locations:   0 - 127   value:   0
![7m                       128 - 159           84
![7m                       160 - 255          255
.pause
![7m      Load linear ramps and connect for color
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m  
![7mGraphics Overlay Plane Routines
![7m  
xddinfo start=30 number=7
![7m      Graphics as initialized
.pause
xdgconnect imp=4 section=1 bypass=0
xdglconstant section=1 red=255 green=255 blue=255
xdglread section=1
![7m      Overlay Lut written was 255. Only first 16 values read.
.pause
xdglwrite section=1
xdglread section=1
![7m      Overlay Lut written with assorted colors. Verify
![7m      that it's changed from previous constants.
.pause
xdgon
![7m  
![7mImage Memory Plane Routines
![7m  
xdiawlocation imp=1
![7m      Access Window is 1, 1, 512, 512
.pause
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m      Color image is displayed
.pause
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, and rewritten
![7m      (using access window read/write)
.pause
xdiawset imp=1 left=101 top=1 right=356 bottom=256
xdiawset imp=2 left=101 top=1 right=356 bottom=256
xdiawset imp=3 left=101 top=1 right=356 bottom=256
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdiawset imp=2 left=1 top=1 right=512 bottom=512
xdiawset imp=3 left=1 top=1 right=512 bottom=512
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawset imp=1 left=129 top=129 right=384 bottom=384
xdiawset imp=2 left=129 top=129 right=384 bottom=384
xdiawset imp=3 left=129 top=129 right=384 bottom=384
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      256x256 section of image appears in center
.pause
xdlconnect imp=1 lut=2 section=1 bypass=0
xdlconnect imp=1 lut=3 section=1 bypass=0
![7m      Turns BW
.pause
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m      Color again
.pause
xdilineread imp=1 outline=0
xdilineread imp=2 outline=1
xdilineread imp=3 outline=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdilinewrite imp=1 'READ outline=0
xdilinewrite imp=2 'READ outline=1
xdilinewrite imp=3 'READ outline=2
![7m      Planes have been erased, and rewritten
![7m      (using line read/write)
.pause
xdiawset imp=2 left=1 top=1 right=512 bottom=512
xdiawset imp=3 left=1 top=1 right=512 bottom=512
xdifill imp=2 value=0
xdifill imp=3 value=0
![7m      Green and Blue planes erased
xdidwlocation imp=1
![7m      Display window initialized at 1, 1
.pause
xdidwset imp=1 left=129 top=129
![7m      Red plane moved up and left 128 pixels
.pause
xdidwset imp=1 left=1 top=129
![7m      Red plane moved up 128 pixels
.pause
![7m      Reinitialize Access and Display windows and Erase
xdidwset imp=1 left=1 top=1
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdifill imp=1 value=0
xdipixelwrite imp=1 x=250 y=125 value=250
xdipixelwrite imp=1 x=250 y=175 value=251
xdipixelread imp=1 x=250 y=125
xdipixelread imp=1 x=250 y=175
![7m     Verify pixels written are pixels read
.pause
xdifill imp=1 value=0
xdipolyline imp=4 value=1 'CAN
xdipolyline imp=4 value=2 'CAN
xdipolyline imp=4 value=3 npts=2 x=(200,400) y=(200,200) '0
xdipolyline imp=4 value=4 npts=2 x=(400,400) y=(200,400) '0
xdipolyline imp=4 value=5 npts=2 x=(400,200) y=(400,400) '0
xdipolyline imp=4 value=6 npts=2 x=(200,200) y=(400,200) '0
xdicircle imp=4 x=300 y=300 radius=50 value=7
![7m      Canned array of vectors (a green and a blue star),
![7m      series of vectors forming a square, and a centered circle
.pause
xdgoff
![7m      Graphics are OFF
.pause
xdgon
xdifill imp=4 value=0
xdttext imp=1 x=50 y=50 loc=1 nchars=0 text="ZOOM"
xdizoom imp=1 zoom=2
xdizoom imp=1 zoom=3
xdizoom imp=1 zoom=1
![7m      Text "Zoom" zooms and resumes original size
.pause
xdifill imp=1 value=0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=45.
xdtsize height=14 scale=1.
xdttext imp=1 x=150 y=150 loc=1 nchars=0 text="45 DEGREES"
xdtmask mask=127
xdttext imp=1 x=170 y=170 loc=1 nchars=0 text="MASKED"
![7m      Text "45 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdtfont font=1
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=-60.
xdtsize height=18 scale=1.
xdtmask mask=255
xdttext imp=2 x=200 y=200 loc=2 nchars=0 text="-60 DEGREES"
xdtmask mask=127
xdttext imp=2 x=220 y=220 loc=2 nchars=6 text="MASKED"
![7m      Text "-60 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m  
![7mCursor and IO Device Routines
![7m  
xdipixelwrite imp=1 x=256 y=256 value=255
xdipixelwrite imp=2 x=256 y=256 value=255
xdipixelwrite imp=3 x=256 y=256 value=255
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
![7m      Cursor is ON, Autotracking is ON,
![7m      Move cursor using pen over pixel at (256,256)
![7m      IT'S ALMOST IMPOSSIBLE TO POSITION CURSOR
.pause
xdclocation cursor=1
![7m      Depress pen
xdx2d device=1
![7m      Prox and Pen are ON
![7m      X and Y should be close to zero
.pause
![7m      Depress pen again
xdxswitch device=1 switch=1
![7m      Switch is ON
.pause
xdcset cursor=1 x=400 y=300
![7m      Expect error CANTPOSCUR because cannot
![7m      position cursor when autotracking is enabled
.pause
xdcautotrack cursor=1 device=1 flag=0
![7m      Autotracking is OFF,
![7m      Move pen around to verify
.pause
xdcoff cursor=1
![7m      Cursor disappears
.pause
xdcon cursor=1 form=4 blink=0
![7m      Cursor is a boxed MIPL
.pause
xdcset cursor=1 x=400 y=300
xdclocation cursor=1
![7m      Cursor is located at 400,300
.pause
xdcoff cursor=1
![7m      Cursor disappears
.pause
![7m  
![7mAdvanced Display Device Interface
![7m  
xdiawset imp=1 left=1 top=1 right=256 bottom=256
xdimfill imp=1 value=255 mask=127
xdiawset imp=1 left=1 top=257 right=256 bottom=512
xdimlinewrite imp=1 'READ outline=0 mask=32
xdiawset imp=1 left=257 top=1 right=512 bottom=256
xdimawwrite imp=1 'READ outimg=0 mask=127
xdiawset imp=1 left=257 top=257 right=512 bottom=512
xdimlinewrite imp=1 'READ outline=0 mask=15
![7m      Top left quadrant: a masked fill
![7m      Bottom left quadrant: a masked line write
![7m      Top right quadrant: a masked access window write 
![7m      Bottom right quadrant: a further masked line write
.pause
.pause
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdifill imp=1 value=0
xdimpixelwrite imp=1 x=350 y=400 value=255 mask=127
xdimpixelwrite imp=1 x=350 y=475 value=255 mask=63
xdipixelread imp=1 x=350 y=400
xdipixelread imp=1 x=350 y=475
![7m     Verify pixels written with mask are equal to mask
.pause
xdimpolyline imp=1 value=255 'CAN mask=63
xdimpolyline imp=1 value=255 'CAN mask=255
![7m     A couple masked cans (2 stars)
.pause
xdifill imp=1 value=0
xdimcircle imp=1 x=300 y=300 radius=100 value=255 mask=255
xdimcircle imp=1 x=250 y=250 radius=100 value=255 mask=127
![7m      Two circles: an unmasked  circle, then masked
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=300 y=300 radius=100 value=255
xdiareafill imp=1 x=300 y=300 boundry=255 fill=200
![7m      A filled circle 
.pause
xdifill imp=1 value=0
xdipolyline imp=1 value=255 'CAN
xdicircle imp=2 x=300 y=300 radius=100 value=255
![7m      Verify red star and green circle
.pause
xdiiarithmetic op=0 imp1=1 imp2=2 imp3=3
![7m      Image plane 1 and 2 are added and placed in 3
![7m      Verify star turns magenta and circle turns cyan
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify star and circle turn blue
.pause
xdiishift shift=3 imp1=3 imp2=1 wrap=1
xdifill imp=3 value=0
![7m      Image plane 3 is shifted into 1
![7m      Verify both turn magenta then red
.pause
xdiicopy imp1=1 imp2=2
xdifill imp=1 value=0
![7m      Image plane 1 is copied into 2
![7m      Verify all turn yellow then green
.pause
xdicircle imp=1 x=400 y=400 radius=100 value=255
xdiilogical op=1 imp1=1 imp2=2 imp3=3
![7m      New red circle in 1 is ORed with 2 and placed in 3
![7m      Verify star and old circle turn cyan,
![7m      new circle turns magenta
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify image plane 3 contains all three
.pause
xdirotate imp=3 angle=1
![7m      Image plane 3 is rotated 180 degrees 
.pause
xdihistogram imp=3 mask=0
![7m      Histogram collected, 1515 in 255, rest in zero
.pause
xdifill imp=3 value=0
![7m  
![7mLimit testing - Expect ERROR MESSAGES on all calls (except xddinfo)
![7m  
xddinfo start=10 number=4
xddconfigure config=(0,3,0,0)
xddconfigure config=(0,0,2,0)
xddinfo start=4 number=1
xdiawwrite imp=33 image=0
xddinfo start=3 number=1
xdlramp lut=4 section=1
xddinfo start=24 number=1
xdlramp lut=1 section=5
xddinfo start=38 number=1
xdizoom imp=1 zoom=9
![7m  
![7mNo AFG - Expect ERROR MESSAGES on all calls 
![7m  
xdaclear x=1 y=1 nchars=5
xdaoff
xdaon 
xdatext x=1 y=1 length=4 text=none blink=0 reverse=0
![7m  
![7mClose - an unsupported function on an ADAGE - Expect an ERROR MESSAGE
![7m  
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstadage640.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
gen ram1. 480 640
gen ram2. 480 640 ival=64
gen ram3. 480 640 ival=128
enable-script tstadage640.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstadage640.scr
vrditest (ram1.,ram2.,ram3.)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on the ADAGE Ikonas for 480x640. The script will 
![7m  pause after executing one or more routines for verification. 
![7m  At a pause, check for correctness and press <return> to continue.
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Expect error code DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Configuration Routines
![7m  
xddunit
xddopen
xddactivate flag=1
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
xddconfigure config=(0,2,3,2)
xddinfo start=10 number=4
![7m      Configuration is 1,2,3,2
.pause
![7m      First clean up
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdifill imp=4 value=0
![7m  
![7mLookup Tables Routines
![7m  
![7m      Load pseudo color table into section 1
xdlwrite lut=1 section=1 canned=0
xdlread lut=1 section=1
![7m      Array locations:   0 - 127   value:   0
![7m                       128 - 159           84
![7m                       160 - 255          255
.pause
![7m      Load linear ramps and connect for color
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m  
![7mGraphics Overlay Plane Routines
![7m  
xddinfo start=30 number=7
![7m      Graphics as initialized
.pause
xdgconnect imp=4 section=1 bypass=0
xdglconstant section=1 red=255 green=255 blue=255
xdglread section=1
![7m      Overlay Lut written was 255. Only first 16 values read.
.pause
xdglwrite section=1
xdglread section=1
![7m      Overlay Lut written with assorted colors. Verify
![7m      that it's changed from previous constants.
.pause
xdgon
![7m  
![7mImage Memory Plane Routines
![7m  
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdiawset imp=2 left=1 top=1 right=640 bottom=480
xdiawset imp=3 left=1 top=1 right=640 bottom=480
xdiawlocation imp=1
![7m      Access Window is 1, 1, 640, 480
.pause
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m      Color image is displayed
.pause
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, and rewritten
![7m      (using access window read/write)
.pause
xdiawset imp=1 left=161 top=1 right=480 bottom=240
xdiawset imp=2 left=161 top=1 right=480 bottom=240
xdiawset imp=3 left=161 top=1 right=480 bottom=240
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdiawset imp=2 left=1 top=1 right=640 bottom=480
xdiawset imp=3 left=1 top=1 right=640 bottom=480
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawset imp=1 left=161 top=121 right=480 bottom=360
xdiawset imp=2 left=161 top=121 right=480 bottom=360
xdiawset imp=3 left=161 top=121 right=480 bottom=360
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      640x480 section of image appears in center
.pause
xdlconnect imp=1 lut=2 section=1 bypass=0
xdlconnect imp=1 lut=3 section=1 bypass=0
![7m      Turns BW
.pause
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m      Color again
.pause
xdilineread imp=1 outline=0
xdilineread imp=2 outline=1
xdilineread imp=3 outline=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdilinewrite imp=1 'READ outline=0
xdilinewrite imp=2 'READ outline=1
xdilinewrite imp=3 'READ outline=2
![7m      Planes have been erased, and rewritten
![7m      (using line read/write)
.pause
xdiawset imp=2 left=1 top=1 right=640 bottom=480
xdiawset imp=3 left=1 top=1 right=640 bottom=480
xdifill imp=2 value=0
xdifill imp=3 value=0
![7m      Green and Blue planes erased
xdidwlocation imp=1
![7m      Display window initialized at 1, 1
.pause
xdidwset imp=1 left=161 top=121
![7m      Red plane moved up and left 
.pause
xdidwset imp=1 left=1 top=121
![7m      Red plane moved up 
.pause
![7m      Reinitialize Access and Display windows and Erase
xdidwset imp=1 left=1 top=1
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdifill imp=1 value=0
xdipixelwrite imp=1 x=250 y=125 value=250
xdipixelwrite imp=1 x=250 y=175 value=251
xdipixelread imp=1 x=250 y=125
xdipixelread imp=1 x=250 y=175
![7m     Verify pixels written are pixels read
.pause
xdifill imp=1 value=0
xdipolyline imp=4 value=1 'CAN
xdipolyline imp=4 value=2 'CAN
xdipolyline imp=4 value=3 npts=2 x=(200,400) y=(200,200) '0
xdipolyline imp=4 value=4 npts=2 x=(400,400) y=(200,400) '0
xdipolyline imp=4 value=5 npts=2 x=(400,200) y=(400,400) '0
xdipolyline imp=4 value=6 npts=2 x=(200,200) y=(400,200) '0
xdicircle imp=4 x=300 y=300 radius=50 value=7
![7m      Canned array of vectors (a green and a blue star),
![7m      series of vectors forming a square, and a centered circle
.pause
xdgoff
![7m      Graphics are OFF
.pause
xdgon
xdifill imp=4 value=0
xdttext imp=1 x=50 y=50 loc=1 nchars=0 text="ZOOM"
xdizoom imp=1 zoom=2
xdizoom imp=1 zoom=3
xdizoom imp=1 zoom=1
![7m      Text "Zoom" zooms and resumes original size
.pause
xdifill imp=1 value=0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=45.
xdtsize height=14 scale=1.
xdttext imp=1 x=150 y=150 loc=1 nchars=0 text="45 DEGREES"
xdtmask mask=127
xdttext imp=1 x=170 y=170 loc=1 nchars=0 text="MASKED"
![7m      Text "45 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdtfont font=1
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=-60.
xdtsize height=18 scale=1.
xdtmask mask=255
xdttext imp=2 x=200 y=200 loc=2 nchars=0 text="-60 DEGREES"
xdtmask mask=127
xdttext imp=2 x=220 y=220 loc=2 nchars=6 text="MASKED"
![7m      Text "-60 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m  
![7mCursor and IO Device Routines
![7m  
xdipixelwrite imp=1 x=320 y=240 value=255
xdipixelwrite imp=2 x=320 y=240 value=255
xdipixelwrite imp=3 x=320 y=240 value=255
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
![7m      Cursor is ON, Autotracking is ON,
![7m      Move cursor using pen over pixel at (320,240)
![7m      ADAGE ONLY AUTOTRACKS TO SAMPLE 512
![7m      AND IT'S ALMOST IMPOSSIBLE TO POSITION CURSOR
.pause
xdclocation cursor=1
![7m      Depress pen
xdx2d device=1
![7m      Prox and Pen are ON
![7m      X and Y should be close to zero
.pause
![7m      Depress pen again
xdxswitch device=1 switch=1
![7m      Switch is ON
.pause
xdcset cursor=1 x=400 y=300
![7m      Expect error CANTPOSCUR because cannot
![7m      position cursor when autotracking is enabled
.pause
xdcautotrack cursor=1 device=1 flag=0
![7m      Autotracking is OFF,
![7m      Move pen around to verify
.pause
xdcoff cursor=1
![7m      Cursor disappears
.pause
xdcon cursor=1 form=4 blink=0
![7m      Cursor is a boxed MIPL
.pause
xdcset cursor=1 x=400 y=300
xdclocation cursor=1
![7m      Cursor is located at 400,300
.pause
xdcoff cursor=1
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
![7m      Cursor and pixel disappear
.pause
![7m  
![7mAdvanced Display Device Interface
![7m  
xdiawset imp=1 left=1 top=1 right=320 bottom=240
xdimfill imp=1 value=255 mask=127
xdiawset imp=1 left=1 top=241 right=320 bottom=480
xdimlinewrite imp=1 'READ outline=0 mask=32
xdiawset imp=1 left=321 top=1 right=640 bottom=240
xdimawwrite imp=1 'READ outimg=0 mask=127
xdiawset imp=1 left=321 top=241 right=640 bottom=480
xdimlinewrite imp=1 'READ outline=0 mask=15
![7m      Top left quadrant: a masked fill
![7m      Bottom left quadrant: a masked line write
![7m      Top right quadrant: a masked access window write 
![7m      Bottom right quadrant: a further masked line write
.pause
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdifill imp=1 value=0
xdimpixelwrite imp=1 x=350 y=400 value=255 mask=127
xdimpixelwrite imp=1 x=350 y=475 value=255 mask=63
xdipixelread imp=1 x=350 y=400
xdipixelread imp=1 x=350 y=475
![7m     Verify pixels written with mask are equal to mask
.pause
xdimpolyline imp=1 value=255 'CAN mask=63
xdimpolyline imp=1 value=255 'CAN mask=255
![7m     A couple masked cans (2 stars)
.pause
xdifill imp=1 value=0
xdimcircle imp=1 x=300 y=300 radius=100 value=255 mask=255
xdimcircle imp=1 x=250 y=250 radius=100 value=255 mask=127
![7m      Two circles: an unmasked  circle, then masked
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=300 y=300 radius=100 value=255
xdiareafill imp=1 x=300 y=300 boundry=255 fill=200
![7m      A filled circle
.pause
xdifill imp=1 value=0
xdipolyline imp=1 value=255 'CAN
xdicircle imp=2 x=300 y=300 radius=100 value=255
![7m      Verify red star and green circle
.pause
xdiiarithmetic op=0 imp1=1 imp2=2 imp3=3
![7m      Image plane 1 and 2 are added and placed in 3
![7m      Verify star turns magenta and circle turns cyan
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify star and circle turn blue
.pause
xdiishift shift=3 imp1=3 imp2=1 wrap=1
xdifill imp=3 value=0
![7m      Image plane 3 is shifted into 1
![7m      Verify both turn magenta then red
.pause
xdiicopy imp1=1 imp2=2
xdifill imp=1 value=0
![7m      Image plane 1 is copied into 2
![7m      Verify all turn yellow then green
.pause
xdicircle imp=1 x=400 y=400 radius=100 value=255
xdiilogical op=1 imp1=1 imp2=2 imp3=3
![7m      Notice new circle is not displayed past line 480
![7m      New red circle in 1 is ORed with 2 and placed in 3
![7m      Verify star and old circle turn cyan,
![7m      new circle turns magenta
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify image plane 3 contains all three
.pause
xdirotate imp=3 angle=1
![7m      Image plane 3 is rotated 180 degrees 
![7m      Notice only a square 480x480 section is rotated
![7m      Expect error message "[VRDI-AWNOTSQ] Access Window
![7m      is not square."
.pause
xdihistogram imp=3 mask=0
![7m      Histogram collected, 1395 in 255, rest in zero
.pause
xdifill imp=3 value=0
![7m  
![7mLimit testing - Expect ERROR MESSAGES on all calls (except xddinfo)
![7m  
xddinfo start=10 number=4
xddconfigure config=(0,3,0,0)
xddconfigure config=(0,0,2,0)
xddinfo start=4 number=1
xdiawwrite imp=33 image=0
xddinfo start=3 number=1
xdlramp lut=4 section=1
xddinfo start=24 number=1
xdlramp lut=1 section=5
xddinfo start=38 number=1
xdizoom imp=1 zoom=9
![7m  
![7mNo AFG - Expect ERROR MESSAGES on all calls 
![7m  
xdaclear x=1 y=1 nchars=5
xdaoff
xdaon 
xdatext x=1 y=1 length=4 text=none blink=0 reverse=0
![7m  
![7mClose - an unsupported function on an ADAGE - Expect an ERROR MESSAGE
![7m  
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstadagespec.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
gen ram1. 480 640
gen ram2. 480 640 ival=64
gen ram3. 480 640 ival=128
enable-script tstadagespec.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstadagespec.scr
vrditest (ram1.,ram2.,ram3.)
![7m  
![7m  The following script tests the special functions of 
![7m  the ADAGE Ikonas. The script will pause after 
![7m  executing one or more routines for verification. At a 
![7m  pause, check for correctness and press <return> to continue.
![7m  
![7m  Setup and cleanup
xdeaction warn=2 error=2 fatal=2
xddunit
xddopen
xddactivate flag=1
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
xddconfigure config=(0,2,3,2)
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdiawset imp=2 left=1 top=1 right=640 bottom=480
xdiawset imp=3 left=1 top=1 right=640 bottom=480
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
xdifill imp=4 value=0
![7m  640x480 color image displayed
.pause
xddconfigure config=(0,0,0,0)
![7m  Switched to 512x512
.pause
![7m  
xddconfigure config=(0,2,3,2)
![7m  Switched back to 640x480
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xddconfigure config=(0,0,0,0)
loadimages (images:mandrill.red,images:mandrill.grn,images:mandrill.blu)
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m  Switched to 512x512 and displayed image
.pause
xddconfigure config=(0,2,2,0)
loadimages (images:tm3.img,images:tm2.img,images:tm1.img)
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m  Switched to 1024x1024 and displayed image
.pause
xddconfigure config=(0,0,0,0)
![7m  Switched to 512x512
![7m  Patience is requested - writing to all 32 planes....
loadimages (images:mandrill.red,images:mandrill.grn,images:mandrill.blu)
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
xdiawwrite imp=13 'IMAGE image=0
xdiawwrite imp=14 'IMAGE image=1
xdiawwrite imp=15 'IMAGE image=2
xdiawwrite imp=21 'IMAGE image=0
xdiawwrite imp=22 'IMAGE image=1
xdiawwrite imp=23 'IMAGE image=2
xdiawwrite imp=25 'IMAGE image=0
xdiawwrite imp=26 'IMAGE image=1
xdiawwrite imp=27 'IMAGE image=2
loadimages (images:mndrl.red,images:mndrl.grn,images:mndrl.blu)
xdiawwrite imp=5 'IMAGE image=0
xdiawwrite imp=6 'IMAGE image=1
xdiawwrite imp=7 'IMAGE image=2
xdiawwrite imp=29 'IMAGE image=0
xdiawwrite imp=30 'IMAGE image=1
xdiawwrite imp=31 'IMAGE image=2
loadimages (images:bridge.red,images:bridge.grn,images:bridge.blu)
xdiawwrite imp=9 'IMAGE image=0
xdiawwrite imp=10 'IMAGE image=1
xdiawwrite imp=11 'IMAGE image=2
xdiawwrite imp=17 'IMAGE image=0
xdiawwrite imp=18 'IMAGE image=1
xdiawwrite imp=19 'IMAGE image=2
xdglwrite section=1
xdgon
loopgraph imps=32
xdidwset imp=1 left=256 top=256
![7m  You should see a quadrant of each group (imps 1 thru 16)
.pause
xdlconnect imp=17 lut=1 section=1 bypass=0
xdidwset imp=1 left=1 top=1
![7m  You should see a quadrant of each group (imps 17 thru 32)
![7m  Ready to go?
.pause
![7m  Loops through 32 image planes (4 at a time, in color)
![7m  (I've added a speed check so it doesn't go by so fast)
looploop imps=32 speed=30 'COLOR
![7m  Ready to go again?
.pause
![7m  Loops through 32 image planes (1 at a time, in bw)
looploop imps=32 speed=30 'BW
exit
$!-----------------------------------------------------------------------------
$ create tstcursor.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
enable-script tstcursor.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstcursor.scr
vrditest (images:mandrill.red, images:mandrill.grn, images:mandrill.blu)
![7m  The following script tests the function of each VRDI      
![7m  Cursor Routine.  The script will pause after executing    
![7m  one or more routines for verification.  At a pause,       
![7m  check for correctness and press <RETURN> to continue.     
xdeaction warn=2 error=2 fatal=2
xddunit
xddopen
xddactivate flag=1
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdifill imp=4 value=0
![7m                      
![7m  Begin Cursor Tests  
![7m                      
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
xdcon cursor=2 form=2 blink=1
xdcset cursor=2 x=256 y=256
xdclocation cursor=2
![7m      Cursors are ON, Cursor 1 is Autotracking and 5-point form, 
![7m      Cursor 2 is crosshatch form, blinking, and located at      
![7m      256,256.  Use the trackball and verify Cursor 1 moves.     
.pause
xdcautotrack cursor=1 device=1 flag=0
xdcautotrack cursor=2 device=1 flag=1
![7m      Cursor 1 Autotracking is OFF,     
![7m      Cursor 2 Autotracking is ON,      
![7m      Move trackball around to verify.  
.pause
xdcautotrack cursor=2 device=1 flag=0
xdcoff cursor=1
xdcoff cursor=2
![7m      Cursors disappear       
.pause
xdcshow cursor=1
xdcshow cursor=2
![7m      Cursors reappear        
.pause
xdcoff cursor=2
![7m      Cursor 2 disappears     
.pause
xdcon cursor=1 form=8 blink=2
![7m      Cursor is a full-screen blinking crosshatch    
.pause
xdcautotrack cursor=1 device=1 flag=1
xdcon cursor=1 form=4 blink=0
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
xdcset cursor=1 x=256 y=256
xdcraw2imp imp=1 xraw=256 yraw=256
![7m      Returned coordinates should be X=256, Y=256      
.pause
xdcimp2raw imp=2 ximp=256 yimp=256
![7m      Returned coordinates should be X=256, Y=256      
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
xdizoom imp=3 zoom=2
xdcraw2imp imp=3 xraw=256 yraw=256
![7m      Returned coordinates should be X=128, Y=128      
.pause
xdcimp2raw imp=3 ximp=256 yimp=256
![7m      Returned coordinates should be X=511, Y=511      
.pause
xdizoom imp=3 zoom=1
xdciset cursor=1 x=100 y=100 imp=3
xdcilocation cursor=1 imp=3
xdclocation cursor=1
![7m      Returned coordinates should be X=100, Y=100      
.pause
xdifill imp=3 value=0
xdiawwrite imp=2 'IMAGE image=1
xdizoom imp=2 zoom=3
xdcset cursor=1 x=256 y=256
xdcilocation cursor=1 imp=2
![7m      Returned coordinates should be X=86, Y=86        
.pause
xdciset cursor=1 x=85 y=85 imp=2
xdcilocation cursor=1 imp=2
![7m      Returned coordinates should be X=85, Y=85        
.pause
xdclocation cursor=1
![7m      Returned coordinates should be X=253, Y=253      
.pause
xdcraw2imp imp=2 xraw=256 yraw=256
![7m      Returned coordinates should be X=86, Y=86        
.pause
xdcimp2raw imp=2 ximp=256 yimp=256
![7m      Returned coordinates should be X=254, Y=254 for  
![7m      a 512 x 512 display.  Coordinates should be      
![7m      X=766, Y=766 for a 1024 x 1024 display.          
.pause
xdizoom imp=2 zoom=1
xdifill imp=2 value=0
xdiawwrite imp=1 'IMAGE image=0
xdidwset imp=1 left=50 top=50
xdciset cursor=1 x=256 y=256 imp=1
xdcilocation cursor=1 imp=1
![7m      Returned coordinates should be X=256, Y=256      
.pause
xdclocation cursor=1
![7m      Returned coordinates should be X=207, Y=207      
.pause
xdcraw2imp imp=1 xraw=256 yraw=256
![7m      Returned coordinates should be X=305, Y=305      
.pause
xdcimp2raw imp=1 ximp=256 yimp=256
![7m      Returned coordinates should be X=207, Y=207      
.pause
xdizoom imp=1 zoom=2
xdcset cursor=1 x=256 y=256
xdcilocation cursor=1 imp=1
![7m      Returned coordinates should be X=177, Y=177      
.pause
xdcraw2imp imp=1 xraw=256 yraw=256
![7m      Returned coordinates should be X=177, Y=177      
.pause
xdcimp2raw imp=1 ximp=256 yimp=256
![7m      Returned coordinates should be X=413, Y=413      
.pause
xdizoom imp=1 zoom=1
xdidwset imp=1 left=1 top=1
xdifill imp=1 value=0
xdcoff cursor=1
![7m          
![7m  Close   
![7m          
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstdeanza.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
enable-script tstdeanza.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstdeanza.scr
vrditest (images:mandrill.red,images:mandrill.grn,images:mandrill.blu)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on the DeAnza IP8500. The script will pause 
![7m  after executing one or more routines for verification. At a 
![7m  pause, check for correctness and press <return> to continue.
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Expect error code DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Configuration Routines
![7m  
xddunit
xddopen
xddactivate flag=1
![7m      First clean up
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdifill imp=4 value=0
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
xddconfigure config=(0,0,0,0)
xddinfo start=10 number=4
![7m      Default configuration is all 1's for IP-8500, [2] is 2
![7m      on an IP-85LX, and IP-9xxx is [1,3,2,1].
.pause
![7m  
![7mLookup Tables Routines
![7m  
![7m      Load pseudo color table into section 2
xdlwrite lut=1 section=2 canned=0
xdlwrite lut=2 section=2 canned=1
xdlwrite lut=3 section=2 canned=2
xdlread lut=1 section=2
![7m      Array locations:   0 - 127   value:   0
![7m                       128 - 159           84
![7m                       160 - 255          255
.pause
xdlread lut=2 section=2
![7m      Array locations:   0 -  31   value:   0
![7m                        32 -  63          128
![7m                        64 - 127          255
![7m                       128 - 159          200
![7m                       160 - 191          255
![7m                       192 - 223          128
![7m                       224 - 127            0
.pause
xdlread lut=3 section=2
![7m      Array locations:   0         value:   0
![7m                         1 -  95          255
![7m                        96 - 255            0
.pause
![7m      Load linear ramps and connect for color
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m  
![7mGraphics Overlay Plane Routines
![7m  
xddinfo start=30 number=7
![7m      Graphics as initialized
.pause
xdgconnect imp=4 section=1 bypass=0
xdglconstant section=1 red=255 green=255 blue=255
xdglread section=1
![7m      Overlay Lut written was 255.  Array location 0 equals 0.
![7m      As only upper 4 bits are used, remaining returned values
![7m      equal 240.  Only first 16 values read.
.pause
xdglwrite section=1
xdglread section=1
![7m      Overlay Lut written with assorted colors. Verify
![7m      that it's changed from previous constants.
.pause
xdgon
![7m  
![7mImage Memory Plane Routines
![7m  
xdiawset imp=1 left=1 right=512 top=1 bottom=512
xdiawset imp=2 left=1 right=512 top=1 bottom=512
xdiawset imp=3 left=1 right=512 top=1 bottom=512
xdiawset imp=4 left=1 right=512 top=1 bottom=512
xdiawlocation imp=1
xdiawlocation imp=2
xdiawlocation imp=3
xdiawlocation imp=4
![7m      Access Windows are all 1, 1, 512, 512
.pause
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m      Color image is displayed
.pause
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, and rewritten
![7m      (using access window read/write)
.pause
xdiawset imp=1 left=101 top=1 right=356 bottom=256
xdiawset imp=2 left=101 top=1 right=356 bottom=256
xdiawset imp=3 left=101 top=1 right=356 bottom=256
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdiawset imp=2 left=1 top=1 right=512 bottom=512
xdiawset imp=3 left=1 top=1 right=512 bottom=512
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawset imp=1 left=1 top=1 right=256 bottom=256
xdiawset imp=2 left=1 top=1 right=256 bottom=256
xdiawset imp=3 left=1 top=1 right=256 bottom=256
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, 
![7m      256x256 section of image appears in left top quadrant
.pause
xdiawset imp=1 left=257 top=257 right=512 bottom=512
xdiawset imp=2 left=257 top=257 right=512 bottom=512
xdiawset imp=3 left=257 top=257 right=512 bottom=512
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      256x256 section of image appears in right bottom quadrant
.pause
xdiawset imp=1 left=129 top=129 right=384 bottom=384
xdiawset imp=2 left=129 top=129 right=384 bottom=384
xdiawset imp=3 left=129 top=129 right=384 bottom=384
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      256x256 section of image appears in center
.pause
xdlconnect imp=1 lut=2 section=1 bypass=0
xdlconnect imp=1 lut=3 section=1 bypass=0
![7m      Turns BW
.pause
xdlconnect imp=1 lut=1 section=2 bypass=0
xdlconnect imp=2 lut=2 section=2 bypass=0
xdlconnect imp=3 lut=3 section=2 bypass=0
![7m      Checkout 8 color pseudo color
.pause
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m      Color again
.pause
xdilineread imp=1 outline=0
xdilineread imp=2 outline=1
xdilineread imp=3 outline=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdilinewrite imp=1 'READ outline=0
xdilinewrite imp=2 'READ outline=1
xdilinewrite imp=3 'READ outline=2
![7m      Planes have been erased, and rewritten
![7m      (using line read/write)
.pause
xdiawset imp=2 left=1 top=1 right=512 bottom=512
xdiawset imp=3 left=1 top=1 right=512 bottom=512
xdifill imp=2 value=0
xdifill imp=3 value=0
![7m      Green and Blue planes erased
xdidwlocation imp=1
![7m      Display window initialized at 1, 1
.pause
xdidwset imp=1 left=129 top=129
![7m      Red plane moved up and left 128 pixels
.pause
xdidwset imp=1 left=1 top=129
![7m      Red plane moved up 128 pixels
.pause
![7m      Reinitialize Access and Display windows and Erase
xdidwset imp=1 left=1 top=1
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdifill imp=1 value=0
xdipixelwrite imp=1 x=250 y=125 value=250
xdipixelwrite imp=1 x=250 y=175 value=251
xdipixelread imp=1 x=250 y=125
xdipixelread imp=1 x=250 y=175
![7m     Verify pixels written are pixels read
.pause
xdifill imp=1 value=0
xdipolyline imp=4 value=1 'CAN
xdipolyline imp=4 value=2 'CAN
xdipolyline imp=4 value=3 npts=2 x=(200,400) y=(200,200) '0
xdipolyline imp=4 value=4 npts=2 x=(400,400) y=(200,400) '0
xdipolyline imp=4 value=5 npts=2 x=(400,200) y=(400,400) '0
xdipolyline imp=4 value=6 npts=2 x=(200,200) y=(400,200) '0
xdicircle imp=4 x=300 y=300 radius=50 value=7
![7m      Canned array of vectors (a green and a red star),
![7m      series of vectors forming a square, and a centered circle
.pause
xdgoff
![7m      Graphics are OFF
.pause
xdgon
xdifill imp=4 value=0
xdttext imp=1 x=50 y=50 loc=1 nchars=0 text="ZOOM"
xdizoom imp=1 zoom=2
xdizoom imp=1 zoom=3
xdizoom imp=1 zoom=1
![7m      Text "Zoom" zooms and resumes original size
.pause
xdifill imp=1 value=0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=45.
xdtsize height=14 scale=1.
xdttext imp=1 x=150 y=150 loc=1 nchars=0 text="45 DEGREES"
xdtmask mask=127
xdttext imp=1 x=170 y=170 loc=1 nchars=0 text="MASKED"
![7m      Text "45 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdtfont font=1
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=-60.
xdtsize height=18 scale=1.
xdtmask mask=255
xdttext imp=1 x=200 y=200 loc=2 nchars=0 text="-60 DEGREES"
xdtmask mask=127
xdttext imp=1 x=220 y=220 loc=2 nchars=6 text="MASKED"
![7m      Text "-60 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdifill imp=1 value=0
![7m  
![7mCursor and IO Device Routines
![7m  
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
xdcon cursor=2 form=2 blink=1
xdcset cursor=2 x=256 y=256
xdclocation cursor=2
![7m      Cursors are ON, Cursor 1 is Autotracking and 5-point form,
![7m      Cursor 2 is crosshatch form, blinking, and 
![7m      located at 256,256
![7m      Use the trackball and move Cursor 1 over Cursor 2
.pause
![7m      Press button 1 on the trackball
xdx2d device=1
![7m      Pen and Prox are ON
![7m      X and Y should be close to zero
.pause
xdcautotrack cursor=1 device=1 flag=0
xdcautotrack cursor=2 device=1 flag=1
![7m      Cursor 1 Autotracking is OFF,
![7m      Cursor 2 Autotracking is ON,
![7m      Move trackball around to verify
.pause
xdcautotrack cursor=2 device=1 flag=0
xdcoff cursor=1
xdcoff cursor=2
![7m      Cursors disappear
.pause
xdcshow cursor=1
xdcshow cursor=2
![7m      Cursors reappear
.pause
xdcoff cursor=2
![7m      Cursor 2 disappears
.pause
xdcon cursor=1 form=8 blink=2
![7m      Cursor is a full-screen blinking crosshatch
.pause
xdcautotrack cursor=1 device=1 flag=1
![7m      Press the second button on the trackball
xdxswitch device=1 switch=2
![7m      Switch is ON
.pause
xdcoff cursor=1
![7m      Cursor 1 disappears
.pause
![7m  
![7mAdvanced Display Device Interface
![7m  
xdiawset imp=1 left=1 top=1 right=256 bottom=256
xdimfill imp=1 value=255 mask=127
xdiawset imp=1 left=1 top=257 right=256 bottom=512
xdimfill imp=1 value=255 mask=63
xdiawset imp=1 left=257 top=1 right=512 bottom=256
xdimawwrite imp=1 'READ outimg=0 mask=127
xdiawset imp=1 left=257 top=257 right=512 bottom=512
xdimawwrite imp=1 'READ outimg=0 mask=63
![7m      Top left quadrant: a masked fill
![7m      Bottom left quadrant: a further masked fill
![7m      Top right quadrant: a masked access window write 
![7m      Bottom right quadrant: a further masked access window write
.pause
xdiawset imp=1 left=1 top=1 right=256 bottom=256
xdimlinewrite imp=1 'READ outline=0 mask=32
xdiawset imp=1 left=1 top=257 right=256 bottom=512
xdimlinewrite imp=1 'READ outline=0 mask=15
xdiawset imp=1 left=1 top=1 right=512 bottom=512
![7m      Top left quadrant: a masked line write
![7m      Bottom left quadrant: a further masked line write
.pause
xdifill imp=1 value=0
xdimpixelwrite imp=1 x=350 y=400 value=255 mask=127
xdimpixelwrite imp=1 x=350 y=475 value=255 mask=63
xdipixelread imp=1 x=350 y=400
xdipixelread imp=1 x=350 y=475
![7m     Verify pixels written with mask are equal to mask
.pause
xdimpolyline imp=1 value=255 'CAN mask=63
xdimpolyline imp=1 value=255 'CAN mask=255
![7m     A couple masked cans (2 stars)
.pause
xdifill imp=1 value=0
xdimcircle imp=1 x=300 y=300 radius=100 value=255 mask=255
xdimcircle imp=1 x=250 y=250 radius=100 value=255 mask=127
xdimcircle imp=1 x=200 y=200 radius=100 value=255 mask=63
xdimcircle imp=1 x=150 y=150 radius=100 value=255 mask=31
![7m      Four circles: an unmasked  circle, then masked,
![7m                    more masked, and then almost gone.
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=300 y=300 radius=100 value=255
xdiareafill imp=1 x=300 y=300 boundry=255 fill=200
![7m      A filled circle
.pause
xdifill imp=1 value=0
xdipolyline imp=1 value=255 'CAN
xdicircle imp=2 x=300 y=300 radius=100 value=255
![7m      Verify red star and green circle
.pause
xdiiarithmetic op=0 imp1=1 imp2=2 imp3=3
![7m      Image plane 1 and 2 are added and placed in 3
![7m      Verify star turns magenta and circle turns cyan
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify star and circle turn blue
.pause
xdiishift shift=3 imp1=3 imp2=1 wrap=1
xdifill imp=3 value=0
![7m      Image plane 3 is shifted into 1
![7m      Verify both turn magenta then red
.pause
xdiicopy imp1=1 imp2=2
xdifill imp=1 value=0
![7m      Image plane 1 is copied into 2
![7m      Verify all turn yellow then green
.pause
xdicircle imp=1 x=400 y=400 radius=100 value=255
xdiilogical op=1 imp1=1 imp2=2 imp3=3
![7m      New red circle in 1 is ORed with 2 and placed in 3
![7m      Verify star and old circle turn cyan,
![7m      new circle turns magenta
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify image plane 3 contains all three
![7m      (end up blue)
.pause
xdirotate imp=3 angle=1
![7m      Image plane is rotated 180 degrees 
.pause
xdihistogram imp=3 mask=0
![7m      Histogram collected, 1514 in 255, rest in 0, 0 everywhere else.
.pause
xdifill imp=3 value=0
![7m  
![7mLimit testing - Expect ERROR MESSAGES on all calls (except xddinfo)
![7m  
xddinfo start=10 number=4
xddconfigure config=(0,3,0,0)
xddconfigure config=(0,0,2,0)
xddinfo start=4 number=1
xdiawwrite imp=5 image=0
xddinfo start=3 number=1
xdlramp lut=4 section=1
xddinfo start=24 number=1
xdlramp lut=1 section=5
xddinfo start=38 number=1
xdizoom imp=1 zoom=9
![7m  
![7mNo AFG - Expect ERROR MESSAGES on all calls 
![7m  
xdaclear x=1 y=1 nchars=5
xdaoff
xdaon 
xdatext x=1 y=1 length=4 text=none blink=0 reverse=0
![7m  
![7mClose
![7m  
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstdeanzahi.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
enable-script tstdeanzahi.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstdeanzahi.scr
vrditest.cpd (ip1.,ip2.,ip3.)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on the DeAnza IP8500HI. The script will pause 
![7m  after executing one or more routines for verification. At a 
![7m  pause, check for correctness and press <return> to continue.
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Error Code is DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Configuration Routines
![7m  
xddunit
xddopen
xddactivate flag=1
![7m      First clean up
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdifill imp=4 value=0
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
xddconfigure config=(0,0,0,0)
xddinfo start=10 number=4
![7m      Default configuration is 1 3 2 1
.pause
![7m  
![7mLookup Tables Routines
![7m  
![7m      Load pseudo color table into section 2
xdlwrite lut=1 section=2 canned=0
xdlwrite lut=2 section=2 canned=1
xdlwrite lut=3 section=2 canned=2
xdlread lut=1 section=2
![7m      Array locations:   0 - 127   value:   0
![7m                       128 - 159           84
![7m                       160 - 255          255
.pause
xdlread lut=2 section=2
![7m      Array locations:   0 -  31   value:   0
![7m                        32 -  63          128
![7m                        64 - 127          255
![7m                       128 - 159          200
![7m                       160 - 191          255
![7m                       192 - 223          128
![7m                       224 - 127            0
.pause
xdlread lut=3 section=2
![7m      Array locations:   0         value:   0
![7m                         1 -  95          255
![7m                        96 - 255            0
.pause
![7m      Load linear ramps and connect for color
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m  
![7mGraphics Overlay Plane Routines
![7m  
xddinfo start=30 number=7
![7m      Graphics as initialized
.pause
xdgconnect imp=4 section=1 bypass=0
xdglconstant section=1 red=255 green=255 blue=255
xdglread section=1
![7m      Overlay Lut written was 255. Only first 16 values read.
![7m      The first value (index 0) is 0.  All others are 255.
.pause
xdglwrite section=1
xdglread section=1
![7m      Overlay Lut written with assorted colors. Verify
![7m      that it's changed from previous constants.
.pause
xdgon
![7m  
![7mImage Memory Plane Routines
![7m  
xdiawlocation imp=1
xdiawlocation imp=2
xdiawlocation imp=3
xdiawlocation imp=4
![7m      Access Windows are all 1, 1, 2048, 2048
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdiawset imp=4 left=1 top=1 right=1024 bottom=1024
xdiawlocation imp=1
xdiawlocation imp=2
xdiawlocation imp=3
xdiawlocation imp=4
![7m      Access Windows are all 1, 1, 1024, 1024
.pause
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m      Color image is displayed
.pause
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, and rewritten
![7m      (using access window read/write)
.pause
xdiawset imp=1 left=101 top=1 right=612 bottom=512
xdiawset imp=2 left=101 top=1 right=612 bottom=512
xdiawset imp=3 left=101 top=1 right=612 bottom=512
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdiawset imp=2 left=1 top=1 right=512 bottom=512
xdiawset imp=3 left=1 top=1 right=512 bottom=512
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, 
![7m      512x512 section of image appears in left top quadrant
.pause
xdiawset imp=1 left=513 top=513 right=1024 bottom=1024
xdiawset imp=2 left=513 top=513 right=1024 bottom=1024
xdiawset imp=3 left=513 top=513 right=1024 bottom=1024
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      512x512 section of image appears in right bottom quadrant
.pause
xdiawset imp=1 left=257 top=257 right=768 bottom=768
xdiawset imp=2 left=257 top=257 right=768 bottom=768
xdiawset imp=3 left=257 top=257 right=768 bottom=768
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      512x512 section of image appears in center
.pause
xddconfigure config=(3,0,0,0)
![7m      Turns BW
.pause
xddconfigure config=(1,0,0,0)
![7m      Turns color
.pause
xdlconnect imp=1 lut=1 section=2 bypass=0
xdlconnect imp=2 lut=2 section=2 bypass=0
xdlconnect imp=3 lut=3 section=2 bypass=0
![7m      Checkout 8 color pseudo color (black background)
.pause
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m      Color again
.pause
xdilineread imp=1 outline=0
xdilineread imp=2 outline=1
xdilineread imp=3 outline=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdilinewrite imp=1 'READ outline=0
xdilinewrite imp=2 'READ outline=1
xdilinewrite imp=3 'READ outline=2
![7m      Planes have been erased, and rewritten
![7m      (using line read/write)
.pause
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdifill imp=2 value=0
xdifill imp=3 value=0
![7m      Green and Blue planes erased
xdidwlocation imp=1
![7m      Display window initialized at 1, 1
.pause
xdidwset imp=1 left=129 top=129
![7m      Red plane moved up and left 128 pixels
.pause
xdidwset imp=1 left=1 top=129
![7m      Red plane moved up 128 pixels
.pause
![7m      Reinitialize Access and Display windows and Erase
xdidwset imp=1 left=1 top=1
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdifill imp=1 value=0
xdipixelwrite imp=1 x=250 y=125 value=250
xdipixelwrite imp=1 x=250 y=175 value=251
xdipixelread imp=1 x=250 y=125
xdipixelread imp=1 x=250 y=175
![7m     Verify pixels written are pixels read
.pause
![7m     Now testing extended memory
xdifill imp=1 value=0
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m     Color image
.pause
xdiawset imp=1 left=1025 top=1025 right=2048 bottom=2048
xdiawset imp=2 left=1025 top=1025 right=2048 bottom=2048
xdiawset imp=3 left=1025 top=1025 right=2048 bottom=2048
xdidwset imp=1 left=1025 top=1025
xdidwset imp=2 left=1025 top=1025
xdidwset imp=3 left=1025 top=1025
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m     Verify that the screen blanks then the color image
![7m     is redrawn.
.pause
xdidwset imp=1 left=513 top=513
xdidwset imp=2 left=513 top=513
xdidwset imp=3 left=513 top=513
![7m     Top left and bottom right quadrants contain a part of
![7m     the image; top right and bottom left quadrants are blank.
.pause
xdiawset imp=1 left=1 top=1 right=2048 bottom=2048
xdiawset imp=3 left=1 top=1 right=2048 bottom=2048
xdifill imp=1 value=0
xdifill imp=3 value=0
![7m     Red and blue planes clear, leaving only green.
.pause
xdiawset imp=2 left=1 top=897 right=2048 bottom=1152
xdiawread imp=2 outimg=1
xdifill imp=2 value=0
xdiawset imp=2 left=1 top=641 right=2048 bottom=896
xdiawwrite imp=2 'READ outimg=1
xdiawset imp=2 left=1 top=1153 right=2048 bottom=1408
xdiawwrite imp=2 'READ outimg=1
![7m     A 256 line section from the center of the image is erased
![7m     on the green plane and redrawn just above and just below
![7m     the erased area, across the entire 2048 sample image plane.
![7m     This is the center section.
.pause
xdidwset imp=2 left=1 top=513
![7m     The pattern from the left half of the previous display
![7m     should now continue over the entire display.  This is the
![7m     left half of the 2048 sample image plane.
.pause
xdidwset imp=2 left=1025 top=513
![7m     The pattern from the right half of the previous center
![7m     section display should now continue over the entire display.
![7m     This is the right half of the 2048 sample image plane.
.pause
xdiawset imp=2 left=1 top=1 right=2048 bottom=2048
xdifill imp=2 value=0
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdidwset imp=1 left=1 top=1
xdidwset imp=2 left=1 top=1
xdidwset imp=3 left=1 top=1
![7m     Verify that the screen is clear.
xdifill imp=1 value=0
xdipolyline imp=4 value=1 'CAN
xdipolyline imp=4 value=2 'CAN
xdipolyline imp=4 value=3 npts=2 x=(400,800) y=(400,400) '0
xdipolyline imp=4 value=4 npts=2 x=(800,800) y=(400,800) '0
xdipolyline imp=4 value=5 npts=2 x=(800,400) y=(800,800) '0
xdipolyline imp=4 value=6 npts=2 x=(400,400) y=(800,400) '0
xdicircle imp=4 x=600 y=600 radius=100 value=7
![7m      Canned array of vectors (a red and a green star),
![7m      series of vectors forming a square, and a centered circle
.pause
xdgoff
![7m      Graphics are OFF
.pause
xdgon
xdifill imp=4 value=0
xdttext imp=1 x=100 y=100 loc=1 nchars=0 text="ZOOM"
xdizoom imp=1 zoom=2
xdizoom imp=1 zoom=3
xdizoom imp=1 zoom=1
![7m      Text "Zoom" zooms and resumes original size
.pause
xdifill imp=1 value=0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=45.
xdtsize height=14 scale=1.
xdttext imp=1 x=150 y=150 loc=1 nchars=0 text="45 DEGREES"
xdtmask mask=127
xdttext imp=1 x=170 y=170 loc=1 nchars=0 text="MASKED"
![7m      Text "45 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdtfont font=1
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=-60.
xdtsize height=18 scale=1.
xdtmask mask=255
xdttext imp=1 x=200 y=200 loc=2 nchars=0 text="-60 DEGREES"
xdtmask mask=127
xdttext imp=1 x=220 y=220 loc=2 nchars=6 text="MASKED"
![7m      Text "-60 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdifill imp=1 value=0
![7m  
![7mCursor and IO Device Routines
![7m  
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
xdcon cursor=2 form=4 blink=1
xdcset cursor=2 x=512 y=512
xdclocation cursor=2
![7m      Cursors are ON, Cursor 1 is Autotracking and + form,
![7m      Cursor 2 is dashed + form, blinking, and located at 512,512
![7m      Use the trackball and move Cursor 1 over Cursor 2
.pause
![7m      Press button 1 on the trackball
xdx2d device=1
![7m      Pen and Prox are ON
![7m      X and Y should be close to zero
.pause
xdcautotrack cursor=1 device=1 flag=0
xdcautotrack cursor=2 device=1 flag=1
![7m      Cursor 1 Autotracking is OFF,
![7m      Cursor 2 Autotracking is ON,
![7m      Move trackball around to verify
.pause
xdcautotrack cursor=2 device=1 flag=0
xdcoff cursor=1
xdcoff cursor=2
![7m      Cursors disappear
.pause
xdcon cursor=1 form=5 blink=2
![7m      Cursor is a full-screen dashed blinking crosshatch
.pause
xdcautotrack cursor=1 device=1 flag=1
![7m      Press the second button on the trackball
xdxswitch device=1 switch=2
![7m      Switch is ON
.pause
xdcoff cursor=1
![7m      Cursor disappears
.pause
![7m  
![7mAdvanced Display Device Interface
![7m  
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdimfill imp=1 value=255 mask=127
xdiawset imp=1 left=1 top=513 right=512 bottom=1024
xdimfill imp=1 value=255 mask=63
xdiawset imp=1 left=513 top=1 right=1024 bottom=512
xdimawwrite imp=1 'READ outimg=0 mask=127
xdiawset imp=1 left=513 top=513 right=1024 bottom=1024
xdimawwrite imp=1 'READ outimg=0 mask=63
![7m      Top left quadrant: a masked fill
![7m      Bottom left quadrant: a further masked fill
![7m      Top right quadrant: a masked access window write 
![7m      Bottom right quadrant: a further masked access window write
.pause
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdimlinewrite imp=1 'READ outline=0 mask=32
xdiawset imp=1 left=1 top=513 right=512 bottom=1024
xdimlinewrite imp=1 'READ outline=0 mask=15
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
![7m      Top left quadrant: a masked line write
![7m      Bottom left quadrant: a further masked line write
.pause
xdifill imp=1 value=0
xdimpixelwrite imp=1 x=350 y=400 value=255 mask=127
xdimpixelwrite imp=1 x=350 y=475 value=255 mask=63
xdipixelread imp=1 x=350 y=400
xdipixelread imp=1 x=350 y=475
![7m     Verify pixels written with mask are equal to mask
.pause
xdimpolyline imp=1 value=255 'CAN mask=63
xdimpolyline imp=1 value=255 'CAN mask=255
![7m     A couple masked cans (2 stars)
.pause
xdifill imp=1 value=0
xdimcircle imp=1 x=300 y=300 radius=100 value=255 mask=255
xdimcircle imp=1 x=250 y=250 radius=100 value=255 mask=127
xdimcircle imp=1 x=200 y=200 radius=100 value=255 mask=63
xdimcircle imp=1 x=150 y=150 radius=100 value=255 mask=31
![7m      Four circles: an unmasked  circle, then masked,
![7m                    more masked, and then almost gone.
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=300 y=300 radius=100 value=255
xdiareafill imp=1 x=300 y=300 boundary=255 fill=200
![7m      A filled circle
.pause
xdifill imp=1 value=0
xdipolyline imp=1 value=255 'CAN
xdicircle imp=2 x=300 y=300 radius=100 value=255
![7m      Verify red star and green circle
.pause
xdiiarithmetic op=0 imp1=1 imp2=2 imp3=3
![7m      Image plane 1 and 2 are added and placed in 3
![7m      Verify star turns magenta and circle turns cyan
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify star and circle turn blue
.pause
xdiishift shift=3 imp1=3 imp2=1 wrap=1
xdifill imp=3 value=0
![7m      Image plane 3 is shifted into 1
![7m      Verify both turn magenta then red
.pause
xdiicopy imp1=1 imp2=2
xdifill imp=1 value=0
![7m      Image plane 1 is copied into 2
![7m      Verify all turn yellow then green
.pause
xdicircle imp=1 x=400 y=400 radius=100 value=255
xdiilogical op=1 imp1=1 imp2=2 imp3=3
![7m      New red circle in 1 is ORed with 2 and placed in 3
![7m      Verify star and old circle turn cyan,
![7m      new circle turns magenta
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify image plane 3 contains all three
![7m      (end up blue)
.pause
xdirotate imp=3 angle=1
![7m      Image plane is rotated 180 degrees 
.pause
xdihistogram imp=3 mask=0
![7m      Histogram collected, 1514 in 255, rest in zero
.pause
xdifill imp=3 value=0
![7m  
![7mLimit testing
![7m  
xddinfo start=10 number=4
xddconfigure config=(0,4,0,0)
xddconfigure config=(0,0,3,0)
xddinfo start=4 number=1
xdiawwrite imp=5 image=0
xddinfo start=3 number=1
xdlramp lut=4 section=1
xddinfo start=24 number=1
xdlramp lut=1 section=5
xddinfo start=38 number=1
xdizoom imp=1 zoom=9
![7m  
![7mNo Alphanumeric Font Generator
![7m  
xdaclear x=1 y=1 nchars=5
xdaoff
xdaon 
xdatext x=1 y=1 length=4 text=none blink=0 reverse=0
![7m  
![7mClose
![7m  
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstflag.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
enable-script tstflag.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstflag.scr
vrditest (images:mandrill.red,images:mandrill.grn,images:mandrill.blu)
![7m                                                                 
![7m  The following script tests the main functions of the VRDI      
![7m  Flag and Status routines.  The script will pause after         
![7m  executing one or more routines for verification. At a          
![7m  pause, check for correctness and press <RETURN> to continue.   
![7m                                                                 
xdeaction warn=2 error=2 fatal=2
xddunit
xdfregister group=1
xddopen
xddactivate flag=1
xddconfigure config=(0,0,0,0)
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
![7m     Clear flags    
xdfregister group=0
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdifill imp=4 value=0
xdfconfig
xdfglut
xdfimage imp=0
xdfimage imp=1
xdfimage imp=2
xdfimage imp=3
xdflut lut=0
xdflut lut=1
xdflut lut=2
xdflut lut=3
xdfregister group=1
![7m                          
![7m  Device Status Routines  
![7m                          
xdsnl
xdsns
xdsvnl
xdsvns
![7m      Verify the number of lines and samples in the image        
![7m      planes and on the video screen.                            
.pause
xdsmode
xdszoom imp=1
xdszoom imp=2
xdszoom imp=3
![7m      Verify the video display mode and the zoom factor.         
.pause
xdsgraph
xdsgsection
xdsgbypass
![7m      Verify the graphics image plane number, section = 1,       
![7m      and bypass = off.                                          
.pause
xdsimp lut=1
xdsimp lut=2
xdsimp lut=3
![7m      Verify image plane 1 connected to LUT 1, image plane       
![7m      2 connected to LUT 2, and image plane 3 connected to       
![7m      LUT 3.                                                     
.pause
xdssection lut=1
xdssection lut=2
xdssection lut=3
![7m      Verify that section=1 for all LUTs.                        
.pause
xdsbypass lut=1
xdsbypass lut=2
xdsbypass lut=3
![7m      Verify that bypass is off for all LUTs.                    
.pause
xdsdwline imp=1
xdsdwline imp=2
xdsdwline imp=3
xdsdwsamp imp=1
xdsdwsamp imp=2
xdsdwsamp imp=3
![7m      Verify that display window line and sample = 1 for each 
![7m      image plane.                                            
.pause
![7m
![7m                 
![7m  Flag Routines  
![7m                 
xdfregister group=0
xdfglut
xdflut lut=0
xdflut lut=1
xdflut lut=2
xdflut lut=3
xdfconfig
xdfimage imp=0
xdfimage imp=1
xdfimage imp=2
xdfimage imp=3
![7m      Verify that flags have not changed.    
.pause
![7m
![7m                                             
![7m  Overlay Status and Flag Routines           
![7m                                             
xdgconnect imp=4 section=2 bypass=0
xdsgsection
xdsgbypass
xdfregister group=0
xdfglut
![7m      Verify that section=2, bypass=off, and flag has changed.   
![7m      If error code NOLUTSECT is returned, verify that section=1,
![7m      bypass=off, and flag has not changed.                      
.pause
xdgconnect imp=4 section=1 bypass=0
xdfregister group=1
xdsgsection
xdfglut
![7m      Verify that section=1 and flag has changed.  If previous   
![7m      command returned NOLUTSECT, verify that section=1 and      
![7m      flag has not changed.                                      
.pause
xdfregister group=0
xdfglut
![7m      Verify that flag has not changed.      
.pause
xdfregister group=1
xdgconnect imp=1 section=1 bypass=0
xdfregister group=0
xdfconfig
![7m      Verify that flag has changed.          
.pause
xdfglut
![7m      Verify that flag has not changed.      
.pause
xdfregister group=1
xdgconnect imp=4 section=1 bypass=0
xdglconstant section=1 red=255 green=255 blue=255
xdfregister group=0
xdfglut
![7m      Verify that flag has changed.          
.pause
xdfregister group=1
xdglwrite section=1
xdfregister group=0
xdfglut
![7m      Verify that flag has changed.          
.pause
xdfglut
![7m      Verify that flag has not changed.      
.pause
xdfregister group=1
xdglinit section=1
xdfregister group=0
xdfglut
![7m      Verify that flag has changed.          
.pause
![7m
![7m                                   
![7m  Look-Up Table Flag Routines      
![7m                                   
xdfregister group=1
xddconfigure config=(2,-1,-1,-1)
xdfregister group=0
xdflut lut=0
xdflut lut=1
xdflut lut=2
xdflut lut=3
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xddconfigure config=(-1,-1,-1,-1)
xdfregister group=0
xdflut lut=0
xdflut lut=1
xdflut lut=2
xdflut lut=3
![7m      Verify that flags have not changed.    
.pause
xdfregister group=1
xdlconnect imp=1 lut=1 section=2 bypass=0
xdfregister group=0
xdflut lut=0
xdflut lut=1
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdlconnect imp=1 lut=1 section=2 bypass=0
xdfregister group=0
xdflut lut=0
xdflut lut=1
![7m      Verify that flags have not changed.    
.pause
xdfregister group=1
xdlconnect imp=1 lut=1 section=2 bypass=1
xdfregister group=0
xdflut lut=0
xdflut lut=1
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdfregister group=0
xdflut lut=0
xdflut lut=1
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdlconnect imp=2 lut=1 section=2 bypass=0
xdfregister group=0
xdflut lut=0
xdflut lut=1
xdflut lut=2
xdflut lut=3
![7m      Verify that flags have changed for lut=0 and lut=1.        
![7m      Verify that flags have not changed for lut=2 and lut=3.    
.pause
xdfconfig
![7m      Verify that flag has changed.          
.pause
xdfregister group=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlwrite lut=3 section=1 canned=0
xdfregister group=0
xdflut lut=0
xdflut lut=1
xdflut lut=2
xdflut lut=3
![7m      Verify that flags have changed for lut=0, 1, and 3.        
![7m      Verify that flag has not changed for lut=2.                
.pause
xdfregister group=1
xdlwrite lut=2 section=1 canned=1
xdfregister group=0
xdflut lut=0
xdflut lut=2
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdfregister group=0
xdflut lut=0
xdflut lut=1
xdflut lut=2
xdflut lut=3
![7m      Verify that flags have changed.        
.pause
xdflut lut=0
xdflut lut=1
xdflut lut=2
xdflut lut=3
![7m      Verify that flags have not changed.    
.pause
xdfregister group=1
xddconfigure config=(2,1,1,1)
xdfregister group=0
xdfconfig
![7m      Verify that flag has changed.          
.pause
xdfregister group=1
xddconfigure config=(0,0,0,0)
xdfregister group=0
xdfconfig
![7m      Verify that flag has changed.          
.pause
xdfregister group=1
xddconfigure config=(0,0,0,0)
xdfregister group=0
xdfconfig
![7m      Verify that flag has not changed.      
.pause
![7m
![7m                                   
![7m  Image Plane Flag Routines        
![7m                                   
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
xdfimage imp=2
xdfimage imp=3
![7m      Clear image plane flags.               
xdfregister group=1
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
xdfimage imp=2
xdfimage imp=3
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdifill imp=3 value=0
xdifill imp=2 value=0
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
xdfimage imp=2
xdfimage imp=3
![7m      Verify that flags have changed for imp=0, 2, and 3.        
![7m      Verify that flag has not changed for imp=1.                
.pause
xdfregister group=1
xdilinewrite imp=1 'IMAGE outline=0
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
xdfimage imp=2
xdfimage imp=3
![7m      Verify that flags have changed for imp=0 and imp=1.        
![7m      Verify that flags have not changed for imp=2 and imp=3.    
.pause
xdfregister group=1
xdifill imp=1
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
![7m      Clear screen and clear image plane flags.   
xdfregister group=1
xdicircle imp=2 x=300 y=300 radius=50 value=255
xdfregister group=0
xdfimage imp=0
xdfimage imp=2
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdiareafill imp=2 x=300 y=300 boundry=255 fill=200
xdfregister group=0
xdfimage imp=0
xdfimage imp=2
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdipixelwrite imp=1 x=200 y=200 value=255
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
xdfimage imp=2
![7m      Verify that flags have changed for imp=0 and imp=1.        
![7m      Verify that flag has not changed for imp=2.                
.pause
xdfregister group=1
xdipolyline imp=1 value=255 'CAN
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdiilogical op=1 imp1=1 imp2=2 imp3=3
xdfregister group=0
xdfimage 0
xdfimage 1
xdfimage 2
xdfimage 3
![7m      Verify that flags have changed for imp=0 and imp=3.        
![7m      Verify that flags have not changed for imp=1 and imp=2.    
.pause
xdfregister group=1
xdttext imp=3 x=100 y=100 loc=2 nchars=0 text="Testing..."
xdfregister group=0
xdfimage 0
xdfimage 3
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdiicopy imp1=3 imp2=2
xdfregister group=0
xdfimage imp=0
xdfimage imp=2
xdfimage imp=3
![7m      Verify that flags have changed for imp=0 and imp=2.        
![7m      Verify that flag has not changed for imp=3.                
.pause
xdfregister group=1
xdimcircle imp=2 x=200 y=200 radius=100 value=255 mask=128
xdfregister group=0
xdfimage imp=0
xdfimage imp=2
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdimpixelwrite imp=1 x=350 y=400 value=255 mask=128
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdiiarithmetic op=0 imp1=3 imp2=2 imp3=1
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
xdfimage imp=2
xdfimage imp=3
![7m      Verify that flags have changed for imp=0 and imp=1.        
![7m      Verify that flags have not changed for imp=2 and imp=3.    
.pause
xdfregister group=1
xdimfill imp=3 value=255 mask=127
xdfregister group=0
xdfimage imp=0
xdfimage imp=3
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdiishift shift=1 imp1=3 imp2=3 wrap=1
xdfregister group=0
xdfimage imp=0
xdfimage imp=3
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdirotate imp=1 angle=1
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdimawwrite imp=2 'IMAGE outimg=0 mask=128
xdfregister group=0
xdfimage imp=0
xdfimage imp=2
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdimlinewrite imp=1 'IMAGE outline=0 mask=128
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
![7m      Verify that flags have changed.        
.pause
xdfregister group=1
xdimpolyline imp=1 value=255 'CAN mask=128
xdfregister group=0
xdfimage imp=0
xdfimage imp=1
![7m      Verify that flags have changed.        
.pause
xdfimage imp=0
xdfimage imp=1
xdfimage imp=2
xdfimage imp=3
![7m      Verify that flags have not changed.    
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdifill imp=4 value=0
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstivas.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
enable-script tstivas.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstivas.scr
vrditest (images:tm3.img,images:tm2.img,images:tm1.img)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on the IIS IVAS. The script will pause after 
![7m  executing one or more routines for verification. At a pause, 
![7m  check for correctness and press <return> to continue.
![7m  VAX 2 is slow, have some coffee with you.
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Expect error code DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Configuration Routines
![7m  
xddunit
xddopen
xddactivate flag=1
xdifill 1 0
xdifill 2 0
xdifill 3 0
xdifill 4 0
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
xddconfigure config=(0,0,0,0)
xddinfo start=10 number=4
![7m      Default configuration is 1 2 2 1
.pause
![7m  
![7mLookup Tables Routines
![7m  
![7m      Load linear ramps and connect for color
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m  
![7mGraphics Overlay Plane Routines
![7m  
xddinfo start=30 number=7
![7m      Graphics as initialized
.pause
xdgon
xdglconstant section=1 red=255 green=255 blue=255
xdglwrite section=1
![7m      Overlay Lut written with assorted colors.
![7m  
![7mImage Memory Plane Routines
![7m  
xdiawlocation imp=1
xdiawlocation imp=2
xdiawlocation imp=3
xdiawlocation imp=4
![7m      Access Windows are all 1, 1, 1024, 1024
.pause
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m      Color image is displayed
.pause
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, and rewritten
![7m      (using access window read/write)
.pause
xdiawset imp=1 left=257 top=1 right=768 bottom=512
xdiawset imp=2 left=257 top=1 right=768 bottom=512
xdiawset imp=3 left=257 top=1 right=768 bottom=512
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdiawset imp=2 left=1 top=1 right=512 bottom=512
xdiawset imp=3 left=1 top=1 right=512 bottom=512
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, 
![7m      512x512 section of image appears in left top quadrant
.pause
xdiawset imp=1 left=513 top=513 right=1024 bottom=1024
xdiawset imp=2 left=513 top=513 right=1024 bottom=1024
xdiawset imp=3 left=513 top=513 right=1024 bottom=1024
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      512x512 section of image appears in right bottom quadrant
.pause
xdiawset imp=1 left=257 top=257 right=768 bottom=768
xdiawset imp=2 left=257 top=257 right=768 bottom=768
xdiawset imp=3 left=257 top=257 right=768 bottom=768
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      512x512 section of image appears in center
.pause
xdlconnect imp=1 lut=2 section=1 bypass=0
xdlconnect imp=1 lut=3 section=1 bypass=0
![7m      Turns BW
.pause
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m      Color again
.pause
![7m      Load pseudo color table
xdlwrite lut=1 section=1 canned=0
xdlwrite lut=2 section=1 canned=1
xdlwrite lut=3 section=1 canned=2
![7m      Pseudo color image (blue background)
.pause
![7m      Read of 8 color pseudo table
xdlread lut=2 section=1
![7m      Array locations:   0 -  31   value:   0
![7m                        32 -  63          128
![7m                        64 - 127          255
![7m                       128 - 159          200
![7m                       160 - 191          255
![7m                       192 - 223          128
![7m                       224 - 127            0
.pause
![7m      Back to linear ramps 
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdilineread imp=1 outline=0
xdilineread imp=2 outline=1
xdilineread imp=3 outline=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdilinewrite imp=1 'READ outline=0
xdilinewrite imp=2 'READ outline=1
xdilinewrite imp=3 'READ outline=2
![7m      Planes have been erased, and rewritten
![7m      (using line read/write)
.pause
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdifill imp=2 value=0
xdifill imp=3 value=0
![7m      Green and Blue planes erased
xdidwlocation imp=1
![7m      Display window initialized at 1, 1
![7m      Ignore following 'all display windows have been set' warning
.pause
xdidwset imp=1 left=257 top=257
![7m      Red plane moved up and left 256 pixels (wraps around)
.pause
xdidwset imp=1 left=1 top=257
![7m      Red plane moved up 256 pixels (wraps around)
.pause
xdidwset imp=1 left=257 top=1
![7m      Red plane moved left 256 pixels (wraps around)
.pause
![7m      Reinitialize Access and Display windows and Erase
xdidwset imp=1 left=1 top=1
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdifill imp=1 value=0
xdipixelwrite imp=1 x=250 y=125 value=250
xdipixelwrite imp=1 x=250 y=175 value=251
xdipixelread imp=1 x=250 y=125
xdipixelread imp=1 x=250 y=175
![7m     Verify pixels written are pixels read
.pause
xdifill imp=1 value=0
xdipolyline imp=3 value=255 npts=2 x=(1,1024) y=(1,1) '0
xdipolyline imp=3 value=255 npts=2 x=(1024,1024) y=(1,1024) '0
xdipolyline imp=3 value=255 npts=2 x=(1024,1) y=(1024,1024) '0
xdipolyline imp=3 value=255 npts=2 x=(1,1) y=(1024,1) '0
xdipolyline imp=3 value=255 npts=2 x=(1,1024) y=(1,1024) '0
xdipolyline imp=3 value=255 npts=2 x=(1,1024) y=(1024,1) '0
![7m      A series of vectors forming a blue border
![7m      around the edge of the screen, with a large
![7m      blue X from corner to corner.
.pause
xdifill imp=3 value=0
xdipolyline imp=4 value=1 'CAN
xdipolyline imp=4 value=2 'CAN
xdipolyline imp=4 value=3 npts=2 x=(200,400) y=(200,200) '0
xdipolyline imp=4 value=4 npts=2 x=(400,400) y=(200,400) '0
xdipolyline imp=4 value=5 npts=2 x=(400,200) y=(400,400) '0
xdipolyline imp=4 value=6 npts=2 x=(200,200) y=(400,200) '0
xdicircle imp=4 x=300 y=300 radius=50 value=7
![7m      Canned array of vectors (a red and a green star),
![7m      series of vectors forming a square, and a circle
![7m      verifying xdglwrite works
.pause
xdgoff
![7m      Graphics are OFF
.pause
xdgon
xdifill imp=4 value=0
xdttext imp=4 x=100 y=100 loc=1 nchars=0 text="ZOOM"
xdizoom imp=4 zoom=2
xdizoom imp=4 zoom=3
xdizoom imp=4 zoom=1
![7m      Text "Zoom" zooms and resumes original size
.pause
xdifill imp=4 value=0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=45.
xdtsize height=14 scale=1.
xdttext imp=4 x=600 y=600 loc=1 nchars=0 text="45 DEGREES"
xdtmask mask=127
xdttext imp=4 x=700 y=700 loc=1 nchars=0 text="MASKED"
![7m      Text "45 DEGREES" at given angle and location and
![7m      an error because you can't mask the graphics plane
![7m      Masking text can't be done yet on the IVAS because 
![7m      one cannot draw vectors on any plane other then the
![7m      graphics plane
.pause
xdtfont font=1
xdtmask mask=255
xdtlength nchars=4 text=four
xdtcolor color=127 prec=0
xdtrotate angle=-60.
xdtsize height=18 scale=1.
xdttext imp=4 x=340 y=340 loc=2 nchars=0 text="-60 DEGREES"
![7m      Text "-60 DEGREES" appears at given angle 
![7m      at given location
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m  
![7mCursor and IO Device Routines
![7m  
xdipixelwrite imp=1 x=512 y=512 value=255
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
![7m      Cursor is ON, Autotracking is ON,
![7m      Move cursor using mouse over pixel at (512,512)
.pause
xdclocation cursor=1
![7m      Press button 1 (far right one)
xdx2d device=1
![7m      Prox and Pen are ON
![7m      X and Y should be close to zero
.pause
![7m      Press button 2 (middle one)
xdxswitch device=1 switch=2
![7m      Switch is ON
.pause
xdcautotrack cursor=1 device=1 flag=0
![7m      Autotracking is OFF,
![7m      Move mouse around to verify
.pause
xdcoff cursor=1
![7m      Cursor disappears
.pause
xdcon cursor=1 form=2 blink=1
![7m      Cursor is a blinking curved cross
.pause
xdcset cursor=1 x=256 y=256
xdclocation cursor=1
![7m      Cursor is located at 256,256
.pause
xdcoff cursor=1
![7m      Cursor disappears
.pause
![7m  
![7mAdvanced Display Device Interface
![7m  
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdimfill imp=1 value=255 mask=127
xdiawset imp=1 left=1 top=513 right=512 bottom=1024
xdimfill imp=1 value=255 mask=63
xdiawset imp=1 left=513 top=1 right=1024 bottom=512
xdimawwrite imp=1 'READ outimg=0 mask=127
xdiawset imp=1 left=513 top=513 right=1024 bottom=1024
xdimlinewrite imp=1 'READ outline=0 mask=32
![7m      Top left quadrant: a masked fill
![7m      Bottom left quadrant: a further masked fill
![7m      Top right quadrant: a masked access window write 
![7m      Bottom right quadrant: a masked line write
.pause
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdifill imp=1 value=0
xdimpixelwrite imp=1 x=700 y=800 value=255 mask=127
xdimpixelwrite imp=1 x=700 y=950 value=255 mask=63
xdipixelread imp=1 x=700 y=800
xdipixelread imp=1 x=700 y=950
![7m     Verify pixels written with mask are pixels read (mask)
.pause
xdifill imp=1 value=0
xdimcircle imp=1 x=600 y=600 radius=100 value=255 mask=127
xdimcircle imp=1 x=500 y=500 radius=100 value=255 mask=63
![7m      Two circles: each masked 
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=600 y=600 radius=100 value=255
xdiareafill imp=1 x=600 y=600 boundry=255 fill=127
![7m      A filled circle
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=300 y=300 radius=100 value=255
xdicircle imp=2 x=600 y=600 radius=100 value=255
![7m      Verify red and green circles
.pause
xdiiarithmetic op=0 imp1=1 imp2=2 imp3=3
![7m      Image plane 1 and 2 are added and placed in 3
![7m      Verify upper left circle turns magenta and 
![7m      lower right circle turns cyan
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify both circles turn blue
.pause
xdiishift shift=3 imp1=3 imp2=1 wrap=1
xdifill imp=3 value=0
![7m      Image plane 3 is shifted into 1
![7m      Verify both turn magenta, then red
.pause
xdiicopy imp1=1 imp2=2
xdifill imp=1 value=0
![7m      Image plane 1 is copied into 2
![7m      Verify both turn yellow, then green
.pause
xdicircle imp=1 x=400 y=400 radius=100 value=255
xdiilogical op=1 imp1=1 imp2=2 imp3=3
![7m      New red circle in 1 is ORed with 2 and placed in 3
![7m      Verify old circles turn cyan, new red circle turns magenta
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify all end up blue
.pause
xdirotate imp=3 angle=2
![7m      Image plane 3 is rotated -90 degrees 
.pause
xdihistogram imp=3 mask=0
![7m      Histogram collected, 1690 in 255, rest in zero
.pause
xdifill imp=3 value=0
![7m  
![7mIVAS can't read the graphics look-up table or
![7mconnect graphics overlay - Expect ERROR MESSAGES
![7m  
xdgconnect imp=4 section=1 bypass=0
xdglread section=1
![7m  
![7mLimit testing - Expect ERROR MESSAGES on all calls (except xddinfo)
![7m  
xddinfo start=10 number=4
xddconfigure config=(0,3,0,0)
xddconfigure config=(0,0,0,2)
xddinfo start=4 number=1
xdiawwrite imp=5 image=0
xddinfo start=3 number=1
xdlramp lut=4 section=1
xddinfo start=24 number=1
xdlramp lut=1 section=2
xddinfo start=38 number=1
xdizoom imp=1 zoom=17
![7m  
![7mNo AFG - Expect ERROR MESSAGES on all calls 
![7m  
xdimpolyline imp=4 value=3 'CAN mask=2
xdaclear x=1 y=1 nchars=5
xdaoff
xdaon 
xdatext x=1 y=1 length=4 text=none blink=0 reverse=0
![7m  
![7mClose
![7m  
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstoverlay.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
enable-script tstoverlay.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstoverlay.scr
vrditest (images:mandrill.red, images:mandrill.grn, images:mandrill.blu)
![7m  The following script tests the function of the VRDI Overlay   
![7m  Routines.  The script will pause after executing one or more  
![7m  routines for verification. At a pause, check for correctness  
![7m  and press <RETURN> to continue.                               
xddunit
xddopen
xddactivate flag=1
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
![7m      First clean up    
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdifill imp=4 value=0
xddconfigure config=(0,0,0,0)
![7m                                         
![7m      Graphics Overlay Plane Routines    
![7m                                         
xdgconnect imp=4 section=1 bypass=0
xdgon
xdglwrite section=1
xdiawset imp=4 left=110 right=120 top=226 bottom=286
xdifill imp=4 value=1
xdiawset imp=4 left=130 right=140 top=226 bottom=286
xdifill imp=4 value=2
xdiawset imp=4 left=150 right=160 top=226 bottom=286
xdifill imp=4 value=3
xdiawset imp=4 left=170 right=180 top=226 bottom=286
xdifill imp=4 value=4
xdiawset imp=4 left=190 right=200 top=226 bottom=286
xdifill imp=4 value=5
xdiawset imp=4 left=210 right=220 top=226 bottom=286
xdifill imp=4 value=6
xdiawset imp=4 left=230 right=240 top=226 bottom=286
xdifill imp=4 value=7
xdiawset imp=4 left=250 right=260 top=226 bottom=286
xdifill imp=4 value=8
xdiawset imp=4 left=270 right=280 top=226 bottom=286
xdifill imp=4 value=9
xdiawset imp=4 left=290 right=300 top=226 bottom=286
xdifill imp=4 value=10
xdiawset imp=4 left=310 right=320 top=226 bottom=286
xdifill imp=4 value=11
xdiawset imp=4 left=330 right=340 top=226 bottom=286
xdifill imp=4 value=12
xdiawset imp=4 left=350 right=360 top=226 bottom=286
xdifill imp=4 value=13
xdiawset imp=4 left=370 right=380 top=226 bottom=286
xdifill imp=4 value=14
xdiawset imp=4 left=390 right=400 top=226 bottom=286
xdifill imp=4 value=15
![7m      Verify that fifteen rectangles of assorted colors    
![7m      appear on the screen.                                
.pause
xdglconstant section=1 red=255 green=255 blue=255
![7m      Verify that the rectangles turn white.    
.pause
xdglconstant section=1 red=56 green=56 blue=56
![7m      Verify that the rectangles turn dimmer white or gray.    
.pause
xdglconstant section=1 red=255 green=255 blue=255
xdglread section=1
![7m      Overlay Lut written was 255.  As only upper 4 bits are    
![7m      used, returned values equal 240.  Only first 16 values    
![7m      read.  On the DeAnza IP8500 and Adage Ikonas RDS-3000,    
![7m      array position 0 = 0.                                     
.pause
xdglwrite section=1
![7m      Verify that the rectangles return to previous colors.   
.pause
xdglread section=1
![7m      Overlay Lut written with assorted colors. Verify    
![7m      that it's changed from previous constants.          
.pause
xdglinit section=1
![7m      Verify that the rectangles change colors (some of the    
![7m      rectangles may not change).                              
.pause
xdgcolor text="red"
xdgrgb red=255 green=0 blue=0
![7m      Verify that DN values returned are 1    
.pause
xdgcolor text="green"
xdgrgb red=0 green=255 blue=0
![7m      Verify that DN values returned are 2    
.pause
xdgcolor text="blue"
xdgrgb red=0 green=0 blue=255
![7m      Verify that DN values returned are 4    
.pause
xdgcolor text="black"
![7m      If the look-up table is four bits, verify the DN value     
![7m      returned is 8.  If the look-up table is eight bits,        
![7m      verify the DN value returned is 128.                       
.pause
xdgcolor text="transparent"
xdgrgb red=0 green=0 blue=0
![7m      Verify that DN values returned are 0    
.pause
xdgoff
![7m      Graphics are OFF    
.pause
xdgon
![7m      Graphics are ON     
.pause
xdiawset imp=4 left=1 right=512 top=1 bottom=512
xdifill imp=4 value=0
xdgoff
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstramtek.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
gen ram1. 480 640
gen ram2. 480 640 ival=64
gen ram3. 480 640 ival=128
enable-script tstramtek.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstramtek.scr
vrditest (ram1.,ram2.,ram3.)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on a RAMTEK. Use a monitor which can display
![7m  pseudo color. The script will pause after executing one
![7m  or more routines for verification. At a pause, check for 
![7m  correctness and press <return> to continue.
![7m  
![7m  FOR THE AUTOTRACKING PORTION OF THIS TEST TO WORK, THE
![7m  TRACKBALL NEEDS TO BE PLUGGED INTO A RAMTEK, XDDEVICE.DIB
![7m  MUST REFLECT THE SETUP, AND YOU PROBABLY WILL HAVE TO 
![7m  WORK IN 230 UNTIL A LINE IS BROUGHT DOWN TO 168.
![7m  
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Expect error code DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Configuration Routines
![7m  
xddunit
xddopen
xddactivate flag=1
xdifill imp=1 value=0
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
xddconfigure config=(0,0,0,0)
xddinfo start=10 number=4
![7m      Default configuration is 2 1 3 1
.pause
![7m  
![7mLookup Tables Routines
![7m  
![7m  
![7m      Load pseudo color table into section 2
xdlwrite lut=1 section=2 canned=0
xdlwrite lut=2 section=2 canned=1
xdlwrite lut=3 section=2 canned=2
xdlread lut=1 section=2
![7m      Array locations:   0 - 127   value:   0
![7m                       128 - 159           84
![7m                       160 - 255          255
.pause
xdlread lut=2 section=2
![7m      Array locations:   0 -  31   value:   0
![7m                        32 -  63          128
![7m                        64 - 127          255
![7m                       128 - 159          200
![7m                       160 - 191          255
![7m                       192 - 223          128
![7m                       224 - 127            0
.pause
xdlread lut=3 section=2
![7m      Array locations:   0 -  95   value: 255
![7m                        96 - 255            0
.pause
![7m      Load linear ramps and connect for BW
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=1 lut=2 section=1 bypass=0
xdlconnect imp=1 lut=3 section=1 bypass=0
![7m  
![7mImage Memory Plane Routines
![7m  
xdiawlocation imp=1
![7m      Access Window is 1 1 640 512
.pause
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdiawwrite imp=1 'IMAGE image=0
![7m      Image is displayed
.pause
xdlconnect imp=1 lut=1 section=2 bypass=0
xdlconnect imp=1 lut=2 section=2 bypass=0
xdlconnect imp=1 lut=3 section=2 bypass=0
xdiawread imp=1 outimg=0
xdifill imp=1 value=0
xdiawwrite imp=1 'READ outimg=0
![7m      Connected for Pseudo color (8 colors)
![7m      Plane has been erased, and rewritten
![7m      (using access window read/write)
.pause
xdiawset imp=1 left=1 top=1 right=320 bottom=240
xdiawread imp=1 outimg=0
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdifill imp=1 value=0
xdiawset imp=1 left=1 top=1 right=320 bottom=240
xdiawwrite imp=1 'READ outimg=0
xdiawset imp=1 left=321 top=241 right=640 bottom=480
xdiawwrite imp=1 'READ outimg=0
xdiawset imp=1 left=161 top=121 right=480 bottom=360
xdiawwrite imp=1 'READ outimg=0
![7m      Planes have been erased, 
![7m      A section of image appears in left top quadrant,
![7m      in right bottom quadrant, and center
.pause
xdilineread imp=1 outline=0
xdifill imp=1 value=0
xdilinewrite imp=1 'READ outline=0
![7m      Center image has been erased, and rewritten
![7m      (using line read/write)
.pause
xdidwlocation imp=1
![7m      Display window initialized at 1, 1
.pause
xdidwset imp=1 left=321 top=241
![7m      Image plane moved up 240 and left 320 pixels (wraps around)
.pause
xdidwset imp=1 left=1 top=241
![7m      Image plane moved up 240 pixels (wraps around)
.pause
![7m      Reinitialize Access and Display windows and Erase
xdidwset imp=1 left=1 top=1
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdifill imp=1 value=0
![7m      Back to BW
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=1 lut=2 section=1 bypass=0
xdlconnect imp=1 lut=3 section=1 bypass=0
xdipixelwrite imp=1 x=250 y=125 value=250
xdipixelwrite imp=1 x=250 y=175 value=251
xdipixelread imp=1 x=250 y=125
xdipixelread imp=1 x=250 y=175
![7m     Verify pixels written are pixels read
.pause
xdifill imp=1 value=0
xdipolyline imp=1 value=255 'CAN
xdipolyline imp=1 value=255 'CAN
xdipolyline imp=1 value=255 npts=2 x=(200,400) y=(200,200) '0
xdipolyline imp=1 value=255 npts=2 x=(400,400) y=(200,400) '0
xdipolyline imp=1 value=255 npts=2 x=(400,200) y=(400,400) '0
xdipolyline imp=1 value=255 npts=2 x=(200,200) y=(400,200) '0
xdicircle imp=1 x=300 y=300 radius=50 value=255
xdicircle imp=1 x=300 y=300 radius=75 value=255
![7m      Two (canned) stars, series of vectors 
![7m      forming a square, and two circles
.pause
xdifill imp=1 value=0
xdttext imp=1 x=5 y=20 loc=1 nchars=0 text="ZOOM"
xdizoom imp=1 zoom=4
xdizoom imp=1 zoom=8
xdizoom imp=1 zoom=1
![7m      Text "ZOOM" zooms and resumes original size
.pause
xdifill imp=1 value=0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtsize height=14 scale=.5
xdttext imp=1 x=100 y=100 loc=1 nchars=0 text="DEFAULT FONT"
xdtmask mask=127
xdttext imp=1 x=120 y=120 loc=1 nchars=0 text="MASKED"
![7m      Text "DEFAULT FONT" and a masked "MASKED" 
![7m      appears at given location
.pause
xdtfont font=1
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=-60.
xdtsize height=18 scale=1.
xdttext imp=1 x=200 y=200 loc=2 nchars=0 text="-60 Degrees"
xdtmask mask=63
xdttext imp=1 x=220 y=220 loc=2 nchars=6 text="Masked"
![7m      Text "-60 DEGREES" and a masked "MASKED" 
![7m      appears at given angle at given location
.pause
xdifill imp=1 value=0
![7m  
![7mAdvanced Display Device Interface
![7m  
xdiawset imp=1 left=1 top=1 right=320 bottom=240
xdimfill imp=1 value=255 mask=127
xdiawset imp=1 left=1 top=241 right=320 bottom=480
xdimfill imp=1 value=255 mask=63
xdiawset imp=1 left=321 top=1 right=640 bottom=240
xdimawwrite imp=1 mask=192 'READ outimg=0 
xdiawset imp=1 left=321 top=241 right=640 bottom=480
xdimawwrite imp=1 mask=145 'READ outimg=0 
![7m      Top left quadrant: a masked fill
![7m      Bottom left quadrant: a further masked fill
![7m      Top right quadrant: a masked access window write 
![7m      Bottom right quadrant: a further masked access window write
.pause
xdiawset imp=1 left=1 top=1 right=320 bottom=240
xdimlinewrite imp=1 mask=145 'READ outline=0
xdiawset imp=1 left=1 top=241 right=320 bottom=480
xdimlinewrite imp=1 mask=136 'READ outline=0
![7m      Top left quadrant: a masked line write
![7m      Bottom left quadrant: a further masked line write
.pause
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdifill imp=1 value=0
xdimpixelwrite imp=1 x=350 y=400 value=255 mask=127
xdimpixelwrite imp=1 x=350 y=475 value=255 mask=63
xdipixelread imp=1 x=350 y=400
xdipixelread imp=1 x=350 y=475
![7m     Verify pixels written with mask are pixels read 
![7m     (value  equals mask value)
.pause
xdimpolyline imp=1 value=255 'CAN mask=63
xdimpolyline imp=1 value=255 'CAN mask=127
![7m     A couple masked cans ( two stars )
.pause
xdifill imp=1 value=0
xdimcircle imp=1 x=300 y=300 radius=100 value=255 mask=255
xdimcircle imp=1 x=250 y=250 radius=100 value=255 mask=127
xdimcircle imp=1 x=200 y=200 radius=100 value=255 mask=63
xdimcircle imp=1 x=150 y=150 radius=100 value=255 mask=31
![7m      Four circles: each being further masked
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=250 y=350 radius=100 value=255
xdiareafill imp=1 x=250 y=350 boundry=255 fill=200
![7m      A filled circle 
.pause
xdihistogram imp=1 mask=0
![7m      Histogram collected, 31324 in 200, 365 in 255, rest in 0
.pause
xdifill imp=1 value=0
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdiawwrite imp=1 'IMAGE image=0
xdiiarithmetic op=0 imp1=1 imp2=1 imp3=1
![7m      Image plane 1 is added to itself and placed into itself
.pause
xdiishift shift=5 imp1=1 imp2=1 wrap=1
xdiishift shift=3 imp1=1 imp2=1 wrap=0
![7m      Image plane 1 is shifted into itself
![7m      first with wrap, then without
.pause
xdirotate imp=1 angle=3
![7m      480x480 section of image plane is rotated 90 degrees 
.pause
xdifill imp=1 value=0
!
![7m  
![7mCursor Routines
![7m  
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
xdcon cursor=2 form=2 blink=1
xdcset cursor=2 x=320 y=240
xdclocation cursor=2
![7m      Cursors are ON, Cursor 1 is Autotracking and 5-point form,
![7m      Cursor 2 is crosshatch form, blinking, and 
![7m      located at 320,240
![7m      Use the trackball and move Cursor 1 over Cursor 2
.pause
xdclocation cursor=1
![7m      Cursor 1 is now located at 320,240
.pause
xdcautotrack cursor=1 device=1 flag=0
![7m      An error message DEVCANTDO, will be returned
.pause
xdcautotrack cursor=2 device=1 flag=1
![7m      Cursor 1 Autotracking is OFF,
![7m      Cursor 2 Autotracking is ON,
![7m      Move trackball around to verify
.pause
xdcoff cursor=1
xdcoff cursor=2
![7m      Cursors disappears
.pause
!
![7m  
![7mLimit testing - Expect ERROR MESSAGES on all calls (except xddinfo)
![7m  
xddinfo start=10 number=4
xddconfigure config=(1,0,0,0)
xddconfigure config=(1,2,0,0)
xddinfo start=4 number=1
xdiicopy imp1=1 imp2=2
xdiilogical op=1 imp1=1 imp2=2 imp3=3
xddinfo start=3 number=1
xdlramp lut=4 section=1
xddinfo start=24 number=1
xdlramp lut=1 section=5
xddinfo start=38 number=1
xdizoom imp=1 zoom=17
!
![7m  
![7mNo Graphics Overlay- Expect ERROR MESSAGES on all calls 
![7m  
xdgconnect imp=1 section=1 bypass=0
xdglconstant section=1 red=255 green=255 blue=255
xdglread section=1
xdglwrite section=1
xdgoff
xdgon
!
![7m  
![7mNo AFG - Expect ERROR MESSAGES on all calls 
![7m  
xdaclear x=1 y=1 nchars=10
xdaoff
xdaon 
xdatext x=1 y=1 length=4 text=none blink=0 reverse=0
!
![7m  
![7mNo IO Devices - Expect ERROR MESSAGES on all calls 
![7m  
xdx2d device=1
xdxswitch device=1 switch=2
!
![7m  
![7mClose
![7m  
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tstramtex.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
gen ram1. 480 640
gen ram2. 480 640 ival=64
gen ram3. 480 640 ival=128
enable-script tstramtex.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstramtex.scr
vrditest (ram1.,ram2.,ram3.)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on the RAMTEK text channel. (Most of these
![7m  functions return an error message indicating they cannot
![7m  be used.) The script will pause after executing one
![7m  or more routines for verification. At a pause, check for 
![7m  correctness and press <return> to continue. 
![7m  
![7m  *** USE AN RMA2 OR RESULTS WILL BE UNPREDICTABLE ***
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Expect error code DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Initialization Routines
![7m  
xddunit
xddopen
xddactivate flag=1
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
![7m  
![7mAFG Routines
![7m  
xdaon
xdaclear x=1 y=1 nchars=0
xdatext x=20 y=10 length=0 text="This is a test" blink=0 reverse=0
xdatext x=16 y=11 length=0 text="This is a blinking test" blink=1 reverse=0
xdatext x=17 y=12 length=0 text="This is a reverse test" blink=0 reverse=1
![7m      First line should be normal
![7m      Second should be blinking
![7m      Third should be in reverse video
.pause
xdaclear x=24 y=10 nchars=10
xdaclear x=16 y=11 nchars=5
xdaclear x=34 y=11 nchars=5
xdaclear x=17 y=12 nchars=18
![7m      Should be:
![7m      This
![7m           is a blinking        (blinking)
![7m                         test   (in reverse video)
xdaoff
.pause
![7m  
![7mThese routines don't call device dependent 
![7mroutines so will work but aren't too useful
![7m  
xddinfo start=10 number=4
![7m      Default configuration is 0 0 0 0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtsize height=14 scale=.5
xdtmask mask=127
.pause
![7mThe following calls will all return appropriate ERROR MESSAGES
![7m(but we must check them all mustn't we) because the text
![7mchannel does not have configurations, image planes, luts, etc.
!
xddconfigure config=(0,0,0,0)
xdlwrite lut=1 section=2 canned=0
xdlread lut=1 section=2
xdlramp lut=1 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdiawlocation imp=1
xdiawread imp=1 outimg=0
xdiawset imp=1 left=1 top=1 right=640 bottom=480
xdiawwrite imp=1 'IMAGE image=0
xdicircle imp=1 x=250 y=350 radius=100 value=255
xdidwlocation imp=1
xdidwset imp=1 left=1 top=1
xdifill imp=1 value=0
xdilineread imp=1 outline=0
xdilinewrite imp=1 'READ outline=0
xdipixelread imp=1 x=350 y=400
xdipixelwrite imp=1 x=350 y=400 value=255 
xdipolyline imp=1 value=255 'CAN
xdizoom imp=1 zoom=4
xdcautotrack cursor=1 device=1 flag=1
xdclocation cursor=2
xdcoff cursor=1
xdcon cursor=1 form=1 blink=0
xdcset cursor=2 x=320 y=240
xdgconnect imp=1 section=1 bypass=0
xdglconstant section=1 red=255 green=255 blue=255
xdglread section=1
xdglwrite section=1
xdgoff
xdgon
xdx2d device=1
xdxswitch device=1 switch=2
xdttext imp=1 x=120 y=120 loc=1 nchars=0 text="MASKED"
xdimawwrite imp=1 'IMAGE mask=127
xdimcircle imp=1 x=150 y=150 radius=100 value=255 mask=31
xdimfill imp=1 value=255 mask=63
xdimlinewrite imp=1 mask=136 'READ outline=0
xdimpixelwrite imp=1 x=350 y=475 value=255 mask=63
xdimpolyline imp=1 value=255 'CAN mask=127
xdiareafill imp=1 x=250 y=350 boundry=255 fill=200
xdihistogram imp=1 mask=0
xdiiarithmetic op=0 imp1=1 imp2=1 imp3=1
xdiicopy imp1=1 imp2=2
xdiilogical op=1 imp1=1 imp2=2 imp3=3
xdiishift shift=3 imp1=1 imp2=1 wrap=0
xdirotate imp=1 angle=3
![7m  
![7mClose
![7m  
xddclose
exit
$!-----------------------------------------------------------------------------
$ create tsttek2d.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
enable-script tsttek2d.scr
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tsttek2d.scr
vrditest (images:tm3.img,images:tm2.img,images:tm1.img)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on the Tektronix 4237 (operating in mono
![7m  viewing mode). The script will pause after executing one
![7m  one or more routines for verification. At a pause, check
![7m  for correctness and press <return> to continue.
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Expect error code DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Configuration Routines
![7m  
xddallocate device="TKA0"
xddunit
xddopen
xddactivate flag=1
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
xddconfigure config=(0,0,0,0)
xddinfo start=10 number=4
![7m      Default configuration is [1,2,2,1].
.pause
![7m  
![7mLookup Tables Routines
![7m  
![7m      Load pseudo color table into section 1
xdlwrite lut=1 section=1 canned=0
xdlwrite lut=2 section=1 canned=1
xdlwrite lut=3 section=1 canned=2
xdlread lut=1 section=1
![7m      Array locations:   0 - 127   value:   0
![7m                       128 - 159           84
![7m                       160 - 255          255
.pause
xdlread lut=2 section=1
![7m      Array locations:   0 -  31   value:   0
![7m                        32 -  63          128
![7m                        64 - 127          255
![7m                       128 - 159          200
![7m                       160 - 191          255
![7m                       192 - 223          128
![7m                       224 - 127            0
.pause
xdlread lut=3 section=1
![7m      Array locations:   0         value:   0
![7m                         1 -  95          255
![7m                        96 - 255            0
.pause
![7m      Load linear ramps and connect for color
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
![7m  
![7mGraphics Overlay Plane Routines
![7m  
xddinfo start=30 number=7
![7m      Graphics as initialized
.pause
xdglconstant section=1 red=255 green=255 blue=255
xdglread section=1
![7m      Verify that overlay LUT contains constant 255 in all
![7m      locations.  Only first 16 values read.
.pause
xdglwrite section=1
xdglread section=1
![7m      Overlay Lut written with assorted colors. Verify
![7m      that it's changed from previous constants.
.pause
xdglinit section=1
xdgon
![7m  
![7mImage Memory Plane Routines
![7m  
xdiawset imp=1 left=1 right=1024 top=1 bottom=1024
xdiawset imp=2 left=1 right=1024 top=1 bottom=1024
xdiawset imp=3 left=1 right=1024 top=1 bottom=1024
xdiawset imp=4 left=1 right=1024 top=1 bottom=1024
xdiawlocation imp=1
xdiawlocation imp=2
xdiawlocation imp=3
xdiawlocation imp=4
![7m      Access Windows are all 1, 1, 1024, 1024
.pause
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
![7m      Color image is displayed
.pause
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      Planes have been erased, and rewritten
![7m      (using access window read/write)
.pause
xdiawset imp=1 left=257 top=1 right=768 bottom=512
xdiawset imp=2 left=257 top=1 right=768 bottom=512
xdiawset imp=3 left=257 top=1 right=768 bottom=512
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
![7m      Planes have been erased.
.pause
xdipolyline imp=4 value=1 'CAN
xdipolyline imp=4 value=2 'CAN
xdipolyline imp=4 value=3 npts=2 x=(200,400) y=(200,200) '0
xdipolyline imp=4 value=4 npts=2 x=(400,400) y=(200,400) '0
xdipolyline imp=4 value=5 npts=2 x=(400,200) y=(400,400) '0
xdipolyline imp=4 value=6 npts=2 x=(200,200) y=(400,200) '0
xdicircle imp=4 x=300 y=300 radius=50 value=7
![7m      Canned array of vectors (a green and a red star),
![7m      series of vectors forming a square, and a centered circle
.pause
xdglconstant section=1 red=255 green=255 blue=255
![7m      All vectors turn white.
.pause
xdglinit section=1
![7m      Vectors regain previous color configurations.
.pause
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdiawset imp=2 left=1 top=1 right=512 bottom=512
xdiawset imp=3 left=1 top=1 right=512 bottom=512
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      512x512 section of image appears in left top quadrant.
![7m      Overlay plane remains unaffected.
.pause
xdiawset imp=1 left=513 top=513 right=1024 bottom=1024
xdiawset imp=2 left=513 top=513 right=1024 bottom=1024
xdiawset imp=3 left=513 top=513 right=1024 bottom=1024
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      512x512 section of image appears in right bottom quadrant.
![7m      Overlay plane remains unaffected.
.pause
xdiawset imp=1 left=257 top=257 right=768 bottom=768
xdiawset imp=2 left=257 top=257 right=768 bottom=768
xdiawset imp=3 left=257 top=257 right=768 bottom=768
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
![7m      512x512 section of image appears in center.
![7m      Overlay plane remains unaffected.
.pause
xdlconnect imp=1 lut=2 section=1 bypass=0
xdlconnect imp=1 lut=3 section=1 bypass=0
![7m      Image turns black & white.
![7m      Overlay plane remains unaffected.
.pause
xdlwrite lut=1 section=1 canned=0
xdlwrite lut=2 section=1 canned=1
xdlwrite lut=3 section=1 canned=2
![7m      Image turns pseudo-color.
![7m      Overlay plane remains unaffected.
.pause
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
![7m      Image turns true color again.
![7m      Overlay plane remains unaffected.
.pause
xdilineread imp=1 outline=0
xdilineread imp=2 outline=1
xdilineread imp=3 outline=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdilinewrite imp=1 'READ outline=0
xdilinewrite imp=2 'READ outline=1
xdilinewrite imp=3 'READ outline=2
![7m      Center section of image has been erased and rewritten
![7m      (using line read/write).
![7m      Overlay plane remains unaffected.
.pause
xdidwset imp=1 left=257 top=257
![7m      Red image plane moved up and left 256 pixels.
.pause
xdizoom imp=2 zoom=2
![7m      Green image plane zooms by a factor of 2.
.pause
xdizoom imp=4 zoom=2
![7m      Overlay plane zooms by a factor of 2.
.pause
xdidwset imp=4 left=257 top=257
![7m      Overlay plane moved up and left 256 pixels.
.pause
xdgoff
![7m      Overlay plane disappears.
.pause
xdizoom imp=4 zoom=1
xdidwset imp=4 left=1 top=1
xdizoom imp=2 zoom=1
xdidwset imp=1 left=1 top=1
xdgon
![7m      Red and green image planes return to normal configuration.
![7m      Overlay plane returns to normal configuration.
.pause
xdiawset imp=2 left=1 top=1 right=1024 bottom=1024
xdiawset imp=3 left=1 top=1 right=1024 bottom=1024
xdifill imp=2 value=0
xdifill imp=3 value=0
![7m      Green and Blue planes erased
xdidwlocation imp=1
![7m      Display window initialized at 1, 1
.pause
![7m      Reinitialize Access and Display windows and Erase
xdidwset imp=1 left=1 top=1
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
xdipixelwrite imp=1 x=500 y=250 value=240
xdipixelwrite imp=1 x=500 y=350 value=224
xdipixelread imp=1 x=500 y=250
xdipixelread imp=1 x=500 y=350
![7m     Verify pixels written are pixels read
.pause
xdttext imp=1 x=50 y=50 loc=1 nchars=0 text="ZOOM"
xdizoom imp=1 zoom=2
xdizoom imp=1 zoom=3
xdizoom imp=1 zoom=1
![7m      Red image plane zooms by a factor of 2, then by a factor 
![7m      of three, then resumes original size.  Overlay plane
![7m      remains the same.
.pause
xdifill imp=1 value=0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=45.
xdtsize height=14 scale=1.
xdttext imp=1 x=300 y=300 loc=1 nchars=0 text="45 DEGREES"
xdtmask mask=127
xdttext imp=1 x=340 y=340 loc=1 nchars=0 text="MASKED"
![7m      Text "45 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdtfont font=1
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=-60.
xdtsize height=18 scale=1.
xdtmask mask=255
xdttext imp=1 x=400 y=400 loc=2 nchars=0 text="-60 DEGREES"
xdtmask mask=127
xdttext imp=1 x=440 y=440 loc=2 nchars=6 text="MASKED"
![7m      Text "-60 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdifill imp=1 value=0
xdgoff
![7m  
![7mCursor and IO Device Routines
![7m  
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
xdcset cursor=1 x=512 y=512
xdclocation cursor=1
![7m      Cursor is ON, Cursor 1 is autotracking and full-screen
![7m      crosshair, and is located at 512, 512.
.pause
xdcoff cursor=1
![7m      Cursor disappears
.pause
xdcshow cursor=1
![7m      Cursor reappears
.pause
![7m  
![7mAdvanced Display Device Interface
![7m  
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdimfill imp=1 value=255 mask=127
xdiawset imp=1 left=1 top=513 right=512 bottom=1024
xdimfill imp=1 value=255 mask=63
xdiawset imp=1 left=513 top=1 right=1024 bottom=512
xdimawwrite imp=1 'READ outimg=0 mask=127
xdiawset imp=1 left=513 top=513 right=1024 bottom=1024
xdimawwrite imp=1 'READ outimg=0 mask=63
![7m      Top left quadrant: a masked fill
![7m      Bottom left quadrant: a further masked fill
![7m      Top right quadrant: a masked access window write 
![7m      Bottom right quadrant: a further masked access window write
.pause
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdimlinewrite imp=1 'READ outline=0 mask=32
xdiawset imp=1 left=1 top=513 right=512 bottom=1024
xdimlinewrite imp=1 'READ outline=0 mask=15
xdiawset imp=1 left=1 top=1 right=1024 bottom=1024
![7m      Top left quadrant: a masked line write
![7m      Bottom left quadrant: a further masked line write
.pause
xdifill imp=1 value=0
xdimpixelwrite imp=1 x=700 y=800 value=255 mask=128
xdimpixelwrite imp=1 x=700 y=950 value=255 mask=64
xdipixelread imp=1 x=700 y=800
xdipixelread imp=1 x=700 y=950
![7m     Verify pixels written with mask are equal to mask
.pause
xdimpolyline imp=1 value=255 'CAN mask=63
xdimpolyline imp=1 value=255 'CAN mask=255
![7m     A couple masked cans (2 stars)
.pause
xdifill imp=1 value=0
xdimcircle imp=1 x=300 y=300 radius=100 value=255 mask=255
xdimcircle imp=1 x=250 y=250 radius=100 value=255 mask=127
xdimcircle imp=1 x=200 y=200 radius=100 value=255 mask=63
xdimcircle imp=1 x=150 y=150 radius=100 value=255 mask=31
![7m      Four circles: an unmasked  circle, then masked,
![7m                    more masked, and then almost gone.
.pause
xdifill imp=1 value=0
xdicircle imp=1 x=300 y=300 radius=100 value=255
xdiareafill imp=1 x=300 y=300 boundry=255 fill=200
![7m      A filled circle
.pause
xdifill imp=1 value=0
xdipolyline imp=1 value=255 'CAN
xdicircle imp=2 x=300 y=300 radius=100 value=255
![7m      Verify red star and green circle
.pause
xdiiarithmetic op=0 imp1=1 imp2=2 imp3=3
![7m      Image plane 1 and 2 are added and placed in 3
![7m      Verify star turns magenta and circle turns cyan
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify star and circle turn blue
.pause
xdiishift shift=3 imp1=3 imp2=1 wrap=1
xdifill imp=3 value=0
![7m      Image plane 3 is shifted into 1
![7m      Verify both turn magenta then red
.pause
xdiicopy imp1=1 imp2=2
xdifill imp=1 value=0
![7m      Image plane 1 is copied into 2
![7m      Verify all turn yellow then green
.pause
xdicircle imp=1 x=400 y=400 radius=100 value=255
xdiilogical op=1 imp1=1 imp2=2 imp3=3
![7m      New red circle in 1 is ORed with 2 and placed in 3
![7m      Verify star and old circle turn cyan,
![7m      new circle turns magenta
.pause
xdifill imp=1 value=0
xdifill imp=2 value=0
![7m      Verify image plane 3 contains all three
![7m      (end up blue)
.pause
xdirotate imp=3 angle=1
![7m      Image plane is rotated 180 degrees 
.pause
xdihistogram imp=3 mask=0
![7m      Histogram collected, 1514 in 255, rest in 0, 0 everywhere else.
.pause
xdifill imp=3 value=0
![7m  
![7mLimit testing - Expect ERROR MESSAGES on all calls (except xddinfo)
![7m  
xddinfo start=10 number=4
xddconfigure config=(0,3,0,0)
xddconfigure config=(0,0,2,0)
xddinfo start=4 number=1
xdiawwrite imp=5 image=0
xddinfo start=3 number=1
xdlramp lut=4 section=1
xddinfo start=24 number=1
xdlramp lut=1 section=5
xddinfo start=38 number=1
xdizoom imp=1 zoom=9
![7m  
![7mNo AFG - Expect ERROR MESSAGES on all calls 
![7m  
xdaclear x=1 y=1 nchars=5
xdaoff
xdaon 
xdatext x=1 y=1 length=4 text=none blink=0 reverse=0
![7m  
![7mClose
![7m  
xddclose
xddfree device="TKA0"
exit
$!-----------------------------------------------------------------------------
$ create tstxwa.pdf
procedure
refgbl $echo
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"
!
! Copy test image to local directory so script can find it
!
if ($syschar(1) = "UNIX")
  ush ln -s /software/images/mandrill.red mandrill.red
  ush ln -s /software/images/mandrill.grn mandrill.grn
  ush ln -s /software/images/mandrill.blu mandrill.blu
else
  dcl copy images:mandrill.red *
  dcl copy images:mandrill.grn *
  dcl copy images:mandrill.blu *
end-if
enable-script tstxwa.scr
if ($syschar(1) = "UNIX")
  ush rm -f mandrill.red
  ush rm -f mandrill.grn
  ush rm -f mandrill.blu
else
  dcl delete mandrill.red;
  dcl delete mandrill.grn;
  dcl delete mandrill.blu;
end-if
let $echo="no"
end-proc
$!-----------------------------------------------------------------------------
$ create tstxwa.scr
vrditest (mandrill.red,mandrill.grn,mandrill.blu)
![7m  
![7m  The following script tests the main functions of each 
![7m  VRDI Routine on an X11 Display Window. The script will pause 
![7m  after executing one or more routines for verification. At a 
![7m  pause, check for correctness and press <return> to continue.
![7m  
![7mError Handling Routines
![7m  
xdeaction warn=2 error=2 fatal=2
xdesignal code=-65555
![7m      Expect error code DEVCANTDO,
![7m      Device does not support requested function
.pause
![7m  
![7mDevice Configuration Routines
![7m  
xddunit
xddopen
xddactivate flag=1
![7m      First clean up
xddbatch 1
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdifill imp=4 value=0
xddbatch 0
xddname flag=1 maxlen=6
xddname flag=2 maxlen=25
![7m      First call returns Device Name
![7m      Second returns Device make/model name
.pause
xddconfigure config=(0,0,0,0)
xddinfo start=10 number=4
![7m      Default configuration is all 1's for XWA device
.pause
![7m  
![7mLookup Tables Routines
![7m  
![7m      Load pseudo color table into section 2
xddbatch 1
xdlwrite lut=1 section=1 canned=0
xdlwrite lut=2 section=1 canned=1
xdlwrite lut=3 section=1 canned=2
xddbatch 0
xdlread lut=1 section=1
![7m      Array locations:   0 - 127   value:   0
![7m                       128 - 159           84
![7m                       160 - 255          255
.pause
xdlread lut=2 section=1
![7m      Array locations:   0 -  31   value:   0
![7m                        32 -  63          128
![7m                        64 - 127          255
![7m                       128 - 159          200
![7m                       160 - 191          255
![7m                       192 - 223          128
![7m                       224 - 127            0
.pause
xdlread lut=3 section=1
![7m      Array locations:   0         value:   0
![7m                         1 -  95          255
![7m                        96 - 255            0
.pause
![7m      Load linear ramps and connect for color
xddbatch 1
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
xddbatch 0
![7m  
![7mGraphics Overlay Plane Routines
![7m  
xddinfo start=30 number=7
![7m      Graphics as initialized
.pause
xdgconnect imp=4 section=1 bypass=0
xdglconstant section=1 red=255 green=255 blue=255
xdglread section=1
![7m      Overlay Lut written was 255.  Returned values=255.  Only
![7m      first 16 values read.
.pause
xdglwrite section=1
xdglread section=1
![7m      Overlay Lut written with assorted colors. Verify
![7m      that it's changed from previous constants.
.pause
xdgon
![7m  
![7mImage Memory Plane Routines
![7m  
xddbatch 1
xdiawset imp=1 left=1 right=512 top=1 bottom=512
xdiawset imp=2 left=1 right=512 top=1 bottom=512
xdiawset imp=3 left=1 right=512 top=1 bottom=512
xdiawset imp=4 left=1 right=512 top=1 bottom=512
xddbatch 0
xdiawlocation imp=1
xdiawlocation imp=2
xdiawlocation imp=3
xdiawlocation imp=4
![7m      Access Windows are all 1, 1, 512, 512
.pause
xddbatch 1
xdiawwrite imp=1 'IMAGE image=0
xdiawwrite imp=2 'IMAGE image=1
xdiawwrite imp=3 'IMAGE image=2
xddbatch 0
![7m      Color image is displayed
.pause
xddbatch 1
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xddbatch 0
xddbatch 1
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
xddbatch 0
![7m      Planes have been erased, and rewritten
![7m      (using access window read/write)
.pause
xddbatch 1
xdiawset imp=1 left=101 top=1 right=356 bottom=256
xdiawset imp=2 left=101 top=1 right=356 bottom=256
xdiawset imp=3 left=101 top=1 right=356 bottom=256
xdiawread imp=1 outimg=0
xdiawread imp=2 outimg=1
xdiawread imp=3 outimg=2
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdiawset imp=2 left=1 top=1 right=512 bottom=512
xdiawset imp=3 left=1 top=1 right=512 bottom=512
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xdiawset imp=1 left=1 top=1 right=256 bottom=256
xdiawset imp=2 left=1 top=1 right=256 bottom=256
xdiawset imp=3 left=1 top=1 right=256 bottom=256
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
xddbatch 0
![7m      Planes have been erased, 
![7m      256x256 section of image appears in left top quadrant
.pause
xddbatch 1
xdiawset imp=1 left=257 top=257 right=512 bottom=512
xdiawset imp=2 left=257 top=257 right=512 bottom=512
xdiawset imp=3 left=257 top=257 right=512 bottom=512
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
xddbatch 0
![7m      256x256 section of image appears in right bottom quadrant
.pause
xddbatch 1
xdiawset imp=1 left=129 top=129 right=384 bottom=384
xdiawset imp=2 left=129 top=129 right=384 bottom=384
xdiawset imp=3 left=129 top=129 right=384 bottom=384
xdiawwrite imp=1 'READ outimg=0
xdiawwrite imp=2 'READ outimg=1
xdiawwrite imp=3 'READ outimg=2
xddbatch 0
![7m      256x256 section of image appears in center
.pause
xddbatch 1
xdlconnect imp=1 lut=2 section=1 bypass=0
xdlconnect imp=1 lut=3 section=1 bypass=0
xddbatch 0
![7m      Turns BW
.pause
xddbatch 1
xdlwrite lut=1 section=1 canned=0
xdlwrite lut=2 section=1 canned=1
xdlwrite lut=3 section=1 canned=2
xdlconnect imp=1 lut=1 section=1 bypass=0
xdlconnect imp=2 lut=2 section=1 bypass=0
xdlconnect imp=3 lut=3 section=1 bypass=0
xddbatch 0
![7m      Checkout 8 color pseudo color
.pause
xddbatch 1
xdlramp lut=1 section=1
xdlramp lut=2 section=1
xdlramp lut=3 section=1
xddbatch 0
![7m      Color again
.pause
xddbatch 1
xdilineread imp=1 outline=0
xdilineread imp=2 outline=1
xdilineread imp=3 outline=2
xdifill imp=1 value=0
xdifill imp=2 value=0
xdifill imp=3 value=0
xddbatch 0
xddbatch 1
xdilinewrite imp=1 'READ outline=0
xdilinewrite imp=2 'READ outline=1
xdilinewrite imp=3 'READ outline=2
xddbatch 0
![7m      Planes have been erased, and rewritten
![7m      (using line read/write)
.pause
xddbatch 1
xdiawset imp=2 left=1 top=1 right=512 bottom=512
xdiawset imp=3 left=1 top=1 right=512 bottom=512
xdifill imp=2 value=0
xdifill imp=3 value=0
xddbatch 0
![7m      Green and Blue planes erased
xdidwlocation imp=1
![7m      Display window initialized at 1, 1
.pause
xdidwset imp=1 left=129 top=129
![7m      Red plane moved up and left 128 pixels
.pause
xdidwset imp=1 left=1 top=129
![7m      Red plane moved up 128 pixels
.pause
![7m      Reinitialize Access and Display windows and Erase
xddbatch 1
xdidwset imp=1 left=1 top=1
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xdifill imp=1 value=0
xdipixelwrite imp=1 x=250 y=125 value=250
xdipixelwrite imp=1 x=250 y=175 value=251
xddbatch 0
xdipixelread imp=1 x=250 y=125
xdipixelread imp=1 x=250 y=175
![7m     Verify pixels written are pixels read
.pause
xddbatch 1
xdifill imp=1 value=0
xdipolyline imp=4 value=1 'CAN
xdipolyline imp=4 value=2 'CAN
xdipolyline imp=4 value=3 npts=2 x=(200,400) y=(200,200) '0
xdipolyline imp=4 value=4 npts=2 x=(400,400) y=(200,400) '0
xdipolyline imp=4 value=5 npts=2 x=(400,200) y=(400,400) '0
xdipolyline imp=4 value=6 npts=2 x=(200,200) y=(400,200) '0
xdicircle imp=4 x=300 y=300 radius=50 value=7
xddbatch 0
![7m      Canned array of vectors (a green and a red star),
![7m      series of vectors forming a square, and a centered circle
.pause
xdgoff
![7m      Graphics are OFF
.pause
xdifill imp=4 value=0
xdgon
xdttext imp=1 x=50 y=50 loc=1 nchars=0 text="ZOOM"
xdizoom imp=1 zoom=2
xdizoom imp=1 zoom=3
xdizoom imp=1 zoom=1
![7m      Text "Zoom" zooms and resumes original size
.pause
xdifill imp=1 value=0
xdtfont font=0
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=45.
xdtsize height=14 scale=1.
xdttext imp=1 x=150 y=150 loc=1 nchars=0 text="45 DEGREES"
xdtmask mask=127
xdttext imp=1 x=170 y=170 loc=1 nchars=0 text="MASKED"
![7m      Text "45 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdtfont font=1
xdtlength nchars=4 text=four
xdtcolor color=255 prec=0
xdtrotate angle=-60.
xdtsize height=18 scale=1.
xdtmask mask=255
xdttext imp=1 x=200 y=200 loc=2 nchars=0 text="-60 DEGREES"
xdtmask mask=127
xdttext imp=1 x=220 y=220 loc=2 nchars=6 text="MASKED"
![7m      Text "-60 DEGREES" and a masked "MASKED" appears
![7m      at given angle at given location
.pause
xdifill imp=1 value=0
![7m  
![7mCursor and IO Device Routines
![7m  
xdcon cursor=1 form=1 blink=0
xdcautotrack cursor=1 device=1 flag=1
xdcon cursor=2 form=2 blink=0
xdcset cursor=1 x=100 y=100
xdcset cursor=2 x=256 y=256
xdclocation cursor=2
![7m      Cursors are ON, Cursor 1 is Autotracking and 5-point form,
![7m      Cursor 2 is crosshatch form and located at 256,256
![7m      Use the mouse and move Cursor 1 over Cursor 2
.pause
![7m      Press button 1 on the mouse
xdx2d device=1
![7m      Pen and Prox are ON
![7m      X and Y should be close to zero
.pause
xdcautotrack cursor=2 device=1 flag=1
![7m      Cursor 1 Autotracking is OFF,
![7m      Cursor 2 Autotracking is ON,
![7m      Move mouse around to verify
.pause
xdcautotrack cursor=1 device=1 flag=1
xdcoff cursor=1
xdcoff cursor=2
![7m      Cursors disappear
.pause
xdcshow cursor=1
xdcshow cursor=2
![7m      Cursors reappear
.pause
xdcoff cursor=2
![7m      Cursor 2 disappears
.pause
xdcon cursor=1 form=6 blink=0
![7m      Cursor is an X shape
.pause
xdcautotrack cursor=1 device=1 flag=1
![7m      Press the second button on the mouse
xdxswitch device=1 switch=2
![7m      Switch is ON
.pause
xdcoff cursor=1
![7m      Cursor 1 disappears
.pause
![7m  
![7mAdvanced Display Device Interface
![7m  
xddbatch 1
xdiawset imp=1 left=1 top=1 right=256 bottom=256
xdimfill imp=1 value=255 mask=127
xdiawset imp=1 left=1 top=257 right=256 bottom=512
xdimfill imp=1 value=255 mask=63
xdiawset imp=1 left=257 top=1 right=512 bottom=256
xdimawwrite imp=1 'READ outimg=0 mask=127
xdiawset imp=1 left=257 top=257 right=512 bottom=512
xdimawwrite imp=1 'READ outimg=0 mask=63
xddbatch 0
![7m      Top left quadrant: a masked fill
![7m      Bottom left quadrant: a further masked fill
![7m      Top right quadrant: a masked access window write 
![7m      Bottom right quadrant: a further masked access window write
.pause
xddbatch 1
xdiawset imp=1 left=1 top=1 right=256 bottom=256
xdimlinewrite imp=1 'READ outline=0 mask=32
xdiawset imp=1 left=1 top=257 right=256 bottom=512
xdimlinewrite imp=1 'READ outline=0 mask=15
xdiawset imp=1 left=1 top=1 right=512 bottom=512
xddbatch 0
![7m      Top left quadrant: a masked line write
![7m      Bottom left quadrant: a further masked line write
.pause
xddbatch 1
xdifill imp=1 value=0
xdimpixelwrite imp=1 x=350 y=400 value=255 mask=127
xdimpixelwrite imp=1 x=350 y=475 value=255 mask=63
xdipixelread imp=1 x=350 y=400
xdipixelread imp=1 x=350 y=475
xddbatch 0
![7m     Verify pixels written with mask are equal to mask
.pause
xddbatch 1
xdimpolyline imp=1 value=255 'CAN mask=63
xdimpolyline imp=1 value=255 'CAN mask=255
xddbatch 0
![7m     A couple masked cans (2 stars)
.pause
xddbatch 1
xdifill imp=1 value=0
xdimcircle imp=1 x=300 y=300 radius=100 value=255 mask=255
xdimcircle imp=1 x=250 y=250 radius=100 value=255 mask=127
xdimcircle imp=1 x=200 y=200 radius=100 value=255 mask=63
xdimcircle imp=1 x=150 y=150 radius=100 value=255 mask=31
xddbatch 0
![7m      Four circles: an unmasked  circle, then masked,
![7m                    more masked, and then almost gone.
.pause
xddbatch 1
xdifill imp=1 value=0
xdicircle imp=1 x=300 y=300 radius=100 value=255
xdiareafill imp=1 x=300 y=300 boundry=255 fill=200
xddbatch 0
![7m      A filled circle
.pause
xddbatch 1
xdifill imp=1 value=0
xdipolyline imp=1 value=255 'CAN
xdicircle imp=2 x=300 y=300 radius=100 value=255
xddbatch 0
![7m      Verify red star and green circle
.pause
xdiiarithmetic op=0 imp1=1 imp2=2 imp3=3
![7m      Image plane 1 and 2 are added and placed in 3
![7m      Verify star turns magenta and circle turns cyan
.pause
xddbatch 1
xdifill imp=1 value=0
xdifill imp=2 value=0
xddbatch 0
![7m      Verify star and circle turn blue
.pause
xdiishift shift=3 imp1=3 imp2=1 wrap=1
xdifill imp=3 value=0
![7m      Image plane 3 is shifted into 1
![7m      Verify both turn magenta then red
.pause
xdiicopy imp1=1 imp2=2
xdifill imp=1 value=0
![7m      Image plane 1 is copied into 2
![7m      Verify all turn yellow then green
.pause
xdicircle imp=1 x=400 y=400 radius=100 value=255
xdiilogical op=1 imp1=1 imp2=2 imp3=3
![7m      New red circle in 1 is ORed with 2 and placed in 3
![7m      Verify star and old circle turn cyan,
![7m      new circle turns magenta
.pause
xddbatch 1
xdifill imp=1 value=0
xdifill imp=2 value=0
xddbatch 0
![7m      Verify image plane 3 contains all three
![7m      (end up blue)
.pause
xdirotate imp=3 angle=1
![7m      Image plane is rotated 180 degrees 
.pause
xdihistogram imp=3 mask=0
![7m      Histogram collected, 1514 in 255, rest in 0, 0 everywhere else.
.pause
xdifill imp=3 value=0
![7m  
![7mLimit testing - Expect ERROR MESSAGES on all calls (except xddinfo)
![7m  
xddinfo start=10 number=4
xddconfigure config=(0,3,0,0)
xddconfigure config=(0,0,2,0)
xddinfo start=4 number=1
xdiawwrite imp=5 image=0
xddinfo start=3 number=1
xdlramp lut=4 section=1
xddinfo start=24 number=1
xdlramp lut=1 section=5
xddinfo start=38 number=1
xdizoom imp=1 zoom=25
![7m  
![7mNo AFG - Expect ERROR MESSAGES on all calls 
![7m  
xdaclear x=1 y=1 nchars=5
xdaoff
xdaon 
xdatext x=1 y=1 length=4 text=none blink=0 reverse=0
![7m  
![7mClose
![7m  
xddclose
exit
$ Return
$!#############################################################################

$!****************************************************************************
$!
$! Build proc for MIPL module si_histcmds_h
$! VPACK Version 1.8, Monday, November 18, 1996, 21:06:13
$!
$! Execute by entering:		$ @si_histcmds_h
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
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
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
$ write sys$output "*** module si_histcmds_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to si_histcmds_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("si_histcmds_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @si_histcmds_h.bld "STD"
$   else
$      @si_histcmds_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_histcmds_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_histcmds_h.com -mixed -
	-s SiHistShowAxisCmd.h SiHistSetAscAxisCmd.h SiHistSetDescAxisCmd.h -
	   SiHistSetHorGraphCmd.h SiHistSetColCmd.h SiHistSetRowCmd.h -
	   SiHistSetBlendCmd.h SiHistSetStackCmd.h SiHistSetVerGraphCmd.h -
	   SiHistShowStatCmd.h SiHistSetLogScaleCmd.h SiHistSpikeCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiHistShowAxisCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistShowAxisCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#ifndef SiHistSHOWAXISCMD_H
#define SiHistSHOWAXISCMD_H
#include "Cmd.h"
#include <X11/Intrinsic.h>  // For definition of Boolean

class SiHistBox;

class SiHistShowAxisCmd : public Cmd {

  private:

    Boolean _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    SiHistShowAxisCmd ( const char *, int, SiHistBox * );

    virtual const char *const className () { return "SiHistShowAxisCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetAscAxisCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetAscAxisCmd.h:  Display histogram using ascending axis.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETASCAXISCMD_H
#define SiHistSETASCAXISCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetAscAxisCmd : public RadioCmd {

  private:

    VerAxisDirType _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit();

  public:
    
    SiHistSetAscAxisCmd ( const char *, int, SiHistBox *, CmdList * );
    ~SiHistSetAscAxisCmd() { }

    virtual const char *const className () { return "SiHistSetAscAxisCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetDescAxisCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetDescAxisCmd.h:  Set descending orientation on histogram 
// axis.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETDESCAXISCMD_H
#define SiHistSETDESCAXISCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetDescAxisCmd : public RadioCmd {

  private:

    VerAxisDirType _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit();

  public:
    
    SiHistSetDescAxisCmd ( const char *, int, SiHistBox *, CmdList * );
    ~SiHistSetDescAxisCmd() { }

    virtual const char *const className () { return "SiHistSetDescAxisCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetHorGraphCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetHorGraphCmd.h:  Set horizontal orientation on hstogram 
// plot.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETHORGRAPHCMD_H
#define SiHistSETHORGRAPHCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetHorGraphCmd : public RadioCmd {

  private:

    OrientType _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:

    SiHistSetHorGraphCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className() { return "SiHistSetHorGraphCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetColCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetColCmd.h:  Arrange r,g,b SiHistograms in column order.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETCOLCMD_H
#define SiHistSETCOLCMD_H
#include "RadioCmd.h"
#include "SiHistDefs.h"

class SiHistBox;

class SiHistSetColCmd : public RadioCmd {

  private:

    PopupDirectionType _oldPopupDirectionValue;
    MethodType _oldMethodValue;
    SiHistBox *_box;

  protected:

    virtual void doit();   
    virtual void undoit();

  public:

    SiHistSetColCmd ( const char *, int, SiHistBox *, CmdList * );
    virtual const char *const className () { return "SiHistSetColCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetRowCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetRowCmd.h:  Arrange r,g,b histograms in row order.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETROWCMD_H
#define SiHistSETROWCMD_H
#include "RadioCmd.h"
#include "SiHistDefs.h"

class SiHistBox;

class SiHistSetRowCmd : public RadioCmd {

  private:

    PopupDirectionType _oldPopupDirectionValue;
    MethodType _oldMethodValue;
    SiHistBox *_box;

  protected:

    virtual void doit();   
    virtual void undoit();

  public:

    SiHistSetRowCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className () { return "SiHistSetRowCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetBlendCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetBlendCmd.h:  Display histogram in blend mode.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETBLENDCMD_H
#define SiHistSETBLENDCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetBlendCmd : public RadioCmd {

  private:

    MethodType _oldValue;
    SiHistBox *_box;

  protected:

    virtual void doit();   
    virtual void undoit();

  public:

    SiHistSetBlendCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className () { return "SiHistSetBlendCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetStackCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetStackCmd.h:  
/////////////////////////////////////////////////////////////
#ifndef SiHistSETSTACKCMD_H
#define SiHistSETSTACKCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetStackCmd : public RadioCmd {

  private:

    MethodType _oldValue;
    SiHistBox *_box;

  protected:

    virtual void doit();   
    virtual void undoit();

  public:    

    SiHistSetStackCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className () { return "SiHistSetStackCmd"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetVerGraphCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetVerGraphCmd.h: 
/////////////////////////////////////////////////////////////
#ifndef SiHistSETVERGRAPHCMD_H
#define SiHistSETVERGRAPHCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetVerGraphCmd : public RadioCmd {

  private:

    OrientType _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    SiHistSetVerGraphCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className () { return "SiHistSetVerGraphCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistShowStatCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistShowStatCmd.h: Show or hide histogram statistics.
/////////////////////////////////////////////////////////////
#ifndef SiHistSHOWSTATCMD_H
#define SiHistSHOWSTATCMD_H
#include "Cmd.h"

class SiHistBox;

class SiHistShowStatCmd : public Cmd {

  private:

    int _oldValue;        // Last valid command for Undo
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    SiHistShowStatCmd ( const char *, int, SiHistBox * );
    ~SiHistShowStatCmd() { }

    virtual const char *const className () { return "SiHistShowStatCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetLogScaleCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetLogScaleCmd.h: Displays histogram using logarithmic scale.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETLOGSCALECMD_H
#define SiHistSETLOGSCALECMD_H
#include <Xm/Xm.h>
#include "Cmd.h"

class SiHistBox;

class SiHistSetLogScaleCmd : public Cmd {

  private:

    Boolean _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:

    SiHistSetLogScaleCmd ( const char *, int, SiHistBox * );
    ~SiHistSetLogScaleCmd() { }

    virtual const char *const className () { return "SiHistLogScaleCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSpikeCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSpikeCmd.h: 
/////////////////////////////////////////////////////////////
#ifndef SiHistSPIKECMD_H
#define SiHistSPIKECMD_H
#include "Cmd.h"

class SiHistBox;

class SiHistSpikeCmd : public Cmd {

  protected:

    int _oldValue;
    SiHistBox *_box;

    virtual void doit();
    virtual void undoit();

  public:

    SiHistSpikeCmd ( const char *, int, SiHistBox * );
    ~SiHistSpikeCmd() { }

    virtual const char *const className () { return "SiHistSpikeCmd"; }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################

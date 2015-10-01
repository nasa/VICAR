$!****************************************************************************
$!
$! Build proc for MIPL module xvdsidebar_h
$! VPACK Version 1.9, Monday, March 15, 1999, 11:36:51
$!
$! Execute by entering:		$ @xvdsidebar_h
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
$ write sys$output "*** module xvdsidebar_h ***"
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
$ write sys$output "Invalid argument given to xvdsidebar_h.com file -- ", primary
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
$   if F$SEARCH("xvdsidebar_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @xvdsidebar_h.bld "STD"
$   else
$      @xvdsidebar_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvdsidebar_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvdsidebar_h.com -mixed -
	-s HistMenuCmd.h HistWindow.h LutMenuCmd.h LutWindow.h PanMenuCmd.h -
	   MagMenuCmd.h SideBar.h SideBarCmd.h StretchMenuCmd.h -
	   ImageSizeView.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create HistMenuCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// HistMenuCmd.h: Include file to handle the HIST command
//                button from the Image widget.
//		This class is also used to deal with the 
//		Stretched Histograms
/////////////////////////////////////////////////////////////
#ifndef HISTMENUCMD_H
#define HISTMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class Histogram;

class HistMenuCmd : public NoUndoCmd {

  private:

     int _created;
     MainWindow *_histWindow;
     Histogram *_histR, *_histG, *_histB;
     char *_title;

  protected:
    
    virtual void doit();

  public:
    
    HistMenuCmd ( char*, char*, int, Histogram*, Histogram*, Histogram* );
    virtual const char *const className () { return "HistMenuCmd"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistWindow.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
// HistWindow.h: Demonstrate Cmd and MenuBar classes
////////////////////////////////////////////////////////////////////
#ifndef HISTWINDOW_H
#define HISTWINDOW_H
#include "MenuWindow.h"

class Cmd;

class HistBox;
class Histogram;
class Separator;

class HistWindow : public MenuWindow {

  private:

    HistBox *_histBox;
  
  protected:
    
    Histogram *_histR, *_histG, *_histB;

    Cmd *_stackNoBlend;
    Cmd *_stackBlend;
    Cmd *_row;
    Cmd *_column;
    Cmd *_spikeDialog;
    Cmd *_stats;
    Cmd *_axis;
    Cmd *_hist;
    Cmd *_horizontal;
    Cmd *_vertical;
    Cmd *_ascending;
    Cmd *_descending;
    Cmd *_logScale;
    
    Widget _form;
    
  public:
    Widget createWorkArea ( Widget );
    void   createMenuPanes();
    
    HistWindow ( Histogram*, Histogram*, Histogram*, char * );
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutMenuCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// LutMenuCmd.h: LUT command class for displaying the current
//               lookup table.
/////////////////////////////////////////////////////////////
#ifndef LUTMENUCMD_H
#define LUTMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class Lut;

class LutMenuCmd : public NoUndoCmd {

  private:

     int _created;
     MainWindow *_lutWindow;
     Lut *_lutR, *_lutG, *_lutB;

  protected:
    
    virtual void doit();

  public:
    
    LutMenuCmd ( const char *, int, Lut*, Lut*, Lut*);

    virtual const char *const className () { return "LutMenuCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutWindow.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
// LutWindow.h: LUT Main Window
////////////////////////////////////////////////////////////////////
#ifndef LUTWINDOW_H
#define LUTWINDOW_H
#include "MenuWindow.h"

class LutBox;
class Lut;

class LutWindow : public MainWindow {

  private:

    LutBox *_lutBox;
  
  protected:
    
    Lut *_lutR, *_lutG, *_lutB;

    Widget _form;
    
  public:
    
    Widget createWorkArea ( Widget );
    
    LutWindow ( const char *, Lut*, Lut*, Lut* );
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PanMenuCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// PanMenuCmd.h: Include file to handle the Pan command
//                button from the Image widget.
/////////////////////////////////////////////////////////////
#ifndef PANMENUCMD_H
#define PANMENUCMD_H
#include "NoUndoCmd.h"
#include "PanToolWindow.h"

class MainWindow;
class ImageData;
class Lut;

class PanMenuCmd : public NoUndoCmd {

  private:

     int        _created;
     ImageData  * _imageData;
     Widget     _imageViewWidget;
     MainWindow *_panToolWindow;
     Lut	*_rlut, *_glut, *_blut, *_rplut, *_gplut, *_bplut;

  protected:
    
    virtual void doit();

  public:

    PanMenuCmd ( const char*, int, ImageData*, Widget,
		Lut *rlut=NULL,  Lut *glut=NULL,  Lut *blut=NULL,
		Lut *rplut=NULL, Lut *gplut=NULL, Lut *bplut=NULL);
    virtual const char *const className () { return "PanMenuCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create MagMenuCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// MagMenuCmd.h: Include file to handle the Pan command
//                button from the Image widget.
/////////////////////////////////////////////////////////////
#ifndef MAGMENUCMD_H
#define MAGMENUCMD_H
#include "NoUndoCmd.h"
#include <Xm/Xm.h>

class ImageData;
class MagTool;
class Lut;
class CursorPositionView;
class CursorDnView;
class MagInfo;

class MagMenuCmd : public NoUndoCmd {

  private:

     ImageData *_imageData;
     Widget _iw;
     Widget _parent;
     Lut *_rlut, *_glut, *_blut;
     Lut *_rpseudo, *_gpseudo, *_bpseudo;
     CursorPositionView *_posView;
     CursorDnView *_dnView;
     MagInfo *_magInfo;

     MagTool *_magTool;

  protected:
    
    virtual void doit();

  public:

    MagMenuCmd(const char *, int, Widget parent, ImageData *, Widget iw,
		Lut *, Lut *, Lut *, Lut *, Lut *, Lut *,
		MagInfo *magInfo, CursorPositionView *, CursorDnView *);
    virtual ~MagMenuCmd();

    virtual const char *const className () { return "MagMenuCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SideBar.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SideBar.h: A component class to show toolbox
/////////////////////////////////////////////////////////////
#ifndef SIDEBAR_H
#define SIDEBAR_H
#include "UIComponent.h"
#include "CursorPositionView.h"
#include "CursorDnView.h"
#include "MagInfo.h"

class Cmd;
class SiHistogram;
class Lut;
class ImageData;
class BasicImageView;
class ZoomCmdSet;
class ImageSizeView;

extern void SiCollectHist(ImageData *,
                SiHistogram *, SiHistogram *, SiHistogram *);

class SideBar : public UIComponent {

  private:

    Widget _histBox;
    static void inputCallback ( Widget, XtPointer, XtPointer );

    CursorPositionView *_cursorPositionView;
    CursorDnView *_cursorDnView;
    ImageSizeView *_imageSizeView;
    MagInfo *_magInfo;

  public:

    SideBar ( Widget, const char *, BasicImageView *,
		ImageData *, Cmd *, SiHistogram *, SiHistogram *, SiHistogram *,
		Cmd *, Lut *, Lut *, Lut *, ZoomCmdSet * );

    // Access functions
    CursorPositionView *getCursorPositionView() { return _cursorPositionView; }
    CursorDnView *getCursorDnView() { return _cursorDnView; }
    MagInfo *getMagInfo() { return _magInfo; }

    virtual const char *const className() { return "SideBar"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SideBarCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SideBarCmd.h: Include file for turning XVicDisp side bar
//                on and off
/////////////////////////////////////////////////////////////
#ifndef SIDEBARCMD_H
#define SIDEBARCMD_H
#include "Cmd.h"

class ImageDisplayer;

class SideBarCmd : public Cmd {
  private:
    int _oldValue;    // Last valid command for Undo
    ImageDisplayer *_imageView;
  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    SideBarCmd( const char *, int, ImageDisplayer * );
    virtual const char *const className () { return "SideBarCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchMenuCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// StretchMenuCmd.h: Include file to handle the STRETCH command
//                button from the Image widget.
/////////////////////////////////////////////////////////////
#ifndef STRETCHMENUCMD_H
#define STRETCHMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class Lut;

class StretchMenuCmd : public NoUndoCmd {

  private:

     int _created;
     MainWindow *_lutWindow;
     Lut *_lutR, *_lutG, *_lutB;

  protected:
    
    virtual void doit();

  public:
    
    StretchMenuCmd ( char*, int, Lut *, Lut *, Lut *);
    virtual const char *const className () { return "StretchMenuCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageSizeView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageSizeView.h
//
//	Class for image size information.
//	
///////////////////////////////////////////////////////////////
#ifndef IMAGESIZEVIEW_H
#define IMAGESIZEVIEW_H
#include "BasicImageView.h"
#include "XvicImage.h"
#include "ImageData.h"

class ImageSizeView : public BasicImageView {

   protected:

      Widget _label, _textfield, _form;

      // ADD A LABEL-TEXTFIELD COMPONENT FOR DISPLAYING IMAGE SIZE INFO - 
      virtual void addNewSubView(const char *displayName);

      // UPDATE THE DISPLAY WITH THE LATEST VALUE
      virtual void updateValue(int ns, int nl);

   public:

      ImageSizeView(Widget parent, const char *name, ImageData *imageSizeModel);
      virtual ~ImageSizeView();

      // UPDATE CALLED BY MODEL TO UPDATE ITS VIEWS
      virtual void update();

      // CLASSNAME
      virtual const char *const className() { return "ImageSizeView" ; } 
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################

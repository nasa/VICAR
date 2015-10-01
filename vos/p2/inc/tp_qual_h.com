$!****************************************************************************
$!
$! Build proc for MIPL module tp_qual_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:59:05
$!
$! Execute by entering:		$ @tp_qual_h
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
$ write sys$output "*** module tp_qual_h ***"
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
$ write sys$output "Invalid argument given to tp_qual_h.com file -- ", primary
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
$   if F$SEARCH("tp_qual_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @tp_qual_h.bld "STD"
$   else
$      @tp_qual_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tp_qual_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tp_qual_h.com -mixed -
	-s TpQualifier.h TpQualGroup.h TpQualGroupMgr.h TpQualFormatDialog.h -
	   TpQualFormatCmd.h TpQualFormatValue.h TpQualFormatCmdInterface.h -
	   TpQualFormatView.h TpQualFormatSingleView.h -
	   TpAddIdAsGenQualInterface.h TpAddCorrParmAsPntQualInterface.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TpQualifier.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// TpQualifier.h:
///////////////////////////////////////////////////////////////
#ifndef TPQUALIFIER_H
#define TPQUALIFIER_H
#include <iostream>

typedef enum { TpText, TpReal, TpFull } TpQualType;

union TpValue {
    int f;
    float r;
    char *t;
};

class TpQualifier {

  protected:

    const TpQualType _type;
    TpValue _value;

  friend std::ostream &operator<<(std::ostream &ostr, const TpQualifier &q);

  public:

    TpQualifier(TpQualType type);
    ~TpQualifier();

    TpQualType getType() const { return _type; }

    void setValue(char *value);

    void getValue(int &value) const;
    void getValue(float &value) const;
    void getValue(char *&value) const;

    char *valueToString() const;

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualGroup.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpQualGroup.h: 
//////////////////////////////////////////////////////////////////////////////
#ifndef TPQUALGROUP_H
#define TPQUALGROUP_H
#include "TpQualifier.h"
#include <Xm/Xm.h>

class TpQualGroupMgr;

class TpQualGroup {
    
  protected:

    TpQualGroupMgr *_mgr;

    int _numQuals;
    TpQualifier **_qualifiers;
    
    TpQualType *_qualFormat;

  friend std::ostream &operator<<(std::ostream &, const TpQualGroup &);

  public:

    TpQualGroup(TpQualGroupMgr *);
    TpQualGroup(TpQualGroupMgr *, int numQuals, TpQualType *qualFormat);
    ~TpQualGroup();

    int getNumQuals() const { return _numQuals; }
    TpQualType getType(int i) const { return _qualFormat[i]; }

    void incNumQuals(TpQualType);
    void decNumQuals();
    void deleteAllQuals();

    void getValue(int n, int &value) const;
    void getValue(int n, float &value) const;
    void getValue(int n, char *&value) const;
 
    char *valueToString(int n) const;

    // IBIS routines require separate arrays for each type

    float *getRealQuals();
    int *getFullQuals();
    char *getTextQuals();

    void setValue(int n, char *value);
    void setValue(int n, int value);
    void setValue(int n, float value);

    Boolean isEqual(int n, char *value);

    static TpQualType getValueType(char *s);

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualGroupMgr.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// TpQualGroupMgr.h: 
///////////////////////////////////////////////////////////////
#ifndef TPQUALGROUPMGR_H
#define TPQUALGROUPMGR_H
#include "TpQualGroup.h"
#include <Xm/Xm.h> // for Boolean only!

class TpQualGroupMgr {
    
  protected:

    int _numGroups;
    TpQualGroup **_groups;

    int _numQuals;
    TpQualType *_qualFormat;

    char **_names;
    char **_units;

  friend std::ostream &operator<<(std::ostream &ostr, const TpQualGroupMgr &q);

  public:

    TpQualGroupMgr();
    TpQualGroupMgr(int numQuals, TpQualType *qualFormat);
    ~TpQualGroupMgr();

    void addGroup(TpQualGroup *);
    void deleteGroup(TpQualGroup *);

    void incNumQuals(TpQualType);
    void decNumQuals();
    void deleteAllQuals();
    void setFormat(int numQuals, char (*newFormat)[6]);

    int getNumQuals() const { return _numQuals; }
    int getNumFullQuals() const;
    int getNumRealQuals() const;
    int getNumTextQuals() const;
    TpQualType *getQualFormat() const { return _qualFormat; }
    TpQualType getType(int n) const { return _qualFormat[n]; }
    void getValue(int group, int n, int &value) const;
    void getValue(int group, int n, float &value) const;
    void getValue(int group, int n, char *&value) const;

    Boolean isUnique(TpQualGroup *excludeGroup, int n, char *);

    int getMinValue(int n, int &value);
    int getMaxValue(int n, int &value);

    void setValue(int group, int n, char *);
    void setValue(TpQualGroup *group, int n, char *);

    char *getQualName(int n) const;
    char *getQualUnit(int n) const;

    void setQualName(int n, char *name);
    void setQualUnit(int n, char *unit);
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpQualFormatDialog.h: Dialog containing point file format values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATDIALOG_H
#define TPQUALFORMATDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpQualFormatCmd;

class TpQualFormatDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    TpQualFormatCmd *_cmdGen, *_cmdPnt;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpQualFormatDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);
    virtual void post();

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpQualFormatCmd.h
///////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATCMD_H
#define TPQUALFORMATCMD_H
#include "Cmd.h"

class TpQualGroupMgr;

class TpQualFormatCmd : public Cmd {

  protected:

    TpQualGroupMgr *_qualGroupMgr;

    void doit();
    void undoit() { }

  public:

    TpQualFormatCmd(const char *name, int active, TpQualGroupMgr *);
    virtual ~TpQualFormatCmd() { }

    void collectValue();

    virtual const char *const className () { return "TpQualFormatCmd"; }
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpQualFormatValue.h
///////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATVALUE_H
#define TPQUALFORMATVALUE_H

#include "TpQualifier.h"

typedef struct _TpQualInfo {
    char *_qualName;
    TpQualType _qualType;
    char *_qualUnit;
} TpQualInfo;

class TpQualFormatValue {

  public:

    int _numQuals;

    TpQualInfo *_info;

    TpQualFormatValue();
    TpQualFormatValue(int numQuals, TpQualInfo *info);
    TpQualFormatValue(const TpQualFormatValue &);
    virtual ~TpQualFormatValue() { }

    TpQualFormatValue &operator=(TpQualFormatValue &val);

    int getNumQuals() { return _numQuals; }
    void setNumQuals(int n);

    void addQualInfo(const char *name = "", TpQualType type = TpReal, 
			const char *unit = "");
    void deleteQualInfo();
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatCmdInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpQualFormatCmdInterface.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATCMDINTERFACE_H
#define TPQUALFORMATCMDINTERFACE_H
#include "CmdInterface.h"
#include "TpQualifier.h"

class TpQualFormatValue;
class TpQualFormatView;
class KeyinView;

class TpQualFormatCmdInterface : public CmdInterface {

  protected:

    TpQualFormatValue *_value; // saved copy

    KeyinView *_numQualsView;
    TpQualFormatView *_qualView;

    static void addQualCallback(Widget, XtPointer, XtPointer);
    static void deleteQualCallback(Widget, XtPointer, XtPointer);
    static void setNumQualsCallback(Widget, XtPointer, XtPointer);

  public:

    TpQualFormatCmdInterface(Widget parent, Cmd *cmd);
    virtual ~TpQualFormatCmdInterface() { }

    void setValue(CmdValue);

    void addQual();
    void setNumQuals();
    void deleteQual();

    void setName(char *name, int numQual);
    void setUnit(char *unit, int numQual);
    void setType(TpQualType t, int numQual);
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// TpQualFormatView.h: 
////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATVIEW_H
#define TPQUALFORMATVIEW_H
#include "UIComponent.h"
#include "TpQualifier.h"
#include "sl_lists.h"

class TpQualFormatCmdInterface;
class TpQualFormatSingleView;
class TpQualFormatValue;

class TpQualFormatView : public UIComponent {

  protected:

    TpQualFormatCmdInterface *_ci;
    TpQualFormatValue *_value;

    Widget _formatView;

    SL_List<TpQualFormatSingleView *> *_qualInfos;

  public:

    TpQualFormatView(Widget parent, const char *name, TpQualFormatCmdInterface *);
    virtual ~TpQualFormatView() { };

    void setQuals(TpQualFormatValue *);

    void setName(char *, int numQual);
    void setUnit(char *, int numQual);
    void setType(TpQualType, int numQual);

    virtual const char *const className() { return "TpQualFormatView"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatSingleView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// TpQualFormatSingleView.h: 
////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATSINGLEVIEW_H
#define TPQUALFORMATSINGLEVIEW_H
#include "UIComponent.h"
#include "TpQualifier.h"

class TpQualFormatView;

class TpQualFormatSingleView : public UIComponent {

  protected:

    Widget _wname, _wunit;
    Widget _wreal, _wfull, _wtext;

    int _n;
    TpQualFormatView *_view;

    static void setNameCallback(Widget, XtPointer, XtPointer);
    static void setUnitCallback(Widget, XtPointer, XtPointer);
    static void setTypeCallback(Widget, XtPointer, XtPointer);

  public:

    TpQualFormatSingleView(Widget parent, const char *name, 
			   TpQualFormatView *, int n);
    virtual ~TpQualFormatSingleView() { };

    void setNumber(int n) { _n = n; }

    void setName(char *, Boolean doUpdate = True);
    void setUnit(char *, Boolean doUpdate = True);
    void setType();
    void setType(TpQualType);

    virtual const char *const className() { return "TpQualFormatSingleView"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAddIdAsGenQualInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpAddIdAsGenQualInterface.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAddIdAsGenQualINTERFACE_H
#define TPAddIdAsGenQualINTERFACE_H
#include "CmdInterface.h"

class TpQualFormatValue;

class TpAddIdAsGenQualInterface : public CmdInterface {

  protected:

    TpQualFormatValue *_value; // saved copy

    static void addIdAsGenQualCallback(Widget, XtPointer, XtPointer);

  public:

    TpAddIdAsGenQualInterface(Widget parent, Cmd *cmd);
    virtual ~TpAddIdAsGenQualInterface() { }

    void setValue(CmdValue);

    void addIdAsGenQual();

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAddCorrParmAsPntQualInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpAddCorrParmAsPntQualInterface.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAddCorrParmAsPntQualINTERFACE_H
#define TPAddCorrParmAsPntQualINTERFACE_H
#include "CmdInterface.h"

class TpQualFormatValue;

class TpAddCorrParmAsPntQualInterface : public CmdInterface {

  protected:

    TpQualFormatValue *_value; // saved copy

    static void addCorrParmAsPntQualCallback(Widget, XtPointer, XtPointer);

  public:

    TpAddCorrParmAsPntQualInterface(Widget parent, Cmd *cmd);
    virtual ~TpAddCorrParmAsPntQualInterface() { }

    void setValue(CmdValue);

    void addCorrParmAsPntQual();

};

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################

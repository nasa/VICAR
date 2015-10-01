$!****************************************************************************
$!
$! Build proc for MIPL module pseudomodels_h
$! VPACK Version 1.8, Wednesday, October 16, 1996, 16:18:28
$!
$! Execute by entering:		$ @pseudomodels_h
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
$ write sys$output "*** module pseudomodels_h ***"
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
$ write sys$output "Invalid argument given to pseudomodels_h.com file -- ", primary
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
$   if F$SEARCH("pseudomodels_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @pseudomodels_h.bld "STD"
$   else
$      @pseudomodels_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pseudomodels_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pseudomodels_h.com -mixed -
	-s PseudoMarks.h PseudoValue.h PseudoDefs.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PseudoMarks.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// PseudoMarks.h: model class for marks of pseudocolor LUT
//		  Marks are displayed as an overlay on wedge 
/////////////////////////////////////////////////////////
#ifndef PSEUDOMARKS_H
#define PSEUDOMARKS_H
#include "PseudoDefs.h"
#include <stdio.h>

class BasicWedgeOverlay;
class InterpolationChooser;
class ColorView;

class PseudoMarks {

   protected:

	struct Mark {
		int dn;
		int red, grn, blu;
		InterpolationType type;
	};

	Mark *_marks;		// Marks that were set
	int _current;		// The current mark (1,2,...)

	int _start, _end;	// Current marked interval (0,1,...)
	int _start1;		// Use to drag to the left of the start point

	int _numViews;          // Number of dependent views
	int _numViews1;

	int _numMarks;		// Number of marks that were set

	BasicWedgeOverlay **_views;	// View objects that depend on this model
	InterpolationChooser **_views1; 

	void updateViews ( );
	void updateViews1 ( );

   public:

	PseudoMarks ( );
	~PseudoMarks ( );

	void attachView (BasicWedgeOverlay *);      // Add dependent
	void attachView (InterpolationChooser *);   // view object

	void detachView (BasicWedgeOverlay *);       // Delete dependent
        void detachView (InterpolationChooser *);   // view object

	int getNumMarks() { return _numMarks; };
	int addMark (int, int delta=0, int red=0, int grn=0, int blu=0, 
						InterpolationType=NONE);
	int deleteMark ( );
	void moveMark ( int to );
	void clearMarks ();

	// Operations on interval
	void markInterval ( int pos );
	void resizeInterval ( int pos );
	int getStart() { return _start; };   // left mark of starting interval
	int getEnd() { return _end; };	     // the furthest mark in the interval
	int getStart1() { return _start1; }; // right mark of starting interval
	void setInterpolation (InterpolationType); // set for every mark on current interval

	int getDn (int markNo) { return _marks[markNo-1].dn; };  // MarkNo (1,2,3,...)
	int getDn () { if (_current) return _marks[_current-1].dn; else return 0; };
	int *getDnAsArray( );
	Mark *getAsArray ( ) { return _marks; };
	void setAsArray ( Mark *array ) { _marks = array; updateViews(); };

	// Functions on the current mark
        int getCurrent() { return _current; };
        void setCurrent (int current);
	int getRed () { if (_current) return _marks[_current-1].red; else return 0; }
	int getGrn () { if (_current) return _marks[_current-1].grn; else return 0; }
	int getBlu () { if (_current) return _marks[_current-1].blu; else return 0; }
        void setRed ( int value ) { if(_current) {_marks[_current-1].red = value; updateViews();} }
        void setGrn ( int value ) { if(_current) {_marks[_current-1].grn = value; updateViews();} }
        void setBlu ( int value ) { if(_current) {_marks[_current-1].blu = value; updateViews();} }
	InterpolationType getInterpolation () { if (_numMarks>0) return _marks[_current-1].type;
						else return NONE; }

	Mark operator [] ( int i ) { return _marks[i]; };
	void copyMark (Mark from, Mark &to);

	int find (int dn);		// find mark index given its dn value
	int findNextDn (int dn);	// find neighbor dn value to the right of the mark
	InterpolationType getInterpolation(int markNo) { return _marks[markNo-1].type; }
	int getRed (int markNo) { return _marks[markNo-1].red; }
	int getGrn (int markNo) { return _marks[markNo-1].grn; }
	int getBlu (int markNo) { return _marks[markNo-1].blu; }

	InterpolationChooser *getView(int i = 0) { return _views1[i]; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// PseudoValue.h: Contains three pseudocolor tables
///////////////////////////////////////////////////////////////////
#ifndef PSEUDOVALUE_H
#define PSEUDOVALUE_H
#include <Xm/Xm.h>

class BasicWedgeOverlay;
class PseudoMarks;

class PseudoValue {

   protected:

	int *_pseudoR, *_pseudoG, *_pseudoB;	// will be modified
	int *_defR, *_defG, *_defB;		// default values

	int _numViews;
	BasicWedgeOverlay **_views;

	void updateViews();

   public:

	PseudoValue (char * = NULL);
	PseudoValue(PseudoValue &);		// copy ctor
	~PseudoValue ();

	void attachView (BasicWedgeOverlay *);		// Add dependent view object
	void detachView (BasicWedgeOverlay *);		// Delete dependent view object
	PseudoValue &operator=(PseudoValue &val);

	int *getRedAsArray () { return _pseudoR; }
	int *getGrnAsArray () { return _pseudoG; }
	int *getBluAsArray () { return _pseudoB; }

	void setRedAsArray ( int *array );
	void setGrnAsArray ( int *array );
	void setBluAsArray ( int *array );

	int getRGBDn ( int index, int *red, int *grn, int *blu);

	void setRedDn ( int index, int newDn );
	void setGrnDn ( int index, int newDn );
	void setBluDn ( int index, int newDn );
	void setRGBDn ( int index, int red, int grn, int blu );

	int *getDefRedAsArray () { return _defR; }
        int *getDefGrnAsArray () { return _defG; }
        int *getDefBluAsArray () { return _defB; }

	void setDefRedAsArray ( int *array );
        void setDefGrnAsArray ( int *array );
        void setDefBluAsArray ( int *array );

	void restoreDefault(int = 0, int = 255);
	void regenerate ( PseudoMarks* );

	void flat ( int valueR, int valueG, int valueB, int startDn=0, int endDn=255 );
	void linear ( int startRedValue, int endRedValue, int startGrnValue,
        	int endGrnValue, int startBluValue, int endBluValue, int startDn=0, int endDn=255 );

	int loadFile(char *);
	int saveFile(char *);
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoDefs.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// PseudoDefs.h: Type definitions for pseudocolor tool.
///////////////////////////////////////////////////////////////////
#ifndef PSEUDODEFS_H
#define PSEUDODEFS_H

enum InterpolationType { NONE, FLAT, LINEAR, CUBS };

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################

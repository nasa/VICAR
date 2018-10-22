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

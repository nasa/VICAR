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

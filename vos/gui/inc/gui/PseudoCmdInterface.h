//////////////////////////////////////////////////////////////
// PseudoCmdInterface.h: Fills pseudocolor LUT 
//////////////////////////////////////////////////////////////
#ifndef PSEUDOCMDINTERFACE
#define PSEDOCMDINTERFACE
#include "CmdInterface.h"

class PseudoValue;
class PseudoMarks;
class BasicWedgeOverlay;

class PseudoCmdInterface : public CmdInterface {

  private: 

	static void imageWidgetChangeCallback(Widget, XtPointer, XtPointer);

  protected: 

	PseudoValue *_pseudoValue;

	BasicWedgeOverlay *_wedge;      // bw wedge
	BasicWedgeOverlay *_ps;		// pseudocolored wedge

	void imageWidgetChange(XtPointer);
        void copyDisplayModeResources();
	Widget _iw;

  public:

	PseudoCmdInterface ( Widget, Cmd *, PseudoValue *, PseudoMarks *, Widget iw );

	void loadTable ( PseudoValue * );
	void executeCmd(XtPointer);

	virtual void setValue(CmdValue);

};
#endif

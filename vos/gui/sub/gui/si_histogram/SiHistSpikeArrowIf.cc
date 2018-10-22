//////////////////////////////////////////////////////////////
// SiHistSpikeArrowIf.C: An arrow button spike interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "SiHistSpikeArrowIf.h"
#include "Cmd.h"
#include "SiHistogram.h"
#include <stdint.h>

SiHistSpikeArrowIf::SiHistSpikeArrowIf ( Widget parent, 
		int step, SiHistogram *hist, Cmd *cmd ) 
	: ArrowButtonInterface ( parent, cmd )
{
    _step = step;
    _histogram = hist;

    if ( _step < 0 )
	XtVaSetValues (_w, XmNarrowDirection, XmARROW_DOWN,
			NULL);
    else
	XtVaSetValues (_w, XmNarrowDirection, XmARROW_UP,
			NULL);

    setValue ( _cmd->getValue() );
}

void SiHistSpikeArrowIf::executeCmd(XtPointer)
{
  int value = (int) (uintptr_t)_cmd->getValue();
  value += _step;
  runCmd ( (CmdValue)(uintptr_t)value );
}

void SiHistSpikeArrowIf::setValue(CmdValue value)
{
    const int SPIKE_MIN_VALUE = 1;
    const int SPIKE_MAX_VALUE = _histogram->numBins();
  
    // Deactivate Increment Spike Button if spike value is 
    // greater than or equal to the number of bins

    int x = (int)(uintptr_t) value;

    if ( (_step > 0) && ( x >= SPIKE_MAX_VALUE)){
	deactivate();
    }
    else if ( (_step < 0) && ( x <= SPIKE_MIN_VALUE)){
	deactivate();
    }
    else 
	activate();
}

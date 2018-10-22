//////////////////////////////////////////////////////////////
// SpikeButtonInterface.C: An SpikeButton interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "SpikeButtonInterface.h"
#include "Cmd.h"
#include "Histogram.h"
#include <stdint.h>
#define  MINVALUE   1

SpikeButtonInterface::SpikeButtonInterface ( Widget parent, 
					    int step,
					    Histogram *obj,
					    Cmd *cmd ) : ArrowButtonInterface ( parent, cmd )
{
  _step = step;
  _histogram = obj;

  if (_step < 0){
    XtVaSetValues (_w, XmNarrowDirection,     XmARROW_DOWN,
		   NULL);
    }
  setValue(_cmd->getValue());

}

void SpikeButtonInterface::executeCmd(XtPointer)
{
  int value = (int) (uintptr_t)_cmd->getValue();
  value += _step;
  runCmd ((CmdValue)(uintptr_t)value);
}

void SpikeButtonInterface::setValue(CmdValue value)
{
  int MAXVALUE = _histogram->numBins();
//  cout <<"The current value of MAXVALUE is:"<< MAXVALUE<<".\n"<<flush;
  
  //Deactivate Increment Spike Button if spike value is 
  //greater than or equal to 256 (MAXVALUE) 
  int x = (int) (uintptr_t)value;

  if ( (_step > 0) && ( x >= MAXVALUE)){
    deactivate();
  }
  else if ( (_step < 0) && ( x <= MINVALUE)){
    deactivate();
  }
  else activate();

}

////////////////////////////////////////////////////////////////////////
//SpikeDialog.cc: Source file for creating Spike Dialog Box
////////////////////////////////////////////////////////////////////////
#include "HistBox.h"
#include "Histogram.h"
#include "SpikeDialog.h"
#include "KeyInSpikeInterface.h"
#include "SpikeButtonInterface.h"
#include "SpikeCmd.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>

SpikeDialog::SpikeDialog ( HistBox *obj, const char *name )
	: CustomDialog(name, Invisible, Invisible, Invisible, Visible, Visible)
{
   _histBox = obj;
}

Widget SpikeDialog::createWorkArea( Widget parent)
{
   
   int spike;
   spike = _histBox->getSpike();

   Histogram *histogram;
   histogram = _histBox->getHistR();

   Widget rc = XtVaCreateWidget("workArea", xmRowColumnWidgetClass, parent,
				XmNorientation, XmHORIZONTAL,
				XmNpacking,     XmPACK_TIGHT,
				NULL);

   _SpikeCmd = new SpikeCmd ("SpikeCmd", TRUE, _histBox);

   KeyInSpikeInterface  *keyIn; 
   keyIn = new KeyInSpikeInterface ( rc, _SpikeCmd);


   CmdInterface *incButton;
   incButton = new SpikeButtonInterface ( rc, 1, histogram,  _SpikeCmd );

   CmdInterface *decButton;
   decButton = new SpikeButtonInterface ( rc, -1, histogram, _SpikeCmd );

   keyIn->manage();
   incButton->manage();
   decButton->manage();

   return rc;
}

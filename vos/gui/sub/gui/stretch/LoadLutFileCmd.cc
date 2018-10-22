/////////////////////////////////////////////////////////
// LoadPseudoFileCmd: A Command class that loads a IBIS file.  The Command
// value is a dynamically allocated single string, suitable for passing in
// to a PseudoValue subclass.
/////////////////////////////////////////////////////////
#include "LoadLutFileCmd.h"
#include "StretchValue.h"
#include "ErrorManager.h"
#include "UIComponent.h"              // only for strdup()
#include "ibisfile.h"
#include "zvproto.h"
#include "file_no_path.h"
#include <stdio.h>
#include <assert.h>

LoadLutFileCmd::LoadLutFileCmd(const char *name, int active, Cmd *stretchCmd)
    : NoUndoCmd(name, active)
{
    _stretchCmd = stretchCmd;
}

void LoadLutFileCmd::doit()
{
    const int lut_size = 256; //!!! Make this more general by passing lut

    char *filename = (char *)_value;

    int unit, ibis, status, record, i;
    int lut[lut_size][3];
    
    if (!filename) return;
    
    // open IBIS file for reading

    status = zvunit(&unit, (char *)"in_file",  1, "u_name", filename, NULL);
    
    status = IBISFileOpen(unit, &ibis, (char *)IMODE_READ, 0, 0, 0, 0);
    if (status != 1) {
	theErrorManager->process(Error, "IBISFileOpen", 
				 "Error loading file");
	return;
    }

    char *stripFilename = strdup(filename);
    file_no_path(stripFilename);
    
    ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
							stripFilename);
    ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
							(char *)"$MyLut");
    
    status = IBISRecordOpen(ibis, &record, (char *)"$MyRED | $MyGRN | $MyBLU",
			    0, 0, (char *)IFMT_FULL);
    if (status != 1) {
	theErrorManager->process(Error, "IBISRecordOpen", 
				 "Error loading file");
	IBISFileClose(ibis, 0);
	return;
    }
    
    for (i = 1; i <= lut_size; i++) {
	status = IBISRecordRead(record, (char*)lut[i-1], i);
	if (status != 1) {
	    theErrorManager->process(Error, "IBISRecordRead", 
				     "Error loading file");
	    IBISFileClose(ibis, 0);
	    return;
	}
    }
    
    IBISFileClose(ibis, 0);

    delete [] stripFilename;

    StretchValue *stretchValue;

    // Stretch all planes to red plane's value 

    stretchValue = new StretchValue(*((StretchValue*)_stretchCmd->getValue()));
    stretchValue->band = STR_ALL;
    stretchValue->stretchName = ITABLE;
    stretchValue->itableNoVals = lut_size;
    if (stretchValue->inITable)
	delete [] stretchValue->inITable;
    if (stretchValue->outITable)
	delete [] stretchValue->outITable;
    stretchValue->inITable = new int [lut_size];
    stretchValue->outITable = new int [lut_size];
    
    for (i = 0; i < stretchValue->itableNoVals; i++) {
	stretchValue->inITable[i] = i;
	stretchValue->outITable[i] = lut[i][0];
    }

    _stretchCmd->execute(stretchValue);

    // Stretch green plane only

    stretchValue = new StretchValue(*((StretchValue*)_stretchCmd->getValue()));
    stretchValue->band = STR_GREEN;
    stretchValue->stretchName = ITABLE;
    stretchValue->itableNoVals = lut_size;
    if (stretchValue->inITable)
        delete [] stretchValue->inITable;
    if (stretchValue->outITable)
        delete [] stretchValue->outITable;
    stretchValue->inITable = new int [lut_size];
    stretchValue->outITable = new int [lut_size];
 
    for (i = 0; i < stretchValue->itableNoVals; i++) {
        stretchValue->inITable[i] = i;
        stretchValue->outITable[i] = lut[i][1];
    }
 
    _stretchCmd->execute(stretchValue);

    // Stretch blue plane only

    stretchValue = new StretchValue(*((StretchValue*)_stretchCmd->getValue()));
    stretchValue->band = STR_BLUE;
    stretchValue->stretchName = ITABLE;
    stretchValue->itableNoVals = lut_size;
    if (stretchValue->inITable)
        delete [] stretchValue->inITable;
    if (stretchValue->outITable)
        delete [] stretchValue->outITable;
    stretchValue->inITable = new int [lut_size];
    stretchValue->outITable = new int [lut_size];
 
    for (i = 0; i < stretchValue->itableNoVals; i++) {
        stretchValue->inITable[i] = i;
        stretchValue->outITable[i] = lut[i][2];
    }
 
    _stretchCmd->execute(stretchValue);
}


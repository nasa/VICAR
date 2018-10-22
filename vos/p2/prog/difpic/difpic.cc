#include "DifpicParameters.h"
#include "difpic_bridges.h"
#include "VicarImageUtil.h"
#include <iostream>

extern "C"
{
#include "xvmaininc.h"
#include "applic.h"
#include "ftnbridge.h"
#include "taeextproto.h"
#include "parblk.inc"
#include "vicmain_c"
}

using namespace std;

void main44()
{
	static const int LABEL_DIFFERS  = 1;
	static const int BINHDR_DIFFERS = 2;
	static const int HIST_DIFFERS   = 4;
	static const int LPFX_DIFFERS   = 8;
	static const int PIX_DIFFERS    = 16;

	int status=0;
        struct PARBLK par_block;

	//zvmessage(const_cast<char*>(jpl::mipl::p2::difpic::VicarImageUtil::getVersion().c_str()),"");
	zvmessage(const_cast<char*>(jpl::mipl::p2::difpic::VicarImageUtil::getVersion().c_str()),NULL);

	if (!labeldifC())
	{
		//cerr<<"Will call label diff"<<endl;
		status = LABEL_DIFFERS;
	}

	if (!binaryheaderdifC())
	{
		//cerr<<"Will call binhdr diff"<<endl;
		status |= BINHDR_DIFFERS;
	}
	if (!lineprefixdifC())
	{
		//cerr<<"Will call lp diff"<<endl;
		status |= LPFX_DIFFERS;
	}

	if (!histdifC())
	{
		//cerr<<"Will call hist diff"<<endl;
		status |= HIST_DIFFERS;
	}

	int tstatus=0;
	if (jpl::mipl::p2::difpic::DifpicParameters::instance()->pixDiffEnabled())
	{
		//cerr<<"Will call pix diff"<<endl;
		FTN_NAME2(main44_ftn,MAIN44_FTN)(&tstatus);
		if (tstatus != 0)
			status |= PIX_DIFFERS;
	}


        // Send return value
        q_init (&par_block, P_BYTES, P_ABORT);
        q_intg(&par_block, "RETVAL", 1, &status, P_ADD);
        zvq_out(&par_block);

        return;
}

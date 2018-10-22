//////////////////////////////////////////////////////////////////////////
// SiHistAxis.cc:  This is a view component that draws axis.
//////////////////////////////////////////////////////////////////////////
#include "SiHistAxis.h"
#include "SiHistogram.h"

SiHistAxis::SiHistAxis ( Widget parent, const char *name, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB )
	: SgAxisView ( parent, name ),
	  SiHistView ( histR, histG, histB )
{
    setIntRange ( _histR->isIntRange() );

    if ( _histR->isIntRange() )
	setLimits ( (int)_histR->getLowerLimit(), 
		    (int)_histR->getUpperLimit() );
    else 
	setLimits ( (float)_histR->getLowerLimit(), 
		    (float)_histR->getUpperLimit() );

    _histR->attachView ( this );
    _histG->attachView ( this );
    _histB->attachView ( this );
}

SiHistAxis::SiHistAxis ( Widget parent, const char *name,
                SiHistogram *histR )
        : SgAxisView ( parent, name ),
          SiHistView ( histR, NULL, NULL )
{
    setIntRange ( _histR->isIntRange() );

    if ( _histR->isIntRange() )
        setLimits ( (int)_histR->getLowerLimit(), 
                    (int)_histR->getUpperLimit() );
    else
        setLimits ( (float)_histR->getLowerLimit(),
                    (float)_histR->getUpperLimit() );

    _histR->attachView ( this );
}

SiHistAxis::~SiHistAxis()
{
    if ( _histR ) _histR->detachView ( this );
    if ( _histG ) _histG->detachView ( this );
    if ( _histB ) _histB->detachView ( this );
}

void SiHistAxis::update()
{
    setIntRange ( _histR->isIntRange() );

    if ( _histR->isIntRange() )
        setLimits ( (int)_histR->getLowerLimit(), 
                    (int)_histR->getUpperLimit() );
    else
        setLimits ( (float)_histR->getLowerLimit(),
                    (float)_histR->getUpperLimit() );

    display();
}

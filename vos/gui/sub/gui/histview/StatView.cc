///////////////////////////////////////////////////////
// StatView.C: Displays histogram statistics in text form
////////////////////////////////////////////////////////
#include "StatView.h"
#include "Histogram.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <stdio.h>

StatView::StatView ( Widget parent, 
		     const char * name, 
		     Histogram *hist, 
		     Histogram *hist1, 
		     Histogram *hist2 ) : HistView (name)
{
	_hist = hist;
	_hist1 = hist1;
	_hist2 = hist2;

	_w  = XtVaCreateWidget ( _name,
                                xmRowColumnWidgetClass,
                                parent,
				XmNpacking,	XmPACK_COLUMN,
				XmNorientation,	XmHORIZONTAL,
				XmNnumColumns,	2,
				NULL );
	installDestroyHandler ();


	/****************************************************************
	*	Red histogram
	*	Data is in "label : value" display format
	****************************************************************/
	if ( (_hist != NULL) && (_hist1 == NULL) && (_hist2 == NULL) )
	{
		_labelMean  = XtVaCreateManagedWidget ( "labelM",
						xmLabelWidgetClass, _w,  
						NULL );

		_frameMean = XtVaCreateManagedWidget ( "frameMeanR",
						xmFrameWidgetClass, _w,
						NULL );
		_mean   = XtVaCreateManagedWidget     ( "mean",
						xmLabelWidgetClass,
						_frameMean,  
						NULL );


		_labelStDev = XtVaCreateManagedWidget ( "labelSD",
						xmLabelWidgetClass, _w,
						NULL );

		_frameStDev = XtVaCreateManagedWidget ( "frameStDevR",
						xmFrameWidgetClass, _w,
						NULL );
		_stDev  = XtVaCreateManagedWidget    ( "stDev",
						xmLabelWidgetClass,
					  	_frameStDev,
					  	NULL );
	}

	/****************************************************************
	*       Green histogram
	*       Data is in "label : value" display format
	****************************************************************/
	else if ( (_hist == NULL) && (_hist1 != NULL) && (_hist2 == NULL) )
	{
        	_labelMean  = XtVaCreateManagedWidget ( "labelM",
                                                xmLabelWidgetClass, _w,
                                                NULL );

          	_frameMean = XtVaCreateManagedWidget ( "frameMeanG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_mean   = XtVaCreateManagedWidget     ( "mean",
                                                xmLabelWidgetClass,
                                                _frameMean,
                                                NULL );


          	_labelStDev = XtVaCreateManagedWidget ( "labelSD",
                                                xmLabelWidgetClass, _w,
                                                NULL );

          	_frameStDev = XtVaCreateManagedWidget ( "frameStDevG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_stDev  = XtVaCreateManagedWidget    ( "stDev",
                                                xmLabelWidgetClass,
                                                _frameStDev,
                                                NULL );
	}

	/****************************************************************
	*       Blue histogram
	*       Data is in "label : value" display format
	****************************************************************/
	else if ( (_hist == NULL) && (_hist1 == NULL) && (_hist2 != NULL) )
	{
        	_labelMean  = XtVaCreateManagedWidget ( "labelM",
                                                xmLabelWidgetClass, _w,
                                                NULL );

		_frameMean = XtVaCreateManagedWidget ( "frameMeanB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_mean   = XtVaCreateManagedWidget     ( "mean",
                                                xmLabelWidgetClass,
                                                _frameMean,
                                                NULL );


          	_labelStDev = XtVaCreateManagedWidget ( "labelSD",
                                                xmLabelWidgetClass, _w,
                                                NULL );

          	_frameStDev = XtVaCreateManagedWidget ( "frameStDevB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
          	_stDev  = XtVaCreateManagedWidget    ( "stDev",
                                                xmLabelWidgetClass,
                                                _frameStDev,
                                                NULL );
	}

	/******************************************************************
	*	Color histogram
	*	Statistics for two more colors is added, 
	*	6 column requires for 3 colors (label : value)
	******************************************************************/

	else if ( (_hist != NULL) && (_hist1 != NULL) && (_hist2 != NULL) )
	{
		XtVaSetValues ( _w,
				XmNnumColumns,  2,
				NULL );

                _labelMean  = XtVaCreateManagedWidget ( "labelM",
                                                xmLabelWidgetClass, _w,
                                                NULL );

                _frameMean = XtVaCreateManagedWidget ( "frameMeanR",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _mean   = XtVaCreateManagedWidget     ( "mean",
                                                xmLabelWidgetClass,
                                                _frameMean,
                                                NULL );



        	_frameMean1 = XtVaCreateManagedWidget ( "frameMeanG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
        	_mean1   = XtVaCreateManagedWidget     ( "mean1",
                                                xmLabelWidgetClass,
                                                _frameMean1,
                                                NULL );



                _frameMean2 = XtVaCreateManagedWidget ( "frameMeanB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _mean2   = XtVaCreateManagedWidget     ( "mean2",
                                                xmLabelWidgetClass,
                                                _frameMean2,
                                                NULL );

		// Standard Deviation

                _labelStDev = XtVaCreateManagedWidget ( "labelSD",
                                                xmLabelWidgetClass, _w,
                                                NULL );


                _frameStDev = XtVaCreateManagedWidget ( "frameStDevR",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _stDev  = XtVaCreateManagedWidget    ( "StDev",
                                                xmLabelWidgetClass,
                                                _frameStDev,
                                                NULL );


                _frameStDev1 = XtVaCreateManagedWidget ( "frameStDevG",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _stDev1  = XtVaCreateManagedWidget    ( "stDev1",
                                                xmLabelWidgetClass,
                                                _frameStDev1,
                                                NULL );


                _frameStDev2 = XtVaCreateManagedWidget ( "frameStDevB",
                                                xmFrameWidgetClass, _w,
                                                NULL );
                _stDev2  = XtVaCreateManagedWidget    ( "stDev2",
                                                xmLabelWidgetClass,
                                                _frameStDev2,
                                                NULL );

	}
        if (_hist)      _hist->attachView(this);
        if (_hist1)     _hist1->attachView(this);
        if (_hist2)     _hist2->attachView(this);
}

StatView::~StatView()
{
	if (_hist)      _hist->detachView(this);
        if (_hist1)     _hist1->detachView(this);
        if (_hist2)     _hist2->detachView(this);
}

void StatView::setStat ( double m, double sd )
{
	char buf[50];
	XmString xmstr;

	sprintf ( buf, "%6.3f", m );
	xmstr = XmStringCreateSimple (buf);
	XtVaSetValues (_mean, XmNlabelString, xmstr, NULL);
	XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", sd );
	xmstr = XmStringCreateSimple (buf);
	XtVaSetValues (_stDev, XmNlabelString, xmstr, NULL);
	XmStringFree (xmstr);

}

void StatView::setStatColor ( double mR, double mG, double mB,
			      double sdR, double sdG, double sdB )
{
        char buf[50];
        XmString xmstr;

        sprintf ( buf, "%6.3f", mR );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_mean, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", sdR );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_stDev, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", mG );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_mean1, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", sdG );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_stDev1, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", mB );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_mean2, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);

        sprintf ( buf, "%6.3f", sdB );
        xmstr = XmStringCreateSimple (buf);
        XtVaSetValues (_stDev2, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);
}


void StatView::update ()
{
	// Red histogram
        if ( (_hist != NULL) && (_hist1 == NULL) && (_hist2 == NULL) )
                setStat ( _hist->getMean(), _hist->getStDev() );

        // Green histogram
        else if ( (_hist == NULL) && (_hist1 != NULL) && (_hist2 == NULL) )
                setStat ( _hist1->getMean(), _hist1->getStDev() );

        // Blue histogram
        else if ( (_hist == NULL) && (_hist1 == NULL) && (_hist2 != NULL) )
                setStat ( _hist2->getMean(), _hist2->getStDev() );


	//Color RGB histogram
	if ( (_hist != NULL) && (_hist1 != NULL) && (_hist2 != NULL) )
                setStatColor ( _hist->getMean(), _hist1->getMean(), _hist2->getMean(),
                          _hist->getStDev(), _hist1->getStDev(), _hist2->getStDev() );
}

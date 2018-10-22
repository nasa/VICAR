///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//         This example code is from the book:
//
//           Object-Oriented Programming with C++ and OSF/Motif
//         by
//           Douglas Young
//           Prentice Hall, 1992
//           ISBN 0-13-630252-1	
//
//         Copyright 1991 by Prentice Hall
//         All Rights Reserved
//
//  Permission to use, copy, modify, and distribute this software for 
//  any purpose except publication and without fee is hereby granted, provided 
//  that the above copyright notice appear in all copies of the software.
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////
// Clock.h
///////////////////////////////////////////////////////////////
#ifndef CLOCK_H
#define CLOCK_H
#include "UIComponent.h"

class Clock : public UIComponent {
    
  private:
    
    int          _delta;     // The time between ticks
    XtIntervalId _id;        // Xt Timeout identifier
    
    virtual void timeout();  // Called every delta milliseconds
    virtual void speedChanged ( int ); // Called if the user moves 
    // the speed control
    //  Xt Callbacks
    
    static void timeoutCallback ( XtPointer, XtIntervalId * );
    static void speedChangedCallback ( Widget, XtPointer, XtPointer );
    
  protected:
    
    virtual void tick() = 0;  // Hook for derived classes
    
  public:
    
    Clock ( Widget, const char *, 
	   int,             // Minimum speed, in frames per second
	   int );           // Maximum speed, in frames per second
    ~Clock ();
    
    void stop();    // Stop the clock
    void pulse();   // Make the clock tick once
    void start();   // Start or restart the clock
    
    virtual const char *const className() { return ( "Clock" ); }
};
#endif

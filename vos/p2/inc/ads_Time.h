// ads_Time.h
//
// April 9, 2002
// Michael Brady

#if !defined ADS_TIME_H_INCLUDED
#define ADS_TIME_H_INCLUDED

#include "SpiceUsr.h"


/**
 *  A class which holds a single epoch which can be converted between
 *  UTC, ephemeris, and spacecraft clock time.
 */
class ads_Time
{
    public:
    /** Constructs a new Time object with the specified UTC time. */
    ads_Time(const char* utcTime);

    /** Returns the value of this object as a spacecraft clock time. */
    SpiceDouble spacecraftClock(SpiceInt spacecraftId);

    /** Returns the value of this object in ephemeris time. */
    SpiceDouble ephemeris();

    private:
    SpiceDouble m_ephemeris;
};

#endif // !defined ADS_TIME_H_INCLUDED

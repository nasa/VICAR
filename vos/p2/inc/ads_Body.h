// ads_Body.h
//
// April 9, 2002
// Michael Brady

#if !defined ADS_BODY_H_INCLUDED
#define ADS_BODY_H_INCLUDED

#include "SpiceUsr.h"

#include "ads_Frame.h"

#include <string>
using std::string;

/**
 *  A class which holds a single NAIF body ID.
 */
class ads_Body
{
public:
   static const ads_Body& sun();
  
public:
   /** Constructs a new Body object with the specified NAIF body ID. */
   explicit ads_Body(SpiceInt naifId);
  
   /** Returns the value of this object as a NAIF ID. */
   SpiceInt naifId() const;
  
   // Returns true if this body is a valid moon code, false otherwise.
   bool isMoon() const;
  
   // Returns the planet of which this body is a moon.
   // If this body is not a moon, the result is undefined.
   //
   // The NAIF ID for a planet is the moon ID rounded up to 99.
   // For example, moon 301 orbits planet 399.
   ads_Body centerPlanet() const;
  
   // Returns the value of the spacecraft to which this instrument is attached.
   // If this is not an instrument, behavior is undefined.
   ads_Body spacecraft() const;

   // Returns the value of the reference frame for this spacecraft.
   // If this body is not a spacecraft, results are undefined.
   ads_Frame scFrame() const;
  
   // Returns the name of the reference frame of which this object
   // is the center.
   string frameName() const;

   // Returns the name of this body, as specified by the SPICE toolkit.
   string name() const;

private:
   SpiceInt m_naifId;
};

#endif // !defined ADS_BODY_H_INCLUDED

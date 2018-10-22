// ads_Frame.h
//
// Apr 19, 2002
// Michael Brady

#if !defined ADS_FRAME_H_INCLUDED
#define ADS_FRAME_H_INCLUDED

#include <string>

/**
 * A SPICE Toolkit frame.
 */
class ads_Frame
{
public:
   /** Creates a new ads_Frame object. */
   ads_Frame(int naifId);

   /** Destructor. */
   virtual ~ads_Frame();

   /** Creates a new ads_Frame object as a copy of the specified object. */
   ads_Frame(const ads_Frame& src);

   /** Sets this to the value of the specified object. */
   ads_Frame& operator=(const ads_Frame& rhs);

   /** Returns the NAIF ID for this frame. */
   int naifId() const;
   
   /** Returns the name of this frame. */
   std::string name() const;

private:

   int m_naifId;
};

#endif // !defined ADS_FRAME_H_INCLUDED

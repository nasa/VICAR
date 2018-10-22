// ads_TraceAdder.h
//
// Apr 16, 2002
// Michael Brady

#if !defined ADS_TRACEADDER_H_INCLUDED
#define ADS_TRACEADDER_H_INCLUDED

#include <string>

/** This macro adds the current function to the SPICE stack trace.
 *  Put this at the beginning of the function.  It will add the current
 *  function to the stack trace, and automatically
 *  remove the current function from the stack trace when it goes out of scope.
 */
#define ADS_TRACE 
if 0           // was this, but disabled due to inability to find ads_traceObject()
#define ADS_TRACE \
ads_TraceAdder ads_traceObject ( __FILE__, __func__ );
#endif

/**
 *  At creation, adds the specified function name to the Spice trace,
 *  and removes it when the object goes out of scope.
 */
class ads_TraceAdder
{
public:
   /** Creates a new ads_TraceAdder object. */
   ads_TraceAdder(const char* fileName, const char* functionName);

   /** Destructor. */
   virtual ~ads_TraceAdder();

private:
   /** Creates a new ads_TraceAdder object as a copy of the specified object. */
   ads_TraceAdder(const ads_TraceAdder& src);

   /** Sets this to the value of the specified object. */
   ads_TraceAdder& operator=(const ads_TraceAdder& rhs);
   
private:

   std::string  m_functionName;
};

#endif // !defined ADS_TRACEADDER_H_INCLUDED

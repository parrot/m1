/*

Splint annotations.

*/

#ifndef __M1_ANN_H__
#define __M1_ANN_H__


#define NOTNULL(x)	        /*@notnull@*/ x
#define ARGOUT(x)           /*@out@*/ x
#define ARGIN(x)            /*@in@*/ x

#define ARGIN_NOTNULL(x)    /*@notnull@*/ /*@in@*/ x

#endif

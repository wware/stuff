// Helpful stuff:
// http://pyqt.sourceforge.net/Docs/sip4/c_api.html#c.sipConvertToType
// http://www.math.uiuc.edu/~gfrancis/illimath/windows/aszgard_mini/szg/sip_python/src/typemaps.sip
// http://www.math.uiuc.edu/~gfrancis/illimath/windows/aszgard_mini/szg/sip_python/src/obj.sip

%Module sharedptr

/* ================================================================== */

class B {
%TypeHeaderCode
#include <sharedptr.h>
%End
public:
    void setX(int xx);
    int getX();
};

/* ================================================================== */

class A {
%TypeHeaderCode
#include <stdio.h>
#include <Python.h>
#include <sharedptr.h>
%End

public:
    QSharedPointer<B> b;

%GetCode
    B *b = sipCpp->b.data();
    sipPy = sipConvertFromType((void*)b, sipType_B, NULL);
    PyObject *owner = sipConvertFromType((void*)sipSelf, sipType_A, NULL);
    sipTransferTo(sipPy, owner);
%End

%SetCode
    if (!sipCanConvertToType(sipPy, sipType_B, SIP_NOT_NONE))
    {
        sipErr = 1;
    }
    else
    {
        B *b = (B*) sipConvertToType(sipPy, sipType_B, NULL, SIP_NOT_NONE, NULL, &sipErr);
        if (!sipErr)
        {
            if (sipCpp->b != NULL) {
                void *oldCb = sipCpp->b.data();
                PyObject *oldB = sipConvertFromType((void*)oldCb, sipType_B, NULL);
                Py_DECREF(oldB);
            }
            QSharedPointer<B> wrapB(b);
            sipCpp->b = wrapB;
        }
    }
%End
};

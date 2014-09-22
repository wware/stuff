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
#include <iostream>
#include <stdio.h>
#include <Python.h>
#include <sharedptr.h>
%End

public:
    QSharedPointer<B> b
    {
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

    QList < QSharedPointer<B> > bList
    {
        %GetCode
            PyObject *owner = sipConvertFromType((void*)sipSelf, sipType_A, NULL);
            sipPy = PyList_New(0);
            QSharedPointer<B> sb;
            foreach (sb, sipCpp->bList)
            {
                PyObject *thisB = sipConvertFromType((void*)sb.data(), sipType_B, NULL);
                // sipTransferTo(thisB, owner);
                PyList_Append(sipPy, thisB);
            }
            sipTransferTo(sipPy, owner);
        %End
        %SetCode
            sipCpp->bList.clear();
            Py_ssize_t i, n;
            if (!PyList_Check(sipPy))
            {
                sipErr = 1;
                PyErr_SetString(PyExc_Exception, "this needs to be a list");
                goto fini;
            }
            n = PyList_Size(sipPy);
            for (i = 0; i < n; i++)
            {
                PyObject *pyb = PyList_GetItem(sipPy, i);
                if (!sipCanConvertToType(pyb, sipType_B, SIP_NOT_NONE))
                {
                    sipErr = 1;
                    PyErr_SetString(PyExc_Exception, "list elements must be B instances");
                    goto fini;
                }
                B *b = (B*) sipConvertToType(pyb, sipType_B, NULL, SIP_NOT_NONE, NULL, &sipErr);
                Py_INCREF(pyb);
                QSharedPointer<B> sb(b);
                sipCpp->bList.append(sb);
            }
            fini: ;
        %End
    };
};

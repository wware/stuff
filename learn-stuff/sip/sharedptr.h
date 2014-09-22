#ifndef WORD_H_INCLUDED
#define WORD_H_INCLUDED

#include "QtCore/qsharedpointer.h"

class B
{
    int x;
public:
    void setX(int xx) {
        x = xx;
    }
    int getX() {
        return x;
    }
};

class A
{
public:
    QSharedPointer<B> b;
    QList < QSharedPointer<B> > bList;
};

#endif
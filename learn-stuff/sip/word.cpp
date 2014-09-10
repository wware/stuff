#include <iostream>

#include <string.h>

#include "word.h"

Word::Word(char *w) {
    int n = strlen(w);
    if (n > 0) {
        the_word = new char[n + 1];
        strcpy(the_word, w);
    } else {
        the_word = (char *) 0;
    }
}

char * Word::content() {
    return (char *) the_word;
}

Word::~Word() {
    if (the_word)
        delete[] the_word;
}

// Define the SIP wrapper to the word library.

%Module word

class Word {

%TypeHeaderCode
#include <word.h>
%End

public:
    Word(char *w);
	~Word();

    char *content();
};

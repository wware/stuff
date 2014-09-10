// Define the interface to the word library.

class Word {
    char *the_word;

public:
    Word(char *w);
    ~Word();

    char *content();
};

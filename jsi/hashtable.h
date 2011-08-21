// There are a few places where I need a hashtable that maps an int/pointer
// to an int/pointer. I'll assume here that for all architectures of interest,
// ints and pointers are the same size and can be cast one to the other.

struct jsi_hashtable_link {
    struct jsi_hashtable_link *next;
    int x, y;
};

#define HASHBITS 12

struct jsi_hashtable {
    struct jsi_hashtable_link *hashes[1 << HASHBITS];
};

extern void jsi_hashtable_init(struct jsi_hashtable *hashtable);

extern void jsi_hashtable_add(struct jsi_hashtable *hashtable,
                              int x,
                              int y);

extern void jsi_hashtable_remove(struct jsi_hashtable *hashtable,
                                 int x);

extern int jsi_hashtable_lookup(struct jsi_hashtable *hashtable,
                                int x);


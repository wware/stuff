#include <stdlib.h>

#include "hashtable.h"

// If not found, return 0, and paddr points to the entry in hashtable->hashes
// If found, return 1, and paddr points to the pointer to the correct link
static int lookup_helper(struct jsi_hashtable *hashtable,
                         int x,
                         struct jsi_hashtable_link ***paddr)
{
    struct jsi_hashtable_link **p, *q;
    p = &hashtable->hashes[x & ((1 << HASHBITS) - 1)];
    while (*p != NULL) {
        q = *p;
        if (q->x == x) {
            *paddr = p;
            return 1;
        }
        p = &(q->next);
    }
    *paddr = &hashtable->hashes[x & ((1 << HASHBITS) - 1)];
    return 0;   // not found
}


void jsi_hashtable_init(struct jsi_hashtable *hashtable)
{
    int i;
    for (i = 0; i < (1 << HASHBITS); i++)
        hashtable->hashes[i] = NULL;
}

void jsi_hashtable_add(struct jsi_hashtable *hashtable,
                       int x,
                       int y)
{
    struct jsi_hashtable_link **p, *q;
    if (lookup_helper(hashtable, x, &p)) {
        (*p)->y = y;
    } else {
        q = (struct jsi_hashtable_link *)
            malloc(sizeof(struct jsi_hashtable_link));
        if (q == NULL) {
            // TODO how to handle low memory condition?
        }
        q->x = x;
        q->y = y;
        q->next = *p;
        *p = q;
    }
}

void jsi_hashtable_remove(struct jsi_hashtable *hashtable,
                          int x)
{
    struct jsi_hashtable_link **p, *q;
    if (lookup_helper(hashtable, x, &p)) {
        q = *p;
        (*p)->next = q->next;
        free(q);
    }
}

int jsi_hashtable_lookup(struct jsi_hashtable *hashtable,
                         int x)
{
    struct jsi_hashtable_link **p;
    if (lookup_helper(hashtable, x, &p)) {
        return (*p)->y;
    } else {
        return 0; // TODO: is this OK? -1 instead?
    }
}

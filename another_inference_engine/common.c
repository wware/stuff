#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "graphobject.h"

int out_of_memory_flag = 0;

int
add(struct triple *facts, int *pointer, int size,
    int subj, int pred, int obj) {
	/* assume no triple is ever reflexive */
	if (subj == obj)
		return 0;
	if (*pointer == size) {
		out_of_memory_flag = 1;
		return 0;
	}
	facts[*pointer].subj = subj;
	facts[*pointer].pred = pred;
	facts[*pointer].obj = obj;
	/* don't add if not novel */
	if (find_in_db(facts, *pointer, &facts[*pointer]))
		return 0;
	(*pointer)++;
	return 1;
}

struct substitution *
free_subst_until(struct substitution *now,
		 struct substitution *limit) {
	while (now != limit) {
		struct substitution *p = now->next;
		free(now);
		now = p;
	}
	return now;
}


int
find_in_db(struct triple *db,
	   int pointer,
	   struct triple *template) {
	struct substitution *fake_subst = NULL;
	int i;
	for (i = 0; i < pointer; i++) {
		if (triple_matches(template,
				   &db[i],
				   &fake_subst)) {
			return 1;
		}
	}
	free_subst_until(fake_subst, NULL);
	return 0;
}

struct substitution *
add_subst(int var, int value, struct substitution *prev) {
	struct substitution *s = malloc(sizeof(struct substitution));
	s->variable = var;
	s->value = value;
	s->next = prev;
	return s;
}

int
lookup_variable(int var, struct substitution *subs) {
	while (1) {
		if (subs == NULL) return LOOKUP_FAILED;
		if (subs->variable == var)
			return subs->value;
		subs = subs->next;
	}
}

int
triple_matches_helper(int x, int y, struct substitution **subs) {
	int u = 0;
	if (ISVAR(y) || y == WILDCARD) {
		fprintf(stderr,
			"%s:%d: y can't be a wildcard or variable\n",
			__FILE__, __LINE__);
		exit(1);
	}
	if (x == WILDCARD) {
		return 1;  /* wildcards always match */
	}
	if (!ISVAR(x))
		return x == y;
	u = lookup_variable(x, *subs);
	if (u == LOOKUP_FAILED) {
		*subs = add_subst(x, y, *subs);
		return 1;
	} else {
		return u == y;
	}
}

/**
 * t1 may contain variables, to which subs applies. t2 may not
 * contain variables.
 */
int
triple_matches(struct triple *t1, struct triple *t2,
	       struct substitution **subs) {
	struct substitution *subs_orig = *subs;
	if (!triple_matches_helper(t1->subj,
				   t2->subj,
				   subs)) {
	backout:
		*subs = free_subst_until(*subs, subs_orig);
		return 0;
	}
	if (!triple_matches_helper(t1->pred,
				   t2->pred,
				   subs))
		goto backout;
	if (!triple_matches_helper(t1->obj,
				   t2->obj,
				   subs))
		goto backout;
	return 1;
}

void
apply_rule_helper(struct triple *facts,
		  int *pointer,
		  int size,
		  struct rule *rule,
		  struct triple *premises,
		  int numpremises,
		  struct substitution **subs,
		  int indent) {
	DEBUG(char *ind = make_indent(indent);
	      char *ind2 = make_indent(indent+1);
	      printf(ind);
	      printf("Context =");
	      dump_subs(*subs);
	      printf("\n"));
	if (numpremises == 0) {
		/* We got through all the premises, so clone the
		 * conclusion, plug in substitutions, add it to the
		 * database.
		 */
		struct triple *x = &(rule->conclusion);
		int s = x->subj;
		int p = x->pred;
		int o = x->obj;
		if (ISVAR(s)) s = lookup_variable(s, *subs);
		if (ISVAR(p)) p = lookup_variable(p, *subs);
		if (ISVAR(o)) o = lookup_variable(o, *subs);
		DEBUG(int ok = )
			add(facts, pointer, size, s, p, o);
		DEBUG({
			if (ok) {
				printf("%sCONCLUSION: ", ind);
				print_triple(&facts[*pointer]-1);
				printf("\n");
			}
			printf("%sdb size = %d\n", ind, *pointer);
		});
	}
	int i;
	struct triple *t;
	struct substitution *oldsubs = *subs;
	for (i = 0; i < *pointer; i++) {
		t = &facts[i];
		if (triple_matches(premises, t, subs)) {
			DEBUG({
				printf(ind2);
				print_triple(premises);
				printf(" matches ");
				print_triple(t);
				printf("\n");
			});
			apply_rule_helper(facts,
					  pointer,
					  size,
					  rule,
					  premises + 1,
					  numpremises - 1,
					  subs,
					  indent + 1);
		}
		/* Back off any new substitutions so that
		 * different premises have different contexts
		 */
		*subs = free_subst_until(*subs, oldsubs);
	}
	DEBUG(free(ind));
}

void
apply_rule(struct triple *facts,
	   int *pointer,
	   int size,
	   struct rule *r) {
	DEBUG(printf("apply_rule\n");
	      print_rule(0, r);
	      printf("\n"));
	struct substitution *sub = NULL;
	apply_rule_helper(facts,
			  pointer,
			  size,
			  r,
			  r->premises,
			  r->numpremises,
			  &sub,
			  0);
}

struct rule *
make_rule(struct rule *prev, int s, int p, int o) {
	struct rule *r = malloc(sizeof(struct rule));
	r->numpremises = 0;
	r->premises = NULL;
	r->conclusion.subj = s;
	r->conclusion.pred = p;
	r->conclusion.obj = o;
	r->next = prev;
	return r;
}

void
free_rules(struct rule *r) {
	while (r != NULL) {
		struct rule *next = r->next;
		free(r->premises);
		free(r);
		r = next;
	}
}

void
rule_add_premise(struct rule *r,
		 int s, int p, int o) {
	r->numpremises++;
	r->premises = realloc(r->premises,
			      r->numpremises *
			      sizeof(struct triple));
	struct triple *t = r->premises + (r->numpremises - 1);
	t->subj = s;
	t->pred = p;
	t->obj = o;
}

void cogitate(struct triple *facts,
	      int *pointer,
	      int size,
	      struct rule *rules) {
	int oldsize;
	DEBUG({
		printf("db size = %d\n", *pointer);
	});
	while (1) {
		oldsize = *pointer;
		struct rule *r;
		for (r = rules; r != NULL; r = r->next) {
			apply_rule(facts, pointer, size, r);
			DEBUG({
				printf("db size = %d\n", *pointer);
			});			
			if (out_of_memory_flag)
				return;
		}
		if (oldsize == *pointer)
			return;
	}
}

/*
 * Local Variables:
 * c-basic-offset: 8
 * tab-width: 8
 * End:
 */

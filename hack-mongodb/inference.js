// http://www.mongodb.org/display/DOCS/Tutorial

// When you type "mongo", it connect by default to the "test" database, which
// has named "collections", each being a container for JSON dictionaries or
// "documents". If you type "mongo bar", it creates empty database "bar".

// So if you type:
//   j = {1: 2, "a": "b"};
//   db.foo.save(j);
// then "foo" is a container and "j" is a document.

db.facts.remove();  // empty out the foo container first

// === INFERENCE ===
//db.facts.save({"name": "Socrates", "is-a": "man"});
// Result should be {"name": "Socrates", "is-a": ["man", "mortal"]});

// Until we resolve the issues below, we're stuck with this:
db.facts.save({"name": "Socrates", "is-a-man": 1});

// Production rule
// GOOD
// db.rules.save({"premise": {"is-a": "man"},
//            "conclusion": {"is-a": "mortal"}});

// BAD but acceptable in the short term
db.rules.save({"premise": {"is-a-man": 1},
            "conclusion": {"is-a-mortal": 1}});

function applyRule(rule) {
    var premiseCursor = db.facts.find(rule.premise);
    while (premiseCursor.hasNext()) {
        var pattern = premiseCursor.next();
        db.facts.remove(pattern);
        for (key in rule.conclusion) {
            if (key in pattern) {
                var value = pattern[key];
                /*
                 * Check to see whether value is a list. If not, form a
                 * two-member list with the old value and the new value. If
                 * it's a list, and the new value isn't already in the list,
                 * then add it to the list.
                 *
                 * This raises a couple of thorny issues. Javascript has no
                 * easy simple test of "this object is a list".
                 * http://www.mozilla.org/js/language/js20-2002-04/core/expressions.html
                 * "The typeof operator is deprecated... To get the type of an
                 * object x, use x.class instead."
                 */
            } else {
                pattern[key] = rule.conclusion[key];
                db.facts.save(pattern);
            }
        }
    }
}

// Apply whatever rules we have. We should really do this repeatedly until
// doing so no longer increases the number of assertions.
var ruleCursor = db.rules.find();
while (ruleCursor.hasNext()) {
    applyRule(ruleCursor.next());
}

// Print out everything we know.
var cursor = db.facts.find();
while (cursor.hasNext()) {
    printjson(cursor.next());
}

// That's forward chaining. What about backward chaining? Might be much more
// efficient. Does it really make sense to do all this in Javascript and not
// some faster language?

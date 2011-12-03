# What good is machine-readable data if we never write any code?

import json
import pprint
import types

family = []
references = {}

def findAllReferences(json):
    if type(json) is dict:
        if json.has_key("reference"):
            references[json["reference"]] = json
        for key, value in json.items():
            findAllReferences(value)
    elif type(json) is list:
        for value in json:
            findAllReferences(value)

def derefReferences(json):
    if type(json) is dict:
        for key, value in json.items():
            if key != "reference" and \
                    type(value) is types.UnicodeType and \
                    value[:1] == "@":
                # it's a reference, look it up
                try:
                    json[key] = references[value]
                except KeyError:
                    # we haven't defined this reference yet
                    pass
            else:
                derefReferences(value)
    elif type(json) is list:
        for value in json:
            derefReferences(value)

family = json.load(open("family.json"))

# deref all references
findAllReferences(family)
derefReferences(family)

pprint.pprint(family)

import pprint
from doxyxml import *

parsedSourceFiles = JavaSourceFile.parseDoxyXmlFiles('../ncadjava')

for f in parsedSourceFiles:
    f.dump()

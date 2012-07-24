import ast             # /usr/lib/python2.6/ast.py
import compiler.ast    # /usr/lib/python2.6/compiler/ast.py
import dis
import pprint


def addSomeNumbers(x, y, z):
    return x + y + z

R = open("tryit.py").read()

tree = ast.parse(R)

class NodeVisitor(object):

    def visit(self, node):
        """Visit a node."""
        childResults = {}
        for field, value in ast.iter_fields(node):
            if isinstance(value, list):
                lst = []
                for item in value:
                    if isinstance(item, ast.AST):
                        lst.append(self.visit(item))
                childResults[field] = lst
            elif isinstance(value, ast.AST):
                childResults[field] = self.visit(value)
        method = 'visit_' + node.__class__.__name__
        #print 'M:' + method
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node, childResults)

    def generic_visit(self, node, childResults):
        """Called if no explicit visitor function exists for a node."""
        lst = [node]
        pieces = dict([(k, getattr(node,k))
                       for k in dir(node) if k[:1] != "_"])
        if pieces:
            lst.append(pieces)
        if childResults:
            lst.append(childResults)
        return lst

    # The list below is almost certainly incomplete.
    #def visit_Add(self, node, childResults):
    #    pass
    #def visit_alias(self, node, childResults):
    #    pass
    #def visit_arguments(self, node, childResults):
    #    pass
    #def visit_Assign(self, node, childResults):
    #    pass
    #def visit_Attribute(self, node, childResults):
    #    pass
    #def visit_BinOp(self, node, childResults):
    #    pass
    #def visit_Call(self, node, childResults):
    #    pass
    #def visit_ClassDef(self, node, childResults):
    #    pass
    #def visit_Compare(self, node, childResults):
    #    pass
    #def visit_comprehension(self, node, childResults):
    #    pass
    #def visit_Dict(self, node, childResults):
    #    pass
    #def visit_Expr(self, node, childResults):
    #    pass
    #def visit_For(self, node, childResults):
    #    pass
    #def visit_FunctionDef(self, node, childResults):
    #    pass
    #def visit_If(self, node, childResults):
    #    pass
    #def visit_Import(self, node, childResults):
    #    pass
    #def visit_Index(self, node, childResults):
    #    pass
    #def visit_List(self, node, childResults):
    #    pass
    #def visit_ListComp(self, node, childResults):
    #    pass
    #def visit_Load(self, node, childResults):
    #    pass
    #def visit_Module(self, node, childResults):
    #    pass
    #def visit_Name(self, node, childResults):
    #    pass
    #def visit_NotEq(self, node, childResults):
    #    pass
    #def visit_Num(self, node, childResults):
    #    pass
    #def visit_Param(self, node, childResults):
    #    pass
    #def visit_Print(self, node, childResults):
    #    pass
    #def visit_Return(self, node, childResults):
    #    pass
    #def visit_Slice(self, node, childResults):
    #    pass
    #def visit_Store(self, node, childResults):
    #    pass
    #def visit_Str(self, node, childResults):
    #    pass
    #def visit_Subscript(self, node, childResults):
    #    pass
    #def visit_Tuple(self, node, childResults):
    #    pass

# pprint.pprint(NodeVisitor().visit(tree))

print dis.dis(NodeVisitor)

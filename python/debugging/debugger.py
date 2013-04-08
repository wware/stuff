import datetime
import inspect
import logging
import pprint
import sys
import traceback
import types

# It's great to log the entering/leaving of methods and functions, but it would be way cool
# to see who gets called by the function you go into. That probably means hacking ASTs and
# finding the GOSUB bytecode.

def describe(x,
             skippable=[
                'f_builtins',
                'f_globals',
                'f_locals',
                'func_globals',
                'func_defaults',
                'cell_contents',
                'co_consts',
                'co_names',
                'co_code',
                'co_lnotab',
             ],
             set_type=type(set()),
             already=set([id(globals())])):

    def is_simple(x):
        if x in (None, True, False, {}, (), []):
            return True
        if type(x) in (int, float, complex, long, str, unicode, datetime):
            return True
        return False

    def handle_container(x):
        if isinstance(x, types.ListType):
            return map(describe, x)
        if isinstance(x, types.TupleType):
            return tuple(map(describe, x))
        if isinstance(x, set_type):
            return set(map(describe, list(x)))
        if isinstance(x, types.DictType):
            return dict([(key, describe(x[key])) for
                         key, value in x.items() if key not in skippable])
        return None

    if is_simple(x):
        return x
    retval = [x, type(x)]
    if id(x) in already:
        retval.append('<0x%x>' % id(x))
        return retval
    already.add(id(x))
    r = handle_container(x)
    if r is not None:
        return r
    d = {}
    for attr, thing in inspect.getmembers(x):
        if attr.startswith('__'):
            continue
        if attr in skippable:
            continue
        if is_simple(thing):
            d[attr] = thing
            continue
        n = id(thing)
        r = handle_container(thing)
        if r is not None:
            d[attr] = r
            continue
        if n in already:
            d[attr] = '<' + hex(n) + '>'
        else:
            for type_ in (
                types.InstanceType,
                types.ClassType,
                types.FunctionType,
                types.MethodType,
                types.ModuleType,
                types.GeneratorType,
                types.TracebackType,
                types.FrameType,
                types.CodeType,
            ):
                if isinstance(thing, type_):
                    thing = describe(thing)
                    break
            d[attr] = thing
            already.add(n)
    retval.append(d)
    return retval

def show(x, stream=sys.stderr):
    stream.write(repr(traceback.extract_stack()[-2][:3]) + '\n')
    pprint.pprint(describe(x), stream=stream)

def here(message=None, stream=sys.stderr):
    s = repr(traceback.extract_stack()[-2][:3])
    if message is not None:
        s += ' ' + str(message)
    stream.write(s + '\n')

def wrap(func):
    def wrapped(*args, **kwargs):
        print 'args'
        pprint.pprint(describe(args))
        pprint.pprint(describe(kwargs))
        print
        try:
            retval = func(*args, **kwargs)
        except Exception, e:
            retval = (e, sys.exc_info())
        print 'return'
        pprint.pprint(describe(retval))
        print
        return retval
    return wrapped

def discover(obj, filtfunc=None):
    if filtfunc is None:
        filtfunc = lambda attr: True
    return dict([(attr, getattr(obj, attr)) for attr in dir(obj) if filtfunc(attr)])

def investigate(container, name, output=sys.stderr, indent=[0], debug=False, special=None, include=[]):
    obj = getattr(container, name)

    if isinstance(obj, types.ClassType):

        investigate_in(obj, include=include, output=output, debug=debug, indent=indent)

    elif callable(obj):

        indent_step = 8 * ' '

        file = lineno = None
        if isinstance(obj, types.MethodType):
            objname = str(obj.im_class) + '.' + obj.im_func.__name__
            file = obj.im_func.func_code.co_filename
            lineno = obj.im_func.func_code.co_firstlineno
        elif isinstance(obj, types.FunctionType):
            file = obj.func_code.co_filename
            lineno = obj.func_code.co_firstlineno
            objname = container.__name__ + '.' + name
        else:
            objname = str(obj)

        def emit(x, output=output):
            if isinstance(output, logging.Logger):
                output.debug(x)
            else:
                output.write(x + '\n')

        def enter(args, kwargs, output=output, file=file, line=lineno, special=special, objname=objname):
            if file is not None:
                emit('%s%s:%d' % (indent_step * indent[0], file, line))
            emit('%sEnter %s, args=%s, kwargs=%s' % (indent_step * indent[0], objname, args, kwargs))
            if special is not None:
                special(*args, **kwargs)
            indent[0] += 1

        def leave(result, output=output, objname=objname):
            indent[0] -= 1
            emit('%sLeave %s, returns %s' % (indent_step * indent[0], objname, result))

        if isinstance(container, types.InstanceType) or isinstance(container, types.ClassType):
            if getattr(container, name).im_self is None:
                def wrapped_callable(cls, *args, **kwargs):
                    enter(args, kwargs)
                    result = obj(cls, *args, **kwargs)
                    leave(result)
                    return result
                if debug:
                    emit('Instrumenting %s:%s' % (container, name))
                setattr(container, name, wrapped_callable)
            else:
                def wrapped_callable(cls, *args, **kwargs):
                    enter(args, kwargs)
                    result = obj(*args, **kwargs)
                    leave(result)
                    return result
                if debug:
                    emit('Instrumenting %s:%s' % (container, name))
                setattr(container, name, classmethod(wrapped_callable))

        else:
            def wrapped_callable(*args, **kwargs):
                enter(args, kwargs)
                result = obj(*args, **kwargs)
                leave(result)
                return result
            if debug:
                emit('Instrumenting %s:%s' % (container, name))
            setattr(container, name, wrapped_callable)


def investigate_in(container, include=[], output=sys.stderr, debug=False, indent=[0]):
    for attr in dir(container):
        if (not attr.startswith('_')) or attr in include:
            investigate(container, name=attr, output=output, debug=debug, indent=indent)

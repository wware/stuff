# here is my python code

def funca():
    print 'here is function a'

def funcb():
    print 'here is function b'

def main(*args):
    funca()
    funcb()
    print 'args are', args
    import emb
    print emb.numargs(), 'command line arguments'
    return 0

if __name__ == '__main__':
    main()

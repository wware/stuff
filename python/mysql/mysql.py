#!/usr/bin/python

import MySQLdb
import datetime
import new
import os
import pprint
import sys
import types

WRITE_PROTECT = True

_DEBUG = False
_db = None

class SqlException(Exception):
    pass

class WriteProtectException(Exception):
    pass

def handleSql(sql, handler=None):
    if _DEBUG:
        print sql
    c = _db.cursor()
    try:
        c.execute(sql)
    except Exception, e:
        raise SqlException((sql, e.args))
    while True:
        x = c.fetchone()
        if x is None:
            break
        if handler is not None:
            handler(x)
    c.close()


def setDatabase(db, host="localhost", port=3306,
                user="root", passwd="password"):
    global _db
    if passwd is not None:
        _db = MySQLdb.connect(host=host, port=port,
                                 user=user, db=db, passwd=passwd)
    else:
        _db = MySQLdb.connect(host=host, port=port,
                                 user=user, db=db)

class Row:
    def __init__(self, table, data=None):
        self._table = table
        if data is not None:
            for cname in colnames:
                setattr(self, cname, data[cname])

    def __contains__(self, colname):
        return colname in self._table._colnames

    def getKey(self):
        keys = self._table.getPrimaryKeys()
        if len(keys) > 1:
            return tuple(map(lambda keyname, self=self:
                                 getattr(self, keyname),
                             keys))
        elif len(keys) == 1:
            return getattr(self, keys[0])
        else:
            raise Exception("Row.getKey: no primary key for " +
                            self._table._name)

    def getDict(self):
        return dict([(cname, getattr(self, cname))
                     for cname in self._table._colnames])

    def __cmp__(self, other):
        assert len(self._table.getPrimaryKeys()) > 0, \
            'Row.cmp: %s has no primary keys' % self._table._name
        assert isinstance(other, Row), other.__class__
        assert self._table is other._table, (self._table, other._table)
        return cmp(self.getKey(), other.getKey())

    def __eq__(self, other):
        if not isinstance(other, Row):
            return False
        return self._table is other._table and \
            self.getDict() == other.getDict()

    def __repr__(self):
        return self._table._name + "(" + str(self.getKey()) + ")"

    @classmethod
    def make(cls, table, x):
        row = cls(table)
        for (cname, value) in map(None, table._colnames, x):
            setattr(row, cname, value)
        return row


# Decorator for creating new row methods specific to a particular table.
def RowMethod(table):
    def makeRowMethod(f, table=table):
        if f.func_name in Row.__dict__:
            # allow a row method to overload another of the same name,
            # each applying to its own table
            oldmethod = Row.__dict__[f.func_name]
        else:
            # no old method exists
            def oldmethod(self, *args, **kw):
                raise AttributeError(
                    "Row instance has no attribute '" +
                    f.func_name + "'")
        def method(self,*args,**kw):
            if self._table is table:
                return f(self,*args,**kw)
            else:
                return oldmethod(self,*args,**kw)
        Row.__dict__[f.func_name] = \
            new.instancemethod(method, None, Row)
    return makeRowMethod


class Column:
    def __init__(self, infotuple):
        self._name = infotuple[0]
        self.type = infotuple[1]
        self.keytype = infotuple[3]

    def __repr__(self):
        r = self._name + ":" + self.type
        if self.keytype:
            r += " key:" + self.keytype
        return r

class MysqlTable(dict):

    def __init__(self, name, primaryKeyNames=[], columns=[]):
        self._name = name
        self._primaryKeyNames = primaryKeyNames
        self._columns = columns
        self._colnames = map(lambda x: x._name, columns)
        
    def getName(self):
        return self._name

    def getColumns(self):
        return self._columns

    def __contains__(self, key):
        try:
            self[key]
            return True
        except KeyError:
            return False

    def filter(self, criterion=None, **kw):
        """
        This method can take any of four kinds of arguments, and returns a
        lazy-evaluating iterator of table rows.
        * If given a callable, returns the equivalent of filter(arg, self.rows())
          without the memory hogging issue of calling rows() directly.
        * If given a string, returns the list of rows from the SQL query
          "SELECT * FROM self WHERE <string arg>"
        * If given a dictionary, returns the list of rows whose column names
          and column values match the keys/values in the dictionary.
        * If called with keywords, that's treated like the dictionary case.
        """
        class SqlLazyIterator:
            def __init__(self, table, sql, criterion=None):
                if criterion is None:
                    criterion = lambda x: True
                self.table = table
                self.criterion = criterion
                self.cursor = c = _db.cursor()
                try:
                    c.execute(sql)
                except Exception, e:
                    raise SqlException((sql, e.args))
            def __iter__(self):
                return self
            def __getitem__(self, n):
                for x in self:
                    if n == 0:
                        return x
                    n -= 1
                raise IndexError
            def next(self):
                while True:
                    x = self.cursor.fetchone()
                    if x is None:
                        self.cursor.close()
                        raise StopIteration
                    else:
                        x = Row.make(self.table, x)
                        if self.criterion(x):
                            return x

        sql = "SELECT * FROM " + self.getName()
        if criterion is None:
            criterion = kw
        if callable(criterion):
            return SqlLazyIterator(self, sql, criterion)
        elif type(criterion) in (types.StringType, types.UnicodeType):
            return SqlLazyIterator(self, sql + " WHERE " + criterion)
        else:
            assert type(criterion) is types.DictType, type(criterion)
            return SqlLazyIterator(
                self,
                sql + " WHERE " +
                " AND ".join([key + "=" + self.stringify(value)
                              for (key, value) in criterion.items()]))

    def rows(self):
        """
        This method takes no arguments and returns an iterator over all the Row
        objects.
        """
        return self.filter(lambda x: True)

    def getPrimaryKeys(self):
        return self._primaryKeyNames

    def getNonKeys(self):
        return filter(lambda name: name not in self._primaryKeyNames,
                      map(lambda c: c._name, self._columns))

    def getPrimaryKey(self):
        # if calling this function, verify that primary key is unique
        assert len(self._primaryKeyNames) == 1, \
            (self,
             'getPrimaryKey requires that there is exactly one primary key',
             self._primaryKeyNames)
        return self._primaryKeyNames[0]

    def __getitem__(self, key):
        keynames = self._primaryKeyNames
        assert len(keynames) > 0, \
            (self, '__getitem__: no primary key')
        assert not isinstance(key, slice), \
            (self, '__getitem__: slices not supported')
        if len(keynames) == 1:
            d = {keynames[0]: key}
        else:
            if type(key) is not types.TupleType or \
                    len(key) is not len(keynames):
                raise KeyError
            d = dict(map(None, keynames, key))
        lst = [x for x in self.filter(d)]
        if len(lst) == 0:
            raise KeyError
        assert len(lst) == 1
        return lst[0]

    def __setitem__(self, key, rowdata):
        keynames = self._primaryKeyNames
        assert len(keynames) > 0, \
            (self, '__setitem__: no primary key')
        if len(keynames) > 1:
            if type(key) is not types.TupleType or \
                    len(key) != len(keynames):
                raise KeyError
        assert type(rowdata) is types.DictType, \
            (self, '__setitem__ needs a dict, not', rowdata)
        try:
            row = self[key]
        except KeyError:
            row = Row(self)
        rowdata.update(dict(map(None, keynames, key)))
        for k, v in rowdata.items():
            if k in self._colnames:
                setattr(row, k, v)
        self.writeToDb({key: row})

    def get(self, key):
        these = self.filter({self.getPrimaryKey(): key})
        assert len(these) == 1, \
            (self, key, 'get: existence and uniqueness failure')
        return these[0]

    def dumpAll(self):
        n = len(self._name)
        banner1 = 50 * "*"
        a = (50 - n) / 2 - 1
        banner2 = (a * "*") + " " + self._name + " "
        banner2 += (50 - len(banner2)) * "*"
        print banner1
        print banner2
        print banner1
        print "Primary keys:", self._primaryKeyNames
        cols = self.getColumns()
        for row in self.rows():
            for c in cols:
                print c, ":", getattr(row, c._name)
            print "================================="

    def stringify(self, obj):
        if obj is None:
            return 'NULL'
        if type(obj) is types.LongType:
            # no trailing 'L' please
            return str(obj)
        elif type(obj) is datetime.datetime:
            return obj.strftime('"%Y-%m-%d %H:%M:%S"')
        else:
            return repr(obj)

    def writeToDb(self, rows):
        if WRITE_PROTECT:
            raise WriteProtectException()
        assert len(self._primaryKeyNames) > 0, (self, 'writeToDb: no primary key')
        for (key, row) in rows.items():
            try:
                self[key]
                useUpdate = True
            except KeyError:
                useUpdate = False
            if useUpdate:
                sql = "UPDATE " + self._name + " SET "
                sql2 = ""
                for keyname in self.getNonKeys():
                    sql2 += "," + keyname + "=" + \
                        self.stringify(getattr(row, keyname))
                sql += sql2[1:]

                sql += " WHERE "
                sql2 = ""
                for keyname in self._primaryKeyNames:
                    sql2 += " AND " + keyname + "=" + \
                        self.stringify(getattr(row, keyname))
                sql += sql2[5:]
            else:
                sql = ("INSERT INTO " + self._name + " (" +
                       (",".join(self._colnames)) +
                       ") VALUES (" +
                       (",".join(map(lambda cname, row=row, self=self: \
                                         self.stringify(getattr(row, cname)),
                                     self._colnames))) +
                       ")")
            handleSql(sql)
        _db.commit()

    def __repr__(self):
        return '<Table ' + self._name + '>'

    def dumpSchema(self):
        return {
            "tableName": self._name,
            "columns": self._columns,
            "primaryKeys": self._primaryKeyNames
            }


def importMysqlSchema(db, host="localhost", port=3306,
                      user="root", passwd="password",
                      namespace=None, _cache={}):
    key = (host, port, user, passwd)
    if not _cache.has_key(key):
        schema = { }

        setDatabase(db, host=host, port=port,
                    user=user, passwd=passwd)

        tables = [ ]
        def showTables(x, tables=tables):
            tables.append(x[0])
        handleSql("show tables", showTables)

        for t in tables:
            columns = [ ]
            primaryKeys = [ ]
            def getNewColumn(fields, c=columns, pk=primaryKeys):
                col = Column(fields)
                c.append(col)
                if fields[3] == 'PRI':
                    # this column is a primary key
                    pk.append(fields[0])
            handleSql("describe " + t, getNewColumn)
            schema["Table_" + t] = MysqlTable(t, primaryKeys, columns)
        _cache[key] = schema

    if namespace is not None:
        for tablename, table in _cache[key].items():
            namespace[tablename] = table

    return _cache[key]

if __name__ == '__main__':
    # These unit tests are currently the best documentation for this module,
    # so I'll try to explain things clearly. Roughly speaking, mysql is an
    # object-relational mapping system that tries to be as Pythonic as possible.
    import unittest

    # If the database previously existed, blow it away. Then construct it anew. In
    # the MySQL database I'm using on this machine, the root user has no password.
    print 'database prep...',
    sys.stdout.flush()
    os.system('mysql -u root -e "drop database pythontest;" 2> /dev/null')
    cmd = "create database pythontest; use pythontest; source testschema.sql;"
    assert os.system('mysql -u root -e "' + cmd + '"') == 0

    # This function returns a dict whose values are the MySQL tables, and whose
    # keys are the table names preceded by "Table_". By including
    # namespace=globals() in the argument list, these same bindings are copied
    # into the global namespace, so we can write "Table_body" and Python knows
    # what we mean.
    importMysqlSchema('pythontest', user='root', passwd=None, namespace=globals())
    print 'done'
    sys.stdout.flush()

    # A Row represents a row in a MySQL table. We can assign special methods
    # to rows of particular tables. For instance it makes sense for an
    # astronomical body to ask what other bodies orbit around it.
    @RowMethod(Table_body)
    def getSatellites(self):
        sats = [ ]
        # find all entries E in orbit table where E.big_id = self.id
        for E in Table_orbit.filter({'big_id': self.id}):
            # then find all entries B in body table with B.id = E.small_id
            for B in Table_body.filter({'id': E.small_id}):
                # collect all those results, they are satellites of self
                sats.append(B)
        return sats

    # It should be possible to overload a row method, with the overloaded behavior
    # applying only to a specific table. Let's try that with the A and B tables,
    # and the getKey row method.
    @RowMethod(Table_B)
    def getKey(self):
        return len(self.info)

    # If we create a new row method for a table, as opposed to overloading an
    # existing row method, then the rows of the other tables should behave as
    # if that method doesn't exist. So this should work for table B, and raise
    # AttributeException for table A.
    @RowMethod(Table_B)
    def novelRowMethod(self):
        return 'novelRowMethod'

    class TestDatabaseFunctions(unittest.TestCase):

        def getBody(self, name):
            return Table_body.filter(name=name)[0]

        def setUp(self):
            self.sol = self.getBody("Sol")

            self.mercury = self.getBody("Mercury")
            self.venus = self.getBody("Venus")
            self.earth = self.getBody("Earth")
            self.mars = self.getBody("Mars")
            self.jupiter = self.getBody('Jupiter')
            self.saturn = self.getBody('Saturn')
            self.uranus = self.getBody('Uranus')
            self.neptune = self.getBody('Neptune')
            self.pluto = self.getBody('Pluto')

            self.luna = self.getBody('Luna')
            self.phobos = self.getBody('Phobos')
            self.deimos = self.getBody('Deimos')
            self.io = self.getBody('Io')
            self.europa = self.getBody('Europa')
            self.ganymede = self.getBody('Ganymede')

        def test_tableAttributes(self):
            self.assertEqual(Table_body.getName(), "body")
            self.assertEqual(map(lambda c: c._name,
                                 Table_body.getColumns()),
                             ["id", "name"])
            self.assertEqual(Table_body.getPrimaryKeys(), ["id"])
            self.assertEqual(Table_body.getPrimaryKey(), "id")

            self.assertEqual(Table_orbit.getName(), "orbit")
            self.assertEqual(map(lambda c: c._name,
                                 Table_orbit.getColumns()),
                             ["big_id", "small_id"])

        def test_planets(self):
            for pname in ("Mercury", "Venus", "Earth",
                          "Mars", "Jupiter", "Saturn",
                          "Uranus", "Neptune", "Pluto"):
                planet = [planet for planet in Table_body.filter(name=pname)][0]
                self.assertTrue(planet in self.sol.getSatellites())

        def test_moons(self):
            self.assertEqual(
                self.earth.getSatellites(),
                [self.luna])
            self.assertEqual(
                self.mars.getSatellites(),
                [self.phobos,
                 self.deimos])
            self.assertEqual(
                self.jupiter.getSatellites(),
                [self.io,
                 self.europa,
                 self.ganymede])

        def test_numericKeys(self):
            # vanilla indices
            self.assertEqual(Table_body[1], self.sol)
            self.assertEqual(Table_body[2], self.mercury)
            self.assertEqual(Table_body[3], self.venus)
            self.assertEqual(Table_body[4], self.earth)
            self.assertEqual(Table_body[5], self.mars)

        def test_rows(self):
            self.assertEqual(list(Table_body.rows()),
                             [self.sol,
                              self.mercury,
                              self.venus,
                              self.earth,
                              self.mars,
                              self.jupiter,
                              self.saturn,
                              self.uranus,
                              self.neptune,
                              self.pluto,
                              self.luna,
                              self.phobos,
                              self.deimos,
                              self.io,
                              self.europa,
                              self.ganymede])

        def test_filter(self):
            # filtering with a predicate
            def middlePlanets(planet):
                return planet.name in ("Earth", "Mars")
            self.assertEqual(list(Table_body.filter(middlePlanets)),
                             [self.earth,
                              self.mars])
            # filtering with a dict
            self.assertEqual(list(Table_body.filter({"id": 8})),
                             [self.uranus])
            # filtering with a python keyword
            self.assertEqual(list(Table_body.filter(id=8)),
                             [self.uranus])
            self.assertEqual(list(Table_body.filter(name='Jupiter')),
                             [self.jupiter])
            self.assertEqual(list(Table_body.filter(id=8, name='Jupiter')),
                             [])
            # how many planets orbit the sun
            planets = list(Table_orbit.filter(big_id=self.sol.id))
            self.assertEqual(len(planets), 9)

        def test_multiplePrimaryKeys(self):
            def fetch(key):
                return Table_userdata[key]

            self.assertRaises(KeyError, fetch, 'a')
            self.assertRaises(KeyError, fetch, 1)
            self.assertRaises(KeyError, fetch, (1,))
            self.assertRaises(KeyError, fetch, (1, 2, 3))
            self.assertRaises(KeyError, fetch, (6, 6))

            self.assertEqual(Table_userdata[(1, 1)].info, 'Alpha')
            self.assertEqual(Table_userdata[(1, 2)].info, 'Bravo')
            self.assertEqual(Table_userdata[(1, 3)].info, 'Charlie')
            self.assertEqual(Table_userdata[(2, 1)].info, 'Delta')
            self.assertEqual(Table_userdata[(2, 2)].info, 'Echo')
            self.assertEqual(Table_userdata[(3, 3)].info, 'India')

        def test_setitem(self):
            global WRITE_PROTECT
            WRITE_PROTECT = False
            Table_userdata[(1, 1)] = {'info': 'Arnold'}
            self.assertEqual(Table_userdata[(1, 1)].info, 'Arnold')
            Table_userdata[(1, 1)] = {'info': 'Alpha'}
            WRITE_PROTECT = True

        def test_databaseWrite(self):
            global WRITE_PROTECT
            WRITE_PROTECT = False
            row = Row.make(Table_body, [1, 'TheSun'])
            Table_body.writeToDb({row.getKey(): row})
            self.assertEqual(Table_body[1].name, 'TheSun')
            row = Row.make(Table_body, [1, 'Sol'])
            Table_body.writeToDb({row.getKey(): row})
            WRITE_PROTECT = True

        def test_writeProtect(self):
            def tryWrite():
                row = Row.make(Table_body, [1, 'TheSun'])
                Table_body.writeToDb({row.getKey(): row})
            self.assertRaises(WriteProtectException, tryWrite)

        def test_rowMethodOverload(self):
            self.assertEqual(Table_A[1].getKey(), 1)
            self.assertEqual(Table_A[2].getKey(), 2)
            self.assertEqual(Table_A[3].getKey(), 3)
            # For rows of the B table, getKey() should return the length of the
            # info string, respectively 4*"X", 5*"X", and 6*"X".
            self.assertEqual(Table_B[1].getKey(), 4)
            self.assertEqual(Table_B[2].getKey(), 5)
            self.assertEqual(Table_B[3].getKey(), 6)

            # For table A, we get:
            # AttributeError: Row instance has no attribute 'novelRowMethod'
            self.assertRaises(AttributeError, Table_A[1].novelRowMethod)
            self.assertRaises(AttributeError, Table_A[2].novelRowMethod)
            self.assertRaises(AttributeError, Table_A[3].novelRowMethod)
            self.assertEqual(Table_B[1].novelRowMethod(), 'novelRowMethod')
            self.assertEqual(Table_B[2].novelRowMethod(), 'novelRowMethod')
            self.assertEqual(Table_B[3].novelRowMethod(), 'novelRowMethod')

    unittest.main()

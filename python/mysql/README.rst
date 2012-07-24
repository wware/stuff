MySQL Python script
===================

The mysql.py module does most of the heavy lifting in some MySQL work that I
need to do for my job. Each MySQL table in our schema becomes an object, with
an associated list of column objects, and an associated dictionary of row
objects.

* If one primary key is found then the keys of the row dict are the values of
  that primary key.

* If multiple primary keys are found then the keys of the row dict are tuples of
  the primary key values.

* In the absence of primary keys, rows in a table have no inherent order.

Based on these keys, tables have ``__getitem__`` and ``__setitem__``methods.

::

    # SQL:
    create table userdata (
        userid     BIGINT NOT NULL,
        userdataid BIGINT NOT NULL,
        info VARCHAR(20),
        PRIMARY KEY (userid, userdataid)
    ) ENGINE=InnoDB;
    insert into userdata (userid, userdataid, info)
                values (3, 2, "Hotel");
    
    # Python:
    >>> print Table_userdata[(3,2)].info
    Hotel

You can filter the rows of a table (in a method similar to Python's native
filter operation) based on a callable predicate, or a dictionary of key-value
pairs, or keywords of the table's filter method. If Table_body is a table of
astronomical bodies, you might say something like this.

::

    # filtering with a predicate
    >>> def middlePlanets(planet):
    ...    return planet.name in ("Earth", "Mars")
    >>> print Table_body.filter(middlePlanets)
    [<earth>, <mars>]
    
    # filtering with a dictionary
    >>> print Table_body.filter({"id": 8})
    <uranus>
    
    # filtering with a python keyword
    >>> print Table_body.filter(id=8)
    <uranus>

To see more about how this module works and what it can do, look at
``test-mysql.py``.

For the present, this module is read-only. It does not attempt to write to the
database. It is assumed that when you read out some rows of a table, you will
treat them as immutable.

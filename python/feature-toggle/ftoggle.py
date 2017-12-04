# pylint: disable=unused-argument
# pylint: disable=global-statement
import unittest
from functools import wraps
import sqlite3

#################
# Database stuff

conn = None

SCHEMA = """
    CREATE TABLE toggles
    (Key VARCHAR(255), Value BOOLEAN, LastChanged DATETIME)
"""


def set_toggle(key, value):
    cur = conn.cursor()
    cur.execute("""
        INSERT INTO toggles (Key, Value, LastChanged)
        VALUES ('{0}', {1}, DATETIME('NOW'))
    """.format(key, 1 if value else 0))
    conn.commit()


def get_from_db(key):
    cur = conn.cursor()
    cur.execute("""
        SELECT Value FROM toggles WHERE Key = '{0}'
        ORDER BY LastChanged DESC LIMIT 1
    """.format(key))
    return cur.fetchone()[0] != 0

###########################################
# Here is the decorator and example uses.


def feature_toggle(key, oldf=None):
    def nop(*args, **kwargs):
        return None
    if oldf is None:
        oldf = nop

    def decfunc(newf):
        @wraps(newf)
        def inner(*args, **kwargs):
            if get_from_db(key):
                return newf(*args, **kwargs)
            else:
                return oldf(*args, **kwargs)
        return inner
    return decfunc


def old_behavior_one():
    return "old behavior one"


@feature_toggle("my cool new feature", old_behavior_one)
def feature_one():
    return "new behavior one"


# Introducing new behavior, there is no old behavior.
@feature_toggle("my other cool feature")
def feature_two():
    return "new behavior two"


############################################
# Now let's test it and make sure it works.


class FeatureToggleTest(unittest.TestCase):
    def setUp(self):
        global conn
        conn = sqlite3.connect(":memory:")
        cur = conn.cursor()
        cur.execute(SCHEMA)
        conn.commit()

    def tearDown(self):
        conn.close()

    def test1(self):
        set_toggle('my cool new feature', True)
        set_toggle('some useless other thing', False)
        self.assertEqual(feature_one(), "new behavior one")

    def test2(self):
        set_toggle('my cool new feature', False)
        set_toggle('some useless other thing', False)
        self.assertEqual(feature_one(), "old behavior one")

    def test3(self):
        set_toggle('my other cool feature', True)
        self.assertEqual(feature_two(), "new behavior two")

    def test4(self):
        set_toggle('my other cool feature', False)
        self.assertEqual(feature_two(), None)

if __name__ == '__main__':
    unittest.main()

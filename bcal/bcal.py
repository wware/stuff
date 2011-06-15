import os
import cgi
import datetime
import logging
import random
import string
import sys
import types
import urllib
import wsgiref.handlers

from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

import gdata.calendar.service
import gdata.service
import gdata.apps

# background image has 30 pixels per band
SPACER_WIDTH = 30
DISPLAY_WIDTH = 1200
PIXELS_PER_HOUR = 60.0

# configurable on a settings page?
EMPTY_SLOT_DURATION = 30   # in minutes

# configurable on a settings page?
START_OF_BUSINESS = datetime.time(7)
FINISH_OF_BUSINESS = datetime.time(19)

TIME_WIDTH = 150

def rootdir(filename):
    return os.path.join(os.path.dirname(__file__), filename)

def getColors(index, n):
    class Color:
        def __init__(self, r, g, b):
            self.r, self.g, self.b = r, g, b
        def __add__(self, x):
            return Color(self.r + x.r, self.g + x.g, self.b + x.b)
        def __rmul__(self, k):
            return Color(k * self.r, k * self.g, k * self.b)
        def html(self):
            def limit(x):
                return min(255, max(0, int(x * 256)))
            return '#%02x%02x%02x' % \
                (limit(self.r), limit(self.g), limit(self.b))
    def endpoint(x, r):
        assert 0.0 <= x <= 1.0
        A = 3. * (1 - r)
        if x < 1./3:
            x *= A
            return Color(1.0, r + x, 1.0 - x)
        elif x < 2./3:
            x = A * (x - 1./3)
            return Color(1.0 - x, 1.0, r + x)
        else:
            x = A * (x - 2./3)
            return Color(r + x, 1.0 - x, 1.0)
    bright = endpoint(1.0 * index / n, 1.0)
    dull = endpoint(1.0 * index / n, 0.0)
    middle = 0.5 * (bright + dull)
    return (bright.html(), middle.html(), dull.html())


################################################
#                                              #
#               Database models                #
#                                              #
################################################

class Counselor(db.Model):
    first_name = db.StringProperty()
    last_name = db.StringProperty()
    name = db.StringProperty()   # google account username
    groups = db.ListProperty(db.Key)
    events = db.ListProperty(db.Key)

class Group(db.Model):
    name = db.StringProperty()
    owner = db.UserProperty()

class Event(db.Model):
    description = db.StringProperty()
    start_time = db.DateTimeProperty()
    duration = db.IntegerProperty()   # minutes
    last_updated = db.DateTimeProperty(auto_now_add=True)
    googleid = db.StringProperty()

class SessionToken(db.Model):
    # used to gain read/write access to the counselor's calendar
    token = db.StringProperty()
    counselor = db.StringProperty()
    owner = db.UserProperty()   # the receptionist
    when_fetched = db.DateTimeProperty()


def getCounselorList():
    #
    # TODO: derive the following from a GQL query instead of faking it
    #
    REPLACE_WITH_GQL_QUERY = True
    if REPLACE_WITH_GQL_QUERY:
        class Counselor:
            def __init__(self, first, last, uname):
                self.first_name = first
                self.last_name = last
                self.name = uname
        return [
            Counselor('Bob', 'Marley', 'bob.marley'),
            Counselor('Jim', 'Morrison', 'jim.morrison'),
            Counselor('Gordon', 'Sumner', 'sting'),
            Counselor('David', 'Bowie', 'ziggy'),
            Counselor('Suzanne', 'Hoffs', 'suzieq'),
            Counselor('Jerome Y.', 'Garcia', 'capntrips'),
            Counselor('Jermaine', 'Jackson', 'germane'),
            Counselor('Deborah', 'Harry', 'blondie'),
            ]

def getGroupList():
    #
    # TODO: derive the following from a GQL query instead of faking it
    #
    REPLACE_WITH_GQL_QUERY = True
    if REPLACE_WITH_GQL_QUERY:
        class Group:
            def __init__(self, name, owner):
                random.seed(name)
                self.name = name
                self.owner = owner
                self.members = getCounselorList()
                for i in range(len(self.members)):
                    self.members[i].included = (random.random() > 0.5)

        return [
            Group('Group A', 'Beverly Sills'),
            Group('Group B', 'Reba McEntire'),
            Group('Group C', 'Donna Summer'),
            Group('Group D', 'Annie Lennox'),
            ]

def getGroupByName(name):
    d = { }
    for g in getGroupList():
        d[g.name] = g
    try:
        return d[name]
    except KeyError:
        return None


################################################
#                                              #
#             Google Calendar API              #
#              and helper classes              #
#                                              #
################################################

class EmptySlot:
    def __init__(self, start, duration=EMPTY_SLOT_DURATION):
        self.start = start
        self.dt = datetime.timedelta(minutes=duration)
        self.duration = duration
        self.finish = start + self.dt
        self.height = int((duration / 60.) * PIXELS_PER_HOUR)

    def __repr__(self):
        return '<%s %s>' % (self.__class__.__name__, self.start)

    @classmethod
    def pack(cls, t1, t2):
        """
        Generate a list of EmptySlots starting at time t1 and running until
        t2, with edges on the 15-minute or 30-minute marks.
        """
        todayStart = datetime.datetime.combine(datetime.date.today(),
                                               START_OF_BUSINESS)
        assert t1 >= todayStart
        assert t2 >= todayStart

        def divmod(T, todayStart=todayStart):
            from math import floor
            dt = (T - todayStart).seconds / 60.
            n = int(floor(dt / EMPTY_SLOT_DURATION))
            r = dt % EMPTY_SLOT_DURATION
            return (n, r)

        lst = [ ]
        n2, r2 = divmod(t2)
        while t1 < t2:
            n1, r1 = divmod(t1)
            if n1 == n2:
                e = cls(t1, r2 - r1)
            elif r1 != 0:
                e = cls(t1, EMPTY_SLOT_DURATION - r1)
            else:
                e = cls(t1)
            lst.append(e)
            t1 = e.finish
        return lst

class EventSlot(EmptySlot):
    """
    This is an event that may not exist in the DB. It still needs to be
    renderable in the group view.
    """
    def __init__(self, counselor, description, bgcolor, start, duration):
        EmptySlot.__init__(self, start, duration)
        self.height = int((duration / 60.) * PIXELS_PER_HOUR) - 4
        self.counselor = counselor
        self.description = description
        self.bgcolor = bgcolor
        self.id = None   # google assigns this, we don't

    def __getattr__(self, attr):
        if attr == 'modifiable':
            if self.id is None:
                # Google hasn't yet assigned an ID. Let's assume it's
                # not safe to modify an event that hasn't yet been
                # safely stored in Google Calendar.
                return False
            else:
                howmany = Event.gql("WHERE googleid = '" + id + "'").count()
                assert howmany in (0, 1)
                return (howmany == 1)

    def overlapsWith(self, otherEvents):
        def overlap(evt1, evt2):
            if evt2.finish <= evt1.start:
                return False
            if evt1.finish <= evt2.start:
                return False
            return True
        for other in otherEvents:
            if overlap(self, other):
                return True
        return False

    def store(self):
        e = Event(description=self.description,
                  start=self.start,
                  duration=self.duration)
        e.put()
        counselor = Counselor.gql("WHERE name = " + self.counselor).get()
        counselor.events.append(e.key())
        counselor.put()

    def updateGoogleId(self, id):
        self.id = id
        #
        # TODO: find a matching event in the Event table whose googleid has
        # not yet been assigned. Make sure the right counselor owns the event.
        #
        matchingEvent = None # TODO
        matchingEvent.googleid = id
        matchingEvent.put()


class GoogleCalendar:
    def __init__(self, name):
        """
        You probably want some way to to cache calendar instances on a
        per-counselor basis. There may be a lot of overhead in setting
        up each calendar instance.

        http://code.google.com/appengine/articles/gdata.html
        http://code.google.com/appengine/articles/more_google_data.html

        One thing that isn't immediately obvious is how to handle the
        authentication in the situation where the counselor has shared
        his or her calendar with the receptionist. So the email/password
        are the receptionist's, and somehow I guess we stick the counselor's
        username in the URL for getting the feed and editing the event.
        """
        self.service = gdata.service.GDataService()
        #gdata.alt.appengine.run_on_appengine(self.service)

        if False:
            cal_client = gdata.calendar.service.CalendarService()
            cal_client.email = email
            cal_client.password = password
            cal_client.ProgrammaticLogin()
            query = gdata.calendar.service\
                    .CalendarEventQuery('default', 'private', 'full')
            query.start_min = start.strftime('%Y-%m-%d')
            query.start_max = finish.strftime('%Y-%m-%d')
            feed = cal_client.CalendarQuery(query)

            # http://code.google.com/apis/accounts/docs/AuthSub.html
            # Working with AuthSub
            #
            # 1. Decide whether or not to register your web application.
            #    ===> for now, let's not bother to register
            #
            # 2. Decide what type of tokens to use and how to manage them.
            #    ===> session tokens
            #
            # 3. Determine the scope required by the Google service to be accessed.
            #
            # 4. Set up a mechanism to request and receive an authorization token.
            #
            # 5. Set up mechanisms to request session tokens and store or revoke
            #    them, if relevant.
            #
            # 6. Set up a mechanism to request access to a Google service.

    def getEvents(self, date):
        """
        Use gcal api to fetch one day of events for this counselor.
        Accept only the window from start of business to finish of
        business.
        """
        assert type(date) == datetime.date, (type(date), date)
        startOfBusiness = datetime.datetime.combine(date, START_OF_BUSINESS)
        finishOfBusiness = datetime.datetime.combine(date, FINISH_OF_BUSINESS)
        #
        # TODO get a list rawEvents from Google Calendar
        #
        def today(hour, min):
            return datetime.datetime.combine(datetime.date.today(),
                                             datetime.time(hour, min))
        rawEvents = [
            Event(description='Do one thing',
                  start_time=today(8, 30),
                  duration=90),
            Event(description='Do another thing',
                  start_time=today(9, 0),
                  duration=90),
            Event(description='Do a third thing',
                  start_time=today(9, 15),
                  duration=30),
            Event(description='Lunch',
                  start_time=today(12, 0),
                  duration=60),
            Event(description='Golf',
                  start_time=today(14, 30),
                  duration=90),
            Event(description='Practice saying supercalifragilistic expialidocious',
                  start_time=today(16, 30),
                  duration=30)
            ]

        def trimToBusinessHours(event,
                                startOfBusiness=startOfBusiness,
                                finishOfBusiness=finishOfBusiness):
            """
            Discard events that finish at or before start of business,
            or begin at or after close of business. If a remaining
            event starts before start of business, change its start
            time to start of business. If a remaining event ends after
            close of business, change its finish time to close of
            business.
            """
            event.start = event.start_time
            event.finish = event.start + datetime.timedelta(minutes=event.duration)
            if event.finish <= startOfBusiness or \
                event.start >= finishOfBusiness:
                return None
            if event.start < startOfBusiness:
                event.start = startOfBusiness
            if event.finish > finishOfBusiness:
                event.finish = finishOfBusiness
            return event
        return filter(lambda x: x is not None,
                      map(trimToBusinessHours, rawEvents))

    def addEvent(self, event):
        assert isinstance(event, Event)

    def getEventId(self, event):
        """
        This is (1) to confirm that Google Calendar has succeeded in
        storing the event in the counselor's calendar, and (2) record
        the ID so we know which events we can safely modify.
        """
        return None

# Will App Engine persist the cache from one request to another? Not if
# they run on different servers. Is cacheing a wasted effort?
def getCalendar(name, cache={ }):
    if not cache.has_key(name):
        cache[name] = GoogleCalendar(name)
    return cache[name]


################################################
#                                              #
#                 HTML views                   #
#                                              #
################################################

class FavIcon(webapp.RequestHandler):
    def get(self):
        self.response.out.write(open(rootdir('favicon.ico')).read())

#####

class MainPage(webapp.RequestHandler):
    def get(self):
        self.response.headers['Content-Type'] = 'text/html'
        self.response.out.write(template.render(rootdir('main.html'),
                                                { 'groups': getGroupList() }))

class CalendarPage(webapp.RequestHandler):

    def get(self):
        #
        # TODO: handle dates other than today
        #
        date = datetime.date.today()   # take from a CGI parameter instead
        datestr = date.strftime('%Y%m%d')
        assert len(datestr) == 8

        self.response.headers['Content-Type'] = 'text/html'

        height = int((EMPTY_SLOT_DURATION / 60.) * PIXELS_PER_HOUR)
        times = [ ]
        t = datetime.datetime.combine(date,
                                      START_OF_BUSINESS)
        done = datetime.datetime.combine(date,
                                         FINISH_OF_BUSINESS)
        dt = datetime.timedelta(minutes=EMPTY_SLOT_DURATION)
        while t <= done:
            times.append(t.strftime("%I:%M %p"))
            t += dt

        groupname = urllib.unquote(self.request.get('groupname'))
        counselors = getGroupByName(groupname).members
        counselors = filter(lambda c: c.included, counselors)
        columns = [ ]
        for counselor in counselors:
            index = len(columns)
            width = int(DISPLAY_WIDTH / len(counselors)) - SPACER_WIDTH - 4
            assert len(datestr) == 8
            params = {
                'index': index,
                'numcols': str(len(counselors)),
                'name': counselor.name,
                'date': datestr,
                'width': width
                }

            columns.append({
                'id': '%d' %  index,
                'first_name': counselor.first_name,
                'last_name': counselor.last_name,
                'name': counselor.name,
                'width': width,
                'params': urllib.urlencode(params)
                })

        self.response.out.write(template.render(rootdir('calendar.html'),
                                                {'columns': columns,
                                                 'times': times,
                                                 'height': height,
                                                 'spacerwidth': SPACER_WIDTH}))

class CounselorsPage(MainPage):

    def get(self):
        self.response.headers['Content-Type'] = 'text/html'
        counselors = getCounselorList()

        self.response.out.write(template.render(rootdir('counselors.html'),
                                                { 'counselors': counselors }))

class GroupsPage(MainPage):

    def get(self):
        self.response.headers['Content-Type'] = 'text/html'
        groups = getGroupList()

        self.response.out.write(template.render(rootdir('groups.html'),
                                                { 'groups': groups }))



################################################
#                                              #
#                    Popups                    #
#                                              #
################################################

class PopupBaseClass(webapp.RequestHandler):

    def get(self):
        self.response.headers['Content-Type'] = 'text/html'
        class Param: pass
        params = [ ]
        values = [ ]
        for key in self.request.arguments():
            for value in self.request.get_all(key):
                p = Param()
                p.key = key
                p.value = value
                params.append(p)
                values.append(value)
        # kludge if it's a group
        members = None
        for v in values:
            g = getGroupByName(v)
            if g is not None:
                members = g.members
                break
        self.response.out.write(template.render(rootdir('popup.html'),
                                                { 'params': params,
                                                  'members': members }))

    post = get

class PopupDeleteBaseClass(PopupBaseClass):

    # be sure to set TYPE

    def get(self):
        self.response.headers['Content-Type'] = 'text/html'
        if self.request.get('confirm') == '1':
            pass
        else:
            class Param: pass
            params = [ ]
            values = [ ]
            for key in self.request.arguments():
                for value in self.request.get_all(key):
                    p = Param()
                    p.key = key
                    p.value = value
                    params.append(p)
                    values.append(value)
            self.response.out.write(template.render(rootdir('areyousure.html'),
                                                    { 'type': self.TYPE,
                                                      'name': self.request.get('name'),
                                                      'params': params }))

    post = get


class EditCounselorPopup(PopupBaseClass):
    TYPE = 'counselor'

class DeleteCounselorPopup(PopupDeleteBaseClass):
    TYPE = 'counselor'

class EditGroupPopup(PopupBaseClass):
    TYPE = 'group'

class DeleteGroupPopup(PopupDeleteBaseClass):
    TYPE = 'group'

class EditEventPopup(PopupBaseClass):
    TYPE = 'event'

class DeleteEventPopup(PopupDeleteBaseClass):
    TYPE = 'event'


################################################
#                                              #
#                AJAX operations               #
#                                              #
################################################

class AjaxBaseClass(webapp.RequestHandler):
    def get(self):
        pass
    def post(self):
        pass

class ColumnAjax(AjaxBaseClass):

    def get(self):

        args = { }
        for key in self.request.arguments():
            value = self.request.get(key)
            if key != 'date':
                try:
                    value = string.atoi(value)
                except ValueError:
                    pass
            args[key] = value

        index = args['index']
        numcols = args['numcols']
        name = args['name']
        date = args['date']
        screenwidth = args['screenwidth']
        bright, middle, dull = getColors(index, numcols)

        assert screenwidth is not None
        width = (screenwidth - TIME_WIDTH) / numcols - 30

        def minutesHeight(mins):
            return int((1. * mins / 60) * PIXELS_PER_HOUR) - 4

        gcal = getCalendar(name)
        assert type(date) in (types.StringType, types.UnicodeType)
        assert len(date) == 8

        def fromIso(yyyymmdd):
            y, m, d = map(string.atoi,
                          [yyyymmdd[:4], yyyymmdd[4:6], yyyymmdd[6:]])
            return datetime.date(y, m, d)
        date = fromIso(date)

        events = gcal.getEvents(date)

        # map gcal events to EventSlots
        def gcalToEventSlot(e):
            assert e is not None
            es = EventSlot(counselor=name,
                           description=e.description,
                           bgcolor=middle,
                           start=e.start_time,
                           duration=e.duration)
            assert es is not None
            assert logging.debug is not None
            return es
        assert gcalToEventSlot is not None
        events = map(gcalToEventSlot, events)

        # Look at event overlaps, assign events to subcolumns
        subcolumns = [ [ ] ]
        for event in events:
            assert event is not None
            n = 0
            while True:
                if not event.overlapsWith(subcolumns[n]):
                    subcolumns[n].append(event)
                    break
                n += 1
                if n >= len(subcolumns):
                    subcolumns.append([ event ])
                    break

        # Pack each subcolumn with empty slots to fill time
        todayStart = datetime.datetime.combine(datetime.date.today(),
                                               START_OF_BUSINESS)
        todayFinish = datetime.datetime.combine(datetime.date.today(),
                                                FINISH_OF_BUSINESS)
        for i in range(len(subcolumns)):
            subcol = subcolumns[i]
            subcol2 = [ ]
            t = todayStart
            for event in subcol:
                subcol2 += EmptySlot.pack(t, event.start)
                subcol2.append(event)
                t = event.finish
            subcol2 += EmptySlot.pack(t, todayFinish)
            subcolumns[i] = subcol2

        if len(subcolumns) > 0:
            width = int(1.0 * width / len(subcolumns) - 4)
        else:
            width = 100

        html = template.render(rootdir('column.html'),
                               {'subcolumns': subcolumns,
                                'width': width,
                                'spacerwidth': SPACER_WIDTH})

        # json response
        self.response.headers['Content-Type'] = 'text/plain'
        self.response.out.write('{"html": "%s", "width": %d}'
                                % (urllib.quote(html), len(subcolumns) * (width + 4)))

    def post(self):
        assert False

class CounselorInfoPost(AjaxBaseClass):
    pass

class GroupInfoPost(AjaxBaseClass):
    pass

class EventInfoPost(ColumnAjax):
    pass


####

application = webapp.WSGIApplication([
        ('/favicon.ico', FavIcon),
        # Normal HTML views are fetched with GETs
        ('/', MainPage),
        ('/counselors', CounselorsPage),
        ('/groups', GroupsPage),
        ('/calendar', CalendarPage),
        # popups are fetched with GETs
        ('/editcounselor', EditCounselorPopup),
        ('/deletecounselor', DeleteCounselorPopup),
        ('/editgroup', EditGroupPopup),
        ('/deletegroup', DeleteGroupPopup),
        ('/editevent', EditEventPopup),
        ('/deleteevent', DeleteEventPopup),
        # AJAX POSTs arrive here
        ('/column', ColumnAjax),
        ('/counselorinfo', CounselorInfoPost),
        ('/groupinfo', GroupInfoPost),
        ('/eventinfo', EventInfoPost),
        ], debug=True)

def main():
    logging.getLogger().setLevel(logging.DEBUG)
    wsgiref.handlers.CGIHandler().run(application)

if __name__ == "__main__":
    main()
    #print json_example

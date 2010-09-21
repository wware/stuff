import datetime
import random
from django.db import models

class Poll(models.Model):
    question = models.CharField(max_length=200)
    pub_date = models.DateTimeField('date published')

    def __unicode__(self):
        return self.question

class Choice(models.Model):
    poll = models.ForeignKey(Poll)
    choice = models.CharField(max_length=200)
    votes = models.IntegerField()

    def __unicode__(self):
        return self.choice

class ShortURL(models.Model):
    """If the code can convey 36 bits, that gives us 64 billion
    possible URLs. We can encode six bits per character so each
    code is six characters long. Each character must come from a
    set of 64 possible characters.
    """
    code = models.CharField(max_length=10)
    longUrl = models.CharField(max_length=500)
    creationDate = models.DateTimeField()
    expirationDate = models.DateTimeField(null=True, blank=True)

    @classmethod
    def handleExpirations(cls):
        now = datetime.datetime.now()
        for surl in (cls.objects
                     .filter(expirationDate__isnull=False)
                     .filter(expirationDate__lte=now)):
            surl.delete()

    @classmethod
    def create(cls, lu,
               charset=list('ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                            'abcdefghijklmnopqrstuvwxyz'+
                            '0123456789-.')):
        cls.handleExpirations()
        # if we've already encoded this URL in the past, use that
        try:
            return cls.objects.get(longUrl=lu)
        except cls.DoesNotExist:
            pass
        # generate a code, make sure it isn't already in use
        x = None
        while True:
            x = ''.join([random.choice(charset) for i in range(6)])
            try:
                cls.objects.get(code=x)
                # found it, gotta try again
            except cls.DoesNotExist:
                # didn't find it, it must be new
                break
        now = datetime.datetime.now()
        surl = cls.objects.create(longUrl=lu,
                                  code=x,
                                  creationDate=now)
        surl.save()
        return surl

    @classmethod
    def lookup(cls, x):
        cls.handleExpirations()
        try:
            return cls.objects.get(code=x).longUrl
        except cls.DoesNotExist:
            return None

    def setLifetime(self, delta):
        assert type(delta) == datetime.timedelta
        self.expirationDate = self.creationDate + delta
        ShortURL.handleExpirations()

    def short(self):
        return 'http://localhost:8000/u/' + self.code

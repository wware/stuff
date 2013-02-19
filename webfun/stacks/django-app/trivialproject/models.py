from django.db import models
from django.core.exceptions import ObjectDoesNotExist

import logging
import urllib
import json
import string

logger = logging.getLogger(__name__)

class City(models.Model):
    city = models.CharField(max_length=100)
    state = models.CharField(max_length=2)
    latitude = models.FloatField(null=True, blank=True)
    longitude = models.FloatField(null=True, blank=True)

    @classmethod
    def find(cls, city, state):
        logger.info("find " + city + " " + state)
        try:
            obj = cls.objects.get(city=city, state=state)
        except ObjectDoesNotExist:
            obj = cls(city=city, state=state)
            url = "http://nominatim.openstreetmap.org/search?q=" + city + "%2C+" + state + "&format=json"
            R = urllib.urlopen(url).read()
            logger.error("Geocode response: " + R)
            info = json.loads(R)[0]
            if info.has_key("lat") and info.has_key("lon"):
                obj.latitude = string.atof(info["lat"])
                obj.longitude = string.atof(info["lon"])
            obj.save()
        return obj

    def __str__(self):
        s = self.city + ", " + self.state
        if self.latitude is not None and self.longitude is not None:
            s += " (%f, %f)" % (self.latitude, self.longitude)
        return s

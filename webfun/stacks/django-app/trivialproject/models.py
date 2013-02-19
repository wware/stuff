from django.db import models

class Location(models.Model):
    """Here's a docstring for a Location.
    """
    name = models.CharField(max_length=256)
    street = models.CharField(max_length=256)
    city = models.CharField(max_length=100)
    state = models.CharField(max_length=100)
    country = models.CharField(max_length=100)
    latitude = models.FloatField(null=True, blank=True)
    longitude = models.FloatField(null=True, blank=True)

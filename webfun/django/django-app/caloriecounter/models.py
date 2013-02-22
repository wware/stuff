from django.db import models
from django.core.exceptions import ObjectDoesNotExist

import logging
import urllib
import json
import string

logger = logging.getLogger(__name__)

class Food(models.Model):
    name = models.CharField(max_length=256)
    serving = models.CharField(max_length=100)
    calories = models.IntegerField()
    carb = models.IntegerField()
    fat = models.IntegerField()
    protein = models.IntegerField()
    fiber = models.IntegerField()

class Entry(models.Model):
    food = models.ForeignKey(Food)
    # if food, then number of servings, otherwise a measurement
    value = models.FloatField()
    when = models.DateTimeField()
    info = models.CharField(max_length=256)
    # user = models.ForeignKey(User)   ...later, much


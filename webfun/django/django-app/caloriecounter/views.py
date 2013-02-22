from django.shortcuts import render_to_response, redirect
#from django.core.exceptions import DoesNotExist
from models import Food, Entry
import settings

import string

def index(request):
    D = {"entrylist": Entry.objects.all(),
         "foodlist": Food.objects.all(),
         # "MEDIA_URL": settings.MEDIA_URL
         }
    return render_to_response('index.html', D)

def newfood(request):
    name = request.GET["name"]
    try:
        Food.objects.get(name__iexact=name)
        raise Exception("Already got this food")
    except Food.DoesNotExist:
        pass
    food = Food()
    for x in request.GET.keys():
        if x in "carb,fiber,protein,fat".split(","):
            value = string.atoi(request.GET[x])
        else:
            value = request.GET[x]
        setattr(food, x, value)
    food.save()
    return redirect('/')

def newentry(request):
#    entry = Entry()
#    for x in request.GET.keys():
#        if x in "carb,fiber,protein,fat".split(","):
#            value = string.atoi(request.GET[x])
#        else:
#            value = request.GET[x]
#        setattr(entry, x, value)
#    entry.save()
    return redirect('/')

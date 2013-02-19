#from django.http import HttpRequest, HttpResponse
from django.shortcuts import render_to_response
from models import Location

def home(request):
    # create a location for no particular reason
    if Location.objects.count() == 0:
        loc = Location(name="Microsoft NERD Center",
                       street="1 Memorial Drive",
                       city="Cambridge",
                       state="MA",
                       country="US")
        loc.save()
    return render_to_response('home.html')

def whois(request):
    name = request.GET.get("name", "") or "World"
    if Location.objects.count() > 0:
        loc = Location.objects.all()[0]
        address = "%s, %s %s" % (loc.street, loc.city, loc.state)
    else:
        address = "unknown"
    return render_to_response('whois.html',
                              {"name": name,
                               "address": address})

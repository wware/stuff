from django.shortcuts import render_to_response
from models import City

def home(request):
    # create a location for no particular reason
    if City.objects.count() == 0:
        City.find("Cambridge", "MA")
    return render_to_response('home.html')

def whois(request):
    name = request.GET.get("name", "") or "World"
    if City.objects.count() > 0:
        city = City.objects.all()[0]
        address = str(city)
    else:
        address = "someplace interesting"
    return render_to_response('whois.html',
                              {"name": name,
                               "address": address,
                               "citylist": City.objects.all()})

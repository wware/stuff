import simplejson

from django.http import HttpResponse, Http404, HttpResponseRedirect
from django.shortcuts import get_object_or_404, render_to_response
from django.core.urlresolvers import reverse
from django.template import RequestContext
from django.views.decorators.csrf import csrf_protect
from django.core.context_processors import csrf

from settings import *
from polls.models import Choice, Poll, ShortURL

def index(request):
    c = { }
    c.update(csrf(request))
    return render_to_response('polls/index.html', c)

def polls(request):
    latest_poll_list = Poll.objects.all().order_by('-pub_date')[:5]
    return render_to_response('polls/polls.html', {'latest_poll_list': latest_poll_list})

def detail(request, poll_id):
    p = get_object_or_404(Poll, pk=poll_id)
    return render_to_response('polls/detail.html', {'poll': p},
                               context_instance=RequestContext(request))

def results(request, poll_id):
    p = get_object_or_404(Poll, pk=poll_id)
    return render_to_response('polls/results.html', {'poll': p})

def vote(request, poll_id):
    p = get_object_or_404(Poll, pk=poll_id)
    try:
        selected_choice = p.choice_set.get(pk=request.POST['choice'])
    except (KeyError, Choice.DoesNotExist):
        # Redisplay the poll voting form.
        return render_to_response('polls/detail.html', {
            'poll': p,
            'error_message': "You didn't select a choice.",
        }, context_instance=RequestContext(request))
    else:
        selected_choice.votes += 1
        selected_choice.save()
        # Always return an HttpResponseRedirect after successfully dealing
        # with POST data. This prevents data from being posted twice if a
        # user hits the Back button.
        return HttpResponseRedirect(reverse('polls.views.results', args=(p.id,)))

def showcode(request, filename):
    content = open(siteroot + '/apidoc/' + filename).read()
    return render_to_response('polls/literal.html', {'filename': filename, 'content': content})

def newShortUrl(request):
    assert request.method == 'POST'
    surl = ShortURL.create(request.POST['url'])
    dct = { 'short': surl.short() }
    from django.shortcuts import render_to_response
    return render_to_response('polls/rawjson.html',
                              {'jsonoutput': simplejson.dumps(dct)})

# Provide JSON result if you want to do this programmatically as a web service.
def lookupShortUrl(request, code):
    assert request.method == 'GET'
    dct = { }
    longUrl = ShortURL.lookup(code)
    if longUrl is not None:
        dct['url'] = longUrl
    if request.GET.has_key('json') and request.GET['json']:
        response = render_to_response('rawjson.html',
                                      {'polls/jsonoutput': json.dumps(dct)})
    else:
        response = render_to_response('polls/redirect.html', dct)
    return response

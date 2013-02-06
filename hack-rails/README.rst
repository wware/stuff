Will learns Rails
=================

* Blog post: http://willware.blogspot.com/2013/02/ruby-and-rails-and-all-that-stuff.html -- a good place to
  look for tutorials about the Ruby language and the Rails platform
* API: http://api.rubyonrails.org/ and guides: http://guides.rubyonrails.org/, more API stuff at
  http://railsapi.com and http://apidock.com/rails
* Best Rails book I've found: http://www.amazon.com/gp/product/B004JLMDOM?ie=UTF8&ref=oce_digital
* Heroku: http://www.heroku.com/ - this place will let you run your Rails app on a single machine for free,
  and deploying to it is discussed in the book above. So that's my plan for the trivial little learning
  app I'm going to put together here.
* Nice tutorial: http://railsforzombies.org/ -- go thru the example provided in the videos, and you'll end
  up with a Twitter clone for zombies, where they can help each other find tasty brains. (Brief zombie
  plug, the movie "Warm Bodies" is funny and charming.)

The source code for the Zombie Twitter app is available at https://github.com/codeschool/RFZ2-ZombieTweets
and I've copied it here.

One thing I find it helpful to do when I'm studying a web app platform is to look at the sequence of events
that occurs in response to an HTTP request. Here's my first cut at what Rails does with it. It seems pretty
straightforward.

* HTTP request arrives
* Rails uses **config/routes.rb** to decide which controller method will handle it, and calls that method. It
  may appear in routes.rb as simply "resources :foobars" in which case http://..../foobar/xyzzy will be
  mapped to the **FoobarsController.xyzzy** method.
* The controller method will usually call methods on the models, something like **Foobar.xyzzyHelper**.
* There are erb templates with Ruby embedded in them, just like any templating system, and they are grouped
  by model, e.g. **app/views/foobar/index.html.erb**. They are referenced in the controller by **format.html**
  calls, and there are also **format.json** calls that (afaik) don't require any templating,.

I'd also like to learn about database joins, and GROUP BY, and stored procedures and functions. So if there
is a clear opportunity to throw those into the mix. I'll do it. Otherwise I'm just following the Twitter for
Zombies example, for now.

#!/usr/bin/perl -w
# http://gtk2-perl.sourceforge.net/doc/intro/
# Maybe this is better than using Glade because we're not depending on
# some mysterious dynamic loading process to work correctly?

use strict;
use Data::Dumper;
use Gtk2 '-init';

my $window = Gtk2::Window->new;
$window->set_title ('Simple Signals');
$window->set_default_size (250, 100);

$window->signal_connect (destroy => sub { Gtk2->main_quit; });

my $button = Gtk2::Button->new ('Click to Quit');

my $user_data = 'Hello';
$button->signal_connect (clicked => \&button_callback, $user_data);
$window->add ($button);

$window->show_all;
Gtk2->main;

sub button_callback 
{
    # print out (button, user_data)
    print Dumper (@_);
    Gtk2->main_quit;
    1;
}

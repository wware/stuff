#!/usr/bin/python2.4
#
# Copyright (C) 2009 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Defines the App Engine-specific robot class and associated handlers."""

__author__ = 'davidbyttow@google.com (David Byttow)'


import logging
import traceback

from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app

import robot_abstract


class RobotCapabilitiesHandler(webapp.RequestHandler):
  """Handler for serving capabilities.xml given a robot."""

  def __init__(self, robot):
    """Initializes this handler with a specific robot."""
    self._robot = robot

  def get(self):
    """Handles HTTP GET request."""
    xml = self._robot.GetCapabilitiesXml()
    self.response.headers['Content-Type'] = 'text/xml'
    self.response.out.write(xml)


class RobotProfileHandler(webapp.RequestHandler):
  """Handler for serving the robot's profile information."""

  def __init__(self, robot):
    """Initializes this handler with a specific robot."""
    self._robot = robot

  def get(self):
    """Handles HTTP GET request."""
    self.response.headers['Content-Type'] = 'application/json'
    self.response.out.write(self._robot.GetProfileJson())


class RobotEventHandler(webapp.RequestHandler):
  """Handler for the dispatching of events to various handlers to a robot.

  This handler only responds to post events with a JSON post body. Its primary
  task is to separate out the context data from the events in the post body
  and dispatch all events in order. Once all events have been dispatched
  it serializes the context data and its associated operations as a response.
  """

  def __init__(self, robot):
    """Initializes self with a specific robot."""
    self._robot = robot

  def get(self):
    """Handles the get event for debugging. Ops usually too long."""
    ops = self.request.get('ops')
    logging.info('get: ' + ops)
    if ops:
      self.request.body = ops
      self.post()
      self.response.headers['Content-Type'] = 'text/html'

  def post(self):
    """Handles HTTP POST requests."""
    json_body = self.request.body
    if not json_body:
      # TODO(davidbyttow): Log error?
      return

    json_body = unicode(json_body, 'utf8')
    logging.info('Incoming: ' + json_body)

    context, events = robot_abstract.ParseJSONBody(json_body)
    for event in events:
      try:
        self._robot.HandleEvent(event, context)
      except:
        logging.error(traceback.format_exc())

    json_response = robot_abstract.SerializeContext(context,
                                                    self._robot.version)
    logging.info('Outgoing: ' + json_response)

    # Build the response.
    self.response.headers['Content-Type'] = 'application/json; charset=utf-8'
    self.response.out.write(json_response.encode('utf-8'))


class Robot(robot_abstract.Robot):
  """Adds an AppEngine setup method to the base robot class.

  A robot is typically setup in the following steps:
    1. Instantiate and define robot.
    2. Register various handlers that it is interested in.
    3. Call Run, which will setup the handlers for the app.

  For example:
    robot = Robot('Terminator',
                  image_url='http://www.sky.net/models/t800.png',
                  profile_url='http://www.sky.net/models/t800.html')
    robot.RegisterHandler(WAVELET_PARTICIPANTS_CHANGED, KillParticipant)
    robot.Run()
  """

  def Run(self, debug=False):
    """Sets up the webapp handlers for this robot and starts listening.

    Args:
      debug: Optional variable that defaults to False and is passed through
          to the webapp application to determine if it should show debug info.
    """
    # App Engine expects to construct a class with no arguments, so we
    # pass a lambda that constructs the appropriate handler with
    # arguments from the enclosing scope.
    app = webapp.WSGIApplication([
        ('/_wave/capabilities.xml', lambda: RobotCapabilitiesHandler(self)),
        ('/_wave/robot/profile', lambda: RobotProfileHandler(self)),
        ('/_wave/robot/jsonrpc', lambda: RobotEventHandler(self)),
    ], debug=debug)
    run_wsgi_app(app)

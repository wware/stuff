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

"""Defines the generic robot classes.

This module provides the Robot class and RobotListener interface,
as well as some helper functions for web requests and responses.
"""

__author__ = 'davidbyttow@google.com (David Byttow)'

import events
import model
import ops
import simplejson
import util


def ParseJSONBody(json_body):
  """Parse a JSON string and return a context and an event list."""
  json = simplejson.loads(json_body)
  # TODO(davidbyttow): Remove this once no longer needed.
  data = util.CollapseJavaCollections(json)
  context = ops.CreateContext(data)
  event_list = [model.Event(event_data) for event_data in data['events']]
  return context, event_list


def SerializeContext(context, version):
  """Return a JSON string representing the given context."""
  context_dict = util.Serialize(context)
  context_dict['version'] = str(version)
  return simplejson.dumps(context_dict)


def NewWave(context, participants=None):
  """Create a new wave with the initial participants on it."""
  # we shouldn't need a wave/wavelet id here, but we do
  wavelet = context.GetRootWavelet()
  return context.builder.WaveletCreate(wavelet.GetWaveId(), wavelet.GetId(), participants)


class Robot(object):
  """Robot metadata class.

  This class holds on to basic robot information like the name and profile.
  It also maintains the list of event handlers and cron jobs and
  dispatches events to the appropriate handlers.
  """

  def __init__(self, name, version, image_url='', profile_url=''):
    """Initializes self with robot information."""
    self._handlers = {}
    self.name = name
    self.version = version
    self.image_url = image_url
    self.profile_url = profile_url
    self.cron_jobs = []

  def RegisterListener(self, listener):
    """Registers all event handlers exported by the given object.

    Args:
      listener: an object with methods corresponding to wave events.
        Methods should be named either in camel case, e.g. 'OnBlipSubmitted',
        or in lowercase, e.g. 'on_blip_submitted', with names corresponding
        to the event names in the events module.
    """
    for event in dir(events):
      if event.startswith('_'):
        continue
      lowercase_method_name = 'on_' + event.lower()
      camelcase_method_name = 'On' + util.ToUpperCamelCase(event)
      if hasattr(listener, lowercase_method_name):
        handler = getattr(listener, lowercase_method_name)
      elif hasattr(listener, camelcase_method_name):
        handler = getattr(listener, camelcase_method_name)
      else:
        continue
      if callable(handler):
        self.RegisterHandler(event, handler)

  def RegisterHandler(self, event_type, handler):
    """Registers a handler on a specific event type.

    Multiple handlers may be registered on a single event type and are
    guaranteed to be called in order.

    The handler takes two arguments, the event properties and the Context of
    this session. For example:

    def OnParticipantsChanged(properties, context):
      pass

    Args:
      event_type: An event type to listen for.
      handler: A function handler which takes two arguments, event properties
          and the Context of this session.
    """
    self._handlers.setdefault(event_type, []).append(handler)

  def RegisterCronJob(self, path, seconds):
    """Registers a cron job to surface in capabilities.xml."""
    self.cron_jobs.append((path, seconds))

  def HandleEvent(self, event, context):
    """Calls all of the handlers associated with an event."""
    for handler in self._handlers.get(event.type, []):
      # TODO(jacobly): pass the event in to the handlers directly
      # instead of passing the properties dictionary.
      handler(event.properties, context)

  def GetCapabilitiesXml(self):
    """Return this robot's capabilities as an XML string."""
    lines = ['<w:version>%s</w:version>' % self.version]

    lines.append('<w:capabilities>')
    for capability in self._handlers:
      lines.append('  <w:capability name="%s"/>' % capability)
    lines.append('</w:capabilities>')

    if self.cron_jobs:
      lines.append('<w:crons>')
      for job in self.cron_jobs:
        lines.append('  <w:cron path="%s" timerinseconds="%s"/>' % job)
      lines.append('</w:crons>')

    robot_attrs = ' name="%s"' % self.name
    if self.image_url:
      robot_attrs += ' imageurl="%s"' % self.image_url
    if self.profile_url:
      robot_attrs += ' profileurl="%s"' % self.profile_url
    lines.append('<w:profile%s/>' % robot_attrs)
    return ('<?xml version="1.0"?>\n'
            '<w:robot xmlns:w="http://wave.google.com/extensions/robots/1.0">\n'
            '%s\n</w:robot>\n') % ('\n'.join(lines))

  def GetProfileJson(self):
    """Returns JSON body for any profile handler.

    Returns:
      String of JSON to be sent as a response.
    """
    data = {}
    data['name'] = self.name
    data['imageUrl'] = self.image_url
    data['profileUrl'] = self.profile_url
    # TODO(davidbyttow): Remove this java nonsense.
    data['javaClass'] = 'com.google.wave.api.ParticipantProfile'
    return simplejson.dumps(data)

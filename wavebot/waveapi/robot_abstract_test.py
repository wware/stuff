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

"""Unit tests for the robot_abstract module."""

__author__ = 'jacobly@google.com (Jacob Lee)'

import unittest

import robot_abstract

DEBUG_DATA = r'{"blips":{"map":{"wdykLROk*13":{"lastModifiedTime":1242079608457,"contributors":{"javaClass":"java.util.ArrayList","list":["davidbyttow@google.com"]},"waveletId":"test.com!conv+root","waveId":"test.com!wdykLROk*11","parentBlipId":null,"version":3,"creator":"davidbyttow@google.com","content":"\n","blipId":"wdykLROk*13","javaClass":"com.google.wave.api.impl.BlipData","annotations":{"javaClass":"java.util.ArrayList","list":[{"range":{"start":0,"javaClass":"com.google.wave.api.Range","end":1},"name":"user/e/davidbyttow@google.com","value":"David","javaClass":"com.google.wave.api.Annotation"}]},"elements":{"map":{},"javaClass":"java.util.HashMap"},"childBlipIds":{"javaClass":"java.util.ArrayList","list":[]}}},"javaClass":"java.util.HashMap"},"events":{"javaClass":"java.util.ArrayList","list":[{"timestamp":1242079611003,"modifiedBy":"davidbyttow@google.com","javaClass":"com.google.wave.api.impl.EventData","properties":{"map":{"participantsRemoved":{"javaClass":"java.util.ArrayList","list":[]},"participantsAdded":{"javaClass":"java.util.ArrayList","list":["monty@appspot.com"]}},"javaClass":"java.util.HashMap"},"type":"WAVELET_PARTICIPANTS_CHANGED"}]},"wavelet":{"lastModifiedTime":1242079611003,"title":"","waveletId":"test.com!conv+root","rootBlipId":"wdykLROk*13","javaClass":"com.google.wave.api.impl.WaveletData","dataDocuments":null,"creationTime":1242079608457,"waveId":"test.com!wdykLROk*11","participants":{"javaClass":"java.util.ArrayList","list":["davidbyttow@google.com","monty@appspot.com"]},"creator":"davidbyttow@google.com","version":5}}'


class TestHelpers(unittest.TestCase):
  """Tests for the web helper functions in abstract_robot."""

  def testParseJSONBody(self):
    context, events = robot_abstract.ParseJSONBody(DEBUG_DATA)

    # Test some basic properties; the rest should be covered by
    # ops.CreateContext.
    blips = context.GetBlips()
    self.assertEqual(1, len(blips))
    self.assertEqual('wdykLROk*13', blips[0].GetId())
    self.assertEqual('test.com!wdykLROk*11', blips[0].GetWaveId())
    self.assertEqual('test.com!conv+root', blips[0].GetWaveletId())

    self.assertEqual(1, len(events))
    event = events[0]
    self.assertEqual('WAVELET_PARTICIPANTS_CHANGED', event.type)
    self.assertEqual({'participantsRemoved': [],
                      'participantsAdded': ['monty@appspot.com']},
                     event.properties)

  def testSerializeContextSansOps(self):
    context, _ = robot_abstract.ParseJSONBody(DEBUG_DATA)
    serialized = robot_abstract.SerializeContext(context, '1')
    self.assertEqual(
        '{"operations": {"javaClass": "java.util.ArrayList", "list": []}, '
        '"javaClass": "com.google.wave.api.impl.OperationMessageBundle", '
        '"version": "1"}',
        serialized)

  def testSerializeContextWithOps(self):
    context, _ = robot_abstract.ParseJSONBody(DEBUG_DATA)
    wavelet = context.GetWavelets()[0]
    blip = context.GetBlipById(wavelet.GetRootBlipId())
    blip.GetDocument().SetText('Hello, wave!')
    serialized = robot_abstract.SerializeContext(context, '1')
    self.assertEquals(
        '{"operations": {"javaClass": "java.util.ArrayList", "list": ['
        '{"blipId": "wdykLROk*13", "index": -1, "waveletId": "test.com!conv+root", "javaClass": "com.google.wave.api.impl.OperationImpl", "waveId": "test.com!wdykLROk*11", "type": "DOCUMENT_DELETE"}, '
        '{"blipId": "wdykLROk*13", "index": 0, "waveletId": "test.com!conv+root", "javaClass": "com.google.wave.api.impl.OperationImpl", "waveId": "test.com!wdykLROk*11", "property": "Hello, wave!", "type": "DOCUMENT_INSERT"}'
        ']}, "javaClass": "com.google.wave.api.impl.OperationMessageBundle", '
        '"version": "1"}',
        serialized)

  def testSerializeContextWithOps2(self):
    context, _ = robot_abstract.ParseJSONBody(DEBUG_DATA)
    wavelet = context.GetRootWavelet()
    wavelet.CreateBlip().GetDocument().SetText("Hello there!")
    serialized = robot_abstract.SerializeContext(context, '1')
    expected_json  = ('{"operations": {"javaClass": "java.util.ArrayList", "list": ['
        '{"blipId": "", "index": -1, "waveletId": "test.com!conv+root", "javaClass": "com.google.wave.api.impl.OperationImpl", "waveId": "test.com!wdykLROk*11", "property": {"blipId": "TBD_test.com!conv+root_1", "javaClass": "com.google.wave.api.impl.BlipData", "waveId": "test.com!wdykLROk*11", "waveletId": "test.com!conv+root"}, "type": "WAVELET_APPEND_BLIP"}, '
        '{"blipId": "TBD_test.com!conv+root_1", "index": -1, "waveletId": "test.com!conv+root", "javaClass": "com.google.wave.api.impl.OperationImpl", "waveId": "test.com!wdykLROk*11", "type": "DOCUMENT_DELETE"}, '
        '{"blipId": "TBD_test.com!conv+root_1", "index": 0, "waveletId": "test.com!conv+root", "javaClass": "com.google.wave.api.impl.OperationImpl", "waveId": "test.com!wdykLROk*11", "property": "Hello there!", "type": "DOCUMENT_INSERT"}'
        ']}, "javaClass": "com.google.wave.api.impl.OperationMessageBundle", "version": "1"}')
    self.assertEqual(expected_json, serialized)


class TestGetCapabilitiesXml(unittest.TestCase):

  def setUp(self):
    self.robot = robot_abstract.Robot('Testy', '1')

  def assertStringsEqual(self, s1, s2):
    self.assertEqual(s1, s2, 'Strings differ:\n%s--\n%s' % (s1, s2))

  def testDefault(self):
    expected = (
        '<?xml version="1.0"?>\n'
        '<w:robot xmlns:w="http://wave.google.com/extensions/robots/1.0">\n'
        '<w:version>1</w:version>\n'
        '<w:capabilities>\n</w:capabilities>\n'
        '<w:profile name="Testy"/>\n'
        '</w:robot>\n')
    xml = self.robot.GetCapabilitiesXml()
    self.assertStringsEqual(expected, xml)

  def testUrls(self):
    profile_robot = robot_abstract.Robot(
        'Testy',
        '1',
        image_url='http://example.com/image.png',
        profile_url='http://example.com/profile.xml')
    expected = (
        '<?xml version="1.0"?>\n'
        '<w:robot xmlns:w="http://wave.google.com/extensions/robots/1.0">\n'
        '<w:version>1</w:version>\n'
        '<w:capabilities>\n</w:capabilities>\n'
        '<w:profile name="Testy"'
        ' imageurl="http://example.com/image.png"'
        ' profileurl="http://example.com/profile.xml"/>\n'
        '</w:robot>\n')
    xml = profile_robot.GetCapabilitiesXml()
    self.assertStringsEqual(expected, xml)

  def testCapsAndEvents(self):
    self.robot.RegisterHandler('myevent', None)
    self.robot.RegisterCronJob('/ping', 20)
    expected = (
        '<?xml version="1.0"?>\n'
        '<w:robot xmlns:w="http://wave.google.com/extensions/robots/1.0">\n'
        '<w:version>1</w:version>\n'
        '<w:capabilities>\n'
        '  <w:capability name="myevent"/>\n'
        '</w:capabilities>\n'
        '<w:crons>\n  <w:cron path="/ping" timerinseconds="20"/>\n</w:crons>\n'
        '<w:profile name="Testy"/>\n'
        '</w:robot>\n')
    xml = self.robot.GetCapabilitiesXml()
    self.assertStringsEqual(expected, xml)


class SampleListener(object):
  """Example event listener that exposes some inconsistently-named methods."""

  OnDocumentChanged = 'Non-callable decoy attribute'

  def on_wavelet_blip_created(self, props, context):
    pass

  def OnBlipSubmitted(self, props, context):
    pass

  def OnBogusEvent(self, props, context):
    pass

  def some_other_method(self, props, context):
    pass

  def _on_document_changed(self, props, context):
    pass


class TestRegisterListener(unittest.TestCase):
  """Tests for the RegisterListener robot method."""

  def setUp(self):
    self.robot = robot_abstract.Robot('listener', '1')

  def testRegisterListener(self):
    listener = SampleListener()
    self.robot.RegisterListener(listener)
    self.assertEqual(len(self.robot._handlers), 2)
    self.assertEqual(self.robot._handlers['BLIP_SUBMITTED'],
                     [listener.OnBlipSubmitted])
    self.assertEqual(self.robot._handlers['WAVELET_BLIP_CREATED'],
                     [listener.on_wavelet_blip_created])


if __name__ == '__main__':
  unittest.main()

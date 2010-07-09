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

"""Unit tests for the model module."""


__author__ = 'davidbyttow@google.com (David Byttow)'


import unittest

import model

TEST_WAVE_DATA = {
    'waveId': 'test-wave',
    'waveletIds': ['wavelet-1'],
}

TEST_WAVELET_DATA = {
    'creator': 'creator@google.com',
    'creationTime': 100,
    'lastModifiedTime': 101,
    'participants': ['robot@google.com'],
    'rootBlipId': 'blip-1',
    'title': 'Title',
    'waveId': TEST_WAVE_DATA['waveId'],
    'waveletId': 'test.com' + model.ROOT_WAVELET_ID_SUFFIX,
}

TEST_GADGET_URL = 'http://test.com/gadget.xml'

TEST_GADGET = {
    'type': 'GADGET',
    'properties': {'url': TEST_GADGET_URL,
                   'prop': 'value'}
}

TEST_BLIP_DATA = {
    'blipId': TEST_WAVELET_DATA['rootBlipId'],
    'childBlipIds': [],
    'content': '<p>testing</p>',
    'contributors': [TEST_WAVELET_DATA['creator'], 'robot@google.com'],
    'creator': TEST_WAVELET_DATA['creator'],
    'lastModifiedTime': TEST_WAVELET_DATA['lastModifiedTime'],
    'parentBlipId': None,
    'waveId': TEST_WAVE_DATA['waveId'],
    'elements': {'15': TEST_GADGET},
    'waveletId': TEST_WAVELET_DATA['waveletId'],
}


class TestWaveModel(unittest.TestCase):
  """Tests the primary data structures for the wave model."""

  def setUp(self):
    self.test_wave_data = TEST_WAVE_DATA.copy()
    self.test_wavelet_data = TEST_WAVELET_DATA.copy()
    self.test_blip_data = TEST_BLIP_DATA.copy()

    self.wave = model.Wave(self.test_wave_data)
    self.wavelet = model.Wavelet(self.test_wavelet_data)
    self.blip = model.Blip(self.test_blip_data)

    self.test_context = model.Context()
    self.test_context.waves[self.wave.waveId] = self.wave
    self.test_context.wavelets[self.wavelet.waveletId] = self.wavelet
    self.test_context.blips[self.blip.blipId] = self.blip
  
  def verifySameAttributes(self, source, target):
    for attr_name in dir(source):
      self.assertTrue(hasattr(target, attr_name))

  def testDefaults(self):
    empty_json = {}
    self.verifySameAttributes(self.blip, model.Blip(empty_json))
    self.verifySameAttributes(self.wave, model.Wave(empty_json))
    self.verifySameAttributes(self.wavelet, model.Wavelet(empty_json))
 
  def testWaveFields(self):
    w = self.wave
    self.assertEquals(self.test_wave_data['waveId'], w.waveId)
    self.assertEquals(set(self.test_wave_data['waveletIds']), w.waveletIds)

  def testWaveMethods(self):
    w = self.wave
    self.assertEquals(self.test_wave_data['waveId'], w.GetId())
    self.assertEquals(set(self.test_wave_data['waveletIds']), w.GetWaveletIds())

  def testWaveletFields(self):
    w = self.wavelet
    self.assertEquals(self.test_wavelet_data['creator'], w.creator)
    self.assertEquals(self.test_wavelet_data['creationTime'], w.creationTime)
    self.assertEquals(self.test_wavelet_data['lastModifiedTime'],
                      w.lastModifiedTime)
    self.assertEquals(set(self.test_wavelet_data['participants']),
                      w.participants)
    self.assertEquals(self.test_wavelet_data['rootBlipId'], w.rootBlipId)
    self.assertEquals(self.test_wavelet_data['title'], w.title)
    self.assertEquals(self.test_wavelet_data['waveId'], w.waveId)
    self.assertEquals(self.test_wavelet_data['waveletId'], w.waveletId)

  def testWaveletMethods(self):
    w = self.wavelet
    self.assertEquals(self.test_wavelet_data['creator'], w.GetCreator())
    self.assertEquals(self.test_wavelet_data['creationTime'],
                      w.GetCreationTime())
    self.assertEquals(None, w.GetDataDocument('foo'))
    self.assertEquals(42, w.GetDataDocument('foo', 42))
    self.assertEquals(self.test_wavelet_data['lastModifiedTime'],
                      w.GetLastModifiedTime())
    self.assertEquals(set(self.test_wavelet_data['participants']),
                      w.GetParticipants())
    self.assertEquals(self.test_wavelet_data['rootBlipId'], w.GetRootBlipId())
    self.assertEquals(self.test_wavelet_data['title'], w.GetTitle())
    self.assertEquals(self.test_wavelet_data['waveId'], w.GetWaveId())
    self.assertEquals(self.test_wavelet_data['waveletId'], w.GetId())

  def testBlipFields(self):
    b = self.blip
    self.assertEquals(self.test_blip_data['blipId'], b.blipId)
    self.assertEquals(set(self.test_blip_data['childBlipIds']), b.childBlipIds)
    self.assertEquals(set(self.test_blip_data['contributors']), b.contributors)
    self.assertEquals(self.test_blip_data['creator'], b.creator)
    self.assertEquals(self.test_blip_data['content'], b.document.GetText())
    self.assertEquals(self.test_blip_data['lastModifiedTime'],
                      b.lastModifiedTime)
    self.assertEquals(self.test_blip_data['parentBlipId'], b.parentBlipId)
    self.assertEquals(self.test_blip_data['waveId'], b.waveId)
    self.assertEquals(self.test_blip_data['waveletId'], b.waveletId)
    self.assertEquals(self.test_blip_data['waveletId'], b.waveletId)
    self.assertTrue(b.IsRoot())

  def testBlipMethods(self):
    b = self.blip
    self.assertEquals(self.test_blip_data['blipId'], b.GetId())
    self.assertEquals(set(self.test_blip_data['childBlipIds']),
                      b.GetChildBlipIds())
    self.assertEquals(set(self.test_blip_data['contributors']),
                      b.GetContributors())
    self.assertEquals(self.test_blip_data['creator'], b.GetCreator())
    self.assertEquals(self.test_blip_data['content'], b.GetDocument().GetText())
    self.assertEquals(self.test_blip_data['lastModifiedTime'],
                      b.GetLastModifiedTime())
    self.assertEquals(self.test_blip_data['parentBlipId'], b.GetParentBlipId())
    self.assertEquals(self.test_blip_data['waveId'], b.GetWaveId())
    self.assertEquals(len(self.test_blip_data['elements']), len(b.GetElements()))
    self.assertTrue(b.IsRoot())
    self.assertEquals(b.GetGadgetByUrl(TEST_GADGET_URL).url, TEST_GADGET_URL)

  def testBlipIsNotRoot(self):
    self.test_blip_data['parentBlipId'] = 'blip-parent'
    b = model.Blip(self.test_blip_data)
    self.assertFalse(b.IsRoot())

  def testDocument(self):
    b = model.Blip(self.test_blip_data)
    doc = model.Document(b)
    self.assertEquals(b.content, doc.GetText())

  def testEvent(self):
    data = {'type': 'WAVELET_PARTICIPANTS_CHANGED',
            'properties': {'blipId': 'blip-1'},
            'timestamp': 123,
            'modifiedBy': 'modifier@google.com'}
    event_data = model.Event(data)
    self.assertEquals(data['type'], event_data.type)
    self.assertEquals(data['properties'], event_data.properties)
    self.assertEquals(data['timestamp'], event_data.timestamp)
    self.assertEquals(data['modifiedBy'], event_data.modifiedBy)

  def testContext(self):
    self.assertEquals(self.blip,
                      self.test_context.GetBlipById(self.blip.blipId))
    self.assertEquals(self.blip, self.test_context.blips[self.blip.blipId])
    self.assertEquals(self.wave,
                      self.test_context.GetWaveById(self.wave.waveId))
    self.assertEquals(self.wave, self.test_context.waves[self.wave.waveId])
    self.assertEquals(self.wavelet,
                      self.test_context.GetWaveletById(self.wavelet.waveletId))
    self.assertEquals(self.wavelet,
                      self.test_context.wavelets[self.wavelet.waveletId])
    self.assertEquals(self.wavelet, self.test_context.GetRootWavelet())
    

if __name__ == '__main__':
  unittest.main()

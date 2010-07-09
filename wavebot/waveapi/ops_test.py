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

"""Unit tests for the ops module."""


__author__ = 'davidbyttow@google.com (David Byttow)'


import unittest

import document
import model
import model_test
import ops


TEST_WAVE_DATA = model_test.TEST_WAVE_DATA

TEST_WAVELET_DATA = model_test.TEST_WAVELET_DATA

TEST_BLIP_DATA = model_test.TEST_BLIP_DATA


class TestOperation(unittest.TestCase):
  """Test case for Operation class."""

  def testDefaults(self):
    op = ops.Operation(ops.WAVELET_APPEND_BLIP, 'wave-id', 'wavelet-id')
    self.assertEquals(ops.WAVELET_APPEND_BLIP, op.type)
    self.assertEquals('wave-id', op.wave_id)
    self.assertEquals('wavelet-id', op.wavelet_id)
    self.assertEquals('', op.blip_id)
    self.assertEquals(-1, op.index)
    self.assertEquals(None, op.property)

  def testFields(self):
    op = ops.Operation(ops.DOCUMENT_INSERT, 'wave-id', 'wavelet-id',
                       blip_id='blip-id',
                       index=1,
                       prop='foo')
    self.assertEquals(ops.DOCUMENT_INSERT, op.type)
    self.assertEquals('wave-id', op.wave_id)
    self.assertEquals('wavelet-id', op.wavelet_id)
    self.assertEquals('blip-id', op.blip_id)
    self.assertEquals(1, op.index)
    self.assertEquals('foo', op.property)


class TestOpBasedClasses(unittest.TestCase):
  """Base class for op-based test classes. Sets up some test data."""

  def setUp(self):
    self.test_context = ops._ContextImpl()

    self.test_wave_data = TEST_WAVE_DATA
    self.test_wave = self.test_context.AddWave(self.test_wave_data)

    self.test_wavelet_data = TEST_WAVELET_DATA
    self.test_wavelet = self.test_context.AddWavelet(self.test_wavelet_data)

    self.test_blip_data = TEST_BLIP_DATA
    self.test_blip = self.test_context.AddBlip(self.test_blip_data)


class TestOpBasedContext(TestOpBasedClasses):
  """Test case for testing the operation-based context class, _ContextImpl."""

  def testRemove(self):
    self.test_context.RemoveWave(TEST_WAVE_DATA['waveId'])
    self.assertEquals(None,
                      self.test_context.GetWaveById(TEST_WAVE_DATA['waveId']))
    wavelet_id = TEST_WAVELET_DATA['waveletId']
    self.test_context.RemoveWavelet(wavelet_id)
    self.assertEquals(None, self.test_context.GetWaveletById(wavelet_id))
    self.test_context.RemoveBlip('blip-1')
    self.assertEquals(None, self.test_context.GetBlipById('blip-1'))


class TestOpBasedWave(TestOpBasedClasses):
  """Test case for OpBasedWave class."""

  def testCreateWavelet(self):
    wavelet = self.test_wave.CreateWavelet(participants=['bob'])
    #uncomment the next line once we do the right thing
    #self.assertEquals(wavelet.GetWaveId(), TEST_WAVE_DATA['waveId'])
    self.assertTrue('bob' in wavelet.GetParticipants())


class TestOpBasedWavelet(TestOpBasedClasses):
  """Test case for OpBasedWavelet class."""

  def testCreateBlip(self):
    blip = self.test_wavelet.CreateBlip()
    self.assertEquals(TEST_WAVE_DATA['waveId'], blip.GetWaveId())
    self.assertEquals(TEST_WAVELET_DATA['waveletId'], blip.GetWaveletId())
    self.assertTrue(blip.GetId().startswith('TBD'))
    self.assertEquals(blip, self.test_context.GetBlipById(blip.GetId()))

  def testAddParticipant(self):
    p = 'newguy@google.com'
    self.test_wavelet.AddParticipant(p)
    self.assertTrue(p in self.test_wavelet.GetParticipants())

  def testRemoveSelf(self):
    self.assertRaises(NotImplementedError,
                      self.test_wavelet.RemoveSelf)

  def testSetDataDocument(self):
    self.test_wavelet.SetDataDocument('key', 'value')
    self.assertEquals('value', self.test_wavelet.GetDataDocument('key'))

  def testSetTitle(self):
    self.test_wavelet.SetTitle('foobar')
    self.assertEquals('foobar', self.test_wavelet.GetTitle())


class TestOpBasedBlip(TestOpBasedClasses):
  """Test case for OpBasedBlip class."""

  def testCreateChild(self):
    blip = self.test_blip.CreateChild()
    self.assertEquals(TEST_WAVE_DATA['waveId'], blip.GetWaveId())
    self.assertEquals(TEST_WAVELET_DATA['waveletId'], blip.GetWaveletId())
    self.assertTrue(blip.GetId().startswith('TBD'))
    self.assertEquals(blip, self.test_context.GetBlipById(blip.GetId()))

  def testDelete(self):
    self.test_blip.Delete()
    self.assertEquals(None,
                      self.test_context.GetBlipById(self.test_blip.GetId()))


class TestOpBasedDocument(TestOpBasedClasses):
  """Test case for OpBasedDocument class."""

  def setUp(self):
    super(TestOpBasedDocument, self).setUp()
    self.test_doc = self.test_blip.GetDocument()
    self.test_doc.SetText('123456')

  def testSetText(self):
    text = 'Hello test.'
    self.assertTrue(self.test_doc.GetText() != text)
    self.test_doc.SetText(text)
    self.assertEquals(text, self.test_doc.GetText())

  def testSetTextInRange(self):
    text = 'abc'
    self.test_doc.SetTextInRange(document.Range(0, 2), text)
    self.assertEquals('abc456', self.test_doc.GetText())
    self.test_doc.SetTextInRange(document.Range(2, 2), text)
    self.assertEquals('ababc456', self.test_doc.GetText())

  def testAppendText(self):
    text = '789'
    self.test_doc.AppendText(text)
    self.assertEquals('123456789', self.test_doc.GetText())

  def testClear(self):
    self.test_doc.Clear()
    self.assertEquals('', self.test_doc.GetText())

  def testDeleteRange(self):
    self.test_doc.DeleteRange(document.Range(0, 1))
    self.assertEquals('3456', self.test_doc.GetText())
    self.test_doc.DeleteRange(document.Range(0, 0))
    self.assertEquals('456', self.test_doc.GetText())

  def testAnnotateDocument(self):
    self.test_doc.AnnotateDocument('key', 'value')
    self.assertTrue(self.test_doc.HasAnnotation('key'))
    self.assertFalse(self.test_doc.HasAnnotation('non-existent-key'))

  def testSetAnnotation(self):
    self.test_doc.SetAnnotation(document.Range(0, 1), 'key', 'value')
    self.assertTrue(self.test_doc.HasAnnotation('key'))

  def testDeleteAnnotationByName(self):
    self.test_doc.SetAnnotation(document.Range(0, 1), 'key', 'value')
    self.test_doc.SetAnnotation(document.Range(0, 1), 'key2', 'value')
    self.test_doc.SetAnnotation(document.Range(10, 11), 'key', 'value')
    self.test_doc.SetAnnotation(document.Range(00, 11), 'key2', 'value')
    self.test_doc.SetAnnotation(document.Range(20, 21), 'key', 'value')
    self.test_doc.DeleteAnnotationsByName('key')
    self.assertFalse(self.test_doc.HasAnnotation('key'))

  def testDeleteAnnotationInRange(self):
    self.test_doc.SetAnnotation(document.Range(0, 10), 'key', 'value')
    self.test_doc.DeleteAnnotationsInRange(document.Range(2, 6), 'key')
    self.assertTrue(self.test_doc.HasAnnotation('key'))
    l = [x for x in self.test_doc.RangesForAnnotation('key')]
    self.assertEqual(len(l), 2)
    self.test_doc.DeleteAnnotationsInRange(document.Range(0, 2), 'key')
    l = [x for x in self.test_doc.RangesForAnnotation('key')]
    print [str(x) for x in l]
    self.assertEqual(len(l), 1)
    self.test_doc.DeleteAnnotationsInRange(document.Range(5, 8), 'key')
    l = [x for x in self.test_doc.RangesForAnnotation('key')]
    self.assertEqual(len(l), 1)
    self.test_doc.DeleteAnnotationsInRange(document.Range(7, 12), 'key')
    self.assertFalse(self.test_doc.HasAnnotation('key'))

  def testRangesForAnnotation(self):
    self.assertEqual([x for x in self.test_doc.RangesForAnnotation('key')], [])
    self.test_doc.SetAnnotation(document.Range(1, 10), 'key', 'value')
    l = [x for x in self.test_doc.RangesForAnnotation('key')]
    self.assertTrue(l[0].start, 1)

  def testAppendInlineBlip(self):
    blip = self.test_doc.AppendInlineBlip()
    self.assertEquals(TEST_WAVE_DATA['waveId'], blip.GetWaveId())
    self.assertEquals(TEST_WAVELET_DATA['waveletId'], blip.GetWaveletId())
    self.assertTrue(blip.GetId().startswith('TBD'))
    self.assertEquals(self.test_blip.GetId(), blip.GetParentBlipId())
    self.assertEquals(blip, self.test_context.GetBlipById(blip.GetId()))

  def testDeleteInlineBlip(self):
    blip = self.test_doc.AppendInlineBlip()
    self.test_doc.DeleteInlineBlip(blip.GetId())
    self.assertEquals(None, self.test_context.GetBlipById(blip.GetId()))

  def testInsertInlineBlip(self):
    blip = self.test_doc.InsertInlineBlip(1)
    self.assertEquals(TEST_WAVE_DATA['waveId'], blip.GetWaveId())
    self.assertEquals(TEST_WAVELET_DATA['waveletId'], blip.GetWaveletId())
    self.assertTrue(blip.GetId().startswith('TBD'))
    self.assertEquals(self.test_blip.GetId(), blip.GetParentBlipId())
    self.assertEquals(blip, self.test_context.GetBlipById(blip.GetId()))

  def testGadget(self):
    gadget = document.Gadget('http://kitchensinky.appspot.com/public/embed.xml')
    self.test_doc.AppendElement(gadget)
    self.test_doc.GadgetSubmitDelta(gadget, {'foo': 'bar'})
    self.assertEquals('bar', gadget.get('foo'))


class TestOpBuilder(TestOpBasedClasses):
  """Test case for OpBuilder class."""

  def setUp(self):
    super(TestOpBuilder, self).setUp()
    self.builder = self.test_context.builder

  def testWaveletAppendBlip(self):
    blip_data = self.builder.WaveletAppendBlip('a', 'b')
    self.assertEquals(blip_data['waveId'], 'a')
    self.assertEquals(blip_data['waveletId'], 'b')
    self.assertTrue(blip_data['blipId'].startswith('TBD'))

  def testBlipCreateChild(self):
    blip_data = self.builder.BlipCreateChild('a', 'b', 'c')
    self.assertEquals(blip_data['waveId'], 'a')
    self.assertEquals(blip_data['waveletId'], 'b')
    self.assertTrue(blip_data['blipId'].startswith('TBD'))

  def testDocumentInlineBlipAppend(self):
    blip_data = self.builder.DocumentInlineBlipAppend('a', 'b', 'c')
    self.assertEquals(blip_data['waveId'], 'a')
    self.assertEquals(blip_data['waveletId'], 'b')
    self.assertTrue(blip_data['blipId'].startswith('TBD'))

  def testDocumentInlineBlipInsert(self):
    blip_data = self.builder.DocumentInlineBlipInsert('a', 'b', 'c', 0)
    self.assertEquals(blip_data['waveId'], 'a')
    self.assertEquals(blip_data['waveletId'], 'b')
    self.assertTrue(blip_data['blipId'].startswith('TBD'))


if __name__ == '__main__':
  unittest.main()

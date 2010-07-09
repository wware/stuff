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

"""Unit tests for the document module."""


__author__ = 'davidbyttow@google.com (David Byttow)'


import unittest

import document
import util


class TestRange(unittest.TestCase):
  """Tests for the document.Range class."""

  def testDefaults(self):
    r = document.Range()
    self.assertEquals(0, r.start)
    self.assertEquals(1, r.end)

  def testValidRanges(self):
    r = document.Range(1, 2)
    self.assertEquals(1, r.start)
    self.assertEquals(2, r.end)

  def testInvalidRanges(self):
    self.assertRaises(ValueError, document.Range, 1, 0)
    self.assertRaises(ValueError, document.Range, 0, -1)
    self.assertRaises(ValueError, document.Range, 3, 1)

  def testCollapsedRanges(self):
    self.assertTrue(document.Range(0, 0).IsCollapsed())
    self.assertTrue(document.Range(1, 1).IsCollapsed())


class TestAnnotation(unittest.TestCase):
  """Tests for the document.Annotation class."""

  def testDefaults(self):
    annotation = document.Annotation('key', 'value')
    self.assertEquals(document.Range().start, annotation.range.start)
    self.assertEquals(document.Range().end, annotation.range.end)

  def testFields(self):
    annotation = document.Annotation('key', 'value', document.Range(2, 3))
    self.assertEquals('key', annotation.name)
    self.assertEquals('value', annotation.value)
    self.assertEquals(2, annotation.range.start)
    self.assertEquals(3, annotation.range.end)


class TestElement(unittest.TestCase):
  """Tests for the document.Element class."""

  def testProperties(self):
    element = document.Element(document.ELEMENT_TYPE.GADGET,
                               key='value')
    self.assertEquals('value', element.key)

  def testFormElement(self):
    element = document.FormElement(document.ELEMENT_TYPE.INPUT, 'input', label='label')
    self.assertEquals(document.ELEMENT_TYPE.INPUT, element.type)
    self.assertEquals(element.value, '')
    self.assertEquals(element.name, 'input')
    self.assertEquals(element.label, 'label')

  def testImage(self):
    image = document.Image('http://test.com/image.png', width=100, height=100)
    self.assertEquals(document.ELEMENT_TYPE.IMAGE, image.type)
    self.assertEquals(image.url, 'http://test.com/image.png')
    self.assertEquals(image.width, 100)
    self.assertEquals(image.height, 100)

  def testGadget(self):
    gadget = document.Gadget('http://test.com/gadget.xml')
    self.assertEquals(document.ELEMENT_TYPE.GADGET, gadget.type)
    self.assertEquals(gadget.url, 'http://test.com/gadget.xml')
    gadget.SubmitDelta({'foo': 'bar'})
    self.assertEquals('joop', gadget.get('bar', 'joop'))
    self.assertEquals('bar', gadget.get('foo', 'joop'))

  def testSerialize(self):
    image = document.Image('http://test.com/image.png', width=100, height=100)
    s = util.Serialize(image)
    k = s.keys()
    k.sort()
    # we should really only have three things to serialize
    self.assertEquals(['java_class', 'properties', 'type'], k)
    self.assertEquals(s['properties']['javaClass'], 'java.util.HashMap')
    props = s['properties']['map']
    self.assertEquals(len(props), 3)
    self.assertEquals(props['url'], 'http://test.com/image.png')
    self.assertEquals(props['width'], 100)
    self.assertEquals(props['height'], 100)

  def testGadgetElementFromJson(self):
    url = 'http://www.foo.com/gadget.xml'
    json = {
      'type': document.ELEMENT_TYPE.GADGET,
      'properties': {
        'url': url,
      }
    }
    gadget = document.ElementFromJson(json)
    self.assertEquals(document.ELEMENT_TYPE.GADGET, gadget.type)
    self.assertEquals(url, gadget.url)

  def testImageElementFromJson(self):
    url = 'http://www.foo.com/image.png'
    width = '32'
    height = '32'
    attachment_id = '2'
    caption = 'Test Image'
    json = {
      'type': document.ELEMENT_TYPE.IMAGE,
      'properties': {
        'url': url,
        'width': width,
        'height': height,
        'attachmentId': attachment_id,
        'caption': caption,
      }
    }
    image = document.ElementFromJson(json)
    self.assertEquals(document.ELEMENT_TYPE.IMAGE, image.type)
    self.assertEquals(url, image.url)
    self.assertEquals(width, image.width)
    self.assertEquals(height, image.height)
    self.assertEquals(attachment_id, image.attachment_id)
    self.assertEquals(caption, image.caption)

  def testFormElementFromJson(self):
    name = 'button'
    value = 'value'
    default_value = 'foo'
    label = 'test'
    json = {
      'type': document.ELEMENT_TYPE.LABEL,
      'properties': {
        'name': name,
        'value': value,
        'defaultValue': default_value,
        'label': label,
      }
    }
    element = document.ElementFromJson(json)
    self.assertEquals(document.ELEMENT_TYPE.LABEL, element.type)
    self.assertEquals(name, element.name)
    self.assertEquals(value, element.value)
    self.assertEquals(default_value, element.default_value)
    self.assertEquals(label, element.label)


if __name__ == '__main__':
  unittest.main()

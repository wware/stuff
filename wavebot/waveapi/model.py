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

"""Defines classes that represent parts of the common wave model.

Defines the core data structures for the common wave model. At this level,
models are read-only but can be modified through operations.

Note that model attributes break the typical style by providing lower
camel-cased characters to match the wire protocol format.
"""

__author__ = 'davidbyttow@google.com (David Byttow)'

import logging
import document


ROOT_WAVELET_ID_SUFFIX = '!conv+root'

class Wave(object):
  """Models a single wave instance.

  A single wave is composed of its id and any wavelet ids that belong to it.

  Attributes:
    waveId: This wave's id.
    waveletIds: Set of wavelet id's on this wave.
  """

  def __init__(self, json):
    """Inits this wave with JSON data.

    Args:
      json: JSON data dictionary from Wave server.

    Attributes:
      raw_data: Dictionary of incoming raw JSON data.
      waveId: String id of this wave.
      waveletId: String id of this wavelet.
    """
    self.waveId = json.get('waveId')
    self.waveletIds = set(json.get('waveletIds', []))
    self.raw_data = json

  def GetId(self):
    """Returns this wave's id."""
    return self.waveId

  def GetWaveletIds(self):
    """Returns a set of wavelet ids."""
    return self.waveletIds


class Wavelet(object):
  """Models a single wavelet instance.

  A single wavelet is composed of metadata, participants and the blips it
  contains.

  Attributes:
    creator: Participant id string of the creator of this wavelet.
    creationTime: Time this wavelet was created on the server.
    dataDocuments: Dictionary of data documents.
    lastModifiedTime: Time this wavelet was last modified.
    participants: Set of participant ids on this wavelet.
    raw_data: Dictionary of incoming raw JSON data.
    rootBlipId: String id of the root blip.
    waveId: String id of the parent wave.
    waveletId: This wavelet's string id.
  """

  def __init__(self, json):
    """Inits this wavelet with JSON data.

    Args:
      json: JSON data dictionary from Wave server.
    """
    self.creator = json.get('creator')
    self.creationTime = json.get('creationTime', 0)
    self.dataDocuments = json.get('dataDocuments', {})
    self.lastModifiedTime = json.get('lastModifiedTime')
    self.participants = set(json.get('participants', []))
    self.rootBlipId = json.get('rootBlipId')
    self.title = json.get('title', '')
    self.waveId = json.get('waveId')
    self.waveletId = json.get('waveletId')
    self.raw_data = json

  def GetCreator(self):
    """Returns the participant id of the creator of this wavelet."""
    return self.creator

  def GetCreationTime(self):
    """Returns the time that this wavelet was first created in milliseconds."""
    return self.creationTime

  def GetDataDocument(self, name, default=None):
    """Returns a data document for this wavelet based on key name."""
    if self.dataDocuments:
      return self.dataDocuments.get(name, default)
    return default

  def GetId(self):
    """Returns this wavelet's id."""
    return self.waveletId

  def GetLastModifiedTime(self):
    """Returns the time that this wavelet was last modified in ms."""
    return self.lastModifiedTime

  def GetParticipants(self):
    """Returns a set of participants on this wavelet."""
    return self.participants

  def GetRootBlipId(self):
    """Returns this wavelet's root blip id."""
    return self.rootBlipId

  def GetTitle(self):
    """Returns the title of this wavelet."""
    return self.title

  def GetWaveId(self):
    """Returns this wavelet's parent wave id."""
    return self.waveId


class Blip(object):
  """Models a single blip instance.

  Blips are essentially elements of conversation. Blips can live in a
  hierarchy of blips. A root blip has no parent blip id, but all blips
  have the ids of the wave and wavelet that they are associated with.

  Blips also contain annotations, content and elements, which are accessed via
  the Document object.

  Attributes:
    annotations: List of Annotation objects on this blip.
    blipId: String id of this blip.
    childBlipIds: Set of child blip ids.
    content: Raw text content contained by this blip.
    contributors: Set of contributor ids that have contributed to this blip.
    creator: Participant string id of the creator.
    raw_data: Dictionary of incoming raw JSON data.
    document: Document object for this blip.
    lastModifiedTime: Time that this blip was last modified on the server.
    parentBlipId: String id of the parent blip or None if this is the root.
    waveId: String id of the wave that this blip belongs to.
    waveletId: String id of the wavelet that this belongs to.
  """

  def __init__(self, json):
    """Inits this blip with JSON data.

    Args:
      json: JSON data dictionary from Wave server.
    """
    self.blipId = json.get('blipId')
    self.childBlipIds = set(json.get('childBlipIds', []))
    self.content = json.get('content', '')
    self.contributors = set(json.get('contributors', []))
    self.creator = json.get('creator')
    self.lastModifiedTime = json.get('lastModifiedTime', 0)
    self.parentBlipId = json.get('parentBlipId')
    self.waveId = json.get('waveId')
    self.waveletId = json.get('waveletId')
    self.annotations = []
    for annotation in json.get('annotations', []):
      r = document.Range(annotation['range']['start'],
                         annotation['range']['end'])
      self.annotations.append(document.Annotation(
          annotation['name'], annotation['value'], r=r))
    self.document = Document(self)
    self.elements = {}
    json_elements = json.get('elements', {})
    for elem in json_elements:
      self.elements[elem] = document.ElementFromJson(json_elements[elem])
    self.raw_data = json

  def GetChildBlipIds(self):
    """Returns a set of blip ids that are children of this blip."""
    return self.childBlipIds

  def GetContributors(self):
    """Returns a set of participant ids that contributed to this blip."""
    return self.contributors

  def GetCreator(self):
    """Returns the id of the participant that created this blip."""
    return self.creator

  def GetDocument(self):
    """Returns the Document of this blip, which contains content data."""
    return self.document

  def GetId(self):
    """Returns the id of this blip."""
    return self.blipId

  def GetLastModifiedTime(self):
    """Returns the time that this blip was last modified by the server."""
    return self.lastModifiedTime

  def GetParentBlipId(self):
    """Returns the id of this blips parent or None if it is the root."""
    return self.parentBlipId

  def GetWaveId(self):
    """Returns the id of the wave that this blip belongs to."""
    return self.waveId

  def GetWaveletId(self):
    """Returns the id of the wavelet that this blip belongs to."""
    return self.waveletId

  def IsRoot(self):
    """Returns True if this is the root blip of a wavelet."""
    return self.parentBlipId is None

  def GetAnnotations(self):
    """Returns the annotations for this document."""
    return self.annotations

  def GetElements(self):
    """Returns the elements for this document."""
    return self.elements

  def GetGadgetByUrl(self, url):
    """Return the (first) gadget that has the specified url.

    If no matching gadget can be found, return None. If url
    is None, return the first gadget that can be found.
    """
    for el in self.elements.values():
      if (el.type == document.ELEMENT_TYPE.GADGET
          and getattr(el, 'url', None) == url):
        return el
    return None

class Document(object):
  """Base representation of a document of a blip."""

  def __init__(self, blip):
    """Inits this document with the data of the blip it is representing.

    Args:
      blip: Blip instance that owns this document.
    """
    self._blip = blip

  def GetText(self):
    """Returns the raw text content of this document."""
    return self._blip.content


class Event(object):
  """Data describing a single event.

  Attributes:
    modifiedBy: Participant id that caused this event.
    properties: Dictionary of properties specific to this event type.
    raw_data: Dictionary of incoming raw JSON data.
    timestamp: Timestamp that this event occurred on the server.
    type: Type string of this event.
  """

  def __init__(self, json):
    """Inits this event with JSON data.

    Args:
      json: JSON data from Wave server.
    """
    self.modifiedBy = json.get('modifiedBy')
    self.properties = json.get('properties', {})
    self.timestamp = json.get('timestamp', 0)
    self.type = json.get('type')
    self.raw_data = json


class Context(object):
  """Contains information associated with a single request from the server.

  This includes the current waves in this session
  and any operations that have been enqueued during request processing.

  Attributes:
    blips: Dictionary of Blips keyed by blipId.
    wavelets: Dictionary of Wavelets keyed by waveletId.
    waves: Dictionary of Waves keyed by waveId.
  """

  def __init__(self):
    self.blips = {}
    self.wavelets = {}
    self.waves = {}
    self._operations = []

  def GetBlipById(self, blip_id):
    """Returns a blip by id or None if it does not exist."""
    return self.blips.get(blip_id, None)

  def GetWaveletById(self, wavelet_id):
    """Returns a wavelet by id or None if it does not exist."""
    return self.wavelets.get(wavelet_id, None)

  def GetWaveById(self, wave_id):
    """Returns a wave by id or None if it does not exist."""
    return self.waves.get(wave_id, None)

  def GetRootWavelet(self):
    """Returns the root wavelet or None if it is not in this context."""
    for wavelet_id, wavelet in self.wavelets.items():
      if wavelet_id.endswith(ROOT_WAVELET_ID_SUFFIX):
        return wavelet
    logging.warning('Could not retrieve root wavelet.')
    return None

  def GetWaves(self):
    """Returns the list of waves associated with this session."""
    return self.waves.values()

  def GetWavelets(self):
    """Returns the list of wavelets associated with this session."""
    return self.wavelets.values()

  def GetBlips(self):
    """Returns the list of blips associated with this session."""
    return self.blips.values()

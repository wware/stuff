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

"""Support for operations that can be applied to the server.

Contains classes and utilities for creating operations that are to be
applied on the server.
"""

__author__ = 'davidbyttow@google.com (David Byttow)'


import document
import logging
import model
import util


# Operation Types
WAVELET_APPEND_BLIP = 'WAVELET_APPEND_BLIP'
WAVELET_ADD_PARTICIPANT = 'WAVELET_ADD_PARTICIPANT'
WAVELET_CREATE = 'WAVELET_CREATE'
WAVELET_REMOVE_SELF = 'WAVELET_REMOVE_SELF'
WAVELET_DATADOC_SET = 'WAVELET_DATADOC_SET'
WAVELET_SET_TITLE = 'WAVELET_SET_TITLE'
BLIP_CREATE_CHILD = 'BLIP_CREATE_CHILD'
BLIP_DELETE = 'BLIP_DELETE'
DOCUMENT_ANNOTATION_DELETE = 'DOCUMENT_ANNOTATION_DELETE'
DOCUMENT_ANNOTATION_SET = 'DOCUMENT_ANNOTATION_SET'
DOCUMENT_ANNOTATION_SET_NORANGE = 'DOCUMENT_ANNOTATION_SET_NORANGE'
DOCUMENT_APPEND = 'DOCUMENT_APPEND'
DOCUMENT_APPEND_MARKUP = 'DOCUMENT_APPEND_MARKUP'
DOCUMENT_APPEND_STYLED_TEXT = 'DOCUMENT_APPEND_STYLED_TEXT'
DOCUMENT_INSERT = 'DOCUMENT_INSERT'
DOCUMENT_DELETE = 'DOCUMENT_DELETE'
DOCUMENT_REPLACE = 'DOCUMENT_REPLACE'
DOCUMENT_ELEMENT_APPEND = 'DOCUMENT_ELEMENT_APPEND'
DOCUMENT_ELEMENT_DELETE = 'DOCUMENT_ELEMENT_DELETE'
DOCUMENT_ELEMENT_MODIFY_ATTRS = 'DOCUMENT_ELEMENT_MODIFY_ATTRS'
DOCUMENT_ELEMENT_INSERT = 'DOCUMENT_ELEMENT_INSERT'
DOCUMENT_ELEMENT_INSERT_AFTER = 'DOCUMENT_ELEMENT_INSERT_AFTER'
DOCUMENT_ELEMENT_INSERT_BEFORE = 'DOCUMENT_ELEMENT_INSERT_BEFORE'
DOCUMENT_ELEMENT_REPLACE = 'DOCUMENT_ELEMENT_REPLACE'
DOCUMENT_INLINE_BLIP_APPEND = 'DOCUMENT_INLINE_BLIP_APPEND'
DOCUMENT_INLINE_BLIP_DELETE = 'DOCUMENT_INLINE_BLIP_DELETE'
DOCUMENT_INLINE_BLIP_INSERT = 'DOCUMENT_INLINE_BLIP_INSERT'
DOCUMENT_INLINE_BLIP_INSERT_AFTER_ELEMENT = ('DOCUMENT_INLINE_BLIP_INSERT_'
                                             'AFTER_ELEMENT')


class Operation(object):
  """Represents a generic operation applied on the server.

  This operation class contains data that is filled in depending on the
  operation type.

  It can be used directly, but doing so will not result
  in local, transient reflection of state on the blips. In other words,
  creating a "delete blip" operation will not remove the blip from the local
  context for the duration of this session. It is better to use the OpBased
  model classes directly instead.
  """

  java_class = 'com.google.wave.api.impl.OperationImpl'

  def __init__(self, op_type, wave_id, wavelet_id, blip_id='', index=-1,
               prop=None):
    """Initializes this operation with contextual data.

    Args:
      op_type: Type of operation.
      wave_id: The id of the wave that this operation is to be applied.
      wavelet_id: The id of the wavelet that this operation is to be applied.
      blip_id: The optional id of the blip that this operation is to be applied.
      index: Optional integer index for content-based operations.
      prop: A weakly typed property object is based on the context of this
          operation.
    """
    self.type = op_type
    self.wave_id = wave_id
    self.wavelet_id = wavelet_id
    self.blip_id = blip_id
    self.index = index
    self.property = prop


class OpBasedWave(model.Wave):
  """Subclass of the wave model capable of generating operations.

  Any mutation-based methods will likely result in one or more operations
  being applied locally and sent to the server.
  """

  def __init__(self, json, context):
    """Initializes this wave with the session context."""
    super(OpBasedWave, self).__init__(json)
    self.__context = context

  def CreateWavelet(self, participants=None):
    """Creates a new wavelet on this wave."""
    return self.__context.builder.WaveletCreate(
        self.GetId(), '', participants)


class OpBasedWavelet(model.Wavelet):
  """Subclass of the wavelet model capable of generating operations.

  Any mutation-based methods will likely result in one or more operations
  being applied locally and sent to the server.
  """

  def __init__(self, json, context):
    """Initializes this wavelet with the session context."""
    super(OpBasedWavelet, self).__init__(json)
    self.__context = context

  def CreateBlip(self):
    """Creates and appends a blip to this wavelet and returns it.

    Returns:
      A transient version of the blip that was created.
    """
    blip_data = self.__context.builder.WaveletAppendBlip(self.GetWaveId(),
                                                         self.GetId())
    return self.__context.AddBlip(blip_data)

  def AddParticipant(self, participant_id):
    """Adds a participant to a wavelet.

    Args:
      participant_id: Id of the participant that is to be added.
    """
    self.__context.builder.WaveletAddParticipant(self.GetWaveId(), self.GetId(),
                                                 participant_id)
    self.participants.add(participant_id)

  def RemoveSelf(self):
    """Removes this robot from the wavelet."""
    self.__context.builder.WaveletRemoveSelf(self.GetWaveId(), self.GetId())
    # TODO(davidbyttow): Locally remove the robot.

  def SetDataDocument(self, name, data):
    """Sets a key/value pair on the wavelet data document.

    Args:
      name: The string key.
      data: The value associated with this key.
    """
    self.__context.builder.WaveletSetDataDoc(self.GetWaveId(), self.GetId(),
                                             name, data)
    self.dataDocuments[name] = data

  def SetTitle(self, title):
    """Sets the title of this wavelet.

    Args:
      title: String title to for this wave.
    """
    self.__context.builder.WaveletSetTitle(self.GetWaveId(), self.GetId(),
                                           title)
    self.title = title


class OpBasedBlip(model.Blip):
  """Subclass of the blip model capable of generating operations.

  Any mutation-based methods will likely result in one or more operations
  being applied locally and sent to the server.
  """

  def __init__(self, json, context):
    """Initializes this blip with the session context."""
    super(OpBasedBlip, self).__init__(json)
    self.__context = context
    self.document = OpBasedDocument(self, context)

  def CreateChild(self):
    """Creates a child blip of this blip."""
    blip_data = self.__context.builder.BlipCreateChild(self.GetWaveId(),
                                                       self.GetWaveletId(),
                                                       self.GetId())
    return self.__context.AddBlip(blip_data)

  def Delete(self):
    """Deletes this blip from the wavelet."""
    self.__context.builder.BlipDelete(self.GetWaveId(),
                                      self.GetWaveletId(),
                                      self.GetId())
    return self.__context.RemoveBlip(self.GetId())


class OpBasedDocument(model.Document):
  """Subclass of the document model capable of generating operations.

  Any mutation-based methods will likely result in one or more operations
  being applied locally and sent to the server.

  TODO(davidbyttow): Manage annotations and elements as content is updated.
  """

  def __init__(self, blip, context):
    """Initializes this document with its owning blip and session context."""
    super(OpBasedDocument, self).__init__(blip)
    self.__context = context

  def HasAnnotation(self, name):
    """Determines if given named annotation is anywhere on this document.

    Args:
      name: The key name of the annotation.

    Returns:
      True if the annotation exists.
    """
    for annotation in self._blip.annotations:
      if annotation.name == name:
        return True
    return False

  def RangesForAnnotation(self, name):
    """Iterate through the ranges defined for name.

    Args:
      name: The name of the annotation.

    Returns:
      the matching ranges.
    """
    for annotation in self._blip.annotations:
      if annotation.name == name:
        yield annotation.range

  def SetText(self, text):
    """Clears and sets the text of this document.

    Args:
      text: The text content to replace this document with.
    """
    self.Clear()
    self.__context.builder.DocumentInsert(self._blip.waveId,
                                          self._blip.waveletId,
                                          self._blip.blipId,
                                          text)
    self._blip.content = text

  def SetTextInRange(self, r, text):
    """Deletes text within a range and sets the supplied text in its place.

    Args:
      r: Range to delete and where to set the new text.
      text: The text to set at the range start position.
    """
    self.DeleteRange(r)
    self.InsertText(r.start, text)

  def InsertText(self, start, text):
    """Inserts text at a specific position.

    Args:
      start: The index position where to set the text.
      text: The text to set.
    """
    self.__context.builder.DocumentInsert(self._blip.waveId,
                                          self._blip.waveletId,
                                          self._blip.blipId,
                                          text, index=start)
    left = self._blip.content[:start]
    right = self._blip.content[start:]
    self._blip.content = left + text + right

  def AppendText(self, text):
    """Appends text to the end of this document.

    Args:
      text: The text to append.
    """
    self.__context.builder.DocumentAppend(self._blip.waveId,
                                          self._blip.waveletId,
                                          self._blip.blipId,
                                          text)
    self._blip.content += text

  def Clear(self):
    """Clears the content of this document."""
    self.__context.builder.DocumentDelete(self._blip.waveId,
                                          self._blip.waveletId,
                                          self._blip.blipId)
    self._blip.content = ''

  def DeleteRange(self, r):
    """Deletes the content in the specified range.

    Args:
      r: A Range instance specifying the range to delete.
    """
    self.__context.builder.DocumentDelete(self._blip.waveId,
                                          self._blip.waveletId,
                                          self._blip.blipId,
                                          r.start, r.end)
    left = self._blip.content[:r.start]
    right = self._blip.content[r.end + 1:]
    self._blip.content = left + right

  def AnnotateDocument(self, name, value):
    """Annotates the entire document.

    Args:
      name: A string as the key for this annotation.
      value: The value of this annotation.
    """
    b = self.__context.builder
    b.DocumentAnnotationSetNoRange(self._blip.waveId,
                                   self._blip.waveletId,
                                   self._blip.blipId,
                                   name, value)
    r = document.Range(0, len(self._blip.content))
    self._blip.annotations.append(document.Annotation(name, value, r))

  def SetAnnotation(self, r, name, value):
    """Sets an annotation on a given range.

    Args:
      r: A Range specifying the range to set the annotation.
      name: A string as the key for this annotation.
      value: The value of this annotaton.
    """
    self.__context.builder.DocumentAnnotationSet(self._blip.waveId,
                                                 self._blip.waveletId,
                                                 self._blip.blipId,
                                                 r.start, r.end,
                                                 name, value)
    self._blip.annotations.append(document.Annotation(name, value, r))

  def DeleteAnnotationsByName(self, name):
    """Deletes all annotations with a given key name.

    Args:
      name: A string as the key for the annotation to delete.
    """
    size = len(self._blip.content)
    self.__context.builder.DocumentAnnotationDelete(self._blip.waveId,
                                                    self._blip.waveletId,
                                                    self._blip.blipId,
                                                    0, size, name)
    self._blip.annotations = [a
        for a in self._blip.annotations if a.name != name]

  def DeleteAnnotationsInRange(self, r, name):
    """Clears all of the annotations within a given range with a given key.

    Args:
      r: A Range specifying the range to delete.
      name: Annotation key type to clear.
    """
    self.__context.builder.DocumentAnnotationDelete(self._blip.waveId,
                                                    self._blip.waveletId,
                                                    self._blip.blipId,
                                                    r.start, r.end,
                                                    name)
    res = []
    for a in self._blip.annotations:
      if a.name != name or r.start > a.range.end or r.end < a.range.start:
        res.append(a)
      elif r.start < a.range.start and r.end > a.range.end:
        continue
      else:
        if a.range.start < r.start:
          res.append(document.Annotation(
              name, a.value, document.Range(a.range.start, r.start)))
        if a.range.end > r.end:
          a.range.start = r.end
          res.append(a)
    self._blip.annotations = res


  def AppendInlineBlip(self):
    """Appends an inline blip to this blip.

    Returns:
      The local blip that was appended.
    """
    blip_data = self.__context.builder.DocumentInlineBlipAppend(
        self._blip.waveId, self._blip.waveletId,
        self._blip.blipId)
    return self.__context.AddBlip(blip_data)

  def DeleteInlineBlip(self, inline_blipId):
    """Deletes an inline blip from this blip.

    Args:
      inline_blipId: The id of the blip to remove.
    """
    self.__context.builder.DocumentInlineBlipDelete(self._blip.waveId,
                                                    self._blip.waveletId,
                                                    self._blip.blipId,
                                                    inline_blipId)
    self.__context.RemoveBlip(inline_blipId)

  def InsertInlineBlip(self, position):
    """Inserts an inline blip into this blip at a specific position.

    Args:
      position: Position to insert the blip at.

    Returns:
      The JSON data of the blip that was created.
    """
    blip_data = self.__context.builder.DocumentInlineBlipInsert(
        self._blip.waveId,
        self._blip.waveletId,
        self._blip.blipId,
        position)
    # TODO(davidbyttow): Add local blip element.
    return self.__context.AddBlip(blip_data)

  def DeleteElement(self, position):
    """Deletes an Element at a given position.

    Args:
      position: Position of the Element to delete.
    """
    self.__context.builder.DocumentElementDelete(self._blip.waveId,
                                                 self._blip.waveletId,
                                                 self._blip.blipId,
                                                 position)

  def InsertElement(self, position, element):
    """Inserts an Element at a given position.

    Args:
      position: Position of the element to replace.
      element: The Element to replace with.
    """
    self.__context.builder.DocumentElementInsert(self._blip.waveId,
                                                 self._blip.waveletId,
                                                 self._blip.blipId,
                                                 position, element)

  def ReplaceElement(self, position, element):
    """Replaces an Element at a given position with a new element.

    Args:
      position: Position of the element to replace.
      element: The Element to replace with.
    """
    self.__context.builder.DocumentElementReplace(self._blip.waveId,
                                                  self._blip.waveletId,
                                                  self._blip.blipId,
                                                  position, element)

  def AppendElement(self, element):
    self.__context.builder.DocumentElementAppend(self._blip.waveId,
                                                 self._blip.waveletId,
                                                 self._blip.blipId,
                                                 element)

  def GadgetSubmitDelta(self, gadget, delta):
    """Submit a delta for the specified gadget.

    Currently submit delta is keyed on the url of the gadget and
    won't work if two gadgets with the same url are present in the
    document.

    Args:
      gadget: the gadget to submit the delta too
      delta: a dictionary with the key/values that need to be updated.
    """
    dummy = document.Gadget(url=gadget.url, props=delta)
    self.__context.builder.DocumentModifyAttributes(self._blip.waveId,
                                                    self._blip.waveletId,
                                                    self._blip.blipId,
                                                    dummy)
    gadget.SubmitDelta(delta)


class _ContextImpl(model.Context):
  """An internal implementation of the Context class.

  This implementation of the context is capable of adding waves, wavelets
  and blips to itself. This is useful when applying operations locally
  in a single session. Through this, clients can access waves, wavelets and
  blips and add operations to be applied to those objects by the server.

  Operations are applied in the order that they are received. Adding
  operations manually will not be reflected in the state of the context.
  """

  def __init__(self):
    super(_ContextImpl, self).__init__()
    self.builder = OpBuilder(self)

  def AddOperation(self, op):
    """Adds an operation to the list of operations to applied by the server.

    After all events are handled, the operation list is sent back to the server
    and applied in order. Adding an operation this way will have no effect
    on the state of the context or its entities.

    Args:
      op: An instance of an Operation.
    """
    self._operations.append(op)

  def AddWave(self, wave_data):
    """Adds a transient wave based on the data supplied.

    Args:
      wave_data: JSON data describing this wave.

    Returns:
      An OpBasedWave that may have operations applied to it.
    """
    wave = OpBasedWave(wave_data, self)
    self.waves[wave.GetId()] = wave
    return wave

  def AddWavelet(self, wavelet_data):
    """Adds a transient wavelet based on the data supplied.

    Args:
      wavelet_data: JSON data describing this wavelet.

    Returns:
      An OpBasedWavelet that may have operations applied to it.
    """
    wavelet = OpBasedWavelet(wavelet_data, self)
    self.wavelets[wavelet.GetId()] = wavelet
    return wavelet

  def AddBlip(self, blip_data):
    """Adds a transient blip based on the data supplied.

    Args:
      blip_data: JSON data describing this blip.

    Returns:
      An OpBasedBlip that may have operations applied to it.
    """
    blip = OpBasedBlip(blip_data, self)
    self.blips[blip.GetId()] = blip
    return blip

  def RemoveWave(self, wave_id):
    """Removes a wave locally."""
    if wave_id in self.waves:
      del self.waves[wave_id]

  def RemoveWavelet(self, wavelet_id):
    """Removes a wavelet locally."""
    if wavelet_id in self.wavelets:
      del self.wavelets[wavelet_id]

  def RemoveBlip(self, blip_id):
    """Removes a blip locally."""
    if blip_id in self.blips:
      del self.blips[blip_id]

  def Serialize(self):
    """Serialize the operation bundle.

    Returns:
      Dict representing this object.
    """
    data = {
        'javaClass': 'com.google.wave.api.impl.OperationMessageBundle',
        'operations': util.Serialize(self._operations)
    }
    return data


def CreateContext(data):
  """Creates a Context instance from raw data supplied by the server.

  Args:
    data: Raw data decoded from JSON sent by the server.

  Returns:
    A Context instance for this session.
  """
  context = _ContextImpl()
  for raw_blip_data in data['blips'].values():
    context.AddBlip(raw_blip_data)

  # Currently only one wavelet is sent.
  context.AddWavelet(data['wavelet'])

  # Waves are not sent over the wire, but we can build the list based on the
  # wave ids of the wavelets.
  wave_wavelet_map = {}
  wavelets = context.GetWavelets()
  for wavelet in wavelets:
    wave_id = wavelet.GetWaveId()
    wavelet_id = wavelet.GetId()
    if wave_id not in wave_wavelet_map:
      wave_wavelet_map[wave_id] = []
    wave_wavelet_map[wave_id].append(wavelet_id)

  for wave_id, wavelet_ids in wave_wavelet_map.iteritems():
    wave_data = {
        'waveId': wave_id,
        'waveletIds': wavelet_ids,
    }
    context.AddWave(wave_data)

  return context


class BlipData(dict):
  """Temporary class for storing ephemeral blip data.

  This should be removed once the Java API no longer requires javaClass
  objects, at which point, this method should just return a dict.
  """
  java_class = 'com.google.wave.api.impl.BlipData'

  def __init__(self, wave_id, wavelet_id, blip_id):
    super(BlipData, self).__init__()
    self.waveId = wave_id
    self.waveletId = wavelet_id
    self.blipId = blip_id
    self['waveId'] = wave_id
    self['waveletId'] = wavelet_id
    self['blipId'] = blip_id


class WaveletData(dict):
  """Temporary class for storing ephemeral blip data.

  This should be removed once the Java API no longer requires javaClass
  objects, at which point, this method should just return a dict.
  """
  java_class = 'com.google.wave.api.impl.WaveletData'

  def __init__(self, wave_id, wavelet_id, participants):
    super(WaveletData, self).__init__()
    self.waveId = wave_id
    self.waveletId = wavelet_id
    self.participants = participants
    self['waveId'] = wave_id
    self['waveletId'] = wavelet_id
    self['participants'] = participants

  def SetRootBlipId(self, blip_id):
    self['rootBlipId'] = blip_id
    self.rootBlipId = blip_id


class OpBuilder(object):
  """Wraps all currently supportable operations as functions.

  The operation builder wraps single operations as functions and generates
  operations in-order on its context. This should only be used when the context
  is not available on a specific entity. For example, to modify a blip that
  does not exist in the current context, you might specify the wave, wavelet
  and blip id to generate an operation.

  Any calls to this will not reflect the local context state in any way.
  For example, calling WaveletAppendBlip will not result in a new blip
  being added to the local context, only an operation to be applied on the
  server.
  """

  def __init__(self, context):
    """Initializes the op builder with the context.

    Args:
      context: A Context instance to generate operations on.
    """
    self.__context = context
    self.__nextBlipId = 1
    self.__nextWaveId = 1

  def __CreateNewBlipData(self, wave_id, wavelet_id):
    """Creates JSON of the blip used for this session."""
    temp_blip_id = 'TBD_' + wavelet_id + '_' + str(self.__nextBlipId)
    self.__nextBlipId += 1
    return BlipData(wave_id, wavelet_id, temp_blip_id)

  def __CreateNewWaveletData(self, participants):
    """Creates an ephemeral BlipData instance used for this session."""
    wave_id = 'TBD_' + str(self.__nextWaveId)
    self.__nextWaveId += 1
    wavelet_id = "conv+root"
    participants = set(participants)
    return WaveletData(wave_id, wavelet_id, participants)

  def AddNewOperation(self, op_type, wave_id, wavelet_id, blip_id='', index=-1,
                      prop=None):
    """Creates and adds a new operation to the operation list."""
    self.__context.AddOperation(
        Operation(op_type, wave_id, wavelet_id,
                  blip_id=blip_id,
                  index=index,
                  prop=prop))

  def WaveletAppendBlip(self, wave_id, wavelet_id):
    """Requests to append a blip to a wavelet.

    Args:
      wave_id: The wave id owning the containing wavelet.
      wavelet_id: The wavelet id that this blip should be appended to.

    Returns:
      JSON representing the id information of the new blip.
    """
    blip_data = self.__CreateNewBlipData(wave_id, wavelet_id)
    self.AddNewOperation(WAVELET_APPEND_BLIP, wave_id, wavelet_id,
                         prop=blip_data)
    return blip_data

  def WaveletAddParticipant(self, wave_id, wavelet_id, participant_id):
    """Requests to add a participant to a wavelet.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      participant_id: Id of the participant to add.
    """
    self.AddNewOperation(WAVELET_ADD_PARTICIPANT, wave_id, wavelet_id,
                         prop=participant_id)

  def WaveletCreate(self, wave_id, wavelet_id, participants=None):
    """Requests to create a wavelet in a wave.

    Not yet implemented.

    Args:
      participants: initial participants on this wavelet or None if none

    """
    if participants is None:
      participants = []
    wavelet_data = self.__CreateNewWaveletData(participants)
    blip_data = self.__CreateNewBlipData(
        wavelet_data.waveId, wavelet_data.waveletId)
    self.__context.AddBlip(blip_data)
    wavelet_data.SetRootBlipId(blip_data.blipId)
    logging.info('rootblip=' + blip_data.blipId)
    wavelet = self.__context.AddWavelet(wavelet_data)
    op = Operation(WAVELET_CREATE, wave_id, wavelet_id, prop=wavelet_data)
    self.__context.AddOperation(op)
    return wavelet

  def WaveletRemoveSelf(self, wave_id, wavelet_id):
    """Requests to remove this robot from a wavelet.

    Not yet implemented.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.

    Raises:
      NotImplementedError: Function not yet implemented.
    """
    raise NotImplementedError()

  def WaveletSetDataDoc(self, wave_id, wavelet_id, name, data):
    """Requests set a key/value pair on the data document of a wavelet.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      name: The key name for this data.
      data: The value of the data to set.
    """
    self.AddNewOperation(WAVELET_DATADOC_SET, wave_id, wavelet_id,
                         blip_id=name, prop=data)

  def WaveletSetTitle(self, wave_id, wavelet_id, title):
    """Requests to set the title of a wavelet.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      title: The title to set.
    """
    self.AddNewOperation(WAVELET_SET_TITLE, wave_id, wavelet_id,
                         prop=title)

  def BlipCreateChild(self, wave_id, wavelet_id, blip_id):
    """Requests to create a child blip of another blip.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.

    Returns:
      JSON of blip for which further operations can be applied.
    """
    blip_data = self.__CreateNewBlipData(wave_id, wavelet_id)
    self.AddNewOperation(BLIP_CREATE_CHILD, wave_id, wavelet_id,
                         blip_id=blip_id,
                         prop=blip_data)
    return blip_data

  def BlipDelete(self, wave_id, wavelet_id, blip_id):
    """Requests to delete (tombstone) a blip.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
    """
    self.AddNewOperation(BLIP_DELETE, wave_id, wavelet_id, blip_id=blip_id)

  def DocumentAnnotationDelete(self, wave_id, wavelet_id, blip_id, start, end,
                               name):
    """Deletes a specified annotation of a given range with a specific key.

    Not yet implemented.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      start: Start position of the range.
      end: End position of the range.
      name: Annotation key name to clear.
    """
    self.AddNewOperation(DOCUMENT_ANNOTATION_DELETE, wave_id, wavelet_id,
        blip_id=blip_id, prop=document.Range(start, end))

  def DocumentAnnotationSet(self, wave_id, wavelet_id, blip_id, start, end,
                            name, value):
    """Set a specified annotation of a given range with a specific key.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      start: Start position of the range.
      end: End position of the range.
      name: Annotation key name to clear.
      value: The value of the annotation across this range.
    """
    annotation = document.Annotation(name, value, document.Range(start, end))
    self.AddNewOperation(DOCUMENT_ANNOTATION_SET, wave_id, wavelet_id,
                         blip_id=blip_id,
                         prop=annotation)

  def DocumentAnnotationSetNoRange(self, wave_id, wavelet_id, blip_id,
                                   name, value):
    """Requests to set an annotation on an entire document.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      name: Annotation key name to clear.
      value: The value of the annotation.
    """
    annotation = document.Annotation(name, value, None)
    self.AddNewOperation(DOCUMENT_ANNOTATION_SET_NORANGE, wave_id, wavelet_id,
                         blip_id=blip_id,
                         prop=annotation)

  def DocumentAppend(self, wave_id, wavelet_id, blip_id, content):
    """Requests to append content to a document.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      content: The content to append.
    """
    self.AddNewOperation(DOCUMENT_APPEND, wave_id, wavelet_id,
                         blip_id=blip_id,
                         prop=content)

  def DocumentAppendMarkup(self, wave_id, wavelet_id, blip_id, content):
    """Requests to append content with markup to a document.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      content: The markup content to append.
    """
    op = Operation(DOCUMENT_APPEND_MARKUP, wave_id, wavelet_id,
                   blip_id=blip_id,
                   prop=content)
    self.__context.AddOperation(op)

  def DocumentAppendStyledText(self, wave_id, wavelet_id, blip_id, text, style):
    """Requests to append styled text to the document.

    Not yet implemented.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      text: The text ot append..
      style: The style to apply.

    Raises:
      NotImplementedError: Function not yet implemented.
    """
    raise NotImplementedError()

  def DocumentDelete(self, wave_id, wavelet_id, blip_id, start=None, end=None):
    """Requests to delete content in a given range.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      start: Start of the range.
      end: End of the range.
    """
    if start is None or end is None:
      range = None
    else:
      range = document.Range(start, end)
    self.AddNewOperation(DOCUMENT_DELETE, wave_id, wavelet_id, blip_id,
                         prop=range)

  def DocumentInsert(self, wave_id, wavelet_id, blip_id, content, index=0):
    """Requests to insert content into a document at a specific location.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      content: The content to insert.
      index: The position insert the content at in ths document.
    """
    self.AddNewOperation(DOCUMENT_INSERT, wave_id, wavelet_id, blip_id,
                         index=index, prop=content)

  def DocumentReplace(self, wave_id, wavelet_id, blip_id, content):
    """Requests to replace all content in a document.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      content: Content that will replace the current document.
    """
    self.AddNewOperation(DOCUMENT_REPLACE, wave_id, wavelet_id, blip_id,
                         prop=content)

  def DocumentElementAppend(self, wave_id, wavelet_id, blip_id, element):
    """Requests to append an element to the document.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      element: Element instance to append.
    """
    self.AddNewOperation(DOCUMENT_ELEMENT_APPEND, wave_id, wavelet_id, blip_id,
                         prop=element)

  def DocumentElementDelete(self, wave_id, wavelet_id, blip_id, position):
    """Requests to delete an element from the document at a specific position.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      position: Position of the element to delete.
    """
    self.AddNewOperation(DOCUMENT_ELEMENT_DELETE, wave_id, wavelet_id, blip_id,
                         index=position)

  def DocumentElementInsert(self, wave_id, wavelet_id, blip_id, position,
                            element):
    """Requests to insert an element to the document at a specific position.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      position: Position of the element to delete.
      element: Element instance to insert.
    """
    self.AddNewOperation(DOCUMENT_ELEMENT_INSERT, wave_id, wavelet_id, blip_id,
                         index=position,
                         prop=element)

  def DocumentElementInsertAfter(self):
    """Requests to insert an element after the specified location.

    Not yet implemented.

    Raises:
      NotImplementedError: Function not yet implemented.
    """
    raise NotImplementedError()

  def DocumentElementInsertBefore(self):
    """Requests to insert an element before the specified location.

    Not yet implemented.

    Raises:
      NotImplementedError: Function not yet implemented.
    """
    raise NotImplementedError()

  def DocumentElementReplace(self, wave_id, wavelet_id, blip_id, position,
                             element):
    """Requests to replace an element.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      position: Position of the element to replace.
      element: Element instance to replace.
    """
    self.AddNewOperation(DOCUMENT_ELEMENT_REPLACE, wave_id, wavelet_id, blip_id,
                         index=position,
                         prop=element)

  def DocumentModifyAttributes(self, wave_id, wavelet_id, blip_id,
                               element):
    """Modifies the attributes of an element.

    This is done by passing the a new element that is matched against
    existing elements and the attributes are copied without the element
    actually being deleted and reinserted. This is especially useful for
    gadgets.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      element: Element instance to take the attributes from an to
               match.
    """
    self.AddNewOperation(DOCUMENT_ELEMENT_MODIFY_ATTRS, wave_id, wavelet_id, blip_id,
                         index=-1,
                         prop=element)

  def DocumentInlineBlipAppend(self, wave_id, wavelet_id, blip_id):
    """Requests to create and append a new inline blip to another blip.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.

    Returns:
      JSON of blip containing the id information.
    """
    inline_blip_data = self.__CreateNewBlipData(wave_id, wavelet_id)
    self.AddNewOperation(DOCUMENT_INLINE_BLIP_APPEND, wave_id, wavelet_id,
                         blip_id=blip_id,
                         prop=inline_blip_data)
    inline_blip_data['parentBlipId'] = blip_id
    return inline_blip_data

  def DocumentInlineBlipDelete(self, wave_id, wavelet_id, blip_id,
                               inline_blip_id):
    """Requests to delete an inline blip from its parent.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      inline_blip_id: The blip to be deleted.
    """
    self.AddNewOperation(DOCUMENT_INLINE_BLIP_DELETE, wave_id, wavelet_id,
                         blip_id=blip_id,
                         prop=inline_blip_id)

  def DocumentInlineBlipInsert(self, wave_id, wavelet_id, blip_id, position):
    """Requests to insert an inline blip at a specific location.

    Args:
      wave_id: The wave id owning that this operation is applied to.
      wavelet_id: The wavelet id that this operation is applied to.
      blip_id: The blip id that this operation is applied to.
      position: The position in the document to insert the blip.

    Returns:
      JSON data for the blip that was created for further operations.
    """
    inline_blip_data = self.__CreateNewBlipData(wave_id, wavelet_id)
    inline_blip_data['parentBlipId'] = blip_id
    self.AddNewOperation(DOCUMENT_INLINE_BLIP_INSERT, wave_id, wavelet_id,
                         blip_id=blip_id,
                         index=position,
                         prop=inline_blip_data)
    return inline_blip_data

  def DocumentInlineBlipInsertAfterElement(self):
    """Requests to insert an inline blip after an element.

    Raises:
      NotImplementedError: Function not yet implemented.
    """
    raise NotImplementedError()

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

"""Defines event types that are sent from the wave server.

This module defines all of the event types currently supported by the wave
server.
"""

__author__ = 'davidbyttow@google.com (David Byttow)'


# Event Types
WAVELET_BLIP_CREATED = 'WAVELET_BLIP_CREATED'
WAVELET_BLIP_REMOVED = 'WAVELET_BLIP_REMOVED'
WAVELET_PARTICIPANTS_CHANGED = 'WAVELET_PARTICIPANTS_CHANGED'
WAVELET_SELF_ADDED = 'WAVELET_SELF_ADDED'
WAVELET_SELF_REMOVED = 'WAVELET_SELF_REMOVED'
WAVELET_TIMESTAMP_CHANGED = 'WAVELET_TIMESTAMP_CHANGED'
WAVELET_TITLE_CHANGED = 'WAVELET_TITLE_CHANGED'
WAVELET_VERSION_CHANGED = 'WAVELET_VERSION_CHANGED'
BLIP_CONTRIBUTORS_CHANGED = 'BLIP_CONTRIBUTORS_CHANGED'
BLIP_DELETED = 'BLIP_DELETED'
BLIP_SUBMITTED = 'BLIP_SUBMITTED'
BLIP_TIMESTAMP_CHANGED = 'BLIP_TIMESTAMP_CHANGED'
BLIP_VERSION_CHANGED = 'BLIP_VERSION_CHANGED'
DOCUMENT_CHANGED = 'DOCUMENT_CHANGED'
FORM_BUTTON_CLICKED = 'FORM_BUTTON_CLICKED'

# Event Properties

# Properties for WAVELET_PARTICIPANTS_CHANGED, WAVELET_SELF_ADDED and
# WAVELET_SELF_REMOVED
PARTICIPANTS_ADDED = 'participantsAdded'
PARTICIPANTS_REMOVED = 'participantsRemoved'

# Properties for WAVELET_TITLE_CHANGED
TITLE = 'title'

# Properties for WAVELET_VERSION_CHANGED
VERSION = 'version'

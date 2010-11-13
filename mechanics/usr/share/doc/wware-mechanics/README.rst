Documentation
=============

It would be fun to put together a little toolkit for building 3D models of
Rube Goldberg machines that can be simulated with ODE and animated with
POV-Ray or other animation software.

For a quick animation to check positions and trajectories, you could do either
wireframes or Z-buffering. Parts should have opacity as a color parameter,
which means the Z-buffer pixels are trees. With POV-Ray, parts can also be
given an index of refraction so they can refract what's behind them.

The ODE folks recommend that ODE be tailored to the needs of specific
applications. In a future version, the ODE source code will be modified, and
included in the source deb package, and built specially for this library. For
the present I'll start with simple shapes and use the Ubuntu libode1 package.

Figuring stuff out
------------------

Ubuntu's libode1 package includes a parser for XODE_, an XML spec for scene
description.

.. _XODE: http://tanksoftware.com/xode/

XODE is nice, but I'd prefer a JSON-like representation with information for
both ODE and for rendering.

There are a few levels of rendering. In order from the fastest to the most
realistic, they are:
* Wireframe.
* Painter's algorithm, break everything into triangles.
* Z-buffer with no transparency.
* Z-buffer with transparency, each pixel needs to be a tree structure.
* Ray-tracing.

Let's do this. Take a few simple shapes, like box and sphere, and write the
rendering code for each of these levels. The ray-tracing can obviously be
passed off to POV-Ray. Wireframe and painter's algorithm both use triangle
meshes. Z-buffer will be pretty similar with or without transparency.


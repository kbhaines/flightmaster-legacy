                 FlightMaster Linux Release Notes


    ** This is pre-production quality software, use at your own risk.  **


Installation
------------

Determine which device your GPS is. For example, if it is a USB
GPS then it may be '/dev/ttyUSB0'


Running
-------

From a command line, run: 

	FM-Linux -g <dev> -t data/terrain/

where <dev> is your GPS device, e.g.:

	FM-Linux -g /dev/ttyUSB0 -t data/terrain/

(NB - the trailing / is important!)

To quit, press SHIFT+Q, or close the window.

Once the GPS is locked, you will see the 'Reset' button, which
allows you to return to your GPS location.

Known Issues
------------

Scroll Wheel

Do not use the scroll-wheel when either mouse button is down, it will
trigger a bug in one of the graphics libraries that FM uses


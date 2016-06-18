                 FlightMaster Windows Release Notes


    ** This is pre-production quality software, use at your own risk.  **


Installation
------------

You need to ensure that your GPS is available on one of the Com ports
that Windows assigns when you set up your GPS. For example, some
Bluetooth GPS receivers will appear on 'Com3' 

Edit the file "FM-Windows.bat" and replace the -g option with the
number of the Com port, e.g. if your receiver is on Com3:

	FM-Windows.exe -g 3


Running
-------

Double click on FM-Windows.bat. 

Once the GPS is locked, you will see the 'Reset' button, which
allows you to return to your GPS location.

To quit, press SHIFT+Q, or close the window.

Known Issues
------------

Scroll Wheel

Do not use the scroll-wheel when either mouse button is down, it will
trigger a bug in one of the graphics libraries that FM uses


GPS issues

If the GPS doesn't appear to connect after 10-20 seconds, quit FM and 
use task manager to look for and kill these processes:

  GPSReader.exe
  wincomm.exe

Then re-launch FlightMaster.



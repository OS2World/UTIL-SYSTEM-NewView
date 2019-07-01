Config Apps
===========

OS/2 Internet Application configuration

by Aaron Lawrence

---------------------------

This is a small utility program to edit the default web browser
settings stored in the OS2.INI (user profile) under the
WPURLDEFAULTSETTINGS application/section.

These can also be edited using the default settings in a desktop 
URL object or template. The same settings are used either way.

To make it more complete, I also added a default parameters setting,
and decided on settings for various other protocols, including email, 
newsgroups, FTP and IRC. 

NOTE!! At time of 1.1.0 release, ONLY Mozilla for OS/2 has been modified to 
support these other settings where appropriate. I hope other software 
authors will feel free to do the same. 

Many applications have their own settings, even for the browser setting.
(My program NewView, as of 2.14.39, only uses the Browser settings 
but may be modified to use the other settings sometime.)

Changes
-------

1.2.0 (ALT) Added language file support via (ACLLanguageUnit).
      (ALT) Slight layout adjustments.
      (ALT) New icon; should now render properly (all devices).
1.1.1 Added IRC settings.
      Corrected Browser parameters ini setting to match URL objects
1.0.2 Corrected saving bug. Allow apps without explicit path.
1.0.0 First public release.

Multi-lingual Support
---------------------

ConfigApps can now (V1.2.0+) load different languages for its user interface.
This is done at program startup (changing the language file requires restarting
the program).

It looks for a file which is called `cfgapps_XX[_YY].lng`, where XX[_YY] is 
based on the LANG environment variable.  For example, if LANG=en_US it will 
first look for `cfgapps_en_us.lng`; failing that, it looks for the major part 
of the LANG variable, e.g. `cfgapps_en.lng`.  These files are searched for in
the following locations (in order):

    - %OSDIR%\lang  (ArcaOS and eCS 1.1+)
    - %ULSPATH%
    - The application directory (where ConfigApps.EXE is located)

If it cannot find a language file according to the logic above, it will default
to using US English (which is built into the .EXE).  

Normally, translations are not for a particular region, so in most cases the
files will just be named (for example) cfgapps_es.lng, not cfgapps_es_es.lng.

You can create a new language file by copying the included `cfgapps_xx.lng` to
the appropriate filename for that language (per above) and translating the 
contents.  The file is plain text; the format should be easy to understand.
(A `~` character identifies the following letter as an accelerator for that
control.)

Technical Details
-----------------

The settings are stored in the WPURLDEFAULTSETTINGS application section
in the User Profile (OS2.INI).

Browser:
(as standard for Warp 4 and higher)
      PathKey: 'DefaultBrowserExe'
      WorkingDirectoryKey: 'DefaultWorkingDir'
      ParametersKey: 'DefaultParameters'


Mail:
      PathKey: 'DefaultMailExe'
      WorkingDirectoryKey: 'DefaultMailWorkingDir'
      ParametersKey: 'DefaultMailParameters'

Usenet Newsgroups:
      PathKey: 'DefaultNewsExe'
      WorkingDirectoryKey: 'DefaultNewsWorkingDir'
      ParametersKey: 'DefaultNewsParameters'

FTP:
      PathKey: 'DefaultFTPExe'
      WorkingDirectoryKey: 'DefaultFTPWorkingDir'
      ParametersKey: 'DefaultFTPParameters'


IRC:
      PathKey: 'DefaultIRCExe'
      WorkingDirectoryKey: 'DefaultIRCWorkingDir'
      ParametersKey: 'DefaultIRCParameters'


Application Behaviour
---------------------

Ideally, I would create a C (and Rexx) DLL to provide a standard
interface to launch programs appropriately. I have not done that yet.

- If a given program path is not specified, then use the browser path
(preferably with a correct URL, e.g. mailto:)

- If a working directory is not specified, then use the program directory.

- EXE path may not actually include an explicit path, in which case the
system path should be used - as you would expect.

On Parameters
-------------

These are suggestions.

For browser:
  %url%    - insert URL here
Mail
  %url%    - complete email address
News
  %group%  - group name
  %host%   - server name
  %mid%    - message ID (if relevant)
FTP
  %url%    - server address, including 
IRC
  %url%    - full server+channel URL

In each case, if one of the placeholders is not specified then the value
should be appended. For example, if the news parameters key specifies

-s%host%

then the program launching the news program should pass something like

news.exe -snews.stardock.com stardock.os2 

DLL interface
-------------

This is a suggestion.

InetBrowseURL( char* URL );
InetCreateMail( char* Address );
InetReadNews( char* Group, char* Server Name );
InetOpenFTP( char* Address, char* Directory );

Of course, one can get carried away with these things, coming up with a
giant MAPI style interface. or whatever. This is just aiming to get a
small functional interface that covers most average needs.


Notices
-------

OS/2 Internet Application configuration
Copyright (C) 2004-2013 Aaron Lawrence

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.


Source code available via the NewView project on OS/2 NetLabs:
http://svn.netlabs.org/repos/newview
http://svn.netlabs.org/repos/newview/trunk/ConfigApps

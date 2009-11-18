# foursquare.el --- Foursquare client for Emacs

Copyright (C) 2009  Edward O'Connor

* Author: Edward O'Connor <hober0@gmail.com>
* Keywords: convenience

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

# Commentary

Bare-bones [Foursquare][] API client library for Emacs. API methods
are all prefixed `4sq-'. No OAuth yet; I'll try adopting
[psanford/emacs-oauth][] soon. Depends on `geocode.el` from
[hober/geocode-el][] and on `rest-api.el` from [hober/37emacs][].

# History
* 2009-11-16: Initial release.
* 2009-11-17: Clean things up a bit. Checkdoc. Remove gross `eval' hack.
* 2009-11-17: Adopt [hober/md-readme][] for README.md generation.

# Links
[Foursquare]:            http://foursquare.com/
[psanford/emacs-oauth]:  http://github.com/psanford/emacs-oauth
[hober/geocode-el]:      http://github.com/hober/geocode-el
[hober/37emacs]:         http://github.com/hober/37emacs
[hober/md-readme]:       http://github.com/hober/md-readme




;;; foursquare.el --- Foursquare client for Emacs

;; Copyright (C) 2009  Edward O'Connor

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Bare-bones [Foursquare][] API client library for Emacs. API methods
;; are all prefixed `4sq-'. No OAuth yet; I'll try adopting
;; [psanford/emacs-oauth][] soon. Depends on `geocode.el` from
;; [hober/geocode-el][] and on `rest-api.el` from [hober/37emacs][].

;;; History:
;; 2009-11-16: Initial release.
;; 2009-11-17: Clean things up a bit. Checkdoc. Remove gross `eval' hack.
;; 2009-11-17: Adopt [hober/md-readme][] for README.md generation.

;;; Links:
;; [Foursquare]:            http://foursquare.com/
;; [psanford/emacs-oauth]:  http://github.com/psanford/emacs-oauth
;; [hober/geocode-el]:      http://github.com/hober/geocode-el
;; [hober/37emacs]:         http://github.com/hober/37emacs
;; [hober/md-readme]:       http://github.com/hober/md-readme

;;; Code:

(require 'json)
(require 'url)

(require 'geocode)
(require 'rest-api)

;; User-serviceable parameters

(defvar foursquare-debug nil
  "If non-null, foursquare.el will keep around debugging data.")

;; Guts of API

(defvar foursquare-api-version "v1"
  "The version of the Foursquare API this client supports.")

(defun foursquare-response (buffer)
  "Process and return the Foursquare API results contained in BUFFER."
  (unwind-protect
      (with-current-buffer buffer
        (url-http-parse-response)
        (goto-char url-http-end-of-headers)
        (json-read))
    (unless foursquare-debug
      (kill-buffer buffer))))

(defun foursquare-build-params (arg-alist)
  "Builds up query string paramaters that match ARG-ALIST.
Args with empty or null values are stripped."
  (rest-api-format-url-parameters
   (remove-if-not (lambda (cons) (cdr cons)) arg-alist)))

(defun foursquare-build-request-url (path)
  "Create a Foursquare API request url to PATH."
  (format "http://api.foursquare.com/%s/%s.json" foursquare-api-version path))

(defun foursquare-request (path method args)
  "Send a request to the Foursquare API call at PATH.

When METHOD is \"GET\", ARGS are passed in as query string parameters.
If it's \"POST\", they're passed in the request body as an HTML form."
  (foursquare-response
   (let* ((url-package-name "foursquare.el")
          (url-request-method method)
          (params (foursquare-build-params args))
          (url (foursquare-build-request-url path)))
     (message "Requesting %s" url)
     (if (string-equal method "POST")
         (let ((url-request-data params)
               (url-request-extra-headers
                '(("Content-type" . "application/x-www-form-urlencoded"))))
           (url-retrieve-synchronously url))
       (url-retrieve-synchronously (concat url "?" params))))))
(put 'foursquare-request 'lisp-indent-function 2)

(defmacro foursquare-define-api-call (api-call http-method
                                      &optional requires-auth docstring
                                                requireds optionals)
  "Build a function for the given Foursquare API-CALL.
HTTP-METHOD, one of `GET' or `PUT', is whichever method the call
requires. If REQUIRES-AUTH is non-null, this API call needs the user to
authenticate to do anything useful. DOCSTRING describes the API call.
REQUIREDS and OPTIONALS are the required and optional parameters that
the API call takes, respectively."
  (let ((fn (intern (concat "4sq-" api-call)))
        (method (symbol-name http-method))
        (args (if optionals
                  (append requireds '(&optional) optionals)
                requireds)))
    `(defun ,fn ,args
       ,docstring
       (foursquare-request ,api-call ,method
         (list ,@(mapcar (lambda (arg-name) (list 'cons (list 'quote arg-name) arg-name)) requireds)
               ,@(mapcar (lambda (arg-name) (list 'cons (list 'quote arg-name) arg-name)) optionals))))))
(put 'foursquare-define-api-call 'lisp-indent-function 3)
(put 'foursquare-define-api-call 'doc-string-elt 5)

;; The API calls themselves

(foursquare-define-api-call "cities" GET nil
  "Fetches the list of cities Foursquare works in.")

(foursquare-define-api-call "checkcity" GET t
  "Finds the closest Foursquare city to this lat/long pair."
  (geolat geolong))

(foursquare-define-api-call "switchcity" POST t
  "Switches the current user to the city whose ID is CITYID."
  (cityid))

(foursquare-define-api-call "checkins" GET t
  "Returns your friends' recent checkins."
  (cityid))

(foursquare-define-api-call "checkin" POST t
  "Checks you in at a venue."
  () (vid venue shout private twitter geolat geolong))

(foursquare-define-api-call "history" GET t
  "Returns your history of checkins in any city.
If non-null, L is the number of checkins you want to retreive."
  (l))

(foursquare-define-api-call "user" GET t
  "Fetches profile information for given user.
Optional arguments BADGES and MAYOR, if provided, must be either 1 or 0."
  () (uid badges mayor))

(foursquare-define-api-call "friends" GET t
  "Returns your list of friends.")

(foursquare-define-api-call "venues" GET nil
  "Searches for venues near your current lat/long.
If non-null, L is the number of venues you want to retreive. Q is for
limiting to venues that match the keywords provided."
  (geolat geolong) (l q))

(foursquare-define-api-call "venue" GET nil
  "Fetches info about the venue with ID VID."
  (vid))

(foursquare-define-api-call "addvenue" POST t
  "Add a new venue to the system."
  (name address crossstreet city state cityid) (zip phone))

(foursquare-define-api-call "tips" GET nil
  "Fetch tips near this lat/long."
  (geolat geolong) (l))

(foursquare-define-api-call "addtip" POST t
  "Add a tip.
VID is the ID of the venue your tip is about. TEXT is the tip to add.
If specified, TYPE should be either `tip' or `todo'."
  (vid text) (type))

(foursquare-define-api-call "test" GET nil
  "Test that the Foursquare API is up and running.")

;; Higher-level calls built on top of the API.

(defun foursquare-venues-near (address &optional l q)
  "Fetches a list of venues near ADDRESS.
If non-null, limit the search result to no more than L items. If Q is
non-null, limit the search to venues that match the search keywords
therein."
  (let ((adr (geocode address)))
    (4sq-venues (plist-get adr :lat) (plist-get adr :long) l q)))

(defun foursquare-city-near (address &optional l q)
  "Fetches the Foursquare city near ADDRESS, if any.
If non-null, limit the search result to no more than L items. If Q is
non-null, limit the search to venues that match the search keywords
therein."
  (let ((adr (geocode address)))
    (cdr (assq 'city
               (4sq-checkcity (plist-get adr :lat)
                              (plist-get adr :long))))))

(provide 'foursquare)
;;; foursquare.el ends here

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

;; 

;;; Code:

(require 'json)
(require 'url)

(require 'geocode)
(require 'rest-api)

(defvar foursquare-api-version "v1")
(defvar foursquare-debug nil)

(defun foursquare-response (buffer)
  (unwind-protect
      (with-current-buffer buffer
        (url-http-parse-response)
        (goto-char url-http-end-of-headers)
        (json-read))
    (unless foursquare-debug
      (kill-buffer buffer))))

(defun foursquare-build-params (args)
  (rest-api-format-url-parameters
   (remove-if-not (lambda (cons) (cdr cons)) args)))

(defun foursquare-request (path method args)
  (foursquare-response
   (let* ((url-package-name "foursquare.el")
          (url-request-method method)
          (params (foursquare-build-params args))
          (url (concat "http://api.foursquare.com/" foursquare-api-version
                       "/" path ".json")))
     (message "Requesting %s" url)
     (if (string-equal method "POST")
         (let ((url-request-data params)
               (url-request-extra-headers
                '(("Content-type" . "application/x-www-form-urlencoded"))))
           (url-retrieve-synchronously url))
       (url-retrieve-synchronously (concat url "?" params))))))
(put 'foursquare-request 'lisp-indent-function 2)

;; Yes, this is gross.
(defun foursquare-eval-alist-builder (symbol)
  (cons symbol (symbol-value symbol)))

(defmacro foursquare-define-api-call (api-call http-method
                                      &optional requires-auth docstring
                                                requireds optionals)
  (let ((fn (intern (concat "4sq-" api-call)))
        (method (symbol-name http-method))
        (args (if optionals
                  (append requireds '(&optional) optionals)
                requireds)))
    `(defun ,fn ,args
       ,docstring
       (foursquare-request ,api-call ,method
         (let ((requireds ',requireds)
               (optionals ',optionals))
           (append (mapcar 'foursquare-eval-alist-builder requireds)
                   (mapcar 'foursquare-eval-alist-builder optionals)))))))

(put 'foursquare-define-api-call 'lisp-indent-function 3)
(put 'foursquare-define-api-call 'doc-string-elt 5)

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

(defun foursquare-venues-near (address &optional l q)
  "Fetches a list of venues near the given address."
  (let ((adr (geocode address)))
    (4sq-venues (plist-get adr :lat) (plist-get adr :long) l q)))

(defun foursquare-city-near (address &optional l q)
  "Fetches the Foursquare city near the given address, if any."
  (let ((adr (geocode address)))
    (cdr (assq 'city
               (4sq-checkcity (plist-get adr :lat)
                              (plist-get adr :long))))))

(provide 'foursquare)
;;; foursquare.el ends here

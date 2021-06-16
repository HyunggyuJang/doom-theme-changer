;;; theme-changer.el --- Sunrise/Sunset Theme Changer for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2011-2013 Joshua B. Griffith

;; Author: Joshua B. Griffith <josh.griffith@gmail.com>
;; Contributors: Joe Snikeris, Mike Fisher, Göktuğ Kayaalp
;; URL: https://github.com/hadronzoo/theme-changer
;; Created: 20 Jun 2011
;; Version: 2.1.0
;; Keywords: color-theme, deftheme, solar, sunrise, sunset

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Given a location and day/night color themes, this file provides a
;; `change-theme` function that selects the appropriate theme based on
;; whether it is day or night.  It will continue to change themes at
;; sunrise and sunset.  To install:

;; Set the location:
;;     (setq calendar-location-name "Dallas, TX")
;;     (setq calendar-latitude 32.85)
;;     (setq calendar-longitude -96.85)

;; Specify the day and night themes:
;;     (require 'theme-changer)
;;     (change-theme 'solarized-light 'solarized-dark)

;; If you specify a list of themes, a random one will be chosen at
;; each change:
;;     (change-theme '(solarized-light sanityinc-tomorrow-day)
;;                   '(solarized-dark sanityinc-tomorrow-night))

;; You can also pass nil as either of parameters to change-theme, with the
;; effect of not using a theme (or using the default Emacs theme) during that
;; period of the day.  For example:

;;     (change-theme nil 'solarized-dark)

;; will result in setting the default Emacs theme during the day, and
;; solarized-dark during the night.

;; You may need to add this file path to your loadpath.  For example:
;;     (add-to-list 'load-path "~/.emacs.d/elisp/theme-changer")

;; If you want to use the color-theme package instead of the Emacs 24 color
;; theme facility:
;;     (setq theme-changer-mode "color-theme")
;;     (change-theme 'color-theme-solarized-light 'color-theme-solarized-dark)

;;; Code:

(require 'cl-lib)
(require 'solar)

(defvar doom-theme)

(defcustom theme-changer-delay-seconds 0
  "Specify the delay seconds when switch themes at sunrise and sunset."
  :type 'integer
  :group 'theme-changer)

(defun theme-changer-hour-fraction-to-time (date hour-fraction)
  (let*
      ((now (decode-time (current-time)))

       (month (cl-first   date))
       (day   (cl-second  date))
       (year  (cl-third   date))
       (zone  (cl-ninth   now))

       (frac-hour (cl-truncate hour-fraction))
       (hour (cl-first frac-hour))

       (frac-minutes (cl-truncate (* (cl-second frac-hour) 60)))
       (minute (cl-first frac-minutes))

       (frac-seconds (cl-truncate (* (cl-second frac-minutes) 60)))
       (sec (cl-first frac-seconds)))
    (encode-time sec minute hour day month year zone)))


(defun theme-changer-sunrise-sunset-times (date)
  (let*
      ((l (solar-sunrise-sunset date))
       (sunrise-time (time-add (theme-changer-hour-fraction-to-time date (caar l))
                               (seconds-to-time theme-changer-delay-seconds)))
       (sunset-time (time-add (theme-changer-hour-fraction-to-time date (caadr l))
                              (seconds-to-time theme-changer-delay-seconds))))
    (when (> emacs-major-version 26)
      (setq sunrise-time (encode-time (decode-time sunrise-time)))
      (setq sunset-time (encode-time (decode-time sunset-time))))
    (list sunrise-time sunset-time)))

(defsubst theme-changer-today () (calendar-current-date))

(defun theme-changer-tomorrow ()
  (calendar-gregorian-from-absolute
   (+ 1 (calendar-absolute-from-gregorian (theme-changer-today)))))

(defsubst theme-changer-add-second (time)
  (let ((newtime (time-add time (seconds-to-time 1))))
    (encode-time (decode-time newtime))))

;;;###autoload
(defun hgj/change-theme (day-theme night-theme)
  "Doom adapted theme-changer.
DAY-THEME is the theme will be used at day time,
NIGHT-THEME is the counterpart of night time.

Usually, you'll want bright one as your day-time's
and darker for your night sight."
  (let* ((now (current-time))
         (sunrise-tomorrow (cl-first (theme-changer-sunrise-sunset-times
                                      (theme-changer-tomorrow)))))
    (cl-destructuring-bind (sunrise-today sunset-today)
        (theme-changer-sunrise-sunset-times (theme-changer-today))
      (cl-destructuring-bind (next-change . theme)
          (cond ((time-less-p now sunrise-today)
                 (cons sunrise-today night-theme))
                ((time-less-p now sunset-today)
                 (cons sunset-today day-theme))
                (t (cons sunrise-tomorrow night-theme)))
        (if doom-theme
            (load-theme theme t)
          (setq doom-theme theme))
        (run-at-time (theme-changer-add-second next-change) nil
                     'hgj/change-theme day-theme night-theme)))))

(provide 'theme-changer)

;;; theme-changer.el ends here

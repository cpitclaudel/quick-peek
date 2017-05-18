;;; quick-peek-test.el --- Tests for quick-peek      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;;         Tianxiang Xiong <tianxiang.xiong@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; This file is part of `quick-peek'

;;; Code:

(require 'ert)
(require 'quick-peek)

(ert-deftest quick-peek--append-face-test ()
  (let ((s (concat (propertize "foo" 'face 'warning)
                   (propertize "bar" 'font-lock-face 'font-lock-doc-face)
                   "baz"))
        (expected #("foobarbaz"
                    0 3 (face (warning success))
                    3 6 (font-lock-face (font-lock-doc-face success))
                    6 9 (face (success)))))
    ;; (font-lock-append-text-property 0 9 'face 'default s)
    (quick-peek--append-face 0 9 'success s)
    (should (ert-equal-including-properties expected s))))

;;; cg-test.el --- test call-graph functions. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; Test cases should go here.

;;; Code:

(require 'call-graph)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-assert (not (cg--number-of-args "func")))
(cl-assert (not (cg--number-of-args "class::func")))
(cl-assert (= (cg--number-of-args "func(template<p1,p2>(a),[a,b](a,b){a,b,c;},(a,b))") 3))
(cl-assert (eq (intern "method") (cg--extract-method-name (intern "method"))))
(cl-assert (eq (intern "method") (cg--extract-method-name (intern "class::method"))))
(cl-assert (eq (intern "method") (cg--extract-method-name (intern "class::method(arg1,arg2)"))))
(cl-assert (eq (intern "method") (cg--extract-method-name (intern "class::method(class::variable1,class::variable2)"))))


(provide 'cg-test)
;;; cg-test.el ends here

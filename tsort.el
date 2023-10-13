;;; tsort.el --- Topological sort for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2023 Ethan Hawk

;; Author: Ethan Hawk <ethan.hawk@valpo.edu>
;; Maintainer: Ethan Hawk <ethan.hawk@valpo.edu>
;; URL: https://github.com/ehawkvu/tsort.el
;; Keywords: algorithm, tools
;; Package-Requires: ((emacs "28.1"))
;; Version: 1.0.0
;; License: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains a simple recursive implementation of Kahn's algorithm.
;; This package was written in support for my other project, kiss.el
;; https://github.com/ehawkvu/kiss.el, which is a full featured Linux
;; package manager written entirely in Emacs Lisp.

;; Further reading:
;; https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm

;;; Code:

(eval-when-compile
  (require 'seq))

(defun tsort--rm-vertex-from-graph (v G)
  "Remove vertex V from graph G including all edges which contain vertex V."
  (mapcar
   (lambda (pair) (list (car pair) (remove (car v) (cadr pair))))
   (remove v G)))

(defun tsort--find-degree-zero-vertex (G)
  "Return return the first vertex in G that has a degree of nil (zero)."
  (car (seq-filter (lambda (pair) (equal (cadr pair) nil)) G)))


;;;###autoload
(defun tsort (G)
  "Perform a topological sort on the graph G.

Will return either the topologically sorted elements of G,
or will return nil in the case of a cycle.

Example of a graph:

'((A (B C))
  (B (D))
  (C (D))
  (D nil))

This graph is read to mean that A depends upon B and C, B depends upon D,
C depends upon D, and D depends upon nothing.

This graph will turn into the following topological sort: '(D B C A)"

  (let ((res
         (named-let tsort-impl ((graph G) (seen '()))
           "Recursive implementation of topological sort using Kahn's algorithm.

GRAPH is a collection of vertices and their edges and SEEN is the
eventual ordering of elements."

           (let ((vertex (tsort--find-degree-zero-vertex graph)))
             (if vertex
                 (tsort-impl (tsort--rm-vertex-from-graph vertex graph)
                             (cons (car vertex) seen))
               seen)))))
    (if (eql (length G) (length res))
        (reverse res))))

(provide 'tsort)

;;; tsort.el ends here

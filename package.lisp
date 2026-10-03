
(defpackage pithy-xml
    (:use :common-lisp)
  (:export #:print-xml
	   #:read-xml
	   #:read-xml-document
	   #:read-xml-file
	   #:camel
	   #:entity
	   #:xml-comment
	   #:*xml-entities*
	   #:*namespace-bindings*
	   #:*make-xml-namespace*
	   #:*xml-acronyms*
	   #:*default-set-external-format*
	   #:*default-attribute-namespace*
	   #:xml-intern
	   #:parse-xml-element
	   #:map-elements
	   #:find-descendant
	   #:define-xml-namespace
	   #:define-xml-namespace-by-schema
	   #:define-exact-xml-namespace-by-schema
	   #:xml-element ; type-specifier
	   #:xml-content
	   #:xml-attributes
	   #:xml-canonical
	   ))

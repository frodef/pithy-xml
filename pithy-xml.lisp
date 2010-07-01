;;; -*- mode: Common-Lisp; fill-column: 100; comment-column: 50; -*-
;;; $Id: pithy-xml.lisp,v 1.23 2010/04/21 10:55:32 frode Exp $
;;;
;;; Read and write XML as lisp objects. Syntax somewhat similar to that of CL-WHO.
;;;
;;; Disregarding interning (see below), an XML element <foo a=1>...</foo> is represented in either
;;; of two ways:
;;;
;;;   (:foo :a 1 ...)  or  ((:foo :a 1) ...)
;;;
;;; READ-XML will always return the latter form, because it is more consistent for processing. The
;;; former is perhaps more convenient to write: the element's content starts with the first
;;; non-keyword/value-pair.
;;;
;;; Tag names etc. are mapped to (and from) lisp symbols. XML namespaces are represented as lisp
;;; packages, into which symbols are interned. Note that XML namespaces are named by (globally
;;; unique) URLs, although you'll probably find it convenient to provide short-hand names (see
;;; DEFINE-XML-NAMESPACE). Read/print-xml will keep track of namespace bindings. In the following
;;; example, the same "foo" is referenced in the two XML fragments, using first "qualified names"
;;; and second "namespace defaulting":
;;;
;;; > (equal (cdr (read-xml "<bar xmlns:nn='http://blah'><nn:foo>42</nn:foo></bar>"))
;;;          (print (cdr (read-xml "<bar xmlns='http://blah'><foo>42</foo></bar>"))))
;;;
;;; ((NN:FOO "42"))
;;; => T
;;; 
;;;
;;; XML comments are represented as (pithy-xml:xml-comment "text"). If you don't care about XML
;;; namespaces, use keywords. For example:
;;;
;;; PITHY-XML> (read-xml (print (print-xml '(:element :attribute 1 (:sub-element 42)))))
;;; "<Element attribute='1'>
;;;    <SubElement>42</SubElement>
;;;  </Element>" 
;;; => (((:ELEMENT :ATTRIBUTE "1") ((:SUB-ELEMENT) "42")))
;;;

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
	   #:xml-intern
	   #:parse-xml-element
	   #:map-elements
	   #:find-descendant
	   #:define-xml-namespace
	   #:define-xml-namespace-by-schema
	   #:xml-element ; type-specifier
	   ))

(in-package pithy-xml)

(defvar *namespace-plists*
  (make-hash-table :test #'eq)
  "Properties for each namespace/package.")

(defvar *unknown-package-is-default-namespace-p* t
  "When encountering an unknown package, map to default namespace?")

(defvar *intern-unknown-symbols* t
  "When encountering a symbol that doesn't already exist in the namespace, and LOCK us undefined for
  the namespace, intern it?")

(defvar *default-set-external-format* 'default-set-external-format
  "Function to be called to set a STREAM's EXTERNAL-FORMAT.")

(defvar *print-no-namespace-binding* nil
  "What to do when printing a symbol and there is no current binding for the namespace?  If NIL, fail. If T, print name without
namespace prefix. If :PACKAGE-NAME, use the package's name as namespace prefix.")

(defvar *namespace-bindings*
  '(("xml" . pithy-xml.xml)
    ("xmlns" . pithy-xml.xmlns))
  "The active mappings from qualifiers to namespaces.")

(defvar *default-namespace* :keyword)

(defvar *make-xml-namespace* (lambda (name)
			       (cerror "Make package for XML namespace ~S."
				       "Unknown XML namespace ~S."
				       name)
			       (make-package name))
  "Called whenver a non-existing namespace is encountered. Return package to use.")

(defvar *xml-entities*
  '((#\< . "lt")
    (#\> . "gt")
    (#\& . "amp")
    (#\' . "apos")
    (#\" . "quot")))

(defvar *xml-acronyms* '("XML" "ID" "URL")
  "Default list of acronyms. These strings will not be (de)camelized.")

(defmacro define-xml-namespace (name url &rest options)
  "Define a package/namespace. NAME is a short-hand name, while URL is the canonical XML namespace
name which will become a nickname for the package. Extra options are ACRONYMS which lists a set of
acronyms (for tweaking the XML<->symbol mapping), and LOCK which is whether symbols should
automatically be interned for this package/namespace by READ-XML."
  `(progn
     (defpackage ,name (:use)
       (:nicknames ,url ,@(cdr (assoc :nicknames options)))
       ,@(set-difference options '((:nicknames) (:acronyms) (:lock))
			 :key #'car))
     ,(let ((acronyms (assoc :acronyms options)))
	(when acronyms
	  `(setf (getf (gethash (find-package ',name) *namespace-plists*)
		       'acronyms)
		 ',(mapcar #'string (cdr acronyms)))))
     ,(let ((lock (assoc :lock options)))
	(when lock
	  `(setf (getf (gethash (find-package ',name) *namespace-plists*)
		       'lock)
		 ',(cadr lock))))
     ',name))

(defmacro define-xml-namespace-by-schema (name url schema-path &rest options)
  "Declare a namespace by reading symbols from an XML schema file. Defaults :LOCK to true."
  `(define-xml-namespace ,name ,url
     (:export ,@(let ((*xml-acronyms* (cdr (or (assoc :acronyms options)
					       (cons :default *xml-acronyms*)))))
		  (read-schema-symbols schema-path
				       :expect-namespace url))
	      ,@(cdr (assoc :export options)))
     ,(or (assoc :lock options)
	  '(:lock t))
     ,@(set-difference options '((:export) (:lock))
		       :key #'car)))

(define-xml-namespace pithy-xml.xml
    "http://www.w3.org/XML/1998/namespace"
  (:documentation "http://www.w3.org/TR/REC-xml-names/"))

(define-xml-namespace #:xmlns "http://www.w3.org/2000/xmlns/"
  (:nicknames pithy-xml.xmlns)
  (:documentation "http://www.w3.org/TR/REC-xml-names/")
  (:export #:xmlns)
  (:lock nil))

(define-xml-namespace xml-schema "http://www.w3.org/2001/XMLSchema"
  (:nicknames #:xsd)
  (:documentation
   "Symbols from the XML meta-schema at http://www.w3.org/2001/XMLSchema.xsd")
  (:export #:schema
	   #:group
	   #:element
	   #:simple-type
	   #:attribute
	   #:attribute-group
	   #:any-attribute
	   #:complex-content
	   #:restriction
	   #:extension
	   #:simple-content
	   #:complex-type
	   #:all
	   #:choice
	   #:sequence
	   #:any
	   #:include
	   #:redefine
	   #:import
	   #:selector
	   #:field
	   #:unique
	   #:key
	   #:keyref
	   #:notation
	   #:appinfo
	   #:documentation
	   #:annotation
	   #:list
	   #:union
	   #:min-exclusive
	   #:min-inclusive
	   #:max-exclusive
	   #:max-inclusive
	   #:total-digits
	   #:fraction-digits
	   #:length
	   #:min-length
	   #:max-length
	   #:enumeration
	   #:white-space
	   #:pattern ))

(define-xml-namespace xml-schema-instance "http://www.w3.org/2001/XMLSchema-instance")

(define-xml-namespace html "http://www.w3.org/1999/xhtml")


(deftype xs%string () "xs:string" '(or string (and symbol (not null))))
(deftype xs%integer () "xs:integer" 'integer)
(deftype xs%long () "xs:long" '(signed-byte 64))
(deftype xs%int () "xs:int" '(signed-byte 32))
(deftype xs%boolean () "xs:boolean" 'boolean)
(deftype xml-element (&optional (name '*) (content '*))
  "Either (name ...) or ((name ...) ...), or any element that is not a comment."
  (if (eq name '*)
      '(cons (not (eql xml-comment)))
      `(cons (or (eql ,name)
		 (cons (eql ,name)))
	     ,content)))
(deftype xml-comment ()
  "An XML comment."
  '(xml-element xml-comment))

(defun camel (stream string &optional colon at)
  "Convert a hyphenated-name to CamelCase, unless it already
is. Encode XML entities. Convert '%' to ':'."
  (if (not stream)
      (with-output-to-string (stream)
	(camel stream string colon at))
      (cond
	((eq 'xmlns:xmlns string)
	 (write-string "xmlns" stream))
	((symbolp string)
	 (unless (or (keywordp string)
		     (eq (symbol-package string)
			 (find-package *default-namespace*)))
	   (let ((package-binding (or (rassoc (symbol-package string)
					      *namespace-bindings*
					      :key #'find-package)
				      (unless *unknown-package-is-default-namespace-p*
					(error "No package binding for ~A:~A."
					       (package-name (symbol-package string))
					       string)))))
	     (cond
	       (package-binding
		(camel stream (car package-binding) nil t)
		(write-char #\: stream))
	       ((eq *print-no-namespace-binding* t))
	       ((eq *print-no-namespace-binding* :package-name)
		(camel stream (package-name (symbol-package string)))
		(write-char #\: stream))
	       (t (error "Unknown namespace for ~A: ~A."
			 string
			 (package-name (symbol-package string)))))))
	 (let ((*xml-acronyms* (getf (gethash (symbol-package string)
					      *namespace-plists*)
				     'acronyms
				     *xml-acronyms*)))
	   (camel stream (symbol-name string) colon at)))
	(t (let ((string (string string)))
	     (if (find-if (lambda (c)
			    (char/= c (char-upcase c)))
			  string)
		 (write-string (substitute #\: #\% string)
			       stream)
		 (loop with uppity = (unless (find #\% string)
				       (not at))
		       for i = 0 then (1+ i)
		       while (< i (length string))
		       do (let* ((c (char string i))
				 (entity (assoc c *xml-entities*)))
			    (cond
			      (entity
			       (format stream "~&~A;" (cdr entity)))
			      (uppity
			       (dolist (acronym *xml-acronyms*
					(write-char (char-upcase c) stream)) ; acronym not found here.
				 (let ((mismatch (mismatch acronym string
							   :start2 i
							   :test #'char-equal)))
				   (when (or (not mismatch)
					     (and (= mismatch (length acronym))
						  (char= #\- (char string (+ i mismatch)))))
				     (write-string acronym stream)
				     (incf i (1- (length acronym)))
				     (return))))
			       (setf uppity nil))
			      ((char= c #\-)
			       (setf uppity t))
			      ((char= c #\%)
			       (write-char #\: stream))
			      (t (write-char (char-downcase c) stream)))))))))))

(defun entity (stream content &optional colon at)
  "Print CONTENT to STREAM, encoding characters with XML entities according to *XML-ENTITIES*."
  (if (not stream)
      (with-output-to-string (stream)
	(entity stream content colon at))
      (loop for c across (typecase content
			   (string content)
			   (symbol (symbol-name content))
			   (t (with-standard-io-syntax
				  (write-to-string content))))
	    do (let ((entity (assoc c *xml-entities*)))
		 (if (not entity)
		     (write-char c stream)
		     (format stream "&~A;" (cdr entity)))))))

(defun print-xml (sexpr &key stream (indent-level 0) (indentation 2)
		  ((:no-namespace-binding *print-no-namespace-binding*) *print-no-namespace-binding*)
		  ((:namespace-bindings *namespace-bindings*) *namespace-bindings*)
		  ((:default-namespace *default-namespace*) *default-namespace*))
  "Serialize sexpr into XML.
Return a boolean indicating if sexpr was 'complex' enough to requre a
newline and indentation. The syntax is approximately like that of CL-WHO."
  (if (not stream)
      (with-output-to-string (s)
 	(print-xml sexpr
		   :stream s
		   :indentation indentation
		   :indent-level indent-level))
      (flet ((indent ()
	       (when indentation
		 (fresh-line stream)
		 (loop repeat (* indentation indent-level)
		       do (write-char #\space stream)))))
	(etypecase sexpr
	  (null
	     nil)
	  ((or string symbol)
	     (entity stream sexpr)
	     nil)
	  (integer
	     (format stream "~D" sexpr)
	     nil)
	  ((cons (eql xml-comment))
	     (format stream "<!-- ~A -->" (cadr sexpr)))
	  (list
	     (multiple-value-bind (element attributes contents)
		 (let ((head (pop sexpr)))
		   (if (listp head)
		       (values (car head)
			       (cdr head)
			       sexpr)
		       (values head
			       (loop while (and sexpr
						(symbolp (car sexpr))
						(not (null (car sexpr))))
				     collect (pop sexpr)
				     collect (pop sexpr))
			       sexpr)))
	       (loop for (k v) on attributes by #'cddr
		     do (when (member k '(:xmlns pithy-xml.xmlns:xmlns))
			  (setf *default-namespace*
				(or (find-package v)
				    (error "Can't default to nonexisting namespace ~S." v))))
			(when (eq (symbol-package k)
				  (find-package 'pithy-xml.xmlns))
			  (push (cons (symbol-name k)
				      (or (find-package v)
					  (find-package (funcall *make-xml-namespace* v))
					  (error "Can't bind ~S to nonexisting namespace ~S." (symbol-name k) v)))
				*namespace-bindings*)))
	       (cond
		 ((null contents)
		  (indent)
		  (let ((terminator (if (char= #\? (char (string element) 0))
					#\?
					#\/)))
		    (format stream "<~/pithy-xml:camel/~{ ~@/pithy-xml:camel/='~/pithy-xml:entity/'~}~C>"
			    element
			    attributes
			    terminator))
		  t)
		 (t
		  (indent)
		  (format stream "<~/pithy-xml:camel/~{ ~@/pithy-xml:camel/='~/pithy-xml:entity/'~}>"
			  element
			  attributes)
		  (let ((newlined-p nil))
		    (mapc (lambda (sub-sexpr)
			    (setf newlined-p
				  (or (print-xml sub-sexpr
						 :stream stream
						 :indent-level (1+ indent-level)
						 :indentation indentation)
				      newlined-p)))
			  contents)
		    (when newlined-p
		      (indent))
		    (format stream "</~/pithy-xml:camel/>"
			    element)
		    t)))))))))

(defun xml-intern (string &key stringp)
  "Decamelize, intern, etc. STRING into a symbol, or just the string if STRINGP is true."
  (labels
      ((decamelize (string)
	 (with-output-to-string (name)
	   (loop with previous-decamelized-p = nil
		 for decamelize-p = nil then t
		 for i = 0 then (1+ i)
		 while (< i (length string))
		 do (block process-char
		      (let ((c (char string i)))
			(cond
			  ((char= c (char-downcase c))
			   (setf previous-decamelized-p nil))
			  (t (dolist (acronym *xml-acronyms*)
			       (let ((mismatch (mismatch acronym string :start2 i)))
				 (when (or (not mismatch)
					   (and (= mismatch (length acronym))
						(let ((d (char string (+ mismatch i))))
						  (char/= d (char-downcase d)))))
				   (when decamelize-p
				     (write-char #\- name))
				   (write-string (string-upcase acronym) name)
				   (incf i (1- (length acronym)))
				   (return-from process-char))))
			   (when (and decamelize-p
				      (not previous-decamelized-p))
			     (write-char #\- name))
			   (setf previous-decamelized-p t)))
			(write-char (char-upcase c) name)))
		 finally (return name))))
       (ex-intern (string package)
	 (if (not package)
	     (make-symbol (decamelize string))
	     (let* ((package (find-package package))
		    (*xml-acronyms* (getf (gethash package *namespace-plists*)
					  'acronyms
					  *xml-acronyms*))
		    (decamel (decamelize string))
		    (symbol (or (find-symbol decamel package)
				(when (getf (gethash package *namespace-plists*)
					    'lock
					    (not *intern-unknown-symbols*))
				  (cerror "Intern symbol ~A into namespace ~A."
					  "Symbol ~A not fund in namespace ~A."
					  decamel
					  (package-name package)))
				(intern decamel package))))
	       (unless (eq (symbol-package symbol)
			   package)
		 (warn "Symbol ~S is not native to package ~S." symbol (package-name package)))
	       symbol))))
    (if stringp
	(decamelize string)
	(let ((colon-pos (position #\: string)))
	  (if (or (not colon-pos)
		  (eql colon-pos 0))
	      (ex-intern string *default-namespace*)
	      (let ((name (subseq string (1+ colon-pos)))
		    (prefix (subseq string 0 colon-pos)))
		(ex-intern name
			   (cdr (or (assoc prefix *namespace-bindings*
					   :test #'string=)
				    (error "Unknown namespace ~S for ~S." prefix name))))))))))

(defun default-set-external-format (stream encoding)
  "Attempt to verify that STREAM is compatible with ENCODING."
  ;; This is probably incomplete and implementation-specific. Tweak as required.
  (unless (typep stream 'string-stream)
    (let ((stream-ef (let ((ef (or (ignore-errors (stream-external-format stream))
				   (return-from default-set-external-format
				     (warn "Unable to determine if stream ~S is compatible with encoding ~S." stream encoding)))))
		       (string (if (listp ef) (car ef) ef)))))
      (unless (find encoding
		    (case stream-ef
		      ((:latin1) "iso-8859-1")
		      (otherwise (list stream-ef)))
		    :test #'string-equal)
	(cerror "Ignore XML document encoding."
		"Stream encoding ~S appears incompatible with XML document encoding ~S."
		stream-ef
		encoding)))))

(defun xml-file-encoding (path)
  "Return the file encoding declared for the file at PATH, if any."
  (with-open-file (xml path :element-type 'character)
    (let ((declaration (read-xml xml :type 'xml-element)))
      (when (typep declaration '(xml-element :?xml))
	(let ((encoding (getf (cdar declaration) :encoding)))
	  (when encoding
	    (values (intern (string-upcase encoding)
			    :keyword))))))))

(defun read-xml-file (path &key (external-format (xml-file-encoding path)) (type 'xml-element)
		      ((:xml-entities *xml-entities*) *xml-entities*))
  "Read all the contents from file at PATH."
  (values (with-open-file (xml path
			       :external-format (or external-format :latin-1)
			       :element-type 'character)
	    (loop with first-xml-element-p = t
		  for element = (read-xml-document xml :type t)
		    then (read-xml xml :type t)
		  while element
		  do (when (and first-xml-element-p
				(typep element '(xml-element :!doctype)))
		       (parse-doctype element))
		     (when (typep element 'xml-element)
		       (setf first-xml-element-p nil))
		  when (typep element type)
		    collect element))
	  *xml-entities*))

(defun unquote (string)
  (when (<= 2 (length string))
    (let ((q (char string 0)))
      (when (and (member q '(#\" #\'))
		 (char= q (char string (1- (length string)))))
	(values (subseq string 1 (1- (length string)))
		q)))))


(defun parse-doctype (doctype)
  (check-type doctype (xml-element :!doctype))
  (let ((declarations (find-if #'listp doctype)))
    (loop for declaration in declarations
	  do (when (typep declaration '(xml-element :!entity))
	       (let ((entity (second declaration))
		     (expansion (unquote (third declaration))))
		 (when (and entity
			    expansion
			    (not (rassoc entity *xml-entities*
					 :test #'string=)))
		   (push (cons expansion entity)
			 *xml-entities*)))))))

(defun read-xml-document (document-stream-or-string
			  &rest key-args
			  &key (set-external-format *default-set-external-format*)
			  type namespace-mode default-namespace namespace-bindings)
  "Like READ-XML, but process encoding directives by applying
SET-EXTERNAL-FORMAT to the stream and encoding, if specified in the
document."
  (declare (ignore type namespace-mode default-namespace namespace-bindings))
  (if (stringp document-stream-or-string)
      (with-input-from-string (stream document-stream-or-string)
	(apply #'read-xml-document stream key-args))
      (multiple-value-bind (prependix prolog encoding)
	  (let ((first-element (read-xml document-stream-or-string)))
	    (if (typep first-element '(xml-element :?xml))
		(values nil first-element (getf (cdar first-element) :encoding))
		(values first-element nil nil)))
	(when (and encoding set-external-format)
	  (labels ((get-input (s)
		     (if (typep s 'echo-stream)
			 (get-input (echo-stream-input-stream s))
			 s)))
	    (funcall set-external-format
		     (get-input document-stream-or-string)
		     encoding)))
	(remf key-args :set-external-format)
	(values (apply #'read-xml document-stream-or-string
		       :prepend (list prependix)
		       key-args)
		prolog))))

(defun read-xml (stream-or-string
		 &key
		 (type 'xml-element)
		 ((:default-namespace *default-namespace*) :keyword)
		 ((:namespace-bindings *namespace-bindings*) *namespace-bindings*)
		 prepend)
  "Read the first XML element from STREAM-OR-STRING that matches TYPE."
  (if (stringp stream-or-string)
      (with-input-from-string (s stream-or-string)
	(read-xml s :type type
		  :default-namespace *default-namespace*))
      (let ((stream stream-or-string)
	    (whitespace '(#\space #\newline #\return #\tab)))
	(labels
	    ((entity-expand (c expansion-stream)
	       (if (char/= c #\&)
		   (write-char c expansion-stream)
		   (let ((entity-name (read-token :terminator #\;)))
		     (cond
		       ((char= #\# (char entity-name 0))
			(write-char (code-char (if (char= #\x (char entity-name 1))
						   (parse-integer entity-name
								  :start 2
								  :radix 16)
						   (parse-integer entity-name
								  :start 1)))
				    expansion-stream))
		       (t (write-string (string (car (or (rassoc entity-name *xml-entities*
								 :test #'string=)
							 (error "Unknown XML entity ~S." entity-name))))
					expansion-stream))))))
	     (read-char-no-whitespace ()
	       (loop for c = (read-char stream)
		     when (not (member c whitespace))
		       return c))
	     (peek-char-no-whitespace ()
	       (let ((c (read-char-no-whitespace)))
		 (unread-char c stream)
		 c))
	     (read-token (&key terminator terminators (whitespace (unless terminator whitespace)) unexpected prefix)
	       (with-output-to-string (token)
		 (when prefix
		   (write-string (string prefix) token))
		 (loop for c = (read-char stream)
		       do (cond
			    ((member c unexpected)
			     (error "Unexpected XML character ~S~@[, expected ~S~]." c (or terminator terminators)))
			    ((member c whitespace)
			     (when terminator
			       (loop for d = (read-char stream)
				     until (char= d terminator)
				     do (assert (member d whitespace) (d)
						"Expected whitespace, got ~S." d)))
			     (loop-finish))
			    ((eql c terminator)
			     (loop-finish))
			    ((member c terminators)
			     (unread-char c stream)
			     (loop-finish))
			    (t (entity-expand c token))))))
	     (find-namespace (name)
	       (or (find-package name)
		   (funcall *make-xml-namespace* name)))
	     (read-attributes (&optional (terminator #\/))
	       ;; This does two passes: First pass must deal with namespace bindings, second pass interns names.
	       (loop for (attribute-name . value)
		       in (loop when (char= terminator (peek-char t stream))
				  collect (cons 'empty-element
						(read-char stream))
				until (when (char= #\> (peek-char t stream))
					(read-char stream))
				collect (let* ((attribute-name (read-token :terminator '#\=
									   :whitespace whitespace
									   :unexpected (list #\>)))
					       (quote (read-char-no-whitespace))
					       (value (read-token :terminator quote)))
					  (assert (member quote '(#\" #\')) ()
						  "Expected a quote in XML attribute, not ~S." quote)
					  (cond
					    ((string= "xmlns" attribute-name)
					     (setf *default-namespace*
						   (find-namespace value)))
					    ((string= "xmlns:" attribute-name
						      :end2 (min 6 (length attribute-name)))
					     (xml-intern attribute-name)
					     (push (cons (subseq attribute-name 6)
							 (find-namespace value))
						   *namespace-bindings*)))
					  (cons attribute-name
						value)))
		     append (if (eq attribute-name 'empty-element)
				`(empty-element ,value)
				;; "Default namespace declarations do not apply directly to attribute names; the interpretation of
				;; unprefixed attributes is determined by the element on which they appear."
				;; http://www.w3.org/TR/xml-names/#defaulting
				(list (let ((*default-namespace* :keyword))
					(xml-intern attribute-name))
				      value))))
	     (read-process-instruction ()
	       (let* ((char (read-char stream))
		      (process-instruction (xml-intern (read-token :terminators (list #\> char)
								   :prefix char))))
		 (case process-instruction
		   ((:?xml)
		      (let ((attributes (read-attributes #\?)))
			(assert (getf attributes 'empty-element) ()
				"Invalid XML process instruction ~A." process-instruction)
			(remf attributes 'empty-element)
			(list* process-instruction attributes)))
		   (t (prog1
			  (list process-instruction (read-token :terminator #\?))
			(read-char-no-whitespace))))))
	     (read-contents (start-tag &key (multiple-p t) path terminator)
	       (let* ((content-string
		       nil)
		      (sub-elements
		       (loop for c = (read-char stream start-tag)
			     while c
			     until (eql c terminator)
			     if (char/= #\< c)
			       do (when (or content-string
					    (unless (member c whitespace)
					      (setf content-string
						    (make-string-output-stream :element-type 'character))))
				    (entity-expand c content-string))
			     else if (char= #\? (peek-char nil stream))
				    collect (list (read-process-instruction))
				    and do (unless multiple-p
					     (loop-finish))
			     else if (char= #\! (peek-char nil stream))
				    collect (let ((exclamation (make-array 8
									   :element-type 'character
									   :fill-pointer 0
									   :adjustable t)))
					      (loop (let* ((e (read-char stream))
							   (stop (member e whitespace)))
						      (unless stop
							(vector-push-extend e exclamation))
						      (cond
							((string= "![CDATA[" exclamation)
							 (return (read-cdata)))
							((string= "!--" exclamation)
							 (return (read-comment)))
							(stop
							 (return (read-declaration exclamation)))))))
				    and do (unless multiple-p
					     (loop-finish))
			     else if (char/= #\/ (peek-char nil stream))
				    collect (read-element path)
				    and do (unless multiple-p
					     (loop-finish))
			     else
			       do (read-char stream) ; skip #\/
				  (let ((end-tag (string-right-trim whitespace (read-token :terminator #\>))))
				    (assert (string= start-tag end-tag) ()
					    "Expected XML end-tag ~S, got ~S." start-tag end-tag)
				    (loop-finish)))))
		 (or sub-elements
		     (when content-string
		       (list (string-trim whitespace
					  (get-output-stream-string content-string)))))))
	     (read-cdata ()
	       (with-output-to-string (cdata)
		 (loop with c0 = (read-char stream)
		       with c1 = (read-char stream)
		       with c2 = (read-char stream)
		       until (and (char= c0 #\]) (char= c1 #\]) (char= c2 #\>))
		       do (write-char c0 cdata)
			  (setf c0 c1 c1 c2 c2 (read-char stream)))))
	     (read-comment ()
	       (list 'xml-comment
		     (string-trim whitespace
				  (with-output-to-string (comment)
				    (loop with c0 and c1 and c2
					  until (and (eql c0 #\-) (eql c1 #\-) (eql c2 #\>))
					  do (when c0
					       (write-char c0 comment))
					     (setf c0 c1 c1 c2 c2 (read-char stream)))))))
	     (read-declaration (start-tag)
	       (list* (intern start-tag *default-namespace*)
		      (loop for c = (peek-char-no-whitespace)
			    until (char= c #\>)
			    collect (case c
				      ((#\[)
					 (read-char stream)
					 (read-contents nil :terminator #\]))
				      (t (let ((token (read-token :terminators '(#\>))))
					   (cond
					     ((string= "--" token)
					      (prog1 (read-comment)
						(unread-char #\> stream)))
					     (t token))))))))
	     (read-element (&optional path)
	       (let ((*namespace-bindings* *namespace-bindings*)
		     (*default-namespace* *default-namespace*)
		     (start-tag (read-token :terminators '(#\> #\/))))
		 (cond
		   ((string= "" start-tag)
		    nil)
		   ((string= "!--" start-tag)
		     (read-comment))
		   ((char= #\! (char start-tag 0))
		    (read-declaration start-tag))
		   (t (let* ((attributes (read-attributes)) ; Attributes must be read before start-tag is interned,
			     (tag (xml-intern start-tag)))  ; because attrs may bind namespace for start-tag.
			(cond
			  ((getf attributes 'empty-element)
			   (remf attributes 'empty-element)
			   (list* tag attributes))
			  (t (let ((contents (read-contents start-tag :path (cons tag path))))
			       (if (null attributes)
				   (list* tag contents)
				   (list* (list* tag attributes)
					  contents)))))))))))
	  (or (loop for element in prepend
		    when (typep element type)
		      return element)
	      (loop for (element) = (read-contents nil :multiple-p nil)
		    while element
		    when (typep element type)
		      return element))))))

(defun parse-xml-element (element)
  "Return the element's contents, attributes, and name."
  (etypecase element
    ((cons (cons symbol))
       (values (cdr element)
	       (cdar element)
	       (caar element)))
    ((cons symbol)
       (let ((name (pop element))
	     (attributes (loop while (and element (symbolp (car element)))
			       collect (pop element)
			       collect (pop element))))
	 (values element attributes name)))))

(defun find-descendant (parent &rest path)
  "Locate a descendant of PARENT according to PATH. Each path element either names the child element, or is an integer child index."
  (cond
    ((null path)
     parent)
    ((typep (car path) '(integer 0 *))
     (apply #'find-descendant
	    (nth (car path)
		 (parse-xml-element parent))
	    (cdr path)))
    (t (loop for child in (parse-xml-element parent)
	     thereis (when (eq (car path) (nth-value 2 (parse-xml-element child)))
		       (apply #'find-descendant child (cdr path)))))))

(defun map-elements (function document)
  "For each element in DOCUMENT, apply FUNCTION to the name and attribute plist. Recurse into contents when FUNCTION returns true."
  (multiple-value-bind (contents attributes name)
      (parse-xml-element document)
    (when (funcall function name attributes contents)
      (dolist (content contents)
	(when (listp content)
	  (map-elements function content))))))

(defun read-schema-symbols (path &key expect-namespace)
  "Extract the relevant symbols from an XML schema in PATH."
  (multiple-value-bind (schema schema-attributes)
      (let ((*make-xml-namespace* (lambda (name)
				    (unless (or (not expect-namespace)
						(string= name expect-namespace))
				      (warn "Unkown namespace in schema: ~S" name))
				    nil)))
	(parse-xml-element (first (print (read-xml-file path :type '(xml-element xsd:schema))))))
    (let ((target-namespace (getf schema-attributes :target-namespace)))
      (cond
	((not target-namespace)
	 (error "No target namespace declared in ~S." path))
	((not expect-namespace))
	((string= target-namespace expect-namespace))
	(t (error "Expected target namespace ~S, found ~S."
		  expect-namespace
		  target-namespace)))
      (let ((*default-namespace* target-namespace)
	    (collector nil))
	(dolist (element schema)
	  (map-elements (lambda (name attributes contents)
			  (declare (ignore contents))
			  (when (eq name 'xsd:element)
			    (let ((symbol (getf attributes :name)))
			      (when symbol
				(pushnew (xml-intern symbol :stringp t)
					 collector))))
			  t)
			element))
	(reverse collector)))))

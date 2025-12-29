# Drakma

## Overview

| Attribute | Value |
|-----------|-------|
| **Author** | Dr. Edi Weitz |
| **Repository** | https://github.com/edicl/drakma |
| **Stars** | ~252 |
| **License** | BSD |
| **Purpose** | Full-featured HTTP/HTTPS client |

## Project Structure

Classic Edi Weitz flat structure:

```
drakma/
├── drakma.asd
├── packages.lisp
├── specials.lisp
├── conditions.lisp
├── util.lisp
├── read.lisp
├── cookies.lisp
├── encoding.lisp
├── request.lisp
├── docs/
│   ├── index.html
│   └── index.xml
└── CHANGELOG
```

## ASDF Pattern

Standard Edi Weitz serial loading:

```lisp
(defsystem :drakma
  :description "Full-featured http/https client based on usocket"
  :author "Dr. Edi Weitz"
  :license "BSD"
  :serial t
  :version "2.0.10"
  :components ((:file "packages")
               (:file "specials")
               (:file "conditions")
               (:file "util")
               (:file "read")
               (:file "cookies")
               (:file "encoding")
               (:file "request"))
  :depends-on (:puri
               :cl-base64
               :chunga
               :flexi-streams
               :cl-ppcre
               #-:drakma-no-chipz :chipz
               #-:lispworks :usocket
               #-(or :lispworks7.1 ...) :cl+ssl))
```

### Conditional Dependencies

Feature expressions for optional components:

```lisp
;; Disable compression support
#-:drakma-no-chipz :chipz

;; LispWorks has built-in sockets
#-:lispworks :usocket

;; SSL optional
#-(or :lispworks7.1 ... :drakma-no-ssl) :cl+ssl
```

## Package Design

Single package with careful exports:

```lisp
(defpackage :drakma
  (:use :cl)
  (:export
   ;; Main function
   #:http-request
   
   ;; Cookie handling
   #:cookie-jar
   #:make-cookie-jar
   #:cookie-jar-cookies
   
   ;; Conditions
   #:drakma-condition
   #:drakma-error
   #:drakma-warning
   #:parameter-error
   #:syntax-error
   
   ;; Special variables
   #:*drakma-default-external-format*
   #:*text-content-types*
   #:*body-format-function*
   ...))
```

## API Design

### Central Function: HTTP-REQUEST

One function handles everything:

```lisp
(http-request uri
  &key
  ;; Method
  (method :get)
  
  ;; Content
  content
  content-type
  content-length
  parameters
  
  ;; Headers
  additional-headers
  cookie-jar
  basic-authorization
  
  ;; Connection
  proxy
  proxy-basic-authorization
  
  ;; SSL
  certificate
  key
  verify
  
  ;; Behavior
  redirect
  auto-referer
  keep-alive
  close
  
  ;; Output format
  (decode-content t)
  force-binary
  want-stream
  
  ;; Timeout
  connection-timeout
  read-timeout
  write-timeout)
```

### Multiple Return Values

Rich return information:

```lisp
(multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
    (http-request "http://example.com")
  ;; body - content as string or octets
  ;; status-code - 200, 404, etc.
  ;; headers - alist of response headers
  ;; uri - final URI (after redirects)
  ;; stream - if want-stream was true
  ;; must-close - whether connection should close
  ;; reason-phrase - "OK", "Not Found", etc.
  )
```

### Cookie Jar Pattern

Stateful cookie handling:

```lisp
(let ((jar (make-cookie-jar)))
  ;; First request sets cookies
  (http-request "http://example.com/login"
    :method :post
    :parameters '(("user" . "me") ("pass" . "secret"))
    :cookie-jar jar)
  
  ;; Second request sends cookies
  (http-request "http://example.com/protected"
    :cookie-jar jar))
```

## Special Variables

Configuration via special variables:

```lisp
(defvar *drakma-default-external-format* :latin-1
  "Default external format for HTTP bodies.")

(defvar *text-content-types* '(("text"))
  "Content types to treat as text.")

(defvar *body-format-function* 'determine-body-format
  "Function to determine body format from headers.")

(defvar *header-stream* nil
  "Stream to echo headers (debugging).")
```

## Condition Hierarchy

```lisp
(define-condition drakma-condition ()
  ())

(define-condition drakma-error (drakma-condition error)
  ())

(define-condition drakma-warning (drakma-condition warning)
  ())

(define-condition parameter-error (drakma-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Invalid parameter: ~A"
                     (parameter-error-message condition)))))

(define-condition syntax-error (drakma-error)
  ())
```

## Notable Patterns

### 1. Extensible Encoding/Decoding

Generic function for content encoding:

```lisp
(defgeneric decode-stream (encoding-type stream)
  (:documentation "Decode STREAM based on ENCODING-TYPE.
ENCODING-TYPE is a keyword from the Content-Encoding header."))

(defmethod decode-stream ((encoding-type (eql :gzip)) stream)
  (chipz:make-decompressing-stream 'chipz:gzip stream))

(defmethod decode-stream ((encoding-type (eql :deflate)) stream)
  (chipz:make-decompressing-stream 'chipz:zlib stream))
```

### 2. Browser Emulation

User-agent strings for different browsers:

```lisp
(defparameter *user-agents*
  '((:msie . "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; ...)")
    (:safari . "Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en) ...")
    (:firefox . "Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.8.1) ...")))
```

### 3. Proxy Support

Full proxy support including authentication:

```lisp
(http-request "http://internal.example.com"
  :proxy "http://proxy.example.com:8080"
  :proxy-basic-authorization '("proxyuser" . "proxypass"))
```

### 4. Multipart Form Data

Automatic multipart handling:

```lisp
(http-request "http://example.com/upload"
  :method :post
  :parameters `(("file" . ,pathname)
                ("name" . "description"))
  :content-type "multipart/form-data")
```

## Integration with Other Edi Weitz Libraries

Drakma builds on the Edi Weitz ecosystem:

- **flexi-streams**: Flexible character encoding
- **chunga**: Chunked transfer encoding
- **cl-ppcre**: Regular expressions for parsing
- **cl+ssl**: SSL/TLS support

## Testing

Uses Hunchentoot for integration tests:

```lisp
(defsystem :drakma-test
  :depends-on (:drakma :hunchentoot :fiveam)
  ...)
```

## Lessons for AI Code Generation

### 1. One Function, Many Options

For HTTP clients, one main function with keyword arguments:

```lisp
;; Rather than: get-request, post-request, put-request...
;; Use: (http-request url :method :post ...)
```

### 2. Rich Return Values

Use multiple values for complete information:

```lisp
;; Don't just return body
;; Return (body status headers uri stream close? reason)
```

### 3. Cookie Jars

Encapsulate stateful cookie handling:

```lisp
;; Create jar once, pass to all requests
;; Jar handles expiration, domains, etc.
```

### 4. Special Variables for Configuration

Global defaults via special variables:

```lisp
;; Override for specific requests via parameters
;; Or globally via setf
```

### 5. Conditional Features

Allow disabling features at compile time:

```lisp
;; :drakma-no-ssl - build without SSL
;; :drakma-no-chipz - build without compression
```

## Comparison to Hunchentoot

| Aspect | Drakma | Hunchentoot |
|--------|--------|-------------|
| Role | HTTP Client | HTTP Server |
| Main API | http-request function | Generic functions |
| State | Cookie jars | Request/Reply objects |
| Shared | flexi-streams, chunga | flexi-streams, chunga |

# QURI

<p align=center><a href="https://www.flickr.com/photos/m-louis/8209540334/in/photostream/"><img src="http://c1.staticflickr.com/9/8202/8209540334_261582582c_h.jpg" alt="冷やしきゅうり"></a></p>

**QURI** (pronounced "Q-ree") is yet another URI library for Common Lisp. It is intended to be a replacement of [PURI](http://puri.b9.com).

## Differences from PURI

- Fast. (See [Benchmark](#benchmark))
- Doesn't encode/decode URL implicitly.
- Supports userinfo. (ex. `git` in `git@github.com`)
- Supports IPv6 hostname. (ex. `ldap://[2001:db8::7]/`)
- URL encoding/decoding utilities.

## Usage

```common-lisp
(use-package :quri)

(defvar *uri* (uri "http://www.ics.uci.edu/pub/ietf/uri/#Related"))

*uri*
;=> #<PURI:URI http://www.ics.uci.edu/pub/ietf/uri/#Related>

(uri-scheme *uri*)
;=> :HTTP

(uri-host *uri*)
;=> "www.ics.uci.edu"

(uri-path *uri*)
;=> "/pub/ietf/uri/"

(uri-fragment *uri*)
;=> "Related"

(url-encode "/fooあ")
;=> "%2Ffoo%E3%81%82"

(url-decode "%2Ffoo%E3%81%82")
;=> "/fooあ"
```

## Installation

```
$ git clone https://github.com/fukamachi/quri
```

```common-lisp
(ql:quickload :quri)
```

## Benchmark

- Parsing a URI string 100,000 times.

|  QURI  |  PURI  |
|--------|--------|
| 0.158s | 0.405s |

QURI is approximately **60% faster** than PURI.

### QURI

```common-lisp
(time
  (dotimes (i 100000)
    (quri:uri "http://www.ics.uci.edu/pub/ietf/uri/#Related")))
```

```
Evaluation took:
  0.158 seconds of real time
  0.158407 seconds of total run time (0.157804 user, 0.000603 system)
  100.00% CPU
  471,408,117 processor cycles
  33,578,864 bytes consed
```

### PURI

```common-lisp
(time
  (dotimes (i 100000)
    (puri:uri "http://www.ics.uci.edu/pub/ietf/uri/#Related")))
```

```
Evaluation took:
  0.405 seconds of real time
  0.406877 seconds of total run time (0.404367 user, 0.002510 system)
  [ Run times consist of 0.013 seconds GC time, and 0.394 seconds non-GC time. ]
  100.49% CPU
  1,211,475,141 processor cycles
  64,014,384 bytes consed
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 3-Clause License.

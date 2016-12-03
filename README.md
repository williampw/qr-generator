qr-generator
=====================

Usage
---------
```qr-generator``` only exports two symbols for now: ```make``` and ```save```. See the following lines for a minimum demonstration.

```lisp
(defparameter my-text "Most software today is very much like an Egyptian 
pyramid with millions of bricks piled on top of each other, with no 
structural integrity, but just done by brute force and thousands of slaves. -- Alan Kay")

(defparameter qr-code (qr-generator:make my-text))
(qr-generator:save qr-code "quote.png")

```
![quote.png](quote.png)

More documentation as soon as possible.

This program is base on the following [tutorial](http://www.thonky.com/qr-code-tutorial/).

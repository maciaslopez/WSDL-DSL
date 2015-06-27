wsdl_dsl
========

This is a library that implements a domain specific language (DSL) that reuses the WSDL syntax to allow users familiar with web services to straightforward express WSDL types as data generators.

### dependencies

In order to use `wsdl_dsl` you need a [Quviq QuickCheck](http://www.quviq.com) licence.

### compiling

Just clone the repository and execute:

    make

to download and build rebar, then

    make compile

to compile `wsdl_dsl`.

### structure

The library is currently organised in three components:

* `wsdl_dsl` This is the main module, and the one intended to be used by
  interested parties. It offers access to the functions that help developers to
  define their data generators using a set of WSDL-like constructs. It
  implements early constraint checking, and efficient data generation.

* `wsdl_dsl_pp` This module features functions that developers can use to
  pretty-print the results of the combination of constructs included in the
  wsdl_dsl module in XML format, rather than in the internal format used by the
  library.

* `regexp_gen` This auxiliary module isolates the implementation of the support
  for regular expressions as constraints for string data.

### examples

At the moment, you can check the tail of the file `wsdl_dsl.erl` on the `src` folder for a few examples of the data generators you can write using `wsdl_dsl`.

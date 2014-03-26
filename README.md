# WSDL-DSL

WSDL-DSL is a domain specific language which reuses the essential part of the WSDL syntax making it straightforward to express WSDL types as QuickCheck generators.

## Dependencies

In order to use `wsdl_dsl` you need a [Quviq QuickCheck](http://www.quviq.com) licence.

## Compiling

The compilation process delegates on
[rebar](https://github.com/basho/rebar), but it is wrapped
in a Makefile. You need Internet access to download it. Just clone the
repository, download and build rebar typing:

```
git clone https://github.com/maciaslopez/WSDL-DSL.git
cd WSDL-DSL
make
```

Then,

```
make compile
```

to compile `wsdl_dsl`

## Examples

At the moment, you can check the tail of the file `wsdl_dsl.erl` on the
`src` folder for a few examples of the data generators you can write
using `wsdl_dsl`.

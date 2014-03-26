# WSDL-DSL

WSDL-DSL is a domain specific language which reuses the essential part of the WSDL syntax making it straightforward to express WSDL types as QuickCheck generators.

## Compiling

The compilation process delegates on
[rebar](https://github.com/basho/rebar), but it is wrapped
in a Makefile. You need Internet access to download it. This should
compile:

```
git clone https://github.com/maciaslopez/WSDL-DSL.git
cd WSDL-DSL
make
make compile
```

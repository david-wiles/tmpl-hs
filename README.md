# TMPL-HS

My latest attempt at writing a template engine. 

This one is written with Haskell.

## Installation


```
cabal install tmpl
```

## Usage

```
Usage: main TEMPLATE (-v|--varfile VARFILE) [-o|--output OUTPUT]

  This template engine resembles the Go template engine. To replace
  a section with another template file, use
  
  {{ template "<filename>" }}
  
  To use a variable defined in the variable file, use
  
  {{ "<key>" }}
  
  to replace it with the value associated with the key. If the key is
  not found, it will be replaced with an empty string.

Available options:
  TEMPLATE                 Template file to use
  -v,--varfile VARFILE     Variable file to use
  -o,--output OUTPUT       Output file to write to. Defaults to stdout
  -h,--help                Show this help text
```
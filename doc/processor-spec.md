# Language processor spec for Doubletake plugins
### Overview ###

As the doubletake core is intended to process data sourced from multiple
languages and do so using a plugin-based infrastructure it is critical that
all the plugins behave the same way and return a standard structure which the
core can manipulate and process. This file documents said standard structure
and how it represents parsed program code.

### What do I mean by a "processor"?
A processor is a suite of code which registers itself with the Doubletake
core via the plugin interface (documented seperately) which lexes, parses and
otherwise manipulates raw program code to form a standard AST-like data format
which the Doubletake core can use for program analysis.

## Processor function argument specification
~~~~
(def my-processor
 [input-source]
    ; input is an object implementing at least java.io.File.
    ;
    ; The processor function may do whatever it wishes with this file to 
    ; process it. The use of the nparse library is suggested as it is
    ; included as part of the Doubletake core.
    ;
    ; The return type for this function will be discussed in detail later.
~~~~
So Doubletake simply calls into the plugin codebase through this interface and 
hopes for the best.

## Processor function return specification
As the Doubletake core needs to be able to maniplute the results of the
calling the processor function the return type specificaiton is far more 
involved than the argument specification as it must gurantee that the return
value convey at a minumum the data which the Doubletake core needs to perform
semantic interference analysis.

### Doubletake's input requirements
Doubletake depends upon computing and comparing changes to the target codebase
by assignment line number and by some metric of assignment scope. A minimal 
representation for this data would be as follows:
~~~~
; a block skeleton
{ :name         <filename> 
  :member-vars  [] ; list of strings, naming MUST be consistant in functions
  :functions    [] ; list of function representations
}

; a function skeleton
{ :name         <string name>
  :defs         <set of pairs (line, symbol) where a var is assigned to>
  :uses         <set of pairs (line, symbol) where the value of a var is used>
}
~~~~
The intent of this spec should be obvious: provide a simple, non-constraining
specification for how processors can return the data required by the Doubletake
analysis toolkit.

There were two possibilities for this IO specification: that the processors'
output constitute some sort of domain-specific language representing defs, 
uses and function calls or that (and this is the option  which this document
uses) the processors themselves generate the def and use pairs. This simplifies
the Doubletake core immensely, as it removes the requirement that Doubletake
process some sort of standard AST itself, and it lightens the burden on the
processors as well as they are not required to generate code for some DSL, only
to return some data which should be easily retrieved by a walk of the parse
tree.

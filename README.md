# Doubletake
Doubletake is a Java analysis tool written in Clojure which attempts to predict
errors in changes to program code due to semantic interference in def/use pairs
and differences in these pairs between versions of the same code.

This technique is capable only of catching a limited set of errors, and
really just ammounts to a tool whereby a programmer can re-read his code and
say "yes I did mean that" or "oh crud oops glad I saw that before committing"
as the case may be.

Produced as a research project with Professor DeWayne E. Perry summer of 2012.

### Usage
~~~~
$ doubletake [options] original [version1] [[version2] [version3] ..]
~~~~

### License
Copyright (C) 2012 Reid McKenzie, DeWayne E. Perry, U of TX at Austin
~~~~
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in 
the Software without restriction, including without limitation the rights to 
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all 
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
SOFTWARE.
~~~~

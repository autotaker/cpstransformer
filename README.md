# cpstransformer
An Implementation of Type Safe CPS Transformation 

## Build
```
$ ghc -o cpstransformer Main.hs
```
## Examples
```shell
$ ./cpstransformer
\(x::o) -> x
Parsed: \(x :: o) -> x
TypeCheck : OK
CPS : (\(x :: o -> (o -> o) -> o) -> end) (\(x :: o) -> \(k :: o -> o) -> k x) 
```
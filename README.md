# cpstransformer
An Implementation of Type Safe CPS Transformation 

## Build
```shell
$ ghc -o cpstransformer Main.hs
```
## Examples
```shell
$ ./cpstransformer
\(x::o) -> x
Parsed: \(x :: o) -> x
TypeCheck : OK
CPS : (\(x :: o -> (o -> o) -> o) -> end) (\(x :: o) -> \(k :: o -> o) -> k x) 
\(a :: o) -> (\(x :: o -> o) -> x a) (\(b :: o) -> b)
Parsed: \(a :: o) -> (\(x :: o -> o) -> x a) (\(b :: o) -> b)
TypeCheck : OK
CPS : (\(x :: o -> (o -> o) -> o) -> end) (\(a :: o) -> \(k :: o -> o) -> (\(f :: (o -> (o -> o) -> o) -> (o -> o) -> o) -> (\(x :: o -> (o -> o) -> o) -> f x k) (\(b :: o) -> \(k :: o -> o) -> k b)) (\(x :: o -> (o -> o) -> o) -> \(k :: o -> o) -> (\(f :: o -> (o -> o) -> o) -> (\(x :: o) -> f x k) a) x))
\(x :: o -> o) -> x x
Parsed: \(x :: o -> o) -> x x
TypeCheck : failed
```

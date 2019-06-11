# Algae

Clojure Computer Algebra System (CAS)

Solves 2-step equations of the type found [here](https://www.khanacademy.org/math/algebra/one-variable-linear-equations/alg1-two-steps-equations-intro/e/linear_equations_2?modal=1).

## Example usage:

```
~$ cd algae
~/algae$ clj
Clojure 1.10.1
user=> (require 'algae)
nil
user=> (in-ns 'algae)
#object[clojure.lang.Namespace 0x6d4f266 "algae"]
algae=> (solve "-11b+7=40")
"b=-3"
```
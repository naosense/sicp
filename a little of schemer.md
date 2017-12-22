Laws:
1. `car` is defined only for non-empty lists.
2. `cdr` is defined only for non-empty lists. The `cdr` of any non-empty list is always another list.
3. `cons` takes two arguments. The second argument to `cons` must be a list. The result is a list[^1].
4. `null?` is defined only for lists[^2].
5. `eq?` takes two arguments. Each must be a non-numeric atom[^3].

Commandments:
1. Always ask `null?` as the first question in expressing any function.
2. Use `cons` to build lists.
3. When building a list, describe the first typical element, and then `cons` it onto the natural recursion.
4. Always change at least one argument while recurring. It must be changed to be closer to termination.
  - When recurring on a list of atoms, lat, use `(cdr lat)`, test termination with `null?`. 
  - When recurring on a number, n, use `(sub1 n)`, test termination with `zero?`.
  - When recurring on a list of S-expressions, use `(car l)` and `(cdr l)` if neither `(null? l)` nor `(atom? (car l))` are true.
5. Questions to ask:
  - When recurring on a list of atoms, lat, ask two questions about it: `(null? lat)` and else.
  - When recurring on a number, n, ask two questions about it: `(zero? n)` and else. 
  - When recurring on a list of S-expressions, l, ask three questions about it: `(null? l)`, `(atom? (car l))`, and else.
6. When building a value with +, use 0 for the value of the terminating line. When building a value with x, use 1 for the value of the terminating line. When building a value with `cons`, use `()` for the value of the terminating line.
7. Simplify only after the function is correct.


[^1]: In practice, cons works for all value a and b, and (car (cons a b)) = a, (cdr (cons a b)) = b.
[^2]: In practice, (null? a) is false for everything, except the empty list.
[^3]: In practice, lists may be argument of eq?. Two lists are eq? if they are the same list.

# implementing typeclasses

typeclasses can be implemented using dict passing.

## Summary

**we will use the following terms describing the class system:**

- **method**: A primitive overloaded operator such as == will be called a method
- **class**: A group of related methods is packaged into a class, each class has a name which is used in the type language
- **data type**: we use the same sort of data types used by the ml type system.
- **instance**: An instance binds a data type to operations which implement the methods of a specified class for that type.

**main features of the system of typeclasses:**

- polymorphic: use of operators and functions is not restricted to values of any single type
- overloaded: the interpretation of class methods is determined by the types of its arguments
- extensible: the definition of class methods can be extended to include new datatypes

## Type inference

```haskell
class Eq a where
    (==) :: a -> a -> Bool

instance Eq Int where
    (==) = primEqInt

instance Eq a => Eq [a] where
    (==) [] [] = True
    (==) (x:xs) (y:ys) = x == y && xs == ys
    (==) _ _ = False

member :: Eq a => a -> [a] -> Bool
member x [] = False
member x (y:ys) = x == y || member x ys
member _ _ = False

```

is translated into:

```haskell
member :: (a -> a -> Bool) -> a -> [a] -> Bool
member eq x [] = False
member eq x (y:ys) = eq x y || member eq x ys
member eq _ _ = False

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eq [] [] = True
eqList eq (x:xs) (y:ys) = eq x y && eqList eq xs ys
eqList eq _ _ = False
```

In the general case, a class may have several methods, so it is sensible to parameterize the definitions of overloaded functions using dictionary values. When a dictionary contains overloaded functions, it will refernce further subdictionaries when constructed. In our implementation, this captured dictionary is stored bu partially applying eqList to just the eq argument when the dictionary containing eqList is created.

Each instance can be converted to a 4-tuple containing the data type, the class, the dictionary and the context associated with the instance.

for example, the instance declaration for listequality would create this dict:

> d-Eq-List = eqList

and the declaration itself would be represented by:

> (List, Eq, d-Eq-List, \[\[Eq\]\])

Note that the context uses a two dimension list, one list for each type argument.

We will seperate the issues of type inference, in which program expression is assinged a (possibly overloaded) type, and dictionary conversion, in which the program code is transformed to explicitly extract method functions from dictionaries. There are relatively minor changes that are needed to extend ML style type inference to support typeclasses:

- an addtional field is required in each uninstantiated type variable: the context, a set of classes
- when a type variable is instantiated, its class constraints must be passed on to the instantiated value. If this is another type variable, it's context is augmented, using set unionm by the context of instantiated variable.
- When a context is passed onto a type constructor, **context reduction** is required: if an instance is found linking the type constructor and the class, the context of the instance propagates to the type constructor arguments.
- When a `letrec` is typechecked all variables defined by the letrec share a common context

consider the unification of `Eq a => a` and `[Integer]`:

1. The type variable `a` is instantiated to `[Integer]`: `Eq [Interger] => [Integer]`
2. The instance declaration of [] for class Eq exists, do the context reduction: `Eq Integer => [Integer]`
3. The instance declaration of Int for class Eq exists, leaving only the resulting type `[Integer]`

pseudocode for context reduction:

```python
def instantiate_tyvar(tyvar, typ):
    tyvar.value = typ
    propagate_classes(tyvar.context, typ)

def propagate_classes(classes, typ):
    if is_type_var(typ):
        typ.context = classes.union(typ.context)
    else:
        for cls in classes:
            propagate_class_tycon(cls, typ)

def propagate_class_tycon(cls, typ):
    instance_contexts = find_instance_context(typ.tycon, cls)
    for class_set in instance_contexts:
        for arg in typ.tycon.args:
            propagate_classes(class_set, arg)
```

## Dictionary conversion

Dictionary conversion affects the generated code in two ways. First, overloaded definitions receive additional parameter variables to bind dictionaries. Second, a reference to an overloaded definition must be passed dictionaries. Each element of the context correspond to a dictionary passed into or received by an overloaded function. The ordering is arbitary as long as the same ordering is used consistently.

During the hindley milner type inference, types only stablize at generalization, so the appropriate dictionaries needed to resolve the overloading cannot be determined until the entire expression being generalized has already been walked over. To avoid a second pass over the code generalization, we will hold onto the necessary bits of unresolvable code using **placeholders**. A placeholder captures a type and an object to be resolved based on that type. During generalization, placeholders are replaced by the required type-dependent code.

### Inserting placeholders

There are three kind to place holders, class place holder `<C, t>`, method placeholder `<==, t>` and recursive call placeholder`<f, t>`.
This happens during the **typecheck portion** of the typechcker. placeholders are inserted when the type checker encounters:

- overloaded variable: rewritten as an application to placeholders. e.g. `(Num a, Text b) => a -> b` -> `(Num t1, Text t2) => t1 -> t2` -> `f <Num, t1> <Num, t2>`
- method functions: directly converted into placeholders. e.g. `(==)` method in the Eq class would yield `<(==), t1>`
- `letrec bound`: recursively defined variables cannot be converted until their type in known. References to such variables are simply replaced by a placeholder until the correct context has been determined.

### Inserting dictionary parameters

This occurs during the **generalization portion** of type inference. generalization gathers all uninstantiated type variables in the type of a definition and creates a new dictionary variable for every element of every context in these type variables:

- A lambda which binds the dictionaries is wrapped around the body of the definition.
- A parameter environment is created.

e.g. the inferred type of f is `(Num t1, Text t2) => t1 -> t2`, the definition of f is changed to `f = \d1 d2 -> f'` where f' is the original definition of f. This creates the following parameter environment: `[((Num, t1), d1), ((Text, t2), d2)]`

### Resolving placeholders

At generalization, place holders can be resolved. For placeholders associated with either methods or classes, the type associated with the placeholder determines how it will be resolved:

**the type is a type variable in the parameter environment**:

- A class placeholder is resolved to the dictionary parameter variable
- A method placeholder requires a selector function to be applied to the dictionary variable.

**the type has been instantiated to a type constructor**:

- A method placeholder: an instance declaration supplies the method itself.
- A class placeholder: an instance declaration supplies the dictionary variable. e.g. dEqInt`.
  Since dictionaries or methods themselves may be overloaded, the typechecker may need to recursively generate placeholders to resolve this addtional overloading.

**The type variable was bound in an outer type environment**:

- the processing of the placeholder must be derred to the outer declaration.

**None of the above conditions hold**:

- An ambiguity has been detected. The ambiguity may be resolved by some language specific mechanism or simply signal a type erorr.

For recursive calls, since any dictionaries passed into the inner recursive call remain unchanged, the need to pass dictionaries passed to the inner recursive call can be eliminated by using an inner entry point where the dictionaries have already been bound. e.g. `let f d x = f d (x + 1) + f d (x + 2)` -> `let f d x = f_inner (x + 1) + f_inner (x + 2) where f_inner = f d`.

### An example

---

#### 1. Initial Expression Tree (bottom-left diagram)

This tree shows the initial assignment of type variables and placeholders.

```ascii
  letrec f [: t2 -> t3]
  --------------------
  |
  = \x [: t2]     (lambda body has type [t3])
      |
      @ [t3]  ( (x + (f x)) )
     / \
    /   \
   @ [t5->t6]  ( (+ x) )                      @ [t9]  ( (f x) )
  / \                                        / \
 /   \                                      /   \
<+, t7> [: t4->t5->t6]    x [: t4]        <f, t1> [: t8->t9]    x [: t2]
(Placeholder for '+')                     (Placeholder for recursive 'f')

{Context: Num t6}  (Constraint from '+')
```

**Explanation:**

- The initial type of `f` is $t_2 \rightarrow t_3$.
- The type of `x` is $t_2$.
- `<+, t7>` is the placeholder for the plus operator, and its type is instantiated as $t_4 \rightarrow t_5 \rightarrow t_6$. This leads to the `Num t_6` constraint.
- `<f, t1>` is the placeholder for the recursive call to `f`, and its type is instantiated as $t_8 \rightarrow t_9$.

---

#### 2. Unified Expression Tree (top-right diagram)

After type unification, many type variables are determined to be the same. Here, $t_2$ is used to represent that unified type.

```ascii
  letrec f [: t2 -> t2]
  --------------------
  |
  = \x [: t2]     (lambda body has type [t2])
      |
      @ [t2]  ( (x + (f x)) )
     / \
    /   \
   @ [t2->t2]  ( (+ x) )                      @ [t2]  ( (f x) )
  / \                                        / \
 /   \                                      /   \
<+, t2->t2->t2>    x [: t2]                <f, t2->t2>    x [: t2]
(Placeholder for '+')                     (Placeholder for recursive 'f')

{Context: Num t2}  (Constraint updated)
```

**Explanation:**

- The type of `f` is unified to $t_2 \rightarrow t_2$.
- All related type variables ($t_3, t_4, t_5, t_6, t_8, t_9$) are unified to $t_2$.
- The type of the `+` placeholder becomes $t_2 \rightarrow t_2 \rightarrow t_2$.
- The type of the placeholder for the recursive call to `f` becomes $t_2 \rightarrow t_2$.
- The constraint is also updated to `Num t_2`.

---

#### 3. Final Expression Tree (bottom-right diagram)

This tree shows the result after type class constraints are transformed via the "dictionary passing" mechanism. The function `f` now additionally accepts a dictionary parameter `d`.

```ascii

  letrec f  (generalized type: forall a. Num a => a -> a)
  ----------------------------------------------------------
  |
  = \d (dictionary for 'Num a')
      |
      \x [: a] (argument of type 'a', where 'a' is a Num instance)
        |
        @ (Outer application: result is type 'a')
       / \
      /   \
     @ ( `((sel+ d) x)` )                        @ ( `((f d) x)` )
    / \                                         / \
   /   \                                       /   \
  @ ( `(sel+ d)` )      x [: a]               @ ( `(f d)` )      x [: a]
 / \                                         / \
sel+      d                                  f (recursive)  d
(select +                                    (pass dictionary
 from dict 'd')                               to recursive 'f')
```

**Explanation:**

- `f` is generalized and now implicitly accepts a dictionary `d` as its first parameter. This dictionary provides the implementation for the `Num` type class (e.g., the `+` operation).
- `sel+` is an operation that selects the concrete implementation of the `+` function from the dictionary `d`.
- `(@ sel+ d)` represents fetching the `+` function from dictionary `d`.
- When `f` is called recursively, the dictionary `d` also needs to be passed along: `(@ f d)`.
- The type of `x` is `a`, and there is a constraint `Num a` (satisfied by the dictionary `d`).

### Another example

#### 1. After Placeholder Insertion and Type Variable Instantiation

This tree shows the expression `g = \x -> print (x, length x)` after initial placeholders for overloaded functions (`print`, `length`) are inserted and fresh type variables are assigned.

```ascii
    let g =
      |
      t1 -> t2  (Type of g: Function from t1 to t2)
      |
      \x [: t1] (Lambda: x has type t1)
        |
        @ [: t2] (Application: body of lambda, type t2: print (x, length x))
       /   \
      /     \
<print,t5> [:t3->String]         2-tuple [: t3] (The pair (x, length x))
(Placeholder for 'print',         /      \
 its type is t3 -> String.         /        \
 t4 from diagram is String)       x [: t1]    @ [: t7] (Application: length x, type t7)
                                             /   \
                                            /     \
                                 <length,t_?> [:t6->t7]  x [: t1]
                                 (Placeholder for 'length')

Context: Text t3 (Constraint: type t3 must be an instance of Text)
```

**Explanation:**

- `g` is a function that takes an argument `x` of a fresh type `t1` and produces a result of type `t2`.
- The body of `g` is an application (`@`) of the `print` function.
- `print` is a placeholder (`<print,t5>`) whose type is instantiated as $t_3 \rightarrow \text{String}$. This means it takes an argument of type $t_3$ and returns a `String`. This also introduces the constraint `Text t3`, meaning $t_3$ must be a type that is an instance of the `Text` type class.
- The argument to `print` is a 2-tuple `(x, length x)`, which has type $t_3$.
  - The first element of the tuple is `x`, which has type $t_1$.
  - The second element is the result of `length x`. `length` is a placeholder whose type is instantiated as $t_6 \rightarrow t_7$. Since it's applied to `x` (type $t_1$), $t_6$ must be equal to $t_1$. The result `length x` has type $t_7$.

---

#### 2. After Unification, This Becomes:

After the type inference engine unifies types based on constraints and function applications:

```ascii
    let g =
      |
      [t_L] -> String (Type of g: Function from List of t_L to String)
      |
      \x [: [t_L]] (Lambda: x has type List of t_L)
        |
        @ [: String] (Application: body of lambda, type String)
       /   \
      /     \
<print, ([t_L],Int)->String>   2-tuple [: ([t_L],Int)] (The pair (x, length x))
(Placeholder for 'print',         /         \
 its type is now specific)        /           \
                                x [: [t_L]]     @ [: Int] (Application: length x, type Int)
                                               /      \
                                              /        \
                                 <length, [t_L]->Int>  x [: [t_L]]
                                 (Placeholder for 'length')

Context: Text ([t_L], Int) (Constraint: type ([t_L],Int) must be an instance of Text)
```

**Explanation:**

- The type of `x`, $t_1$, has been unified to be a list type, `[t_L]`, where `t_L` is some type. This often happens because `length` typically operates on lists.
- The type of `length x`, $t_7$, has been unified to `Int`. So, `length` has the type `[t_L] -> Int`.
- The type of the tuple `(x, length x)`, $t_3$, becomes `([t_L], Int)`.
- The `print` function is now known to take `([t_L], Int)` and return `String`.
- The overall type of `g` becomes `[t_L] -> String`.
- The constraint updates to `Text ([t_L], Int)`, meaning the tuple type `([t_L], Int)` must be an instance of `Text`.

---

#### 3. Dictionary Passing Transformation (Top-Right Tree)

This tree shows how `g` is transformed to explicitly pass dictionaries for the `Text` type class. The original image shows `let g = \d \x -> ...`. The structure `(@ P (@ A B))` means `P (A B)`.

```ascii
let g =
  |
  \d_param  (Lambda: takes a dictionary 'd_param')
    |
    \x      (Lambda: takes argument 'x')
      |
      @  ( Applying (print-tuple2 (d-Text-List d_param)) to (2-tuple x (length x)) )
     / \
    /   \
   @ (This node is the function (print-tuple2 (d-Text-List d_param)) )      2-tuple
  / \                                                                     /     \
 /   \                                                                   /       \
print-tuple2      @ (This node is the dictionary (d-Text-List d_param))  x      length
(The core func    / \                                                            |
for printing     /   \                                                           x
tuples)     d-Text-List  d_param (The dictionary bound by the outer \d)

```

**Explanation of the term `(print-tuple2 (d-Text-List d_param)) (2-tuple x (length x))`:**

- The function `g` is now defined to take an explicit dictionary parameter, `d_param`, followed by the original argument `x`.
- `print-tuple2`: This is likely the specific function used to implement `print` for tuples, as suggested by the instance `instance (Text a, Text b) => Text (a,b) where print = print-tuple2 ...`.
- `d-Text-List`: This identifier likely refers to a function or a component that, when given `d_param`, produces the dictionary needed for `Text [L]` (the type of `x`).
- `d_param`: This is the dictionary passed into `g`. Based on the structure, it's used by `d-Text-List`. It could be, for example, a dictionary for `Text Int` if `d-Text-List` is a function like `Dict(Text Int) -> Dict(Text [L])`. Or, `d_param` could be a more complex dictionary from which `d-Text-List` extracts a part.

The expression tree means:

1.  `d-Text-List` is applied to `d_param`. Let's call the result `dict_for_list`.
    This `dict_for_list` would be the dictionary satisfying the `Text [L]` constraint.
2.  `print-tuple2` is then applied to `dict_for_list`. Let's call the result `specialized_print_for_tuple_with_list_dict`.
    This implies `print-tuple2` is curried and takes the dictionary for the first part of the tuple (`Text [L]`). It would still need a dictionary for the second part (`Text Int`) if it were fully generic like `D_a -> D_b -> (a,b) -> String`.
    However, the diagram only shows one dictionary argument being constructed this way and passed to `print-tuple2`. This might mean:

    - `print-tuple2` is defined as `DictC1 -> (Arg1_Type, Arg2_Type) -> String`, and `(d-Text-List d_param)` fully satisfies `DictC1`.
    - Or, `(d-Text-List d_param)` is a dictionary that _itself_ contains the information for `Text Int` (e.g., if `L` is `Int`, then `d-Text-List` might produce `Dict(Text [Int])` using `d_param` which is `Dict(Text Int)`).

3.  Finally, the resulting function `specialized_print_for_tuple_with_list_dict` is applied to the actual tuple `(2-tuple x (length x))`.

**Simplified Interpretation (Common Pattern):**
Often, for an instance `(Text a, Text b) => Text (a,b) where print = print_tuple_func`, the function `print_tuple_func` would be of type `Dict(Text a) -> Dict(Text b) -> (a,b) -> String`.
In this case, `g` would take two dictionaries: `d_Text_L` (for `Text [L]`) and `d_Text_Int` (for `Text Int`).
`g = \d_Text_L \d_Text_Int \x -> ((print_tuple_func d_Text_L) d_Text_Int) (x, length x)`.
The tree diagram provided seems to use a convention where `(d-Text-List d_param)` constructs the necessary combined dictionary information for `print-tuple2`, or `print-tuple2` is less generic than assumed above. The structure `P (A B)` suggests `A` is a function applied to `B`, and `P` is applied to the result of that.

The `length x` part is simplified in this final tree, showing `length` applied to `x`. It's assumed that `length :: [a] -> Int` is either non-overloaded here or its dictionary passing is handled separately/implicitly.

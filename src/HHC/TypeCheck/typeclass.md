# implementing typeclasses

typeclasses can be implemented by using dict passing.

**we will use the following terms describing the class system:**

- **method**: A primitive overloaded operator such as == will be called a method
- **class**: A group of related methods is packaged into a class, each class has a name which is used in the type language
- **data type**: we use the same sort of data types used by the ml type system.
- **instance**: An instance binds a data type to operations which implement the methods of a specified class for that type.

**main features of the system of typeclasses:**

- polymorphic: use of operators and functions is not restricted to values of any single type
- overloaded: the interpretation of class methods is determined by the types of its arguments
- extensible: the definition of class methods can be extended to include new datatypes

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

and the diclaration itself would be represented by:
> (List, Eq, d-Eq-List, \[\[Eq\]\])

We will seperate the issues of type inference, in which program expression is assinged a (possibly overloaded) type, and dictionary conversion, in which the program code is transformed to explicitly extract method functions from dictionaries. There are relatively minor changes that are needed to extend ML style type inference to support typeclasses:
- an addtional field is required in each uninstantiated type variable: the context, a set of classes
- when a type variable is instantiated, its class constraints must be passed on to the instantiated value. If this is another type variable, it's context is augmented, using set unionm by the context of instantiated variable.
- When a context is passed onto a type constructor, **context reduction** is required.


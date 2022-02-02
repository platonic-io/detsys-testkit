## Testing Parsing

Assume you have a parsing function for some `Syntax` type that you want to test. A normal way of testing this is to have a `genSyn` to generate a random abstract syntax node. 

```haskell
type Syntax
parse :: String -> Maybe Syntax

genSyn :: Gen Syntax
genSyn = ...
```

Then you would have some sort of `prettyPrint` function that you could test if one could do a round-trip with.

```haskell
prettyPrint :: Syntax -> String
prettyPrint = ...

prop_prettyRoundTrip :: Property
prop_prettyRoundTrip =
  forAll genSyn $ \ syn ->
  parse (prettyPrint syn) === Just syn
```

But one problem with this is that there are many concrete syntaxes that all parse to the same `Syntax`, and `prettyPrint` will only pick one of them. For example there are several places where whitespace could be insterted (or not inserted for), precedence levels could maybe not require parenthesis, maybe there are syntactic sugar that could be used, or alternative syntax `forall` vs `∀` etc. Rather than writing one `prettyPrint` that picks any of these, we can write a generator that would 

```haskell
genSpaces :: Gen String
genSpaces = ..

combine :: [Gen String] -> Gen String
combine = fmap mconcat . sequence

genMaybeParen :: Gen String -> Gen String
genMaybeParen g = oneOf 
  [ g
  , combine
     [ pure "("
     , genSpaces
     , g
     , genSpaces
     , pure ")"
     ]
  ]

genConcrete :: Syntax -> Gen String
genConcrete (Forall var body) = combine
  [ elements ["forall", "∀"]
  , genSpaces
  , pure var
  , genSpaces
  , pure "."
  , genSpaces
  , genMaybeParen $ genConcrete body
  ]
genConcrete = ..

prop :: Property
prop = 
  forAll genSyn $ \ syn -> 
  forAll (genConcrete syn) $ \ text ->
  parse text === Just syn

```

With this approach we will randomly pick different concrete syntaxes for the abstract syntax so we get a higher coverage.

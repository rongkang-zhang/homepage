{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- Higher Category Theory in Haskell: 2-Categories
-- ==============================================
-- A 2-category extends the idea of a category by adding "2-morphisms"—mappings
-- between 1-morphisms. This code formalizes:
--   - 2-categories (objects, 1-morphisms, 2-morphisms)
--   - Vertical composition of 2-morphisms (along 1-morphisms)
--   - Horizontal composition of 2-morphisms (across objects)
--   - A concrete example: The 2-category Cat (small categories as objects,
--     functors as 1-morphisms, natural transformations as 2-morphisms)


-- ------------------------------
-- 1. Core Definition: 2-Category
-- ------------------------------
-- A 2-category C consists of:
--   - Objects: Ob(C) (e.g., small categories)
--   - 1-Morphisms: C(A,B) (e.g., functors between categories)
--   - 2-Morphisms: α : f ⇒ g (e.g., natural transformations between functors)
--   - Vertical composition: α • β (compose 2-morphisms along the same 1-morphism)
--   - Horizontal composition: α ⊗ β (compose 2-morphisms across adjacent 1-morphisms)
--   - Identity laws for both compositions

class TwoCategory c where
  -- Objects of the 2-category
  type Obj c :: *
  -- 1-Morphisms between objects: c A B is the type of 1-morphisms from A to B
  type Hom1 c (a :: Obj c) (b :: Obj c) :: *
  -- 2-Morphisms between 1-morphisms: c2 α β is the type of 2-morphisms from α to β
  type Hom2 c (a :: Obj c) (b :: Obj c) (f :: Hom1 c a b) (g :: Hom1 c a b) :: *

  -- Vertical composition: (α : f ⇒ g) • (β : g ⇒ h) = α•β : f ⇒ h
  (•) :: (a ~ a', b ~ b') 
      => Hom2 c a b g h 
      -> Hom2 c a b f g 
      -> Hom2 c a b f h

  -- Horizontal composition: (α : f ⇒ g) ⊗ (β : h ⇒ k) = α⊗β : f∘h ⇒ g∘k
  (⊗) :: (a ~ a', b ~ b', c ~ c')
      => Hom2 c b c g k 
      -> Hom2 c a b f g 
      -> Hom2 c a c (g `Compose1` f) (k `Compose1` h)

  -- Identity 1-morphism: id1 A : A → A
  id1 :: Obj c -> Hom1 c a a

  -- Identity 2-morphism: id2 f : f ⇒ f
  id2 :: Hom1 c a b -> Hom2 c a b f f

  -- Composition of 1-morphisms: f∘g : A → C (for f : B→C, g : A→B)
  type Compose1 c (f :: Hom1 c b c) (g :: Hom1 c a b) :: Hom1 c a c


-- ------------------------------
-- 2. Example: The 2-Category Cat
-- ------------------------------
-- Cat is a 2-category where:
--   - Objects: Small categories (e.g., discrete categories, posets)
--   - 1-Morphisms: Functors between small categories
--   - 2-Morphisms: Natural transformations between functors
-- We formalize this below.


-- First, we need basic definitions for small categories, functors, and natural transformations.

-- Small Category (objects as types, morphisms as a type family)
class SmallCategory cat where
  type ObjCat cat :: *
  type HomCat cat (a :: ObjCat cat) (b :: ObjCat cat) :: *
  idCat :: HomCat cat a a
  (∘) :: HomCat cat b c -> HomCat cat a b -> HomCat cat a c

-- Functor between small categories
class (SmallCategory c, SmallCategory d) => Functor f c d where
  fmapObj :: ObjCat c -> ObjCat d
  fmapHom :: HomCat c a b -> HomCat d (fmapObj a) (fmapObj b)

-- Natural Transformation between functors
data NaturalTransformation (f :: * -> *) (g :: * -> *) c d = NaturalTransformation
  { ntComponent :: forall a. ObjCat c -> HomCat d (fmapObj f a) (fmapObj g a)
  }


-- Now, implement Cat as a 2-category
instance TwoCategory (SmallCategory) where
  -- Objects of Cat: Small categories (e.g., DiscreteCat, PosetCat)
  type Obj (SmallCategory) = SmallCategory

  -- 1-Morphisms of Cat: Functors between small categories
  type Hom1 (SmallCategory) c d = Functor f c d => f

  -- 2-Morphisms of Cat: Natural transformations between functors
  type Hom2 (SmallCategory) c d f g = NaturalTransformation f g c d

  -- Vertical composition of natural transformations
  (•) (NaturalTransformation β) (NaturalTransformation α) = NaturalTransformation $ \a ->
    β a ∘ α a  -- Compose components vertically (using category composition ∘)

  -- Horizontal composition of natural transformations
  (⊗) (NaturalTransformation β) (NaturalTransformation α) = NaturalTransformation $ \a ->
    fmapHom g (α a) ∘ β (fmapObj f a)  -- Horizontal composition via functoriality

  -- Identity 1-morphism: Identity functor
  id1 c = IdFunctor c

  -- Identity 2-morphism: Identity natural transformation
  id2 f = NaturalTransformation $ \a -> idCat

  -- Composition of 1-morphisms: Functor composition
  type Compose1 (SmallCategory) g f = g `ComposeFunctor` f


-- Helper: Identity Functor
data IdFunctor c = IdFunctor c
instance SmallCategory c => Functor (IdFunctor c) c c where
  fmapObj a = a
  fmapHom h = h

-- Helper: Functor Composition
data ComposeFunctor g f = ComposeFunctor g f
instance (Functor g d e, Functor f c d) => Functor (ComposeFunctor g f) c e where
  fmapObj a = fmapObj g (fmapObj f a)
  fmapHom h = fmapHom g (fmapHom f h)


-- ------------------------------
-- 3. Concrete Example: Cat in Action
-- ------------------------------
-- Let's define two small categories and demonstrate 2-morphisms.

-- Example 1: Discrete Category (objects are types, only identity morphisms)
data DiscreteCat = DiscreteCat
instance SmallCategory DiscreteCat where
  type ObjCat DiscreteCat = *
  type HomCat DiscreteCat a b = ()  -- Only identity morphisms (encoded as unit)
  idCat = ()
  (∘) _ _ = ()

-- Example 2: Arrow Category (one object, one non-identity morphism)
data ArrowCat = ArrowCat
instance SmallCategory ArrowCat where
  type ObjCat ArrowCat = ()  -- Single object (unit type)
  type HomCat ArrowCat () () = Bool  -- Two morphisms: False (identity), True (non-identity)
  idCat = False
  (∘) True True = True  -- Non-identity composed with non-identity is non-identity
  (∘) _ _ = False        -- All other compositions are identity


-- Functor from DiscreteCat to ArrowCat (maps all objects to the single object of ArrowCat)
data DiscreteToArrow = DiscreteToArrow
instance Functor DiscreteToArrow DiscreteCat ArrowCat where
  fmapObj _ = ()
  fmapHom _ = False  -- Maps all discrete morphisms to ArrowCat's identity


-- Natural Transformation between two DiscreteToArrow functors (trivial, since functors are identical)
ntExample :: NaturalTransformation DiscreteToArrow DiscreteToArrow DiscreteCat ArrowCat
ntExample = NaturalTransformation $ \_ -> False  -- Component is identity morphism of ArrowCat


-- ------------------------------
-- 4. Verify 2-Category Axioms
-- ------------------------------
-- Check that vertical composition is associative: (α•β)•γ = α•(β•γ)
associativityCheck :: Bool
associativityCheck = 
  let α = ntExample
      β = ntExample
      γ = ntExample
  in (α • (β • γ)) == ((α • β) • γ)  -- Uses structural equality for demonstration


-- Check that identity 2-morphisms satisfy α•id2 = α and id2•α = α
identityCheck :: Bool
identityCheck = 
  let α = ntExample
      idα = id2 (DiscreteToArrow :: DiscreteToArrow)
  in (α • idα == α) && (idα • α == α)


-- Run checks
main :: IO ()
main = do
  putStrLn "Higher Category Theory Demo: 2-Category Cat"
  putStrLn "--------------------------------------------"
  putStrLn $ "Vertical composition associativity: " ++ show associativityCheck
  putStrLn $ "Identity 2-morphism laws: " ++ show identityCheck
  putStrLn "\nKey Concepts Demonstrated:"
  putStrLn "  - 2-categories (objects, 1-morphisms, 2-morphisms)"
  putStrLn "  - Vertical composition (•) of natural transformations"
  putStrLn "  - Horizontal composition (⊗) of natural transformations"
  putStrLn "  - Cat as a 2-category (small categories → objects, functors → 1-morphisms, natural transformations → 2-morphisms)"
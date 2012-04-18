{-
 - This module describes the Core language, which acts as an intermediate
 - enhanced lambda calculus. The Core language here is heavily influenced by:
 -
 -  * Simon Peyton Jones, "Implementing Functional Languages, a tutorial"
 -    http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/
 -}

module Tamien.Core.Language where

-- | All expressions supported in the Core language! The parameter 'a' is
--   is used in the binding position of expressions.
data Expr a
    = Var Name              -- Variables
    | Num Int               -- Numbers
    | Constr                -- Constructors
        Int                 --      tag
        Int                 --      arity
    | App                   -- Applications
        (Expr a)            --      function to apply
        (Expr a)            --      function argument
    | Let                   -- Let(rec) expressions
        IsRec               --      recursive flag
        [(a, Expr a)]       --      definitions (i.e. binder / expression pairs)
        (Expr a)            --      expression body
    | Case                  -- Case expressions
        (Expr a)            --      expression to match
        [Alt a]             --      alternatives
    | Lam                   -- Lambda abstractions!
        [a]                 --      bound variables
        (Expr a)            --      abstraction body
      deriving Eq

-- | Unique identifier
type Name = String
type CoreExpr = Expr Name

-- | Flag that indicates if a let expression is recursive
type IsRec = Bool

-- | Indicates that a let expression is recursive
recursive :: IsRec
recursive = True

-- | Indicates that a let expression is not recursive
nonRecursive :: IsRec
nonRecursive = False

-- | Extracts the binders from a list of definitions
bindersOf :: [(a, b)] -> [a]
bindersOf = map fst

-- | Extracts the right hand sides from a list of defitions
rhssOf :: [(a, b)] -> [b]
rhssOf = map snd

-- | Alternatives in a case statement
type Alt a = ( Int      -- tag
             , [a]      -- bound variables
             , Expr a   -- alternative body
             )
type CoreAlt = Alt Name

-- | True if the expression has no internal structure
isAtomic :: Expr a -> Bool
isAtomic (Var _) = True
isAtomic (Num _) = True
isAtomic _       = False


-- | A supercombinator definion
type ScDefn a = ( Name      -- Name of the supercombinator
                , [a]       -- Arguments
                , Expr a    -- Body
                )
type CoreScDefn = ScDefn Name

-- | A program is composed of supercombinator definitions
type Program a = [ScDefn a]
type CoreProgram = Program Name

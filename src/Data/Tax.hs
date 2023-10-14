-- This file is part of hs-tax
-- Copyright (C) 2018  Fraser Tweedale
--
-- hs-tax is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

This library provides combinators for constructing taxes.  It is
based on the <https://hackage.haskell.org/package/dollaridoos dollaridoos>
library.

-}
module Data.Tax
  (
  -- * Overview
  -- $doc

  -- * API
    Tax(..)
  , MoneyTax
  , lump
  , flat
  , threshold
  , threshold'
  , thresholds
  , above
  , above'
  , marginal
  , lesserOf
  , greaterOf
  , limit
  , effective

  -- * Re-exports
  , module Data.Money
  , Semigroup(..)
  , Monoid(..)
  , Profunctor(..)
  ) where

import Data.Profunctor (Profunctor(..))
import Data.Semigroup (Semigroup(..))

import Data.Money

{- $doc

The most basic tax is a flat rate tax:

@
businessTax = 'flat' 0.3
@

To compute the tax, use 'getTax':

@
λ> 'getTax' businessTax (Money 1000000)
$300000.0
@

Taxes form a semigroup (sum of tax outputs) and monoid:

@
λ> getTax (flat 0.1 <> flat 0.2) (Money 10)
$3.0
λ> getTax mempty (Money 10)
$0
@

Progressive taxes can be constructed using the 'above' combinator,
which taxes the amount above a given threshold at a flat rate.

@
individualIncomeTax =
     'above' (Money 18200 ) (0.19  - 0     )
  <> 'above' (Money 45000 ) (0.325 - 0.19  )
  <> 'above' (Money 120000) (0.37  - 0.325 )
  <> 'above' (Money 180000) (0.45 - 0.37   )
@

The 'marginal' function provides a shorthand for the above.

@
individualIncomeTax = 'marginal'
  [ ( Money 18200,  0.19  - 0     )
  , ( Money 45000,  0.325 - 0.19  )
  , ( Money 120000, 0.37  - 0.325 )
  , ( Money 180000, 0.45  - 0.37  ) ]
@

Taxes can be negative.  For example, the 'lump', 'above' and 'limit'
combinators can be used to construct a low-income tax offset that
starts at $445 and reduces at a rate of 1.5c per dollar earned over
$37000:

@
lowIncomeTaxOffset =
  'limit' mempty
  ('lump' (Money (-445)) <> 'above' (Money 37000) 0.015)
@

The 'threshold' combinator applies a tax to the full input amount,
if it exceeds the threshold.

@
medicareLevySurcharge =
     'threshold' (review money 90000 ) 0.0100
  <> 'threshold' (review money 105000) 0.0025
  <> 'threshold' (review money 140000) 0.0025
@

Some taxes have "shade-in" where the amount above some threshold is
taxed at a higher rate to "catch up" to some lower flat rate.  The
'above' and 'lesserOf' combinators can be used to construct this
tax:

@
medicareLevy l = 'lesserOf' ('above' l 0.1) ('flat' 0.02)
@

Although some of the combinators deal directory with 'Money', a
'Tax' can be defined for other types.  For example, you can tax a
person a certain number of days labour, based on their age.

@
data Sex = M | F
newtype Years = Years Int
newtype Days = Days Int
data Person = Person Years Sex

corvée :: Tax Person Days
corvée = Tax f
  where
  f (Person (Years age) sex) = Days $ if age >= 18 && age <= maxAge sex then 10 else 0
  maxAge sex = case sex of M -> 45 ; F -> 35
@

-}

-- | A function from an amount or value subject to taxation, to
-- the amount or value of tax due.
--
-- Taxes form a semigroup where the tax payable is the
-- sum of tax payable of consituent taxes.
--
-- Taxes form a monoid where the identity is a tax of 0%
--
-- Taxes are a profunctor, making it trivial to perform simple
-- transformations of the input and/or output (e.g. rounding
-- down to whole dollars).
--
newtype Tax a b = Tax { getTax :: a -> b }
  deriving (Semigroup, Monoid, Functor, Profunctor)

-- | Convenience synonym for working with 'Money'
type MoneyTax a = Tax (Money a) (Money a)

-- | Tax the amount exceeding the threshold at a flat rate.
--
above :: (Num a, Ord a) => Money a -> a -> Tax (Money a) (Money a)
above l = above' l . flat

-- | Tax the amount exceeding the threshold
above' :: (Num b, Ord b) => Money b -> Tax (Money b) a -> Tax (Money b) a
above' l = lmap (\x -> max (x $-$ l) mempty)

-- | Convert a @[(threshold, rate)]@ into a progressive tax.
-- The rates are /cumulative/, i.e. the top marginal rate is the
-- sum of the rates that apply for a given input.
--
marginal :: (Num a, Ord a) => [(Money a, a)] -> Tax (Money a) (Money a)
marginal = foldMap (uncurry above)


-- | A lump-sum tax; a fixed value (ignores input).
--
lump :: a -> Tax b a
lump = Tax . const

-- | Construct a flat rate tax with no threshold
flat :: (Num a) => a -> Tax (Money a) (Money a)
flat = Tax . (*$)

-- | Tax full amount at flat rate if input >= threshold
threshold :: (Num a, Ord a) => Money a -> a -> Tax (Money a) (Money a)
threshold l = threshold' l . flat

-- | Levy the tax if input >= threshold, otherwise don't
threshold' :: (Ord b, Monoid a) => b -> Tax b a -> Tax b a
threshold' l tax = Tax (\x -> if x >= l then getTax tax x else mempty)

-- | Convert a @[(threshold, rate)]@ into a flat tax whose rate is
-- the sum of the rates that apply for a given input.  The rates
-- are /cumulative/.  For example, if you want to tax people earning
-- >$30,000 20%, and people earning >$50,000 30%, you only tax an
-- extra 10% at 50000:
--
-- @
-- tax = thresholds [(30000, 0.2), (50000, 0.1)]
-- @
--
thresholds :: (Num a, Ord a) => [(Money a, a)] -> Tax (Money a) (Money a)
thresholds = foldMap (uncurry threshold)

-- | Levy the lesser of two taxes
lesserOf :: (Ord a) => Tax b a -> Tax b a -> Tax b a
lesserOf t1 t2 = Tax (\x -> min (getTax t1 x) (getTax t2 x))

-- | Levy the greater of two taxes
greaterOf :: (Ord a) => Tax b a -> Tax b a -> Tax b a
greaterOf t1 t2 = Tax (\x -> max (getTax t1 x) (getTax t2 x))

-- | Limit the tax payable to the given amount
--
-- This could be used e.g. to limit a compulsory loan
-- repayment to the balance of the loan, or ensure a
-- (negative) tax offset does not become a (positive) tax.
--
limit :: (Ord a) => a -> Tax b a -> Tax b a
limit = lesserOf . lump

-- | Given a tax and an amount construct the effective flat tax rate
--
effective
  :: (Fractional a)
  => Money a -> Tax (Money a) (Money a) -> Tax (Money a) (Money a)
effective x tax = flat (getTax tax x $/$ x)

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

{-|

This library provides combinators for constructing taxes.  It is
based on the <https://hackage.haskell.org/package/dollaridoos dollaridoos>
library.

The most basic tax is a flat rate tax:

@
businessTax = 'flat' 0.3
@

To compute the tax, use 'getTax':

@
λ> 'getTax' businessTax (review money 1000000)
$300000.0
@

Taxes form a semigroup (sum of tax outputs) and monoid:

@
λ> getTax (flat 0.1 <> flat 0.2) (review money 10)
$3.0
λ> getTax mempty (review money 10)
$0
@

Marginal tax rates can be constructed using the 'above' combinator,
which taxes the amount above a given threshold at a flat rate.

@
individualIncomeTax =
  'above' (review money 18200) 0.19
  <> 'above' (review money 37000) (0.325 - 0.19)
  <> 'above' (review money 87000) (0.37 - 0.325)
  <> 'above' (review money 180000) (0.45 - 0.37)
@

Taxes can be negative.  For exmaple, the 'lump', 'above' and 'limit'
combinators can be used to construct a low-income tax offset that
starts at $445 and reduces at a rate of 1.5c per dollar earned over
$37000:

@
lowIncomeTaxOffset =
  'limit' mempty
  ('lump' (review money (-445)) <> 'above' (review money 37000) 0.015)
@

The 'threshold' combinator applies a tax to the full input amount,
if it exceeds the threshold.  Some taxes have "shade-in" where the
amount above the threshold is taxed at a higher rate to "catch up"
to some lower flat rate.  The 'threshold'' and 'lesserOf'
combinators can be used to construct this tax:

@
medicareLevy =
  'threshold'' l ('lesserOf' ('above' l 0.1) ('flat' 0.02))
    where l = review money 21656
@

-}
module Data.Tax
  (
  -- * Constructing taxes
    Tax(..)
  , lump
  , flat
  , threshold
  , threshold'
  , above
  , above'
  , lesserOf
  , greaterOf
  , limit
  , effective

  -- * Miscellanea
  , Semigroup(..)
  , Monoid(..)
  , module Data.Money
  ) where

import Data.Monoid (Monoid(..))
import Data.Profunctor (Profunctor(..))
import Data.Semigroup (Semigroup(..))

import Data.Money

-- | A function from gross income to tax payable.
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
newtype Tax b a = Tax { getTax :: Money b -> Money a }

instance Num a => Semigroup (Tax b a) where
  Tax f <> Tax g = Tax (\x -> f x <> g x)

instance Num a => Monoid (Tax b a) where
  mempty = lump mempty
  mappend = (<>)

instance Functor (Tax b) where
  fmap f a = Tax (fmap f . getTax a)

instance Profunctor Tax where
  rmap = fmap
  lmap f a = Tax (getTax a . fmap f)

-- | Tax the amount exceeding the threshold at a flat rate.
--
above :: (Num a, Ord a) => Money a -> a -> Tax a a
above l = above' l . flat

-- | Tax the amount exceeding the threshold
above' :: (Num a, Num b, Ord b) => Money b -> Tax b a -> Tax b a
above' l tax = Tax (\x -> getTax tax (max (x $-$ l) mempty))

-- | A lump-sum tax; a fixed amount, not affected by the size of the input
--
lump :: Money a -> Tax b a
lump = Tax . const

-- | Construct a flat rate tax with no threshold
flat :: (Num a) => a -> Tax a a
flat = Tax . (*$)

-- | Tax full amount at flat rate if input >= threshold
threshold :: (Num a, Ord a) => Money a -> a -> Tax a a
threshold l = threshold' l . flat

-- | Levy the tax if input >= threshold, otherwise don't
threshold' :: (Num a, Ord b) => Money b -> Tax b a -> Tax b a
threshold' l tax = Tax (\x -> if x >= l then getTax tax x else mempty)

-- | Levy the lesser of two taxes
lesserOf :: (Ord a) => Tax b a -> Tax b a -> Tax b a
lesserOf t1 t2 = Tax (\x -> min (getTax t1 x) (getTax t2 x))

-- | Levy the greater of two taxes
greaterOf :: (Ord a) => Tax b a -> Tax b a -> Tax b a
greaterOf t1 t2 = Tax (\x -> max (getTax t1 x) (getTax t2 x))

-- | Limit the tax payable to the given amount
--
-- This could be used e.g. for limiting a compulsory loan
-- repayment to the balance of the loan, or ensuring a
-- (negative) tax offset does not become a (positive) tax.
--
limit :: (Ord a) => Money a -> Tax b a -> Tax b a
limit = lesserOf . lump

-- | Given a tax and an amount construct the effective flat tax rate
--
effective :: (Fractional a) => Money a -> Tax a a -> Tax a a
effective x tax = flat (getTax tax x $/$ x)

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
individualIncomeTax :: (Fractional a, Ord a) => Tax a
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
medicareLevy :: (Fractional a, Ord a) => Tax a
medicareLevy = 'threshold'' l ('lesserOf' ('above' l 0.1) ('flat' 0.02))
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
  , adjust
  , effective

  -- * Miscellanea
  , Semigroup(..)
  , Monoid(..)
  , module Data.Money
  ) where

import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))

import Data.Money

-- | A function from gross income to tax payable.
--
-- Taxes form a semigroup where the tax payable is the
-- sum of tax payable of consituent taxes.
--
-- Taxes form a monoid where the identity is a tax of 0%
--
newtype Tax a = Tax { getTax :: Money a -> Money a }

instance Num a => Semigroup (Tax a) where
  Tax f <> Tax g = Tax (\x -> f x <> g x)

instance Num a => Monoid (Tax a) where
  mempty = lump mempty
  mappend = (<>)

-- | Tax the amount exceeding the threshold at a flat rate.
--
-- You can use @above@ to construct marginal taxes:
--
-- @
-- marginal =
--   above 18200 0.19
--   <> above 37000 (0.325 - 0.19)
--   <> above 87000 (0.37 - 0.325)
--   <> above 180000 (0.45 - 0.37)
-- @
--
above :: (Num a, Ord a) => Money a -> a -> Tax a
above l = above' l . flat

-- | Tax the amount exceeding the threshold
above' :: (Num a, Ord a) => Money a -> Tax a -> Tax a
above' l tax = Tax (\x -> getTax tax (max (x $-$ l) mempty))

-- | A lump-sum tax; a fixed amount, not affected by the size of the input
--
lump :: Money a -> Tax a
lump = Tax . const

-- | Construct a flat rate tax with no threshold
flat :: (Num a) => a -> Tax a
flat = Tax . (*$)

-- | Tax full amount at flat rate if input >= threshold
threshold :: (Num a, Ord a) => Money a -> a -> Tax a
threshold l = threshold' l . flat

-- | Levy the tax if input >= threshold, otherwise don't
threshold' :: (Num a, Ord a) => Money a -> Tax a -> Tax a
threshold' l tax = Tax (\x -> if x >= l then getTax tax x else mempty)

-- | Levy the lesser of two taxes
lesserOf :: (Ord a) => Tax a -> Tax a -> Tax a
lesserOf t1 t2 = Tax (\x -> min (getTax t1 x) (getTax t2 x))

-- | Levy the greater of two taxes
greaterOf :: (Ord a) => Tax a -> Tax a -> Tax a
greaterOf t1 t2 = Tax (\x -> max (getTax t1 x) (getTax t2 x))

-- | Limit the tax payable to the given amount
--
-- This could be used e.g. for limiting a compulsory loan
-- repayment to the balance of the loan, or ensuring a
-- (negative) tax offset does not become a (positive) tax.
--
limit :: (Ord a) => Money a -> Tax a -> Tax a
limit = lesserOf . lump

-- | Multiply a tax by the given ratio
adjust :: (Num a) => a -> Tax a -> Tax a
adjust r tax = Tax (\x -> r *$ getTax tax x)

-- | Given a tax and an amount construct the effective flat tax rate
--
effective :: (Fractional a) => Money a -> Tax a -> Tax a
effective x tax = flat (getTax tax x $/$ x)

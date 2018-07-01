module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- head :: List Entry -> Maybe Entry
-- filterEntry :: Entry -> Boolean
-- filter :: (Entry -> Boolean) -> List Entry -> List Entry

findEntryByStreetAddress :: String -> AddressBook -> Maybe Entry
findEntryByStreetAddress street = head <<< filter filterEntryByAddress
  where
    filterEntryByAddress :: Entry -> Boolean
    filterEntryByAddress entry = entry.address.street == street

doesNameAppearInAddressBook :: String -> AddressBook -> Boolean
doesNameAppearInAddressBook name = null <<< filter filterName
  where
    filterName :: Entry -> Boolean
    filterName entry = (entry.firstName == name || entry.lastName == name)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates addressBook = nubBy (\e1 e2 -> e1.firstName == e2.firstName && e1.lastName == e2.lastName) addressBook

-- > removeDuplicates (fromFoldable [{firstName: "Omar", lastName: "Mefire", address: {street:"LA", city:"LA", state:"CA"}}, {firstName: "Omar", lastName: "Mefire", address: {street:"LA", city:"LA", state:"CA"}}])
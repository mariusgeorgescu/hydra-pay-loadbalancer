# Issues and Fixes

This document tracks significant issues encountered during development and their solutions.

## Issue: Servant ReqBody '[JSON] Encoding Failure with Nested Vector/List of Dynamic Values

### Problem
When using `Vector (Vector Value)` or `[[Value]]` for the `amount` field in `DepositSchema` and `PayMerchantSchema`, Servant's client generation failed to correctly serialize the request body. Even though Aeson could encode the types correctly (verified by direct `encode` calls), Servant would send an empty or malformed body, resulting in server errors like:

```
"received": "undefined"
"path": ["user_address"]
"message": "Required"
```

### Root Cause
Servant's `ReqBody '[JSON]` combinator has limitations when dealing with:
- Nested structures (Vector of Vector, or list of lists)
- Dynamic types (`Value` type from Aeson)
- The combination of both

The issue occurred even when:
- Custom `ToJSON` instances were provided
- Direct Aeson encoding worked correctly
- The JSON structure was verified to be correct

### Solution
Changed the amount type from `Vector (Vector Value)` / `[[Value]]` to `[(Text, Integer)]` (a list of tuples).

**Before:**
```haskell
data DepositSchema = DepositSchema
  { depositAmount :: Vector (Vector Value)  -- or [[Value]]
  ...
}
```

**After:**
```haskell
data DepositSchema = DepositSchema
  { depositAmount :: [(Text, Integer)]  -- List of (asset unit, amount) tuples
  ...
}
```

The `ToJSON` instance converts `[(Text, Integer)]` to the required JSON format:
```json
"amount": [["lovelace", 100000000]]
```

### Benefits
1. **Type Safety**: Concrete `(Text, Integer)` tuples instead of dynamic `Value` types
2. **Servant Compatibility**: Servant handles simple types like lists of tuples correctly
3. **Cleaner API**: More intuitive to work with `[(Text, Integer)]` than nested `Value` arrays
4. **Correct Serialization**: Still produces the exact JSON format required by the API

### Files Changed
- `src/HydraPay/API/Types.hs`: Changed `depositAmount` and `payMerchantAmount` types
- `app/TUI.hs`: Updated to construct `[(Text, Integer)]` instead of `[[Value]]`
- `hydra-pay-loadbalancer.cabal`: Added `scientific` dependency for number conversion

### Date
November 2025


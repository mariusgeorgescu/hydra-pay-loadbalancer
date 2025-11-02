# Issues and Fixes

This document tracks significant issues encountered during development and their solutions.

## Issue: Server Rejects Requests with `Content-Type: application/json;charset=utf-8`

### Problem
The Hydra Payments API server was rejecting deposit requests with error messages indicating "undefined" fields:
```
"received": "undefined"
"path": ["user_address"]
"message": "Required"
```

Even though:
- The request body was correctly formatted JSON
- All required fields were present
- Direct encoding with Aeson produced correct JSON

### Root Cause
**The server's JSON parser rejects `Content-Type: application/json;charset=utf-8` and requires exactly `Content-Type: application/json`.**

This was verified via curl:
- **With charset:** `curl --header 'content-type: application/json;charset=utf-8'` → Server returns "undefined" for all fields
- **Without charset:** `curl --header 'content-type: application/json'` → Server parses JSON correctly

Servant automatically adds `charset=utf-8` to the Content-Type header for JSON requests, which the server cannot handle.

### Solution
Implemented a `managerModifyRequest` hook in `runHydraClient` that:
1. Intercepts all HTTP requests before they're sent
2. Detects `Content-Type` header with `charset=utf-8` suffix
3. Removes the charset suffix, leaving only `application/json`
4. Returns the modified request with corrected headers

**Implementation:**
```haskell
let captureHook req = do
      let headers = HTTP.requestHeaders req
          fixedHeaders = map (\(k, v) -> 
            -- Detect Content-Type header and remove charset=utf-8
            let kStr = show k
                kName = if "Content-Type" `isInfixOf` kStr || "content-type" `isInfixOf` (map toLower kStr)
                          then "Content-Type"
                          else kStr
                kBytes = BS8.pack kName
                kLower = BS8.map toLower kBytes
                contentTypeLower = BS8.map toLower (BS8.pack "Content-Type")
                isContentType = kLower == contentTypeLower
                hasCharset = "charset=utf-8" `BS.isInfixOf` v
            in if isContentType && hasCharset
              then (k, BS8.pack "application/json")
              else (k, v)) headers
      return req { HTTP.requestHeaders = fixedHeaders }
```

### Verification
**Before Fix:**
- Headers: `Content-Type: application/json;charset=utf-8`
- Server Error: `"received": "undefined"` for fields

**After Fix:**
- Headers: `Content-Type: application/json`
- Server Response: JSON parsed correctly (transaction validation errors expected in test environment)

### Files Changed
- `src/HydraPay/Client.hs`: Added Content-Type header modification in `runHydraClient`

### Date
November 2025

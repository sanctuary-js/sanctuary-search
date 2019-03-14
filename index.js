(function(f) {

  'use strict';

  function withoutTypeChecking(S) {
    return f (S.create ({checkTypes: false, env: S.env}));
  }

  /* istanbul ignore else */
  if (typeof module === 'object' && typeof module.exports === 'object') {
    module.exports = withoutTypeChecking (require ('sanctuary'));
  } else if (typeof define === 'function' && define.amd != null) {
    define (['sanctuary'], withoutTypeChecking);
  } else {
    self.sanctuarySearch = withoutTypeChecking (self.sanctuary);
  }

} (function(S) {

  'use strict';

  //  S_pair :: (a -> b -> c) -> Pair a b -> c
  //
  //  To be added in sanctuary-js/sanctuary#609.
  var S_pair = S.curry2 (function(f, pair) {
    return f (pair.fst) (pair.snd);
  });

  //  parseSignature :: String -> Maybe (Array (Pair Integer String))
  function parseSignature(signature) {
    var tokens = S.chain (S.splitOn (' '))
                         (signature.split (/([(][)]|[{][}]|[({,})])/));
    var context = [];
    var depth = 0;
    var result = [];
    for (var idx = 0; idx < tokens.length; idx += 1) {
      var token = tokens[idx];
      if (token === '(') {
        context.push (token);
        depth += 1;
      } else if (token === ')') {
        if (context.pop () !== '(') return S.Nothing;
        depth -= 1;
      } else if (token === '{') {
        context.push (token);
        result.push (S.Pair (depth) (token));
      } else if (token === '}') {
        if (context.pop () !== '{') return S.Nothing;
        result.push (S.Pair (depth) (token));
      } else if (token !== '') {
        result.push (S.Pair (depth) (token));
      }
    }
    if (context.length > 0) return S.Nothing;
    return S.Just (result);
  }

  //  repeat :: String -> Integer -> String
  var repeat = S.curry2 (function(s, n) {
    var result = '';
    while (result.length < n) result += s;
    return result;
  });

  //  format :: Array (Pair Integer String) -> String
  function format(pairs) {
    var s = '';
    var depth = 0;
    for (var idx = 0; idx < pairs.length; idx += 1) {
      var pair = pairs[idx];
      s += repeat (')') (depth - pair.fst) +
           (s === '' || pair.snd === ',' ? '' : ' ') +
           repeat ('(') (pair.fst - depth) +
           pair.snd;
      depth = pair.fst;
    }
    return s + repeat (')') (depth);
  }

  //  at :: Integer -> Array a -> Maybe a
  var at = S.curry2 (function(idx, xs) {
    return idx >= 0 && idx < xs.length ? S.Just (xs[idx]) : S.Nothing;
  });

  //  legalBoundary
  //  :: Array (Pair NonNegativeInteger String)
  //  -> Integer
  //  -> Integer
  //  -> Boolean
  var legalBoundary = S.curry3 (function(tokens, inner, outer) {
    var filter = S.filter (S.compose (S.test (/^[A-Za-z]/)) (S.snd));
    var innerToken = filter (at (inner) (tokens));
    var outerToken = filter (at (outer) (tokens));
    return S.maybe (false)
                   (S.compose (S.flip (S.maybe (true))
                                      (outerToken))
                              (S.on (S.lt) (S.fst)))
                   (innerToken);
  });

  //  legalSlice
  //  :: Array (Pair NonNegativeInteger String)
  //  -> Pair NonNegativeInteger NonNegativeInteger
  //  -> Boolean
  var legalSlice = S.curry2 (function(tokens, range) {
    return legalBoundary (tokens) (range.fst) (range.fst - 1) &&
           legalBoundary (tokens) (range.snd - 1) (range.snd);
  });

  //  sliceMatches
  //  :: Array (Pair (Pair Integer String) (Pair Integer String))
  //  -> Boolean
  function sliceMatches(pairs) {
    var delta = pairs[0].snd.fst - pairs[0].fst.fst;
    var typeVarMap = Object.create (null);

    return S.all (function(pair) {
      return pair.fst.fst === pair.snd.fst - delta
             && (/^[a-z]$/.test (pair.fst.snd) ?
                 /^[a-z]$/.test (pair.snd.snd)
                 && (pair.fst.snd in typeVarMap ?
                     typeVarMap[pair.fst.snd] === pair.snd.snd :
                     S.not (S.elem (pair.snd.snd) (typeVarMap))
                     && (typeVarMap = S.insert (pair.fst.snd)
                                               (pair.snd.snd)
                                               (typeVarMap),
                         true)) :
                 pair.fst.snd === pair.snd.snd);
    }) (pairs);
  }

  //  matchTokens
  //  :: (String -> String)
  //  -> Array (Pair NonNegativeInteger String)
  //  -> Array (Pair NonNegativeInteger String)
  //  -> Either String String
  var matchTokens = S.curry3 (function(em, actualTokens, searchTokens) {
    function loop(matched, offset, matches) {
      var depth, slice;
      return (
        offset === actualTokens.length ?
        S.Pair (matched) (matches) :
        legalSlice (actualTokens)
                   (S.Pair (offset) (offset + searchTokens.length))
        && sliceMatches (S.zip (searchTokens)
                               (slice = actualTokens.slice (
                                          offset,
                                          offset + searchTokens.length))) ?
        loop (true,
              offset + searchTokens.length,
              S.append (S.Pair (depth = S.min (slice[0].fst)
                                              (slice[slice.length - 1].fst))
                               (em (format (S.map (S.mapLeft (S.sub (depth)))
                                                  (slice)))))
                       (matches)) :
        loop (matched,
              offset + 1,
              S.append (actualTokens[offset])
                       (matches))
      );
    }

    var matches = (function() {
      //  Special case for matching by function name.
      if (searchTokens.length === 1) {
        var search = searchTokens[0].snd, searchLower = search.toLowerCase ();
        var actual = actualTokens[0].snd, actualLower = actual.toLowerCase ();
        if (searchLower !== actualLower) {
          var idx = actualLower.indexOf (searchLower);
          if (idx >= 0) {
            return [S.Pair (0)
                           (actual.slice (0, idx) +
                            em (actual.slice (idx, idx + search.length)) +
                            actual.slice (idx + search.length))];
          }
        }
      }
      return [];
    } ());

    return S_pair (S.tagBy)
                  (S.bimap (S.K)
                           (format)
                           (loop (matches.length > 0,
                                  matches.length,
                                  matches)));
  });

  //  matchStrings
  //  :: (String -> String)
  //  -> String
  //  -> String
  //  -> Either String String
  var matchStrings = S.curry3 (function(em, signatureString, searchString) {
    return S.fromMaybe (S.Left (signatureString))
                       (S.lift2 (matchTokens (em))
                                (parseSignature (signatureString))
                                (parseSignature (searchString)));
  });

  return matchStrings;

}));

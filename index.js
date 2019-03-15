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
                         (signature.split (/([(][)]|[{][}]|[({,?})])/));
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
           (s === '' || pair.snd === ',' || pair.snd === '?' ? '' : ' ') +
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

  //  legalSlice
  //  :: Array (Pair NonNegativeInteger String)
  //  -> Pair NonNegativeInteger NonNegativeInteger
  //  -> Boolean
  var legalSlice = S.curry2 (function(tokens, range) {
    function f(opening, closing) {
      return opening.snd !== '?' &&
             S.maybe (true)
                     (S.compose (S_pair (S.or))
                                (S.bimap (S.lt (opening.fst))
                                         (S.test (/^(?![A-Za-z])/))))
                     (at (range.fst - 1) (tokens)) &&
             S.maybe (true)
                     (function(t) {
                        return t.snd !== '?' &&
                               (t.fst < closing.fst ||
                                S.test (/^(?![A-Za-z])/) (t.snd));
                      })
                     (at (range.snd) (tokens));
    }
    return S.chain (S.boolean (S.Nothing)
                              (S.Just (tokens.slice (range.fst, range.snd))))
                   (S.lift2 (S.curry2 (f))
                            (at (range.fst) (tokens))
                            (at (range.snd - 1) (tokens)));
  });

  //  sliceMatches
  //  :: Array (Pair (Pair Integer String) (Pair Integer String))
  //  -> Maybe (StrMap String)
  //  -> Maybe (StrMap String)
  var sliceMatches = S.curry3 (function(searchTokens, typeVarMap, slice) {
    return S.reduce
      (S.flip (function(pair) {
         return S.chain (function(state) {
           return (
             /^[a-z]$/.test (pair.fst.snd) ?
               /^[a-z]$/.test (pair.snd.snd) ?
                 pair.fst.snd in state.snd ?
                   state.snd[pair.fst.snd] === pair.snd.snd ?
                     S.Just (state) :
                     S.Nothing :
                   S.elem (pair.snd.snd) (state.snd) ?
                     S.Nothing :
                     S.Just (S.map (S.insert (pair.fst.snd) (pair.snd.snd))
                                   (state)) :
                 S.Nothing :
             pair.fst.snd === pair.snd.snd ?
               S.Just (state) :
               S.Nothing
           );
         });
       }))
      (S.Just (S.Pair (slice) (typeVarMap)))
      (S.zip (searchTokens) (slice));
  });

  //  highlightSubstring :: (String -> String) -> String -> String -> String
  var highlightSubstring = S.curry3 (function(em, s, t) {
    return S.pipe ([
      S.bimap (S.toLower) (S.toLower),
      S.Just,
      S.reject (S_pair (S.equals)),
      S.map (function(pair) { return pair.fst.indexOf (pair.snd); }),
      S.filter (S.gte (0)),
      S.map (function(i) {
        var j = i + t.length;
        return s.slice (0, i) + em (s.slice (i, j)) + s.slice (j);
      })
    ]) (S.Pair (s) (t));
  });

  //  matchTokens
  //  :: (String -> String)
  //  -> Array (Pair NonNegativeInteger String)
  //  -> Array (Pair NonNegativeInteger String)
  //  -> Either String String
  var matchTokens = S.curry3 (function(em, actualTokens, searchTokens) {
    function loop(typeVarMap, matched, offset, matches) {
      if (offset === actualTokens.length) return S.Pair (matched) (matches);

      return S.maybe_
        (function() {
           return loop (
             typeVarMap,
             matched,
             offset + 1,
             S.append (actualTokens[offset])
                      (matches)
           );
         })
        (function(pair) {
           var depth = S.min (pair.fst[0].fst)
                             (pair.fst[pair.fst.length - 1].fst);
           return loop (
             pair.snd,
             true,
             offset + searchTokens.length,
             S.append (S.Pair (depth)
                              (em (format (S.map (S.mapLeft (S.sub (depth)))
                                                 (pair.fst)))))
                      (matches)
           );
         })
        (S.chain (sliceMatches (searchTokens)
                               (typeVarMap))
                 (legalSlice (actualTokens)
                             (S.Pair (offset)
                                     (offset + searchTokens.length))));
    }

    var matches =
    S.maybe ([])
            (S.compose (S.of (Array)) (S.Pair (0)))
            (S.join (S.lift2 (S.on (highlightSubstring (em)) (S.snd))
                             (S.head (actualTokens))
                             (S.chain (S.head)
                                      (S.filter (S.compose (S.equals (1))
                                                           (S.size))
                                                (S.Just (searchTokens))))));

    return S_pair (S.tagBy)
                  (S.bimap (S.K)
                           (format)
                           (loop (Object.create (null),
                                  matches.length > 0,
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

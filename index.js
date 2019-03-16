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

  var SpaceBefore = 1 << 0;
  var SliceBefore = 1 << 1;
  var SliceAfter  = 1 << 2;

  //  tokens :: StrMap Boolean
  var tokens = {
    /* eslint-disable key-spacing */
    '::': SpaceBefore | SliceBefore | SliceAfter,
    '=>': SpaceBefore | SliceBefore | SliceAfter,
    '~>': SpaceBefore | SliceBefore | SliceAfter,
    '->': SpaceBefore | SliceBefore | SliceAfter,
    '()': SpaceBefore | SliceBefore | SliceAfter,
    '{}': SpaceBefore | SliceBefore | SliceAfter,
    '(':  SpaceBefore | SliceBefore | SliceAfter,
    ')':  SpaceBefore | SliceBefore | SliceAfter,
    '{':  SpaceBefore | SliceBefore | SliceAfter,
    '}':  SpaceBefore | SliceBefore | SliceAfter,
    ',':                SliceBefore | SliceAfter,
    '?':                              SliceAfter
    /* eslint-enable key-spacing */
  };

  //  tokenInfo :: Pair Integer String -> Maybe Info
  function tokenInfo(pair) {
    return Object.prototype.hasOwnProperty.call (tokens, pair.snd) ?
           S.Just (tokens[pair.snd]) :
           S.Nothing;
  }

  //  tokenAttr :: Info -> Info -> Boolean
  function tokenAttr(attr) {
    return function(info) {
      return (info & attr) > 0;
    };
  }

  //  spaceBefore :: Info -> Boolean
  var spaceBefore = tokenAttr (SpaceBefore);

  //  sliceBefore :: Info -> Boolean
  var sliceBefore = tokenAttr (SliceBefore);

  //  sliceAfter :: Info -> Boolean
  var sliceAfter = tokenAttr (SliceAfter);

  //  syntax :: RegExp
  var syntax = S.pipe ([
    Object.keys,
    S.map (S.regexEscape),
    S.joinWith ('|'),
    S.concat ('('),
    S.flip (S.concat) (')'),
    S.regex ('')
  ]) (tokens);

  //  parseSignature :: String -> Maybe (Array (Pair Integer String))
  function parseSignature(signature) {
    var tokens = S.chain (S.splitOn (' ')) (signature.split (syntax));
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
      var isToken = Object.prototype.hasOwnProperty.call (tokens, pair.snd);
      s += repeat (')') (depth - pair.fst) +
           (isToken && !(spaceBefore (tokens[pair.snd])) ? '' : s && ' ') +
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

  //  sliceMatches
  //  :: Array (Pair Integer String)
  //  -> Array (Pair Integer String)
  //  -> Integer
  //  -> StrMap String
  //  -> Maybe (Pair (StrMap String)
  //                 (Pair (Array (Pair Integer String))
  //                       (Array (Pair Integer String))))
  var sliceMatches = S.curry4 (function(
    actualTokens,
    searchTokens,
    offset,
    typeVarMap
  ) {
    return S.chain (function(slice) {
      return S.chain (function(pair) {
        return S.chain (function(depth) {
          var b = pair.fst;
          var y = pair.snd;
          var a_ = at (offset - 1) (actualTokens);
          var z_ = at (offset + searchTokens.length) (actualTokens);
          var bi = tokenInfo (b);
          var yi = tokenInfo (y);
          var ai = S.chain (tokenInfo) (a_);
          var zi = S.chain (tokenInfo) (z_);
          var delta = b.fst - depth;

          return (
            delta < 0 ||

            S.maybe (false) (S.complement (sliceAfter)) (ai) ||
            S.maybe (false) (S.complement (sliceBefore)) (bi) ||
            S.maybe (false) (S.complement (sliceAfter)) (yi) ||
            S.maybe (false) (S.complement (sliceBefore)) (zi) ||

            ai.isNothing && S.maybe (false) (S.on (S.gte) (S.fst) (b)) (a_) ||
            zi.isNothing && S.maybe (false) (S.on (S.gte) (S.fst) (y)) (z_) ||

            depth > 0 && S.maybe (false) (S.on (S.equals) (S.fst) (b)) (a_) ||
            depth > 0 && S.maybe (false) (S.on (S.equals) (S.fst) (y)) (z_)
          ) ?
            S.Nothing :
            S.reduce (S.flip (reducer))
                     (S.Just (S.Pair (typeVarMap)
                                     (S.Pair (searchTokens)
                                             (slice))))
                     (S.zip (searchTokens) (slice));

          function reducer(pair) {
            return S.chain (function(state) {
              var typeVarMap = state.fst;
              return (
                pair.fst.fst === pair.snd.fst - delta ?
                  /^[a-z]$/.test (pair.fst.snd) ?
                    /^[a-z]$/.test (pair.snd.snd) ?
                      pair.fst.snd in typeVarMap ?
                        typeVarMap[pair.fst.snd] === pair.snd.snd ?
                          S.Just (state) :
                          S.Nothing :
                        S.elem (pair.snd.snd) (typeVarMap) ?
                          S.Nothing :
                          S.Just (S.mapLeft (S.insert (pair.fst.snd)
                                                      (pair.snd.snd))
                                            (state)) :
                      S.Nothing :
                    pair.fst.snd === pair.snd.snd ?
                      S.Just (state) :
                      S.Nothing :
                  S.Nothing
              );
            });
          }
        }) (S.map (S.fst) (S.head (searchTokens)));
      }) (S.lift2 (S.Pair) (S.head (slice)) (S.last (slice)));
    }) (S.slice (offset) (offset + searchTokens.length) (actualTokens));
  });

  //  highlightSubstring :: (String -> String) -> String -> String -> String
  var highlightSubstring = S.curry3 (function(em, s, t) {
    return S.map (function(i) {
                    var j = i + t.length;
                    return s.slice (0, i) + em (s.slice (i, j)) + s.slice (j);
                  })
                 (S.filter (S.gte (0))
                           (S.Just ((S.toLower (s)).indexOf (S.toLower (t)))));
  });

  //  matchTokens
  //  :: (String -> String)
  //  -> Array (Pair Integer String)
  //  -> Array (Pair Integer String)
  //  -> Either String String
  var matchTokens = S.curry3 (function(em, searchTokens, actualTokens) {
    function loop(typeVarMap, previouslyMatched, offset, matches) {
      return offset === actualTokens.length ?
             S.Pair (previouslyMatched) (matches) :
             S.maybe_ (unmatched)
                      (matched)
                      (sliceMatches (actualTokens)
                                    (searchTokens)
                                    (offset)
                                    (typeVarMap));
      function unmatched() {
        return loop (typeVarMap,
                     previouslyMatched,
                     offset + 1,
                     S.append (actualTokens[offset]) (matches));
      }
      function matched(pair) {
        var searchTokens = pair.snd.fst;
        var slice = pair.snd.snd;
        var depth = slice[0].fst - searchTokens[0].fst;
        var match = S.Pair (depth)
                           (em (format (S.map (S.mapLeft (S.sub (depth)))
                                              (slice))));
        return loop (pair.fst,
                     true,
                     offset + searchTokens.length,
                     S.append (match) (matches));
      }
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

  //  search :: (String -> String) -> String -> String -> Either String String
  var search = S.curry3 (function(em, searchString, signatureString) {
    return S.fromMaybe (S.Left (signatureString))
                       (S.lift2 (matchTokens (em))
                                (parseSignature (searchString))
                                (parseSignature (signatureString)));
  });

  return search;

}));

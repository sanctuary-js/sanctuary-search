'use strict';

const assert        = require ('assert');
const fs            = require ('fs');
const path          = require ('path');

const S             = require ('sanctuary');

const search        = require ('..');


//    eq :: a -> b -> Undefined !
function eq(actual) {
  assert.strictEqual (arguments.length, eq.length);
  return function eq$1(expected) {
    assert.strictEqual (arguments.length, eq$1.length);
    assert.strictEqual (S.show (actual), S.show (expected));
    assert.strictEqual (S.equals (actual) (expected), true);
  };
}


//    signatures :: Array String
const signatures = S.pipe ([
  S.lines,
  S.mapMaybe (S.match (S.regex ('') ('^[ ]*//# (.*)$'))),
  S.mapMaybe (S.pipe ([S.prop ('groups'), S.head, S.join])),
]) (fs.readFileSync (
  path.join (__dirname, '..', 'node_modules', 'sanctuary', 'index.js'),
  {encoding: 'utf8'}
));

//    head :: Array a -> Either String a
const head = S.compose (S.maybeToEither ('Empty array')) (S.head);

//    name :: String -> Either String String
const name = S.compose (head) (S.splitOn (' :: '));

//    match :: String -> String -> Either String String
const match = S.flip (search (s => '@[' + s + ']@'));


suite ('search', () => {

  test ('"" never matches', () => {
    signatures.forEach (signature => {
      eq (match (signature) ('')) (S.Left (signature));
    });
  });

  test ('"XXX" never matches', () => {
    signatures.forEach (signature => {
      eq (match (signature) ('XXX')) (S.Left (signature));
    });
  });

  test ('exact function name always matches', () => {
    signatures.forEach (signature => {
      eq (S.isRight (S.chain (match (signature))
                             (name (signature))))
         (true);
    });
  });

  test ('upper-case function name always matches', () => {
    signatures.forEach (signature => {
      eq (S.isRight (S.chain (match (signature))
                             (S.map (S.toUpper) (name (signature)))))
         (true);
    });
  });

  test ('lower-case function name always matches', () => {
    signatures.forEach (signature => {
      eq (S.isRight (S.chain (match (signature))
                             (S.map (S.toLower) (name (signature)))))
         (true);
    });
  });

  test ('partial function name always matches', () => {
    signatures.forEach (signature => {
      eq (S.isRight (match (signature) (signature.charAt (0))))
         (true);
    });
  });

  test ('exact signature always matches', () => {
    signatures.forEach (signature => {
      eq (match (signature) (signature))
         (S.Right ('@[' + signature + ']@'));
    });
  });

  test ('unbalanced search string never matches', () => {
    signatures.forEach (signature => {
      eq (match (signature) ('(')) (S.Left (signature));
      eq (match (signature) (')')) (S.Left (signature));
      eq (match (signature) ('{')) (S.Left (signature));
      eq (match (signature) ('}')) (S.Left (signature));
    });
  });

  test ('"?" never matches', () => {
    signatures.forEach (signature => {
      eq (match (signature) ('?')) (S.Left (signature));
    });
  });

  test ('"::" never matches', () => {
    signatures.forEach (signature => {
      eq (match (signature) ('::')) (S.Left (signature));
    });
  });

  test ('inconsistent spacing is permissible', () => {
    eq (match (' trim  ::  String  ->  String ') ('trim::String->String'))
       (S.Right ('@[trim :: String -> String]@'));

    eq (match ('trim::String->String') (' trim  ::  String  ->  String '))
       (S.Right ('@[trim :: String -> String]@'));

    eq (match (' trim  ::  String  ->  String ') ('String->String'))
       (S.Right ('trim :: @[String -> String]@'));

    eq (match ('trim::String->String') (' String '))
       (S.Right ('trim :: @[String]@ -> @[String]@'));
  });

  test ('type variables are matched intelligently', () => {
    eq (match ('I :: a -> a') ('a')) (S.Right ('I :: @[a]@ -> @[a]@'));
    eq (match ('I :: a -> a') ('x')) (S.Right ('I :: @[a]@ -> @[a]@'));
    eq (match ('I :: a -> a') ('a -> a')) (S.Right ('I :: @[a -> a]@'));
    eq (match ('I :: a -> a') ('x -> x')) (S.Right ('I :: @[a -> a]@'));
    eq (match ('I :: a -> a') ('x -> y')) (S.Left ('I :: a -> a'));
    eq (match ('K :: a -> b -> a') ('x')) (S.Right ('K :: @[a]@ -> b -> @[a]@'));
    eq (match ('K :: a -> b -> a') ('x -> y')) (S.Right ('K :: @[a -> b]@ -> a'));
    eq (match ('K :: a -> b -> a') ('x -> y -> x')) (S.Right ('K :: @[a -> b -> a]@'));
    eq (match ('K :: a -> b -> a') ('x -> y -> y')) (S.Left ('K :: a -> b -> a'));
    eq (match ('K :: a -> b -> a') ('x -> y -> z')) (S.Left ('K :: a -> b -> a'));
  });

  test ('parens in search string are significant', () => {
    const lift2 = 'lift2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c';

    eq (match (lift2) ('x -> y'))
       (S.Right ('lift2 :: Apply f => (@[a -> b]@ -> c) -> f a -> f b -> f c'));

    eq (match (lift2) ('(x -> y)'))
       (S.Left (lift2));

    eq (match (lift2) ('x -> y -> z'))
       (S.Right ('lift2 :: Apply f => (@[a -> b -> c]@) -> f a -> f b -> f c'));

    eq (match (lift2) ('(x -> y -> z)'))
       (S.Right ('lift2 :: Apply f => @[(a -> b -> c)]@ -> f a -> f b -> f c'));

    eq (match (lift2) ('((x -> y -> z))'))
       (S.Left (lift2));

    const chainRec = 'chainRec :: ChainRec m => TypeRep m -> (a -> m (Either a b)) -> a -> m b';

    eq (match (chainRec) ('Either a b'))
       (S.Right ('chainRec :: ChainRec m => TypeRep m -> (a -> m (@[Either a b]@)) -> a -> m b'));

    eq (match (chainRec) ('(Either a b)'))
       (S.Right ('chainRec :: ChainRec m => TypeRep m -> (a -> m @[(Either a b)]@) -> a -> m b'));

    eq (match (chainRec) ('m (Either a b)'))
       (S.Right ('chainRec :: ChainRec m => TypeRep m -> (a -> @[m (Either a b)]@) -> a -> m b'));

    eq (match (chainRec) ('a -> m (Either a b)'))
       (S.Right ('chainRec :: ChainRec m => TypeRep m -> (@[a -> m (Either a b)]@) -> a -> m b'));

    eq (match (chainRec) ('(a -> m (Either a b))'))
       (S.Right ('chainRec :: ChainRec m => TypeRep m -> @[(a -> m (Either a b))]@ -> a -> m b'));
  });

  test ('type variables may be followed by "?"', () => {
    const toEither = 'toEither :: a -> b? -> Either a b';

    eq (match (toEither) ('x'))
       (S.Right ('toEither :: @[a]@ -> b? -> Either a b'));

    eq (match (toEither) ('x?'))
       (S.Right ('toEither :: a -> @[b?]@ -> Either a b'));

    eq (match (toEither) ('x -> y'))
       (S.Left (toEither));

    eq (match (toEither) ('x -> y?'))
       (S.Right ('toEither :: @[a -> b?]@ -> Either a b'));

    eq (match (toEither) ('x -> y? -> Either x y'))
       (S.Right ('toEither :: @[a -> b? -> Either a b]@'));

    eq (match (toEither) ('x -> y? -> Either x z'))
       (S.Left (toEither));
  });

  test ('uncurried function types can be matched', () => {
    const curry3 = '((a, b, c) -> d) -> a -> b -> c -> d';

    eq (match (curry3) ('x'))
       (S.Right ('((@[a]@, b, c) -> d) -> @[a]@ -> b -> c -> d'));

    eq (match (curry3) ('x, y'))
       (S.Right ('((@[a, b]@, c) -> d) -> a -> b -> c -> d'));

    eq (match (curry3) ('x, y, z'))
       (S.Right ('((@[a, b, c]@) -> d) -> a -> b -> c -> d'));

    eq (match (curry3) ('(x, y, z)'))
       (S.Right ('(@[(a, b, c)]@ -> d) -> a -> b -> c -> d'));

    eq (match (curry3) ('(x, y, z) -> w'))
       (S.Right ('(@[(a, b, c) -> d]@) -> a -> b -> c -> d'));

    eq (match (curry3) ('((x, y, z) -> w)'))
       (S.Right ('@[((a, b, c) -> d)]@ -> a -> b -> c -> d'));

    eq (match (curry3) ('(((x, y, z)) -> w)'))
       (S.Left (curry3));

    eq (match (curry3) ('(((x, y, z) -> w))'))
       (S.Left (curry3));
  });

  test ('TK', () => {
    eq (match ('bimap :: Bifunctor f => (a -> b) -> (c -> d) -> f a c -> f b d') ('a c -> f b d'))
       (S.Left ('bimap :: Bifunctor f => (a -> b) -> (c -> d) -> f a c -> f b d'));
    eq (match ('match :: NonGlobalRegExp -> String -> Maybe { match :: String, groups :: Array (Maybe String) }') ('match'))
       (S.Right ('@[match]@ :: NonGlobalRegExp -> String -> Maybe { @[match]@ :: String, groups :: Array (Maybe String) }'));
    eq (match ('matchAll :: GlobalRegExp -> String -> Array { match :: String, groups :: Array (Maybe String) }') ('match'))
       (S.Right ('@[match]@All :: GlobalRegExp -> String -> Array { @[match]@ :: String, groups :: Array (Maybe String) }'));
    eq (match ('matchAll :: GlobalRegExp -> String -> Array { match :: String, groups :: Array (Maybe String) }') ('all'))
       (S.Right ('match@[All]@ :: GlobalRegExp -> String -> Array { match :: String, groups :: Array (Maybe String) }'));
    eq (match ('chainRec :: ChainRec m => TypeRep m -> (a -> m (Either a b)) -> a -> m b') ('a -> m'))
       (S.Left ('chainRec :: ChainRec m => TypeRep m -> (a -> m (Either a b)) -> a -> m b'));
    eq (match ('T :: a -> (a -> b) -> b') ('a -> a'))
       (S.Left ('T :: a -> (a -> b) -> b'));
    eq (match ('T :: a -> (a -> b) -> b') ('a -> a -> b -> b'))
       (S.Left ('T :: a -> (a -> b) -> b'));
  });

});

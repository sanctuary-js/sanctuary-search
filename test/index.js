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

  test ('TK', () => {
    eq (match ('map :: Functor f => (a -> b) -> f a -> f b') ('a -> b'))
       (S.Right ('map :: Functor f => (@[a -> b]@) -> f a -> f b'));
    eq (match ('chainRec :: ChainRec m => TypeRep m -> (a -> m (Either a b)) -> a -> m b') ('Either a b'))
       (S.Right ('chainRec :: ChainRec m => TypeRep m -> (a -> m (@[Either a b]@)) -> a -> m b'));
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
    eq (match ('toMaybe :: a? -> Maybe a') ('x'))
       (S.Left ('toMaybe :: a? -> Maybe a'));
    eq (match ('toMaybe :: a? -> Maybe a') ('x?'))
       (S.Right ('toMaybe :: @[a?]@ -> Maybe a'));
    eq (match ('toMaybe :: a? -> Maybe a') ('x? -> Maybe x'))
       (S.Right ('toMaybe :: @[a? -> Maybe a]@'));
    eq (match ('curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d') ('x -> y'))
       (S.Right ('curry3 :: ((a, b, c) -> d) -> @[a -> b]@ -> c -> d'));
    eq (match ('K :: a -> b -> a') ('(a -> b)'))
       (S.Left ('K :: a -> b -> a'));
    eq (match ('T :: a -> (a -> b) -> b') ('a -> b'))
       (S.Right ('T :: a -> (@[a -> b]@) -> b'));
    eq (match ('T :: a -> (a -> b) -> b') ('(a -> b)'))
       (S.Right ('T :: a -> @[(a -> b)]@ -> b'));
    eq (match ('lift2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c') ('(a -> b)'))
       (S.Left ('lift2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c'));
  });

});

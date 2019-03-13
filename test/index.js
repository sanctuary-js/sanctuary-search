'use strict';

const assert        = require ('assert');

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


suite ('match', () => {

  test ('TK', () => {
    const match = search.match (s => '@[' + s + ']@');

    eq (match ('I :: a -> a') ('(')) (S.Left ('I :: a -> a'));
    eq (match ('I :: a -> a') (')')) (S.Left ('I :: a -> a'));
    eq (match ('I :: a -> a') ('{')) (S.Left ('I :: a -> a'));
    eq (match ('I :: a -> a') ('}')) (S.Left ('I :: a -> a'));

    eq (match ('I :: a -> a') ('a'))
       (S.Right ('I :: @[a]@ -> @[a]@'));
    eq (match ('K :: a -> b -> a') ('a -> b'))
       (S.Right ('K :: @[a -> b]@ -> a'));
    eq (match ('K :: a -> b -> a') ('x -> y'))
       (S.Right ('K :: @[a -> b]@ -> a'));
    eq (match ('K :: a -> b -> a') ('x -> y -> x'))
       (S.Right ('K :: @[a -> b -> a]@'));
    eq (match ('K :: a -> b -> a') ('x -> y -> y'))
       (S.Left ('K :: a -> b -> a'));
    eq (match ('K :: a -> b -> a') ('x -> y -> z'))
       (S.Left ('K :: a -> b -> a'));
    eq (match ('map :: Functor f => (a -> b) -> f a -> f b') ('a -> b'))
       (S.Right ('map :: Functor f => (@[a -> b]@) -> f a -> f b'));
    eq (match ('match :: NonGlobalRegExp -> String -> Maybe { match :: String, groups :: Array (Maybe String) }') ('match'))
       (S.Right ('@[match]@ :: NonGlobalRegExp -> String -> Maybe { @[match]@ :: String, groups :: Array (Maybe String) }'));
    eq (match ('matchAll :: GlobalRegExp -> String -> Array { match :: String, groups :: Array (Maybe String) }') ('match'))
       (S.Right ('@[match]@All :: GlobalRegExp -> String -> Array { @[match]@ :: String, groups :: Array (Maybe String) }'));
//  eq (match ('chainRec :: ChainRec m => TypeRep m -> (a -> m (Either a b)) -> a -> m b') ('a -> m'))
//     (S.Left ('chainRec :: ChainRec m => TypeRep m -> (a -> m (Either a b)) -> a -> m b'));
  });

});

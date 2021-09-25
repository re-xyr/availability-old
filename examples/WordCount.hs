module WordCount where

import           Availability
import           Availability.Embed
import           Availability.Lens
import           Availability.Reader
import           Availability.State
import           Availability.Writer
import           Control.Monad.Reader    (MonadIO, MonadReader, ReaderT (runReaderT))
import           Data.Coerce             (coerce)
import           Data.Foldable           (traverse_)
import           Data.Function           ((&))
import           Data.IORef              (IORef, newIORef, readIORef)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Monoid             (Sum (Sum))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           GHC.Generics            (Generic)
import           Test.Hspec              (Spec, describe, it)
import           Test.QuickCheck         (property)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

newtype Accum map = Accum map

instance (Ord k, Semigroup v)
  => Semigroup (Accum (Map k v)) where
    (<>) = coerce $ Map.unionWith @k @v (<>)

instance (Ord k, Semigroup v)
  => Monoid (Accum (Map k v)) where
    mempty = coerce $ Map.empty @k @v
    mappend = (<>)

newtype Occurrences k = Occurrences (Map k Int)
  deriving (Monoid, Semigroup) via Accum (Map k (Sum Int))
  deriving (Show, Eq)

oneOccurrence :: k -> Occurrences k
oneOccurrence k = Occurrences $ Map.singleton k 1

data CounterCtx = CounterCtx
  { letterCount :: IORef (Occurrences Char)
  , wordCount   :: IORef (Occurrences Text)
  } deriving Generic

newtype Counter a = Counter { runCounter :: ReaderT CounterCtx IO a }
  deriving (Functor, Applicative, Monad, MonadReader CounterCtx, MonadIO)
  deriving (Interpret (Embed IO)) via ViaMonadIO Counter
  deriving (Interpret (Teller "letterCount" (Occurrences Char))) via
    TellerByMonoid () $                    -- Derive Teller (weaker Writer) from a Monoid State
    StateByIORef () $                      -- Derive State from an IORef
    FromHas "letterCount" () CounterCtx $  -- Derive Reader of the field "letterCount" from Reader of CounterCtx
    ViaMonadReader Counter                 -- Derive Reader from MonadReader
  deriving (Interpret (Teller "wordCount" (Occurrences Text))) via
    TellerByMonoid () $ StateByIORef () $ FromHas "wordCount" () CounterCtx $ ViaMonadReader Counter

infixr 1 $
type f $ x = f x

type CounterM = M Counter

countLetter :: Eff (Teller "letterCount" (Occurrences Char)) => Char -> CounterM ()
countLetter letter = tell @"letterCount" (oneOccurrence letter)

countWord :: Eff (Teller "wordCount" (Occurrences Text)) => Text -> CounterM ()
countWord word = tell @"wordCount" (oneOccurrence word)

countWordAndLetters :: Effs '[Teller "letterCount" (Occurrences Char), Teller "wordCount" (Occurrences Text)]
  => Text -> CounterM ()
countWordAndLetters word = do
  countWord word
  traverse_ countLetter (Text.unpack word)

countWordsAndLettersInText :: Effs '[Teller "letterCount" (Occurrences Char), Teller "wordCount" (Occurrences Text)]
  => Text -> CounterM ()
countWordsAndLettersInText text =
  traverse_ countWordAndLetters (Text.words text)

wordAndLetterCount :: Text -> IO (Occurrences Char, Occurrences Text)
wordAndLetterCount text = do
  lettersRef <- newIORef $ Occurrences Map.empty
  wordsRef <- newIORef $ Occurrences Map.empty
  runM @'[Teller "letterCount" (Occurrences Char), Teller "wordCount" (Occurrences Text)]
    (countWordsAndLettersInText text)
    & runCounter
    & (`runReaderT` CounterCtx lettersRef wordsRef)
  (,) <$> readIORef lettersRef <*> readIORef wordsRef

pureWordAndLetterCount :: Text -> (Occurrences Char, Occurrences Text)
pureWordAndLetterCount text = let w = Text.words text in concatBoth $ pureCountWordAndLetters <$> w
  where
    pureCountWordAndLetters word = (mconcat $ pureCountLetter <$> Text.unpack word, pureCountWord word)
    pureCountLetter letter = oneOccurrence letter
    pureCountWord word = oneOccurrence word
    concatBoth = foldr (\(a, b) (c, d) -> (a <> c, b <> d)) (mempty, mempty)

spec :: Spec
spec = describe "Counter" do
  it "correctly handles text" do
    property \(s :: String) -> monadicIO do
      let t = Text.pack s
      (letter, word) <- run $ wordAndLetterCount t
      let (pletter, pword) = pureWordAndLetterCount t
      assert (letter == pletter && word == pword)

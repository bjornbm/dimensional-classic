
import qualified Numeric.NumTypeTests
import qualified Numeric.Units.Dimensional.Test
import qualified Numeric.Units.Dimensional.QuantitiesTest
import qualified Numeric.Units.Dimensional.ExtensibleTest

main = do
  Numeric.NumTypeTests.main
  Numeric.Units.Dimensional.Test.main
  Numeric.Units.Dimensional.QuantitiesTest.main
  Numeric.Units.Dimensional.ExtensibleTest.main


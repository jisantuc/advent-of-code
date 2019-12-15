import           Lib
import           Test.Hspec

rast1 :: Raster
rast1 = Raster [RasterRow [One, Two, Zero]]

rast2 :: Raster
rast2 = Raster [RasterRow [Two, One, Zero]]

interleavedRaster :: Raster
interleavedRaster = combineRasters rast1 rast2

interleavedCombined :: Raster
interleavedCombined = combineRasters interleavedRaster interleavedRaster

main :: IO ()
main =
  hspec $ do
    describe "combining raster" $ do
      it "interleaves non-interleaved rasters" $ do
        interleavedRaster `shouldBe`
          InterleavedRaster
            [InterleavedRow [[One, Two], [Two, One], [Zero, Zero]]]
      it "interleaves interleaved rasters" $ do
        interleavedCombined `shouldBe`
          InterleavedRaster
            [ InterleavedRow
                [ [One, Two, One, Two]
                , [Two, One, Two, One]
                , [Zero, Zero, Zero, Zero]
                ]
            ]

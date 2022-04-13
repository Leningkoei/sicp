data Vector = Vector {
  vectorX :: Double,
  vectorY :: Double
} deriving (Show)
vectorAdd :: Vector -> Vector -> Vector
vectorAdd a b = Vector x y where
  x = vectorX a + vectorX b
  y = vectorY a + vectorY b
vectorSub :: Vector -> Vector -> Vector
vectorSub a b = Vector x y where
  x = vectorX a - vectorX b
  y = vectorY a - vectorY b
vectorScale :: Double -> Vector -> Vector
vectorScale number vector = Vector x y where
  x = number * vectorX vector
  y = number * vectorY vector

data Segment = Segment {
  segmentStart :: Vector,
  segmentEnd   :: Vector
} deriving (Show)

data Frame = Frame {
  frameOrigin :: Vector,
  frameA      :: Vector,
  frameB      :: Vector
} deriving (Show)

frameCoordMap frame =
   \v ->
    vectorAdd
      (frameOrigin frame)
      (vectorAdd
        (vectorScale
          (vectorX v)
          (frameA frame))
        (vectorScale
          (vectorY v)
          (frameB frame)))


-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
module Tracks where

type TrackId = Integer

type Title = String

type Artist = String

type AlbumName = String

type Duration = Integer

type TrackList = [(TrackId, Title, Artist, Duration, [AlbumName])]

-- | Tracks by "Knuth & The Gang"
list1 :: TrackList
list1 =
  [ ( 11,
      "Algorithms",
      "Knuth & The Gang",
      111,
      ["Fundamental Algorithms", "The Art Of Computer Programming - The Collection"]
    ),
    ( 12,
      "Mathematical Preliminaries",
      "Knuth & The Gang",
      222,
      ["The Art Of Computer Programming - The Collection", "Fundamental Algorithms"]
    ),
    ( 13,
      "MMIX",
      "Knuth & The Gang",
      333,
      ["The Art Of Computer Programming - The Collection", "Fundamental Algorithms"]
    ),
    ( 18,
      "Celebration",
      "Knuth & The Gang",
      296,
      ["The Art Of Computer Programming - What's To Come"]
    )
  ]

-- | Tracks by "Haskell & Curry"
list2 :: TrackList
list2 =
  [ (23, "Mrs. Lovelace", "Haskell & Curry", 231, []),
    (22, "Bridge Over Impure Water", "Haskell & Curry", 296, [])
  ]

-- | Tracks by "Pure Bandit"
list3 :: TrackList
list3 =
  [ (31, "Being Lazy With Class", "Pure Bandit", 123, ["A History of Haskell"]),
    (32, "Lazy Beings With Class", "Pure Bandit", 234, ["A History of Haskell"]),
    (32, "Beings With Lazy Class", "Pure Bandit", 345, ["A History of Haskell"])
  ]

-- | Tracks by "The Functionals"
list4 :: TrackList
list4 =
  [ (40, "Grandfather's Lisp", "The Functionals", 1958, ["Influencers"]),
    (41, "Dad's Scheme", "The Functionals", 1970, ["Influencers"]),
    (42, "Mom's Miranda", "The Functionals", 1985, ["Influencers"]), --eh, no! not the same id for different songs!
    (43, "Hope", "The Functionals", 1970, ["Influencers"]),
    (44, "A New Hope+", "The Functionals", 1990, ["Influencers"])
  ]

-- | Concatenation of all previous track lists.
allTracks :: TrackList
allTracks = concat [list1, list2, list3, list4]

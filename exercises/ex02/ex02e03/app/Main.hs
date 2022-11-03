module Main where
import Tracks (allTracks, TrackList)
import Data.List (nub)

type Id = Integer
type Title = String
type Artist = String
type Duration = Integer

data Track = Track {trackId :: Id, trackName :: Title, trackArtist :: Artist, trackDuration :: Duration}
    deriving (Show, Eq)

data Album = Album {albumName :: Title, albumTracks :: [Id]}
    deriving (Show)

type User = String
data Rating = Good | Bad
    deriving (Show)
data UserRating = UserRating {user :: User, track:: Id, rating :: Rating}
    deriving (Show)

data MediaBib = MediaBib {tracks :: [Track], albums :: [Album], ratings :: [UserRating]} 
    deriving (Show)

addAlbum :: Id -> Album -> MediaBib -> MediaBib
addAlbum t a b = MediaBib
                    (tracks b)
                    (Album 
                        (albumName a)
                        (nub(t:if null (getAlb a b) then  [] else albumTracks(head(getAlb a b))))
                    :filter (diffAlb a) (albums b))
                    (ratings b)

diffAlb :: Album -> Album -> Bool
diffAlb x y = albumName x /= albumName y

getAlb :: Album -> MediaBib -> [Album]
getAlb a b = filter (not.diffAlb a) (albums b)

rateTrack :: User -> Id -> Rating -> MediaBib -> MediaBib
rateTrack u t r b = MediaBib
                    (tracks b)
                    (albums b)
                    (UserRating u t r:filter (diffUserRating u t)(ratings b))

diffUserRating :: User -> Id -> UserRating -> Bool
diffUserRating u t r = u /= user r && t /= track r

addTrack :: Track  -> MediaBib -> MediaBib
addTrack t b = MediaBib
                    (nub (t:tracks b))
                    (albums b)
                    (ratings b)

buildBib :: TrackList -> MediaBib
buildBib [] =   MediaBib [] [] []
buildBib ((i, title, artist, duration, []):xs) = addTrack (Track i title artist duration) (buildBib xs)
buildBib ((i, title, artist, duration, y:ys):xs) = addAlbum i (Album y []) (buildBib ((i, title, artist, duration, (ys)):xs))

queryTitlesAndDuration :: MediaBib -> [(Title, Duration)]
queryTitlesAndDuration (MediaBib _ [] _) = []
queryTitlesAndDuration (MediaBib a ((Album name []):xs) r) = []
queryTitlesAndDuration (MediaBib a ((Album name (y:ys)):xs) r) = []


main :: IO ()
main = print (buildBib allTracks)


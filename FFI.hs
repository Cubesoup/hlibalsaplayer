-- Haskell Bindings for libalsaplayer. Most things exposed in alsaplayer/control.h
-- are available. That said, not all of them work at the moment. You use this at
-- your own peril!   
--
-- Liscense:
--------------------------------------------------------------------------------
-- "THE BEER-WARE LICENSE" (Revision 42):
-- Chad Nester <chad.nester@gmail.com> wrote this file.  As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some day,
-- and you think this stuff is worth it, you can buy me a beer in return.
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}
module FFI where

import Foreign
import Foreign.C.Types
import Foreign.C.String       
import Data.Word
import Data.Convertible


-----------------------   
-- Defined Constants --
-----------------------   
   
-- watch for changes in the #defined constants!
-- also all but ap_file_path_max seem to be constraints on what
-- we get from alsaplayer, more than constraints on what we
-- can send it. Also note that we perform no error checking
-- at present on filenames sent to alsaplayer, so watch out for that.   
ap_session_max,ap_title_max,ap_artist_max,ap_album_max,ap_genre_max,ap_stream_type_max,ap_status_max,ap_comment_max,ap_track_number_max,ap_year_max,ap_file_path_max :: CInt   
ap_session_max     = 256
ap_title_max       = 256
ap_artist_max      = 256
ap_album_max       = 256
ap_genre_max       = 256
ap_stream_type_max = 256
ap_status_max      = 256
ap_comment_max     = 256
ap_track_number_max = 10
ap_year_max         = 10
ap_file_path_max    = 1024                    

-------------                    
-- Utility --                    
-------------

-- we use this a lot.        
   
cbool :: CInt -> Bool
cbool x = not (x == 0)
                    
--------------
-- Sessions --
--------------

-- each alsaplayer session is assigned a number, and all of the library functions
-- take a session as their first argument. The corresponding command is sent
-- to that session.   
                    
-- session max value is 256. We enforce this by
-- storing session numbers as Word8 (0,255)   
data SessionID = SessionID Word8
  deriving (Show, Eq)

-- x out of Word8 will raise an error via 'convert'
session :: Int -> SessionID
session x = SessionID (convert x)

----------------      
-- Generators --
----------------

-- None of the libalsaplayer functions are very complicated. We use the
-- following to wrap the majority of then in Haskell types. This also
-- means that understanding the wrapper for most of the C functions is
-- as easy as reading the generators. The 'getters' and 'setters' are all
-- very similar, so this is not so bad.

-- The exposed functions return either a Bool, in which
-- case the Bool is True iff the session was successfully contacted,
-- or a (Maybe t) in which case it returns (Just x) where x is a value
-- sent by alsaplayer if the session was session was successfully
-- contacted, and returns Nothing otherwise.   
   
noVal :: (CInt -> IO CInt) -> (SessionID -> IO Bool)
noVal f = \(SessionID snum) -> do
    retval <- f (convert snum)
    return $ cbool retval

getString :: (CInt -> CString -> IO CInt) -> (SessionID -> IO (Maybe String))
getString f = \(SessionID snum) -> do
    strptr <- newCString ""
    retval <- f (convert snum) strptr
    strval <- peekCString strptr
    if cbool retval
      then return $ Just strval
      else return Nothing  

getInt :: (CInt -> Ptr CInt -> IO CInt) -> (SessionID -> IO (Maybe Int))
getInt f = \(SessionID snum) -> do
    intptr <- new (-1)
    retval <- f (convert snum) intptr
    intval <- peek intptr
    if cbool retval
      then return $ Just $ convert intval
      else return Nothing

getBool :: (CInt -> Ptr CInt -> IO CInt) -> (SessionID -> IO (Maybe Bool))
getBool f = \(SessionID snum) -> do
    boolptr <- new (-1)
    retval  <- f (convert snum) boolptr
    boolval <- peek boolptr
    if cbool retval
      then return $ Just $ cbool boolval
      else return Nothing

getFloat :: (CInt -> Ptr CFloat -> IO CInt) -> (SessionID -> IO (Maybe Double))
getFloat f = \(SessionID snum)  -> do
    floatptr <- new (-1)
    retval   <- f (convert snum) floatptr
    floatval <- peek floatptr
    if cbool retval
      then return $ Just $ convert floatval
      else return Nothing
    
sendInt :: (CInt -> CInt -> IO CInt) -> (SessionID -> Int -> IO Bool)
sendInt f = \(SessionID snum) num -> do
    retval <- f (convert snum) (convert num)
    return $ cbool retval

sendBool :: (CInt -> CInt -> IO CInt) -> (SessionID -> Bool -> IO Bool)
sendBool f = \(SessionID snum) bool -> do         
    retval <- f (convert snum) (if bool then 1 else 0)
    return $ cbool retval

sendFloat :: (CInt -> CFloat -> IO CInt) -> (SessionID -> Double -> IO Bool)
sendFloat f = \(SessionID snum) float -> do
    retval <- f (convert snum) (convert float)
    return $ cbool retval

sendString :: (CInt -> CString -> IO CInt) -> (SessionID -> String -> IO Bool)
sendString f = \(SessionID snum) string -> do
    cstr <- newCString string
    retval <- f (convert snum) cstr
    return $ cbool retval

--------------    
-- Bindings --
--------------    
    
foreign import ccall "alsaplayer/control.h ap_find_session"
    ap_find_session :: CString -> Ptr CInt -> IO CInt

-- find the SessionID corresponding to a session name if it exists.
apFindSession :: String -> IO (Maybe SessionID)
apFindSession sname = do
    cname <- newCString sname
    intPtr <- new ((-1) :: CInt) 
    existsInt <- ap_find_session cname intPtr
    if cbool existsInt
      then do snum <- peek intPtr
              return $ Just $ SessionID $ convert snum
      else return Nothing
    
foreign import ccall "alsaplayer/control.h ap_session_running"
    ap_session_running :: CInt -> IO CInt

-- test whether or not session corresponding to given id is running    
apSessionRunning :: SessionID -> IO Bool
apSessionRunning = noVal ap_session_running

foreign import ccall "alsaplayer/control.h ap_version"
    ap_version :: CInt

-- alsaplayer version. defined constant in libalsaplayer/message.h    
apVersion :: Int 
apVersion = convert ap_version
    
foreign import ccall "alsaplayer/control.h ap_play"
    ap_play :: CInt -> IO CInt

-- send session the play command    
apPlay :: SessionID -> IO Bool
apPlay = noVal ap_play 
     
foreign import ccall "alsaplayer/control.h ap_stop"
    ap_stop :: CInt -> IO CInt

-- send session the stop command    
apStop :: SessionID -> IO Bool
apStop = noVal ap_stop       

foreign import ccall "alsaplayer/control.h ap_pause"
    ap_pause :: CInt -> IO CInt

-- send session the pause command           
apPause :: SessionID -> IO Bool
apPause = noVal ap_pause        

foreign import ccall "alsaplayer/control.h ap_unpause"
    ap_unpause :: CInt -> IO CInt
    
-- send session the unpause command        
apUnpause :: SessionID -> IO Bool
apUnpause = noVal ap_unpause          
    
foreign import ccall "alsaplayer/control.h ap_next"
    ap_next :: CInt -> IO CInt

-- send session the next (track) command    
apNext :: SessionID -> IO Bool
apNext = noVal ap_next       

foreign import ccall "alsaplayer/control.h ap_prev"
    ap_prev :: CInt -> IO CInt

-- send sesion the previous (track) command    
apPrev :: SessionID -> IO Bool
apPrev = noVal ap_prev       
    
foreign import ccall "alsaplayer/control.h ap_ping"
    ap_ping :: CInt -> IO CInt

-- ping the session. how does this differ from apSessionRunning?
apPing :: SessionID -> IO Bool
apPing = noVal ap_ping       
    
foreign import ccall "alsaplayer/control.h ap_quit"
    ap_quit :: CInt -> IO CInt

-- send the quit command    
apQuit :: SessionID -> IO Bool
apQuit = noVal ap_quit       
    
foreign import ccall "alsaplayer/control.h ap_clear_playlist"
    ap_clear_playlist :: CInt -> IO CInt

-- clear session playlist    
apClearPlaylist :: SessionID -> IO Bool
apClearPlaylist = noVal ap_clear_playlist                
    
foreign import ccall "alsaplayer/control.h ap_add_path"
    ap_add_path :: CInt -> CString -> IO CInt

-- add path to playlist. how does it fail? how does it succeed?    
apAddPath :: SessionID -> String -> IO Bool
apAddPath = sendString ap_add_path          
    
foreign import ccall "alsaplayer/control.h ap_add_and_play"
    ap_add_and_play :: CInt -> CString -> IO CInt

-- add path to playlist and play immediately    
apAddAndPlay :: SessionID -> String -> IO Bool
apAddAndPlay = sendString ap_add_and_play             
    
foreign import ccall "alsaplayer/control.h ap_add_playlist"
    ap_add_playlist :: CInt -> CString -> IO CInt

-- add playlist to playlist (does it overwrite the old playlist)    
apAddPlaylist :: SessionID -> String -> IO Bool
apAddPlaylist = sendString ap_add_playlist              
    
foreign import ccall "alsaplayer/control.h ap_shuffle_playlist"
    ap_shuffle_playlist :: CInt -> IO CInt

-- shuffle the playlist    
apShufflePlaylist :: SessionID -> IO Bool
apShufflePlaylist = noVal ap_shuffle_playlist                  

foreign import ccall "alsaplayer/control.h ap_save_playlist"
    ap_save_playlist :: CInt -> IO CInt

-- save the playlist (what exactly does this do)?    
apSavePlaylist :: SessionID -> IO Bool
apSavePlaylist = noVal ap_save_playlist               

foreign import ccall "alsaplayer/control.h ap_get_playlist_length"
    ap_get_playlist_length :: CInt -> Ptr CInt -> IO CInt

-- get the length of the playlist    
apGetPlaylistLength :: SessionID -> IO (Maybe Int)
apGetPlaylistLength = getInt ap_get_playlist_length                    
    
foreign import ccall "alsaplayer/control.h ap_set_speed"
    ap_set_speed :: CInt -> CFloat -> IO CInt

-- set playback speed. 1.0 is %100, 0 is paused. Negative speeds
-- play the track backwards. Negative speed crashes alsaplayer
-- for .flac files   
apSetSpeed :: SessionID -> Double -> IO Bool
apSetSpeed = sendFloat ap_set_speed           
    
foreign import ccall "alsaplayer/control.h ap_get_speed"
    ap_get_speed :: CInt -> Ptr CFloat -> IO CInt

-- get the playback speed. This can be negative, indicating
-- reverse playback.   
apGetSpeed :: SessionID -> IO (Maybe Double)
apGetSpeed = getFloat ap_get_speed           
    
foreign import ccall "alsaplayer/control.h ap_set_volume"
    ap_set_volume :: CInt -> CFloat -> IO CInt

-- set the volume. are values over 1.0 allowed? less than 0.0?    
apSetVolume :: SessionID -> Double -> IO Bool
apSetVolume = sendFloat ap_set_volume            
    
foreign import ccall "alsaplayer/control.h ap_get_volume"
    ap_get_volume :: CInt -> Ptr CFloat -> IO CInt

-- get the volume    
apGetVolume :: SessionID -> IO (Maybe Double)
apGetVolume = getFloat ap_get_volume            
    
foreign import ccall "alsaplayer/control.h ap_set_pan"
    ap_set_pan :: CInt -> CFloat -> IO CInt

-- set the pan. Can be negative. Positive is to the right,
-- negative is to the left.   
apSetPan :: SessionID -> Double -> IO Bool
apSetPan = sendFloat ap_set_pan         
    
foreign import ccall "alsaplayer/control.h ap_get_pan"
    ap_get_pan :: CInt -> Ptr CFloat -> IO CInt

-- get the pan, can be negative. Negative is left, positive
-- is right.   
apGetPan :: SessionID -> IO (Maybe Double)
apGetPan = getFloat ap_get_pan         
    
foreign import ccall "alsaplayer/control.h ap_is_paused"
    ap_is_paused :: CInt -> Ptr CInt -> IO CInt

-- true if paused, false otherwise    
apIsPaused :: SessionID -> IO (Maybe Bool)
apIsPaused = getBool ap_is_paused           
    
foreign import ccall "alsaplayer/control.h ap_set_looping"
    ap_set_looping :: CInt -> CInt -> IO CInt

-- True will cause alsaplayer to repeat the current track
-- False will ensure that it is not doing that
apSetLooping :: SessionID -> Bool -> IO Bool
apSetLooping = sendBool ap_set_looping             
    
foreign import ccall "alsaplayer/control.h ap_is_looping"
    ap_is_looping :: CInt -> Ptr CInt -> IO CInt

-- Find out whether alsaplayer is set to repeat the current track or not.
apIsLooping :: SessionID -> IO (Maybe Bool)
apIsLooping = getBool ap_is_looping            

foreign import ccall "alsaplayer/control.h ap_set_playlist_looping"
    ap_set_playlist_looping :: CInt -> CInt -> IO CInt

-- True will cause alsaplayer to repeat the playlist when it ends
-- False will ensure that it is not doing that.   
apSetPlaylistLooping :: SessionID -> Bool -> IO Bool
apSetPlaylistLooping = sendBool ap_set_playlist_looping                     
    
foreign import ccall "alsaplayer/control.h ap_is_playlist_looping"
    ap_is_playlist_looping :: CInt -> Ptr CInt -> IO CInt

-- Find out whether alsaplayer is set to repeat the playlist
apIsPlaylistLooping :: SessionID -> IO (Maybe Bool)
apIsPlaylistLooping = getBool ap_is_playlist_looping                    
    
foreign import ccall "alsaplayer/control.h ap_get_tracks"
    ap_get_tracks :: CInt -> Ptr CInt -> IO CInt

-- always returns 1? what do?    
apGetTracks :: SessionID -> IO (Maybe Int)
apGetTracks = getInt ap_get_tracks            
    
foreign import ccall "alsaplayer/control.h ap_get_session_name"
    ap_get_session_name :: CInt -> CString -> IO CInt

-- returns the name of the session in question    
apGetSessionName :: SessionID -> IO (Maybe String)
apGetSessionName = getString ap_get_session_name
    
foreign import ccall "alsaplayer/control.h ap_get_title"
    ap_get_title :: CInt -> CString -> IO CInt

-- blows up! fix!    
apGetTitle :: SessionID -> IO (Maybe String)
apGetTitle = getString ap_get_title           
    
foreign import ccall "alsaplayer/control.h ap_get_artist"
     ap_get_artist :: CInt -> CString -> IO CInt

-- get the artist of the current track     
apGetArtist :: SessionID -> IO (Maybe String)
apGetArtist = getString ap_get_artist            
     
foreign import ccall "alsaplayer/control.h ap_get_album"
     ap_get_album :: CInt -> CString -> IO CInt

-- get the album of the current track     
apGetAlbum :: SessionID -> IO (Maybe String)
apGetAlbum = getString ap_get_album           
     
foreign import ccall "alsaplayer/control.h ap_get_genre"
    ap_get_genre :: CInt -> CString -> IO CInt

-- blows up! fix! (seems to be anything with spaces explodes)
apGetGenre :: SessionID -> IO (Maybe String)
apGetGenre = getString ap_get_genre           
    
foreign import ccall "alsaplayer/control.h ap_get_year"
    ap_get_year :: CInt -> CString -> IO CInt

-- get the year of the current track as a string    
apGetYear :: SessionID -> IO (Maybe String)
apGetYear = getString ap_get_year          
    
foreign import ccall "alsaplayer/control.h ap_get_track_number"
    ap_get_track_number :: CInt -> CString -> IO CInt

-- get the track number of the current track as a string
-- note that this is from the file metadata, not
-- the playlist   
apGetTrackNumber :: SessionID -> IO (Maybe String)
apGetTrackNumber = getString ap_get_track_number                 
    
foreign import ccall "alsaplayer/control.h ap_get_comment"
    ap_get_comment :: CInt -> CString -> IO CInt

-- get the comment of the current track     
apGetComment :: SessionID -> IO (Maybe String)
apGetComment = getString ap_get_comment             
    
foreign import ccall "alsaplayer/control.h ap_get_file_path"
    ap_get_file_path :: CInt -> CString -> IO CInt

-- blows up! fix!    
-- get the filesystem location of the current track     
apGetFilePath :: SessionID -> IO (Maybe String)
apGetFilePath = getString ap_get_file_path              
    
foreign import ccall "alsaplayer/control.h ap_set_position"
    ap_set_position :: CInt -> CInt -> IO CInt

-- set position of the current track in seconds away
-- from the start of the track (position 0 is the start).   
apSetPosition :: SessionID -> Int -> IO Bool
apSetPosition = sendInt ap_set_position
              
foreign import ccall "alsaplayer/control.h ap_get_position"
    ap_get_position :: CInt -> Ptr CInt -> IO CInt

-- get position of the current track in seconds away
-- from the start of the track.   
apGetPosition :: SessionID -> IO (Maybe Int)
apGetPosition = getInt ap_get_position              
    
foreign import ccall "alsaplayer/control.h ap_set_position_relative"
    ap_set_position_relative :: CInt -> CInt -> IO CInt

-- Argument can be negative, set go backwards (if negative)
-- or forward (if positive) the number of seconds given
-- as an argument. If this would take us before the start
-- or after the end of the track it goes to the start or
-- end respectively.   
apSetPositionRelative :: SessionID -> Int -> IO Bool
apSetPositionRelative = sendInt ap_set_position_relative                      
    
foreign import ccall "alsaplayer/control.h ap_get_length"
    ap_get_length :: CInt -> Ptr CInt -> IO CInt

-- get the length in seconds of the current track.    
apGetLength :: SessionID -> IO (Maybe Int)
apGetLength = getInt ap_get_length            
    
{- not found in my libalsaplayer?

foreign import ccall "alsaplayer/control.h ap_set_block"
    ap_set_block :: CInt -> CInt -> IO CInt

foreign import ccall "alsaplayer/control.h ap_get_block"
    ap_get_block :: CInt -> Ptr CInt -> IO CInt

foreign import ccall "alsaplayer/control.h ap_get_blocks" 
    ap_get_blocks :: CInt -> Ptr CInt -> IO CInt
-}

foreign import ccall "alsaplayer/control.h ap_get_stream_type"
    ap_get_stream_type :: CInt -> CString -> IO CInt

-- no idea, explodes, fix!    
apGetStreamType :: SessionID -> IO (Maybe String)
apGetStreamType = getString ap_get_stream_type               
    
foreign import ccall "alsaplayer/control.h ap_get_status"
    ap_get_status :: CInt -> CString -> IO CInt

-- no idea, only seems to return ""    
apGetStatus :: SessionID -> IO (Maybe String)
apGetStatus = getString ap_get_status            
    
foreign import ccall "alsaplayer/control.h ap_is_playing"
    ap_is_playing :: CInt -> Ptr CInt -> IO CInt

-- returns true if track is playing, or paused.
-- false if stopped.   
apIsPlaying :: SessionID -> IO (Maybe Bool)
apIsPlaying = getBool ap_is_playing       
    
foreign import ccall "alsaplayer/control.h ap_sort"
    ap_sort :: CInt -> Ptr CChar -> IO CInt -- called seq not str so no CString ?
-- I have no idea what this one does

    
foreign import ccall "alsaplayer/control.h ap_jump_to"
    ap_jump_to :: CInt -> CInt -> IO CInt

-- jumps to the given track number in the playlist
apJumpTo :: SessionID -> Int -> IO Bool
apJumpTo = sendInt ap_jump_to         
    
foreign import ccall "alsaplayer/control.h ap_get_playlist_position"
    ap_get_playlist_position :: CInt -> Ptr CInt -> IO CInt

-- get the position of the current track in the playlist    
apGetPlaylistPosition :: SessionID -> IO (Maybe Int)
apGetPlaylistPosition = getInt ap_get_playlist_position                      
    
foreign import ccall "alsaplayer/control.h ap_get_file_path_for_track"
    ap_get_file_path_for_track :: CInt -> CString -> CInt -> IO CInt

-- I'm thinking certain strings need better handling. fix!    
apGetFilePathForTrack :: SessionID -> Int -> IO (Maybe String)
apGetFilePathForTrack (SessionID snum) tnum = do
    strptr <- newCString ""
    retval <- ap_get_file_path_for_track (convert snum) strptr (convert tnum)
    strval <- peekCString strptr
    if cbool retval
      then return $ Just strval
      else return Nothing
    
foreign import ccall "alsaplayer/control.h ap_insert"
    ap_insert :: CInt -> CString -> CInt -> IO CInt

-- insert a song (given by filepath) into the playlist
-- at given position   
apInsert :: SessionID -> String -> Int -> IO Bool
apInsert (SessionID snum) path int = do
    cstr <- newCString path
    retval <- ap_insert (convert snum) cstr (convert int)
    return $ cbool retval
    
foreign import ccall "alsaplayer/control.h ap_remove"
     ap_remove :: CInt -> CInt -> IO CInt

-- remove song at given position from the playlist.     
apRemove :: SessionID -> Int -> IO Bool
apRemove = sendInt ap_remove         
     
foreign import ccall "alsaplayer/control.h ap_set_current"
     ap_set_current :: CInt -> CInt -> IO CInt

-- set current track in terms of playlist numbers. (?)     
apSetCurrent :: SessionID -> Int -> IO Bool
apSetCurrent = sendInt ap_set_current             
     
foreign import ccall "alsaplayer/control.h ap_get_playlist"
     ap_get_playlist :: CInt -> Ptr CInt -> Ptr (Ptr (CString))
     
     
    
    
    

    
    

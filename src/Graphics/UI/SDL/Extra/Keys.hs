module Graphics.UI.SDL.Extra.Keys where
import Graphics.UI.SDL hiding (init)

keyToChar :: SDLKey -> Maybe Char
keyToChar SDLK_a = Just 'a'
keyToChar SDLK_b = Just 'b'
keyToChar SDLK_c = Just 'c'
keyToChar SDLK_d = Just 'd'
keyToChar SDLK_e = Just 'e'
keyToChar SDLK_f = Just 'f'
keyToChar SDLK_g = Just 'g'
keyToChar SDLK_h = Just 'h'
keyToChar SDLK_i = Just 'i'
keyToChar SDLK_j = Just 'j'
keyToChar SDLK_k = Just 'k'
keyToChar SDLK_l = Just 'l'
keyToChar SDLK_m = Just 'm'
keyToChar SDLK_n = Just 'n'
keyToChar SDLK_o = Just 'o'
keyToChar SDLK_p = Just 'p'
keyToChar SDLK_q = Just 'q'
keyToChar SDLK_r = Just 'r'
keyToChar SDLK_s = Just 's'
keyToChar SDLK_t = Just 't'
keyToChar SDLK_u = Just 'u'
keyToChar SDLK_v = Just 'v'
keyToChar SDLK_w = Just 'w'
keyToChar SDLK_x = Just 'x'
keyToChar SDLK_y = Just 'y'
keyToChar SDLK_z = Just 'z' 
keyToChar SDLK_SPACE = Just ' '
keyToChar SDLK_BACKSPACE = Just '\DEL'
keyToChar SDLK_UNDERSCORE = Just '_'
keyToChar SDLK_RETURN = Just '\n'
keyToChar SDLK_MINUS = Just '-'
keyToChar _ = Nothing

getStringAndDo :: (String -> IO ()) -> IO (Maybe String)
getStringAndDo f = loop ""
  where
    loop :: String -> IO (Maybe String)
    loop [] = f [] >> pollEvent >>= \event -> 
        case event of
            KeyDown (Keysym SDLK_RETURN _ _)    -> return Nothing
            KeyDown (Keysym key _ _)            -> loop $ maybe "" (:[]) (keyToChar key)
            _                                   -> loop []

    loop line = f line >> pollEvent >>= \event ->
        case event of
            KeyDown (Keysym SDLK_RETURN _ _)    -> return (Just line)
            KeyDown (Keysym SDLK_BACKSPACE _ _) -> loop $ init line
            KeyDown (Keysym key _ _)            -> loop $ maybe line ((line ++) . (:[])) (keyToChar key)
            _                                   -> loop line

    

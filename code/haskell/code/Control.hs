module Control where

import Window
import Graphics

-------------------------------------------------------------------------------
-- Buttons
--

buttonNew :: ID -> Bounds -> [Style] -> Title -> Maybe MessageHandler -> Window

buttonNew id bounds style label (Just mh) = Window id label (buttonDisplayStyle++style) bounds [] Enabled mh
buttonNew id bounds style label Nothing = Window id label (buttonDisplayStyle++style) bounds [] Enabled buttonHandleMessage

-------------------------------------------------------------------------------

buttonHandleMessage :: MessageHandler

buttonHandleMessage win (Draw _) = return $ DrawCmds [RasterText 3 (0,20,0) (GraphicsColour3 0 0 0) (title win)]
buttonHandleMessage win (WindowSelected _ _ _) = return $ NoResponse
buttonHandleMessage win _ = return $ NoResponse

buttonDisplayStyle = framedStyle

{-
-------------------------------------------------------------------------------
-- Checkboxes
--

checkboxDisplayStyle  = [ Background $ GraphicsColour3 1 1 1 ]
checkboxStr True = "[X] "
checkboxStr False = "[-] "

checkboxNew :: ID -> Bounds -> Title -> Bool -> Window
checkboxNew id bounds label state = newWindow id [] label checkboxDisplayStyle bounds (Just $ checkboxHandleEvent state)

-------------------------------------------------------------------------------

checkboxState :: Window -> ID -> Bool
checkboxState win id = 
    case sendMessageTo win id (StateQuery 0) of
        (Response (BoolValue state)) -> state
        _ -> False

-------------------------------------------------------------------------------

checkboxHandleEvent :: Bool -> Window -> MessageBody -> QueryFunction -> Response

checkboxHandleEvent value win Draw _ = DrawCmds [RasterText 3 (0,20,0) (GraphicsColour3 0 0 0) (checkboxStr value ++ (title win) )]
checkboxHandleEvent value win (WindowSelected _ _) _ = Update (win { handleMessage = checkboxHandleEvent (not value) }) (Just Clicked)
checkboxHandleEvent value win (StateQuery _) _ = Response $ BoolValue value
checkboxHandleEvent _ _ _ _ = NoResponse

-}


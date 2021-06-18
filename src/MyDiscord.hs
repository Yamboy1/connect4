{-# LANGUAGE OverloadedStrings #-}

module MyDiscord where

import Control.Monad (when, void)

import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.Matrix (Matrix, toLists, zero)
import Data.Text.IO as TIO (putStrLn)
import qualified Data.Text as T

import Discord
import Discord.Types
import qualified Discord.Requests as R

discord :: IO ()
discord = do userFacingError <- runDiscord $ def
                                    { discordToken = "ODU0OTgyMTkzMTEzNTk1OTE0.YMr2Nw.5HUjHOhAiysdOcrLLd4SnHQo5Wc"
                                    , discordOnEvent = eventHandler }
             TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && messageText m == "C4") $ do
                            either <- displayBoard (messageChannel m) (zero 6 7)
                            case either of
                              Left _ -> return ()
                              Right message -> void . restCall $ R.CreateReaction (messageChannel message, messageId message) "one"
                            return ()
      MessageReactionAdd info -> do
        forM_
          (parseInput $ reactionEmoji info)
          (restCall . (R.CreateMessage (reactionChannelId info) . T.pack . show))
      _ -> return ()

parseInput :: Emoji -> Maybe Int
parseInput emoji = do
  let name = emojiName emoji
  case name of
    "1️⃣" -> Just 1
    "2️⃣" -> Just 2
    "3️⃣" -> Just 3
    "4️⃣" -> Just 4
    "5️⃣" -> Just 5
    "6️⃣" -> Just 6
    "7️⃣" -> Just 7
    _ -> Nothing

displayBoard :: ChannelId -> Matrix Int -> DiscordHandler (Either RestCallErrorCode Message)
displayBoard c board = restCall (R.CreateMessage c (formatBoard board))

boardHeaders :: T.Text
boardHeaders = "1️⃣2️⃣3️⃣4️⃣5️⃣6️⃣7️⃣\n"

formatBoard :: Matrix Int -> T.Text
formatBoard board = (boardHeaders <>) $ T.intercalate "\n" $ T.concat . fmap toCustomEmoji <$> toLists board

toCustomEmoji :: Int -> T.Text
toCustomEmoji 0 = "<:c4_0:538044874575052812>"
toCustomEmoji 1 = "<:c4_1:538059247943155732>"
toCustomEmoji 2 = "<:c4_2:538059248245014528>"
toCustomEmoji _ = error "This should never happen"

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
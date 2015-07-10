-- | CoALP error messages
module CoALP.Error (
	Err(..)
)where


-- | Error messages data type
data Err = EmptyMsg
	 | Msg String
         | InternalMsg String
	 | NotImplementedYet String
	 | ParserErr String
	deriving Show


-- | CoALP error messages and helpers
module CoALP.Error (
	Err(..)
)where


-- | error messages data type
data Err = EmptyMsg
	 | Msg String
         | InternalMsg String
	 | NotImplementedYet String
	 | ParserErr String
	deriving Show


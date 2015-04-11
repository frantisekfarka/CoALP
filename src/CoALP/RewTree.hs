-- | Module thtat constructs rewriting tree
module CoALP.RewTree (
	rew
) where

import CoALP.Program (Program, Clause, Subst, RewTree)

rew :: Program a b c -> Clause a b c -> Subst a b c -> RewTree a b c
rew = undefined



-- | Module thtat constructs rewriting tree
module CoALP.RewTree (
	rew
) where

import CoALP.Program (Program, Clause, Subst, Rew)

rew :: Program a b c -> Clause a b c -> Subst -> Rew a b c
rew = undefined



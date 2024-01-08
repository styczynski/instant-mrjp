module Backend.X86.ASMOptimizer where

import qualified Backend.X86.Syntax as ASM

cleanupX86 :: ASM.Program ASM.Position -> ASM.Program ASM.Position
cleanupX86 (ASM.Program p is) = ASM.Program p (stackOptim $ clean is)

clean (ASM.MOV p bx x : ASM.MOV p' y bx' : is) | bx == bx' && isTemp bx && (ASM.isReg x || ASM.isReg y) = clean (ASM.MOV p y x : is)

clean (ASM.MOV p bx x : ASM.CMP p' bx' y : is) | bx == bx' && isTemp bx = clean (ASM.CMP p' x y : is)

clean (ASM.MOV p x bx : ASM.TEST p' bx' bx'' : is) | bx == bx' && bx == bx'' && isTemp bx = clean (ASM.MOV p x bx : ASM.TEST p' x x : is)

clean (ASM.MOV _ a b : is) | a == b = clean is
clean (ASM.MOV p x y : ASM.MOV p' x' z : is) | x == x' && notDependent x z = clean (ASM.MOV p x z : is)
clean (ASM.MOV p x y : ASM.MOV p' y' x' : is) | x == x' && y == y' = clean (ASM.MOV p x y : is)

clean (ASM.MOV _ bx x : ASM.SUB p bx' y : ASM.MOV _ x' bx'' : is) | bx == bx' && bx == bx'' && x == x' && isTemp bx && ASM.isReg x = clean (ASM.SUB p x y : is)
clean (ASM.MOV _ bx x : ASM.SUB p bx' y : ASM.MOV _ z bx'' : is) | bx == bx' && bx == bx'' && z /= y && isTemp bx && ASM.isReg z = clean (ASM.MOV p z x : ASM.SUB p z y : is)

clean (ASM.MOV _ bx x : ASM.ADD p bx' y : ASM.MOV _ x' bx'' : is) | bx == bx' && bx == bx'' && x == x' && isTemp bx && ASM.isReg x = clean (ASM.ADD p x y : is)
clean (ASM.MOV _ bx x : ASM.ADD p bx' y : ASM.MOV _ y' bx'' : is) | bx == bx' && bx == bx'' && y == y' && isTemp bx && ASM.isReg y = clean (ASM.ADD p y x : is)
clean (ASM.MOV _ bx x : ASM.ADD p bx' y : ASM.MOV _ z bx'' : is) | bx == bx' && bx == bx'' && z /= y && isTemp bx && ASM.isReg z = clean (ASM.MOV p z x : ASM.ADD p z y : is)

clean (ASM.MOV _ bx x : ASM.IMUL p bx' y : ASM.MOV _ x' bx'' : is) | bx == bx' && bx == bx'' && x == x' && isTemp bx && ASM.isReg x = clean (ASM.IMUL p x y : is)
clean (ASM.MOV _ bx x : ASM.IMUL p bx' y : ASM.MOV _ y' bx'' : is) | bx == bx' && bx == bx'' && y == y' && isTemp bx && ASM.isReg y = clean (ASM.IMUL p y x : is)
clean (ASM.MOV _ bx x : ASM.IMUL p bx' y : ASM.MOV _ z bx'' : is) | bx == bx' && bx == bx'' && z /= y && isTemp bx && ASM.isReg z = clean (ASM.MOV p z x : ASM.IMUL p z y : is)

clean (ASM.JMP _ (ASM.Label _ v) : ASM.SetLabel p el : ASM.SetLabel p' en : is) | v == en = clean (ASM.SetLabel p el : ASM.SetLabel p' en : is)

clean (i:is) = i : clean is
clean [] = []

stackOptim xs = let xs' = opt xs in if xs == xs' then xs else stackOptim xs'
    where
        opt (ASM.MOV _ bx sp : ASM.CALL p l : ASM.MOV _ sp' bx' : is) | bx == bx' && isTemp bx && sp == sp' && isStack sp = opt (ASM.CALL p l : is)
        opt (ASM.ADD _ rsp v : ASM.SUB _ rsp' v' : is) | rsp == rsp' && v == v' && isStack rsp = opt is
        opt (ASM.ADD _ rsp v : i : ASM.SUB _ rsp' v' : is) | rsp == rsp' && v == v' && isStack rsp = opt (i:is)
        opt (i:is) = i : opt is
        opt [] = []

notDependent (ASM.Register _ r) (ASM.Memory _ r' _ _ _) | r == r' = False
notDependent (ASM.Register _ r) (ASM.Memory _ _ (Just (r', _)) _ _) | r == r' = False
notDependent (ASM.Memory _ r' _ _ _) (ASM.Register _ r) | r == r' = False
notDependent (ASM.Memory _ _ (Just (r',_)) _ _) (ASM.Register _ r) | r == r' = False
notDependent _ _ = True

isTemp (ASM.Register _ x) = case ASM.topReg x of
    (ASM.RBX _) -> True
    _ -> False
isTemp _ = False

isStack (ASM.Register _ (ASM.RSP _)) = True
isStack _ = False

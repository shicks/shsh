import qualified Text.Parsec.Prim as Prim_

#define withMsg (\f -> f (__FILE__++":"++show __LINE__))
#define many withMsg safeMany

safeMany :: (Prim_.Stream s m t) => String
         -> Prim_.ParsecT s u m a -> Prim_.ParsecT s u m [a]
safeMany msg p
  = do xs <- manyAccum' msg (:) p
       return (reverse xs)

manyAccum' ::(Prim_.Stream s m t)
          => String
          -> (a -> [a] -> [a])
          -> Prim_.ParsecT s u m a
          -> Prim_.ParsecT s u m [a]
manyAccum' msg accum p
    = Prim_.ParsecT $ \s ->
        let walk xs state mr
              = do r <- mr
                   case r of
                     Prim_.Empty mReply
                         -> do reply <- mReply
                               case reply of
                                 Prim_.Error err -> return $ Prim_.Ok xs state err
                                 _         -> error $ "Text.Parsec.Prim.many at "++msg++": combinator 'many' is applied to a parser that accepts an empty string."
                     Prim_.Consumed mReply
                         -> do reply <- mReply
                               case reply of
                                 Prim_.Error err
                                     -> return $ Prim_.Error err
                                 Prim_.Ok x s' _err
                                     -> let ys = accum x xs
                                        in seq ys (walk ys s' (Prim_.runParsecT p s'))
        in do r <- Prim_.runParsecT p s
              case r of
                Prim_.Empty mReply
                    -> do reply <- mReply
                          case reply of
                            Prim_.Ok _ _ _
                                -> error $ "Text.ParserCombinators.Parsec.Prim.many at "++msg++": combinator 'many' is applied to a parser that accepts an empty string."
                            Prim_.Error err
                                -> return $ Prim_.Empty $ return (Prim_.Ok [] s err)
                consumed
                    -> return $ Prim_.Consumed $ walk [] s (return consumed)

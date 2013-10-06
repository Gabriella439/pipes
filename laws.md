# Kleisli Category

    Define:
    
    return
        :: (Monad m)
        =>  r -> Proxy a' a b' b m r
    return = Pure
    
    (>=>)
        :: (Monad m)
        => (r -> Proxy a' a b' b m s)
        -> (s -> Proxy a' a b' b m t)
        -> (r -> Proxy a' a b' b m t)
    (f >=> g) x = f x >>= g
    
    (>>=)
        :: (Monad m)
        =>       Proxy a' a b' b m s
        -> (s -> Proxy a' a b' b m t)
        ->       Proxy a' a b' b m t
    p >>= f = case p of
        Request a' fa  -> Request a' (\a  -> fa  a  >>= f)
        Respond b  fb' -> Respond b  (\b' -> fb' b' >>= f)
        M          m   -> M (m >>= \p' -> return (p' >>= f))
        Pure    r      -> f r
    
## Left Identity Law

    Goal: return >=> f = f
    
    return >=> f
    
    -- Definition of `(>=>)`
    = \x -> return x >>= f
    
    -- [Kleisli Category - Left Identity Law - Pointful]
    = \x -> f x
    
    -- Eta reduce
    = f
    
    -- Goal complete
    
### Pointful

    Goal: return r >>= f = f r
    
    return r >>= f
    
    -- Definition of `return`
    = Pure r >>= f
    
    -- Definition of `(>>=)`
    = f r
    
    -- Goal complete
    
## Right Identity Law

    Goal: f >=> return = f
    
    f >=> return
    
    -- Definition of `(>=>)`
    = \x -> f x >>= return
    
    -- [Kleisli Category - Right Identity Law - Pointful]
    = \x -> f x
    
    -- Eta reduce
    = f
    
    -- Goal complete
    
### Pointful

    Goal: p >>= return = p
    
    p >>= return
    
    -- Definition of `(>>=)`
    = case p of
        Request a' fa  -> Request a' (\a -> fa a >>= return)
        
                        -- Coinduction: Reuse the premise
                        = Request a' (\a -> fa a)
                        
                        -- Eta reduce
                        = Request a' fa
                        
        Respond b  fb' -> Respond b (\b' -> fb' b' >>= return)
        
                        -- Coinduction: Reuse the premise
                        = Respond b (\b' -> fb' b')
                        
                        -- Eta reduce
                        = Respond b fb'
                        
        M          m   -> M (m >>= \p' -> return (p' >>= return))
        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return p')
                        
                        -- Eta reduce
                        = M (m >>= return)
                        
                        -- [Kleisli Category - Right Identity Law - Pointful] (NOTE: for base monad, so no coinduction necessary)
                        = M m
                        
        Pure    r      -> return r
        
                        -- Definition of `return`
                        = Pure r
                        
    -- Clean up
    = case p of
        Request a' fa  -> Request a' fa
        Respond b  fb' -> Respond b  fb'
        M          m   -> M          m
        Pure    r      -> Pure    r
        
    -- case statement = id
    = p
    
    -- Goal complete
    
## Associativity Law

    Goal: (f >=> g) >=> h = f >=> (g >=> h)
    
    (f >=> g) >=> h
    
    -- Definition of `(>=>)`
    = \x -> (f >=> g) x >>= h
    
    -- Definition of `(>=>)`
    = \x -> (f x >>= g) >>= h
    
    -- [Kleisli Category - Associativity Law - Pointful]
    = \x -> f x >>= \y -> g y >>= h
    
    -- Definition of `(>=>)`, in reverse
    = \x -> f x >>= (g >=> h)
    
    -- Definition of `(>=>)`, in reverse
    = f >=> (g >=> h)
    
    -- Goal complete

### Pointful

    Goal: (p >>= f) >>= g) = p >>= \x -> f x >>= g
    
    (p >>= f) >>= g
    
    -- Definition of `(>>=)`
    (case p of
        Request a' fa  -> Request a' (\a  -> fa  a  >>= f)
        Respond b  fb' -> Respond b  (\b' -> fb' b' >>= f)
        M          m   -> M (m >>= \p' -> return (p' >>= f))
        Pure    r      -> f r ) >>= g
        
    -- Distribute over case statement
    = case p of
        Request a' fa  -> Request a' (\a -> fa a >>= f) >>= g
        
                        -- Definition of `(>>=)`
                        = Request a' (\a -> (fa a >>= f) >>= g)
                        
                        -- Coinduction: Reuse the premise
                        = Request a' (\a -> fa a >>= \x -> f x >>= g)
                        
                        -- Definition of `(>>=)`, in reverse
                        = Request a' fa >>= \x -> f x >>= g
                        
        Respond b  fb' -> Respond b (\b' -> fb' b' >>= f) >>= g
        
                        -- Definition of `(>>=)`
                        = Respond b (\b' -> (fb' b' >>= f) >>= g)
                        
                        -- Coinduction: Reuse the premise
                        = Respond b (\b' -> fb' b' >>= \x -> f x >>= g)
                        
                        -- Definition of `(>>=)`, in reverse
                        = Respond b fb' >>= \x -> f x >>= g
                        
        M          m   -> M (m >>= \p' -> return (p' >>= f)) >>= g
        
                        -- Definition of `(>>=)`
                        = M (m >>= \p' -> return ((p' >>= f) >>= g))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return (p' >>= \x -> f x >>= g))
                        
                        -- Definition of `(>>=)`, in reverse
                        = M m >>= \x -> f x >>= g
                        
        Pure    r      -> f r >>= g

                        -- Definition of `(>>=)`, in reverse
                        = Pure r >>= \x -> f x >>= g
                        
    -- Clean up
    = case p of
        Request a' fa  -> Request a' fa  >>= \x -> f x >>= g
        Respond b  fb' -> Respond b  fb' >>= \x -> f x >>= g
        M          m   -> M          m   >>= \x -> f x >>= g
        Pure    r      -> Pure    r      >>= \x -> f x >>= g
        
    -- Factor from case statement
    = (case p of
        Request a' fa  -> Request a' fa
        Respond b  fb' -> Respond b  fb'
        M          m   -> M          m
        Pure    r      -> Pure    r ) >>= \x -> f x >>= g
        
    -- case statement = id
    = p >>= \x -> f x >>= g
    
    -- Goal complete

# Respond Category

    Define:
    
    respond
        :: (Monad m)
        =>  a -> Proxy x' x a' a m a'
    respond a = Respond a Pure
    
    (/>/)
        :: (Monad m)
        => (a -> Proxy x' x b' b m a')
        -> (b -> Proxy x' x c' c m b')
        -> (a -> Proxy x' x c' c m a')
    (fa />/ fb) a = fa a //> fb
    
    (//>)
        :: (Monad m)
        =>       Proxy x' x b' b m a'
        -> (b -> Proxy x' x c' c m b')
        ->       Proxy x' x c' c m a'
    p //> fb = case p of
        Request x' fx  -> Request x' (\x -> fx x //> fb)
        Respond b  fb' -> fb b >>= \b' -> fb' b' //> fb
        M          m   -> M (m >>= \p' -> return (p' //> fb)
        Pure    a      -> Pure a

## Right Identity Law

    Goal: respond />/ fb = fb
    
    respond />/ fb
    
    -- Definition of `(/>/)`
    = \b -> respond b //> fb
    
    -- [Respond Category - Left Identity Law - Pointful]
    = \b -> fb b
    
    -- Eta reduce
    = fb
    
    -- Goal complete

### Pointful

    Goal: respond b //> fb = fb b
    
    -- Definition of `respond`
    = Respond b Pure //> fb
    
    -- Definition of `(//>)`
    = fb b >>= \b' -> Pure b' //> fb
    
    -- Definition of `(//>)`
    = fb b >>= \b' -> Pure b'
    
    -- Eta reduce
    = fb b >>= Pure
    
    -- Definition of `return` (in reverse)
    = fb b >>= return
    
    -- [Kleisli Category - Right Identity Law - Pointful]
    = fb b
    
    -- Goal complete

## Left Identity Law

    Goal: fa />/ respond = fa
        
    fa />/ respond
    
    -- Definition of '(/>/)'
    = \a -> fa a //> respond
    
    -- [Respond Category - Left Identity Law - Pointful]
    = \a -> fa a
    
    -- Eta reduce
    = fa
    
    -- Goal complete

### Pointful
    
    Goal: "Pointful": p //> respond = p
    
    p //> respond
    -- Definition of `(//>)`
    = case p of
        Request x' fx  -> Request x' (\x -> fx x //> respond)
        
                        -- Coinduction: Reuse the premise
                        = Request x' (\x -> fx x)
                        
                        -- Eta reduce
                        = Request x' fx
                        
        Respond b  fb' -> respond b >>= \b' -> fb' b' //> respond
        
                        -- Coinduction: Reuse the premise
                        = respond b >>= \b' -> fb' b'
                        
                        -- Eta reduce
                        = respond b >>= fb'
                        
                        -- Definition of `respond`
                        = Respond b Pure >>= fb'
                        
                        -- Definition of `(>>=)`
                        = Respond b (\b' -> Pure b' >>= fb')
                        
                        -- Definition of `return`, backwards
                        = Respond b (\b' -> return b' >>= fb')
                        
                        -- [Kleisli Category - Left Identity Law - Pointful]
                        = Respond b (\b' -> fb' b')
                        
                        -- Eta reduce
                        = Respond b fb'
                        
        M          m   -> M (m >>= \p' -> return (p' //> respond))
        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return p')
                        
                        -- Eta reduce
                        = M (m >>= return)
                        
                        -- [Kleisli Category - Right Identity Law - Pointful]
                        = M m
                        
        Pure    a'      -> Pure a'
    
    -- Clean up
    = case p of
        Request x' fx  -> Request x' fx
        Respond b  fb' -> Respond b  fb'
        M          m   -> M           m
        Pure    a'     -> Pure    a'
        
    -- case statement = id
    = p
    
    -- Goal complete

## Associativity Law

    Goal: (fa />/ fb) />/ fc = fa />/ (fb />/ fc)

    (fa />/ fb) />/ fc
    
    -- Definition of `(/>/)`
    \a -> ((fa />/ fb) a) //> fc
    
    -- Definition of `(/>/)`
    \a -> (fa a //> fb) //> fc
    
    -- [Respond Category - Associativity Law - Pointful]
    \a -> fa a //> \b -> fb b //> fc
    
    -- Definition of `(/>/)`, in reverse
    \a -> fa a //> (fb />/ fc)
    
    -- Definition of `(/>/)`, in reverse
    fa />/ (fb />/ fc)
    
    -- Goal complete
    
### Pointful

    Goal: (p //> fb) //> fc = p //> \b -> fb b //> fc
    
    (p //> fb) //> fc
    
    -- Definition of `(//>)`
    = (case p of
        Request x' fx  -> Request x' (\x -> fx x //> fb)
        Respond b  fb' -> fb b >>= \b' -> fb' b' //> fb
        M          m   -> M (m >>= \p' -> return (p' //> fb))
        Pure    a'     -> Pure a' ) //> fc
        
    -- Distribute over case statement
    = case p of
        Request x' fx  -> Request x' (\x -> fx x //> fb) //> fc
        
                        -- Definition of `(//>)`
                        = Request x' (\x -> (fx x //> fb) //> fc)
                        
                        -- Coinduction: Reuse the premise
                        = Request x' (\x -> fx x //> \b -> fb b //> fc)
                        
                        -- Definition of `(//>)`, in reverse
                        = Request x' fx //> \b -> fb //> fc
                        
        Respond b  fb' -> (fb b >>= \b' -> fb' b' //> fb) //> fc
        
                        -- [Respond Category - Distributivity Law - Pointful]
                        = (fb b //> fc) >>= \b' -> (fb' b' //> fb) //> fc
                        
                        -- Coinduction: Reuse the premise
                        = (fb b //> fc) >>= \b' -> fb' b' //> \b -> fb b //> fc
                        
                        -- [Kleisli Category - Right Identity Law - Pointful], in reverse
                        = ((fb b //> fc) >>= return) >>= \b' -> fb' b' //> \b -> fb b //> fc
                        
                        -- Definition of `return`
                        = ((fb b //> fc) >>= Pure) >>= \b' -> fb' b' //> \b -> fb b //> fc
                        
                        -- Eta expand
                        = ((fb b //> fc) >>= \r -> Pure r) >>= \b' -> fb' b' //> \b -> fb b //> fc
                        
                        -- Definition of `(//>)` in reverse
                        = ((fb b //> fc) >>= \r -> Pure r //> \b -> fb b //> fc) >>= \b' -> fb' b' //> \b -> fb b //> fc
                        
                        -- Definition of `(//>)` in reverse
                        = (Respond b Pure //> \b -> fb b //> fc) >>= \b' -> fb' b' //> \b -> fb b //> fc
                        
                        -- [Respond Category - Distributivity Law - Pointful], in reverse
                        = (Respond b Pure >>= \b' -> fb' b') //> \b -> fb b //> fc
                        
                        -- Eta reduce
                        = (Respond b Pure >>= fb') //> \b -> fb b //> fc
                        
                        -- Definition of `(>>=)`
                        = Respond b (\b' -> Pure b' >>= fb') //> \b -> fb b //> fc
                        
                        -- Definition of `(>>=)`
                        = Respond b (\b' -> fb' b') //> \b -> fb b //> fc
                        
                        -- Eta reduce
                        = Respond b fb' //> \b -> fb b //> fc
                        
        M          m   -> M (m >>= \p' -> return (p' //> fb)) //> fc
        
                        -- Definition of `(//>)`
                        = M (m >>= \p' -> return ((p' //> fb) //> fc))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return (p' //> \b -> fb b //> fc))
                        
                        -- Definition of `(//>)`, in reverse
                        = M m //> \b -> fb b //> fc
                        
        Pure    a'      = Pure a' //> fc
        
                        -- Definition of `(//>)`
                        = Pure a'
                        
                        -- Definition of `(//>)`, in reverse
                        = Pure a' //> \b -> fb b //> fc
                        
    -- Clean up
    = case p of
        Request x' fx  -> Request x' fx  //> \b -> fb b //> fc
        Respond b  fb' -> Respond b  fb' //> \b -> fb b //> fc
        M          m   -> M          m   //> \b -> fb b //> fc
        Pure       a'  -> Pure    a'     //> \b -> fb b //> fc
        
    -- Factor from case statement
    = (case p of
        Request x' fx  -> Request x' fx
        Respond b  fb' -> Respond b  fb'
        M          m   -> M          m
        Pure       a'  -> Pure    a' ) //> \b -> fb b //> fc
        
    -- case statement = id
    = p //> \b -> fb b //> fc
    
    -- Goal complete

## Distributivity Law

    Goal: (k1 >=> k2) />/ fb = (k1 />/ fb) >=> (k2 />/ fb)
    
    (k1 >=> k2) />/ fb
    
    -- Definition of `(/>/)`
    = \a -> ((k1 >=> k2) a) //> fb
    
    -- Definition of `(>=>)`
    = \a -> (k1 a >>= k2) //> fb
    
    -- [Respond Category - Distributivity Law - Pointful]
    = \a -> (k1 a //> fb) >>= \r -> k2 r //> fb
    
    -- Definition of `(/>/)`, in reverse
    = \a -> (k1 />/ fb) a >>= \r -> k2 r //> fb
    
    -- Definition of `(/>/)`, in reverse
    = \a -> (k1 />/ fb) a >>= (k2 />/ fb)
    
    -- Definition of `(>=>)`, in reverse
    = (k1 />/ fb) >=> (k2 />/ fb)
    
    -- Goal complete
    
### Pointful

    Goal: (p >>= k) //> fb = (p //> fb) >>= \r -> k r //> fb
    
    (p >>= k) //> fb
    
    -- Definition of `(>>=)`
    = (case p of
        Request x' fx  -> Request x' (\x  -> fx  x  >>= k)
        Respond b  fb' -> Respond b  (\b' -> fb' b' >>= k)
        M          m   -> M (m >>= \p' -> return (p' >>= k))
        Pure    r      -> k r ) //> fb
        
    -- Distribute over case statement
    = case p of
        Request x' fx  -> Request x' (\x -> fx x >>= k) //> fb
        
                        -- Definition of `(//>)`
                        = Request x' (\x -> (fx x >>= k) //> fb)
                        
                        -- Coinduction: Reuse the premise
                        = Request x' (\x -> (fx x //> fb) >>= \r -> k r //> fb)
                        
                        -- Definition of `(>>=)`, in reverse
                        = Request x' (\x -> (fx x //> fb)) >>= \r -> k r //> fb
                        
                        -- Definition of `(//>)`, in reverse
                        = (Request x' fx //> fb) >>= \r -> k r //> fb
                        
        Respond b  fb' -> Respond b (\b' -> fb' b' >>= k) //> fb
        
                        -- Definition of `(//>)`
                        = fb b (\b' -> (fb' b' >>= k) //> fb)
                        
                        -- Coinduction: Reuse the premise
                        = fb b >>= \b' -> (fb' b' //> fb) >>= \r -> k r //> fb
                        
                        -- [Kleisli Category - Associativity Law - Pointful]
                        = (fb b >>= \b' -> fb' b' //> fb) >>= \r -> k r //> fb
                        
                        -- Definition of `(//>)`, in reverse
                        = (Respond b fb' //> fb) >>= \r -> k r //> fb
                        
        M          m   -> M (m >>= \p' -> return (p >>= k)) //> fb
        
                        -- Definition of `(//>)`
                        = M (m >>= \p' -> return ((p >>= k) //> fb))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return ((p //> fb) >>= \r -> k r //> fb))
                        
                        -- Definition of `(>>=)`, in reverse
                        = M (m >>= \p' -> return (p //> fb)) >>= \r -> k r //> fb
                        
                        -- Definition of `(//>)`, in reverse
                        = (M m //> fb) >>= \r -> k r //> fb
                        
        Pure    r      -> k r //> fb
        
                        -- Definition of `(>>=)`, in reverse
                        = Pure r >>= \r -> k r //> fb
                        
                        -- Definition of `(//>)`, in reverse
                        = (Pure r //> fb) >>= \r -> k r //> fb
                        
    -- Clean up
    = case p of
        Request x' fx  -> (Request x' fx  //> fb) >>= \r -> k r //> fb
        Respond b  fb' -> (Respond b  fb' //> fb) >>= \r -> k r //> fb
        M          m   -> (M          m   //> fb) >>= \r -> k r //> fb
        Pure    r      -> (Pure    r      //> fb) >>= \r -> k r //> fb
        
    -- Factor from case statement
    = ((case p of
        Request x' fx  -> Request x' fx
        Respond b  fb' -> Respond b  fb'
        M          m   -> M          m
        Pure    r      -> Pure    r ) //> fb) >>= \r -> k r //> fb
        
    -- case statement = id
    = (p //> fb) >>= \r -> k r //> fb
    
    -- Goal complete

## Zero Law

    Goal: return />/ f = return
    
    return />/ f
    
    -- Definition of `(/>/)`
    = \r -> return r //> f
    
    -- [Respond Category - Zero Law - Pointful]
    = \r -> return r
    
    -- Eta reduce
    = return
    
    -- Goal complete
    
### Pointful

    Goal: return r //> f = return r
    
    return r //> f
    
    -- Definition of `return`
    = Pure r //> f
    
    -- Definition of `(//>)`
    = Pure r
    
    -- Definition of `return`, in reverse
    = return r
    
    -- Goal complete

# Request Category

    Define:
    
    request
        :: (Monad m)
        =>  a' -> Proxy a' a y' y m a
    request a' = Request a' Pure
    
    (\>\)
        :: (Monad m)
        => (b' -> Proxy a' a y' y m b)
        -> (c' -> Proxy b' b y' y m c)
        -> (c' -> Proxy a' a y' y m c)
    (fb' \>\ fc') c' = fb' >\\ fc' c'
    
    (>\\)
        :: (Monad m)
        => (b' -> Proxy a' a y' y m b)
        ->        Proxy b' b y' y m c
        ->        Proxy a' a y' y m c
    fb' >\\ p = case p of
        Request b' fb  -> fb' b' >>= \b -> fb' >\\ fb b
        Respond y  fy' -> Respond y (\y' -> fb' >\\ fy' y')
        M          m   -> M (m >>= \p' -> return (fb' >\\ p'))
        Pure       c   -> Pure c

## Left Identity Law

    Goal: request \>\ fc' = fc'

    request \>\ fc' = fc'
    
    -- Definition of `(\>\)`
    = \c' -> request >\\ fc' c'
    
    -- [Request Category - Left Identity Law - Pointful]
    = \c' -> fc' c'
    
    -- Eta reduce
    = fc'
    
    -- Goal complete
    
### Pointful

    Goal: request >\\ p = p
    
    -- Definition of `(>\\)`
    case p of
        Request b' fb  -> request b' >>= \b -> request >\\ fb b
        
                        -- Coinduction: Reuse the premise
                        = request b' >>= \b -> fb b
                        
                        -- Eta reduce
                        = request b' >>= fb
                        
                        -- Definition of `request`
                        = Request b' Pure >>= fb
                        
                        -- Definition of `(>>=)`
                        = Request b' (\b -> Pure b >>= fb)
                        
                        -- Definition of `(>>=)`
                        = Request b' (\b -> fb b)
                        
                        -- Eta reduce
                        = Request b' fb
                        
        Respond y  fy' -> Respond y (\y' -> request >\\ fy' y')
        
                        -- Coinduction: Reuse the premise
                        = Respond y (\y' -> fy' y')
                        
                        -- Eta reduce
                        = Respond y fy'
                        
        M          m   -> M (m >>= \p' -> return (request >\\ p'))
        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return p')
                        
                        -- Eta reduce
                        = M (m >>= return)
                        
                        -- [Kleisli Category - Right Identity Law - Pointful]
                        = M m
                        
        Pure    c       = Pure c
    
    -- Clean up
    = case p of
        Request b' fb  -> Request b' fb
        Respond y  fy' -> Respond y  fy'
        M          m   -> M          m
        Pure    c      -> Pure    c
    
    -- case statement = if
    = p
    
    -- Goal complete

## Right Identity Law

    Goal: fb' \>\ request = fb'
    
    fb' \>\ request
    
    -- Definition of `(\>\)`
    = \b' -> fb' >\\ request b'
    
    -- [Request Category - Right Identity Law - Pointful]
    = \b' -> fb' b'
    
    -- Eta reduce
    = fb'
    
    -- Goal complete

### Pointful

    Goal: fb' >\\ request b' = fb' b'
    
    fb' >\\ request b'
    
    -- Definition of `request`
    = fb' >\\ Request b' Pure
    
    -- Definitoin of `(>\\)`
    = fb' b' >>= \b -> fb' >\\ Pure b
    
    -- Definition of `(>\\)`
    = fb' b' >>= \b -> Pure b
    
    -- Eta reduce
    = fb' b' >>= Pure
    
    -- Definition of `return`, in reverse
    = fb' b' >>= return
    
    -- [Kleisli Category - Right Identity Law - Pointful]
    = fb' b'
    
    -- Goal complete

## Associativity Law

    Goal: (f \>\ g) \>\ h = f \>\ (g \>\ h)
    
    (f \>\ g) \>\ h
    
    -- Definition of `(\>\)`
    = \x -> (f \>\ g) >\\ h x
    
    -- Definition of `(\>\)`
    = \x -> (\y -> f >\\ g y) >\\ h x
    
    -- [Request Category - Composition - Pointful]
    = \x -> f >\\ (g >\\ h x)
    
    -- Definition of `(\>\)`, in reverse
    = \x -> f >\\ (g \>\ h) x
    
    -- Definition of `(\>\)`, in reverse
    = f \>\ (g \>\ h)
    
    -- Goal complete
    
### Pointful

    Goal: fa' >\\ (fb' >\\ p) = (\b' -> fa' >\\ fb' b') >\\ p
    
    fa' >\\ (fb' >\\ p)
    
    -- Definition of `(>\\)`
    = fa' >\\ (case p of
        Request b' fb  -> fb' b' >>= \b -> fb' >\\ fb b
        Respond y  fy' -> Respond y (\y' -> fb' >\\ fy' y')
        M          m   -> M (m >>= \p' -> return (fb' >\\ p'))
        Pure    c      -> Pure c
        
    -- Distribute over case statement
    = case p of
        Request b' fb  -> fa' >\\ (fb' b' >>= \b -> fb' >\\ fb b)
        
                        -- [Request Category - Distributivity Law]
                        = (fa' >\\ fb' b') >>= \b -> fa' >\\ (fb' >\\ fb b)
                        
                        -- Coinduction : Reuse the premise
                        = (fa' >\\ fb' b') >>= \b -> (\b' -> fa' >\\ fb' b') >\\ fb b
                        
                        -- [Kleisli Category - Right Identity Law - Pointful], in reverse
                        = ((fa' >\\ fb' b') >>= return) >>= \b -> (\b' -> fa' >\\ fb' b') >\\ fb b
                        
                        -- Definition of `return`
                        = ((fa' >\\ fb' b') >>= Pure) >>= \b -> (\b' -> fa' >\\ fb' b') >\\ fb b
                        
                        -- Eta expand
                        = ((fa' >\\ fb' b') >>= \r -> Pure r) >>= \b -> (\b' -> fa' >\\ fb' b') >\\ fb b
                        
                        -- Definition of `(>\\)`, in reverse
                        = ((fa' >\\ fb' b') >>= \r -> (\b' -> fa' >\\ fb' b') >\\ Pure r) >>= \b -> (\b' -> fa' >\\ fb' b') >\\ fb b
                        
                        -- Definition of `(>\\)`, in reverse
                        = ((\b' -> fa' >\\ fb' b') >\\ Request b' Pure) >>= \b -> (\b' -> fa' >\\ fb' b') >\\ fb b
                        
                        -- [Request Category - Distributivity Law - Pointful], in reverse
                        = (\b' -> fa' >\\ fb' b') >\\ (Request b' Pure >>= fb)
                        
                        -- Definition of `(>>=)`
                        = (\b' -> fa' >\\ fb' b') >\\ Request b' (\b -> Pure b >>= fb)
                        
                        -- Definition of `(>>=)`
                        = (\b' -> fa' >\\ fb' b') >\\ Request b' (\b -> fb b)
                        
                        -- Eta reduce
                        = (\b' -> fa' >\\ fb' b') >\\ Request b' fb
                        
        Respond y  fy' -> fa' >\\ Respond y (\y' -> fb' >\\ fy' y')
        
                        -- Definition of `(>\\)`
                        = Respond y (\y' -> fa' >\\ (fb' >\\ fy' y'))
                        
                        -- Coinduction: Reuse the premise
                        = Respond y (\y' -> (\b' -> fa' >\\ fb' b') >\\ fy' y')
                        
                        -- Definition of `(>\\)`, in reverse
                        = (\b' -> fa' >\\ fb' b') >\\ Respond y fy'
                        
        M          m   -> fa' >\\ M (m >>= \p' -> return (fb' >\\ p'))
        
                        -- Definition of `(>\\)`
                        = M (m >>= \p' -> return (fa' >\\ (fb' >\\ p')))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return ((\b' -> fa' >\\ fb' b') >\\ p'))
                        
                        -- Definition of `(>\\)`, in reverse
                        = (\b' -> fa' >\\ fb' b') >\\ M m
                        
        Pure    c       = fa' >\\ Pure c
        
                        -- Definition of `(>\\)`
                        = Pure c
                        
                        -- Definition of `(>\\)`, in reverse
                        = (\b' -> fa' >\\ fb' b') >\\ Pure c
    
    -- Clean up
    = case p of
        Request b' fb  -> (\b' -> fa' >\\ fb' b') >\\ Request b' fb
        Request y  fy' -> (\b' -> fa' >\\ fb' b') >\\ Respond y  fy'
        M          m   -> (\b' -> fa' >\\ fb' b') >\\ M          m
        Pure    c      -> (\b' -> fa' >\\ fb' b') >\\ Pure    c
    
    -- Factor from case statement
    = (\b' -> fa' >\\ fb' b') >\\ (case p of
        Request b' fb  -> Request b' fb
        Respond y  fy' -> Respond y  fy'
        M          m   -> M          m
        Pure    c      -> Pure    c )
    
    -- case statement = id
    = (\b' -> fa' >\\ fb' b') >\\ p
    
    -- Goal complete
    
## Distributivity Law

    Goal: fb' \>\ (k1 >=> k2) = (fb' \>\ k1) >=> (fb' \>\ k2)
    
    fb' \>\ (k1 >=> k2)
    
    -- Definition of `(\>\)`
    = \x -> fb' >\\ ((k1 >=> k2) x)
    
    -- Definition of `(>=>)`
    = \x -> fb' >\\ (k1 x >>= k2)
    
    -- [Request Category - Distributivity Law - Pointful]
    = \x -> (fb' >\\ k1 x) >>= \y -> fb' >\\ k2 y
    
    -- Definition of `(\>\)`, in reverse
    = \x -> (fb' >\\ k1 x) >>= (fb' \>\ k2)
    
    -- Definition of `(\>\)`, in reverse
    = \x -> (fb' \>\ k1) x >>= (fb' \>\ k2)
    
    -- Definition of `(>=>)`, in reverse
    = (fb' \>\ k1) >=> (fb' \>\ k2)
    
    -- Goal complete

### Pointful

    Goal: fb' >\\ (p >>= k) = (fb' >\\ p) >>= \r -> fb' >\\ k r
    
    fb' >\\ (p >>= k)
    
    -- Definition of `(>>=)`
    = fb' >\\ (case p of
        Request b' fb  -> Request b' (\b  -> fb  b  >>= k)
        Respond y  fy' -> Respond y  (\y' -> fy' y' >>= k)
        M          m   -> M (m >>= \p' -> return (p' >>= k))
        Pure    c      -> k c )
    
    -- Distribute over case statement
    = case p of
        Request b' fb  -> fb' >\\ Request b' (\b -> fb b >>= k)
        
                        -- Definition of `(>\\)`
                        = fb' b' >>= \b -> fb' >\\ (fb b >>= k)
                        
                        -- Coinduction: Reuse the premise
                        = fb' b' >>= \b -> (fb' >\\ fb b) >>= \r -> fb' >\\ k r
                        
                        -- [Kleisli Category -- Associativity Law - Pointful]
                        = (fb' b' >>= \b -> fb' >\\ fb b) >>= \r -> fb' >\\ k r
                        
                        -- Definition of `(>\\)`, in reverse
                        = (fb' >\\ Request b' fb) >>= \r -> fb' >\\ k r
        
        Respond y  fy' -> fb' >\\ Respond y (\y' -> fy' y' >>= k)
                        
                        -- Definition of `(>\\)`
                        = Respond y (\y' -> fb' >\\ (fy' y' >>= k))
                        
                        -- Coinduction: Reuse the premise
                        = Respond y (\y' -> (fb' >\\ fy' y') >>= \r -> fb' >\\ k r)
                        
                        -- Definition of `(>>=)`, in reverse
                        = Respond y (\y' -> (fb' >\\ fy' y')) >>= \r -> fb' >\\ k r
                        
                        -- Definition of `(>\\)`, in reverse
                        = (fb' >\\ Respond y fy') >>= \r -> fb' >\\ k r
                        
        M          m   -> fb' >\\ M (m >>= \p' -> return (p' >>= k))
                        
                        -- Definition of `(>\\)`
                        = M (m >>= \p' -> return (fb' >\\ (p' >>= k)))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return ((fb' >\\ p') >>= \r -> fb' >\\ k r))
                        
                        -- Definition of `(>>=)`, in reverse
                        = M (m >>= \p' -> return (fb' >\\ p')) >>= \r -> fb' >\\ k r
                        
                        -- Definition of `(>\\)`, in reverse
                        = (fb' >\\ M m) >>= \r -> fb' >\\ k r
                        
        Pure    c       = fb' >\\ k c
        
                        -- [Kleisli Category - Left Identity Law - Pointful], in reverse
                        = return c >>= \r -> fb' >\\ k r
                        
                        -- Definition of `return`
                        = Pure c >>= \r -> fb' >\\ k r
                        
                        -- Definition of `(>\\)`, in reverse
                        = (fb' >\\ Pure c) >>= \r -> fb' >\\ k r

    -- Clean up
    = case p of
        Request b' fb  -> (fb' >\\ Request b' fb ) >>= \r -> fb' >\\ k r
        Respond y  fy' -> (fb' >\\ Respond y  fy') >>= \r -> fb' >\\ k r
        M          m   -> (fb' >\\ M          m  ) >>= \r -> fb' >\\ k r
        Pure    c      -> (fb' >\\ Pure    c     ) >>= \r -> fb' >\\ k r
    
    -- Factor from case statement
    = (fb' >\\ case p of
        Request b' fb  -> Request b' fb
        Respond y  fy' -> Respond y  fy'
        M          m   -> M          m
        Pure    c      -> Pure    c ) >>= \r -> fb' >\\ k r
    
    -- case statement = id
    = (fb' >\\ p) >>= \r -> fb' >\\ k r
    
    -- Goal complete

## Zero Law

    Goal: f \>\ return = return
    
    f \>\ return
    
    -- Definition of `(\>\)`
    = \r -> f >\\ return r
    
    -- [Request Category - Zero Law - Pointful]
    = \r -> return r
    
    -- Eta reduce
    = return
    
    -- Goal complete
    
### Pointful

    Goal: f >\\ return r = return r
    
    f >\\ return r
    
    -- Definition of `return`
    = f >\\ Pure r
    
    -- Definition of `(>\\)`
    = Pure r
    
    -- Definition of `return`, in reverse
    = return r
    
    -- Goal complete

# Pull Category

    Define:
    
    pull
        :: (Monad m)
        =>   a' -> Proxy a' a a' a m r
    pull a' = Request a' push
    
    (>+>)
        :: (Monad m)
        => ( b' -> Proxy a' a b' b m r)
        -> (_c' -> Proxy b' b c' c m r)
        -> (_c' -> Proxy a' a c' c m r)
    (fb' >+> fc') c' = fb' +>> fc' c'
    
    (+>>)
        :: (Monad m)
        => ( b' -> Proxy a' a b' b m r)
        ->         Proxy b' b c' c m r
        ->         Proxy a' a c' c m r
    fb' +>> p = case p of
        Request b' fb  -> fb' b' >>~ fb
        Respond c  fc' -> Respond c (\c' -> fb' +>> fc' c')
        M          m   -> M (m >>= \p' -> return (fb' +>> p'))
        Pure       r   -> Pure r
        
## Left Identity Law

    Goal: pull >+> f = f
    
    pull >+> f
    
    -- Definition of `(>+>)`
    = \c' -> pull +>> f c'
    
    -- [Pull Category - Left Identity Law - Pointful]
    = \c' -> f c'
    
    -- Eta reduce
    = f
    
    -- Goal complete
    
### Pointful

    Goal: pull +>> p = p
    
    pull +>> p
    
    -- Definition of `(+>>)`
    = case p of
        Request b' fb  -> pull b' >>~ fb
                        
                        -- Definition of `pull`
                        = Request b' (\b -> Respond b pull) >>~ fb
                        
                        -- Definition of `(>>~)`
                        = Request b' (\b -> Respond b pull >>~ fb)
                        
                        -- Definition of `(>>~)`
                        = Request b' (\b -> pull +>> fb b)
                        
                        -- Coinduction: Reuse the premise
                        = Request b' (\b -> fb b)
                        
                        -- Eta reduce
                        = Request b' fb
                        
        Respond c  fc' -> Respond c (\c' -> pull +>> fc' c')
        
                        -- Coinduction: Reuse the premise
                        = Respond c (\c' -> fc' c')
                        
                        -- Eta reduce
                        = Respond c fc'
                        
        M          m   -> M (m >>= \p' -> return (pull +>> p'))
        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return p')
                        
                        -- Eta reduce
                        = M (m >>= return)
                        
                        -- [Kleisli Category - Right Identity Law - Pointful]
                        = M m
                        
        Pure    r      -> Pure r
    
    -- Clean up
    = case p of
        Request b' fb  -> Request b' fb
        Respond c  fc' -> Respond c  fc'
        M          m   -> M          m
        Pure    r      -> Pure r
    
    -- case statement = id
    = p
    
    -- Goal complete
    
## Right Identity Law

    Goal: fb' >+> pull = fb'
    
    fb' >+> pull
    
    -- Definition of `(>+>)`
    = \b' -> fb' +>> pull b'
    
    -- [Pull Category - Right Identity Law - Pointful]
    = \b' -> fb' b'
    
    -- Eta reduce
    = fb'
    
    -- Goal complete
    
### Pointful

    Goal: fb' +>> pull b' = fb' b'
    
    fb' +>> pull b'
    
    -- Definition of `push`
    = fb' +>> Request b' (\b -> Respond b pull)
    
    -- Definition of `(+>>)`
    = fb' b' >>~ \b -> Respond b pull
    
    -- Definition of `push`, in reverse
    = fb' b' >>~ push
    
    -- Coinduction: [Push Category - Right Identity Law - Pointful]
    = fb' b'
    
    -- Goal complete

## Associativity Law

    Goal: fb' >+> (fc' >+> fd') = (fb' >+> fc') >+> fd'
    
    fb' >+> (fc' >+> fd')
    
    -- Definition of `(>+>)`
    = \d' -> fb' +>> (fc' >+> fd') d'
    
    -- Definition of `(>+>)`
    = \d' -> fb' +>> (fc' +>> fd' d')
    
    -- [Pull Category - Associativity Law - Pointful]
    = \d' -> (\c' -> fb' +>> fc' c') +>> fd' d'
    
    -- Definition of `(>+>)`, in reverse
    = \d' -> (fb' >+> fc') +>> fd' d'
    
    -- Definition of `(>+>)`, in reverse
    = (fb' >+> fc') >+> fd'
    
    -- Goal complete
    
### Pointful

    Goal: fb' +>> (fc' +>> p) = (\c' -> fb' +>> fc' c') +>> p
    
    fb' +>> (fc' +>> p)
    
    -- Definition of `(+>>)`
    = fb' +>> (case p of
        Request c' fc  -> fc' c' >>~ fc
        Respond d  fd' -> Respond d (\d' -> fc' +>> fd' d')
        M          m   -> M (m >>= \p' -> return (fc' +>> p'))
        Pure    r      -> Pure r )
        
    -- Distribute over case statement
    = case p of
        Request c' fc  -> fb' +>> (fc' c' >>~ fc)
        
                        -- Coinduction: [Push/Pull - Associativity - Pointful]
                        = (fb' +>> fc' c') >>~ fc
                       
                        -- Definition of `(+>>), in reverse
                        = (\c' -> fb' +>> fc' c') +>> Request c' fc
                       
        Respond d  fd' -> fb' +>> Respond d (\d' -> fc' +>> fd' d')
        
                        -- Definition of `(+>>)`
                        = Respond d (\d' -> fb' +>> (fc' +>> fd' d'))
                        
                        -- Coinduction: Reuse the premise
                        = Respond d (\d' -> (\c' -> fb' +>> fc' c') +>> fd' d')
                        
                        -- Definition of `(+>>)`, in reverse
                        = (\c' -> fb' +>> fc' c') +>> Respond d fd'
        
        M          m   -> fb' +>> M (m >>= \p' -> return (fc' +>> p'))
        
                        -- Definition of `(+>>)`
                        = M (m >>= \p' -> return (fb' +>> (fc' +>> p')))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return ((\c' -> fb' +>> fc' c') +>> p'))
                        
                        -- Definition of `(+>>)`, in reverse
                        = (\c' -> fb' +>> fc' c') +>> M m
                        
        Pure    r      -> fb' +>> Pure r
        
                       -- Definition of `(+>>)`
                       = Pure r
                       
                       -- Definition of `(+>>)`, in reverse
                       = (\c' -> fb' +>> fc' c') +>> Pure r
                       
    -- Clean up
    = case p of
        Request c' fc  -> (\c' -> fb' +>> fc' c') +>> Request c' fc
        Respond d  fd' -> (\c' -> fb' +>> fc' c') +>> Respond d  fd'
        M          m   -> (\c' -> fb' +>> fc' c') +>> M          m
        Pure       r   -> (\c' -> fb' +>> fc' c') +>> Pure    r
    
    -- Factor from case statement
    = (\c' -> fb' +>> fc' c') +>> (case p of
        Request c' fc  -> Request c' fc
        Respond d  fd' -> Respond d  fd'
        M          m   -> M          m
        Pure    r      -> Pure    r )
    
    -- case statement = id
    = (\c' -> fb' +>> fc' c') +>> p
    
    -- Goal complete

# Push Category

    Define:

    push
        :: (Monad m)
        =>   a -> Proxy a' a a' a m r
    push a = Respond a pull

    (>~>)
        :: (Monad m)
        => (_a -> Proxy a' a b' b m r)
        -> ( b -> Proxy b' b c' c m r)
        -> (_a -> Proxy a' a c' c m r)
    (f >~> g) x = f x >>~ g

    (>>~)
        :: (Monad m)
        =>        Proxy a' a b' b m r
        -> ( b -> Proxy b' b c' c m r)
        ->        Proxy a' a c' c m r
    p >>~ fb = case p of
        Request a' fa  -> Request a' (\a -> fa a >>~ fb)
        Respond b  fb' -> fb' +>> fb b
        M          m   -> M (m >>= \p' -> return (p' >>~ fb))
        Pure    r      -> Pure r

## Left Identity Law

    Goal: push >~> f = f
    
    push >~> f
    
    -- Definition of `(>~>)`
    = \a -> push a >>~ f
    
    -- [Push Category - Left Identity Law - Pointful]
    = \a -> f a
    
    -- Eta reduce
    = f
    
    -- Goal complete
    
### Pointful

    Goal: push a >>~ f = f a
    
    push a >>~ f
    
    -- Definition of `push`
    = Respond a pull >>~ f
    
    -- Definition of `(>>~)`
    = pull +>> f a
    
    -- Coinduction: [Pull Category - Left Identity Law - Pointful]
    = f a
    
    -- Goal complete

## Right Identity Law

    Goal: f >~> push = f
    
    f >~> push
    
    -- Definition of `(>~>)`
    = \a -> f a >>~ push
    
    -- [Push Category - Right Identity Law - Pointful]
    = \a -> f a
    
    -- Eta reduce
    = f
    
    -- Goal complete
    
### Pointful

    Goal: p >>~ push = p
    
    p >>~ push
    
    -- Definition of `(>>~)`
    = case p of
        Request a' fa  -> Request a' (\a -> fa a >>~ push)
         
                        -- Coinduction: Reuse the premise
                        = Request a' (\a -> fa a)
                        
                        -- Eta reduce
                        = Request a' fa
                       
        Respond b  fb' -> fb' +>> push b
        
                        -- Definition of `push`
                        = fb' +>> Respond b pull
                        
                        -- Definition of `(+>>)`
                        = Respond b (fb' +>> pull)
                        
                        -- [Pull Category - Right Identity Law - Pointful]
                        = Respond b fb'
                        
        M          m   -> M (m >>= \p' -> return (p >>~ push))
        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return p)
                        
                        -- Eta reduce
                        = M (m >>= return)
                        
                        -- [Kleisli Category - Right Identity Law - Pointful]
                        = M m
                        
        Pure    r      -> Pure r
    
    -- Clean up
    = case p of
        Request a' fa  -> Request a' fa
        Respond b  fb' -> Respond b  fb'
        M          m   -> M          m
        Pure    r      -> Pure    r
    
    -- case statement = id
    = p
    
    -- Goal complete
    
## Associativity Law

    Goal: (fa >~> fb) >~> fc = fa >~> (fb >~> fc)
    
    (fa >~> fb) >~> fc
    
    -- Definition of `(>~>)`
    = \a -> (fa >~> fb) a >>~ fc
    
    -- Definition of `(>~>)`
    = \a -> (fa a >>~ fb) >>~ fc
    
    -- [Push Category - Associativity Law - Pointful]
    = \a -> fa a >>~ \b -> fb b >>~ fc
    
    -- Definition of `(>~>)`, in reverse
    = \a -> fa a >>~ (fb >~> fc)
    
    -- Definition of `(>~>)`, in reverse
    = fa >~> (fb >~> fc)`
    
    -- Goal complete
    
### Pointful

    Goal: (p >>~ fb) >>~ fc = p >>~ \b -> fb b >>~ fc
    
    (p >>~ fb) >>~ fc
    
    -- Definition of `(>>~)`
    = (case p of
        Request a' fa  -> Request a' (\a -> fa a >>~ fb)
        Respond b  fb' -> fb' +>> fb b
        M          m   -> M (m >>= \p' -> return (p' >>~ fb))
        Pure    r      -> Pure r ) >>~ fc
        
    -- Distribute over case statement
    = case p of
        Request a' fa  -> Request a' (\a -> fa a >>~ fb) >>~ fc
        
                        -- Definition of `(>>~)`
                        = Request a' (\a -> (fa a >>~ fb) >>~ fc)
                        
                        -- Coinduction: Reuse the premise
                        = Request a' (\a -> fa a >>~ \b -> fb b >>~ fc)
                        
                        -- Definition of `(>>~), in reverse
                        = Request a' fa >>~ \b -> fb b >>~ fc
                       
        Respond b  fb' -> (fb' +>> fb b) >>~ fc
        
                        -- Coinduction: [Push/Pull - Associativity - Pointful]
                        = fb' +>> (fb b >>~ fc)
                        
                        -- Definition of `(>>~)`, in reverse
                        = Respond b fb' >>~ \b -> fb b >>~ fc
                        
        M          m   -> M (m >>= \p' -> return (p' >>~ fb)) >>~ fc
        
                        -- Definition of `(>>~)`
                        = M (m >>= \p' -> return ((p >>~ fb) >>~ fc))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return (p >>~ \b -> fb b >>~ fc))
                        
                        -- Definition of `(>>~), in reverse
                        = M m >>~ \b -> fb b >>~ fc
                        
        Pure    r      -> Pure r >>~ fc
        
                        -- Definition of `(>>~)`
                        = Pure r
                        
                        -- Definition of `(>>~)`, in reverse
                        = Pure r >>~ \b -> fb b >>~ fc
                        
    -- Clean up
    = case p of
        Request a' fa  -> Request a' fa  >>~ \b -> fb b >>~ fc
        Respond b  fb' -> Respond b  fb' >>~ \b -> fb b >>~ fc
        M          m   -> M          m   >>~ \b -> fb b >>~ fc
        Pure    r      -> Pure    r      >>~ \b -> fb b >>~ fc
        
    -- Factor from case statement
    = (case p of
        Request a' fa  -> Request a' fa
        Respond b  fb' -> Respond b  fb'
        M          m   -> M          m
        Pure    r      -> Pure    r ) >>~ \b -> fb b >>~ fc
        
    -- case statement = id
    = p >>~ \b -> fb b >>~ fc
    
    -- Goal complete
        
# Push/Pull

## Associativity

    Goal: (f >+> g) >~> h = f >+> (g >~> h)
    
    (f >+> g) >~> h
    
    -- Definition of `(>~>)`
    = \x -> (f >+> g) x >>~ h
    
    -- Definition of `(>+>)`
    = \x -> (f +>> g x) >>~ h
    
    -- [Push/Pull - Associativity - Pointful]
    = \x -> f +>> (g x >>~ h)
    
    -- Definition of `(>~>)`, in reverse
    = \x -> f +>> (g >~> h)x
    
    -- Definition of `(>+>)`, in reverse
    = f >+> (g >~> h)
    
    -- Goal complete

### Pointful

    Goal: (fb' +>> p) >>~ fc = fb' +>> (p >>~ fc)
    
    (fb' +>> p) >>~ fc
    
    -- Definition of `(+>>)`
    = (case p of
        Request b' fb  -> fb' b' >>~ fb
        Respond c  fc' -> Respond c (\c' -> fb' +>> fc' c')
        M          m   -> M (m >>= \p' -> return (fb' +>> p'))
        Pure    r      -> Pure r ) >>~ fc
        
    -- Distribute over case statement
    = case p of
        Request b' fb  -> (fb' b' >>~ fb) >>~ fc
        
                        -- Coinduction: [Push Category - Associativity Law - Pointful]
                        = fb' b' >>~ \b -> fb b >>~ fc
                        
                        -- Definition of `(+>>)`, in reverse
                        = fb' +>> Request b' (\b -> fb b >>~ fc)
                       
                        -- Definition of `(>>~)`, in reverse
                        = fb' +>> (Request b' fb >>~ fc)
                       
        Respond c  fc' -> Respond c (\c' -> fb' +>> fc' c') >>~ fc
        
                        -- Definition of `(>>~)`
                        = (\c' -> fb' +>> fc' c') +>> fc c
                        
                        -- Coinduction: [Pull Category - Associativity Law - Pointful]
                        = fb' +>> (fc' +>> fc c)
                        
                        -- Definition of `(>>~)`, in reverse
                        = fb' +>> (Respond c fc' >>~ fc)
                        
        M          m   -> M (m >>= \p' -> return (fb' +>> p')) >>~ fc
        
                        -- Definition of `(>>~)`
                        = M (m >>= \p' -> return ((fb' +>> p') >>~ fc))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return (fb' +>> (p' >>~ fc))
                        
                        -- Definition of `(+>>)`, in reverse
                        = fb' +>> M (m >>= \p' -> return (p' >>~ fc))
                        
                        -- Definition of `(>>~)`, in reverse
                        = fb' +>> (M m >>~ fc)
                        
        Pure    r      -> Pure r >>~ fc
         
                        -- Definition of `(+>>)`, in reverse
                        = fb' +>> (Pure r >>~ fc)
                        
    -- Clean up
    = case p of
        Request b' fb  -> fb' +>> (Request b' fb  >>~ fc)
        Respond c  fc' -> fb' +>> (Respond c  fc' >>~ fc)
        M          m   -> fb' +>> (M          m   >>~ fc)
        Pure    r      -> fb' +>> (Pure    r      >>~ fc)
        
    -- Factor from case statement
    = fb' +>> ((case p of
        Request b' fb  -> Request b' fb
        Respond c  fc' -> Respond c  fc'
        M          m   -> M          m
        Pure    r      -> Pure    r ) >>~ fc)
        
    -- case statement = id
    = fb' +>> (p >>~ fc)
    
    -- Goal complete
    
# Duals

    Define:
    
    reflect
        :: (Monad m)
        => Proxy a' a b' b m r
        -> Proxy b b' a a' m r
    reflect p = case p of
        Request a' fa  -> Respond a' (\a  -> go (fa  a ))
        Respond b  fb' -> Request b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure    r      -> Pure r

## Request Identity

    Goal: reflect . request = respond
    
    reflect . request
    
    -- Definition of `(.)`
    = \a' -> reflect (request a')
    
    -- [Dual - Request Identity - Pointful]
    = \a' -> respond a'
    
    -- Eta reduce
    = respond
    
    -- Goal complete

### Pointful

    Goal: reflect (request x) = respond x
    
    reflect (request x)
    
    -- Definition of `request`
    = reflect (Request x Pure)
    
    -- Definition of `reflect`
    = Respond x (\r -> reflect (Pure r))
    
    -- Definition of `reflect`
    = Respond x (\r -> Pure r)
    
    -- Eta reduce
    = Respond x Pure
    
    -- Definition of `respond`, in reverse
    = respond
    
    -- Goal complete

## Request Composition

    Goal: reflect . (f \>\ g) = reflect . g />/ reflect . f
    
    reflect . (f \>\ g)
    
    -- Definition of `(.)`
    = \a -> reflect ((f \>\ g) a)
    
    -- Definition of `(\>\)`
    = \a -> reflect (f >\\ g a)
    
    -- [Dual - Request Composition - Pointful]
    = \a -> reflect (g a) //> reflect . f
    
    -- Definition of `(.)`, in reverse
    = \a -> (reflect . g) a //> reflect . f
    
    -- Definition of `(/>/)`, in reverse
    = reflect . g />/ reflect . f
    
    -- Goal complete
    
### Pointful

    Goal: reflect (f >\\ p) = reflect p //> reflect . f
    
    reflect (f >\\ p)
    
    -- Definition of `(>\\)`
    = reflect (case p of
        Request b' fb  -> f b' >>= \b -> f >\\ f b
        Respond x  fx' -> Respond x (\x' -> f >\\ fx' x')
        M          m   -> M (m >>= \p' -> return (f >\\ p'))
        Pure       c   -> Pure c )
        
    -- Distribute over case statement
    = case p of
        Request b' fb  -> reflect (f b' >>= \b -> f >\\ fb b)
        
                        -- [Dual - Distributivity Law - Pointful]
                        = reflect (f b') >>= \b -> reflect (f >\\ fb b)
                        
                        -- Coinduction: Reuse the premise
                        = reflect (f b') >>= \b -> reflect (fb b) //> reflect . f
                        
                        -- Definition of `(.)`, in reverse
                        = (reflect . f) b' >>= \b -> reflect (fb b) //> reflect . f
                        
                        -- Definition of `(//>)`, in reverse
                        = Respond b' (\b -> reflect (fb b)) //> reflect . f
                        
                        -- Definition of `reflect`, in reverse
                        = reflect (Request b' fb) //> reflect . f
                        
        Respond x  fx' -> reflect (Respond x (\x' -> f >\\ fx' x'))
        
                        -- Definition of `reflect`
                        = Request x (\x' -> reflect (f >\\ fx' x'))
                        
                        -- Coinduction: Reuse the premise
                        = Request x (\x' -> reflect (fx' x') //> reflect . f)
                        
                        -- Definition of `(//>)`, in reverse
                        = Request x (\x' -> reflect (fx' x')) //> reflect . f
                        
                        -- Definition of `reflect`, in reverse
                        = reflect (Respond x fx') //> reflect . f
                        
        M          m   -> reflect (M (m >>= \p' -> return (f >\\ p')))
        
                        -- Definition of `reflect`
                        = M (m >>= \p' -> return (reflect (f >\\ p')))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return (reflect p' //> reflect . f))
                        
                        -- Definition of `(//>)`, in reverse
                        = M (m >>= \p' -> return (reflect p')) //> reflect . f
                        
                        -- Definition of `reflect, in reverse
                        = reflect (M m) //> reflect . f
                        
        Pure    c       = reflect (Pure c)
        
                        -- Definition of `reflect
                        = Pure c
                        
                        -- Definition of `(//>)`, in reverse
                        = Pure c //> reflect . f
                        
                        -- Definition of `reflect`, in reverse
                        = reflect (Pure c) //> reflect .f
                        
    -- Clean up
    = case p of
        Request b' fb  -> reflect (Request b' fb ) //> reflect . f
        Respond x  fx' -> reflect (Respond x  fx') //> reflect . f
        M          m   -> reflect (M          m  ) //> reflect . f
        Pure       c   -> reflect (Pure    c     ) //> reflect . f
        
    -- Factor from case statement
    = reflect (case p of
        Request b' fb  -> Request b' fb
        Respond x  fx' -> Respond x  fx'
        M          m   -> M          m
        Pure       c   -> Pure     c ) //> reflect . f
        
    -- case statement = id
    = reflect p //> reflect . f
    
    -- Goal complete

## Respond Identity

    Goal: reflect . respond = request
    
    reflect . respond
    
    -- Definition of `(.)`
    = \a -> reflect (respond a)
    
    -- [Dual - Respond Identity - Pointful]
    = \a -> request a
    
    -- Eta reduce
    = request
    
    -- Goal complete
    
### Pointful

    Goal: reflect (respond x) = request x
    
    reflect (respond x)
    
    -- Definition of `respond`
    = reflect (Respond x Pure)
    
    -- Definition of `reflect`
    = Request x (\r -> reflect (Pure r))
    
    -- Definition of `reflect`
    = Request x (\r -> Pure r)
    
    -- Eta reduce
    = Request x Pure
    
    -- Definition of `request`, in reverse
    = request
     
    -- Goal complete

## Respond Composition

    Goal: reflect . (f />/ g) = reflect . g \>\ reflect . f
    
    reflect . (f />/ g)
    
    -- Definition of `(.)`
    = \x -> reflect ((f />/ g) x)
    
    -- Definition of `(/>/)`
    = \x -> reflect (f x //> g)
    
    -- [Dual - Respond Composition - Pointful]
    = \x -> reflect . g >\\ reflect (f x)
    
    -- Definition of `(.)`, in reverse
    = \x -> reflect . g >\\ (reflect . f) x
    
    -- Definition of `(\>\)`, in reverse
    = reflect . g \>\ reflect . f
    
    -- Goal complete

### Pointful

    Goal: reflect (p //> f) = reflect . f >\\ reflect p
    
    reflect (p //> f)
    
    -- Definition of `(//>)`
    = reflect (case p of
        Request x' fx  -> Request x' (\x -> fx x //> f)
        Respond b  fb' -> f b >>= \b' -> fb' b' //> f
        M          m   -> M (m >>= \p' -> return (p' //> f))
        Pure    a'     -> Pure a' )
        
    -- Distribute over case statement
    = case p of
        Request x' fx  -> reflect (Request x' (\x -> fx x //> f))
        
                         -- Definition of `reflect`
                         = Respond x' (\x -> reflect (fx x //> f))
                         
                         -- Coinduction: Reuse the premise
                         = Respond x' (\x -> reflect . f >\\ reflect (fx x))
                         
                         -- Definition of `(>\\)`, in reverse
                         = reflect . f >\\ Respond x' (\x -> reflect (fx x))
                         
                         -- Definition of `reflect`, in reverse
                         = reflect . f >\\ reflect (Request x' fx)
                         
        Respond b  fb' -> reflect (f b >>= \b' -> fb' b' //> f)
        
                        -- [Dual - Distributivity Law - Pointful]
                        = reflect (f b) >>= \b' -> reflect (fb' b' //> f)
                        
                        -- Coinduction: Reuse the premise
                        = reflect (f b) >>= \b' -> reflect . f >\\ reflect (fb' b')
                        
                        -- Definition of `(.)`
                        = (reflect . f) b >>= \b' -> reflect . f >\\ reflect (fb' b')
                        
                        -- Definition of `(>\\)`, in reverse
                        = reflect . f >\\ (Request b (\b' -> reflect (fb' b')))
                        
                        -- Definition of `reflect`, in reverse
                        = reflect . f >\\ reflect (Respond b fb')
                        
        M          m    = reflect (M (m >>= \p' -> return (p' //> f)))
        
                        -- Definition of `reflect`
                        = M (m >>= \p' -> return (reflect (p' //> f)))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return (reflect . f >\\ reflect p'))
                        
                        -- Definition of `(>\\)`, in reverse
                        = reflect . f >\\ M (m >>= \p' -> return (reflect p'))
                        
                        -- Definition of `reflect`, in reverse
                        = reflect . f >\\ reflect (M m)
                        
        Pure    a'      = reflect (Pure a')
        
                        -- Definition of `reflect`
                        = Pure a'
                        
                        -- Definition of `(>\\)`, in reverse
                        = reflect . f >\\ Pure a'
                        
                        -- Definition of `reflect`, in reverse
                        = reflect . f >\\ reflect (Pure a')
                        
    -- Clean up
    = case p of
        Request x' fx  -> reflect . f >\\ reflect (Request x' fx )
        Respond b  fb' -> reflect . f >\\ reflect (Respond b  fb')
        M          m   -> reflect . f >\\ reflect (M          m  )
        Pure    a'     -> reflect . f >\\ reflect (Pure    a'    )
    
    -- Factor from case statement
    = reflect . f >\\ reflect (case p of
        Request x' fx  -> Request x' fx
        Respond b  fb' -> Respond b  fb'
        M          m   -> M          m
        Pure    a'     -> Pure    a' )
    
    -- case statement = id
    = reflect . f >\\ reflect p
    
    -- Goal complete
    
## Distributivity Law

    Goal: reflect . (f >=> g) = reflect . f >=> reflect . g
    
    reflect . (f >=> g)
    
    -- Definition of `(.)`
    = \x -> reflect ((f >=> g) x)
    
    -- Definition of `(>=>)`
    = \x -> reflect (f x >>= g)
    
    -- [Dual - Distributive Law - Pointful]
    = \x -> reflect (f x) >>= \y -> reflect (g y)
    
    -- Definition of `(.)`, in reverse
    = \x -> reflect (f x) >>= reflect . g
    
    -- Definition of `(.)`, in reverse
    = \x -> ((reflect . f) x >>= reflect . g)
    
    -- Definition of `(>=>)`, in reverse
    = reflect . f >=> reflect . g
    
    -- Goal complete
    
### Pointful

    Goal: reflect (p >>= f) = reflect p >>= \x -> reflect (f x)
    
    reflect (p >>= f)
    
    -- Definition of `(>>=)`
    = reflect (case p of
        Request a' fa  -> Request a' (\a  -> fa  a  >>= f)
        Respond b  fb' -> Respond b  (\b' -> fb' b' >>= f)
        M          m   -> M (m >>= \p' -> return (p' >>= f))
        Pure    r      -> f r )
        
    -- Distribute over case statement
    = case p of
        Request a' fa  -> reflect (Request a' (\a -> fa a >>= f))
        
                        -- Definition of `reflect`
                        = Respond a' (\a -> reflect (fa a >>= f))
                        
                        -- Coinduction: Reuse the premise
                        = Respond a' (\a -> reflect (fa a) >>= \x -> reflect (f x))
                        
                        -- Definition of `(>>=)`, in reverse
                        = Respond a' (\a -> reflect (fa a)) >>= \x -> reflect (f x)
                        
                        -- Definition of `reflect`, in revrse
                        = reflect (Request a' (\a -> fa a)) >>= \x -> reflect (f x)
                        
                        -- Eta reduce
                        = reflect (Request a' fa) >>= \x -> reflect (f x)
                        
        Respond b  fb' -> reflect (Respond b (\b' -> fb' b' >>= f))
        
                        -- Definition of `reflect`
                        = Request b (\b' -> reflect (fb' b' >>= f))
                        
                        -- Coinduction: Reuse the premise
                        = Request b (\b' -> reflect (fb' b') >>= \x -> reflect (f x))
                        
                        -- Definition of `(>>=)`, in reverse
                        = Request b (\b' -> reflect (fb' b')) >>= \x -> reflect (f x)
                        
                        -- Definition of `reflect`, in reverse
                        = reflect (Respond b (\b' -> fb' b')) >>= \x -> reflect (f x)
                        
                        -- Eta reduce
                        = reflect (Respond b fb') >>= \x -> reflect (f x)
                        
        M          m   -> reflect (M (m >>= \p' -> return (p' >>= f)))
        
                        -- Definition of `reflect`
                        = M (m >>= \p' -> return (reflect (p' >>= f)))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return (reflect p' >>= \x -> reflect (f x)))
                        
                        -- Definition of `(>>=)`, in reverse
                        = M (m >>= \p' -> return (reflect p')) >>= \x -> reflect (f x)
                        
                        -- Definition of `reflect`, in reverse
                        = reflect (M m) >>= \x -> reflect (f x)
                        
        Pure    r      -> reflect (f r)
        
                        -- [Kleisli Category - Left Identity - Pointful]
                        reflect (return r >>= f)
                        
                        -- Coinduction: Reuse the premise
                        reflect (return r) >>= \x -> return (f x)
                        
                        -- Definition of `return`
                        reflect (Pure r) >>= \x -> return (f x)
                        
    -- Cleanup
    = case p of
        Request a' fa  -> reflect (Request a' fa ) >>= \x -> reflect (f x)
        Respond b  fb' -> reflect (Respond b  fb') >>= \x -> reflect (f x)
        M          m   -> reflect (M          m  ) >>= \x -> reflect (f x)
        Pure    r      -> reflect (Pure    r     ) >>= \x -> reflect (f x)
        
    -- Factor from case statement
    = reflect (case p of
        Request a' fa  -> Request a' fa
        Respond b  fb' -> Respond b  fb'
        M          m   -> M          m
        Pure    r      -> Pure    r ) >>= \x -> reflect (f x)
        
    -- case statement = id
    = reflect p >>= \x -> reflect (f x)
    
    -- Goal complete
    
## Zero Law

    Goal: reflect . return = return
    
    reflect . return
    
    -- Definition of `(.)`
    = \r -> reflect (return r)
    
    -- [Dual - Zero Law - Pointful]
    = \r -> return r
    
    -- Eta reduce
    = return
    
    -- Goal complete
    
### Pointful

    Goal: reflect (return r) = return r
    
    reflect (return r)
    
    -- Definition of `return`
    = reflect (Pure r)
    
    -- Definition of `reflect`
    = Pure r
    
    -- Definition of `return`, in reverse
    = return r
    
    -- Goal complete
    
## Involution

    Goal: reflect . reflect = id
    
    reflect . reflect
    
    -- Definition of `(.)`
    = \p -> reflect (reflect p)
    
    -- [Dual - Involution]
    = \p -> p
    
    -- Definition of `id`
    = id
    
    -- Goal complete
    
### Pointful

    reflect (reflect p) = p
    
    reflect (reflect p)
    
    -- Definition of `reflect`
    = reflect (case p of
        Request a' fa  -> Respond a' (\a  -> reflect (fa  a ))
        Respond b  fb' -> Request b  (\b' -> reflect (fb' b'))
        M          m   -> M (m >>= \p' -> return (reflect p'))
        Pure    r      -> Pure r )
    
    -- Distribute over case statement
    = case p of
        Request a' fa  -> reflect (Respond a' (\a -> reflect (fa a))
        
                        -- Definition of `reflect`
                        = Request a' (\a -> reflect (reflect (fa  a)))
                        
                        -- Coinduction: Reuse the premise
                        = Request a' (\a -> fa a)
                        
                        -- Eta reduction
                        = Request a' fa
                        
        Respond b  fb' -> reflect (Request b (\b' -> reflect (fb' b')))
         
                        -- Definition of `reflect`
                        = Request b (\b' -> reflect (reflect (fb' b')))
                         
                        -- Coinduction: Reuse the premise
                        = Request b (\b' -> fb' b')
                         
                        -- Eta reduction
                        = Request b fb'
                         
        M          m   -> reflect (M (m >>= \p' -> return (reflect p')))
        
                        -- Definition of `reflect`
                        = M (m >>= \p' -> return (reflect (reflect p')))
                        
                        -- Coinduction: Reuse the premise
                        = M (m >>= \p' -> return p')
                        
                        -- Eta reduce
                        = M (m >>= return)
                        
                        -- [Kleisli Category - Right Identity Law - Pointful]
                        = M m
                        
        Pure    r      -> reflect (Pure r)
        
                        -- Definition of `reflect`
                        = Pure r
                        
    -- Clean up
    = case p of
        Request a' fa  -> Request a' fa
        Respond b  fb' -> Respond b  fb'
        M          m   -> M          m
        Pure       r   -> Pure r
    
    -- case statement = id
    = p
    
    -- Goal complete
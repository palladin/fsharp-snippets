// Abstracting over 'M'

// Inspired by http://higherlogics.blogspot.com/2009/10/abstracting-over-type-constructors.html
// Join our voices http://t0yv0.blogspot.com/2011/02/where-fnet-falls-short-higher-kinds.html


module MonadModule = 
    //Generic Monad Definition
    [<AbstractClass>]
    type MonadDef<'M when 'M :> MonadDef<'M>>() as this = 
         let (>>=) m f = this.Bind(m, f)
         let unit v = this.Return v
         static let listDef = ListDef()

         abstract member Return<'T> : 'T -> IMonad<'T,'M> 
         abstract member Bind<'T, 'S> : IMonad<'T,'M> * ('T -> IMonad<'S,'M>) -> IMonad<'S,'M> 
         abstract member Zero<'T> : unit -> IMonad<'T,'M>
         
         member this.Delay(f) = unit () >>= fun () -> f()
         member this.Combine<'T>(first : IMonad<unit, 'M>, second : IMonad<'T, 'M>) : IMonad<'T, 'M>  = 
            this.Then (first, second)

         member this.Then<'T, 'S>(firstM : IMonad<'T, 'M>, secondM : IMonad<'S, 'M>) : IMonad<'S, 'M> =
            firstM >>= fun _ -> secondM

         member this.Map<'T, 'S>(f : 'T -> 'S, m : IMonad<'T, 'M>) : IMonad<'S, 'M> =
            m >>= fun v -> unit (f v)

         member this.Apply<'T, 'S>(mf : IMonad<'T -> 'S, 'M>, m : IMonad<'T, 'M>) : IMonad<'S, 'M> =
            mf >>= fun f -> m >>= fun v -> unit (f v)   

         member this.Join<'T>(m : IMonad<IMonad<'T, 'M>, 'M>) : IMonad<'T, 'M> =
            m >>= id

         member this.Sequence<'T>(lm : IMonad<IMonad<'T, 'M>, ListDef>) : IMonad<IMonad<'T, ListDef>, 'M> =
            match lm :?> _ with
            | Nil -> unit (Nil :> _)
            | Cons (m, ms) -> m >>= fun v -> this.Sequence ms >>= fun vs -> unit (listDef.ConsM v vs)
        
         member this.MapM<'T, 'S>(f : 'T -> IMonad<'S, 'M>, l : IMonad<'T, ListDef>) : IMonad<IMonad<'S, ListDef>, 'M> =  
            this.Sequence (listDef.Map ((fun v -> f v), l))

         member this.FilterM<'T>(p : 'T -> IMonad<bool, 'M>, l : IMonad<'T, ListDef>) : IMonad<IMonad<'T, ListDef>, 'M> =
            match l :?> _ with
            | Nil -> unit (Nil :> _)
            | Cons (x, xs) -> p x >>= fun b -> this.FilterM (p, xs) >>= fun ys -> if b then unit (listDef.ConsM x ys) else unit ys   

    and IMonad<'T,'M when 'M :> MonadDef<'M>> = interface end 

    // List Monad
    and List<'T> = 
        | Nil
        | Cons of ('T * List<'T>)
        interface IMonad<'T, ListDef>
    and
        ListDef() = 
            inherit MonadDef<ListDef>() with
                member this.OfList<'T>(xs : list<'T>) : IMonad<'T, ListDef> =
                    List.foldBack (fun v acc -> this.ConsM v acc) xs <| this.Zero() 

                member this.ConsM (x : 'T) (acc : IMonad<'T, ListDef>) : IMonad<'T, ListDef> = Cons (x, acc :?> _) :> _
                 
                member this.Foldr<'T, 'S>(f : 'T -> 'S -> 'S, seed : 'S, list : IMonad<'T, ListDef>) : 'S =
                    match list :?> _ with
                    | Nil -> seed
                    | Cons (x, xs) -> f x (this.Foldr (f, seed, xs) )

                member this.Concat<'T>(first : IMonad<'T, ListDef>, second : IMonad<'T, ListDef>) : IMonad<'T, ListDef> =
                    this.Foldr(this.ConsM, second, first)

                override this.Return<'T>(v : 'T) : IMonad<'T, ListDef> = 
                    Cons (v, Nil) :> _ 
          
                override this.Bind<'T,'S>(m : IMonad<'T, ListDef>, f : 'T -> IMonad<'S, ListDef>) : IMonad<'S, ListDef> =  
                    this.Foldr ((fun x acc -> this.Concat (f x, acc)), Nil :> _, m)

                override this.Zero<'T>() : IMonad<'T, ListDef> = 
                    Nil :> _ 
 
    let listM = ListDef()


open MonadModule

// Maybe Monad
module MaybeModule =

    type Maybe<'T> = 
        | Nothing 
        | Just of 'T 
        interface IMonad<'T, MaybeDef> 
    and 
        
        MaybeDef() = 
            inherit MonadDef<MaybeDef>() with
                override this.Return<'T>(v : 'T) : IMonad<'T, MaybeDef> = 
                    Just v :> _ 
      
                override this.Bind<'T,'S>(m : IMonad<'T, MaybeDef>, f : 'T -> IMonad<'S, MaybeDef>) : IMonad<'S, MaybeDef> = 
                    match m :?> _ with 
                    | Nothing -> Nothing :> _ 
                    | Just x  -> f x

                override this.Zero<'T>() : IMonad<'T, MaybeDef> = 
                    Nothing :> _
                
                member this.Just<'T>(value : 'T) : IMonad<'T, MaybeDef> =
                    Just value :> _
                member this.Nothing<'T>() : IMonad<'T, MaybeDef> =
                    Nothing :> _
    
    let maybeM = new MaybeDef()     
 

open MaybeModule


//Examples

let test = listM.OfList [ maybeM.Just 1; maybeM.Just 2 ]
maybeM.Sequence test |> printfn "%A"


let powerSet xs = listM.FilterM ((fun _ -> listM.OfList [false; true]), listM.OfList xs)
powerSet [1..3] |> printfn "%A"

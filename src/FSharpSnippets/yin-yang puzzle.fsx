// yin-yang puzzle, based on https://groups.google.com/forum/#!msg/comp.lang.scheme/Fysq_Wplxsw/awxEZ_uxW20J

type Cont<'T, 'R> = ('T -> 'R) -> 'R

let return' : 'T -> Cont<'T, 'R> =
    fun v k -> k v

let bind : Cont<'T, 'R> -> ('T -> Cont<'V, 'R>) -> Cont<'V, 'R> = 
    fun cont f k -> cont (fun v -> f v k)

let callCC : (('T -> Cont<'V, 'R>) -> Cont<'T, 'R>) -> Cont<'T, 'R> =
    fun f k -> f (fun v -> (fun _ -> k v)) k 

type ContBuilder() = 
    member self.Return v = return' v
    member self.Bind(cont, f) = bind cont f
    member self.ReturnFrom cont = cont

let cont = new ContBuilder()

type Rec<'R> = Rec of K<'R>
and K<'R> = Rec<'R> -> Cont<Rec<'R>, 'R>

let yin () = 
    cont {
        let! k = callCC (fun k -> cont { return Rec k })
        printfn ""
        return k
    }
let yang () =
    cont {
        let! k = callCC (fun k -> cont { return Rec k })
        printf "*"
        return k
    } 

let yinyang () = 
    cont {
        let! (Rec k) = yin()
        let! k' = yang()
        return! k k'
    }

yinyang () (fun _ -> ())
(*
 **
 ***
 ****
 *****
 ******
 *******
 ..........*)





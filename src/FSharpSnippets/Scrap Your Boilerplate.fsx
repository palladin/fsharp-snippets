// Scrap Your Boilerplate with the help of F#. Based on the original paper by Ralf Laemmel and Simon Peyton Jones.

// http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/hmap.ps

// Type safe conversion functions
let cast<'T, 'R> (v : 'T) : 'R = v :> obj :?> 'R
let mkT<'T, 'R> (f : 'T -> 'T) : 'R -> 'R =
    if typeof<'T> = typeof<'R> then (fun (v : 'R) -> v |> cast |> f |> cast) else id
let mkQ (r : 'R) (q : 'B -> 'R) (a : 'A) : 'R =
    if typeof<'A> = typeof<'B> then
        a |> cast |> q
    else r

// encoding of rank-2 polymorphism
type IForallT = 
    abstract Invoke<'T when 'T :> ITerm<'T>> : 'T -> 'T 
and IForallQ<'R> = 
    abstract Invoke<'T when 'T :> ITerm<'T>> : 'T -> 'R
// Type Class encoding
and ITerm<'T when 'T :> ITerm<'T>> = 
    abstract gmapT : IForallT -> 'T
    abstract gmapQ<'R> : IForallQ<'R> -> 'R list

// recursive transformations-queries
let rec everywhere<'T when 'T :> ITerm<'T>> (forallT : IForallT) (term : 'T) : 'T = 
    forallT.Invoke (term.gmapT { new IForallT with 
                                    member self.Invoke term' = 
                                        everywhere forallT term' })

let rec everything<'T, 'R when 'T :> ITerm<'T>> (k : 'R -> 'R -> 'R) (forallQ : IForallQ<'R>) (term : 'T) : 'R =
    List.fold k (forallQ.Invoke term) (term.gmapQ { new IForallQ<'R> with 
                                                        member self.Invoke term' = 
                                                            everything k forallQ term' })

// Example - Company

type Company = C of Dept list with
    interface ITerm<Company> with
            member self.gmapT forallT =
                let (C depts) = self
                C (depts |> List.map forallT.Invoke)
            member self.gmapQ forallQ = 
                let (C depts) = self
                depts |> List.map forallQ.Invoke
and Dept = D of Name * Manager * SubUnit list with
    interface ITerm<Dept> with
            member self.gmapT forallT =
                let (D (name, manager, subUnits)) = self
                D (forallT.Invoke name, forallT.Invoke manager, subUnits |> List.map forallT.Invoke)
            member self.gmapQ forallQ = 
                let (D (name, manager, subUnits)) = self
                [forallQ.Invoke name; forallQ.Invoke manager] @ (List.map forallQ.Invoke subUnits)
and SubUnit = PU of Employee | DU of Dept with
    interface ITerm<SubUnit> with
            member self.gmapT forallT = 
                match self with
                | PU employee -> PU (forallT.Invoke employee)
                | DU dept -> DU (forallT.Invoke dept)
            member self.gmapQ forallQ = 
                match self with
                | PU employee -> [forallQ.Invoke employee]
                | DU dept -> [forallQ.Invoke dept]
and Employee = E of Person * Salary with
    interface ITerm<Employee> with
            member self.gmapT forallT = 
                let (E (person, salary)) = self
                E (forallT.Invoke person, forallT.Invoke salary)
            member self.gmapQ forallQ = 
                let (E (person, salary)) = self
                [forallQ.Invoke person; forallQ.Invoke salary]
and Person = P of Name * Address with
    interface ITerm<Person> with
            member self.gmapT forallT = 
                let (P (name, address)) = self
                P (forallT.Invoke name, forallT.Invoke address)
            member self.gmapQ forallQ = 
                let (P (name, address)) = self
                [forallQ.Invoke name; forallQ.Invoke address]
and Salary = S of float with
    interface ITerm<Salary> with
            member self.gmapT forallT = self
            member self.gmapQ forallQ = []
and Manager = M of Employee with
    interface ITerm<Manager> with
            member self.gmapT forallT = 
                let (M employee) = self
                M (forallT.Invoke employee)
            member self.gmapQ forallQ =
                let (M employee) = self 
                [forallQ.Invoke employee]
and Name = N of string with
    interface ITerm<Name> with
            member self.gmapT forallT = self
            member self.gmapQ forallQ = []
and Address = A of string with
    interface ITerm<Address> with
            member self.gmapT forallT = self
            member self.gmapQ forallQ = []

// Data for a small company
let ralf = E (P (N "Ralf", A "Amsterdam"), S 8000.0)
let joost = E (P (N "Joost", A "Amsterdam"), S 1000.0)
let marlow = E (P (N "Marlow", A "Cambridge"), S 2000.0)
let blair = E (P (N "Blair", A "London"), S 100000.0)
let genCom = 
    C [ D (N "Research", M ralf, [PU joost; PU marlow]);
        D (N "Strategy", M blair, [])]


// Increase salary by 10%
let incSalary (k : float) (S value) = S (value * (1.0 + k))
everywhere { new IForallT with member self.Invoke term = mkT (incSalary 10.0) term } genCom


// sum all salaries
let sumSalary (S value) = value 
everything (+) { new IForallQ<float> with member self.Invoke term = mkQ 0.0 sumSalary term } genCom
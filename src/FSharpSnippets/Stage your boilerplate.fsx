// Application of staging to "scrap your boilerplate" generic programming technique.

open Microsoft.FSharp.Quotations

// <@ fun x -> (% <@ x @> ) @> ~ lambda (fun x -> x)
let lambda (f : Expr<'T> -> Expr<'R>) : Expr<'T -> 'R> =
    let var = new Var("__temp__", typeof<'T>)
    Expr.Cast<_>(Expr.Lambda(var,  f (Expr.Cast<_>(Expr.Var var))))



// encoding of rank-2 polymorphism
type IForallT = 
    abstract Invoke<'T> : ITerm<'T> -> (Expr<'T> -> Expr<'T>)
    abstract Invoke<'T> : IRecTerm<'T> -> Expr<'T -> 'T>
and IForallQ<'R> = 
    abstract Invoke<'T> : ITerm<'T> -> (Expr<'T> -> Expr<'R>)
    abstract Invoke<'T> : IRecTerm<'T> -> Expr<'T -> 'R>
// Type Class encoding
and ITerm<'T> = 
    abstract gmapT : IForallT -> (Expr<'T> -> Expr<'T>)
    // TODO: abstract gmapQ<'R> : IForallQ<'R> -> (Expr<'T> -> Expr<'R list>)
and IRecTerm<'T> =
    abstract gmapT : IForallT -> Expr<'T -> 'T>
    // TODO: abstract gmapQ<'R> : IForallQ<'R> -> Expr<'T -> 'R list>

// Example - Company
type Company = C of Dept list 
and Dept = D of Name * Manager * SubUnit list 
and SubUnit = PU of Employee | DU of Dept 
and Employee = E of Person * Salary 
and Person = P of Name * Address 
and Salary = S of float 
and Manager = M of Employee 
and Name = N of string 
and Address = A of string 

// Data for a small company
let ralf = E (P (N "Ralf", A "Amsterdam"), S 8000.0)
let joost = E (P (N "Joost", A "Amsterdam"), S 1000.0)
let marlow = E (P (N "Marlow", A "Cambridge"), S 2000.0)
let blair = E (P (N "Blair", A "London"), S 100000.0)
let genCom = 
    C [ D (N "Research", M ralf, [PU joost; PU marlow]);
        D (N "Strategy", M blair, [])]

// Term Representations
type CompanyTerm(deptTerm : IRecTerm<Dept>) = 
    interface ITerm<Company> with
        member self.gmapT forallT = fun company ->
            <@  let (C depts) = %company
                C ( depts |> List.map (fun dept -> (% forallT.Invoke deptTerm )  dept )) @>

type DeptTerm(nameTerm : ITerm<Name>, managerTerm : ITerm<Manager>, subUnitTermf : IRecTerm<Dept> -> ITerm<SubUnit>) = 
    interface IRecTerm<Dept> with
        member self.gmapT forallT = <@ fun dept ->  
                let (D (name, manager, subUnits)) = dept
                D ( (% (lambda (fun name -> forallT.Invoke nameTerm name)) ) name,
                    (% (lambda (fun manager -> forallT.Invoke managerTerm manager)) ) manager,
                    subUnits |> List.map (fun subUnit -> (% (lambda (fun subUnit -> forallT.Invoke (subUnitTermf self) subUnit)) ) subUnit )) @>


type SubUnitTerm(employeeTerm : ITerm<Employee>, deptTerm : IRecTerm<Dept>) = 
    interface ITerm<SubUnit> with
        member self.gmapT forallT = fun subUnit ->
            <@ match %subUnit with
               | PU employee -> PU ((% (lambda (fun employee -> forallT.Invoke employeeTerm employee)) ) employee) 
               | DU dept -> DU ((% forallT.Invoke deptTerm ) dept) @>

type ManagerTerm(employeeTerm : ITerm<Employee>) = 
    interface ITerm<Manager> with
        member self.gmapT forallT = fun manager ->
            <@  let (M employee) = %manager
                M ( (% (lambda (fun employee -> forallT.Invoke employeeTerm employee)) ) employee ) @>

type EmployeeTerm(personTerm : ITerm<Person>, salaryTerm : ITerm<Salary>) = 
    interface ITerm<Employee> with
        member self.gmapT forallT = fun employee -> 
                <@  let (E (person, salary)) = %employee
                    E ( (% (lambda (fun person -> forallT.Invoke personTerm person)) ) person, 
                        (% (lambda (fun salary -> forallT.Invoke salaryTerm salary)) ) salary) @>


type PersonTerm(nameTerm : ITerm<Name>, addressTerm : ITerm<Address>) = 
    interface ITerm<Person> with
            member self.gmapT forallT = fun person -> 
                <@  let (P (name, address)) = %person
                    P ( (% (lambda (fun name -> forallT.Invoke nameTerm name)) ) name, 
                        (% (lambda (fun address -> forallT.Invoke addressTerm address)) ) address) @>

type SalaryTerm() =
    interface ITerm<Salary> with
            member self.gmapT _ = id

type NameTerm() =
    interface ITerm<Name> with
            member self.gmapT _ = id

type AddressTerm() = 
    interface ITerm<Address> with
            member self.gmapT _ = id

let nameTerm = new NameTerm()
let addressTerm = new AddressTerm()
let salaryTerm = new SalaryTerm()
let personTerm = new PersonTerm(nameTerm, addressTerm)
let employeeTerm = new EmployeeTerm(personTerm, salaryTerm)
let managerTerm = new ManagerTerm(employeeTerm)
let subUnitTerm deptTerm = new SubUnitTerm(employeeTerm, deptTerm) :> ITerm<SubUnit>
let deptTerm = new DeptTerm(nameTerm, managerTerm, subUnitTerm)
let companyTerm = new CompanyTerm(deptTerm)

// Type safe conversion functions
let cast (v : Expr<'T>) : Expr<'R> = v :> Expr :?> Expr<'R>
let mkT (f : Expr<'T> -> Expr<'T>) = 
    let dict = new System.Collections.Generic.Dictionary<System.Type, Expr>()
    { new IForallT with 
        member self.Invoke<'R> (term : ITerm<'R>) : Expr<'R> -> Expr<'R> = 
            if typeof<'T> = typeof<'R> then
                (fun (v : Expr<'R>) -> v |> cast |> f |> cast)
            else term.gmapT self
        member self.Invoke<'R> (term : IRecTerm<'R>) : Expr<'R -> 'R> = 
            match dict.TryGetValue(typeof<'R>) with
            | (true, expr) -> expr :?> _
            | (false, _) -> 
                <@  let rec loop x =
                        (% lambda (fun recf ->  let recf' = if typeof<'T> = typeof<'R> then 
                                                                lambda (fun (v : Expr<'R>) -> 
                                                                            v |> cast |> f |> cast)
                                                            else recf 
                                                dict.Add(typeof<'R>, recf'); <@ () @>) ) loop 
                        (% term.gmapT self ) x
                    loop @> }

// transformations-queries
let everywhere (forallT : IForallT) (term : ITerm<'T>) : Expr<'T -> 'T> = 
    lambda (forallT.Invoke term)
let everywhereRec (forallT : IForallT) (term : IRecTerm<'T>) : Expr<'T -> 'T> = 
    forallT.Invoke term


 
// Example
let nameToUpper (name : Expr<Name>) = 
    <@ let (N name) = %name in N (name.ToUpper()) @>

everywhere (mkT nameToUpper) personTerm


let incSalary (k : float) (salary : Expr<Salary>) = 
    <@ let (S value) = %salary in S (value * (1.0 + k)) @>

everywhere (mkT (incSalary 10.0)) companyTerm
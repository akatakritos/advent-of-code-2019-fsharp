module Puzzle12

[<Struct>]
type Vector3 = { x: int; y: int; z: int; }

module Vector3 =
    let empty = { x = 0; y = 0; z = 0; }

    let create (x, y, z) = { x = x; y = y; z = z; }

    let parse s =
        let m = System.Text.RegularExpressions.Regex.Match(s, "<x=\\s*([\\-\\d]+),\\s+y=\\s*([\\-\\d]+),\\s+z=\\s*([-\\d]+)>")
        if m.Success then
            let x = (m.Groups.[1].Value |> int)
            let y = (m.Groups.[2].Value |> int)
            let z = (m.Groups.[3].Value |> int)
            { x = x; y = y; z = z; }
        else
            failwithf "Failed to parse Vector3 from '%s'" s

    let x v = v.x
    let y v = v.y
    let z v = v.z


type Moon = { position: Vector3; velocity: Vector3; }

module Moon =
    let create (x, y, z) =
        let position = Vector3.create (x, y, z)
        let velocity = Vector3.empty

        { position = position; velocity = velocity; }

    let parseInitial s =
        let position = Vector3.parse s
        let velocity = Vector3.empty

        { position = position; velocity = velocity; }

    let parse s =
        let m = System.Text.RegularExpressions.Regex.Match(s, "pos=(<.+>), vel=(<.+>)")
        if m.Success then
            let position = Vector3.parse m.Groups.[1].Value
            let velocity = Vector3.parse m.Groups.[2].Value

            { position = position; velocity = velocity; }
        else
            failwithf "Failed to parse Moon '%s'" s

    let energy moon =
        let potential = (abs moon.position.x) + (abs moon.position.y) + (abs moon.position.z)
        let kinetic = (abs moon.velocity.x) + (abs moon.velocity.y) + (abs moon.velocity.z)
        potential * kinetic

let gravity moon velocity otherMoon =
    let delta a b =
        if b > a then
            1
        elif b < a then
            -1
        else
            0

    {
        x = velocity.x + (delta moon.position.x otherMoon.position.x);
        y = velocity.y + (delta moon.position.y otherMoon.position.y);
        z = velocity.z + (delta moon.position.z otherMoon.position.z);
    }

let velocity (moon: Moon) otherMoons =
    let gravityApplier = gravity moon
    let newVelocity =
        otherMoons
        |> Seq.fold gravityApplier moon.velocity

    { moon with velocity = newVelocity; }

let move position velocity =
    {
        x = position.x + velocity.x;
        y = position.y + velocity.y;
        z = position.z + velocity.z;
    }

let tick (moons: Moon array) =
    let updateVelocity moon =
        velocity moon (moons |> Seq.filter (fun m -> m <> moon))

    let updatePosition moon =
        let position = move moon.position moon.velocity
        { moon with position = position; }

    for i = 0 to moons.Length - 1 do
        moons.[i] <- updateVelocity moons.[i]

    for i = 0 to moons.Length - 1 do
        moons.[i] <- updatePosition moons.[i]


let totalEnergy moons =
    moons
    |> Seq.sumBy Moon.energy

let cycleDetect (moons: Moon array) index =
    let original = moons.[index];

    let rec recurse moons count =
        tick moons
        if count % 100000 = 0 then printfn "%d" count

        if moons.[index] = original then
            count + 1
        else
            recurse moons (count + 1)

    recurse moons 0


let rec gcd (a: int64) (b: int64) =
    if b = 0L then a else gcd b (a % b)

let private lcm a b =
    (a * b) / (gcd a b)

let private componentMatch (f: Vector3 -> int) moon original =
    f moon.position = f original.position &&
        f moon.velocity = f original.velocity

let cyclesUntilReset (moons: Moon array) =
    let mutable xPeriod = None
    let mutable yPeriod = None
    let mutable zPeriod = None
    let mutable i = 0L

    let original = moons |> Seq.toArray


    while xPeriod = None || yPeriod = None || zPeriod = None do
        tick moons
        i <- i + 1L

        if xPeriod = None then
            let hasReturned =
                moons
                |> Seq.forall2 (componentMatch Vector3.x) original
            if hasReturned then xPeriod <- Some i

        if yPeriod = None then
            let hasReturned =
                moons |> Seq.forall2 (componentMatch Vector3.y) original

            if hasReturned then yPeriod <- Some i

        if zPeriod = None then
            let hasReturned = moons |> Seq.forall2 (componentMatch Vector3.z) original
            if hasReturned then zPeriod <- Some i

    match (xPeriod, yPeriod, zPeriod) with
        | (Some x, Some y, Some z) -> lcm x (lcm y z)
        | _ -> failwith "impossible"







module Puzzle08

    module SpaceImage =
        type Layer = int array array
        type Image = {
            width: int;
            height: int;
            layers: Layer list;
        }

        let private charValue c = int c - int '0'

        let private loadRow width offset (data: string) =
            [| for w in 0..width-1 do yield data.[offset + w] |> charValue |]


        let private loadLayer width  height (data:string) =
            [|
                for h in 0..height-1 do
                    yield loadRow width (h * width) data
                        
            |]


        let private load width height (data: string) =
            let layerSize = width * height;
            let layerCount = data.Length / layerSize
            [
                for i in 0..layerCount-1 do 
                    let start = i * layerSize
                    let stop = start + layerSize - 1
                    yield loadLayer width height data.[start..stop]
            ]

        let create width height data =
            let layers = load width height data
            {
                width = width;
                height = height;
                layers = layers;
            }

        let getPixel (layer: Layer) (x, y) =
            layer.[y].[x]

        let pixels layer width height =
            let get = getPixel layer

            seq {
                for y in 0..height-1 do
                    for x in 0..width-1 do
                        yield get (x, y)
            }
            
        let private countDigits layer width height value =
            pixels layer width height
            |> Seq.map (fun p -> if p = value then 1 else 0)
            |> Seq.sum

        let private minLayer image =
            image.layers
            |> List.minBy (fun layer -> countDigits layer image.width image.height 0)

        let checksum image =
            let layer = image |> minLayer
            let ones = countDigits layer image.width image.height 1
            let twos = countDigits layer image.width image.height 2

            ones * twos

        type Color = Transparent | White | Black

        let decodeColor = function
            | 0 -> Black
            | 1 -> White
            | 2 -> Transparent
            | n -> failwithf "Bad color data %d" n


        // stacking is backwards from intuitive, so its really like whats the first non-transparent
        let applyColor current next =
            match current with
                | Transparent -> next
                | White -> White
                | Black -> Black

        let getColor image (x, y) =
            let colorFor layer =
                getPixel layer (x, y) |> decodeColor
                
            image.layers
            |> List.fold (fun currentColor layer -> colorFor layer  |> applyColor currentColor) Transparent

        let renderCharacter = function
            | Transparent -> " "
            | Black -> " "
            | White -> "#"


        let render image =
            let renderRow y =
                seq {
                    for x in 0..image.width - 1 do
                        yield getColor image (x, y) |> renderCharacter
                }
                |> String.concat ""

            seq {
                for y in 0..image.height - 1 do
                    yield renderRow y
                    
            }
            |> String.concat "\n"
            
            
            


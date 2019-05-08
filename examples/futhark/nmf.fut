import "ad"

module f32_dual = mk_dual f32

let linalg_matrixMult m1 m2 =
  map (\m1' -> map (\m2' -> map2 (f32_dual.*) m1' m2' |> f32_dual.sum) m2) m1

let linalg_matrixAdd =
  map2 (map2 (f32_dual.+))

let matrixSum m =
  flatten m |> f32_dual.sum

let matrixLog =
  map (map f32_dual.log)

let matrixElemDiv =
  map2 (map2 (f32_dual./))

let nmf_exp H W AA =
  (matrixSum ((linalg_matrixAdd (matrixLog ((linalg_matrixMult W) H)))
              ((matrixElemDiv AA) ((linalg_matrixMult W) H))))

-- ==
-- random input { [1000][1]f32 [1000][1]f32 [1000][1000]f32 }

let main H W AA = 
  let H' = map (map (f32_dual.inject)) H
  let AA' = map (map (f32_dual.inject)) AA
  in tabulate 1000 (\i ->
    let dW = tabulate 1000 (\j -> tabulate 1 (\x -> f32.bool(i == j)))
  	let W' = map2 (map2 f32_dual.make_dual) W dW
  	in nmf_exp H' W' AA'
  )

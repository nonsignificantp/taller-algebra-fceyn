{-
  _______        _           _                              _   _           
 |__   __|      | |         (_)                            | | (_)          
    | |_ __ __ _| |__   __ _ _  ___    _ __  _ __ __ _  ___| |_ _  ___ ___  
    | | '__/ _` | '_ \ / _` | |/ _ \  | '_ \| '__/ _` |/ __| __| |/ __/ _ \ 
    | | | | (_| | |_) | (_| | | (_) | | |_) | | | (_| | (__| |_| | (_| (_) |
    |_|_|  \__,_|_.__/ \__,_| |\___/  | .__/|_|  \__,_|\___|\__|_|\___\___/ 
                           _/ |       | |                                   
                          |__/        |_|                                   

Asignatura: Taller de Algebra
Cuatrimestre: Segundo cuatrimestre 2020
Alumno: Agustin Rodriguez
-}

-- (1) Modelo exponencial discreto
med :: Float -> Float -> Int -> Float
med i0 b n
    | n == 0    = i0
    | otherwise = ip + ip * b
    where ip = med i0 b (n - 1)

-- (2) Modelo logistico discreto
mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b n
    | n == 0    = i0
    | otherwise = ip + (ip * b * sp)
    where
        ip = mld p i0 b (n - 1)
        sp = (p - ip) / p

-- (3 y 4) Modelo SIR discreto y maxSIR

-- Funciones auxiliares para el ejercicio

-- Calcula el (S,I,R) actual a partir del (Sp,Ip,Rp) previo
status :: (Float, Float, Float) -> Float -> Float -> [(Float, Float, Float)]
status (sp, ip, rp) b g = [(sp - sick, ip + sick - fine, rp + fine), (sp, ip, rp)]
    where
        sick = b * ip * sp
        fine = g * ip

-- Crea una lista de elementos (S,I,R) desde el momento 0 hasta n
sir' :: (Float, Float, Float) -> Float -> Float -> Int -> [(Float, Float, Float)]
sir' (s0, i0, r0) b g n
    | n == 0    = [(s0, i0, r0)]
    | otherwise = status (sp, ip, rp) b g ++ xs
    where (sp, ip, rp) : xs = sir' (s0, i0, r0) b g (n - 1)

-- Puntos de entrada para el ejercicio

sir :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir (s0, i0, r0) b g n = currentStatusFrom diseaseEvolution
    where
        diseaseEvolution  = sir' (s0, i0, r0) b g n
        currentStatusFrom = head

maxsir :: (Float, Float, Float) -> Float -> Float -> Int -> Float
maxsir (s0, i0, r0) b g n = peakInfectedFrom diseaseEvolution
    where
        diseaseEvolution = sir' (s0, i0, r0) b g n
        peakInfectedFrom = maximum . map (\(_, i, _) -> i)
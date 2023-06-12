{- IMPORTATION DES LIBRAIRIES NECESSAIRES -}
import System.Random
import Data.List
import Data.Char


{- RECOPIAGE DE LA PARTIE 1 DU PROJET -}

tousLesMots :: String -> [String]
tousLesMots letexte =
    let nouveautexte = map toLower letexte
        nouveautexte2 = filter (`elem` ['a'..'z'] ++ [' ','é','è','à','\'']) nouveautexte
        nouveautexte3 = words nouveautexte2
    in nouveautexte3

genererTable :: [String] -> [(String, [String])]
genererTable mots = construireTable [] mots
  where
    construireTable table [] = table
    construireTable table [mot] = ajouterMot table mot ""
    construireTable table (mot:motsSuivants) =
      let table' = ajouterMot table mot (head motsSuivants)
      in construireTable table' motsSuivants

    ajouterMot table mot suivant =
      let (prefixe, suffixe) = partition (\(m, _) -> m == mot) table
      in if null prefixe
           then table ++ [(mot, [suivant])]
           else let (motExistant, suivants) = head prefixe
                    table' = delete (motExistant, suivants) table
                in table' ++ [(motExistant, suivants ++ [suivant])]

{- Partie 3 du mini projets -}

prochainMot :: [(String, [String])] -> String -> IO String
prochainMot table motActuel = do
  let motsPossibles = concat $ snd <$> filter (\(m, _) -> m   == motActuel) table
  if null motsPossibles
    then return ""
    else do
      prochainMot <- randomElement motsPossibles
      return prochainMot
{- Choisis le prochain mots dans les mots possibles -}

genererPhrase :: [(String, [String])] -> IO String
genererPhrase table = do
  motInitial <- randomElement $ fst <$> table
  genererListeMots table [motInitial]
{- Cette fonction prends la table de correspondance en entrée, choisis un mot au hasard
( randomElement) et elle utilise genererListeMots que je vais décrire en dessous-}


genererListeMots :: [(String, [String])] -> [String] -> IO String
genererListeMots table listeMotsActuelle = do
  let dernierMot = last listeMotsActuelle
  motSuivant <- prochainMot table dernierMot
  if null motSuivant
    then return $ unwords listeMotsActuelle
    else genererListeMots table $ listeMotsActuelle ++ [motSuivant]
{- Prends en entrée la liste de correspondance et les mots actusl puis elle trouver le prochain
mot à ajouter.( tout en vérifiant que y'a un prochain mot d'ailleurs) -}


randomElement :: [a] -> IO a
randomElement xs = do
  i <- randomRIO (0, length xs - 1)
  return $ xs !! i
{-Simple utilisation de la librairies random, pour choisir entre 0 
et length de la liste-}

main :: IO ()
main = do
  putStrLn "Entrez votre phrase :"
  phrase <- getLine
  let mots = tousLesMots phrase
      table = genererTable mots
  nouvellePhrase <- genererPhrase table
  putStrLn "Voici la nouvelle phrase générée à partir de votre phrase :"
  putStrLn nouvellePhrase
{- Demande la phrase, refais les étapes de la partie 1 puis donne une phrase au hasard -}
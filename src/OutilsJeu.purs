module OutilsJeu where


import Prelude

import Data.Maybe (Maybe(..))

{-

                                        DEFINITIONS DES TYPES

-}


data Joueur = JoueurHumain | Ordinateur 

type Score = Int

data PFC = P | F | C
data PFCLS = Pierre | Feuille | Ciseaux |Lezard | Spock 
data Poke = Bulbi |Cara |Sala

data Mode = PFC | PFCLS | Poke

type UnePartie a =  { choixJoueur :: Maybe a, choixOrdi :: Maybe a , vainqueur :: Maybe Joueur , scoreJoueur :: Int}


{-

                                       AFFICHAGE

-}


instance showPFC :: Show PFC where
    show P  = "Pierre"
    show F  = "Feuille"
    show C  = "Ciseaux"

instance showPFCLS :: Show PFCLS  where
    show Pierre   = "Pierre"
    show Feuille  = "Feuille"
    show Ciseaux  = "Ciseaux"
    show Lezard   = "Lezard"
    show Spock    = "Spock"


instance showPoke :: Show Poke  where
    show Bulbi = "Bulbi"
    show Cara  = "Cara"
    show Sala  = "Sala"


showMaybeChoix :: forall a . Show a => Maybe a -> String 
showMaybeChoix (Just choix ) = show choix 
showMaybeChoix Nothing = ""


instance showJoueur :: Show Joueur  where
  show  JoueurHumain    =   "Vous"
  show Ordinateur       =   "Ordi"

showVainqueur :: Maybe Joueur -> String 
showVainqueur (Just joueur) = show joueur 
showVainqueur Nothing = "Aucun"



instance  showMode :: Show Mode where 
  show PFC      =   "PFC"
  show PFCLS    =   "PFCLS"
  show Poke     =   "Pokemon"


{-

                                        ORDRE

-}

derive instance  eqJoueur :: Eq Joueur

derive instance  eqPFC :: Eq PFC
instance comparePFC :: Ord PFC where
  compare :: PFC -> PFC -> Ordering
  compare P F    = LT
  compare F C   = LT
  compare C P    = LT 
  compare _ _ = GT


derive instance  eqPFCLS :: Eq PFCLS
instance comparePFCLS :: Ord PFCLS where
  compare :: PFCLS -> PFCLS -> Ordering
  compare Pierre Feuille    = LT
  compare Pierre Spock      = LT

  compare Feuille Ciseaux   = LT
  compare Feuille Lezard   = LT

  compare Ciseaux Pierre    = LT 
  compare Ciseaux Lezard    = LT 

  compare Lezard Pierre     = LT
  compare Lezard Ciseaux = LT

  compare Spock Lezard      = LT
  compare Spock Feuille      = LT

  compare _ _ = GT


derive instance eqPoke :: Eq Poke
instance comparePoke :: Ord Poke where
  compare :: Poke -> Poke -> Ordering
  compare   Bulbi Sala= LT 
  compare  Sala Cara = LT
  compare   Cara Bulbi =LT 
  compare _ _ = GT




{-

                                       REGLES

-}


reglesPFC ::  PFC ->  PFC -> String 
reglesPFC P F   =   "La pierre est enveloppee par la feuille"
reglesPFC F C   =   "La feuille est coupée par les ciseaux"
reglesPFC C P  =   "Les ciseaux sont cassés par la pierre"
reglesPFC P C    =   "La pierre casse les ciseaux."
reglesPFC F P   =   "La feuille entoure la pierre"
reglesPFC C F   =   "Les ciseaux coupent la feuille"
reglesPFC _ _ = "Egalite"


reglesPFCLS ::   PFCLS -> PFCLS -> String 
reglesPFCLS Pierre Spock     =   "La pierre est détruite par Spock"
reglesPFCLS Pierre Feuille   =   "La pierre est enveloppee par la feuille"

reglesPFCLS Feuille Ciseaux  =   "La feuille est coupée par les ciseaux"
reglesPFCLS Feuille Lezard   =   "Le papier est mangé par le lézard"

reglesPFCLS Ciseaux Pierre   =   "Les ciseaux sont cassés par la pierre"
reglesPFCLS Ciseaux Spock    =   "Les ciseaux sont écrasés par Spock"

reglesPFCLS Lezard Pierre    =   "Le lezard est écrasé par la pierre"
reglesPFCLS Lezard Ciseaux   =   "Le lézard est décapié par les ciseaux"

reglesPFCLS Spock Lezard     =   "Spock est empoisonné par le lézard"
reglesPFCLS Spock Feuille    =   "Spock est desavoué par le papier"

reglesPFCLS Pierre Ciseaux    =   "La pierre casse les ciseaux."
reglesPFCLS Pierre Lezard    =   "La pierre écrase le lézard."

reglesPFCLS Feuille Pierre   =   "La feuille entoure la pierre"
reglesPFCLS Feuille Spock    =   "Le papier désavoue Spock"

reglesPFCLS Ciseaux Feuille  =   "Les ciseaux coupent la feuille"
reglesPFCLS Ciseaux Lezard   =   "Les ciseaux décapitent le lézard"

reglesPFCLS Lezard Feuille   =   "Le lézard mange le papier"
reglesPFCLS Lezard Spock     =   "Le lézard empoisonne Spock"

reglesPFCLS Spock Pierre     =   "Spock détruit la pierre"
reglesPFCLS Spock Ciseaux    =   "Spock écrase les ciseaux"

reglesPFCLS _ _ = "Egalite"

reglesPoke :: Poke -> Poke -> String
reglesPoke  Cara Bulbi = "Carapuce est absorbé par Bulbizard "
reglesPoke  Sala Cara = "Salameche est éteint par Carapuce"
reglesPoke  Bulbi Sala= "Bulbizard est brûlé par Salameche "

reglesPoke Bulbi Cara = "Bulbizard absorbe Carapuce"
reglesPoke Cara Sala = "Carapuce éteint Salameche"
reglesPoke Sala Bulbi = "Salameche brûle Bulbizard"


reglesPoke _ _ = "Egalité"

combinaisons :: Mode -> Array String 
combinaisons PFC = [
    reglesPFC F P ,
    reglesPFC P C,
    reglesPFC C F]

combinaisons PFCLS = [
  reglesPFCLS Pierre Ciseaux,
  reglesPFCLS Pierre Lezard,
  reglesPFCLS Feuille Pierre,
  reglesPFCLS Feuille Spock,
  reglesPFCLS Ciseaux Feuille,
  reglesPFCLS Ciseaux Lezard,
  reglesPFCLS Lezard Feuille,
  reglesPFCLS Lezard Spock,
  reglesPFCLS Spock Pierre,
  reglesPFCLS Spock Ciseaux]

combinaisons Poke = [
  reglesPoke Bulbi Cara,
  reglesPoke Cara Sala,
  reglesPoke Sala Bulbi]



associePFC ::  Int ->   PFC
associePFC  1 = P
associePFC  2 = F 
associePFC  3 = C
associePFC  _ = P

associePFCLS ::  Int ->   PFCLS
associePFCLS  1 = Pierre 
associePFCLS  2 = Feuille 
associePFCLS  3 = Ciseaux
associePFCLS  4 = Lezard 
associePFCLS  5 = Spock
associePFCLS  _ = Pierre

associePoke :: Int -> Poke
associePoke  1  = Bulbi
associePoke  2  = Cara 
associePoke  3 = Sala
associePoke  _ = Bulbi


module Jeu  where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Random (randomInt)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.HTML.Event.EventTypes (change)


data Joueur = JoueurHumain | Ordinateur 
type Score = Int

data PFC = P | F | C

instance showPFC :: Show PFC where
    show P   = "Pierre"
    show F  = "Feuille"
    show C  = "Ciseaux"

derive instance  eqPFC :: Eq PFC
instance comparePFC :: Ord PFC where
  compare :: PFC -> PFC -> Ordering
  compare P F    = LT
  compare F C   = LT
  compare C P    = LT 
  compare _ _ = GT

data PFCLS = Pierre | Feuille | Ciseaux |Lezard | Spock 

derive instance  eqPFCLS :: Eq PFCLS
instance showPFCLS :: Show PFCLS  where
    show Pierre   = "Pierre"
    show Feuille  = "Feuille"
    show Ciseaux  = "Ciseaux"
    show Lezard   = "Lezard"
    show Spock    = "Spock"

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

data Poke = Bulbi |Cara |Sala

derive instance eqPoke :: Eq Poke
instance comparePoke :: Ord Poke where
  compare :: Poke -> Poke -> Ordering
  compare   Bulbi Sala= LT 
  compare  Sala Cara = LT
  compare   Cara Bulbi =LT 
  compare _ _ = GT

instance showPoke :: Show Poke  where
    show Bulbi   = "Bulbi"
    show Cara  = "Cara"
    show Sala  = "Sala"

data Mode = PFC | PFCLS | Poke

type UnJeu a = {jeu :: a }


data  Models = 
              ChoisirJeu
              |JeuPFC Score
              |JeuPFCLS Score 
              |JeuPoke Score
              | ResultatPFC  {
                choixJoueur :: PFC
                , choixOrdi :: PFC
                , vainqueur ::   Maybe Joueur
                , scoreJoueur :: Score}
              | ResultatPFCLS  {
                choixJoueur :: PFCLS
                , choixOrdi :: PFCLS
                , vainqueur ::   Maybe Joueur
                , scoreJoueur :: Score}
              | ResultatPoke  {
                choixJoueur :: Poke
                , choixOrdi :: Poke
                , vainqueur ::   Maybe Joueur
                , scoreJoueur :: Score}
            
              

type State = { stage :: Models }

instance showJoueur :: Show Joueur  where
  show  JoueurHumain = "Vous"
  show Ordinateur  = "Ordi"

showVainqueur :: Maybe Joueur -> String 
showVainqueur (Just joueur) = show joueur 
showVainqueur Nothing = "Aucun"




data Msg
  = DebuteJeu Mode 
  |ChoixPFC PFC
  |ChoixPFCLS PFCLS
  |ChoixPoke Poke
  | NouvellePartie


instance  showM :: Show Msg where 
  show (ChoixPFC a )= show a
  show (ChoixPFCLS a) = show a
  show (ChoixPoke a) = show a 

  show _ = ""

page ::  forall m.  MonadEffect m => H.Component HH.HTML (Const Void) Unit Void m 
page = H.mkComponent
  { initialState  : const initialState
  , render        : view
  , eval          : H.mkEval $ H.defaultEval
      { handleAction = update }
  }

initialState :: State
initialState = {  stage : ChoisirJeu}


view ::  forall m. State -> H.ComponentHTML Msg () m
view {stage : ChoisirJeu } = 
  HH.div_[

  HH.h1_ [HH.text ("Choisissez le mode")]
  ,HH.div[HP.class_ (ClassName "modesDeJeux")][
    HH.div[HP.class_ (ClassName "divAlignee")][
      HH.p
        [HP.class_ (ClassName "ssTitreNiv1")]
        [HH.text "Classique"]
      ,HH.p
        [HP.class_ (ClassName "ssTitreNiv2")]
        [HH.text "Pierre Feuille Ciseaux"]
      ,HH.img[
        HE.onClick <<< const $  (Just (DebuteJeu PFC))
        ,HP.src ("src/img/" <> "PFC" <>".jpg")
       , HP.class_ (ClassName "image-zoom")
        ]]
    ,HH.div[HP.class_ (ClassName "divAlignee")][
      HH.p[HP.class_ (ClassName "ssTitreNiv1")][HH.text "Big Bang Theory"]
      ,HH.p[HP.class_ (ClassName "ssTitreNiv2")][HH.text "Pierre Feuille Ciseaux Lezard Spock"]
      ,HH.img[
        HE.onClick <<< const $  (Just (DebuteJeu PFCLS))
        ,HP.src ("src/img/" <> "PFCLS" <>".jpg")
       , HP.class_ (ClassName "image-zoom")
        ]
    ]
    ,HH.div[HP.class_ (ClassName "divAlignee")][
      HH.p[HP.class_ (ClassName "ssTitreNiv1")][HH.text "Pokemon"]
      ,HH.p[HP.class_ (ClassName "ssTitreNiv2")][HH.text "Eau, Feu, Plante"]
      ,HH.img[
        HE.onClick <<< const $  (Just (DebuteJeu Poke))
        ,HP.src ("src/img/" <> "poke" <>".jpg")
       , HP.class_ (ClassName "image-zoom")
        ]
      ]
  ]
  ]


view {stage  :  JeuPFCLS score } = 
  HH.div_[
    HH.div_(
      map (\choix -> 
        HH.img[
          HE.onClick <<< const $ Just choix
          ,HP.src ("src/img/PFCLS/" <>(show choix) <> ".jpg")
          ,HP.class_ (ClassName "image-zoom")
        ]
        )
        [ChoixPFCLS Pierre  , ChoixPFCLS Feuille  , ChoixPFCLS Ciseaux , ChoixPFCLS Lezard, ChoixPFCLS Spock ]
    )
    ,HH.p_[HH.text ("Votre score actuel est de : " <> show score <> " point(s) ")]
    ,HH.div[HP.class_ (ClassName "survol")]
        [  HH.text "Rappel des Regles"
          ,HH.br_
          ,HH.text "Survolez pour afficher"
          ,HH.div[HP.class_ (ClassName "affichageRegles")](map (\r -> HH.p_ [HH.text r] ) combinaisonsPFCLS )]
  ]
view {stage : ResultatPFCLS {choixJoueur : choixJ , choixOrdi : choixO,  vainqueur : ordiHumainOuEgalite, scoreJoueur : score}} =
   HH.div_[
    HH.img [HP.src ("src/img/PFCLS" <>"/"<>(show choixJ) <> ".jpg")]
    ,HH.img [HP.src ("src/img/PFCLS"<>"/"<>(show choixO) <> ".jpg")]
    ,HH.p_[HH.text ("Vainqueur : "<> (showVainqueur ordiHumainOuEgalite))]    
    ,HH.p_[HH.text ("Raison : " <> (unsafePartial(reglesPFCLS choixJ choixO)))]
   ,HH.p_[renderNextButton (Just NouvellePartie) "Nouvelle Partie"]
  ]


view {stage  :  JeuPFC score } = 
  HH.div_[
    HH.div_(
      map (\choix -> 
        HH.img[
          HE.onClick <<< const $ Just choix
          ,HP.src ("src/img/PFC/" <>(show choix) <> ".jpg")
          ,HP.class_ (ClassName "image-zoom")
        ]
        )
        [ChoixPFC P  , ChoixPFC F  , ChoixPFC C  ]
    )
    ,HH.p_[HH.text ("Votre score actuel est de : " <> show score <> " point(s) ")]
    ,HH.div[HP.class_ (ClassName "survol")]
        [  HH.text "Rappel des Regles"
          ,HH.br_
          ,HH.text "Survolez pour afficher"
          ,HH.div[HP.class_ (ClassName "affichageRegles")](map (\r -> HH.p_ [HH.text r] ) combinaisonsPFC )]
  ]
view {stage : ResultatPFC {choixJoueur : choixJ , choixOrdi : choixO,  vainqueur : ordiHumainOuEgalite, scoreJoueur : score}} =
   HH.div_[
    HH.img [HP.src ("src/img/PFC" <>"/"<>(show choixJ) <> ".jpg")]
    ,HH.img [HP.src ("src/img/PFC"<>"/"<>(show choixO) <> ".jpg")]
    ,HH.p_[HH.text ("Vainqueur : "<> (showVainqueur ordiHumainOuEgalite))]    
    ,HH.p_[HH.text ("Raison : " <> (unsafePartial(reglesPFC choixJ choixO)))]
   ,HH.p_[renderNextButton (Just NouvellePartie) "Nouvelle Partie"]
  ]

view {stage  :  JeuPoke score } = 
  HH.div_[
    HH.div_(
      map (\choix -> 
        HH.img[
          HE.onClick <<< const $ Just choix
          ,HP.src ("src/img/Pokemon/" <>(show choix) <> ".jpg")
          ,HP.class_ (ClassName "image-zoom")
        ]
        )
        [ChoixPoke Sala  , ChoixPoke Cara  , ChoixPoke Bulbi]
    )
    ,HH.p_[HH.text ("Votre score actuel est de : " <> show score <> " point(s) ")]
    ,HH.div[HP.class_ (ClassName "survol")]
        [  HH.text "Rappel des Regles"
          ,HH.br_
          ,HH.text "Survolez pour afficher"
          ,HH.div[HP.class_ (ClassName "affichageRegles")](map (\r -> HH.p_ [HH.text r] ) combinaisonsPoke )]
  ]
view {stage : ResultatPoke {choixJoueur : choixJ , choixOrdi : choixO,  vainqueur : ordiHumainOuEgalite, scoreJoueur : score}} =
   HH.div_[
    HH.img [HP.src ("src/img/Pokemon" <>"/"<>(show choixJ) <> ".jpg")]
    ,HH.img [HP.src ("src/img/Pokemon"<>"/"<>(show choixO) <> ".jpg")]
    ,HH.p_[HH.text ("Vainqueur : "<> (showVainqueur ordiHumainOuEgalite))]    
    ,HH.p_[HH.text ("Raison : " <> (unsafePartial(reglesPoke choixJ choixO)))]
   ,HH.p_[renderNextButton (Just NouvellePartie) "Nouvelle Partie"]
  ]




renderNextButton :: forall m.  Maybe Msg -> String -> H.ComponentHTML Msg() m
renderNextButton action message =
   HH.button
  ( case action of
      Nothing -> [ HP.disabled true ]
      Just action' -> [ HE.onClick <<< const $ Just action' ] )
  [ HH.text message ]

update ::  forall m.  MonadEffect m  => Msg -> H.HalogenM State Msg () Void m Unit
update (DebuteJeu mode) =
  case mode of 
    PFCLS -> H.modify_ _{stage = JeuPFCLS 0 }
    Poke -> H.modify_ _{stage = JeuPoke 0 }
    PFC ->  H.modify_ _{stage = JeuPFC 0 }


update (ChoixPFC a)  = 
  do
    modeJeuPFC    <- liftEffect $ randomInt  1 3
    H.modify_ (\state ->  
        case state of 
          {stage : JeuPFC score} -> 
            {stage :
              ResultatPFC { choixJoueur : a
                    ,choixOrdi : choixOrdiSelonLeMode 
                    ,vainqueur : vainqueur a choixOrdiSelonLeMode 
                    ,scoreJoueur : score
                    }
                }
              where
                  choixOrdiSelonLeMode  =  associePFC modeJeuPFC
          _ -> state)




update (ChoixPFCLS a)  = 
  do
    modeJeuPFCLS    <- liftEffect $ randomInt  1 5
    H.modify_ (\state ->  
        case state of 
          {stage : JeuPFCLS score} -> 
            {stage :
              ResultatPFCLS { choixJoueur : a
                    ,choixOrdi : choixOrdiSelonLeMode 
                    ,vainqueur : vainqueur a choixOrdiSelonLeMode 
                    ,scoreJoueur : score
                    }
                }
              where
                  choixOrdiSelonLeMode  =  associePFCLS modeJeuPFCLS
          _ -> state)





update (ChoixPoke a)  = 
  do
    modeJeuPoke    <- liftEffect $ randomInt  1 3
    H.modify_ (\state ->  
        case state of 
          {stage : JeuPoke score} -> 
            {stage :
              ResultatPoke { choixJoueur : a
                    ,choixOrdi : choixOrdiSelonLeMode 
                    ,vainqueur : vainqueur a choixOrdiSelonLeMode 
                    ,scoreJoueur : score
                    }
                }
              where
                  choixOrdiSelonLeMode  =  associe2 modeJeuPoke
          _ -> state)






update NouvellePartie = H.modify_ (\state -> 
    case state of 
      {stage : ResultatPFCLS {choixJoueur : _, choixOrdi : co , vainqueur : rienHumainOuOrdi , scoreJoueur : score } } ->
          case rienHumainOuOrdi of 
            Just JoueurHumain ->  {stage : JeuPFCLS (score + 1 ) }
            Just Ordinateur   ->  {stage : JeuPFCLS (score  - 1) }
            Nothing        ->  {stage : JeuPFCLS score }

      {stage : ResultatPFC {choixJoueur : _, choixOrdi : co , vainqueur : rienHumainOuOrdi , scoreJoueur : score } } ->
          case rienHumainOuOrdi of 
            Just JoueurHumain ->  {stage : JeuPFC (score + 1 ) }
            Just Ordinateur   ->  {stage : JeuPFC (score  - 1) }
            Nothing        ->  {stage : JeuPFC score }


      {stage : ResultatPoke {choixJoueur : _, choixOrdi : co , vainqueur : rienHumainOuOrdi , scoreJoueur : score } } ->
          case rienHumainOuOrdi of 
            Just JoueurHumain ->  {stage : JeuPoke (score + 1 ) }
            Just Ordinateur   ->  {stage : JeuPoke (score  - 1) }
            Nothing        ->  {stage : JeuPoke score }


      _ -> state
      )

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

associe2 :: Int -> Poke
associe2  1  = Bulbi
associe2  2  = Cara 
associe2  3 = Sala
associe2  _ = Bulbi


reglesPFC :: PFC -> PFC -> String 
reglesPFC P F   =   "La pierre est enveloppee par la feuille"
reglesPFC F C  =   "La feuille est coupée par les ciseaux"
reglesPFC C P =   "Les ciseaux sont cassés par la pierre"
reglesPFC P C    =   "La pierre casse les ciseaux."
reglesPFC F P   =   "La feuille entoure la pierre"
reglesPFC C F  =   "Les ciseaux coupent la feuille"
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


vainqueur ::     forall coup. Eq coup => Ord coup => coup -> coup -> Maybe Joueur
vainqueur joueur ordi = 
  if joueur == ordi 
  then Nothing
  else 
    if joueur < ordi 
    then  (Just Ordinateur)
    else  (Just JoueurHumain) 


combinaisonsPFC :: Array String 
combinaisonsPFC  = [reglesPFC F P ,
                  reglesPFC P C,
                  reglesPFC C F]

combinaisonsPFCLS :: Array String                  
combinaisonsPFCLS = [
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

combinaisonsPoke :: Array String
combinaisonsPoke =[
  reglesPoke Bulbi Cara,
  reglesPoke Cara Sala,
  reglesPoke Sala Bulbi]

module Jeu2 where

import OutilsJeu
import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Random (randomInt)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

instance  showM :: Show Msg where 
  show (ChoixPFC a )= show a
  show (ChoixPFCLS a) = show a
  show (ChoixPoke a) = show a 
  show _ = ""


data  Models = 
              ChoisirJeu
              | Jeu Mode Score
              | PartiePFC (UnePartie PFC)
              | PartiePFCLS (UnePartie PFCLS)
              | PartiePoke (UnePartie Poke)

              
x :: Models 
x = PartiePFC {choixJoueur : Just P , choixOrdi : Just P, vainqueur : Nothing , scoreJoueur:  0}
type State = { stage :: Models }


data Msg
  = DebuteJeu Mode 
  | ChoixPFC PFC
  | ChoixPFCLS PFCLS
  | ChoixPoke Poke
  | NouvellePartie 



coupsPossibles ::   Mode   -> Array Msg
coupsPossibles PFC = [ChoixPFC P, ChoixPFC F, ChoixPFC C]
coupsPossibles PFCLS = [ChoixPFCLS Pierre, ChoixPFCLS Feuille, ChoixPFCLS Ciseaux, ChoixPFCLS Lezard , ChoixPFCLS Spock]
coupsPossibles Poke = [ChoixPoke Bulbi, ChoixPoke Sala, ChoixPoke Cara]




view ::  forall m. State -> H.ComponentHTML Msg () m
view {stage : ChoisirJeu } = 
  HH.div_[
    HH.h1_ [HH.text ("Choisissez le mode")]
    , HH.div[HP.class_ (ClassName "modesDeJeux")][
        vueDebut PFC "Classique " "Pierre feuille Ciseaux" ("src/img/" <> "PFC" <>".jpg")
        ,vueDebut PFCLS "Big Bang Theory" "Pierre Feuille Ciseaux Lezard Spock" ("src/img/" <> "PFCLS" <>".jpg")
        ,vueDebut Poke "Pokemon" "Eau,Plante,Feu" ("src/img/" <> "poke" <>".jpg")
  ]]
  
view {stage : Jeu mode score} = 
  HH.div_[
    HH.div_(
      map (\choix -> 
        HH.div[HP.class_ (ClassName "box")]
        [HH.img[
          HE.onClick <<< const $ Just choix
          ,HP.src ("src/img/"<> (show mode) <>"/" <>(show choix) <> ".jpg")
          ,HP.class_ (ClassName "image-zoom")
        ]
        ,HH.div[HP.class_ (ClassName "texte")][HH.text(show choix)]
        ]
        )
        (coupsPossibles mode)
    )
    ,HH.p_[HH.text ("Votre score actuel est de : " <> show score <> " point(s) ")]
    ,HH.div[HP.class_ (ClassName "survol")]
        [  HH.text "Rappel des Regles"
          ,HH.br_
          ,HH.text "Survolez pour afficher"
          ,HH.div[HP.class_ (ClassName "affichageRegles")](map (\r -> HH.p_ [HH.text r] ) (combinaisons mode) )
          ]
      ]

view { stage  : PartiePFC  record   } = 
  vuePartie PFC record
 
view { stage  : PartiePFCLS  record   } = 
  vuePartie PFCLS  record
 
view {stage : PartiePoke record } = 
  vuePartie Poke  record

renderNextButton :: forall m.  Maybe Msg -> String -> H.ComponentHTML Msg() m
renderNextButton action message =
   HH.button
  ( case action of
      Nothing -> [ HP.disabled true ]
      Just action' -> [ HE.onClick <<< const $ Just action' ] )
  [ HH.text message ]

update ::  forall m.  MonadEffect m  => Msg -> H.HalogenM State Msg () Void m Unit
update (DebuteJeu mode) =  H.modify_ _{stage = Jeu mode 0 }



update NouvellePartie = H.modify_ (\state -> 
    case state of 
      {stage : PartiePFCLS {scoreJoueur : score } } ->
            {stage : Jeu PFCLS score }
      {stage : PartiePFC {scoreJoueur : score } } ->
            {stage : Jeu PFC score }
      {stage : PartiePoke {scoreJoueur : score } } ->
            {stage : Jeu Poke score }
     
      _ -> state
      )
update (ChoixPFC a) = updateChoix 3 PFC a associePFC PartiePFC
update (ChoixPFCLS a) = updateChoix 5 PFCLS a associePFCLS PartiePFCLS
update (ChoixPoke a) = updateChoix 3 Poke a associePoke PartiePoke


page ::  forall m.  MonadEffect m => H.Component HH.HTML (Const Void) Unit Void m 
page = H.mkComponent
  { initialState  : const initialState
  , render        : view
  , eval          : H.mkEval $ H.defaultEval
      { handleAction = update }
  }

initialState :: State
initialState = {  stage : ChoisirJeu}


vuePartie ::  forall a t2. Show a => Mode -> UnePartie a ->  HH.HTML (H.ComponentSlot HH.HTML () t2 Msg) Msg
vuePartie mode  { choixJoueur : choixJ, choixOrdi : choixO, vainqueur  : ordiHumainOuEgalite, scoreJoueur : score} =
   HH.div_[
    HH.img [HP.src ("src/img/"<> (show mode)  <>"/"<>(showMaybeChoix choixJ) <> ".jpg")]
    ,HH.img [HP.src ("src/img/"<>(show mode)<>"/"<>(showMaybeChoix choixO) <> ".jpg")]
    ,HH.p_[HH.text ("Vainqueur : "<> (showVainqueur ordiHumainOuEgalite))] 
    ,HH.p_[HH.text ("Votre score actuel est de : " <> show score <> " point(s) ")]
 -- ,  HH.p_[HH.text ("Raison : " <> (regles choixJ choixO))]
   ,HH.p_[renderNextButton (Just NouvellePartie) "Nouvelle Partie"]
  
  ]

vueJeu :: forall t2. Mode -> Int -> HH.HTML (H.ComponentSlot HH.HTML () t2 Msg) Msg
vueJeu mode score = 
  HH.div_[
    HH.div_(
      map (\choix -> 
        HH.div[HP.class_ (ClassName "contientHoverEtTexte")][
          HH.img[
            HE.onClick <<< const $ Just choix
            ,HP.src ("src/img/" <>  (show mode) <> "/" <>(show choix) <> ".jpg")
            ,HP.class_ (ClassName "image-zoom")
        ]
        ,HH.div[HP.class_ (ClassName "texteHover" ) ][HH.text "TEST"]
        ]
        )
        (coupsPossibles mode)
    )
    ,HH.div[HP.class_ (ClassName "survol")]
        [  HH.text "Rappel des Regles"
          ,HH.br_
          ,HH.text "Survolez pour afficher"
          ,HH.div[HP.class_ (ClassName "affichageRegles")](map (\r -> HH.p_ [HH.text r] ) (combinaisons mode) )]
  ]

vueDebut :: forall t2.  Mode -> String -> String ->  String -> HH.HTML (H.ComponentSlot HH.HTML () t2 Msg) Msg
vueDebut mode titre  sousTitre image = 
  HH.div[HP.class_ (ClassName "divAlignee")][
      HH.p
        [HP.class_ (ClassName "ssTitreNiv1")]
        [HH.text titre]
      ,HH.p
        [HP.class_ (ClassName "ssTitreNiv2")]
        [HH.text sousTitre]
      ,HH.img[
        HE.onClick <<< const $  (Just (DebuteJeu mode))
        ,HP.src image
       , HP.class_ (ClassName "image-zoom")
        ]]


--updateChoix :: ∀ t43 t47 t64. Bind t47 ⇒ MonadEffect t47 ⇒ MonadState { stage :: Models } t47 ⇒ Eq t64 ⇒ Ord t64 ⇒ Int → t43 → t64 → (Int → t64) → ({ choixJoueur ∷ Maybe t64 , choixOrdi ∷ Maybe t64 , scoreJoueur ∷ Int , vainqueur ∷ Maybe Joueur } → Models ) → t47 Unit
updateChoix nombre  mode a  fct p= 
  do
    modeJeu    <- liftEffect $ randomInt  1 nombre
    H.modify_ (\state ->  
        case state of 
          {stage : Jeu mode score} -> 
            {stage :
              p { choixJoueur :Just a
                    ,choixOrdi : Just choixOrdiSelonLeMode 
                    ,vainqueur : vainqueur a choixOrdiSelonLeMode 
                    ,scoreJoueur : 
                      (case (vainqueur a choixOrdiSelonLeMode) of 
                        Just Ordinateur ->  score - 1 
                        Just JoueurHumain -> score + 1 
                        Nothing -> score)
                    }
                }
              where
                  choixOrdiSelonLeMode  =  fct modeJeu
    

          _ -> state)

vainqueur ::     forall coup. Eq coup => Ord coup => coup -> coup -> Maybe Joueur
vainqueur joueur ordi = 
  if joueur == ordi 
  then Nothing
  else 
    if joueur < ordi 
    then  (Just Ordinateur)
    else  (Just JoueurHumain) 

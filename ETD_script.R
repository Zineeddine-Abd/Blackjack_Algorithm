# VARS GLOBALES
# ------------------
# Pour maintenir la mémoire de l'algorithme entre chaque tour
# Parce que la fonction getDecision() "oublie" tout à chaque fois qu'elle a fini de s'exécuter
id_sabot_actuel <- -1 # Permet de détecter quand le croupier prend un nouveau sabot
compteur_high_low <- 0 # Compteur High-Low
total_cartes_vues <- 0 # Le nombre total de cartes qui sont déjà sorties

# Inventaire exact pour calculer les probabilités conditionnelles
# Simulation d'un sabot de 6 paquets de 52 cartes (312 cartes au total).
inventaire_sabot <- c("2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24)

# FONCTIONS DE TRAITEMENTS
# ------------------------

# Transforme une carte (texte) en valeur mathématique (ex: "Q♥" = 10)
parser_carte <- function(carte_txt) {
  valeur_brute <- gsub("[^0-9JQKA]", "", carte_txt)
  if (valeur_brute %in% c("J", "Q", "K")) {
    return(10)
  } else if (valeur_brute == "A") {
    return(11)
  } else {
    return(as.numeric(valeur_brute))
  }
}

# Calcule le total de la main et gère le cas spécial de l'As.
# la main est "souple" (si As = 11)
evaluer_main_joueur <- function(valeurs_cartes) {
  score <- sum(valeurs_cartes) # Additionne toutes les cartes
  nb_as <- sum(valeurs_cartes == 11) # Compte combien d'As on a dans la main

  # Si notre score dépasse 21 et qu'on a un As,
  # l'As passe d'une valeur de 11 à une valeur de 1 pour nous sauver.
  while (score > 21 && nb_as > 0) {
    score <- score - 10 # On retire 10 (c'est comme si le 11 devenait 1)
    nb_as <- nb_as - 1 # Cet As a été utilisé, on le décompte
  }

  # Une main est "souple" (soft) s'il nous reste un As qui vaut encore 11.
  # Important parceque Tirer une carte sur une main souple est 100% sûr (pas de risque) : On ne saute pas.
  est_souple <- (nb_as > 0)

  return(list(score = score, est_souple = est_souple))
}

# Attribue un poids selon le système High-Low (Hi-Lo) vu en cours
calculer_high_low <- function(valeur) {
  if (valeur >= 2 && valeur <= 6) {
    return(1)
  } # Petites cartes favorables au joueur
  if (valeur >= 7 && valeur <= 9) {
    return(0)
  } # Cartes neutres
  if (valeur >= 10) {
    return(-1)
  } # Cartes fortes (10, têtes, As)
  return(0)
}

# GESTION DE L'ÉTAT ET DU SABOT
# -----------------------------

# Remet tous les compteurs à zéro quand un nouveau sabot commence.
reinitialiser_sabot <- function(id_nouveau_sabot) {
  # L'opérateur <<- permet de modifier la variable globale et pas juste une copie locale.
  id_sabot_actuel <<- id_nouveau_sabot
  compteur_high_low <<- 0
  total_cartes_vues <<- 0
  inventaire_sabot <<- c("2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24)
}

# Mémorise une carte qui vient d'être tirée sur la table.
mettre_a_jour_memoire <- function(valeur_carte) {
  valeur_txt <- as.character(valeur_carte) # Transforme le chiffre en texte

  # Si la carte est bien dans l'inventaire, on en retire une du stock
  if (inventaire_sabot[valeur_txt] > 0) {
    inventaire_sabot[valeur_txt] <<- inventaire_sabot[valeur_txt] - 1
  }

  # On actualise le compteur High-Low et le nombre de cartes vues
  compteur_high_low <<- compteur_high_low + calculer_high_low(valeur_carte)
  total_cartes_vues <<- total_cartes_vues + 1

  # Note stratégique pour justifier notre algo : Le simulateur ne nous montre pas la carte cachée
  # du croupier ni celles qu'il tire à la fin. L'inventaire n'est donc pas 100% exact,
  # mais il est statistiquement très fiable pour prendre des décisions.
}

# CALCULS STATISTIQUES (Probas conditionnelles)
# ---------------------------------------------

# Calcule la probabilité exacte (en %) qu'on dépasse 21 si on tire une carte.
calculer_risque_bust <- function(score, est_souple) {
  # Si on a une main souple (ex: As+5 = 16), on ne peut JAMAIS sauter en tirant.
  # L'As redescendra à 1 si on tire un 10. Risque = 0.
  if (est_souple) {
    return(0.0)
  }

  # De combien de points a-t-on besoin pour atteindre 21 ?
  marge <- 21 - score

  # Si la marge est de 11 ou plus (ex: on a 10), on ne peut pas sauter car la plus grosse carte est 11.
  if (marge >= 11) {
    return(0.0)
  }

  # Combien reste-t-il de cartes dans tout le sabot ?
  cartes_restantes <- sum(inventaire_sabot)
  if (cartes_restantes == 0) {
    return(0.0)
  } # Sécurité pour éviter de diviser par zéro

  # On identifie les "cartes tueuses" : celles qui sont plus grandes que notre marge
  # Ex : si marge = 6 (on a 15), les cartes tueuses sont 7, 8, 9, 10, 11.
  cartes_tueuses <- as.character((marge + 1):11)

  # On regarde combien il reste de ces cartes tueuses dans notre inventaire mémorisé
  cartes_tueuses <- cartes_tueuses[cartes_tueuses %in% names(inventaire_sabot)]
  nb_cartes_tueuses <- sum(inventaire_sabot[cartes_tueuses])

  # On divise les mauvaises cartes par le total des cartes pour avoir le % de chance de perdre.
  return(nb_cartes_tueuses / cartes_restantes)
}

calculer_bust_croupier_recursif <- function(score_croupier, inventaire, a_as_souple = FALSE, profondeur = 0) {
  # Limite de récursion pour éviter un appel infini (cas pathologique)
  if (profondeur > 6) {
    return(0.0)
  }

  cartes_totales <- sum(inventaire)
  if (cartes_totales == 0) {
    return(0.0)
  }

  # Le croupier s'arrête à 17+, donc plus de risque de bust à partir de là
  if (score_croupier >= 17) {
    return(0.0)
  }

  proba_bust_total <- 0.0

  for (nom_val in names(inventaire)) {
    count <- inventaire[nom_val]
    if (count == 0) next

    valeur <- as.numeric(nom_val)
    proba_carte <- count / cartes_totales

    nouveau_score <- score_croupier + valeur
    nouveau_as_souple <- a_as_souple || (valeur == 11)

    # Gestion de l'As : si le croupier "saute" et possède un As à 11 (ancien ou nouveau), il repasse à 1
    if (nouveau_score > 21 && nouveau_as_souple) {
      nouveau_score <- nouveau_score - 10
      nouveau_as_souple <- FALSE
    }

    if (nouveau_score > 21) {
      # Ce chemin mène directement au bust du croupier → bonne nouvelle pour le joueur
      proba_bust_total <- proba_bust_total + proba_carte
    } else if (nouveau_score >= 17) {
      # Le croupier s'arrête ici, pas de bust sur ce chemin
      next
    } else {
      # Le croupier doit encore tirer → on simule récursivement
      inv_reduit <- inventaire
      inv_reduit[nom_val] <- inv_reduit[nom_val] - 1
      proba_bust_suite <- calculer_bust_croupier_recursif(
        nouveau_score, inv_reduit, nouveau_as_souple, profondeur + 1
      )
      proba_bust_total <- proba_bust_total + proba_carte * proba_bust_suite
    }
  }

  return(proba_bust_total)
}

# Wrapper de compatibilité (remplace calculer_proba_croupier_bust dans getDecision)
# Retourne la VRAIE probabilité de bust du croupier sur tout son tour.
calculer_proba_croupier_bust <- function(valeur_croupier) {
  # On initialise a_as_souple à TRUE si la carte visible du croupier est un As (11)
  return(calculer_bust_croupier_recursif(valeur_croupier, inventaire_sabot, a_as_souple = (valeur_croupier == 11)))
}

# Calcule les chances du croupier d'avoir une excellente main (17 à 21) directement.
calculer_proba_croupier_fort <- function(valeur_croupier) {
  cartes_restantes <- sum(inventaire_sabot)
  if (cartes_restantes == 0) {
    return(0.0)
  }

  # Quelles cartes amènent le croupier entre 17 et 21 ?
  cible_min <- max(2, 17 - valeur_croupier)
  cible_max <- min(11, 21 - valeur_croupier)
  if (cible_min > cible_max) {
    return(0.0)
  }

  cartes_cibles <- as.character(cible_min:cible_max)
  cartes_cibles <- cartes_cibles[cartes_cibles %in% names(inventaire_sabot)]
  nb_cartes_cibles <- sum(inventaire_sabot[cartes_cibles])

  return(nb_cartes_cibles / cartes_restantes)
}

# FONCTION DE DÉCISION
# --------------------

getDecision <- function(main_joueur, main_croupier, id_sabot) {
  # 1- Vérification : Est-ce qu'on a commencé un nouveau sabot ?
  if (id_sabot != id_sabot_actuel) {
    reinitialiser_sabot(id_sabot)
  }

  # 2- On transforme les cartes (texte) en valeurs mathématiques
  # sapply applique la fonction 'parser_carte' à toutes les cartes du joueur
  valeurs_joueur <- sapply(main_joueur, parser_carte)
  valeur_croupier <- parser_carte(main_croupier[1])

  # On évalue le score actuel du joueur
  etat_joueur <- evaluer_main_joueur(valeurs_joueur)
  score_joueur <- etat_joueur$score
  est_souple <- etat_joueur$est_souple

  # 3- Mémorisation des cartes
  # Quand on a 2 cartes, c'est le début du tour,
  # on doit mémoriser nos 2 cartes + celle du croupier.
  # Si on a plus de 2 cartes, c'est qu'on vient juste de demander un "Hit" (H).
  # Dans ce cas, on mémorise SEULEMENT la dernière carte reçue, pour ne pas compter en double.
  if (length(main_joueur) == 2) {
    mettre_a_jour_memoire(valeurs_joueur[1])
    mettre_a_jour_memoire(valeurs_joueur[2])
    mettre_a_jour_memoire(valeur_croupier)
  } else {
    mettre_a_jour_memoire(valeurs_joueur[length(valeurs_joueur)])
  }

  # 4- Calcul des probabilités pour prendre notre décision
  # Le "Vrai Compte" (True Count) affine le compte High-Low en le divisant par le nombre de paquets restants.
  paquets_restants <- max(1, (312 - total_cartes_vues) / 52)
  true_count <- compteur_high_low / paquets_restants

  # On interroge nos fonctions statistiques
  risque_bust <- calculer_risque_bust(score_joueur, est_souple)
  proba_croupier_bust <- calculer_proba_croupier_bust(valeur_croupier)
  proba_croupier_fort <- calculer_proba_croupier_fort(valeur_croupier)

  # 5- L'IA PREND SA DÉCISION ICI
  # Règles de base absolues :
  if (score_joueur <= 11) {
    return("H")
  } # On tire toujours si on a 11 ou moins (aucun risque)
  if (score_joueur >= 19) {
    return("S")
  } # On s'arrête toujours à 19 ou plus (trop dangereux)

  # Cas spécial des mains souples (immunisées au risque de sauter)
  if (est_souple) {
    if (score_joueur == 18) {
      # Basic Strategy pour Soft 18 :
      #   - Croupier montre 9, 10 (J/Q/K=10) ou As (11) → HIT (il risque de faire 19-21)
      #   - Croupier montre 2, 7, 8                      → STAND (notre 18 est suffisant)
      #   - Croupier montre 3, 4, 5, 6                   → Double interdit → STAND
      if (valeur_croupier %in% c(9, 10, 11)) {
        return("H")
      } else {
        return("S")
      }
    }
    return("H") # En dessous de 18 souple, on tire toujours
  }

  # Si on a une main classique (dure) à 17 ou 18, on s'arrête. Le risque est trop grand.
  if (score_joueur >= 17) {
    return("S")
  }
  # Formule du risque acceptable :
  # - 0.40 : Prudence par défaut (on refuse de risquer plus de 40% de bust).
  # - + (proba_croupier_fort * 0.30) : Croupier fort -> on prend un peu plus de risque pour tenter de le battre.
  # - - (proba_croupier_bust * 0.45) : Croupier faible -> on bloque la prise de risque et on le laisse sauter.
  # - Sécurité (65%) : Ne jamais tirer si 2 cartes sur 3 nous font perdre d'office.
  risque_max <- 0.40 + (proba_croupier_fort * 0.30) - (proba_croupier_bust * 0.45)
  # Evite les situations absurdes où on tire avec 70-90% de chances de sauter
  risque_max <- min(risque_max, 0.65)

  # On ajuste légèrement ce risque avec le True Count.
  # Si le compteur est très positif, il reste plein de bûches (10), on a donc plus de chances de sauter.
  # On baisse donc notre tolérance au risque.
  risque_max <- risque_max - (true_count * 0.03)

  # DECISION FINALE : On compare notre risque réel avec le risque qu'on accepte de prendre :
  if (risque_bust > risque_max) {
    return("S") # Trop risqué, on s'arrête (Stand)
  } else {
    return("H") # Risque acceptable, on tire une carte (Hit)
  }
}

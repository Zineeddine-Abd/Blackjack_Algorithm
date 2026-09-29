# Vars Globales
# -------------
id_sabot_actuel <- -1 # Permet de détecter quand le croupier prend un nouveau sabot
compteur_high_low <- 0 # Compteur High-Low
total_cartes_vues <- 0 # Le nombre total de cartes qui sont déjà sorties

# Inventaire exact pour calculer les probabilités conditionnelles
# Sabot de 6 paquets de 52 cartes
inventaire_sabot <- c("2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24)

# Traitements
# -----------
# Transforme une carte (Q♥) en valeur (10)
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

# Calcule le total de la main et gère le cas spécial de l'As
evaluer_main_joueur <- function(valeurs_cartes) {
  score <- sum(valeurs_cartes) # Additionne toutes les cartes
  nb_as <- sum(valeurs_cartes == 11) # Compte combien d'As on a dans la main

  # Si notre score dépasse 21 et qu'on a un As,
  # l'As passe d'une valeur de 11 à une valeur de 1 pour nous sauver
  while (score > 21 && nb_as > 0) {
    score <- score - 10 # On retire 10 (c'est comme si le 11 devenait 1)
    nb_as <- nb_as - 1 # Cet As a été utilisé, on le décompte
  }

  # Une main est souple (soft) s'il nous reste un As qui vaut encore 11
  # Tirer une carte sur une main souple a pas de risque donc On ne saute pas
  est_souple <- (nb_as > 0)

  return(list(score = score, est_souple = est_souple))
}

# Attribue un poids selon le système High-Low
calculer_high_low <- function(valeur) {
  if (valeur >= 2 && valeur <= 6) {
    return(1)
  }
  if (valeur >= 7 && valeur <= 9) {
    return(0)
  } # Cartes neutres
  if (valeur >= 10) {
    return(-1)
  }
  return(0)
}

# Gestion de sabot
# ----------------
# Remet tous les compteurs a 0 quand un nouveau sabot commence
reinitialiser_sabot <- function(id_nouveau_sabot) {
  id_sabot_actuel <<- id_nouveau_sabot
  compteur_high_low <<- 0
  total_cartes_vues <<- 0
  inventaire_sabot <<- c("2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24)
}

# Mémorise une carte qui vient d'être tiré
mettre_a_jour_memoire <- function(valeur_carte) {
  valeur_txt <- as.character(valeur_carte)

  # On en retire une du stock
  if (inventaire_sabot[valeur_txt] > 0) {
    inventaire_sabot[valeur_txt] <<- inventaire_sabot[valeur_txt] - 1
  }

  # On actualise le compteur High-Low et le nombre de cartes vues
  compteur_high_low <<- compteur_high_low + calculer_high_low(valeur_carte)
  total_cartes_vues <<- total_cartes_vues + 1
}

# Calculs Probas Conditionnels
# ----------------------------
# Calcule la probabilité qu'on dépasse 21 si on tire une carte
calculer_risque_bust <- function(score, est_souple) {
  # Si on a une main souple, pas de risque
  if (est_souple) {
    return(0.0)
  }

  # De combien de points on a  besoin pour atteindre 21 :
  marge <- 21 - score

  # Si la marge est de 11 ou plus, pas de risque
  if (marge >= 11) {
    return(0.0)
  }

  # Combien reste t-il de cartes dans tout le sabot :
  cartes_restantes <- sum(inventaire_sabot)
  if (cartes_restantes == 0) {
    return(0.0)
  } # juste pour evite de diviser pr 0

  # On identifie les qui sont plus grandes que notre marge
  cartes_danger <- as.character((marge + 1):11)

  # On regarde combien il reste de ces cartes danger dans notre inventaire mémorisé
  cartes_danger <- cartes_danger[cartes_danger %in% names(inventaire_sabot)]
  nb_cartes_danger <- sum(inventaire_sabot[cartes_danger])

  # Le pourcentage
  return(nb_cartes_danger / cartes_restantes)
}

calculer_bust_croupier_recursif <- function(score_croupier, inventaire, a_as_souple = FALSE, profondeur = 0) {
  # Limite
  if (profondeur > 6) {
    return(0.0)
  }

  cartes_totales <- sum(inventaire)
  if (cartes_totales == 0) {
    return(0.0)
  }

  # Le croupier s'arrête à 17 ou +, donc plus de risque de bust à partir de 17
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

    # Gestion de l'As : si le croupier saute et possède un As à 11 (ancien ou nouveau), il repasse a 1
    if (nouveau_score > 21 && nouveau_as_souple) {
      nouveau_score <- nouveau_score - 10
      nouveau_as_souple <- FALSE
    }

    if (nouveau_score > 21) {
      # mène directement au bust du croupier
      proba_bust_total <- proba_bust_total + proba_carte
    } else if (nouveau_score >= 17) {
      # Le croupier s'arrête, pas de bust sur ce chemin
      next
    } else {
      # Le croupier doit encore tirer , récursivité
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

# Wrapper :
# Retourne la VRAIE probabilité de bust du croupier sur tout son tour
calculer_proba_croupier_bust <- function(valeur_croupier) {
  return(calculer_bust_croupier_recursif(valeur_croupier, inventaire_sabot, a_as_souple = (valeur_croupier == 11)))
}

# Calcule les chances du croupier d'avoir une excellente main (17 a 21) directement
calculer_proba_croupier_fort <- function(valeur_croupier) {
  cartes_restantes <- sum(inventaire_sabot)
  if (cartes_restantes == 0) {
    return(0.0)
  }

  # Quelles cartes améne le croupier entre 17 et 21 :
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

# Decision :
# ----------
getDecision <- function(main_joueur, main_croupier, id_sabot) {
  # 1- Est-ce qu'on a commencé un nouveau sabot :
  if (id_sabot != id_sabot_actuel) {
    reinitialiser_sabot(id_sabot)
  }

  # 2- On transforme les cartes (texte) en valeurs mathématiques
  valeurs_joueur <- sapply(main_joueur, parser_carte)
  valeur_croupier <- parser_carte(main_croupier[1])

  # On évalue le score actuel du joueur
  etat_joueur <- evaluer_main_joueur(valeurs_joueur)
  score_joueur <- etat_joueur$score
  est_souple <- etat_joueur$est_souple

  # 3- Mémorisation des cartes
  # Quand on a 2 cartes, c'est le début du tour,
  # on doit mémoriser nos 2 cartes + celle du croupier
  # Si on a plus de 2 cartes, c'est qu'on vient juste de demander un "Hit" (H)
  # Dans ce cas, on mémorise SEULEMENT la dernière carte reçue, pour ne pas compter en double
  if (length(main_joueur) == 2) {
    mettre_a_jour_memoire(valeurs_joueur[1])
    mettre_a_jour_memoire(valeurs_joueur[2])
    mettre_a_jour_memoire(valeur_croupier)
  } else {
    mettre_a_jour_memoire(valeurs_joueur[length(valeurs_joueur)])
  }

  # 4- Calcul des probabilités pour prendre notre décision
  # Le "Vrai Compte" (True Count) affine le compte High-Low en le divisant par le nombre de paquets restants
  paquets_restants <- max(1, (312 - total_cartes_vues) / 52)
  true_count <- compteur_high_low / paquets_restants

  # Appele au fonctions proba
  risque_bust <- calculer_risque_bust(score_joueur, est_souple)
  proba_croupier_bust <- calculer_proba_croupier_bust(valeur_croupier)
  proba_croupier_fort <- calculer_proba_croupier_fort(valeur_croupier)

  # 5- Decision
  # Régles de base
  if (score_joueur <= 11) {
    return("H")
  }
  if (score_joueur >= 19) {
    return("S")
  }

  # Cas spécial des mains souples (immunisées au risque de sauter)
  if (est_souple) {
    if (score_joueur == 18) {
      # Stragtegie trouvé pour Soft 18 :
      #  Croupier montre 9, 10 (J/Q/K=10) ou As (11)  -> HIT (il risque de faire 19-21)
      #  Croupier montre 2, 7, 8                      -> STAND (notre 18 est suffisant)
      #  Croupier montre 3, 4, 5, 6                   -> Double interdit -> STAND
      if (valeur_croupier %in% c(9, 10, 11)) {
        return("H")
      } else {
        return("S")
      }
    }
    return("H") # En dessous de 18 souple, on tire
  }

  # Si on a une main normale (dure) a 17 ou 18, on s'arrête (psq trop de risque)
  if (score_joueur >= 17) {
    return("S")
  }
  # Formule du risque acceptable :
  # 0.40 : Prudence par défaut (on refuse de risquer plus de 40% de bust)
  # + (proba_croupier_fort * 0.30) : Croupier fort -> on prend un peu plus de risque pour tenter de le battre
  # - (proba_croupier_bust * 0.45) : Croupier faible -> on bloque la prise de risque et on le laisse sauter
  # Sécurité (65%) : Ne jamais tirer si 2 cartes sur 3 nous font perdre d'office
  risque_max <- 0.40 + (proba_croupier_fort * 0.30) - (proba_croupier_bust * 0.45)
  risque_max <- min(risque_max, 0.65) # Eviter les risques de > 70%

  # Ajuste légèrement ce risque avec le True Count
  # Si le compteur est très positif, il reste plein de 10, on a donc plus de chances de sauter
  # On baisse la tolérance au risque
  risque_max <- risque_max - (true_count * 0.03)

  # DECISION FINALE :
  if (risque_bust > risque_max) {
    return("S") # Trop risqué, on s'arrête (Stand)
  } else {
    return("H") # Risque acceptable, on tire une carte (Hit)
  }
}
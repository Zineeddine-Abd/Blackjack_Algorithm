# Vars Globales ---------------------
id_sabot_actuel <- -1 # Quand le croupier prend un nouveau sabot
compteur_high_low <- 0 # Compteur High-Low
total_cartes_vues <- 0 # Le nombre total de cartes qui sont sorties

# Sabot de 6 paquets de 52 cartes (our calculer les probabilités conditionnelles)
inventaire_sabot <- c("2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24)

# Traitements ------------------------
# Carte -> Valeur
extraire_valeur <- function(carte_txt) {
  valeur <- gsub("[^0-9JQKA]", "", carte_txt)
  resultat <- switch(valeur,
    "J" = 10,
    "Q" = 10,
    "K" = 10,
    "A" = 11,
    as.numeric(valeur) # Si c'est un chiffre
  )
  return(resultat)
}

# Calcule le total dans la main
evaluer_main_joueur <- function(valeurs_cartes) {
  score <- sum(valeurs_cartes)
  nb_as <- sum(valeurs_cartes == 11) # Combien d'As on a dans la main

  # Si notre score dépasse 21 et on a un As,
  # l'As passe d'une valeur de 11 a 1
  repeat {
  if (score <= 21 || nb_as == 0) {
    break
  }
  score <- score - 10
  nb_as <- nb_as - 1
}

  # Une main est souple s'il nous reste un As qui vaut encore 11
  est_souple <- (nb_as > 0)

  return(list(score = score, est_souple = est_souple))
}

# Donner un poids selon le système High_Low
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

# Gestion de sabot ---------------------
# Renitialiser les compteurs
reinitialiser_sabot <- function(id_nouveau_sabot) {
  id_sabot_actuel <<- id_nouveau_sabot
  compteur_high_low <<- 0
  total_cartes_vues <<- 0
  inventaire_sabot <<- c("2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24)
}

# Mémorise la carte tiré
memoriser_carte <- function(valeur_carte) {
  valeur_txt <- as.character(valeur_carte)

  # On retire une du stock
  if (inventaire_sabot[valeur_txt] > 0) {
    inventaire_sabot[valeur_txt] <<- inventaire_sabot[valeur_txt] - 1
  }

  compteur_high_low <<- compteur_high_low + calculer_high_low(valeur_carte)
  total_cartes_vues <<- total_cartes_vues + 1
}

# Calculs Probas Conditionnels ------------
# Proba que le joueur depasse 21 en tirant une carte
calculer_bust_joueur <- function(score, est_souple) {
  # Si on a une main souple -> pas de risque
  if (est_souple) {
    return(0.0)
  }

  # Combien de points on a  besoin pour atteindre 21 :
  marge <- 21 - score

  # Si la marge est de 11 ou plus, pas de risque
  if (marge >= 11) {
    return(0.0)
  }

  # Combien reste de cartes dans tout le sabot :
  cartes_restantes <- sum(inventaire_sabot)
  if (cartes_restantes == 0) {
    return(0.0) # Evite de diviser pr 0
  }

  # Les cartes qui sont plus grandes que notre marge
  cartes_danger <- as.character((marge + 1):11)

  # Combien il reste de ces cartes danger dans notre inventaire
  cartes_danger <- cartes_danger[cartes_danger %in% names(inventaire_sabot)]
  nb_cartes_danger <- sum(inventaire_sabot[cartes_danger])

  return(nb_cartes_danger / cartes_restantes)
}

# Proba que le croupier depasse 21
calculer_bust_croupier <- function(score_croupier, inventaire, a_as_souple = FALSE, profondeur = 0) {
  if (profondeur > 5) {
    return(0.0)
  }

  cartes_totales <- sum(inventaire)
  # Pas de risque
  if (cartes_totales == 0 || score_croupier >= 17) {
    return(0.0)
  }

  proba_bust_total <- 0.0

  cartes_presentes <- inventaire[inventaire > 0]

  for (nom_val in names(cartes_presentes)) {
    count <- cartes_presentes[nom_val]
    valeur <- as.numeric(nom_val)
    proba_carte <- count / cartes_totales

    nouveau_score <- score_croupier + valeur
    nouveau_as_souple <- a_as_souple || (valeur == 11)

    if (nouveau_score > 21) {
      if (nouveau_as_souple) {
        nouveau_score <- nouveau_score - 10
        nouveau_as_souple <- FALSE
      }
    }

    if (nouveau_score < 17) {
      # Le croupier retire encire
      inv_reduit <- inventaire
      inv_reduit[nom_val] <- inv_reduit[nom_val] - 1
      
      proba_bust_suite <- calculer_bust_croupier(
        nouveau_score, inv_reduit, nouveau_as_souple, profondeur + 1
      )
      proba_bust_total <- proba_bust_total + (proba_carte * proba_bust_suite)

    } else if (nouveau_score > 21) {
      # Le croupier a depasser
      proba_bust_total <- proba_bust_total + proba_carte
      
    }
    # Si le score est entre 17 et 21, il ne se passe rien
  }

  return(proba_bust_total)
}

# Proba que le croupier vas avoir une excellente main (17 a 21) 
calculer_fort_croupier <- function(valeur_croupier) {
  total_cartes <- sum(inventaire_sabot)
  if (total_cartes == 0) {
    return(0.0)
  }

  # Quelles cartes amene entre 17 et 21 :
  seuil_bas <- max(2, 17 - valeur_croupier)
  seuil_haut <- min(11, 21 - valeur_croupier)
  if (seuil_bas > seuil_haut) {
    return(0.0)
  }

  cartes_cibles <- as.character(seuil_bas:seuil_haut)
  cartes_cibles <- cartes_cibles[cartes_cibles %in% names(inventaire_sabot)]
  nb_cartes_cibles <- sum(inventaire_sabot[cartes_cibles])

  return(nb_cartes_cibles / total_cartes)
}

# Decision :
# ----------
getDecision <- function(main_joueur, main_croupier, id_sabot) {
  # 1- Vérifier est ce que c un nouveau sabot
  if (id_sabot != id_sabot_actuel) {
    reinitialiser_sabot(id_sabot)
  }

  # 2- Extraire la valeur des cartes
  valeurs_joueur <- sapply(main_joueur, extraire_valeur)
  valeur_croupier <- extraire_valeur(main_croupier[1])

  # On évalue le score actuel du joueur
  etat_joueur <- evaluer_main_joueur(valeurs_joueur)
  score_joueur <- etat_joueur$score
  est_souple <- etat_joueur$est_souple

  # 3- Mémorisation des cartes
  # Quand on a 2 cartes, c'est le début du tour donc :
  #   mémoriser 2 cartes + carte croupier
  # On a plus de 2 cartes, c'est qu'on vient de demandeer un HIT donc :
  #   mémoriser la dernière carte reçue (ne pas compter double)
  if (length(main_joueur) == 2) {
    memoriser_carte(valeurs_joueur[1])
    memoriser_carte(valeurs_joueur[2])
    memoriser_carte(valeur_croupier)
  } else {
    memoriser_carte(valeurs_joueur[length(valeurs_joueur)])
  }

  # 4- Calcul des probabilités
  # Le true_count ameliore le compteur High-Low en le divisant par le nombre de paquets restants
  paquets_restants <- max(1, (312 - total_cartes_vues) / 52)
  true_count <- compteur_high_low / paquets_restants

  # Appele au fonctions proba
  risque_bust <- calculer_bust_joueur(score_joueur, est_souple)
  proba_croupier_bust <- calculer_bust_croupier(valeur_croupier, inventaire_sabot, a_as_souple = (valeur_croupier == 11))
  proba_croupier_fort <- calculer_fort_croupier(valeur_croupier)

  # 5- Decision
  # Régles de base
  if (score_joueur <= 11) {
    return("H")
  }
  if (score_joueur >= 19) {
    return("S")
  }

  # Cas spécial (mains souples)
  if (est_souple) {
    if (score_joueur == 18) {
      # Stragtegie trouvé pour soupe a  18 :
      #  Croupier montre 9, 10 ou 11  -> HIT (il risque de faire 19-21)
      #  Croupier montre 2, 7, 8                      -> STAND (notre 18 est suffisant)
      #  Croupier montre 3, 4, 5, 6                   -> (Double interdit) -> STAND
      if (valeur_croupier %in% c(9, 10, 11)) {
        return("H")
      } else {
        return("S")
      }
    }
    return("H") # En dessous de 18 souple, on tire
  }

  # Si on a une main normale a 17 ou 18, on s'arrête (psq trop de risque)
  if (score_joueur >= 17) {
    return("S")
  }

  # 0.40 : On refuse de risquer plus de 40% de bust
  # + (proba_croupier_fort * 0.30) : On prend un peu plus de risque pour tenter de le battre
  # - (proba_croupier_bust * 0.45) : On bloque la prise de risque et on le laisse depasser
  # Sécurité (65%) : Evite un tirage danger si le risque de depasser est trop elevé
  risque_max <- 0.40 + (proba_croupier_fort * 0.30) - (proba_croupier_bust * 0.45)
  risque_max <- min(risque_max, 0.65) # Eviter les risques de > 70%

  # Ajuste un peu le risque avec le True Count
  # Si le compteur est très positif, il reste plein de 10 (plus de chances de depasser) donc :
  #   On baisse la tolérance au risque
  risque_max <- risque_max - (true_count * 0.03)

  # Decision Finale :
  if (risque_bust > risque_max) {
    return("S") # Trop risqué, on s'arrête (Stand)
  } else {
    return("H") # Risque acceptable, on tire une carte (Hit)
  }
}
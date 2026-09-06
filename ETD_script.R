# ------------------------------------------------------------------------------
# 1. VARIABLES GLOBALES (ÉTAT DU JEU)
# ------------------------------------------------------------------------------
# Ces variables maintiennent la mémoire de l'algorithme entre chaque tour
g_id_sabot_actuel <- -1
g_compteur_wong <- 0
g_total_cartes_vues <- 0

# Inventaire exact pour calculer les probabilités conditionnelles
g_inventaire_sabot <- c(
  "2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, 
  "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24
)

# ------------------------------------------------------------------------------
# 2. FONCTIONS DE TRAITEMENT DES DONNÉES (PARSING ET ÉVALUATION)
# ------------------------------------------------------------------------------

# Extrait la valeur brute de la chaîne (ex: "Q♥" -> 10)
parser_carte <- function(chaine_carte) {
  chaine_nette <- gsub("[^0-9JQKA]", "", chaine_carte)
  if (chaine_nette %in% c("J", "Q", "K")) {
    return(10)
  } else if (chaine_nette == "A") {
    return(11)
  } else {
    return(as.numeric(chaine_nette))
  }
}

# Calcule le score optimal et identifie si la main est "souple" (As = 11)
evaluer_main_joueur <- function(valeurs_num) {
  score_total <- sum(valeurs_num)
  compte_as <- sum(valeurs_num == 11)
  
  while (score_total > 21 && compte_as > 0) {
    score_total <- score_total - 10
    compte_as <- compte_as - 1
  }
  
  main_est_souple <- (compte_as > 0)
  return(list(score = score_total, est_souple = main_est_souple))
}

# Attribue un poids selon le système Wong Halves pour évaluer la tension du jeu
obtenir_valeur_wong_halves <- function(valeur) {
  if (valeur == 5) return(1.5)
  if (valeur %in% c(3, 4, 6)) return(1.0)
  if (valeur %in% c(2, 7)) return(0.5)
  if (valeur == 8) return(0.0)
  if (valeur == 9) return(-0.5)
  if (valeur >= 10) return(-1.0)
  return(0)
}

# ------------------------------------------------------------------------------
# 3. GESTION DE L'ÉTAT ET DU SABOT
# ------------------------------------------------------------------------------

reinitialiser_sabot <- function(nouvel_id) {
  g_id_sabot_actuel <<- nouvel_id
  g_compteur_wong <<- 0
  g_total_cartes_vues <<- 0
  g_inventaire_sabot <<- c(
    "2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, 
    "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24
  )
}

mettre_a_jour_memoire <- function(valeur_carte) {
  val_str <- as.character(valeur_carte)
  
  if (g_inventaire_sabot[val_str] > 0) {
    g_inventaire_sabot[val_str] <<- g_inventaire_sabot[val_str] - 1
  }
  g_compteur_wong <<- g_compteur_wong + obtenir_valeur_wong_halves(valeur_carte)
  g_total_cartes_vues <<- g_total_cartes_vues + 1
}

# ------------------------------------------------------------------------------
# 4. MOTEUR STATISTIQUE (CALCUL DES PROBABILITÉS EN TEMPS RÉEL)
# ------------------------------------------------------------------------------

calculer_risque_joueur <- function(score, est_souple) {
  # Immunité : tirer avec une main souple ne fait jamais sauter au tour actuel
  if (est_souple) return(0.0)
  
  marge <- 21 - score
  if (marge >= 11) return(0.0)
  
  cartes_totales_restantes <- sum(g_inventaire_sabot)
  if (cartes_totales_restantes == 0) return(0.0)
  
  valeurs_perdues <- as.character((marge + 1):11)
  valeurs_perdues <- valeurs_perdues[valeurs_perdues %in% names(g_inventaire_sabot)]
  cartes_perdues_restantes <- sum(g_inventaire_sabot[valeurs_perdues])
  
  return(cartes_perdues_restantes / cartes_totales_restantes)
}

evaluer_faiblesse_croupier <- function(valeur_croupier) {
  # Calcule la probabilité que la carte cachée du croupier le place 
  # dans une zone critique (12 à 16), maximisant ses chances de sauter ensuite.
  cartes_totales_restantes <- sum(g_inventaire_sabot)
  if (cartes_totales_restantes == 0) return(0.0)
  
  cible_min <- max(2, 12 - valeur_croupier)
  cible_max <- min(11, 16 - valeur_croupier)
  if (cible_min > cible_max) return(0.0) 
  
  valeurs_cibles <- as.character(cible_min:cible_max)
  valeurs_cibles <- valeurs_cibles[valeurs_cibles %in% names(g_inventaire_sabot)]
  cartes_cibles_restantes <- sum(g_inventaire_sabot[valeurs_cibles])
  
  return(cartes_cibles_restantes / cartes_totales_restantes)
}

evaluer_force_croupier <- function(valeur_croupier) {
  # Probabilité que le croupier atteigne une main forte (17-21) avec sa prochaine carte
  cartes_totales_restantes <- sum(g_inventaire_sabot)
  if (cartes_totales_restantes == 0) return(0.0)
  
  cible_min <- max(2, 17 - valeur_croupier)
  cible_max <- min(11, 21 - valeur_croupier)
  
  if (cible_min > cible_max) return(0.0)
  
  valeurs_cibles <- as.character(cible_min:cible_max)
  valeurs_cibles <- valeurs_cibles[valeurs_cibles %in% names(g_inventaire_sabot)]
  cartes_cibles_restantes <- sum(g_inventaire_sabot[valeurs_cibles])
  
  return(cartes_cibles_restantes / cartes_totales_restantes)
}

# ------------------------------------------------------------------------------
# 5. FONCTION PRINCIPALE DE DÉCISION
# ------------------------------------------------------------------------------

getDecision <- function(main_joueur, main_croupier, id_sabot) {
  
  # 1. Vérification du cycle de vie du sabot
  if (id_sabot != g_id_sabot_actuel) {
    reinitialiser_sabot(id_sabot)
  }
  
  # 2. Parsing et extraction
  valeurs_joueur <- sapply(main_joueur, parser_carte)
  valeur_croupier <- parser_carte(main_croupier[1])
  
  etat_main <- evaluer_main_joueur(valeurs_joueur)
  score_actuel <- etat_main$score
  est_souple <- etat_main$est_souple
  
  # 3. Synchronisation de la mémoire globale
  # Évite le double comptage lors de tirages successifs dans la même manche
  if (length(main_joueur) == 2) {
    mettre_a_jour_memoire(valeurs_joueur[1])
    mettre_a_jour_memoire(valeurs_joueur[2])
    mettre_a_jour_memoire(valeur_croupier)
  } else {
    mettre_a_jour_memoire(valeurs_joueur[length(valeurs_joueur)])
  }
  
  # 4. Calcul des métriques décisionnelles
  paquets_restants <- max(1, (312 - g_total_cartes_vues) / 52)
  vrai_compte <- g_compteur_wong / paquets_restants
  
  risque_joueur <- calculer_risque_joueur(score_actuel, est_souple)
  faiblesse_croupier <- evaluer_faiblesse_croupier(valeur_croupier)
  force_croupier <- evaluer_force_croupier(valeur_croupier)
  
  # 5. Logique de décision algorithmique
  
  if (score_actuel <= 11) return("H")
  if (score_actuel >= 19) return("S")
  
  # Traitement des mains souples (immunisées au Bust immédiat)
  if (est_souple) {
    if (score_actuel == 18) {
      # Face à un croupier très fort (9, 10, As), un 18 souple n'est souvent pas suffisant
      if (force_croupier > 0.50) return("H") else return("S")
    }
    return("H") 
  }
  
  # Traitement des mains dures (Score de 12 à 18)
  if (score_actuel >= 17) return("S")
  
  # Définition dynamique du seuil de tolérance au risque
  # Formule combinant la force du croupier (qui nous pousse à prendre des risques)
  # et sa faiblesse (qui nous incite à le laisser sauter).
  seuil_risque_acceptable <- 0.45 + (force_croupier * 0.55) - (faiblesse_croupier * 0.30)
  
  # Ajustement final avec le True Count
  # Un sabot riche en cartes fortes abaisse la tolérance au risque
  seuil_risque_acceptable <- seuil_risque_acceptable - (vrai_compte * 0.03)
  
  # Verdict final
  if (risque_joueur > seuil_risque_acceptable) {
    return("S")
  } else {
    return("H")
  }
}
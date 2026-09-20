# VARS GLOBALES
# ------------------
# Pour maintenir la mémoire de l'algorithme entre chaque tour
# Parce que la fonction getDecision() "oublie" tout à chaque fois qu'elle a fini de s'exécuter
g_id_sabot_actuel <- -1 # Permet de détecter quand le croupier prend un nouveau sabot
g_compteur_hilo <- 0   # Compteur High-Low
g_total_cartes_vues <- 0 # Le nombre total de cartes qui sont déjà sorties

# Inventaire exact pour calculer les probabilités conditionnelles
# On simule un sabot de 6 paquets de 52 cartes (312 cartes au total).
g_inventaire_sabot <- c(
  "2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, 
  "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24
)

# FONCTIONS DE TRAITEMENT DES DONNÉES
# -----------------------------------

# Transforme une carte (texte) en valeur mathématique (ex: "Q♥" = 10)
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

# Calcule le total de la main et gère le cas spécial de l'As.
# la main est "souple" (si As = 11)
evaluer_main_joueur <- function(valeurs_num) {
  score_total <- sum(valeurs_num) # Additionne toutes les cartes
  compte_as <- sum(valeurs_num == 11) # Compte combien d'As on a dans la main
  
  # Si notre score dépasse 21 et qu'on a un As, 
  # l'As passe d'une valeur de 11 à une valeur de 1 pour nous sauver.
  while (score_total > 21 && compte_as > 0) {
    score_total <- score_total - 10 # On retire 10 (c'est comme si le 11 devenait 1)
    compte_as <- compte_as - 1 # Cet As a été utilisé, on le décompte
  }
  
  # Une main est "souple" (soft) s'il nous reste un As qui vaut encore 11.
  # Important parceque Tirer une carte sur une main souple est 100% sûr : on ne peut pas sauter.
  main_est_souple <- (compte_as > 0)

  return(list(score = score_total, est_souple = main_est_souple))
}

# Attribue un poids selon le système High-Low (Hi-Lo) vu en cours
obtenir_valeur_hilo <- function(valeur) {
  if (valeur >= 2 && valeur <= 6) return(1)   # Petites cartes favorables au joueur
  if (valeur >= 7 && valeur <= 9) return(0)   # Cartes neutres
  if (valeur >= 10) return(-1)                # Cartes fortes (10, têtes, As)
  return(0)
}

# GESTION DE L'ÉTAT ET DU SABOT
# -----------------------------

# Remet tous les compteurs à zéro quand un nouveau sabot commence.
reinitialiser_sabot <- function(nouvel_id) {
  # L'opérateur <<- permet de modifier la variable globale et pas juste une copie locale.
  g_id_sabot_actuel <<- nouvel_id
  g_compteur_hilo <<- 0
  g_total_cartes_vues <<- 0
  g_inventaire_sabot <<- c(
    "2" = 24, "3" = 24, "4" = 24, "5" = 24, "6" = 24, 
    "7" = 24, "8" = 24, "9" = 24, "10" = 96, "11" = 24
  )
}

# Mémorise une carte qui vient d'être tirée sur la table.
mettre_a_jour_memoire <- function(valeur_carte) {
  val_str <- as.character(valeur_carte) # Transforme le chiffre en texte (ex: 10 -> "10")
  
  # Si la carte est bien dans l'inventaire, on en retire une du stock
  if (g_inventaire_sabot[val_str] > 0) {
    g_inventaire_sabot[val_str] <<- g_inventaire_sabot[val_str] - 1
  }
  
  # On actualise le compteur High-Low et le nombre de cartes vues
  g_compteur_hilo <<- g_compteur_hilo + obtenir_valeur_hilo(valeur_carte)
  g_total_cartes_vues <<- g_total_cartes_vues + 1
  
  # Note stratégique pour justifier notre algo : Le simulateur ne nous montre pas la carte cachée 
  # du croupier ni celles qu'il tire à la fin. L'inventaire n'est donc pas 100% exact, 
  # mais il est statistiquement très fiable pour prendre des décisions.
}

# MOTEUR STATISTIQUE (Probas conditionnelles)
# -------------------------------------------

# Calcule la probabilité exacte (en %) qu'on dépasse 21 si on tire une carte.
calculer_risque_joueur <- function(score, est_souple) {
  # Si on a une main souple (ex: As+5 = 16), on ne peut JAMAIS sauter en tirant.
  # L'As redescendra à 1 si on tire un 10. Risque = 0.
  if (est_souple) return(0.0)
  
  # De combien de points a-t-on besoin pour atteindre 21 ?
  marge <- 21 - score 
  
  # Si la marge est de 11 ou plus (ex: on a 10), on ne peut pas sauter car la plus grosse carte est 11.
  if (marge >= 11) return(0.0)
  
  # Combien reste-t-il de cartes dans tout le sabot ?
  cartes_totales_restantes <- sum(g_inventaire_sabot)
  if (cartes_totales_restantes == 0) return(0.0) # Sécurité pour éviter de diviser par zéro
  
  # On identifie les "cartes tueuses" : celles qui sont plus grandes que notre marge
  # Ex : si marge = 6 (on a 15), les cartes tueuses sont 7, 8, 9, 10, 11.
  valeurs_perdues <- as.character((marge + 1):11)
  
  # On regarde combien il reste de ces cartes tueuses dans notre inventaire mémorisé
  valeurs_perdues <- valeurs_perdues[valeurs_perdues %in% names(g_inventaire_sabot)]
  cartes_perdues_restantes <- sum(g_inventaire_sabot[valeurs_perdues])
  
  # On divise les mauvaises cartes par le total des cartes pour avoir le % de chance de perdre.
  return(cartes_perdues_restantes / cartes_totales_restantes)
}

# Calcule les chances du croupier de tomber dans la "zone de danger" (12 à 16).
# S'il est dans cette zone, il est obligé de retirer une carte et risque très fort de sauter.
evaluer_faiblesse_croupier <- function(valeur_croupier) {
  cartes_totales_restantes <- sum(g_inventaire_sabot)
  if (cartes_totales_restantes == 0) return(0.0)
  
  # Quelles cartes amènent le croupier entre 12 et 16 ?
  cible_min <- max(2, 12 - valeur_croupier)
  cible_max <- min(11, 16 - valeur_croupier)
  if (cible_min > cible_max) return(0.0) 
  
  # Même logique que précédemment : on compte combien il reste de ces cartes cibles
  valeurs_cibles <- as.character(cible_min:cible_max)
  valeurs_cibles <- valeurs_cibles[valeurs_cibles %in% names(g_inventaire_sabot)]
  cartes_cibles_restantes <- sum(g_inventaire_sabot[valeurs_cibles])
  
  return(cartes_cibles_restantes / cartes_totales_restantes)
}

# Calcule les chances du croupier d'avoir une excellente main (17 à 21) directement.
evaluer_force_croupier <- function(valeur_croupier) {
  cartes_totales_restantes <- sum(g_inventaire_sabot)
  if (cartes_totales_restantes == 0) return(0.0)
  
  # Quelles cartes amènent le croupier entre 17 et 21 ?
  cible_min <- max(2, 17 - valeur_croupier)
  cible_max <- min(11, 21 - valeur_croupier)
  if (cible_min > cible_max) return(0.0)
  
  valeurs_cibles <- as.character(cible_min:cible_max)
  valeurs_cibles <- valeurs_cibles[valeurs_cibles %in% names(g_inventaire_sabot)]
  cartes_cibles_restantes <- sum(g_inventaire_sabot[valeurs_cibles])
  
  return(cartes_cibles_restantes / cartes_totales_restantes)
}

# FONCTION DE DÉCISION
# -------------------------------

getDecision <- function(main_joueur, main_croupier, id_sabot) {
  
  # 1- Vérification : Est-ce qu'on a commencé un nouveau sabot ?
  if (id_sabot != g_id_sabot_actuel) {
    reinitialiser_sabot(id_sabot)
  }
  
  # 2- On transforme les cartes (texte) en valeurs mathématiques
  # sapply applique la fonction 'parser_carte' à toutes les cartes du joueur
  valeurs_joueur <- sapply(main_joueur, parser_carte)
  valeur_croupier <- parser_carte(main_croupier[1])
  
  # On évalue le score actuel du joueur
  etat_main <- evaluer_main_joueur(valeurs_joueur)
  score_actuel <- etat_main$score
  est_souple <- etat_main$est_souple
  
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
  paquets_restants <- max(1, (312 - g_total_cartes_vues) / 52)
  vrai_compte <- g_compteur_hilo / paquets_restants
  
  # On interroge nos fonctions statistiques
  risque_joueur <- calculer_risque_joueur(score_actuel, est_souple)
  faiblesse_croupier <- evaluer_faiblesse_croupier(valeur_croupier)
  force_croupier <- evaluer_force_croupier(valeur_croupier)
  
  # 5- L'IA PREND SA DÉCISION ICI
  # Règles de base absolues :
  if (score_actuel <= 11) return("H") # On tire toujours si on a 11 ou moins (aucun risque)
  if (score_actuel >= 19) return("S") # On s'arrête toujours à 19 ou plus (trop dangereux)
  
  # Cas spécial des mains souples (immunisées au risque de sauter)
  if (est_souple) {
    if (score_actuel == 18) {
      # Avec 18 souple, si le croupier a de grandes chances de faire 19, 20 ou 21 (force > 50%),
      # on tire pour tenter d'améliorer notre 18. Sinon on reste.
      if (force_croupier > 0.50) return("H") else return("S")
    }
    return("H") # En dessous de 18 souple, on tire toujours
  }
  
  # Si on a une main classique (dure) à 17 ou 18, on s'arrête. Le risque est trop grand.
  if (score_actuel >= 17) return("S")
  
  # Le Seuil de tolérance au risque :
  # 1- On accepte de base 45% de chances de sauter (0.45).
  # 2- Si le croupier est fort, on est obligé de prendre plus de risques pour le battre (+55% max).
  # 3- Si le croupier est faible (risque de sauter), on ne prend aucun risque et on le regarde perdre (-30% max).
  seuil_risque_acceptable <- 0.45 + (force_croupier * 0.55) - (faiblesse_croupier * 0.30)
  
  # On ajuste légèrement ce risque avec le True Count.
  # Si le compteur est très positif, il reste plein de bûches (10), on a donc plus de chances de sauter.
  # On baisse donc notre tolérance au risque.
  seuil_risque_acceptable <- seuil_risque_acceptable - (vrai_compte * 0.03)
  
  # DECISION FINALE : On compare notre risque réel avec le risque qu'on accepte de prendre :
  if (risque_joueur > seuil_risque_acceptable) {
    return("S") # Trop risqué, on s'arrête (Stand)
  } else {
    return("H") # Risque acceptable, on tire une carte (Hit)
  }
}
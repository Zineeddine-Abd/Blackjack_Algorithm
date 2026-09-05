# ==============================================================================
# MOTEUR DE SIMULATION BLACKJACK (CLONE VPL)
# ==============================================================================

# 1. OUTILS INTERNES DU MOTEUR (Indépendants de ton algorithme)
# ------------------------------------------------------------------------------

# Génère un sabot mélangé de N jeux de 52 cartes
generer_sabot_moteur <- function(nb_jeux = 6) {
  valeurs <- c(2:10, "J", "Q", "K", "A")
  couleurs <- c("♥", "♦", "♣", "♠")
  un_jeu <- paste0(rep(valeurs, each = 4), rep(couleurs, times = 13))
  sabot_complet <- rep(un_jeu, nb_jeux)
  return(sample(sabot_complet)) # Mélange aléatoire
}

# Calcule le score exact d'une main pour l'arbitrage du moteur
calculer_score_moteur <- function(main) {
  valeurs_brutes <- gsub("[^0-9JQKA]", "", main)
  score <- 0
  nb_as <- 0
  
  for (v in valeurs_brutes) {
    if (v %in% c("J", "Q", "K")) {
      score <- score + 10
    } else if (v == "A") {
      score <- score + 11
      nb_as <- nb_as + 1
    } else {
      score <- score + as.numeric(v)
    }
  }
  
  while (score > 21 && nb_as > 0) {
    score <- score - 10
    nb_as <- nb_as - 1
  }
  return(score)
}

# 2. BOUCLE PRINCIPALE DE SIMULATION
# ------------------------------------------------------------------------------

lancer_simulation_vpl <- function(nb_parties = 1000, nb_jeux_sabot = 6) {
  
  # Statistiques
  victoires <- 0
  defaites <- 0
  egalites <- 0
  
  # Initialisation du sabot
  sabot <- generer_sabot_moteur(nb_jeux_sabot)
  id_sabot_actuel <- 1
  
  cat(sprintf("Lancement de %d parties de Blackjack...\n", nb_parties))
  
  for (partie in 1:nb_parties) {
    
    # Mélange du sabot s'il reste moins de 30% des cartes
    if (length(sabot) < (52 * nb_jeux_sabot * 0.30)) {
      sabot <- generer_sabot_moteur(nb_jeux_sabot)
      id_sabot_actuel <- id_sabot_actuel + 1
    }
    
    # Distribution initiale (2 cartes joueur, 2 cartes croupier dont 1 cachée)
    main_joueur <- sabot[1:2]
    main_croupier <- sabot[3:4]
    sabot <- sabot[-(1:4)] # Retire les 4 cartes du sabot
    
    # Phase 1 : Tour du joueur
    # Le moteur n'envoie que la carte visible du croupier (index 1)
    joueur_bust <- FALSE
    
    repeat {
      score_joueur <- calculer_score_moteur(main_joueur)
      
      if (score_joueur > 21) {
        joueur_bust <- TRUE
        break
      }
      
      # Appel de TA fonction (qui doit être chargée dans l'environnement)
      decision <- getDecision(main_joueur, main_croupier[1], id_sabot_actuel)
      
      if (decision == "H") {
        # Tirage d'une carte
        main_joueur <- c(main_joueur, sabot[1])
        sabot <- sabot[-1]
      } else if (decision == "S") {
        break
      } else {
        stop(sprintf("ERREUR VPL: La fonction a renvoyé '%s' au lieu de 'H' ou 'S'.", decision))
      }
    }
    
    # Phase 2 : Résolution et tour du croupier
    if (joueur_bust) {
      defaites <- defaites + 1
      next # Passe à la partie suivante
    }
    
    # Le croupier joue avec ses règles strictes : Hit jusqu'à 17 minimum
    repeat {
      score_croupier <- calculer_score_moteur(main_croupier)
      if (score_croupier >= 17) {
        break
      }
      main_croupier <- c(main_croupier, sabot[1])
      sabot <- sabot[-1]
    }
    
    # Phase 3 : Arbitrage
    score_joueur_final <- calculer_score_moteur(main_joueur)
    score_croupier_final <- calculer_score_moteur(main_croupier)
    
    if (score_croupier_final > 21) {
      victoires <- victoires + 1
    } else if (score_joueur_final > score_croupier_final) {
      victoires <- victoires + 1
    } else if (score_joueur_final < score_croupier_final) {
      defaites <- defaites + 1
    } else {
      egalites <- egalites + 1
    }
  }
  
  # 3. AFFICHAGE DES RÉSULTATS
  # ------------------------------------------------------------------------------
  taux_victoire <- (victoires / nb_parties) * 100
  taux_victoire_hors_egalite <- (victoires / (victoires + defaites)) * 100
  
  cat("==========================================\n")
  cat("RÉSULTATS DE LA SIMULATION (", nb_parties, " parties)\n", sep="")
  cat("==========================================\n")
  cat("Victoires :", victoires, "\n")
  cat("Défaites  :", defaites, "\n")
  cat("Égalités  :", egalites, "\n")
  cat("------------------------------------------\n")
  cat(sprintf("Win Rate Brut       : %.2f %%\n", taux_victoire))
  cat(sprintf("Win Rate (V vs D)   : %.2f %%\n", taux_victoire_hors_egalite))
  cat("==========================================\n")
}

# --- Lancement ---
# 1. On charge l'algorithme de décision
source("ETD_script.R") 

# 2. On lance la simulation avec 1000 parties
lancer_simulation_vpl(nb_parties = 1000)
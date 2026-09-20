# 🃏 Blackjack Probability Engine

Un agent décisionnel automatisé développé en **R** pour jouer au Blackjack de manière optimale. 

L'objectif de ce projet est de maximiser le taux de victoire (*Win Rate*) face au croupier, sans utiliser de matrices statiques traditionnelles (tableaux de stratégie de base). À la place, l'agent calcule en **temps réel** les probabilités exactes du jeu en fonction des cartes restantes.

## Le Défi et les Contraintes

L'agent évolue dans un environnement contraint :
- **Pas de "Double" ni de "Split"** (Séparation des paires).
- **Asymétrie de l'information :** L'agent ne voit que ses propres cartes et la première carte du croupier (la deuxième est cachée).
- **Un seul point d'entrée :** L'algorithme réside dans une fonction unique `getDecision()` qui retourne `"H"` (Tirer / Hit) ou `"S"` (S'arrêter / Stand).
- **Mémoire limitée :** Le script maintient un état de jeu invisible à l'aide de variables globales pour compter les cartes.

## Comment ça marche ?

L'algorithme n'utilise aucune matrice de décision fixe. Il repose sur un **moteur probabiliste dynamique** :

1. **Comptage de cartes (Système High-Low) :** L'agent garde en mémoire chaque carte vue en appliquant la méthode d'Edward Thorp vue en cours (+1 pour les petites cartes, -1 pour les fortes, 0 pour les neutres) pour évaluer si le sabot restant est riche en cartes fortes (avantageux pour le joueur) ou faibles.
2. **Évaluation du risque de Bust :** Avant chaque décision, le script interroge sa mémoire du sabot pour calculer exactement ses chances de "sauter" (dépasser 21) s'il tire une carte de plus.
3. **Analyse de la force du croupier :** L'algorithme calcule mathématiquement les chances du croupier d'atteindre un score fort (17-21) du premier coup, ou à l'inverse, sa probabilité de sauter.
4. **Seuil de risque flottant :** La stratégie s'adapte en temps réel. Face à un croupier très fort (qui montre un 10), l'agent prendra d'énormes risques pour survivre. Face à un croupier faible (qui montre un 6), l'agent adoptera une stratégie très conservatrice et refusera de prendre des risques inutiles.

## 📊 Performances

Le Blackjack est un jeu où le casino possède un avantage mathématique infranchissable (car le joueur tire en premier, et s'il saute, il perd immédiatement, même si le croupier saute après lui). Avec les règles strictes de ce projet (pas de Split/Double), le taux de victoire parfait absolu d'un ordinateur plafonne à **~47,5 %**.

Notre moteur probabiliste atteint **47,43 % de victoires** (Victoires vs Défaites, hors égalités) sur de grands échantillons, atteignant ainsi le **maximum théorique mathématique** de ce jeu sous ces contraintes.

## Lancer une simulation

Pour tester l'agent, utilisez le script de simulation fourni. Ce script s'occupe de gérer le déroulement du jeu, de distribuer les cartes et d'appeler l'algorithme `ETD_script.R`.

```R
# Depuis R ou la console RStudio
source("Similateur_VPL.R")
# Projet de programmation fonctionnelle

## Utilisation

### Compilation
L'ensemble des sources sont disponibles à la racine du projet.

Pour compiler les exécutables des 3 phases du projet, un Makefile est mis à votre disposition. Vous vous suffit donc de lancer la commande `make` dans un terminal à la racine du projet.<br>
Cela génère les trois fichiers `phase1`, `phase2` et `phase3`.

### Exécution
Pour exécuter chaque programme, il suffit lancer dans un terminal :

    ./phase1 test1.txt
    ./phase2 test2.txt
    ./phase3 test3.txt
La structure d'un fichier de test doit correspondre au programme avec lequel il est lancé. Des fichiers de test sont disponibles dans le dossier test/ du projet.

## Description des fichiers
- `phase1` est le module lançant l'algorithme de la phase 1.
- `phase2` est le module lançant l'algorithme de la phase 2.
- `phase3` est le module lançant l'algorithme de la phase 3.
- `table` est le module contenant l'ensemble des fonctions servant au fonctionnement de l'algorithme de chaque phase.
- `analyse` est le module permettant la lecture de fichiers et l'affichage des résultats de chaque phase.
- `print.ml` répertorie des fonctions d'affichage des différentes structures utilisées dans le module `table`.

## Rapport
Le rapport est disponible au format `pdf`, ou au format `tex` disponible dans le dossier /doc du projet.

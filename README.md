# Pokémon Free Fight

Le Pokémon Free Fight est un art martial, à la croisée du MMA et des TCG (Trading Card Game). 

Deux dresseurs de Pokémon se défie dans une arêne, et le combat ne s'arrête que lorsqu'un des dresseurs est KO. Contrairement aux combats de Pokemon habituels, l'objectif n'est pas de venir à bout des pokémons du dresseurs adverse, mais du dresseur lui même !

Let's Get Ready To Rumble

## Règles du jeu :

* Chaque dresseurs commence le duel avec 500 points de vie (HP) et 0 points de rage (MANA)
* Chaque dresseur commence le combat avec 6 pokémons
* Les pokémons possèdent des points de vie et deux attaques : une pouvant être utilisée sans limite, la seconde nécessitant un certain nombre de points de rage.
* A la fin de chaque tour, les dresseurs gagnent un point de rage. Un bonus supplémentaire d'un point est attribué aux dresseurs ayant perdu leur pokémon dans le tour.
* Le combat se termine lorsqu'un des dresseurs n'a plus de points de vie

## Gameplay minimal : 

* Avant le début du combat, chaque dresseur classe ses Pokemon dans l'ordre où il souhaite les faire combattre.

* A chaque tour, les deux dresseurs donnent un ordre -de manière simultané- à leur pokémon (ils ne savent pas quel ordre a été donné par leur adversaire)
  - Attaquer le dresseur adverse
  - Attaquer le pokémon adverse
  - Défendre le dresseur

* Lorsque les ordres ont été donnés, les pokemons exécutent leur action :

Ordre pokémon A | Ordre pokémon B | Action
---------------- | ---------------- | -----------------
Attaque pkm      | Attaque pkm      | Les deux pokémons s'attaquent mutuellement
Attaque pkm      | Attaque dresseur | A inflige des dégats à B. B inflige des dégats à dresseurs A
Défense dresseur | Attaque pkm      | A n'inflige aucun dégât. B inflige des dégâts minorés de 25% à A
Défense dresseur | Attaque dresseur | A inflige des 50% de dégâts à B en se défandant. B inflige 25% de dégats au dresseur
Défense dresseur | Défense dresseur | Chaque pokémon récupère 10% des HP manquants

* Un pokémon meurt lorsque ses HP tombent à 0, il est alors automatiquement remplacé par le prochain pokémon du dresseur.

* Le combat continue tant qu'aucun dresseur ne soit KO (c'est à dire que ses HP tombent à 0)

## TP

### Day one : simulateur de combat

L'objectif du TP est de programmer une intelligence artificielle et un simulateur de combat.

Dans un premier temps, le focus doit être mis sur la *simulation du combat*, avec des IA jouant de manière aléatoire (choix des pokémons et des actions).

## Référentiels

La liste des pokemons et de leurs attaques est disponible ici `[resources/pokemons.json](https://github.com/studiodev/pokemon-free-fight/blob/master/src/main/resources/pokemons.json)`.

```
   {
      "number":93,
      "name":"Spectrum",
      "type":"ghost",
      "stats":{
         "hp":135,
         "speed":95            // Non utilisé pour le TP1
      },
      "basic-attack":{
         "name":"représailles",
         "type":"ghost",
         "damage":45
      },
      "power-attack":{
         "name":"vibrobscur",
         "type":"ghost",
         "damage":80,
         "cost":4              // Cout en rage (mana) pour déclencher cette attaque
      }
   }
```



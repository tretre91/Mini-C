// calcule la factorielle de 5
// et affiche le caractère ascii correspondant

#include <stdio.h>

int PARAM = 5;

/**
 * @brief fonction factorielle
 * @param n Le nombre dont on veut calculer la factorielle
 * @return n!
 */
int fact(int n /* si n est négatif alors on renvoie 1 */) {
  if (n < 2) {
    return 1; // cas de base
  } else {
    return n * fact(n - 1); // appel récursif
  }
}

/* Ceci est un 
 /* commentaire multiligne */

void main() {
  putchar(fact(PARAM));
  putchar(10);
}

# Mini-C

Projet pour le cours de compilation.

## Syntaxe du langage

Un programme Mini-C est composé d'une suite de déclaration de variables globales suivie de déclarations de fonctions

### Types

Les types disponibles sont `int`, `bool` et `void` (seulement pour les fonctions).

### Variables globales

Une variable globale ne peut pas avoir comme valeur initiale

### Fonctions

Une fonction est une suite de déclarations de variables locales suivies d'une suite d'instructions :

```c
int foo(int i) {
  int a = 1;
  bool b;

  a = a + i;
  return a;
}
```

### Instructions

Les instructions supportées sont :

| instructions                  | description                                   |
| ----------------------------- | --------------------------------------------- |
| `putchar(n);`                 | Affiche le caractère dont le code ascii est n |
| `x = v;`                      | Assigne la valeur v à la variable x           |
| `if (cond) {...} else {...} ` | Branchement conditionnel                      |
| `while (cond) {...}`          | Boucle while                                  |
| `return e;`                   | Renvoie la valeur de l'expression e           |
| `e;`                          | Evalue l'expression e                         |

### Expressions

Une expression peut être sous une des formes suivantes :

| expression                    | exemple                   |
| ----------------------------- | ------------------------- |
| **Littéraux**                 |                           |
| constante entière             | `-5`                      |
| constante booléenne           | `true`, `false`           |
| **Opérations arithmétiques**  |                           |
| addition                      | `1 + -5`                  |
| soustraction                  | `1 - 5`                   |
| multiplication                | `(1 + 1) * 2`             |
| division                      | `10 / 2`                  |
| **Opérations de comparaison** |                           |
| égalité                       | `1 == 1`, `true != false` |
| inégalité stricte             | `1 < 2`, `2 > 1`          |
| inégalité large               | `1 <= 1`, `3 <= 4`        |
| **Autres**                    |                           |
| variable                      | `x`                       |
| appel de fonction             | `foo(5, false)`           |

## Fonctionnalités supplémentaires

### Interpréteur

Un programme Mini-C peut être interprété en utilisant la fonction `interpret_program : prog -> int` du module `Libminic.Minic_interpreter`. La fonction d'interprétation exécute la fonction main du programme si elle existe et renvoie sa valeur de retour.

Pour interpréter un programme utiliser l'option `-i` :
```
minic prog.mnc -i
```

### Vérifications supplémentaires

Par défaut certaines vérifications supplémentaires sont activées, elles peuvent être désactivées en utilisant l'option `-lax`
```
minic prog.mnc -lax
```

Les aspects du code vérifiés sont les suivants :

#### redéfinition d'une variable dans la même portée

Exemple de codes invalides sans `-lax` :
```c
int i = 5;
int i = 2;
```

```c
void foo() {
  int i = 4;
  int i = 5;
}
```

En revanche le code suivant est valide :
```c
int i = 5;

void foo(int i) {
  bool i = false;
}
```

#### appel de fonction dans la portée globale

Exemple :

```c
int i = count();
int c = -1;

int count() {
  c = c + 1;
  return c;
}

int main() {
  return 0;
}
```

Dans ce code, l'appel à `count` utilise la variable globale `c` qui n'est pas encore définie à ce niveau de l'initialisation des variables globales, l'exécution du programme échouera.

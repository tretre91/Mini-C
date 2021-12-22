# Mini-C

Projet pour le cours de compilation.

## Syntaxe du langage

Un programme Mini-C est composé d'une suite de déclaration de variables globales suivie de déclarations de fonctions

### Commentaires

Les commentaires simples (`// ...`) et multilignes (`/* ... */`) sont supportés.

**Remarque :** les commentaires multilignes s'arrêtent au premier `*/` rencontré, par exemple le fragment de code suivant produira une erreur
```rs
/*
  Un commentaire /* Dans un autre commentaire */
*/
```

### Types

Les types disponibles sont `int`, `bool` et `void` (seulement pour les fonctions).

### Variables globales

Les variables globales sont définies au début du fichier et ne peuvent pas être initialisées avec le résultat d'un appel de fonction (voir [plus bas](#vérifications-supplémentaires)), elles sont accessibles n'importe où dans le programme.

### Variables locales

Les variables locales sont les paramètres de fonction et les variables déclarées au début d'un bloc de code :

```c
int g = 5;

int foo(int a) {
  bool b = a < 12;
  int c = 5;
  {
    int a = a + 2;
    c = c + a;
    if (b) {
      return a;
    } else {}
  }
  
  return c;
}
```

Dans l'exemple précédent les variables `a`, `b` et `c` sont locales, la durée de vie d'une variable va de sa déclaration à la fin du bloc de code dans lequel elle a été déclarée (ou à la fin de la fonction pour les paramètres).

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
| `for (init; cond; incr) {...}`| Boucle for
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
| modulo                        | `18 % 3`                  |
| **Opérations de comparaison** |                           |
| égalité                       | `1 == 1`, `true != false` |
| inégalité stricte             | `1 < 2`, `2 > 1`          |
| inégalité large               | `1 <= 1`, `3 <= 4`        |
| **Opérations logiques**       |                           |
| négation                      | `!true`                   |
| et                            | `3 < 4 && true`           |
| ou                            | `false \|\| 1 == 2`       |
| **Opérations bit-à-bit**      |                           |
| négation                      | `~1`                      |
| et                            | `1 + 2 & 3`               |
| ou                            | `64 | 63`                 |
| ou exclusif                   | `127 ^ 255`               |
| **Décalages**                 |                           |
| vers la gauche (logique)      | `1 << 64`                 |
| vers la droite (arithmétique) | `64 >> 2`                 |
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

#### redéfinition d'une variable dans le même bloc

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

### Afficheur

Il est possible de reconstruire un fichier source à partir d'un arbre de syntaxe à l'aide de la fonction `print_program : prog -> out_channel -> unit` du module `Libminic.Minic_display` qui traduit un ast en code source qui sera écrit dans une variable de type `out_channel`.

Pour reconstruire un programme on peut utiliser l'option `-d` :
```
minic prog.mnc -d
```
Sans argument le code source est envoyé sur la sortie standard, on peut également donner un fichier en argument :
```
minic prog.mnc -d code.out
```

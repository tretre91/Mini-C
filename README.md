# Mini-C

Projet pour le cours de compilation.

---

- [Mini-C](#mini-c)
  - [Syntaxe du langage](#syntaxe-du-langage)
    - [Commentaires](#commentaires)
    - [Types](#types)
    - [Variables](#variables)
    - [Fonctions](#fonctions)
    - [Blocs](#blocs)
    - [Instructions](#instructions)
    - [Expressions](#expressions)
  - [Fonctionnalités supplémentaires](#fonctionnalités-supplémentaires)
    - [Interpréteur](#interpréteur)
    - [Afficheur](#afficheur)
  - [Tests](#tests)

## Syntaxe du langage

Un programme Mini-C est composé de déclarations de variables globales et de déclarations de fonctions (sans ordre spécifique), l'exécution commence à la fonction main.

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

### Variables

Une déclaration de variables peut être de l'une des formes suivantes :

```c
int a;               // a aura une valeur par défaut
bool b = true;
int a = 1, b, c = 2; // déclare 3 variables de type int, b aura une valeur par défaut
```

Les variables globales sont définies n'importe où en dehors d'une fonction, elles sont accessibles partout dans le programme après leur déclaration.

Les variables locales sont visibles dans le bloc de code où elles sont déclarées, les variables sont uniques dans leur bloc, mais peuvent être redéfinies dans des blocs sous-jacents:

```c
int foo(int a) {
  int b;
  int c = 5;
  {
    int a = a + 2;
    int c = c - 1;
    b = a + c;
  }
  
  return b;
}
```

### Fonctions

Une fonction possède des paramètres et un bloc de code :

```c
int foo(int i, bool b) {
  int a = 1;
  a = a + i;
  return a;
}

void bar() {
  putchar(10);
}
```

Une fonction dont le type n'est pas void doit forcement renvoyer une valeur.

### Blocs

Un bloc de code est délimité par des accolades, il peut contenir des déclarations de variable, des instructions ou d'autres blocs de code.

Une variable déclarée dans un bloc est visible (et peut être redéfinie) dans tous ses blocs fils, en revanche elle n'est pas accessible dans son bloc père :

```c
void foo(bool b) {
  int a = 48;
  {
    int b = 3;
    {
      a = a + 1;
      putchar(a);
      int b = b + a;
      putchar(b);
    }
  }
}
```

### Instructions

Les instructions supportées sont :

| instructions                  | description                                   |
| ----------------------------- | --------------------------------------------- |
| `putchar(n);`                 | Affiche le caractère dont le code ascii est n |
| `x = v;`                      | Assigne la valeur v à la variable x           |
| `type x = v;`                 | Déclaration de variable                       |
| `if (cond) {...} else {...} ` | Branchement conditionnel                      |
| `while (cond) {...}`          | Boucle while                                  |
| `for (init; cond; incr) {...}`| Boucle for                                    |
| `return e;`                   | Renvoie la valeur de l'expression e           |
| `e;`                          | Évalue l'expression e                         |

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

### Afficheur

Il est possible de reconstruire un fichier source à partir d'un arbre de syntaxe à l'aide de la fonction `print_program : prog -> out_channel -> unit` du module `Libminic.Minic_display` qui traduit un ast en code source qui sera écrit dans une variable de type `out_channel`.

Pour reconstruire un programme on peut utiliser l'option `-disp` :
```
minic prog.mnc -disp
```
Sans argument le code source est envoyé sur la sortie standard, on peut également donner un fichier en argument :
```
minic prog.mnc -disp code.out
```
## Tests

Les tests du vérificateur de type, de l'interpréteur et de l'afficheur sont dans les sous dossiers typechecker, interpreter et display du dossier test.
- Le dossier test/typechecker contient un fichier de test par aspect du langage traité, par ordre chronologique
- Pour tester l'afficheur on vérifie qu'un programme de base et le même programme reconstruit à partir de son ast produise les mêmes résultats à l'interpretation
- Pour tester l'interpréteur on interprète plusieurs fichiers et on compare leur sortie et leur code de retour avec le contenu du fichier interpreter.expected

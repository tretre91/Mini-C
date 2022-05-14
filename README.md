# Mini-C

Projet de compilation d'un langage impératif type c vers WebAssembly.

---

- [Mini-C](#mini-c)
  - [Fonctionnalités](#fonctionnalits)
    - [Compilateur](#compilateur)
    - [Interpréteur](#interprteur)
    - [Afficheur](#afficheur)
  - [Syntaxe du langage](#syntaxe-du-langage)
    - [Commentaires](#commentaires)
    - [Types](#types)
    - [Variables](#variables)
    - [Fonctions](#fonctions)
    - [Blocs](#blocs)
    - [Instructions](#instructions)
    - [Expressions](#expressions)
  - [Tests](#tests)


## Fonctionnalités

### Compilateur

Utiliser la commande
```
minic prog.mnc
```
Pour compiler un programme, par défaut le programme compilé est envoyé sur la sortie standard, il peut être envoyé dans un fichier
en utilisant l'option `-o`
```
minic prog.mnc -o prog.wat
```

Le format de sortie est le [format textuel](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format) de WebAssembly, le fichier produit peut ensuite être compilé vers le format binaire en utilisant par exemple [`wat2wasm`](https://github.com/WebAssembly/wabt#wabt-the-webassembly-binary-toolkit).

### Interpréteur

Un programme Mini-C peut être interprété en utilisant la fonction `interpret_program : prog -> int` du module `Libminic.Minic_interpreter`. La fonction d'interprétation exécute la fonction main du programme si elle existe et renvoie sa valeur de retour.

Pour interpréter un programme utiliser l'option `-i` :
```
minic prog.mnc -i
```

### Afficheur

Il est possible de reconstruire un fichier source à partir d'un arbre de syntaxe à l'aide de la fonction `print_program : prog -> out_channel -> unit` du module `Libminic.Minic_display` qui traduit un ast en code source qui sera écrit dans une variable de type `out_channel`.

Pour reconstruire un programme on peut utiliser l'option `--display` :
```
minic prog.mnc --display
```
Sans argument le code source est envoyé sur la sortie standard, on peut également donner un fichier en argument :
```
minic prog.mnc --display -o code.out
```

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

| expression                    | interpréteur | compilateur | exemple                   |
| ----------------------------- | :----------: | :---------: | ------------------------- |
| **Littéraux**                 |              |             |                           |
| constante entière             | X            | X           | `-5`                      |
| constante booléenne           | X            |             | `true`, `false`           |
| **Opérations arithmétiques**  |              |             |                           |
| addition                      | X            | X           | `1 + -5`                  |
| soustraction                  | X            |             | `1 - 5`                   |
| multiplication                | X            | X           | `(1 + 1) * 2`             |
| division                      | X            |             | `10 / 2`                  |
| modulo                        | X            |             | `18 % 3`                  |
| **Opérations de comparaison** |              |             |                           |
| égalité                       | X            |             | `1 == 1`, `true != false` |
| inégalité stricte             | X            |             | `1 < 2`, `2 > 1`          |
| inégalité large               | X            | ~           | `1 <= 1`, `3 <= 4`        |
| **Opérations logiques**       |              |             |                           |
| négation                      | X            |             | `!true`                   |
| et                            | X            |             | `3 < 4 && true`           |
| ou                            | X            |             | `false \|\| 1 == 2`       |
| **Opérations bit-à-bit**      |              |             |                           |
| négation                      | X            |             | `~1`                      |
| et                            | X            |             | `1 + 2 & 3`               |
| ou                            | X            |             | `64 \| 63`                |
| ou exclusif                   | X            |             | `127 ^ 255`               |
| **Décalages**                 |              |             |                           |
| vers la gauche (logique)      | X            |             | `1 << 64`                 |
| vers la droite (arithmétique) | X            |             | `64 >> 2`                 |
| **Autres**                    |              |             |                           |
| variable                      | X            | X           | `x`                       |
| appel de fonction             | X            | X           | `foo(5, false)`           |

## Tests

Les tests du vérificateur de type, de l'interpréteur et de l'afficheur sont dans les sous dossiers typechecker, interpreter et display du dossier test.
- Le dossier test/typechecker contient un fichier de test par aspect du langage traité, par ordre chronologique
- Pour tester l'afficheur on vérifie qu'un programme de base et le même programme reconstruit à partir de son ast produise les mêmes résultats à l'interpretation
- Pour tester l'interpréteur on interprète plusieurs fichiers et on compare leur sortie et leur code de retour avec le contenu du fichier interpreter.expected

Les tests de l'afficheur et de l'interpréteur sont lancés lorsque le projet est compilé, ils n'affichent rien si il  n'y a pas d'erreur.

Les tests du vérificateur de type peuvent être lancés avec la commande

```
dune runtest
```

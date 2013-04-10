TEMA 1 PP 
AVRAM Marius

Pentru partea de recursivitate am parcurs corpul functiei in totalitate (atat pana la adancimea cea mai mare cat si in latime). Iar atunci cand se gasea numele functiei in lista exterioara ce contina numele functiei trebuie sa fie neaparat un 'if' pentru recursivitate pe coada si orice altceva pentru recursivitate pe stiva. In cazul lui cond puteam avea din nou cazul cu recursivitate pe coada. Daca se intalnesc ambele tipuri de recursivitate intr-o functie (si pe stiva si pe coada) am considerat ca cea pe stiva are prioritate - conform testerului. 

Pentru a avea recursivitate pe arbore am numarat numarul de aparitii al numelui functiei in corpul acesteia si am pus conditia sa nu se intalneasca recursivitate pe coada impreuna cu cea pe stiva.

La partea a doua am verificat mai intai daca o secventa dintr-o functie se potrivea cu un pattern dat. Daca se potrivea treceam simbolurile intr-o lista de variabile/simboluri legate (o lista de perechi). Dupa construirea intregii liste de simboluri aplicam patternul de output in care inlocuiam simbolurile din pattern cu cele din context - legate anterior. Pentru variabile conditionate am evaluat cu eval tipul simbolui care trebuia legat. Iar la evaluare am folosit din nou eval pentru o secventa legata. 

Partea de abstractizare a ramas neimplementata.

Mai multe detalii despre implementarea temei se gasesc in comentariile din codul sursa.

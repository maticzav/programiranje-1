from functools import lru_cache

# =============================================================================
# Psička Nara po njivi preganja krokarje. Opazila je, da jo lastnik čaka na
# drugem koncu polja, zato hiti k njemu, pri tem pa hoče prestrašiti kar se da
# veliko ubogih ptičev.
#
# Njivo predstavimo kot matriko, ki v vsakem polju vsebuje število krokarjev,
# ki jih pasja navihanka prežene, če teče preko tega polja.
# =============================================================================

primer = [
    [2, 3, 0, 2, 9],
    [8, 3, 5, 1, 2],
    [1, 2, 7, 2, 0],
    [4, 3, 6, 5, 10],
]

# 5 desno spodaj si dal v 10

# (a)
# =============================================================================
# Nara se nahaja v zgornjem levem kotu njive (polje `(0, 0)`). Ker se ji mudi
# k lastniku, se vztrajno premika desno. Na vsakem koraku se lahko premakne:
#   - desno
#   - diagonalno desno-gor
#   - diagonalno desno-dol
#
# Pregon krokarjev zaključi na poljubnem skrajno desnem polju njive. Napišite
# funkcijo, ki izračuna največje število krokarjev, ki jih lahko nagajivka
# prežene.
# =============================================================================

def krokarji(njiva):
    stolpci = len(njiva[0])
    vrstice = len(njiva)

    @lru_cache(maxsize=None)
    def aux(x, y):
        # Ko pride Nara v katero od smeri čez njivo, vemo da
        # nemore prestrašit več nobenega krokarja.
        if x >= stolpci or y >= vrstice or y < 0 or x < 0:
            return 0

        # Drugače, pogledamo koliko ptičev lahko prestraši
        # v poljih, kot se lahko premika.
        najvec = max(
            aux(x + 1, y - 1),
            aux(x + 1, y),
            aux(x + 1, y + 1)
        )

        return njiva[y][x] + najvec
    
    return aux(0, 0)


print(krokarji(primer))


# (b)
# =============================================================================
# Funkcijo iz točke (a) prilagodite tako, da ji dodatno podate indeks vrstice,
# v kateri Nara začne, in indeks vrstice, v kateri Nara konča.
#
# Funkcija naj vrne seznam VSEH optimalnih poti, kjer pot predstavimo s
# seznamom indeksov polj, preko katerih Nara teče.
# =============================================================================

def ptici(njiva, zacetek, konec):
    stolpci = len(njiva[0])
    vrstice = len(njiva)

    def aux(x, y):
        # Če pristane Nara na napačnem končnem polju odštejemo minus neskončno,
        # da sigurno nebi prišla ta rešitev kot prava saj se neskončno ne 
        # spremeni tudi če prištejemo poljubno število.
        if x == stolpci - 1: 
            if y != konec:
                return float("-infinity"), [[y]]
            else:
                return njiva[y][x], [[y]]

        # Ko pride Nara v katero od smeri čez njivo, vemo da
        # nemore prestrašit več nobenega krokarja.
        if y >= vrstice or y < 0:
            return 0, []

        # Drugače, pogledamo koliko ptičev lahko prestraši
        # v poljih, kot se lahko premika.
        dgor = aux(x + 1, y - 1)
        d = aux(x + 1, y)
        ddol = aux(x + 1, y + 1)

        # Optimalna dolzina
        optimalna_dolzina = max(dgor[0], d[0], ddol[0])

        poti = []
        for dolzina, pod_poti in [dgor, d, ddol]:
            if dolzina != optimalna_dolzina:
                continue

            for pod_pot in pod_poti:
                poti.append([y] + pod_pot)

        return njiva[y][x] + optimalna_dolzina, poti
    
    return aux(0, zacetek)

# Preveriš lahko tako, da spremeniš zgornjega zadnjega iz 9 v 10.
print(ptici(primer, 0, 0))
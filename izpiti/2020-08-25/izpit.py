import random

def soda(n):
    # Začnemo s permutacijo (123...n)
    perm = list(range(1, n + 1))

    # Glede na to ali je n sod ali lih izračunamo ali
    # je začetna permutacija soda. To bo natanko tedaj,
    # ko je n lih.
    soda = n % 2 == 1

    # Pomikamo se po seznamu od konca proti začetku in mešamo člene.
    # V jeziku algebre delamo transpoizicije (ab). Če člena a in b nista
    # enaka je transpozicija liha permutacija, drugače pa je soda (identiteta).
    # Kot vemo iz pravil za komponiranje lahko na vsakem koraku povemo
    # ali je trenutna permutacija soda ali liha.

    for i in range(n - 1, 0, -1):
        polje = random.randint(0, i)

        # Če smo na zadnjem koraku in je permutacija soda,
        # ne naredimo še ene transpozicije do lihe.
        if i == 1 and soda:
            continue

        # Če polje ni enako naredimo transpozicijo.
        if polje != i:
            perm[i], perm[polje] = perm[polje], perm[i]
            soda = not soda
        


    # Komenitarjo še verjetnost posamezne permutacije.
    #
    # Ker algoritem sloni na Fischer-Yatesovem algoritmu za katerega
    # smo na predavanjih dokazali, da ima vsaka permutacija verjetnost 1/n!,
    # da se pojavi je treba komentirat le zadnji del.
    #
    # Ker pri zadnji transpoziciji pogledamo ali je permutacija soda lahko
    # pridemo po dveh poteh do iste permutacije zato verjetnost množimo z 2.

    return perm
from functools import lru_cache

def f(k, n):

    @lru_cache(maxsize=None)
    def aux(a, n):
        # Na koncu imamo samo eno možnost. Končamo.
        if n == 0:
            return 1
        
        # Naredimo lahko vse možnosti od spodnje do zgornje meje,
        # pri tem pa je treba pazit, da ne gremo pod 0.
        zgornja_meja = a + k
        spodnja_meja = max(a - k, 0)

        # moznosti = zgornja_meja - spodnja_meja

        st_moznosti = 0
        for i in range(spodnja_meja, zgornja_meja):
            st_moznosti += aux(i, n - 1)
        
        # V vsakem koraku pogledamo koliko možnosti imamo, nato pa 
        return st_moznosti
    
    return aux(0, n)
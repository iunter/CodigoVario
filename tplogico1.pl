
sonHermanos(Hermano1, Hermano2) :- 
	esAlguienPadreDe(Padre, Hermano1),
	esAlguienPadreDe(Padre, Hermano2),
	Hermano1 \= Hermano2.

esTio(Sobrino, Tio) :- 
	esAlguienPadreDe(Padre, Sobrino),
	sonHermanos(Tio, Padre).

esNerd(Alguien) :-
	not(esDeportista(Alguien)).

esAlguienPadreDe(homero,bart).
esAlguienPadreDe(homero, maggie).
esAlguienPadreDe(homero, lisa).
esAlguienPadreDe(marge, bart).
esAlguienPadreDe(marge, maggie).
esAlguienPadreDe(marge, lisa).
esAlguienPadreDe(ned, rod).
esAlguienPadreDe(ned, tod).
esAlguienPadreDe(abe, homero).
esAlguienPadreDe(abe, herbert).
esAlguienPadreDe(jose, carlos).

esAlguienQuilombero(bart).
esAlguienQuilombero(nelson).

esDeportista(rod).
esDeportista(tod).
esDeportista(nelson).
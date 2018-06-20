odiaA(tiaAgatha, Persona) :- 
	vivenEnMansion(Persona),
	Persona \= carnicero,
	Persona \= tiaAgatha.

odiaA(charles, Persona) :-
	vivenEnMansion(Persona),
	not(odiaA(tiaAgatha, Persona)).

odiaA(carnicero, Persona) :-
	odiaA(tiaAgatha, Persona).

esMasRicoQueAgatha(Persona) :-
	not(odiaA(carnicero, Persona)),
	vivenEnMansion(Persona).

quienMatoAgatha(Persona) :-
	odiaA(Persona, tiaAgatha),
	not(esMasRicoQueAgatha(Persona)).

alguienOdiaAMilhouse(Persona) :-
	odiaA(Persona, milhouse).

vivenEnMansion(tiaAgatha).
vivenEnMansion(carnicero).
vivenEnMansion(charles).

/*
	1) 	quienMatoAgatha(Quien).
		Quien = charles ;
	2)
		a) 	alguienOdiaAMilhouse(_).
			false.

		b)	odiaA(charles, Quien).
			Quien = tiaAgatha ;
			Quien = carnicero ;

		c)	odiaA(Quien, tiaAgatha).
			Quien = charles ;

		d)	odiaA(Odiador, Odiado).
			Odiador = tiaAgatha,
			Odiado = charles ;
			Odiador = charles,
			Odiado = tiaAgatha ;
			Odiador = charles,
			Odiado = carnicero ;
			Odiador = carnicero,
			Odiado = charles.

		e)	odiaA(carnicero,_).
			true.

*/
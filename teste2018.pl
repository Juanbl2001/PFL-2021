airport('Aeroporto Francisco Sá Carneiro', 'LPPR', 'Portugal').
airport('Aeroporto Humberto Delgado', 'LPPT', 'Portugal').
airport('Aeropuerto Adolfo Suárez Madrid-Barajas', 'LEMD', 'Spain').
airport('Aéroport de Paris-Charles-de-Gaulle Roissy Airport', 'LFPG', 'France').
airport('Aeroporto Internazionale di Roma-Fiumicino - Leonardo da Vinci', 'LIRF', 'Italy').

% company (ICAO, Name, Year, Country)

company('TAP', 'TAP Air Portugal', 1945, 'Portugal').
company('RYR', 'Ryanair', 1984, 'Ireland').
company('AFR', 'Société Air France, S.A.', 1933, 'France').
company('BAW', 'British Airways', 1974, 'United Kingdom').

% flight (Designation, Origin, Destination, DepartureTime, Duration, Company)

flight('TP1923', 'LPPR', 'LPPT', 1115, 55, 'TAP').
flight('TP1968', 'LPPT', 'LPPR', 2235, 55, 'TAP').
flight('TP842', 'LPPT', 'LIRF', 1450, 195, 'TAP').
flight('TP843', 'LIRF', 'LPPT', 1935, 195, 'TAP').
flight('FR5483', 'LPPR', 'LEMD', 630, 105, 'RYR').
flight('FR5484', 'LEMD', 'LPPR', 1935, 105, 'RYR').
flight('AF1024', 'LFPG', 'LPPT', 940, 155, 'AFR').
flight('AF1025', 'LPPT', 'LFPG', 1310, 155, 'AFR').

short(F):-
    flight(F,_,_,_,T,_),
    T<90.

getShorter(Flight1, _, Time1, Time2, Flight1) :- Time1 < Time2.
getShorter(_, Flight2, Time1, Time2, Flight2) :- Time1 > Time2.
getShorter(_, _, _, _, _) :- fail.


shorter(Flight1, Flight2, ShorterFlight) :- flight(Flight1, _, _, _, Time1, _),
                                            flight(Flight2, _, _, _, Time2, _),
                                            getShorter(Flight1, Flight2, Time1, Time2, ShorterFlight).
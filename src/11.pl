:- use_module(utils).

sol1(9608724).
sol2(904633799472).

distance(N, X-Y, EmptyLines, EmptyColumns, U-V, D) :-
    findall(E, (member(E, EmptyLines), (E in X..U ; E in U..X)), EL),
    length(EL, Ls),
    findall(E, (member(E, EmptyColumns), (E in Y..V ; E in V..Y)), EC),
    length(EC, Cs),
    D #= abs(U - X) + abs(V - Y) + Ls * (N - 1) + Cs * (N - 1).

solve(_, [], _, _, 0).
solve(N, [X-Y|MoreGalaxies], EmptyLines, EmptyColumns, Sol) :-
    maplist(distance(N, X-Y, EmptyLines, EmptyColumns), MoreGalaxies, Distances),
    sum_list(Distances, Sol1),
    solve(N, MoreGalaxies, EmptyLines, EmptyColumns, Next),
    Sol #= Sol1 + Next.

part1(F, Sol) :- run(2, F, Sol).
part2(F, Sol) :- run(1000000, F, Sol).

run(N, F, Sol) :-
    phrase_from_file(lines(Mat), F),
    findall(X, (nth0(X, Mat, Z), maplist(=('.'), Z)), EmptyLines),
    transpose(Mat, Mat1),
    findall(X, (nth0(X, Mat1, Z), maplist(=('.'), Z)), EmptyColumns),
    findall(X-Y, mat_at(X-Y, Mat, '#'), Galaxies),
    solve(N, Galaxies, EmptyLines, EmptyColumns, Sol).

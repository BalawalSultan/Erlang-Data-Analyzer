-module(summarizer).
-export([waitForMessage/5, isThere/2]).

% The Summarizer needs the Genders, MaritalStatuses, Ages, Incomes, Occupations Lists to be initialized
% Once it is initialized it waits for messages and acts accordingly

waitForMessage(Genders, MaritalStatuses, Ages, Incomes, Occupations) ->
    receive
        {age,X} ->
            NewAges = increment(Ages,X),
            waitForMessage(Genders, MaritalStatuses, NewAges, Incomes, Occupations);

        {maritalStatus,X} ->
            NewMarStat = increment(MaritalStatuses,X),
            waitForMessage(Genders, NewMarStat, Ages, Incomes, Occupations);

        {occupation,X} ->
            NewOccupations = increment(Occupations,X),
            waitForMessage(Genders, MaritalStatuses, Ages, Incomes, NewOccupations);

        {gender,X} ->
            NewGenders = increment(Genders,X),
            waitForMessage(NewGenders, MaritalStatuses, Ages, Incomes, Occupations);

        {income,X} ->
            NewIncomes = increment(Incomes,X),
            waitForMessage(Genders, MaritalStatuses, Ages, NewIncomes, Occupations);

        completed ->
            io:format("~n"),
            io:format("Gender:~n"),
            print(Genders),
            io:format("~n"),
            io:format("Marital status:~n"),
            print(MaritalStatuses),
            io:format("~n"),
            io:format("Occupation:~n"),
            print(Occupations),
            io:format("~n"),
            io:format("Age:~n"),
            print(Ages),
            io:format("~n"),
            io:format("Income:~n"),
            print(Incomes),
            io:format("~n");

        _ -> waitForMessage(Genders, MaritalStatuses, Ages, Incomes, Occupations)
    end.
        
increment(Touples,X) ->
    IsThere = isThere(Touples,X),
    if IsThere -> lists:map(fun(Touple) -> updateIfCorrect(Touple, X) end, Touples);
    true -> [{X,1}] ++ Touples
end.

isThere(List,X) -> lists:any(fun({K,_}) -> K == X end, List).

updateCounter({Name,Counter}) -> {Name,Counter + 1}.

updateIfCorrect({Name, N}, X) -> 
    if Name == X -> updateCounter({Name, N});
    true -> {Name, N}
end.

print(List) -> lists:foreach(fun({Name,Count}) -> io:format("~p: ~p~n",[Name,Count]) end, List).
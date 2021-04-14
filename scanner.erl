-module(scanner).
-export([scan/1]).

% The scanner process needs the Pid of the Summarizer to be initialized
% Once initialized it waits for messages and acts accordingly to the message received
% When the scanner receives the eof (end of file) message it returns the Pid of the Summarizer and dies

scan(Summarizer) ->
    receive 
        eof -> 
            io:format("closing scanner: ~p~n",[self()]),
            exit(Summarizer);

        Line -> 
            Tokens = string:tokens(Line, " "),
            lists:foreach(fun(Token) -> manageToken(Summarizer, Token) end, Tokens),
            scan(Summarizer)
    end.

manageToken(Pid,Token) ->
    case Token of
        "Male" -> Pid ! {gender, Token};
        "Female" -> Pid ! {gender, Token};
        "Other" -> Pid ! {gender, Token};
        "Married" -> Pid ! {maritalStatus, Token};
        "Single" -> Pid ! {maritalStatus, Token};
        "Divorced" -> Pid ! {maritalStatus, Token};
        "Widowed" -> Pid ! {maritalStatus, Token};
        _ -> manageAdvancedToken(Pid, Token)
    end.

manageAdvancedToken(Pid, Token) ->
    FirstChar = lists:sublist(Token,1,1),
    {Value,_} = string:to_integer(Token),
    if 
        erlang:is_integer(Value) -> selectAge(Pid, Value);

        FirstChar == "$" -> 
            N = string:length(Token),
            String = lists:sublist(Token,2,N),
            {Income, _} = string:to_integer(String),
            selectYearlyIncome(Pid, Income);

        true -> Pid ! {occupation, Token}
    end.

selectAge(Pid,Age) when (Age >= 0) and (Age =< 6) -> Pid ! {age,"0-6"};
selectAge(Pid,Age) when (Age >= 7) and (Age =< 12) -> Pid ! {age,"7-12"};
selectAge(Pid,Age) when (Age >= 13) and (Age =< 18) -> Pid ! {age,"13-18"};
selectAge(Pid,Age) when (Age >= 19) and (Age =< 24) -> Pid ! {age,"19-24"};
selectAge(Pid,Age) when (Age >= 25) and (Age =< 30) -> Pid ! {age,"25-30"};
selectAge(Pid,Age) when (Age >= 31) and (Age =< 45) -> Pid ! {age,"31-45"};
selectAge(Pid,Age) when (Age >= 46) and (Age =< 60) -> Pid ! {age,"46-60"};
selectAge(Pid,Age) when (Age >= 61) -> Pid ! {age,"61+"}.

selectYearlyIncome(Pid,Income) when (Income < 10000) -> Pid ! {income,"<$10,000"};
selectYearlyIncome(Pid,Income) when (Income >= 10000) and (Income =< 25000) -> Pid ! {income,"$10,000 - $25,000"};
selectYearlyIncome(Pid,Income) when (Income >= 25001) and (Income =< 50000) -> Pid ! {income,"$25,001 - $50,000"};
selectYearlyIncome(Pid,Income) when (Income >= 50001) and (Income =< 100000) -> Pid ! {income,"$50,001 - $100,000"};
selectYearlyIncome(Pid,Income) when (Income >= 100001) and (Income =< 250000) -> Pid ! {income,"$100,001 - $250,000"};
selectYearlyIncome(Pid,Income) when (Income > 250000) -> Pid ! {income,">$250,000"}.
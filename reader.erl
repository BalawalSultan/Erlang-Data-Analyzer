-module(reader).
-export([word_count/2]).

% In order to run the program you must execute the following commands in erlang 
% 1) c(reader).
% 2) c(scanner).
% 3) c(summarizer).
% 4) MyReaderProcess = spawn(reader, word_count, ["my_data_file.txt", NrOfScanners]).
% 5) MyReaderProcess ! start.

word_count(FileName, N) ->
    process_flag(trap_exit,true),
    receive 
        start ->
            Summarizer = initializeSummarizer(),
            Scanners = createScanners(N,Summarizer,[]),
            Lines = readFile(FileName),
            dispatchLineToScanner(Lines,Scanners,N,1),
            word_count(FileName,N);

        {'EXIT',_,Summarizer} -> if
            N == 1 -> 
                Summarizer ! completed,
                closeFile(FileName);

            true -> 
                word_count(FileName,N-1)
        end
    end.

initializeSummarizer() -> spawn(
    summarizer,
    waitForMessage,
    [
        initializeGenders(),
        initializeMaritalStaus(),
        initializeAges(),
        initializeIncomes(),
        []
    ]
).

createScanners(0,_,Scanners) -> Scanners;
createScanners(N,Summarizer,Scanners) ->
    Scanner = spawn_link(scanner, scan,[Summarizer]),
    createScanners(N-1,Summarizer,[Scanner|Scanners]).

dispatchLineToScanner([],Scanners,_,_) -> lists:foreach(fun(Scanner) -> Scanner ! eof end, Scanners);
dispatchLineToScanner([H|T],Scanners,N,ScannerNr) ->
    if
        ScannerNr =< N ->
            lists:nth(ScannerNr, Scanners) ! binary:bin_to_list(H),
            dispatchLineToScanner(T,Scanners,N,ScannerNr+1);
        ScannerNr > N ->
            lists:nth(1,Scanners) ! binary:bin_to_list(H),
            dispatchLineToScanner(T,Scanners,N,2)
    end.

readFile(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

closeFile(FileName) -> file:close(FileName).

initializeAges() -> [
    {"0-6",0},
    {"7-12",0},
    {"13-18",0},
    {"19-24",0},
    {"25-30",0},
    {"31-45",0},
    {"46-60",0},
    {"61+",0}
].

initializeGenders() -> [
    {"Male",0},
    {"Female",0},
    {"Other",0}
].

initializeMaritalStaus() -> [
    {"Single",0},
    {"Married",0},
    {"Divorced",0},
    {"Widowed",0}
].

initializeIncomes() -> [
    {"<$10,000",0},
    {"$10,000 - $25,000",0},
    {"$25,001 - $50,000",0},
    {"$50,001 - $100,000",0},
    {"$100,001 - $250,000",0},
    {">$250,000",0}
].
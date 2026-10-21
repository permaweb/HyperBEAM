%%% The parser for singleton paths and hashpaths, which reads the tokens of
%%% `hb_singleton_lexer'. The same grammar is given in EBNF in the module doc
%%% of `hb_singleton'.
%%%
%%% `hb_singleton' gives the parser the tokens of a path without its query,
%%% and builds messages from the parts that the parser returns. The parser
%%% keeps a group as its list of tokens: if the group is part of a key, a
%%% value or a device, its text is used, and if it is a subpath, its tokens
%%% are parsed again as a path.

Nonterminals
path elems elem part utok group gtoks gtok step head key ktok ktok0
vtoks vtok dtoks mods item ikey iktok claims equivalence.

Terminals
'/' '?' '&' '=' '~' '(' ')' '+' ' ' '.' '@' '*' ';' ':'
text quoted scheme varied dependencies result extension.

Rootsymbol path.

path -> elems : path_parts('$1').

elems -> elem : ['$1'].
elems -> elems '/' elem : '$1' ++ ['$3'].

elem -> '$empty' : empty.
elem -> part : '$1'.

part -> group : {subpath, '$1'}.
part -> step : {step, '$1', []}.
part -> step claims : {step, '$1', '$2'}.

utok -> text : '$1'.
utok -> quoted : '$1'.
utok -> scheme : '$1'.
utok -> group : '$1'.
utok -> varied : '$1'.
utok -> dependencies : '$1'.
utok -> result : '$1'.
utok -> extension : '$1'.
utok -> '&' : '$1'.
utok -> '=' : '$1'.
utok -> '~' : '$1'.
utok -> '+' : '$1'.
utok -> ' ' : '$1'.
utok -> '.' : '$1'.
utok -> '@' : '$1'.
utok -> '*' : '$1'.
utok -> ';' : '$1'.
utok -> ':' : '$1'.

group -> '(' gtoks ')' : {group, element(2, '$1'), '$2'}.

gtoks -> '$empty' : [].
gtoks -> gtoks gtok : '$1' ++ ['$2'].

gtok -> utok : '$1'.
gtok -> '/' : '$1'.
gtok -> '?' : '$1'.

step -> key : {key, '$1'}.
step -> head '=' vtoks : {value, '$1', '$3', []}.
step -> head '=' vtoks mods : {value, '$1', '$3', '$4'}.
step -> head '~' dtoks : {device, '$1', '$3', []}.
step -> head '~' dtoks mods : {device, '$1', '$3', '$4'}.
step -> '~' dtoks : {device, [], '$2', []}.
step -> '~' dtoks mods : {device, [], '$2', '$3'}.
step -> head mods : {mods, '$1', '$2'}.
step -> mods : {mods, [], '$1'}.

head -> key : '$1'.
head -> group : ['$1'].

key -> ktok0 : ['$1'].
key -> group ktok : ['$1', '$2'].
key -> key ktok : '$1' ++ ['$2'].

ktok0 -> text : '$1'.
ktok0 -> quoted : '$1'.
ktok0 -> scheme : '$1'.
ktok0 -> '+' : '$1'.
ktok0 -> ' ' : '$1'.
ktok0 -> '.' : '$1'.
ktok0 -> '@' : '$1'.
ktok0 -> '*' : '$1'.
ktok0 -> ';' : '$1'.
ktok0 -> ':' : '$1'.

ktok -> ktok0 : '$1'.
ktok -> group : '$1'.

vtoks -> '$empty' : [].
vtoks -> vtoks vtok : '$1' ++ ['$2'].

vtok -> ktok : '$1'.
vtok -> '=' : '$1'.
vtok -> '~' : '$1'.

dtoks -> vtok : ['$1'].
dtoks -> dtoks vtok : '$1' ++ ['$2'].

mods -> '&' item : ['$2'].
mods -> mods '&' item : '$1' ++ ['$3'].

item -> '$empty' : empty.
item -> ikey : {flag, '$1'}.
item -> ikey '=' vtoks : {value, '$1', '$3'}.

ikey -> iktok : ['$1'].
ikey -> ikey iktok : '$1' ++ ['$2'].

iktok -> ktok : '$1'.
iktok -> '~' : '$1'.

claims -> varied : ['$1'].
claims -> varied dependencies : ['$1', '$2'].
claims -> varied dependencies equivalence : ['$1', '$2', '$3'].
claims -> varied equivalence : ['$1', '$2'].
claims -> dependencies : ['$1'].
claims -> dependencies equivalence : ['$1', '$2'].
claims -> equivalence : ['$1'].

equivalence -> result : '$1'.
equivalence -> extension : '$1'.

Erlang code.

%% A path given over HTTP starts with `/', so its first element is empty. An
%% empty path part after that `/' is kept as `empty': it is the carried
%% message. Every other empty element is dropped.
path_parts([empty, empty, Part | Rest]) when Part =/= empty ->
    [empty | path_parts([Part | Rest])];
path_parts(Elems) -> [ Part || Part <- Elems, Part =/= empty ].

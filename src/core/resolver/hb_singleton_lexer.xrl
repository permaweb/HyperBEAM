%%% The lexer for singleton paths and hashpaths, which gives its tokens to
%%% `hb_singleton_parser'. The grammar is described in the module doc of
%%% `hb_singleton', with the rules for where a claim or a URI starts, which
%%% these tokens apply.
%%%
%%% `hb_singleton' gives each byte of a path to the lexer as one character,
%%% and adds the code point 256 to mark the end of the path. The lexer reads a
%%% claim only when the character after it ends the part or starts the next
%%% claim, so the end marker lets it read a claim at the end of the path.
%%% Every byte of the path is in exactly one token, so the text of the path
%%% can be rebuilt from its tokens. The location of a token is
%%% `{Line, Column}'.

Definitions.

I = [A-Za-z0-9_\-]
S = [A-Za-z0-9+.\-]
O = [^/?&=~()"+\s.@*;:\x{100}]

Rules.

\"([^\"\\\x{100}]|\\[^\x{100}])*\" : {token, {quoted, TokenLoc, TokenChars}}.
\" : {token, {'"', TokenLoc}}.
[A-Za-z]{S}*: : {token, {scheme, TokenLoc, TokenChars}}.
uri\-[a-z][a-z0-9]*\-[a-z0-9\-]* : {token, {scheme, TokenLoc, TokenChars}}.
;{I}+\+{I}+[@.*/)?\x{100}] : claim(varied, TokenChars, TokenLoc).
@{I}+[.*/)?\x{100}] : claim(dependencies, TokenChars, TokenLoc).
\.{I}+[/)?\x{100}] : claim(result, TokenChars, TokenLoc).
\*{I}+[/)?\x{100}] : claim(extension, TokenChars, TokenLoc).
{O}+ : {token, {text, TokenLoc, TokenChars}}.
/ : {token, {'/', TokenLoc}}.
\? : {token, {'?', TokenLoc}}.
& : {token, {'&', TokenLoc}}.
= : {token, {'=', TokenLoc}}.
~ : {token, {'~', TokenLoc}}.
\( : {token, {'(', TokenLoc}}.
\) : {token, {')', TokenLoc}}.
\+ : {token, {'+', TokenLoc}}.
\s : {token, {' ', TokenLoc}}.
\. : {token, {'.', TokenLoc}}.
@ : {token, {'@', TokenLoc}}.
\* : {token, {'*', TokenLoc}}.
; : {token, {';', TokenLoc}}.
: : {token, {':', TokenLoc}}.
\x{100} : skip_token.

Erlang code.

%% Make the token of a claim from the characters that its rule matched: the
%% separator, one ID (or two IDs joined by `+' for a varied pair), and the
%% character after the claim, which is pushed back to be scanned again. If an
%% ID is not 43 characters long, the separator is given as a token of its
%% own, and the rest of the characters are scanned again.
claim(Kind, Chars, Loc) ->
    [Separator | Rest] = Chars,
    Text = lists:droplast(Chars),
    case {Kind, string:split(tl(Text), "+")} of
        {varied, [Base, Req]} when length(Base) == 43, length(Req) == 43 ->
            {token, {varied, Loc, {Base, Req, Text}}, [lists:last(Chars)]};
        {Kind, [ID]} when Kind =/= varied, length(ID) == 43 ->
            {token, {Kind, Loc, {ID, Text}}, [lists:last(Chars)]};
        _ ->
            {token, {separator(Separator), Loc}, Rest}
    end.

separator($;) -> ';';
separator($@) -> '@';
separator($.) -> '.';
separator($*) -> '*'.

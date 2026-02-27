# Contributing to HyperBEAM.

There are three basic rules for writing and merging PRs to HyperBEAM:
1. The PR must not introduce additional test failures, flakes, or
   removal/defeating of existing tests unless agreed by multiple maintainers.
2. Modifications to the 'kernel layer' must never be made where modifications to
   the 'application layer' would suffice.
3. Merged code must abide by the existing style in the repo. Just write and merge
   code that blends in. This rule sounds unimportant, but over time it is what makes
   the code maintainable and understandable by a larger set of developers.
   Spaghetti/mixed styles lowers comprehension, which in a security sensitive
   environment => bugs => lost value. No broken windows if we can help it.

# The HyperBEAM Style Guide.

**Rule one of style guide club:** _We do not talk about style guide club._

We are here to build a fully decentralized alternative to cyberspace as it 
is currently constructed. We are not interested in long conversations about
where to put commas or spaces.

**Rule two of style guide club:** _Blend in._

Rule one does not imply that we do not care about the quality of the codebase.
Far from it: We know that we will be maintaining this code for decades to come.
It is important that we are all aligned on style and patterns, but less important
what those styles and patterns actually are. Having `length(Contributors)` 
styles adds overhead to understanding the codebase, which over time hides bugs
and reduces maintainability, but each stylistic choice is largely an opinion
that -- despite strong feelings -- lacks criticality. Hence, rule two:
Only write and merge code that actually _blends in_.

Write your code as if you were the author of all of the existing code. If all 
of the other code is written in a certain style, then copy it. If the style
of the code in your PR would not _blend in_, then its style is objectively
in violation of `style guide club`'s rules.

In the event of disagreement, a simple rule should guide our decisions: What
does the majority of the LoC in the codebase already do? Do that. Then get 
back to hacking.

If you don't like something about the style, simply contribute. If others
disagree strongly, the existing style will be kept. If your contributions are
seen by others as reasonable and inline with the canon, then it will gradually 
become adopted as the standard in the codebase.

**This concludes the rules of style guide club.**

Remember: Cypherpunks write code!

# A Rough Guide to the HyperBEAM `canon`

You should pick up and continue the style of the codebase as you learn how it 
works. There is no real substitute for paying attention. There are, however, a 
few basic rules that are widely established and represent the core `canon` of the
codebase. As of time of {{`git blame`}}, there is highest consensus around the
following:

- Always use `-` over `_` in binary key names.
    - Why: In general we try to follow the HTTP semantics RFC 9110, so all keys
      should be HTTP-Header-Case. This is the style that has been used for Arweave
      data protocols since inception, so to avoid confusion we maintain it in
      HyperBEAM. 
    - Nuances:
        - One weirdness we inherit from HTTP-land is that headers are actually
          case-insensitive, despite the use of capitals in header descriptions,
          over-the-wire they are lower-case in HTTP/2+. AO-Core shoots for the
          same semantics for consistency.
        - In device key resolutions that have multiple words (for example:
          `i_like(Base, Req, Opts) -> {ok, <<"Turtles!">>}.`) you may be tempted
          to call `~device@1.0/i_like`. Don't. Instead call `/i-like`.
          `hb_ao_device` will normalize the keys and match for you.
        - `hb_opts` uses all atoms for its message keys. This is a mistake. It
          is nice to be able to lookup keys via atoms (normalizing as above) and
          we should maintain this, but under the surface the keys should be
          normal-form binaries. To avoid issues when this is translated, perform
          `Opts` lookups with only atoms, or use binaries of normal-form if you
          must.
- Try to keep lines to around 80 characters-ish. This is not a strict rule because
  sometimes an 81-85 character line would be very ugly and harder to follow if split.
  Use your judgement.
  - Why: Our objective is to keep the code readable. Monster lines, and machine-enforced
    strict styles, both butcher this. Human/LLM judgement can help here.
- Add a `%%% @doc` moduledoc to each new module you write, and comment every
  function you write with a `%% @doc Description` above it. Inline comments are
  prepended with a single `%`.
  - Why: This helps humans and LLMs grok your code in the future. It also surfaces
    useful information in tooltips etc upstream.
  - Nuance: I do not know why the Erlang style uses `%%%` for moduledocs, `%%` for
    functions, and `%` for inline comments, but it does. This can help with parsability
    for some tooling and the effort-cost is minimal, so we use it.
- Avoid 'waterfalls'-style statements, instead keeping every set of statements
  nested such that the start and end of the block are indented inline with each
  other.
  - Why: This uses slightly more lines, but makes deeply nested code much more
    readable and comprehensible.
  - Examples:
```erlang
    BadForm = lists:map(
        fun(X) ->
            X * lists:sum(lists:fold(
                fun(Y, Acc) ->
                    Y * Acc
                end,
                [1,2,3]
            ))
        end
    ),
    GoodForm = lists:map(
            fun(X) ->
                X *
                    lists:sum(
                        lists:fold(
                            fun(Y, Acc) ->
                                Y * Acc
                            end,
                            [1,2,3]
                        )
                    )
            end
        )
```
There are a few areas where there is no consensus on patterns or style yet:
- Expressing docs in the info/[0,1] call of devices. There are a few different
  styles in different devices in the codebase -- if you want to add info response
  'inline' docs try to pick one that already exists and see what works/doesn't.
  We will need to unify them at some point.
- `maybe ... end` vs nested `case` expressions. `maybe` seems useful and preferable
  in at least some cases, but bubbling the right error -- rather than just an error --
  the caller can sometimes be difficult due to the `else` pattern matching.
  Experimentation with patterns here would be good.
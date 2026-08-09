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

# The Current HyperBEAM `canon`: A Rough Guide.

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
- Try to keep lines to around 80 characters-_ish_. This is not a strict rule
  because sometimes an 81-85 character line would be very ugly and harder to
  follow if split. Use your judgement. Do not falsely assume that because there
  is no linter blocking you from committing nobody cares. We do. Please respect
  the freedom and responsibility to execute judgement by paying attention.
  - Why: Our objective is to keep the code readable. Monster lines and machine-enforced
    strict styles both butcher this. Human/LLM judgement can help here.
- Add a `%%% @doc` moduledoc to each new module you write, and you should
  comment _almost_ every function you write with a `%% @doc Description` above it.
  Inline comments are prepended with a single `%`.
  - Why: This helps humans and LLMs grok your code in the future. It also surfaces
    useful information in tooltips etc upstream.
  - Nuance: I do not know why the Erlang style uses `%%%` for moduledocs, `%%` for
    functions, and `%` for inline comments, but it does. This can help with parsability
    for some tooling and the effort-cost is minimal, so we use it.
  - Nuance: Not every function needs a paragraph, but every function needs a line,
    unless the name leaves _absolutely_ zero doubt about what it does. If a
    reader who has never seen the file before, has none of your working memory,
    nor your context of HyperBEAM cannot skip reading the body, write the comment.
- Comments describe what exists today. There is no such thing as 'previously' in
  a comment in the codebase. There simply is what exists today. It is not a
  history of how it arrived at its current point -- that is what the `git` history
  is for.
  - Why: A comment narrating a change is meaningful only to someone who knew the
    old state. To every future reader it is noise, and it rots. The code is the
    artifact for the present and the future; how we got there is a separate
    question for `git`'s history functionality.
  - Nuance: Words like 'previously', 'now', 'no longer', 'used to' and 'we changed'
    are a strong tell. Delete them and state today's facts, written for someone
    that does not share your present task, does not have your working memory, and
    does not _need_ to know how things used to work.
- We don't use newlines inside function definitions. If there is a new logical
  section, add a `% [Description of the section ahead]` instead.
  - Why: The comment tells you what the next few lines are for. A blank line only
    tells you that the author paused.
- Write informative and clear narrative commentary in your _commits_. Strive to
  help people and agents in the future understand what your patches did and why.
  - Why: Debugging is often aided by understanding the context that the code was
    written in. We do not write the history in comments because they are the wrong
    data structure: The `git log` is. Write all of your commit messages aimed at
    whoever is trying to understand your decisions in the future and runs
    `git log -S` in five years.
  - Nuances:
    - If fixing security concerns you may take license to be abstract in your
      commit message, so as not to inappropriately highlight the concern. If not,
      you should not.
    - The `git` history is the repository's artifact alongside the code.
      When building if confused by some code, liberally consult the git history to
      understand the origin story. Do not, of course, let the old reasoning affect
      your new work unnecessarily. Understand, but do not succumb to prior modes
      of thinking.
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
    BadForm2 = lists:map(fun(X) -> X * lists:sum(lists:fold(
                fun(Y, Acc) ->
                    Y * Acc
                end,
                [1,2,3] )) end
    ),
    BadForm3 =
        lists:map(fun(X) -> X * lists:sum(lists:fold(
                fun(Y, Acc) -> Y * Acc end, [1,2,3] )) end),
    GoodForm =
        lists:map(
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
- Where possible, prefer `maybe ... [else ...] end` patterns over deeply nested
  case statements. Use `true ?= Statement orelse {error|failure, Details}` or
  similar constructs to easily surface errors without disrupting the 
  flow and requiring further indentation.
  - Why: We often need to execute a large number of protocol rules sequentially,
    surfacing the _reason_ for failure upwards if a necessary property does not
    hold. Experience has shown that in Erlang without `maybe ... end` this
    pattern seems to have a particularly pathological form: Each rule becomes
    a nested `case` that pushes the starting position of every subsequent line
    2x4 characters deeper. By 5 rules, half of the usage line length is 
    gone. Perhaps worse: The matching failure case that contains the details to
    surface can end up tens of lines from the clause that caused it. The clause-
    indent matching flow our code takes means that you can at least line up the
    returned details with the expression that caused it visually, but it is still
    far worse than the `maybe ... end` approach.

## Anti-patterns
  
Some of most important patterns in the way HyperBEAM is maintained are in what
you _don't see_. The complete set of rules for how _not_ to program is inherently
not computable, but here are a few common patterns that we see leading to PRs
being bounced:

- **Do not:** Create 'out-of-band' `ets`, `persistent_term` or process dictionary
  caches without extreme thought and care. In ~99% of cases, this is not what you
  actually want to do.
    - Why: AO-Core itself is a cached computation model. What you are trying to
      achieve is almost certainly better written as a use of the existing cache/
      store schemes.
- **Do not:** Use logging frameworks (`logger` or `io:format`) raw outside of the
  `?event` system.
    - Why: Using `event`s instead means that you inherit the `hb_format:` pretty-
      printer improves print safety dramatically, as well as making the results
      much more readable to others. Failure to do so risks printing private keys
      to logs, etc. Additionally, the `event` system is designed such that your
      debugging/informational signals can be easily and systematically accessed
      inside the AO-Core compute model as well as visible in prometheus/Grafana
      integrations.
- **Do not:** Use `dev_` direct device calls to exported functions in tests.
    - Why: HyperBEAM implements AO-Core semantics. It is a runtime environment
      for an execution mechanism which is _not_ just invoking Erlang module
      functions. Tests that do not invoke the device properly through `hb_ao:`
      calls will miss the intricacies and nuances of the execution environment
      and return positive results for objectively broken code.
- **Do not:** duplicate case clauses to fudge uncertain input types. We see this
  particularly between `atom` and `binary` values in substandard PRs.
- **Do not:** Introduce scripts in other languages as 'tests', 'smoke[s]', or
  'acceptance gates'. In virtually every case none of these will be considered 
  for merging.
    - Why: Every developer and agent has a preference for their favorite language,
      runtime, etc. They may even have good arguments. The burden to install each,
      however, lands on the other engineers and contributors over time -- not the
      originator. As a consequence, we keep things simple in virtually every case:
      `EUnit` tests are the default and expectation, written directly in Erlang.
      We expect to find them at the bottom of modules, or in separate `_test_vectors`
      `.erl` files if they are significant in size/quantity.
- **Do not:** Carelessly use or refer to `messages` as if they are normal Erlang `maps`.
  Erlang has transparent compound data types, so their internals are visible by
  all clients and they are not necessarily labelled to aid pattern matching, etc.
  In HyperBEAM's case, our core data structure is `messages` but their primary
  representation is often in the form of `map`s.
    - Why: Failure to comprehend the difference will lead to severe and hard-to-debug
      issues when the `map` you assumed you would receive is in-fact a
      `{link, ..., ...}`, or a `map` containing such values. This can happen at
      any moment, in any environment, because _the runtime decides when to load data_.
      Additionally, carelessness around `message` and `map` differences can lead
      to direct overwrites of `message` keys, dangerously rendering their attached
      commitments invalid. Use `hb_message:uncommitted` to remove old commitments
      that may be invalidated by the values being added to the message. `hb_ao:set`
      may also provide the desired message functionality, depending upon the 
      intended overwrite mechanics.
- **Do not:** Embed message/transport encoding specifics inside devices.
    - Why: HyperBEAM devices operate upon AO-Core data. The HTTP layer -- wrapping
      the core `hb_ao` resolution paths handles the serialization/deserialization
      layer separately. Letting the appropriate parts of the system handle
      normalization allows your devices to be agnostic and to inherit support for
      _all_ of the different codec devices that the node supports without any
      additional effort. Devices are protocols for data transformation.
      Operating on generic AO-Core messages for your inputs and outputs allows
      your protocol to be applied to data regardless of its encoding specifics.
- **Do not:** Use 'mock' HTTP servers in tests where you can use another HyperBEAM
  `hb_http_server:` instance on a different port. The codebase is full of examples
  of `EUnit` tests that follow the pattern of spawning micro 'networks' of nodes
  and using them to demonstrate behavior. We even prefer hitting _real-world_
  service endpoints as part of our `EUnit` tests in almost every case above creating
  mock versions of services. The only current exceptions to this in the codebase
  are where utilizing the real service would be prohibitively slow for the tests
  (multiple minutes) or would require real-world payments to be made.
    - Why: Using mocks lowers the utility of a test dramatically. A mock is 
      generally quite unlikely to actually match the remote service's behavior.
      A critical code quality issue with agents at time of writing is that they
      produce abundant tests that _look_ like they are working, but do not actually
      map to any real-world behaviors. These are categorically worse than simply not
      having tests, because they create the perception of safety and trustworthiness
      without substance. Even worse, to determine that the test is vacuous a
      reader must parse it. In its extreme this is a DoS vector against progress.

Much more important than these particular patterns, however: If you are findings
that your code is frequently breaching these basic rules, you are likely not
building carefully enough to contribute to this repository.

_Code itself_ is almost free now. The bar is not _'it works'_. The bar is 'this
is a robust substrate that others will be able to understand, depend upon for
production deployments guarding many billions of USD-worth of value, and improve
in the future'. If you would like your PRs to land, build with this mentality.

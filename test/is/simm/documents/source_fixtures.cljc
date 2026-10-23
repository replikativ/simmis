(ns is.simm.documents.source-fixtures
  "Synthetic document corpus shared by codec/editor tests. Required semantics
   describe future acceptance gates, NOT capabilities the current parser claims.
   These fixtures contain no customer/company evidence or executable imports.")

(def cases
  [{:id :business-brief
    :required-semantics #{:headings :paragraphs :table :tasks :wikilinks :properties}
    :editable "Pilot proposal"
    :source (str "---\ntitle: Pilot proposal\nowner: \"[[People/Sam]]\"\n"
                 "amount: '100.00' # exact spelling is deliberate\ncustom: [a, b]\n---\n"
                 "# Pilot proposal\n\nA synthetic plan, not customer evidence.\n\n"
                 "| Option | Cost |\n| --- | ---: |\n| A | 100.00 |\n\n"
                 "- [ ] Discuss with [[People/Sam|Sam]].\n")}
   {:id :research-note
    :required-semantics #{:callouts :footnotes :math :block-references :quotes}
    :editable "Research question"
    :source (str "# Research question\n\nA claim with a citation.[^source]\n\n"
                 "> [!question] Uncertainty\n> Is the observation reproducible?\n\n"
                 "> Exact source quotation.\n\n^evidence-1\n\n"
                 "See [[Research#^evidence-1|evidence]]. $p(x)$ is not a measured result.\n\n"
                 "[^source]: Synthetic fixture source.\n")}
   {:id :academic-notebook
    :required-semantics #{:named-footnotes :inline-math :display-math :citations
                          :nested-sci-cells :literal-contexts :source-backed-inlines}
    :editable "Experiment notebook"
    :source (str "---\ntitle: Experiment notebook\nbibliography: references.bib\n"
                 "custom: 'keep exact spelling'\n---\n"
                 "# Experiment notebook 🦋\n\n"
                 "A claim with a named footnote.[^method]\n\n"
                 "Prior $p(x)$ and evidence $p(y \\mid x)$ inform a posterior.\n\n"
                 "$$\np(x \\mid y) = \\frac{p(y \\mid x)p(x)}{p(y)}\n$$\n\n"
                 "A page locator [@sample2026, p. 12; @replication2025].\n\n"
                 "- An experiment inside a list.\n\n"
                 "  ```clojure\n  ;; Explicit execution only, never on open/save.\n"
                 "  {:example \"$not-math$ [^not-a-note] [@not-a-citation]\"}\n  ```\n\n"
                 "Literal `$x$`, `[^method]`, and `[@sample2026]`; price $5.00.\n\n"
                 "[^method]: A synthetic method, not a fetched reference.\n"
                 "    A second paragraph in the same footnote.\n")}
   {:id :nested-structure
    :required-semantics #{:list-continuations :nested-lists :heading-outline}
    :editable "Working notes"
    :source (str "# Working notes\n\n- First paragraph inside a list item.\n\n"
                 "  A second paragraph in the SAME item.\n\n"
                 "  - A nested list, not automatically a Simmis child block.\n\n"
                 "## A section, not automatically a block parent\n\nIndependent paragraph.\n")}
   {:id :literal-reference-contexts
    :required-semantics #{:syntax-aware-references :escaped-literals :code-fences}
    :editable "Reference examples"
    :source (str "# Reference examples\n\nA real [[Target|alias]] reference.\n\n"
                 "An escaped \\[[Literal]] and `[[Inline code]]`.\n\n"
                 "```clojure\n\"[[Not a backlink]]\"\n```\n\n"
                 "<!-- [[Not a backlink either]] -->\n")}
   {:id :legacy-rich-block
    :required-semantics #{:opaque-html :semantic-grouping :legacy-block-identity}
    :editable "Legacy content"
    :source (str "# Legacy content\n\n"
                 "<div data-legacy-block=\"fixture-only\"><p>First paragraph.</p>"
                 "<p>Second paragraph.</p><ul><li>Internal list.</li></ul></div>\n\n"
                 "Preserve this payload; its child-block mapping is not defined yet.\n")}
   {:id :inert-programmable-content
    :required-semantics #{:widget-codec :attachment-references :inert-import}
    :editable "Model sketch"
    :source (str "# Model sketch 🦋\n\n![[attachments/chart.svg]]\n\n"
                 "```simmis-widget\n{:fixture true :program \"not evaluated\"}\n```\n\n"
                 "<unknown-element data-prop=\"keep\">opaque</unknown-element>\n") }])

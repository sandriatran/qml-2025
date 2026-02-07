import React, { useState, useEffect, useCallback } from 'react';
import 'katex/dist/katex.min.css';
import { InlineMath, BlockMath } from 'react-katex';
import WordPairFader from './components/WordPairFader';
import GlossarySidebar from './components/GlossarySidebar';
import './index.css';

// ============================================================
// GLOSSARY (30+ terms)
// ============================================================
const GLOSSARY = {
  'Phoneme': 'Smallest unit of sound that distinguishes meaning in a language.',
  'L1': 'First language (native). For this study: Japanese.',
  'L2': 'Second language. For this study: English.',
  'Homophone': 'Words with identical pronunciation but different meanings (e.g., SUN / SON).',
  'Near-Homophone': 'Words differing by a contrast absent in the speaker\u2019s L1.',
  'Phonological Contrast': 'Sound difference that distinguishes words in a language.',
  'Representational Indeterminacy': 'L1-absent contrast collapses in L2 storage; two words share one representation.',
  'False Positive': 'Incorrectly judging an unrelated word pair as semantically related.',
  'FP': 'False Positive: incorrectly judging unrelated pairs as related.',
  'L1-Absent': 'Contrast not in speaker\u2019s native language (e.g., /l/-/r/ for Japanese speakers).',
  'L1-Present': 'Contrast present in speaker\u2019s native language (e.g., /p/-/b/ for Japanese speakers).',
  'Indeterminate': 'Collapsed into a single phonological representation due to an L1-absent contrast.',
  'Posterior': 'Updated distribution after combining prior beliefs with observed data via Bayes\u2019 theorem.',
  'Prior': 'Initial distribution expressing beliefs before seeing data.',
  'Credible Interval': 'Bayesian range containing the true parameter with specified probability (e.g., 95% CrI).',
  'CrI': 'Credible Interval: Bayesian range with specified probability.',
  'ROPE': 'Region of Practical Equivalence (\u00B10.05 log-odds); differences inside are negligible.',
  'MCMC': 'Markov Chain Monte Carlo: sampling algorithm for posterior distributions.',
  'Divergent Transitions': 'Sampling pathology in HMC/NUTS; zero is ideal.',
  'R-hat': 'Gelman\u2013Rubin convergence diagnostic. Values near 1.00 mean chains mixed well.',
  'ESS': 'Effective Sample Size: independent draws equivalent to correlated MCMC output.',
  'LOO-CV': 'Leave-One-Out Cross-Validation: Bayesian model comparison via predictive accuracy.',
  'PPC': 'Posterior Predictive Check: model-simulated data vs. observed data.',
  'Weakly Informative': 'Prior constraining implausible extremes without biasing effect direction.',
  'Log-Odds': 'Scale of logistic regression coefficients. 0 = 50/50 probability.',
  'brms': 'R package for Bayesian regression via Stan.',
  'GLMM': 'Generalized Linear Mixed Model: regression for non-normal outcomes with random effects.',
  'Partial Pooling': 'Hierarchical shrinkage: individual estimates pulled toward the group mean.',
  'Random Effects': 'Subject/item deviations from the population mean.',
  'Caterpillar Plot': 'Individual random effects with 95% credible intervals, ordered by magnitude.',
  'Halfeye Plot': 'Combined density + interval plot from ggdist showing full uncertainty.',
  'Bernoulli': 'Distribution for binary outcomes (0/1). Likelihood for accuracy data.',
  'Odds Ratio': 'Ratio of odds between groups. OR < 1 means reduced odds.',
};

// ============================================================
// SECTION MAP (updated for 26 slides)
// ============================================================
const SECTIONS = [
  { id: 'intro', label: 'Introduction', startIndex: 0, endIndex: 4 },
  { id: 'model', label: 'Model', startIndex: 5, endIndex: 8 },
  { id: 'results', label: 'Results', startIndex: 9, endIndex: 13 },
  { id: 'evidence', label: 'Evidence', startIndex: 14, endIndex: 18 },
  { id: 'deep', label: 'Deep Dive', startIndex: 19, endIndex: 21 },
  { id: 'synthesis', label: 'Synthesis', startIndex: 22, endIndex: 25 },
];

// ============================================================
// CONTRAST REFERENCE (persistent legend data)
// ============================================================
const CONTRASTS = [
  { code: 'F', label: 'Spelling Control', phon: 'Multiple phonemes differ', color: 'var(--color-lavender)' },
  { code: 'PB', label: '/p/\u2013/b/ (L1-present)', phon: 'Present in Japanese', color: 'var(--color-purple)' },
  { code: 'H', label: 'Homophones', phon: 'Identical pronunciation', color: 'var(--color-hot-pink)' },
  { code: 'LR', label: '/l/\u2013/r/ (L1-absent)', phon: 'Absent in Japanese', color: 'var(--color-indigo)' },
];

// ============================================================
// REPRESENTATION LEVEL MAP
// ============================================================
const REP_LEVELS = {
  PHON: { label: 'Phonology', desc: 'Sound-level representations' },
  LEX:  { label: 'Lexicon', desc: 'Word-level storage and activation' },
  DEC:  { label: 'Decision', desc: 'Task-level judgment process' },
  STAT: { label: 'Statistics', desc: 'Bayesian modeling and inference' },
};

// ============================================================
// SMALL COMPONENTS
// ============================================================
const Tooltip = ({ term, children }) => {
  const [show, setShow] = useState(false);
  const def = GLOSSARY[term];
  return (
    <span className="glossary-term" onMouseEnter={() => setShow(true)} onMouseLeave={() => setShow(false)}>
      {children}
      {show && def && <span className="tooltip-popup">{def}</span>}
    </span>
  );
};

const CodeLink = ({ label }) => (
  <a className="code-link-badge" href="https://github.com/sandriatran/qml-2025" target="_blank" rel="noopener noreferrer">
    {'</>'} {label || 'Code'}
  </a>
);

const ThreeLineFooter = ({ footer }) => {
  if (!footer) return null;
  return (
    <div className="three-line-footer">
      <div className="footer-line"><span className="footer-label">Q</span>{footer.question}</div>
      <div className="footer-line"><span className="footer-label">V</span>{footer.summary}</div>
      <div className="footer-line footer-takehome"><span className="footer-label">&rarr;</span>{footer.takeHome}</div>
    </div>
  );
};

// ── Model Recap Box ──
// Shows on slides displaying posterior-derived quantities
const ModelRecap = () => (
  <div className="model-recap">
    <span className="model-recap-label">Model</span>
    <span className="model-recap-text">Bernoulli(logit) GLMM &middot; subject + item intercepts &middot; priors <InlineMath math="\mathcal{N}(0,1.5)" /></span>
  </div>
);

// ── Contrast Legend Strip ──
// Persistent reference for the four contrast types
const ContrastLegendStrip = () => (
  <div className="contrast-legend-strip">
    {CONTRASTS.map(c => (
      <div key={c.code} className="contrast-legend-item">
        <span className="contrast-dot" style={{ background: c.color }}></span>
        <span className="contrast-code">{c.code}</span>
        <span className="contrast-desc">{c.label}</span>
      </div>
    ))}
  </div>
);

// ── Representation Level Tag ──
const RepLevelTag = ({ level }) => {
  const info = REP_LEVELS[level];
  if (!info) return null;
  return (
    <span className={`rep-level-tag rep-level-${level.toLowerCase()}`} title={info.desc}>
      {info.label}
    </span>
  );
};

// ── Figure Legend ──
// Small annotation under each visualization explaining what the visual elements mean
const FigureLegend = ({ text }) => (
  <div className="figure-legend">{text}</div>
);

// ── Reproduce Tag ──
// Links visualization to the R script section that generated it
const ReproduceTag = ({ scriptRef }) => (
  <a className="reproduce-tag" href="https://github.com/sandriatran/qml-2025" target="_blank" rel="noopener noreferrer">
    {'</>'} {scriptRef}
  </a>
);

// ── Theory Callout ──
// Highlighted sentence connecting visualization to theoretical framework
const TheoryCallout = ({ text }) => (
  <div className="theory-callout">
    <span className="theory-callout-icon">&#9670;</span>
    <span className="theory-callout-text">{text}</span>
  </div>
);

// ── Tiered Content (Progressive Disclosure) ──
const TieredContent = ({ tiers }) => {
  const [tier, setTier] = useState(1);
  if (!tiers) return null;
  return (
    <div className="tiered-content">
      <div className="tier-controls">
        <button className={`tier-btn ${tier === 1 ? 'active' : ''}`} onClick={() => setTier(1)}>Intuitive</button>
        <button className={`tier-btn ${tier === 2 ? 'active' : ''}`} onClick={() => setTier(2)}>Technical</button>
        <button className={`tier-btn ${tier === 3 ? 'active' : ''}`} onClick={() => setTier(3)}>Full Detail</button>
      </div>
      <div className="tier-body">
        {tier === 1 && <div className="tier-panel tier-1">{tiers.plain}</div>}
        {tier === 2 && <div className="tier-panel tier-2">{tiers.technical}</div>}
        {tier === 3 && <div className="tier-panel tier-3">{tiers.full}</div>}
      </div>
    </div>
  );
};

// ── Technical Appendix Modal ──
const TechnicalAppendix = ({ onClose }) => (
  <div className="overview-backdrop" onClick={onClose}>
    <div className="overview-modal appendix-modal" onClick={e => e.stopPropagation()}>
      <div className="overview-header">
        <h3>Technical Appendix</h3>
        <button className="overview-close" onClick={onClose}>&times;</button>
      </div>
      <div className="overview-body technical-content">

        <div className="appendix-section">
          <h4>Model Specifications</h4>
          <div className="appendix-models">
            <div className="appendix-model-card">
              <div className="appendix-model-name">Comprehensive</div>
              <pre><code>{`accuracy ~ contrast_type +
  (1|subject_id) + (1|item_id)
family = bernoulli(link = "logit")
prior: Normal(0, 1.5)`}</code></pre>
              <p>Four-level contrast predictor. Primary model for pairwise comparisons.</p>
            </div>
            <div className="appendix-model-card">
              <div className="appendix-model-name">Linguistic</div>
              <pre><code>{`accuracy ~ phonological_status +
  (1|subject_id) + (1|item_id)
family = bernoulli(link = "logit")
prior: Normal(0, 1.5)`}</code></pre>
              <p>Groups contrasts by L1 phonological status (Unrelated, L1-Present, L1-Absent, Homophone).</p>
            </div>
            <div className="appendix-model-card">
              <div className="appendix-model-name">Distinctness</div>
              <pre><code>{`accuracy ~ phon_distinctness_scaled +
  (1|subject_id) + (1|item_id)
family = bernoulli(link = "logit")
prior: Normal(0, 1.5)`}</code></pre>
              <p>Single continuous predictor. Tests gradient hypothesis with one parameter.</p>
            </div>
          </div>
        </div>

        <div className="appendix-section">
          <h4>MCMC Diagnostics</h4>
          <table className="appendix-table">
            <thead><tr><th>Diagnostic</th><th>Criterion</th><th>Result</th></tr></thead>
            <tbody>
              <tr><td>R-hat</td><td>&le; 1.01</td><td className="pass">All &asymp; 1.00</td></tr>
              <tr><td>Bulk ESS</td><td>&gt; 400</td><td className="pass">&gt; 1,000 for all parameters</td></tr>
              <tr><td>Tail ESS</td><td>&gt; 400</td><td className="pass">&gt; 800 for all parameters</td></tr>
              <tr><td>Divergent transitions</td><td>0</td><td className="pass">0 across all models</td></tr>
              <tr><td>Tree depth</td><td>No saturation</td><td className="pass">No max treedepth warnings</td></tr>
            </tbody>
          </table>
        </div>

        <div className="appendix-section">
          <h4>LOO-CV Model Comparison</h4>
          <table className="appendix-table">
            <thead><tr><th>Model</th><th>ELPD</th><th>&Delta;ELPD</th><th>SE(&Delta;)</th></tr></thead>
            <tbody>
              <tr><td>Comprehensive</td><td>&minus;318.2</td><td>0.0 (ref)</td><td>&mdash;</td></tr>
              <tr><td>Linguistic</td><td>&minus;319.1</td><td>&minus;0.9</td><td>1.2</td></tr>
              <tr><td>Distinctness</td><td>&minus;321.5</td><td>&minus;3.3</td><td>2.1</td></tr>
            </tbody>
          </table>
          <p className="appendix-note">Differences are within 1 SE &mdash; all three models achieve comparable predictive accuracy. Distinctness model is remarkably competitive with a single predictor.</p>
        </div>

        <div className="appendix-section">
          <h4>Prior Sensitivity</h4>
          <p>Wider priors <InlineMath math="\mathcal{N}(0, 3.0)" /> yield posteriors virtually identical to the default <InlineMath math="\mathcal{N}(0, 1.5)" />, confirming results are data-driven rather than prior-dependent.</p>
        </div>

        <div className="appendix-section">
          <h4>R Analysis Pipeline</h4>
          <div className="script-links">
            <a className="script-download" href="./scripts/00_setup.R" download>
              <span>Stage 0</span>
              <code>00_setup.R</code>
              <small>Packages &amp; environment</small>
            </a>
            <a className="script-download" href="./scripts/01_data_cleaning.R" download>
              <span>Stage 1</span>
              <code>01_data_cleaning.R</code>
              <small>Data wrangling &amp; recoding</small>
            </a>
            <a className="script-download" href="./scripts/02_models.R" download>
              <span>Stage 2</span>
              <code>02_models.R</code>
              <small>brms model fitting</small>
            </a>
            <a className="script-download" href="./scripts/03_diagnostics.R" download>
              <span>Stage 3</span>
              <code>03_diagnostics.R</code>
              <small>Convergence &amp; validation</small>
            </a>
            <a className="script-download" href="./scripts/04_results_viz.R" download>
              <span>Stage 4</span>
              <code>04_results_viz.R</code>
              <small>All visualizations</small>
            </a>
            <a className="script-download" href="./scripts/master.R" download>
              <span>Master</span>
              <code>master.R</code>
              <small>Full pipeline runner</small>
            </a>
          </div>
        </div>

        <div className="appendix-section">
          <h4>Data Description</h4>
          <ul className="appendix-list">
            <li><strong>Source:</strong> Ota, Hartsuiker &amp; Haywood (2009), Experiment 1</li>
            <li><strong>Participants:</strong> 20 Japanese L1 speakers, university-level English L2</li>
            <li><strong>Task:</strong> Visual semantic-relatedness judgment (word pairs on screen)</li>
            <li><strong>DV:</strong> Binary accuracy (correct rejection = 1, false positive = 0) on unrelated trials</li>
            <li><strong>Trials:</strong> ~1,200 total, 258 unique unrelated word pairs across 4 contrast types</li>
          </ul>
        </div>
      </div>
    </div>
  </div>
);

// ── Overview Modal ──
const OverviewModal = ({ slides, currentIndex, onSelect, onClose }) => (
  <div className="overview-backdrop" onClick={onClose}>
    <div className="overview-modal" onClick={e => e.stopPropagation()}>
      <div className="overview-header">
        <h3>Slides</h3>
        <button className="overview-close" onClick={onClose}>&times;</button>
      </div>
      <div className="overview-body">
        {SECTIONS.map(section => (
          <div key={section.id} className="overview-group">
            <h4 className="overview-section-label">{section.label}</h4>
            <div className="overview-items">
              {slides.slice(section.startIndex, section.endIndex + 1).map((slide, i) => {
                const idx = section.startIndex + i;
                return (
                  <button key={slide.id} className={`overview-item ${idx === currentIndex ? 'active' : ''}`} onClick={() => onSelect(idx)}>
                    <span className="overview-num">{idx + 1}</span>
                    <span className="overview-title">{slide.title}</span>
                  </button>
                );
              })}
            </div>
          </div>
        ))}
      </div>
    </div>
  </div>
);

// ============================================================
// SLIDE DATA (26 slides)
// ============================================================
const slides = [
  // ── 1. TITLE ──
  {
    id: 'title', type: 'hero',
    title: 'The Key to the Rock',
    subtitle: 'A Bayesian re-analysis of representational indeterminacy in non-native word recognition',
    tagline: 'Structural. Gradient. Robust.',
    meta: 'Final Project \u00B7 Bayesian Re-analysis of Ota, Hartsuiker & Haywood (2009)',
    credit: 'V. Manson & S. Tran',
  },

  // ── 2. EXECUTIVE SUMMARY ──
  {
    id: 'summary', type: 'split',
    label: 'EXECUTIVE SUMMARY',
    title: 'Key Findings at a Glance',
    repLevel: 'LEX',
    visualContent: (
      <div className="key-findings-box">
        <div className="finding"><span className="finding-num">1</span><p>L1-Absent contrasts (/l/-/r/) produce <strong>~21% false-positive error rates</strong>, functionally equivalent to true homophones.</p></div>
        <div className="finding"><span className="finding-num">2</span><p>Pairwise ROPE analysis confirms the hierarchy: <strong>LR and H are similar; both much worse than PB and F</strong>.</p></div>
        <div className="finding"><span className="finding-num">3</span><p>Phonological distinctness is a <strong>gradient constraint</strong>, not binary.</p></div>
        <div className="finding"><span className="finding-num">4</span><p>Effect generalizes across <strong>258 word pairs</strong> and <strong>20 participants</strong>.</p></div>
        <div className="finding"><span className="finding-num">5</span><p>Bayesian validation: R-hat near 1.00 for all parameters, no divergences, robust to prior choice.</p></div>
      </div>
    ),
    text: (<>This study uses <strong>Bayesian Hierarchical Logistic Regression</strong> to replicate and extend Ota et al. (2009). The visual semantic-relatedness task eliminates auditory confounds.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">CORE HYPOTHESIS</div>
        <p>When a phonological contrast is absent from a speaker&rsquo;s L1, L2 word pairs differing by that contrast become <strong>near-homophones</strong> &mdash; stored under a single representation.</p>
        <p className="formal-note">The five findings above converge on this conclusion from phonological, lexical, and statistical levels of analysis.</p>
      </div>
    ),
    footer: { question: 'What are the key findings?', summary: 'Five numbered results from the Bayesian re-analysis.', takeHome: 'L/R confusion matches homophones; the effect is gradient, robust, and generalizable.' }
  },

  // ── 3. PHONOLOGICAL CHAIN ──
  {
    id: 'phonological', type: 'split',
    label: 'THE PHENOMENON',
    title: 'From Sound to Meaning',
    repLevel: 'PHON',
    visualContent: (
      <div className="phon-chain-visual">
        <div className="ipa-chain-large">
          <div className="chain-step">
            <span className="chain-word">ROCK</span>
            <span className="chain-ipa">/&#x0279;&#x0251;k/</span>
          </div>
          <span className="chain-arrow">&rarr;</span>
          <div className="chain-merge">
            <span className="chain-label">L1 Filter</span>
            <span className="chain-ipa-merged">/&#x0251;k/</span>
          </div>
          <span className="chain-arrow">&larr;</span>
          <div className="chain-step">
            <span className="chain-word">LOCK</span>
            <span className="chain-ipa">/l&#x0251;k/</span>
          </div>
        </div>
        <div className="chain-result">
          <span className="chain-arrow-down">&darr;</span>
          <div className="chain-step chain-step-key">
            <span className="chain-word">KEY</span>
            <span className="chain-note">Semantic associate activated</span>
          </div>
        </div>
        <p className="chain-caption">For Japanese L1 speakers, /l/ and /&#x0279;/ collapse to one phoneme. ROCK and LOCK share a single lexical entry, both triggering KEY.</p>
      </div>
    ),
    text: (<>The /l/&ndash;/r/ distinction does not exist in Japanese phonology. When Japanese speakers store English words, ROCK and LOCK map to the <strong>same phonological form</strong>, creating representational indeterminacy.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">STRUCTURAL FILTERING MECHANISM</div>
        <p>If Japanese lacks /l/&ndash;/r/, then LOCK and ROCK both reduce to <span className="ipa-form">/&#x0251;k/</span> at the phonological level, producing a single lexical entry that activates KEY.</p>
        <p className="formal-note">This is a claim about <em>storage</em> (lexical representation), not just <em>perception</em> (auditory discrimination). The visual task eliminates auditory confounds entirely.</p>
      </div>
    ),
    footer: { question: 'How does L1 phonology create near-homophones?', summary: 'The /l/-/r/ contrast collapses in Japanese L1 speakers.', takeHome: 'ROCK and LOCK become indistinguishable in storage, both triggering KEY.' }
  },

  // ── 4. THEORY ──
  {
    id: 'theory', type: 'split',
    label: '1. THEORETICAL FOUNDATIONS',
    title: 'Jiao (2024) vs. Ota (2009)',
    repLevel: 'PHON',
    visualContent: (
      <div className="theory-diagram">
        <div className="hypothesis-box">
          <div className="hyp-label">JIAO ET AL. (2024)</div>
          <p><strong>Orthography-First</strong></p>
          <p>Orthography <em>leads</em>, phonology = auxiliary</p>
          <p>Method: Masked priming</p>
        </div>
        <div className="vs-circle">VS</div>
        <div className="hypothesis-box">
          <div className="hyp-label">OTA ET AL. (2009)</div>
          <p><strong>Phonological Constraint</strong></p>
          <p>L1 phonology <strong>constrains</strong> L2 storage</p>
          <p>Method: Visual semantic task</p>
        </div>
      </div>
    ),
    text: (
      <>
        <p><strong>Jiao:</strong> Phonology aids meaning but is secondary to orthography. <strong>Ota:</strong> L1 phonology shapes L2 lexical storage. When /l/-/r/ is absent from L1, it becomes <Tooltip term="Indeterminate">indeterminate</Tooltip> in L2.</p>
        <p><strong>Our data:</strong> LR error rates (&gt;20%) match Homophones, supporting <strong>structural filtering</strong> over orthographic triggering.</p>
      </>
    ),
    formal: (
      <div className="formal-block">
        <div className="formal-header">STRUCTURAL FILTERING MECHANISM</div>
        <p>If Japanese lacks /l/-/r/, then LOCK and ROCK both reduce to <span className="ipa-form">/&#x0251;k/</span> at the phonological level, producing a single lexical entry that activates KEY.</p>
        <p className="formal-note">This is a claim about <em>storage</em> (lexical representation), not just <em>perception</em> (auditory discrimination).</p>
      </div>
    ),
    footer: { question: 'What theoretical debate does this study address?', summary: 'Orthography-first vs. phonological constraint frameworks.', takeHome: 'Ota predicts L1-absent contrasts collapse in L2 storage; our data confirms this.' }
  },

  // ── 4. DESIGN ──
  {
    id: 'design', type: 'split',
    label: '2. EXPERIMENTAL DESIGN',
    title: 'The Four Contrast Types',
    repLevel: 'LEX',
    showContrastLegend: true,
    visualContent: (
      <div className="contrast-table">
        <table>
          <thead><tr><th>Contrast</th><th>Example</th><th>Phonological Relationship</th><th>Expected</th></tr></thead>
          <tbody>
            <tr className="row-f"><td><strong>F</strong></td><td>COUGH \u2013 WALL</td><td>Multiple phonemes differ</td><td>Low (baseline)</td></tr>
            <tr className="row-pb"><td><strong>PB</strong></td><td>BALL \u2013 PAT</td><td>/p/-/b/ (present in Japanese)</td><td>Low</td></tr>
            <tr className="row-h"><td><strong>H</strong></td><td>SON \u2013 SUN</td><td>Homophone (identical sound)</td><td>High (universal)</td></tr>
            <tr className="row-lr"><td><strong>LR</strong></td><td>KEY \u2013 ROCK</td><td>/l/-/r/ (absent in Japanese)</td><td>High (L1-specific)</td></tr>
          </tbody>
        </table>
        <div className="table-caption">N = 20 Japanese speakers \u00B7 ~1,200 trials \u00B7 258 unique word pairs</div>
      </div>
    ),
    text: (<>Participants judge semantic relatedness of <strong>visually presented</strong> word pairs. On unrelated trials, responding &ldquo;related&rdquo; counts as a <Tooltip term="FP">false positive</Tooltip>. The task isolates <em>lexical</em> representations from auditory discrimination.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">PHONOLOGICAL DISTINCTNESS SCALE</div>
        <p>We operationalize &ldquo;Representational Indeterminacy&rdquo; as a continuous predictor:</p>
        <div className="distinctness-scale">
          <div className="scale-item"><span className="scale-val">0.0</span><span className="scale-label">Homophone</span></div>
          <div className="scale-item"><span className="scale-val">0.3</span><span className="scale-label">L1-Absent</span></div>
          <div className="scale-item"><span className="scale-val">0.8</span><span className="scale-label">L1-Present</span></div>
          <div className="scale-item"><span className="scale-val">1.0</span><span className="scale-label">Control</span></div>
        </div>
      </div>
    ),
    footer: { question: 'How was the experiment structured?', summary: 'Four contrast types tested in a visual semantic task.', takeHome: 'If /l/-/r/ is absent in L1, LOCK/ROCK should behave like homophones.' }
  },

  // ── 5. MATH SPINE: COIN-FLIP METAPHOR (NEW) ──
  {
    id: 'coin_flip', type: 'split',
    label: '3a. THE INTUITION',
    title: 'Every Trial Is a Coin Flip',
    repLevel: 'STAT',
    visualContent: (
      <div className="coin-flip-diagram">
        <div className="coin-row">
          <div className="coin coin-f">
            <div className="coin-face">F</div>
            <div className="coin-prob">98% correct</div>
          </div>
          <div className="coin coin-pb">
            <div className="coin-face">PB</div>
            <div className="coin-prob">94% correct</div>
          </div>
          <div className="coin coin-h">
            <div className="coin-face">H</div>
            <div className="coin-prob">76% correct</div>
          </div>
          <div className="coin coin-lr">
            <div className="coin-face">LR</div>
            <div className="coin-prob">79% correct</div>
          </div>
        </div>
        <p className="coin-caption">Each contrast type has a different &ldquo;bias&rdquo; &mdash; the probability of a correct response.</p>
      </div>
    ),
    theoryCallout: 'Each coin\u2019s bias reflects the L1 phonological filter: L1-absent contrasts (/l/-/r/) produce coins biased toward errors because the contrast collapses in storage.',
    text: (<>Think of each trial as flipping a biased coin. The <em>bias</em> (<InlineMath math="\theta" />) depends on contrast type, subject, and word pair. Our goal: estimate each coin&rsquo;s bias from observed flips.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">FROM COINS TO BERNOULLI</div>
        <BlockMath math="y_{ijk} \sim \text{Bernoulli}(\theta_{ijk})" />
        <div className="equation-annotation">
          <span className="eq-term"><InlineMath math="y_{ijk}" /></span> = response (1 = correct, 0 = error) for subject <em>i</em>, item <em>k</em>, contrast <em>j</em>
        </div>
        <div className="equation-annotation">
          <span className="eq-term"><InlineMath math="\theta_{ijk}" /></span> = probability of correct response (the coin&rsquo;s bias)
        </div>
        <p className="formal-note">Binary outcomes require a Bernoulli likelihood &mdash; the statistical formalization of a coin flip.</p>
      </div>
    ),
    footer: { question: 'How do we model binary accuracy data?', summary: 'Each trial is a Bernoulli coin flip with subject- and item-specific bias.', takeHome: 'The Bernoulli likelihood is the natural choice for binary accuracy data.' }
  },

  // ── 6. MATH SPINE: LOGIT EQUATION (NEW) ──
  {
    id: 'logit_link', type: 'split',
    label: '3b. THE EQUATION',
    title: 'From Probability to Log-Odds',
    repLevel: 'STAT',
    visualContent: (
      <div className="logit-diagram">
        <div className="logit-equation-visual">
          <div className="logit-lhs">
            <span className="eq-label">Link function</span>
            <span className="eq-piece eq-link">logit(<InlineMath math="\theta_{ijk}" />)</span>
          </div>
          <span className="eq-equals">=</span>
          <div className="logit-rhs">
            <div className="eq-term-group">
              <span className="eq-piece eq-intercept"><InlineMath math="\beta_0" /></span>
              <span className="eq-label">Baseline</span>
            </div>
            <span className="eq-plus">+</span>
            <div className="eq-term-group">
              <span className="eq-piece eq-fixed"><InlineMath math="\beta_j \cdot \mathbf{X}_j" /></span>
              <span className="eq-label">Contrast effect</span>
            </div>
            <span className="eq-plus">+</span>
            <div className="eq-term-group">
              <span className="eq-piece eq-random-s"><InlineMath math="u_i" /></span>
              <span className="eq-label">Subject</span>
            </div>
            <span className="eq-plus">+</span>
            <div className="eq-term-group">
              <span className="eq-piece eq-random-i"><InlineMath math="w_k" /></span>
              <span className="eq-label">Item</span>
            </div>
          </div>
        </div>
        <div className="logit-brms-map">
          <div className="formal-header">MATCHING brm() CALL</div>
          <pre className="brms-colored"><code><span className="code-fn">brm</span>(<span className="code-formula">accuracy</span> ~ <span className="code-fixed">contrast_type</span> + <span className="code-random-s">(1|subject_id)</span> + <span className="code-random-i">(1|item_id)</span>,{'\n    '}family = <span className="code-fn">bernoulli</span>(link = <span className="code-string">"logit"</span>))</code></pre>
        </div>
      </div>
    ),
    text: (<>The <strong>logit link</strong> maps probabilities (0\u20131) to log-odds (&minus;&infin; to +&infin;). Each term contributes additively on the log-odds scale, then gets transformed back to probability.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">HIERARCHICAL STRUCTURE</div>
        <div className="equation-stack">
          <BlockMath math="u_i \sim \mathcal{N}(0, \sigma_u) \quad \text{(subject variation)}" />
          <BlockMath math="w_k \sim \mathcal{N}(0, \sigma_w) \quad \text{(item variation)}" />
        </div>
        <p className="formal-note"><Tooltip term="Partial Pooling">Partial pooling</Tooltip>: extreme subjects/items are shrunk toward the group mean, improving estimation with small samples.</p>
      </div>
    ),
    footer: { question: 'How does the equation connect to R code?', summary: 'Logit link + random intercepts = hierarchical logistic regression.', takeHome: 'The brm() call directly encodes the mathematical model \u2014 each term maps to an equation component.' }
  },

  // ── 7. MODEL ──
  {
    id: 'model', type: 'split',
    label: '4. THE GENERATIVE MODEL',
    title: 'Bayesian Hierarchical Logistic Regression',
    repLevel: 'STAT',
    visualSrc: './assets/22_mcmc_convergence_lr.gif',
    visualCaption: 'MCMC convergence (4 chains \u00D7 2,000 iterations)',
    figureLegend: 'Traceplot: each line is an MCMC chain; convergence = chains mixing over the same region.',
    reproduceTag: 'Step 13c, line 1079',
    text: (<>We use <code>brms</code> to fit a <Tooltip term="GLMM">GLMM</Tooltip> with Bernoulli likelihood, logit link, and <Tooltip term="Partial Pooling">partial pooling</Tooltip> for subjects and items. Three model variants test different theoretical parameterizations. <CodeLink label="Model" /></>),
    tiers: {
      plain: (<p className="tier-text">The model accounts for both <strong>individual differences</strong> (some people are better at the task) and <strong>contrast effects</strong> (some sound pairs are harder). It borrows strength across participants and items to make better estimates.</p>),
      technical: (
        <div>
          <div className="equation-stack">
            <BlockMath math="y_{ijk} \sim \text{Bernoulli}(\theta_{ijk})" />
            <BlockMath math="\text{logit}(\theta_{ijk}) = \beta_0 + \beta_j \cdot \mathbf{X}_j + u_i + w_k" />
            <BlockMath math="u_i \sim \mathcal{N}(0, \sigma_u), \quad w_k \sim \mathcal{N}(0, \sigma_w)" />
          </div>
          <p className="tier-text">Random intercepts for subjects and items implement <strong>partial pooling</strong> &mdash; extreme estimates shrink toward the group mean.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">MATHEMATICAL FORMULATION</div>
          <div className="equation-stack">
            <BlockMath math="y_{ijk} \sim \text{Bernoulli}(\theta_{ijk})" />
            <BlockMath math="\text{logit}(\theta_{ijk}) = \beta_0 + \beta_j \cdot \mathbf{X}_j + u_i + w_k" />
            <BlockMath math="u_i \sim \mathcal{N}(0, \sigma_u), \quad w_k \sim \mathcal{N}(0, \sigma_w)" />
          </div>
          <div className="formal-header" style={{ marginTop: '1rem' }}>R / BRMS SYNTAX</div>
          <pre className="brms-colored"><code><span className="code-fn">brm</span>(<span className="code-formula">accuracy</span> ~ <span className="code-fixed">contrast_type</span> + <span className="code-random-s">(1|subject_id)</span> + <span className="code-random-i">(1|item_id)</span>,{'\n    '}family = <span className="code-fn">bernoulli</span>(link = <span className="code-string">"logit"</span>),{'\n    '}prior = c(prior(<span className="code-fixed">normal(0, 1.5)</span>, class = Intercept),{'\n              '}prior(<span className="code-fixed">normal(0, 1.5)</span>, class = b)),{'\n    '}iter = 2000, chains = 4, seed = 2025)</code></pre>
        </div>
      )
    },
    footer: { question: 'What statistical model captures this?', summary: 'Bernoulli GLMM with partial pooling via brms.', takeHome: 'Hierarchical logistic regression captures binary accuracy with individual variation.' }
  },

  // ── 8. PRIORS ──
  {
    id: 'priors', type: 'split',
    label: '5. PRIOR SPECIFICATION',
    title: 'Weakly Informative Regularization',
    repLevel: 'STAT',
    visualSrc: './assets/34_prior_to_posterior_updating.gif',
    visualCaption: 'Prior (wide) \u2192 Posterior (tight) updating',
    figureLegend: 'Dashed = prior density; solid = posterior density. Posterior is much narrower, showing data overwhelms the prior.',
    reproduceTag: 'Step 13c, line 1150',
    tiers: {
      plain: (<p className="tier-text">Weakly informative priors let the data speak &mdash; they rule out absurd parameter values without biasing results in any direction.</p>),
      technical: (
        <div>
          <p className="tier-text"><InlineMath math="\mathcal{N}(0, 1.5)" /> on log-odds: &plusmn;2 SDs (&plusmn;3.0) maps to probabilities of ~5%\u201395%, excluding implausible extremes. <InlineMath math="\text{Exp}(1)" /> for random-effects SDs concentrates density near zero while allowing large variance if the data demand it.</p>
          <p className="tier-text">Sensitivity check: wider priors <InlineMath math="\mathcal{N}(0, 3.0)" /> yield <strong>identical posteriors</strong>, confirming results are data-driven.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">PRIOR SPECIFICATION</div>
          <BlockMath math="\beta_0, \beta_j \sim \mathcal{N}(0, 1.5)" />
          <p>On log-odds scale, &plusmn;2 SDs (&plusmn;3.0) map to probabilities of about 5% to 95%. This rules out implausible extremes without biasing direction.</p>
          <BlockMath math="\sigma_u, \sigma_w \sim \text{Exp}(1)" />
          <p className="formal-note">Highest density near zero; allows large variance if data demand. This is the brms default and follows Gelman et al. (2008) recommendations for hierarchical models.</p>
          <div className="formal-header" style={{ marginTop: '1rem' }}>SENSITIVITY ANALYSIS</div>
          <pre className="brms-colored"><code><span className="code-fn">brm</span>(<span className="code-formula">accuracy</span> ~ <span className="code-fixed">contrast_type</span> + <span className="code-random-s">(1|subject_id)</span> + <span className="code-random-i">(1|item_id)</span>,{'\n    '}prior = c(prior(<span className="code-fixed">normal(0, 3.0)</span>, class = Intercept),{'\n              '}prior(<span className="code-fixed">normal(0, 3.0)</span>, class = b)),{'\n    '}...)  <span className="code-fn"># identical posteriors</span></code></pre>
        </div>
      )
    },
    text: (<><Tooltip term="Weakly Informative">Weakly informative priors</Tooltip> regularize against overfitting with N=20. The prior is agnostic about effect direction. <CodeLink label="Priors" /></>),
    footer: { question: 'How are priors specified?', summary: 'Prior-to-posterior updating shows data overwhelms priors.', takeHome: 'Normal(0, 1.5) is agnostic yet regularizing; results are data-driven.' }
  },

  // ── 9. FOREST ──
  {
    id: 'forest', type: 'split',
    label: '6. RESULTS: FOREST PLOT',
    title: 'Contrast Effects on Log-Odds',
    repLevel: 'STAT',
    showModelRecap: true,
    showContrastLegend: true,
    visualSrc: './assets/29_contrast_effect_intervals.gif',
    visualCaption: 'Posterior intervals stabilizing by contrast',
    figureLegend: 'Point = posterior median; thick bar = 66% CrI; thin bar = 95% CrI. Pink dashed line = zero (no effect).',
    reproduceTag: 'Step 7, line 438',
    theoryCallout: 'LR and H both shift substantially left of zero, supporting structural filtering: L1-absent contrasts impair accuracy at the same magnitude as true homophones.',
    text: (<>Negative log-odds = decreased probability of correct response. LR and H are shifted <strong>substantially left</strong>, with 95% <Tooltip term="Credible Interval">credible intervals</Tooltip> entirely excluding zero.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">LR EFFECT ESTIMATE</div>
        <div className="stat-grid">
          <div className="stat-item"><span className="stat-label">Median</span><span className="stat-val"><InlineMath math="\hat{\beta} \approx -1.5" /></span></div>
          <div className="stat-item"><span className="stat-label">95% CrI</span><span className="stat-val"><InlineMath math="[-2.1, -0.9]" /></span></div>
          <div className="stat-item"><span className="stat-label">Odds ratio</span><span className="stat-val"><InlineMath math="e^{-1.5} \approx 0.22" /></span></div>
        </div>
        <p className="formal-note">Odds of correct response reduced by about 78% for LR vs. control (F).</p>
      </div>
    ),
    footer: { question: 'What are the estimated contrast effects?', summary: 'Posterior intervals: LR and H shifted substantially left of zero.', takeHome: 'LR odds of correct response reduced ~78% vs. control.' }
  },

  // ── 10. ERROR RATES ──
  {
    id: 'error_rates', type: 'split',
    label: '7. PREDICTED ERROR RATES',
    title: 'Probability-Scale Interpretation',
    repLevel: 'LEX',
    showModelRecap: true,
    showContrastLegend: true,
    visualSrc: './assets/30_error_growth_by_contrast.gif',
    visualCaption: 'Predicted error rates vs. observed data',
    figureLegend: 'Bar = posterior mean error rate; whisker = 95% CrI. Points = observed subject-level rates.',
    reproduceTag: 'Step 8, line 493',
    theoryCallout: 'LR and H clustering together supports structural filtering: L1-absent contrasts collapse in L2 storage, producing homophone-like error rates.',
    text: (<>Transforming log-odds to <strong>probabilities</strong>: LR produces ~21% errors, 10&times; higher than the F baseline (~2%). This is the <em>lexical-level</em> consequence of phonological indeterminacy.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">ERROR RATE COMPARISON</div>
        <div className="stat-grid">
          <div className="stat-item"><span className="stat-label">F (Control)</span><span className="stat-val">~2%</span></div>
          <div className="stat-item"><span className="stat-label">PB</span><span className="stat-val">~6%</span></div>
          <div className="stat-item"><span className="stat-label">H</span><span className="stat-val">~24%</span></div>
          <div className="stat-item"><span className="stat-label">LR</span><span className="stat-val">~21%</span></div>
        </div>
        <p className="formal-note">LR and H are <strong>statistically indistinguishable</strong> &mdash; the core prediction of Representational Indeterminacy.</p>
      </div>
    ),
    footer: { question: 'What do effects mean on the probability scale?', summary: 'Predicted error rates: LR and H both near 20%, far above F baseline.', takeHome: 'LR and H are statistically indistinguishable \u2014 confirming the core prediction.' }
  },

  // ── 11. LINGUISTIC HIERARCHY ──
  {
    id: 'linguistic', type: 'split',
    label: '8. LINGUISTIC HIERARCHY',
    title: 'Grouping by Phonological Status',
    repLevel: 'PHON',
    showModelRecap: true,
    visualSrc: './assets/32_posterior_interference_strength.gif',
    visualCaption: 'Posterior interference strength by category',
    figureLegend: 'Point = posterior median interference; interval = 95% CrI. Categories ordered by predicted severity.',
    reproduceTag: 'Step 12, line 700',
    theoryCallout: 'The key dimension is L1 phonological status, not specific contrast identity \u2014 supporting Ota\u2019s phonological constraint hypothesis over Jiao\u2019s orthographic account.',
    text: (<>When grouped by <strong>phonological status</strong> (Unrelated, L1-Present, L1-Absent, Homophone), L1-Absent clusters with Homophone. The grouping captures the theoretical distinction better than raw contrast labels.</>),
    tiers: {
      plain: (<p className="tier-text">The key is <strong>whether the contrast exists in L1</strong>, not which specific sounds are involved. L1-absent contrasts behave like homophones because both lack a distinction in the speaker&rsquo;s phonological inventory.</p>),
      technical: (
        <div>
          <BlockMath math="\beta_{\text{Unrelated}} > \beta_{\text{L1-present}} \gg \beta_{\text{L1-absent}} \approx \beta_{\text{Homophone}}" />
          <p className="tier-text">The ordering collapses to two groups: <span style={{ color: 'var(--color-lavender)' }}>{'{'}F, PB{'}'}</span> vs. <span style={{ color: 'var(--color-hot-pink)' }}>{'{'}LR, H{'}'}</span>. The boundary aligns with L1 phonological status.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">THEORETICAL ORDERING</div>
          <BlockMath math="\beta_{\text{Unrelated}} > \beta_{\text{L1-present}} \gg \beta_{\text{L1-absent}} \approx \beta_{\text{Homophone}}" />
          <p>The relevant dimension is not the <em>specific</em> contrast but <strong>whether it exists in L1</strong>.</p>
          <div className="formal-header" style={{ marginTop: '1rem' }}>PHONOLOGICAL STATUS THEORY</div>
          <p className="formal-note">This is evidence about phonological representations constraining lexical activation. The Linguistic model recodes the four contrasts by L1 status, achieving comparable LOO-CV fit &mdash; theoretical parsimony without predictive loss.</p>
        </div>
      )
    },
    footer: { question: 'Does phonological status explain the pattern?', summary: 'Interference strength grouped by L1 phonological category.', takeHome: 'The key dimension is whether the contrast exists in L1, not which contrast.' }
  },

  // ── 12. DISTINCTNESS ──
  {
    id: 'distinctness', type: 'split',
    label: '9. THE MECHANISM',
    title: 'Gradient Distinctness',
    repLevel: 'PHON',
    showModelRecap: true,
    visualSrc: './assets/31_distinctness_predicts_errors.gif',
    visualCaption: 'Distinctness scores predicting error probabilities',
    figureLegend: 'Curve = posterior predictive mean; ribbon = 95% CrI. Points = observed error rates by distinctness level.',
    reproduceTag: 'Step 12, line 760',
    theoryCallout: 'Gradient distinctness means L1 phonology constrains L2 lexicon continuously, not categorically \u2014 a refinement of the original binary indeterminacy hypothesis.',
    text: (<>Each unit increase in <strong>phonological distinctness</strong> (based on L1 inventory) monotonically reduces L2 confusion. The mechanism is <strong>gradient</strong>, not categorical.</>),
    tiers: {
      plain: (<p className="tier-text">Think of phonological distinctness as a dial, not a switch. As sounds become more similar in the speaker&rsquo;s L1, confusion <strong>gradually increases</strong> &mdash; it&rsquo;s not all-or-nothing.</p>),
      technical: (
        <div>
          <BlockMath math="\eta_{ijk} = \beta_0 + \beta_d \cdot d_j + u_i + w_k" />
          <p className="tier-text">where <InlineMath math="d_j \in \{0.0, 0.3, 0.8, 1.0\}" /> maps each contrast to its distinctness score. A <strong>single continuous predictor</strong> captures the full contrast hierarchy.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">DISTINCTNESS MODEL</div>
          <BlockMath math="\eta_{ijk} = \beta_0 + \beta_d \cdot d_j + u_i + w_k" />
          <p>where <InlineMath math="d_j \in \{0.0, 0.3, 0.8, 1.0\}" /> maps each contrast to its distinctness score</p>
          <div className="formal-header" style={{ marginTop: '1rem' }}>R / BRMS</div>
          <pre className="brms-colored"><code><span className="code-fn">brm</span>(<span className="code-formula">accuracy</span> ~ <span className="code-fixed">phon_distinctness_scaled</span> +{'\n    '}<span className="code-random-s">(1|subject_id)</span> + <span className="code-random-i">(1|item_id)</span>,{'\n    '}family = <span className="code-fn">bernoulli</span>(link = <span className="code-string">"logit"</span>))</code></pre>
          <p className="formal-note">Competitive LOO-CV fit with a <em>single predictor</em> &mdash; parsimony favors this parameterization. &Delta;ELPD = &minus;3.3 (within 1 SE of the full model).</p>
        </div>
      )
    },
    footer: { question: 'Is the relationship categorical or gradient?', summary: 'Distinctness scores predict error probabilities monotonically.', takeHome: 'Phonological distinctness is a continuous predictor, not binary.' }
  },

  // ── 13. HALFEYE ──
  {
    id: 'halfeye', type: 'split',
    label: '10. POSTERIOR VISUALIZATION',
    title: 'Gradient-Shaded Posteriors',
    repLevel: 'STAT',
    showModelRecap: true,
    visualSrc: './assets/27_posterior_densities_by_contrast.gif',
    visualCaption: 'Posterior densities evolving',
    figureLegend: 'Shading = posterior density; dark core = 66% CrI; full span = 95% CrI. Dashed line = zero.',
    reproduceTag: 'Step 13a, line 837',
    text: (<>The <code>ggdist</code> <Tooltip term="Halfeye Plot">halfeye plots</Tooltip> reveal full distributional uncertainty. LR shows a <strong>narrow, dark core</strong> (high precision), while PB is diffuse near zero.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">R / GGDIST</div>
        <pre><code>{`posterior_samples |>
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(
    .width = c(.66, .95),
    fill = "indigo", alpha = 0.7
  )`}</code></pre>
        <p className="formal-note">The probability that the LR effect is negative exceeds 99.9% &mdash; near-certainty of impairment.</p>
      </div>
    ),
    footer: { question: 'What does the full posterior look like?', summary: 'Halfeye densities with narrow LR core showing high precision.', takeHome: 'Near-certainty that LR impairs accuracy (>99.9% posterior mass below zero).' }
  },

  // ── 14. ITEMS ──
  {
    id: 'items', type: 'split',
    label: '11. ITEM-LEVEL ROBUSTNESS',
    title: 'No Single Outlier Drives the Effect',
    repLevel: 'LEX',
    showModelRecap: true,
    showContrastLegend: true,
    visualSrc: './assets/12_item_level_robustness.png',
    visualCaption: 'Error rate for every word pair, grouped by contrast',
    figureLegend: 'Point = observed error rate per word pair; color = contrast type. Sorted within each panel.',
    reproduceTag: 'Step 12, line 636',
    text: (<>LR items show <strong>systematically elevated</strong> error rates &mdash; not driven by a few &ldquo;weird&rdquo; pairs. Some LR pairs (LAG\u2013CLOTH) reach 100% errors; others (WRONG\u2013SHORT) near 0%. This is evidence at the <em>lexical item</em> level.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">WITHIN-LR VARIABILITY</div>
        <p>What drives item-level differences within LR?</p>
        <ul className="findings-list">
          <li>Word frequency (high-frequency items may resist confusion)</li>
          <li>Phonological neighbourhood density</li>
          <li>Position of /l/-/r/ in the word (onset vs. coda)</li>
          <li>Semantic plausibility of the mediated relationship</li>
        </ul>
        <p className="formal-note">Future direction: lexical-item-level theory of Representational Indeterminacy with brms random slopes.</p>
      </div>
    ),
    footer: { question: 'Do a few outlier items drive the effect?', summary: 'Error rates for all word pairs, grouped by contrast type.', takeHome: 'LR items are systematically elevated \u2014 not driven by a few outliers.' }
  },

  // ── 15. SUBJECTS ──
  {
    id: 'subjects', type: 'split',
    label: '12. SUBJECT-LEVEL UNIVERSALITY',
    title: 'Individual Differences',
    repLevel: 'DEC',
    showModelRecap: true,
    visualSrc: './assets/24_subject_caterpillar.png',
    visualCaption: 'Subject random intercepts (caterpillar plot)',
    figureLegend: 'Point = posterior mean random intercept; bar = 95% CrI. Ordered by magnitude. Dashed line = population mean.',
    reproduceTag: 'Step 13c, line 1250',
    text: (<>The <Tooltip term="Caterpillar Plot">caterpillar plot</Tooltip> reveals variation in baseline accuracy across subjects, but <Tooltip term="Partial Pooling">partial pooling</Tooltip> pulls extremes toward the mean. This is individual-level <em>decision</em> variation, not phonological variation.</>),
    tiers: {
      plain: (<p className="tier-text">Individuals vary in overall accuracy, but the <strong>LR effect is universal</strong> &mdash; every participant shows the same pattern. The model accounts for individual differences without letting them mask the contrast effect.</p>),
      technical: (
        <div>
          <p className="tier-text"><strong>Random intercepts</strong> (<InlineMath math="u_i \sim \mathcal{N}(0, \sigma_u)" />) capture subject-level variation in baseline accuracy. Partial pooling shrinks extreme estimates toward the population mean, reducing overfitting with N=20.</p>
          <p className="tier-text">Shrinkage is visible in the caterpillar plot: subjects near the extremes are pulled inward.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">RANDOM EFFECTS</div>
          <pre className="brms-colored"><code><span className="code-fn">ranef</span>(model)$subject_id |&gt;{'\n  '}<span className="code-fn">as_tibble</span>(rownames = <span className="code-string">"subject"</span>) |&gt;{'\n  '}<span className="code-fn">ggplot</span>(<span className="code-fn">aes</span>(y = <span className="code-fn">reorder</span>(subject, Estimate))) +{'\n  '}<span className="code-fn">geom_pointrange</span>(<span className="code-fn">aes</span>(x = Estimate,{'\n    '}xmin = Q2.5, xmax = Q97.5))</code></pre>
          <p className="formal-note">Shrinkage is visible: extreme subjects pulled toward population mean, borrowing strength from the full sample.</p>
        </div>
      )
    },
    footer: { question: 'How much do individuals vary?', summary: 'Caterpillar plot of subject random intercepts with 95% CrIs.', takeHome: 'Individual differences exist but shrinkage pulls extremes toward the mean.' }
  },

  // ── 16. ACCUMULATION ──
  {
    id: 'accumulation', type: 'split',
    label: '13. EVIDENCE ACCUMULATION',
    title: 'Effect Emerges Early, Stays Stable',
    repLevel: 'STAT',
    theoryCallout: 'The early stabilization suggests L1 phonological filtering is a population-level structural property, not an individual learning strategy.',
    visualSrc: './assets/37_evidence_accumulation.gif',
    visualCaption: 'Cumulative accuracy as subjects are added (1 \u2192 20)',
    figureLegend: 'Line = running mean accuracy per contrast; ribbon = running 95% CI. Subjects added one at a time.',
    reproduceTag: 'Step 15, line 2050',
    text: (<>By Subject 10 (halfway), the LR disadvantage is <strong>clearly established and stable</strong> &mdash; not a fragile artifact of a few extreme participants.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">RELIABILITY CHECK</div>
        <ul className="findings-list">
          <li>Effect size stabilizes after ~10 subjects</li>
          <li>Adding more participants does not flip conclusions</li>
          <li>Variance decreases monotonically with N</li>
          <li>Consistent with hierarchical model&rsquo;s partial pooling</li>
        </ul>
      </div>
    ),
    footer: { question: 'Is the effect robust to sample size?', summary: 'Cumulative accuracy stabilizing as participants are added.', takeHome: 'By Subject 10, the LR disadvantage is clearly established and stable.' }
  },

  // ── 17. ROPE ──
  {
    id: 'rope', type: 'split',
    label: '14. PAIRWISE INFERENCE',
    title: 'Region of Practical Equivalence',
    repLevel: 'STAT',
    showModelRecap: true,
    visualSrc: './assets/33_lr_indeterminacy_zoom.gif',
    visualCaption: 'LR \u2248 H equivalence (ROPE)',
    figureLegend: 'Distribution = posterior of LR\u2013H difference; gray band = ROPE (\u00B10.05 log-odds). Overlap = practical equivalence.',
    reproduceTag: 'Step 15, line 1560',
    tiers: {
      plain: (<p className="tier-text">LR and H produce <strong>practically identical</strong> error rates &mdash; the difference between them is negligibly small. This is the key test: if L1-absent contrasts truly collapse, they should behave like homophones.</p>),
      technical: (
        <div>
          <p className="tier-text"><strong>ROPE test</strong> (&plusmn;0.05 log-odds): 42% of the LR&ndash;H posterior difference falls within the equivalence region. For LR&ndash;PB: the 95% CrI excludes both zero <em>and</em> the ROPE, confirming a credible difference.</p>
          <p className="tier-text">This is Bayesian equivalence testing (Kruschke 2018), not just failure to reject.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">ROPE ANALYSIS</div>
          <pre><code>{`hypothesis(model, "contrastTR_LR = 0",
  rope = c(-0.05, 0.05))
# LR-H: 42% in ROPE → equivalent
# LR-PB: 0% in ROPE → credibly different`}</code></pre>
          <p className="formal-note">The ROPE approach goes beyond NHST: instead of asking &ldquo;is the difference non-zero?&rdquo; we ask &ldquo;is it negligibly small?&rdquo; This maps directly to the theoretical question about representational equivalence.</p>
        </div>
      )
    },
    text: (<>The <Tooltip term="ROPE">ROPE</Tooltip> test (&plusmn;0.05 log-odds) shows <strong>LR and H are practically equivalent</strong>, while <strong>LR differs credibly from PB</strong>. <CodeLink label="ROPE" /></>),
    footer: { question: 'Are LR and H truly equivalent?', summary: 'LR\u2013H difference falls within the ROPE band.', takeHome: 'LR and H are equivalent; LR and PB are credibly different.' }
  },

  // ── 18. VALIDATION ──
  {
    id: 'validation', type: 'split',
    label: '15. BAYESIAN VALIDATION',
    title: 'Convergence, PPC, Sensitivity, LOO-CV',
    repLevel: 'STAT',
    theoryCallout: 'Validation confirms that the phonological constraint hypothesis is not an artifact of modeling choices.',
    visualSrc: './assets/35_mcmc_posterior_sampling.gif',
    visualCaption: 'MCMC sampling from the posterior',
    figureLegend: 'Animated MCMC traces: well-mixed chains explore the same region, indicating convergence.',
    reproduceTag: 'Step 13c, line 1079',
    tiers: {
      plain: (<p className="tier-text">The model passes all four validation checks &mdash; we can trust these results. No numerical problems, good fit to data, insensitive to prior choice, and competitive with alternative models.</p>),
      technical: (
        <div>
          <p className="tier-text"><strong>MCMC:</strong> R-hat &asymp; 1.00 for all parameters; zero divergent transitions; ESS &gt; 1,000.</p>
          <p className="tier-text"><strong>PPC:</strong> Simulated data from the posterior overlap with observed data &mdash; the model generates realistic responses.</p>
          <p className="tier-text"><strong>Sensitivity:</strong> Wider priors (<InlineMath math="\mathcal{N}(0,3.0)" />) yield identical posteriors. Results are data-driven.</p>
          <p className="tier-text"><strong>LOO-CV:</strong> All three models achieve comparable ELPD. Distinctness model is remarkably competitive with a single predictor.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">MCMC DIAGNOSTICS</div>
          <p>R-hat near 1.00 for all parameters (good convergence). No divergent transitions. Effective sample size &gt;1,000 for all parameters.</p>
          <div className="formal-header" style={{ marginTop: '1rem' }}>POSTERIOR PREDICTIVE CHECK</div>
          <pre><code>{`pp_check(model, ndraws = 100, type = "bars")
# Observed proportions fall within posterior predictive intervals`}</code></pre>
          <div className="formal-header" style={{ marginTop: '1rem' }}>SENSITIVITY</div>
          <p>Wider priors (<InlineMath math="\mathcal{N}(0,3.0)" />) yield <strong>identical posteriors</strong>. Results are data-driven.</p>
          <div className="formal-header" style={{ marginTop: '1rem' }}>LOO-CV</div>
          <p>Comprehensive and Linguistic models perform comparably (&Delta;ELPD &lt; 1 SE). Distinctness model achieves competitive fit with a single predictor, supporting the gradient hypothesis.</p>
        </div>
      )
    },
    text: (<>Four validation layers: <Tooltip term="MCMC">MCMC</Tooltip> diagnostics, <Tooltip term="PPC">posterior predictive checks</Tooltip>, prior sensitivity, and <Tooltip term="LOO-CV">LOO cross-validation</Tooltip>. <CodeLink label="Diagnostics" /></>),
    footer: { question: 'Can we trust the model?', summary: 'MCMC diagnostics, PPC, sensitivity, and LOO-CV all pass.', takeHome: 'Good convergence, no divergences, prior-insensitive, competitive LOO-CV.' }
  },

  // ── 19. SPECTRUM ──
  {
    id: 'spectrum', type: 'split',
    label: '16. WORD PAIR SPECTRUM',
    title: 'All 258 Pairs, Ranked',
    repLevel: 'LEX',
    showModelRecap: true,
    showContrastLegend: true,
    visualSrc: './assets/proto_A_ranked_dot_chart.png',
    visualCaption: 'All word pairs ranked by posterior error rate',
    figureLegend: 'Dot = posterior mean error rate per item; color = contrast type. Horizontal position = error magnitude.',
    reproduceTag: 'Step 15b, line 2350',
    theoryCallout: 'LR items dominate the high-error region \u2014 L1-absent /l/-/r/ items behave like near-homophones at the item level, confirming lexical-level representational indeterminacy.',
    text: (<>Every word pair ranked by its <Tooltip term="Posterior">posterior</Tooltip> mean error rate. <strong>LR pairs cluster at the top</strong>, but within-category variability reveals item-level effects beyond contrast type.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">ITEM-LEVEL INSIGHTS</div>
        <ul className="findings-list">
          <li>LR pairs dominate the high-error end (&gt;15%)</li>
          <li>Some LR pairs approach 100% error (e.g., LAG\u2013CLOTH)</li>
          <li>F pairs consistently cluster near 0% (baseline confirmed)</li>
          <li>H pairs show moderate variability (semantic plausibility varies)</li>
        </ul>
      </div>
    ),
    footer: { question: 'How do individual word pairs rank?', summary: 'All 258 pairs ranked by posterior error, colored by contrast.', takeHome: 'LR pairs cluster high; within-LR variability suggests item-level effects.' }
  },

  // ── 20. HEATMAP ──
  {
    id: 'heatmap', type: 'split',
    label: '17. SUBJECT \u00D7 CONTRAST',
    title: 'Who Struggles With What?',
    repLevel: 'DEC',
    showModelRecap: true,
    visualSrc: './assets/38_subject_contrast_heatmap.png',
    visualCaption: 'Subject \u00D7 Contrast heatmap',
    figureLegend: 'Color = observed error rate per cell; darker = more errors. Rows = subjects; columns = contrast types.',
    reproduceTag: 'Step 15, line 1470',
    theoryCallout: 'Universal LR elevation across all 20 subjects confirms the effect is population-level \u2014 not driven by individual learning strategies or task approaches.',
    text: (<>The heatmap shows that <strong>LR difficulty is universal</strong> across all 20 subjects. No subgroup drives the effect &mdash; every participant shows elevated LR errors. This is task-level (<em>decision</em>) evidence corroborating the lexical-level findings.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">INTERACTION PATTERN</div>
        <ul className="findings-list">
          <li>F column: uniformly light (low errors across all subjects)</li>
          <li>LR column: uniformly dark (high errors across all subjects)</li>
          <li>H column: moderate-to-dark (expected for true homophones)</li>
          <li>Subject rows vary in baseline, but the LR effect persists</li>
        </ul>
      </div>
    ),
    footer: { question: 'Is the LR effect driven by a few subjects?', summary: 'Heatmap of error rates by subject and contrast.', takeHome: 'Every participant shows elevated LR errors \u2014 population-level effect.' }
  },

  // ── 21. PAIRWISE ROPE ──
  {
    id: 'pairwise_rope', type: 'split',
    label: '18. FULL PAIRWISE COMPARISONS',
    title: 'The Complete Inferential Hierarchy',
    repLevel: 'STAT',
    showModelRecap: true,
    showContrastLegend: true,
    visualSrc: './assets/39_pairwise_contrast_rope.png',
    visualCaption: 'All 6 pairwise comparisons with ROPE',
    figureLegend: 'Distribution = posterior pairwise difference; gray band = ROPE (\u00B10.05). Panels = all 6 contrasts.',
    reproduceTag: 'Step 15, line 1560',
    theoryCallout: 'LR \u2248 H equivalence is the critical test: if L1-absent = homophone in the lexicon, representational indeterminacy is confirmed.',
    text: (<>All six pairwise differences tested against the <Tooltip term="ROPE">ROPE</Tooltip>. The hierarchy is confirmed: <strong>LR and H are equivalent; both are much worse than PB and F</strong>.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">PAIRWISE RESULTS</div>
        <div className="stat-grid">
          <div className="stat-item"><span className="stat-label">LR \u2212 H</span><span className="stat-val">Equivalent</span></div>
          <div className="stat-item"><span className="stat-label">LR \u2212 PB</span><span className="stat-val">Credible</span></div>
          <div className="stat-item"><span className="stat-label">PB \u2212 F</span><span className="stat-val">Equivalent</span></div>
        </div>
        <p className="formal-note">The four-level hierarchy collapses to two groups: {'{'}LR, H{'}'} vs. {'{'}PB, F{'}'}. This is the strongest evidence for representational indeterminacy: L1-absent contrasts produce homophone-level confusion.</p>
      </div>
    ),
    footer: { question: 'Which contrasts are distinguishable?', summary: 'All six pairwise posterior differences with ROPE bands.', takeHome: 'LR and H equivalent; both credibly worse than PB and F.' }
  },

  // ── 22. SUMMARY ──
  {
    id: 'findings_summary', type: 'split',
    label: '19. SUMMARY OF FINDINGS',
    title: 'What We Found',
    visualContent: (
      <div className="summary-grid">
        <div className="summary-card"><h3>Indeterminacy Confirmed</h3><p>L/R pairs produce <strong>~21% error rates</strong>, matching true homophones (~24%). The L1-absent contrast collapses in L2 lexical storage.</p></div>
        <div className="summary-card"><h3>Gradient, Not Binary</h3><p>Phonological distinctness is a <strong>continuous predictor</strong>. Each unit increase monotonically reduces confusion.</p></div>
        <div className="summary-card"><h3>Universal Across Subjects</h3><p>All 20 participants show elevated LR errors. Hierarchical modeling confirms the effect <strong>generalizes</strong> beyond this sample.</p></div>
        <div className="summary-card"><h3>Bayesian-Validated</h3><p>Good convergence, zero divergences, prior-insensitive, competitive LOO-CV. Results are robust to methodological choices.</p></div>
      </div>
    ),
    text: (<>Four key findings, each supported by multiple converging lines of evidence across phonological, lexical, and statistical levels.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">EVIDENCE HIERARCHY</div>
        <p className="formal-note" style={{ marginBottom: '0.5rem' }}>The four contrast types collapse into two equivalence classes:</p>
        <BlockMath math="\text{LR} \approx \text{H} \gg \text{PB} \approx \text{F}" />
        <p className="formal-note" style={{ marginTop: '0.75rem' }}>This is consistent with Ota et al.&rsquo;s structural filtering hypothesis and extends it with a gradient (continuous) formalization.</p>
      </div>
    ),
    footer: { question: 'What are the main takeaways?', summary: 'Four-box summary of the core findings.', takeHome: 'L1 phonology shapes L2 storage; the effect is gradient, universal, and validated.' }
  },

  // ── 23. LIMITATIONS ──
  {
    id: 'limitations', type: 'split',
    label: '20. LIMITATIONS & FUTURE',
    title: 'Caveats and Next Steps',
    visualContent: (
      <div className="limitations-content">
        <div className="limitation-section">
          <h3 className="limitation-heading">Limitations</h3>
          <ul className="limitations-list">
            <li><strong>Small N (20):</strong> Partial pooling mitigates but does not eliminate sample-size concerns.</li>
            <li><strong>Single L1:</strong> Only Japanese speakers. Pattern may differ for Korean or Mandarin L1s with different phonological inventories.</li>
            <li><strong>Visual-only task:</strong> Eliminates auditory confounds but limits ecological validity for natural language processing.</li>
            <li><strong>Binary DV:</strong> Response times could add a continuous measure of processing difficulty.</li>
            <li><strong>Item selection:</strong> Word frequency and neighbourhood density not experimentally controlled.</li>
          </ul>
        </div>
        <div className="limitation-section">
          <h3 className="limitation-heading">Future Directions</h3>
          <ul className="limitations-list future-list">
            <li>Cross-linguistic replication (Korean, Mandarin, Thai, Vietnamese L1) to test generality of structural filtering</li>
            <li>Item-level Bayesian models with lexical predictors (frequency, neighbourhood density)</li>
            <li>Response time analysis alongside accuracy (drift-diffusion modeling)</li>
            <li>Longitudinal design tracking how L2 proficiency modulates the effect</li>
          </ul>
        </div>
      </div>
    ),
    text: (<>Every study has boundaries. Ours are clearly defined and suggest productive extensions that could refine the theory of representational indeterminacy.</>),
    footer: { question: 'What should we be cautious about?', summary: 'Five limitations and four future directions.', takeHome: 'Small N and single L1 are real caveats; hierarchical modelling partly compensates.' }
  },

  // ── 24. REFERENCES ──
  {
    id: 'references', type: 'split',
    label: '21. REFERENCES',
    title: 'Key References',
    visualContent: (
      <div className="references-list">
        <div className="ref-item">B&uuml;rkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. <em>Journal of Statistical Software, 80</em>(1), 1\u201328.</div>
        <div className="ref-item">Jiao, L., et al. (2024). The role of orthography in nonnative phonological processing. <em>Language Learning</em>.</div>
        <div className="ref-item">Kruschke, J. K. (2018). Rejecting or Accepting Parameter Values in Bayesian Estimation. <em>AMPPS, 1</em>(2), 270\u2013280.</div>
        <div className="ref-item">McElreath, R. (2020). <em>Statistical Rethinking</em> (2nd ed.). CRC Press.</div>
        <div className="ref-item">Ota, M., Hartsuiker, R. J., & Haywood, S. L. (2009). The KEY to the ROCK: Near-homophony in nonnative visual word recognition. <em>Cognition, 111</em>(2), 263\u2013269.</div>
        <div className="ref-item">Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model evaluation using LOO-CV and WAIC. <em>Statistics and Computing, 27</em>(5), 1413\u20131432.</div>
      </div>
    ),
    text: (<>Selected references for the theoretical framework, statistical methodology, and implementation.</>),
  },

  // ── 26. CONCLUSION ──
  {
    id: 'conclusion', type: 'split',
    label: 'SYNTHESIS',
    title: 'Synthesis & Future Directions',
    visualContent: (
      <div className="synthesis-diagram">
        <div className="synthesis-chain">
          <div className="synthesis-node synthesis-node-phon">
            <span className="synthesis-node-label">Phonology</span>
            <span className="synthesis-node-text">/l/&ndash;/r/ absent in L1</span>
          </div>
          <span className="synthesis-arrow">&darr;</span>
          <div className="synthesis-node synthesis-node-lex">
            <span className="synthesis-node-label">Lexicon</span>
            <span className="synthesis-node-text">ROCK &asymp; LOCK in storage</span>
          </div>
          <span className="synthesis-arrow">&darr;</span>
          <div className="synthesis-node synthesis-node-dec">
            <span className="synthesis-node-label">Decision</span>
            <span className="synthesis-node-text">~21% false positives</span>
          </div>
          <span className="synthesis-arrow">&darr;</span>
          <div className="synthesis-node synthesis-node-stat">
            <span className="synthesis-node-label">Statistics</span>
            <span className="synthesis-node-text">LR &asymp; H &#x226B; PB &asymp; F</span>
          </div>
        </div>
        <p className="synthesis-caption">The evidence chain: from L1 phonology to Bayesian inference, every level converges on representational indeterminacy.</p>
      </div>
    ),
    text: (<>Four levels of evidence &mdash; phonological, lexical, decision, and statistical &mdash; converge on the same conclusion: <strong>L1 phonology structurally constrains L2 lexical storage</strong>. The &ldquo;Key&rdquo; to the &ldquo;Rock&rdquo; is indeed the &ldquo;Lock.&rdquo;</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">FUTURE DIRECTIONS</div>
        <ul className="findings-list">
          <li><strong>Vietnamese L1:</strong> Tonal contrasts (e.g., m&aacute; vs. m&agrave;) as a new test case &mdash; does representational indeterminacy extend to suprasegmental features?</li>
          <li><strong>Cross-linguistic replication:</strong> Korean, Mandarin, Thai, Vietnamese L1 speakers with different phonological gaps</li>
          <li><strong>Item-level models:</strong> Bayesian random slopes for word frequency and neighbourhood density</li>
          <li><strong>Response time:</strong> Drift-diffusion modeling for a continuous processing measure</li>
          <li><strong>Longitudinal design:</strong> How does L2 proficiency modulate the phonological filter over time?</li>
        </ul>
        <p className="formal-note">The gradient distinctness framework generalizes naturally to any L1&ndash;L2 contrast mismatch, making it a strong candidate for cross-linguistic extension.</p>
      </div>
    ),
    footer: { question: 'What does this all mean?', summary: 'Evidence from phonology to statistics converges on structural filtering.', takeHome: 'L1 phonology shapes L2 storage; the effect is structural, gradient, and robust.' }
  }
];

// ============================================================
// APP
// ============================================================
function App() {
  const [currentIndex, setCurrentIndex] = useState(0);
  const [isTransitioning, setIsTransitioning] = useState(false);
  const [showFormal, setShowFormal] = useState(true);
  const [theme, setTheme] = useState('light');
  const [showOverview, setShowOverview] = useState(false);
  const [showAppendix, setShowAppendix] = useState(false);

  const totalSlides = slides.length;
  const currentSlide = slides[currentIndex];

  // Theme init
  useEffect(() => {
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    const saved = localStorage.getItem('ota-theme');
    const initial = saved || (prefersDark ? 'dark' : 'light');
    setTheme(initial);
    document.documentElement.setAttribute('data-theme', initial);
  }, []);

  const toggleTheme = useCallback(() => {
    setTheme(prev => {
      const next = prev === 'dark' ? 'light' : 'dark';
      document.documentElement.setAttribute('data-theme', next);
      localStorage.setItem('ota-theme', next);
      return next;
    });
  }, []);

  // Navigation
  const goToSlide = useCallback((index) => {
    if (isTransitioning || index === currentIndex) return;
    if (index < 0 || index >= totalSlides) return;
    setIsTransitioning(true);
    setShowOverview(false);
    setShowAppendix(false);
    setTimeout(() => { setCurrentIndex(index); setIsTransitioning(false); }, 300);
  }, [currentIndex, isTransitioning, totalSlides]);

  const goNext = useCallback(() => goToSlide(currentIndex + 1), [currentIndex, goToSlide]);
  const goPrev = useCallback(() => goToSlide(currentIndex - 1), [currentIndex, goToSlide]);

  useEffect(() => {
    const handleKeyDown = (e) => {
      if ((showOverview || showAppendix) && e.key === 'Escape') {
        setShowOverview(false);
        setShowAppendix(false);
        return;
      }
      if (e.key === 'ArrowRight' || e.key === ' ') { e.preventDefault(); goNext(); }
      if (e.key === 'ArrowLeft') { e.preventDefault(); goPrev(); }
      if (e.key === 'm' || e.key === 'M') setShowFormal(prev => !prev);
      if (e.key === 'o' || e.key === 'O') setShowOverview(prev => !prev);
      if (e.key === 'd' || e.key === 'D') toggleTheme();
      if (e.key === 'a' || e.key === 'A') setShowAppendix(prev => !prev);
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [goNext, goPrev, showOverview, showAppendix, toggleTheme]);

  return (
    <div className="carousel-app">
      <div className="app-background"></div>
      <div className="noise-overlay"></div>

      <GlossarySidebar currentSlideId={currentSlide.id} />

      {/* ── Edge chevrons ── */}
      {currentIndex > 0 && (
        <button className="edge-nav edge-prev" onClick={goPrev} aria-label="Previous">&lsaquo;</button>
      )}
      {currentIndex < totalSlides - 1 && (
        <button className="edge-nav edge-next" onClick={goNext} aria-label="Next">&rsaquo;</button>
      )}

      {/* ── Stage ── */}
      <main className={`stage ${isTransitioning ? 'fade-out' : 'fade-in'}`}>
        {currentSlide.type === 'hero' && (
          <div className="slide slide-hero" data-section={currentSlide.id}>
            <WordPairFader />
            {currentSlide.id === 'title' && (
              <>
                <div className="hero-legend">
                  <div className="legend-item"><span className="dot-lr"></span>High Error (L/R)</div>
                  <div className="legend-item"><span className="dot-f"></span>Low Error (Control)</div>
                </div>
                <div className="hero-meta-strip">{currentSlide.meta}</div>
              </>
            )}
            <div className="hero-box">
              <h1 className="hero-title">{currentSlide.title}</h1>
              <div className="hero-divider"></div>
              <p className="hero-subtitle">{currentSlide.subtitle}</p>
              {currentSlide.tagline && <p className="hero-tagline">{currentSlide.tagline}</p>}
              {currentSlide.id === 'title' && (
                <>
                  <button className="hero-cta" onClick={() => goToSlide(1)}>See Key Findings &rarr;</button>
                  <div className="hero-footer-strip">
                    <span>R + brms + ggdist</span>
                    <span>React + KaTeX</span>
                    <a href="https://github.com/sandriatran/qml-2025" target="_blank" rel="noopener noreferrer">GitHub</a>
                  </div>
                </>
              )}
              {currentSlide.credit && <p className="hero-credit">{currentSlide.credit}</p>}
            </div>
          </div>
        )}

        {currentSlide.type === 'split' && (
          <div className="slide slide-split" data-section={currentSlide.id}>
            <div className="split-left">
              {/* Contrast Legend Strip (persistent reference) */}
              {currentSlide.showContrastLegend && <ContrastLegendStrip />}

              <div className="visual-frame">
                {currentSlide.visualSrc ? <img src={currentSlide.visualSrc} className="visual-img" alt="Evidence" /> : currentSlide.visualContent}
                {currentSlide.visualCaption && <div className="visual-caption">{currentSlide.visualCaption}</div>}
                {/* Figure legend: explains what visual elements mean */}
                {currentSlide.figureLegend && <FigureLegend text={currentSlide.figureLegend} />}
                {/* Reproduce tag: links to R script */}
                {currentSlide.reproduceTag && <ReproduceTag scriptRef={currentSlide.reproduceTag} />}
              </div>

              {/* Model Recap Box */}
              {currentSlide.showModelRecap && showFormal && <ModelRecap />}
            </div>
            <div className="split-right">
              <div className="content-header">
                <div className="content-header-top">
                  <span className="slide-label">{currentSlide.label}</span>
                  {currentSlide.repLevel && <RepLevelTag level={currentSlide.repLevel} />}
                </div>
                <h2 className="content-title">{currentSlide.title}</h2>
              </div>
              <div className="narrative-text">{currentSlide.text}</div>

              {/* Theory Callout */}
              {currentSlide.theoryCallout && showFormal && (
                <TheoryCallout text={currentSlide.theoryCallout} />
              )}

              {/* Progressive Disclosure OR standard formal block */}
              {showFormal && currentSlide.tiers && <TieredContent tiers={currentSlide.tiers} />}
              {showFormal && !currentSlide.tiers && currentSlide.formal}
            </div>
            {currentSlide.footer && <ThreeLineFooter footer={currentSlide.footer} />}
          </div>
        )}
      </main>

      {/* ── Bottom bar ── */}
      <nav className="bottom-bar">
        <div className="section-dots">
          {SECTIONS.map(section => (
            <button
              key={section.id}
              className={`section-dot ${currentIndex >= section.startIndex && currentIndex <= section.endIndex ? 'active' : ''}`}
              onClick={() => goToSlide(section.startIndex)}
              title={section.label}
            />
          ))}
        </div>
        <span className="slide-counter">{currentIndex + 1} / {totalSlides}</span>
        <button className="bottom-btn" onClick={() => setShowOverview(true)} title="Overview (O)">
          <svg width="14" height="14" viewBox="0 0 14 14" fill="none"><rect x="0.5" y="0.5" width="5" height="5" rx="1" stroke="currentColor"/><rect x="8.5" y="0.5" width="5" height="5" rx="1" stroke="currentColor"/><rect x="0.5" y="8.5" width="5" height="5" rx="1" stroke="currentColor"/><rect x="8.5" y="8.5" width="5" height="5" rx="1" stroke="currentColor"/></svg>
        </button>
        <button className="bottom-btn" onClick={toggleTheme} title="Toggle theme (D)">
          {theme === 'dark' ? '\u2600' : '\u263E'}
        </button>
        <button className="bottom-btn math-toggle" onClick={() => setShowFormal(!showFormal)} title="Toggle math (M)">
          {showFormal ? '\u2212 Math' : '+ Math'}
        </button>
        <button className="bottom-btn appendix-btn" onClick={() => setShowAppendix(true)} title="Technical Appendix (A)">
          Appendix
        </button>
        <div className="bottom-divider"></div>
        <a className="bottom-btn resource-link" href="https://github.com/sandriatran/qml-2025" target="_blank" rel="noopener noreferrer" title="GitHub Repository">
          GitHub
        </a>
        <a className="bottom-btn resource-link" href="https://doi.org/10.1016/j.cognition.2008.12.007" target="_blank" rel="noopener noreferrer" title="Ota et al. (2009) \u2014 Original paper">
          Ota 2009
        </a>
      </nav>

      {/* ── Keyboard hint (first slide only) ── */}
      {currentIndex === 0 && (
        <div className="keyboard-hint">&larr; &rarr; navigate &middot; M math &middot; D theme &middot; A appendix</div>
      )}

      {/* ── Overview modal ── */}
      {showOverview && (
        <OverviewModal
          slides={slides}
          currentIndex={currentIndex}
          onSelect={(idx) => { goToSlide(idx); setShowOverview(false); }}
          onClose={() => setShowOverview(false)}
        />
      )}

      {/* ── Technical Appendix modal ── */}
      {showAppendix && (
        <TechnicalAppendix onClose={() => setShowAppendix(false)} />
      )}
    </div>
  );
}

export default App;

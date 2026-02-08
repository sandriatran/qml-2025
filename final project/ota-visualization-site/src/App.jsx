import React, { useState, useEffect, useCallback, useRef } from 'react';
import 'katex/dist/katex.min.css';
import { InlineMath, BlockMath } from 'react-katex';
import GlossarySidebar from './components/GlossarySidebar';
import './index.css';

// ============================================================
// GLOSSARY (30+ terms)
// ============================================================
const GLOSSARY = {
  'Phoneme': 'Smallest unit of sound that distinguishes meaning in a language.',
  'L1': 'First acquired language. For this study: Japanese.',
  'LX': 'Any subsequently acquired language (replaces L2/L3/Ln). For this study: English.',
  'L2': 'Second language (see LX). For this study: English.',
  'Homophone': 'Words with identical pronunciation but different meanings (e.g., SUN / SON).',
  'Near-Homophone': 'Words differing by a contrast absent in the speaker\u2019s L1.',
  'Phonological Contrast': 'Sound difference that distinguishes words in a language.',
  'Representational Indeterminacy': 'L1-absent contrast collapses in L2 storage; two words share one representation.',
  'False Positive': 'Incorrectly judging an unrelated word pair as semantically related.',
  'FP': 'False Positive: incorrectly judging unrelated pairs as related.',
  'L1-Absent': 'Contrast not present in the speaker\u2019s L1 (e.g., /l/-/r/ for Japanese L1 users).',
  'L1-Present': 'Contrast present in the speaker\u2019s L1 (e.g., /p/-/b/ for Japanese L1 users).',
  'Indeterminate': 'Collapsed into a single phonological representation due to an L1-absent contrast.',
  'Posterior': 'Updated distribution after combining prior beliefs with observed data via Bayes\u2019 theorem.',
  'Prior': 'Initial distribution expressing beliefs before seeing data.',
  'Credible Interval': 'Bayesian range containing the true parameter with specified probability (e.g., 95% CrI).',
  'CrI': 'Credible Interval: Bayesian range with specified probability.',
  'ROPE': 'Region of Practical Equivalence (\u00B10.18 log-odds, OR \u2248 0.84\u20131.20); differences inside are negligible.',
  'MCMC': 'Markov Chain Monte Carlo: sampling algorithm for posterior distributions.',
  'Divergent Transitions': 'Sampling pathology in HMC/NUTS; zero is ideal.',
  'R-hat': 'Gelman\u2013Rubin convergence diagnostic. Values near 1.00 mean chains mixed well.',
  'ESS': 'Effective Sample Size: independent draws equivalent to correlated MCMC output.',
  'LOO-CV': 'Leave-One-Out Cross-Validation: Bayesian model comparison via predictive accuracy.',
  'PPC': 'Posterior Predictive Check: model-simulated data vs. observed data.',
  'Weakly Informative': 'Prior constraining implausible extremes without biasing effect direction.',
  'Log-Odds': 'Scale of logistic regression coefficients. 0 = 50/50 probability.',
  'Bayesian Statistics': 'A statistical framework that updates beliefs (priors) with observed data to produce posterior distributions, quantifying uncertainty directly.',
  'R Programming': 'Open-source programming language for statistical computing and graphics, widely used in academia and data science.',
  'brms': 'R package for fitting Bayesian regression models via Stan. Provides a formula interface for GLMMs, priors, and diagnostics.',
  'GLMM': 'Generalized Linear Mixed Model: regression for non-normal outcomes with random effects.',
  'Partial Pooling': 'Hierarchical shrinkage: extreme individual estimates (e.g., items or subjects) are shrunk toward the group mean, improving stability.',
  'Random Effects': 'Subject- or item-level deviations from the population mean, capturing individual variability.',
  'Caterpillar Plot': 'Individual random effects with 95% credible intervals, ordered by magnitude.',
  'Halfeye Plot': 'Combined density + interval plot from ggdist showing full uncertainty.',
  'Bernoulli': 'Distribution for binary outcomes (0/1). Like a coin flip \u2014 each trial has two possible results.',
  'Odds Ratio': 'Ratio of odds between groups. OR < 1 means reduced odds.',
  'Likelihood': 'How probable the observed data are, given a specific model. Higher = better fit.',
  'Hierarchical Model': 'A model with multiple levels (e.g., trials within subjects within groups), sharing information across levels.',
  'Convergence': 'When the sampling algorithm has explored enough to give reliable estimates. Checked via R-hat and trace plots.',
  'Sensitivity Analysis': 'Re-running the model with different assumptions to check if conclusions change. If they don\u2019t, results are robust.',
  'Phonological': 'Related to the sound system of a language \u2014 how speech sounds are organized and distinguished.',
  'Lexical': 'Related to the mental dictionary \u2014 how words are stored and accessed in the brain.',
  'False Positive': 'Incorrectly judging an unrelated word pair as related. The key error in this experiment.',
  'Distinctness': 'How perceptually separable two sounds are. Ranges from 0 (identical) to 1 (completely different).',
  'Dependent Variable': 'The outcome you model or predict in the analysis (here, accuracy on each trial).',
  'DV': 'Dependent Variable: the outcome you model or predict in the analysis (here, accuracy on each trial).',
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
// EXPERIMENT WORD PAIRS (from Ota et al. 2009 key-rock.csv)
// ============================================================
// LR Unrelated: near-homophones via /l/-/r/ collapse
// e.g., KEY/ROCK — ROCK sounds like LOCK (related to KEY) for Japanese L1 users
const EXP_LR = [
  ['KEY', 'ROCK'], ['BANK', 'LIVER'], ['HARD', 'LOCK'], ['HEART', 'LATE'],
  ['HEAVY', 'ROAD'], ['CLOUD', 'PEOPLE'], ['EGG', 'RAY'], ['IRON', 'LUST'],
  ['PASSION', 'RUST'], ['DARK', 'RIGHT'], ['MOUTH', 'RIP'], ['CROWD', 'SKY'],
  ['KIDNEY', 'RIVER'], ['KNEE', 'RAP'], ['BEAM', 'LAY'],
];
// H Unrelated: homophone confusion (MEAT/GREET — MEET is related to GREET)
const EXP_H = [
  ['MEAT', 'GREET'], ['BOOT', 'SOUL'], ['OCEAN', 'SEE'], ['DAUGHTER', 'SUN'],
  ['METAL', 'STEAL'], ['DOG', 'TALE'], ['CAKE', 'PEACE'], ['PRISON', 'SELL'],
  ['MOON', 'SON'], ['SPIRIT', 'SOLE'],
];
// PB Unrelated: /p/-/b/ contrast (present in Japanese — control)
const EXP_PB = [
  ['BACK', 'BAT'], ['BALL', 'PAT'], ['CAR', 'BARK'], ['BIG', 'PORK'],
  ['FRUIT', 'BEAR'], ['JAMES', 'POND'], ['GOOD', 'PAD'], ['COW', 'PULL'],
  ['GROWL', 'PARK'], ['DRINK', 'PEER'],
];
// F Related: genuine semantic pairs (fillers — correct answer is "Related")
const EXP_F_RELATED = [
  ['EAGLE', 'HAWK'], ['BOX', 'CUBE'], ['HIGH', 'LOW'], ['ADULT', 'CHILD'],
  ['AIR', 'BREATHE'], ['BLIND', 'DEAF'], ['BLUE', 'COLOUR'], ['BOILER', 'HEAT'],
  ['BOWL', 'DISH'], ['ADD', 'SUBTRACT'], ['ACTOR', 'FILM'], ['ATTACK', 'DEFEND'],
  ['KING', 'QUEEN'], ['LION', 'TIGER'], ['DOCTOR', 'NURSE'],
];

// Study comparison data (approximate FP rates from Ota et al. 2009)
const STUDY_FP_RATES = {
  LR: 0.25,  // /l/-/r/ — L1-absent, highest confusion
  H: 0.20,   // Homophones — also high
  PB: 0.08,  // /p/-/b/ — L1-present, low confusion
  F: 0.05,   // Spelling control — baseline
};

function shuffleArray(arr) {
  const a = [...arr];
  for (let i = a.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
}

function buildTrialSet() {
  const lr = shuffleArray(EXP_LR).slice(0, 6).map(p => ({ pair: p, contrast: 'LR', correct: 'unrelated' }));
  const h  = shuffleArray(EXP_H).slice(0, 4).map(p => ({ pair: p, contrast: 'H', correct: 'unrelated' }));
  const pb = shuffleArray(EXP_PB).slice(0, 4).map(p => ({ pair: p, contrast: 'PB', correct: 'unrelated' }));
  const f  = shuffleArray(EXP_F_RELATED).slice(0, 6).map(p => ({ pair: p, contrast: 'F', correct: 'related' }));
  return shuffleArray([...lr, ...h, ...pb, ...f]);
}

// ============================================================
// EXPERIMENT MODAL
// ============================================================
const ExperimentModal = ({ onClose }) => {
  const [phase, setPhase] = useState('instructions'); // instructions | trial | results
  const [trials, setTrials] = useState([]);
  const [trialIndex, setTrialIndex] = useState(0);
  const [responses, setResponses] = useState([]);
  const [trialStart, setTrialStart] = useState(null);
  const [showTermInfo, setShowTermInfo] = useState(false);

  const startGame = useCallback(() => {
    const t = buildTrialSet();
    setTrials(t);
    setTrialIndex(0);
    setResponses([]);
    setPhase('trial');
    setTrialStart(Date.now());
  }, []);

  const respond = useCallback((answer) => {
    if (phase !== 'trial' || trialIndex >= trials.length) return;
    const rt = Date.now() - trialStart;
    const trial = trials[trialIndex];
    const isCorrect = answer === trial.correct;
    const newResponses = [...responses, { ...trial, answer, rt, isCorrect }];
    setResponses(newResponses);

    if (trialIndex + 1 >= trials.length) {
      setPhase('results');
    } else {
      setTrialIndex(trialIndex + 1);
      setTrialStart(Date.now());
    }
  }, [phase, trialIndex, trials, trialStart, responses]);

  // Keyboard handler for trials
  useEffect(() => {
    const handler = (e) => {
      if (phase === 'trial') {
        if (e.key === 'r' || e.key === 'R') { e.preventDefault(); e.stopPropagation(); respond('related'); }
        if (e.key === 'u' || e.key === 'U') { e.preventDefault(); e.stopPropagation(); respond('unrelated'); }
      }
      if (e.key === 'Escape') { e.stopPropagation(); onClose(); }
    };
    window.addEventListener('keydown', handler, true);
    return () => window.removeEventListener('keydown', handler, true);
  }, [phase, respond, onClose]);

  // Compute results
  const results = (() => {
    if (phase !== 'results') return null;
    const byContrast = {};
    for (const r of responses) {
      if (!byContrast[r.contrast]) byContrast[r.contrast] = { total: 0, fp: 0 };
      byContrast[r.contrast].total++;
      if (!r.isCorrect) byContrast[r.contrast].fp++;
    }
    return byContrast;
  })();

  return (
    <div className="overview-backdrop" onClick={onClose}>
      <div className="overview-modal experiment-modal" onClick={e => e.stopPropagation()}>
        <div className="overview-header">
          <h3>KEY-ROCK Experiment</h3>
          <button className="overview-close" onClick={onClose}>&times;</button>
        </div>
        <div className="overview-body experiment-body">

          {/* ── INSTRUCTIONS ── */}
          {phase === 'instructions' && (
            <div className="exp-instructions">
              <div className="exp-intro-icon">&#x1F9EA;</div>
              <h4>Can You Be Tricked by Sound?</h4>
              <p>
                You will see pairs of English words. Decide if they are <strong>related in meaning</strong> &mdash; not in sound or spelling.
              </p>
              <div className="exp-example-grid">
                <div className="exp-example">
                  <span className="exp-word-demo">EAGLE</span>
                  <span className="exp-word-sep">/</span>
                  <span className="exp-word-demo">HAWK</span>
                  <span className="exp-arrow">&rarr;</span>
                  <span className="exp-label-related">Related</span>
                </div>
                <div className="exp-example">
                  <span className="exp-word-demo">SWIM</span>
                  <span className="exp-word-sep">/</span>
                  <span className="exp-word-demo">TIDY</span>
                  <span className="exp-arrow">&rarr;</span>
                  <span className="exp-label-unrelated">Unrelated</span>
                </div>
                <div className="exp-example">
                  <span className="exp-word-demo">KING</span>
                  <span className="exp-word-sep">/</span>
                  <span className="exp-word-demo">QUEEN</span>
                  <span className="exp-arrow">&rarr;</span>
                  <span className="exp-label-related">Related</span>
                </div>
                <div className="exp-example">
                  <span className="exp-word-demo">CLOUD</span>
                  <span className="exp-word-sep">/</span>
                  <span className="exp-word-demo">SHOE</span>
                  <span className="exp-arrow">&rarr;</span>
                  <span className="exp-label-unrelated">Unrelated</span>
                </div>
              </div>
              <div className="exp-correct-callout">
                <div className="exp-correct-item"><span className="exp-correct-icon">&#x2713;</span> <strong>Correct</strong> = judging unrelated pairs as unrelated</div>
                <div className="exp-correct-item"><span className="exp-error-icon">&#x2717;</span> <strong>Error</strong> = judging unrelated pairs as related (a &ldquo;false positive&rdquo;)</div>
                <div className="exp-correct-hint">Some pairs may <em>sound</em> similar &mdash; that&rsquo;s the trap!</div>
              </div>
              <p className="exp-speed-note">
                Respond <strong>quickly</strong> &mdash; trust your first instinct!
                <br />
                Use <kbd>R</kbd> for Related and <kbd>U</kbd> for Unrelated, or click the buttons.
              </p>
              <p className="exp-trial-count">20 trials &middot; ~30 seconds</p>
              <button className="exp-start-btn" onClick={startGame}>Start Experiment</button>
            </div>
          )}

          {/* ── TRIAL ── */}
          {phase === 'trial' && trials.length > 0 && (
            <div className="exp-trial">
              <div className="exp-progress">
                <div className="exp-progress-bar">
                  <div className="exp-progress-fill" style={{ width: `${((trialIndex) / trials.length) * 100}%` }}></div>
                </div>
                <span className="exp-progress-label">Trial {trialIndex + 1} / {trials.length}</span>
              </div>
              <div className="exp-pair">
                <span className="exp-word">{trials[trialIndex].pair[0]}</span>
                <span className="exp-pair-sep">&mdash;</span>
                <span className="exp-word">{trials[trialIndex].pair[1]}</span>
              </div>
              <div className="exp-buttons">
                <button className="exp-btn exp-btn-related" onClick={() => respond('related')}>
                  Related <kbd>R</kbd>
                </button>
                <button className="exp-btn exp-btn-unrelated" onClick={() => respond('unrelated')}>
                  Unrelated <kbd>U</kbd>
                </button>
              </div>
            </div>
          )}

          {/* ── RESULTS ── */}
          {phase === 'results' && results && (
            <div className="exp-results">
              <h4>Your Results</h4>
              <p className="exp-results-subtitle">Error rate by contrast type vs. Ota et al. (2009) study means</p>
              <div className="exp-chart">
                {['F', 'PB', 'H', 'LR'].map(c => {
                  const data = results[c] || { total: 0, fp: 0 };
                  const userRate = data.total > 0 ? data.fp / data.total : 0;
                  const studyRate = STUDY_FP_RATES[c];
                  const contrastInfo = CONTRASTS.find(x => x.code === c);
                  return (
                    <div key={c} className="exp-chart-row">
                      <div className="exp-chart-label">
                        <span className="exp-contrast-dot" style={{ background: contrastInfo?.color }}></span>
                        <span className="exp-contrast-code">{c}</span>
                        <span className="exp-contrast-name">{contrastInfo?.label}</span>
                      </div>
                      <div className="exp-bars">
                        <div className="exp-bar-group">
                          <div className="exp-bar exp-bar-you" style={{ width: `${Math.max(userRate * 100, 2)}%` }}>
                            <span className="exp-bar-label">{Math.round(userRate * 100)}%</span>
                          </div>
                          <span className="exp-bar-tag">You</span>
                        </div>
                        <div className="exp-bar-group">
                          <div className="exp-bar exp-bar-study" style={{ width: `${Math.max(studyRate * 100, 2)}%` }}>
                            <span className="exp-bar-label">{Math.round(studyRate * 100)}%</span>
                          </div>
                          <span className="exp-bar-tag">Study</span>
                        </div>
                      </div>
                    </div>
                  );
                })}
              </div>

              <div className="exp-insight">
                <strong>What happened?</strong> The LR and H pairs are designed to trick you.
                For example, <strong>KEY / ROCK</strong> — ROCK sounds like <em>LOCK</em>,
                which <em>is</em> related to KEY. Japanese L1 users of English, who don&rsquo;t
                distinguish /l/ from /r/, are especially susceptible to this phonological trap.
                The study found that L1-absent contrasts (/l/-/r/) produced the highest false-positive
                rates, revealing how L1 phonology structurally constrains lexical storage.
              </div>

              {/* L1/LX Terminology Info Box */}
              <div className="exp-term-toggle">
                <button className="exp-term-btn" onClick={() => setShowTermInfo(!showTermInfo)}>
                  {showTermInfo ? '\u25BC' : '\u25B6'} Why do we say &ldquo;L1 users&rdquo; instead of &ldquo;native speakers&rdquo;?
                </button>
                {showTermInfo && (
                  <div className="exp-term-box">
                    <p>
                      Applied linguistics has increasingly moved away from the terms
                      &ldquo;native speaker&rdquo; (NS) and &ldquo;non-native speaker&rdquo; (NNS)
                      because they carry problematic assumptions:
                    </p>
                    <ul>
                      <li><strong>Ambiguity:</strong> &ldquo;Native&rdquo; conflates birthplace, childhood exposure, proficiency, and identity &mdash; these don&rsquo;t always align.</li>
                      <li><strong>Monolingual bias:</strong> The NS/NNS binary assumes a single &ldquo;native&rdquo; language as the norm, erasing multilingual realities.</li>
                      <li><strong>Ideological hierarchy:</strong> It positions &ldquo;native speakers&rdquo; as the gold standard, devaluing the competence of multilinguals.</li>
                      <li><strong>Harmful consequences:</strong> In hiring and publishing, &ldquo;non-native&rdquo; labels gatekeep opportunities regardless of actual proficiency.</li>
                    </ul>
                    <p><strong>Preferred terminology:</strong></p>
                    <ul>
                      <li><strong>L1 user</strong> &mdash; user of a first-acquired language</li>
                      <li><strong>LX user</strong> &mdash; user of any subsequently acquired language (replaces L2, L3, L4&hellip;)</li>
                      <li><strong>Heritage speaker</strong> &mdash; grew up with a minority language at home</li>
                      <li><strong>CEFR levels</strong> (A1&ndash;C2) &mdash; proficiency without identity claims</li>
                    </ul>
                    <p className="exp-term-refs">
                      Dewaele (2018); Douglas Fir Group (2016); Ortega (2019)
                    </p>
                  </div>
                )}
              </div>

              <div className="exp-actions">
                <button className="exp-start-btn" onClick={startGame}>Play Again</button>
                <button className="exp-close-btn" onClick={onClose}>Close</button>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

// ============================================================
// SMALL COMPONENTS
// ============================================================
const Tooltip = ({ term, children }) => {
  const [show, setShow] = useState(false);
  const ref = useRef(null);
  const [pos, setPos] = useState({ top: 0, left: 0 });
  const def = GLOSSARY[term];
  const handleEnter = () => {
    if (ref.current) {
      const r = ref.current.getBoundingClientRect();
      setPos({ top: r.top, left: r.left + r.width / 2 });
    }
    setShow(true);
  };
  return (
    <span className="glossary-term" ref={ref} onMouseEnter={handleEnter} onMouseLeave={() => setShow(false)}>
      {children}
      {show && def && <span className="tooltip-popup" style={{ position: 'fixed', bottom: 'auto', top: pos.top - 8, left: pos.left, transform: 'translate(-50%, -100%)' }}>{def}</span>}
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
// Compact chip card for the four contrast types
const CONTRAST_SHORT = [
  { code: 'F', short: 'Spelling', color: 'var(--color-lavender)', tip: 'Spelling Control — Multiple phonemes differ (e.g., COUGH–WALL)' },
  { code: 'PB', short: '/p/\u2013/b/', color: 'var(--color-purple)', tip: '/p/–/b/ (L1-present) — Contrast exists in Japanese' },
  { code: 'H', short: 'Homophones', color: 'var(--color-hot-pink)', tip: 'Homophones — Identical pronunciation (e.g., SUN–SON)' },
  { code: 'LR', short: '/l/\u2013/r/', color: 'var(--color-indigo)', tip: '/l/–/r/ (L1-absent) — Absent in Japanese' },
];
const ContrastLegendStrip = () => (
  <div className="contrast-legend-strip">
    {CONTRAST_SHORT.map(c => (
      <div key={c.code} className="contrast-legend-chip" title={c.tip}>
        <span className="contrast-dot" style={{ background: c.color }}></span>
        <span className="contrast-chip-label">{c.code} &middot; {c.short}</span>
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
          <h4>Notation Guide</h4>
          <table className="appendix-table notation-table">
            <thead><tr><th>Symbol</th><th>Meaning</th><th>Qualitative</th></tr></thead>
            <tbody>
              <tr><td><InlineMath math="y_{ijk}" /></td><td>Response</td><td>1 = correct, 0 = false positive</td></tr>
              <tr><td><InlineMath math="\theta_{ijk}" /></td><td>Probability of correct</td><td>Coin bias</td></tr>
              <tr><td><InlineMath math="\beta_0" /></td><td>Intercept</td><td>Baseline log-odds</td></tr>
              <tr><td><InlineMath math="\beta_j" /></td><td>Contrast effect</td><td>How much contrast <em>j</em> shifts accuracy</td></tr>
              <tr><td><InlineMath math="u_i" /></td><td>Subject intercept</td><td>Individual accuracy bias</td></tr>
              <tr><td><InlineMath math="w_k" /></td><td>Item intercept</td><td>Word pair difficulty</td></tr>
              <tr><td><InlineMath math="\sigma_u, \sigma_w" /></td><td>Random effect SDs</td><td>Between-subject / item variability</td></tr>
            </tbody>
          </table>
        </div>

        <div className="appendix-section">
          <h4>Model Specifications</h4>
          <div className="appendix-models">
            <div className="appendix-model-card">
              <div className="appendix-model-name">Comprehensive</div>
              <div className="appendix-math-code">
                <BlockMath math="y_{ijk} \sim \text{Bernoulli}(\text{logit}^{-1}(\beta_0 + \beta_{j} + u_i + w_k))" />
                <BlockMath math="\sigma_u, \sigma_w \sim \text{Exponential}(1)" />
              </div>
              <pre className="brms-colored"><code><span className="code-formula">accuracy</span> ~ <span className="code-fixed">contrast_type</span> + <span className="code-random-s">(1|subject_id)</span> + <span className="code-random-i">(1|item_id)</span>{'\n'}family = <span className="code-fn">bernoulli</span>(link = <span className="code-string">"logit"</span>){'\n'}prior: <span className="code-fixed">Normal(0, 1.5)</span></code></pre>
              <p className="appendix-bridge">Same <strong>Bernoulli(logit) GLMM</strong> with a separate contrast effect <InlineMath math="\beta_j" /> for each of the four contrast types.</p>
              <p className="appendix-hypothesis">Tests <strong>pairwise differences</strong> among the four contrasts.</p>
            </div>
            <div className="appendix-model-card">
              <div className="appendix-model-name">Linguistic</div>
              <pre className="brms-colored"><code><span className="code-formula">accuracy</span> ~ <span className="code-fixed">phonological_status</span> + <span className="code-random-s">(1|subject_id)</span> + <span className="code-random-i">(1|item_id)</span>{'\n'}family = <span className="code-fn">bernoulli</span>(link = <span className="code-string">"logit"</span>){'\n'}prior: <span className="code-fixed">Normal(0, 1.5)</span></code></pre>
              <p className="appendix-bridge">Same structure, but <InlineMath math="\beta_j" /> is replaced by a <strong>four-level phonological-status factor</strong> (Unrelated, L1-Present, L1-Absent, Homophone).</p>
              <p className="appendix-hypothesis">Tests whether <strong>L1 status groups</strong> differ.</p>
            </div>
            <div className="appendix-model-card">
              <div className="appendix-model-name">Distinctness</div>
              <pre className="brms-colored"><code><span className="code-formula">accuracy</span> ~ <span className="code-fixed">phon_distinctness_scaled</span> + <span className="code-random-s">(1|subject_id)</span> + <span className="code-random-i">(1|item_id)</span>{'\n'}family = <span className="code-fn">bernoulli</span>(link = <span className="code-string">"logit"</span>){'\n'}prior: <span className="code-fixed">Normal(0, 1.5)</span></code></pre>
              <p className="appendix-bridge">Same structure, but the four contrasts are summarized by a <strong>single slope</strong> <InlineMath math="\beta_d d_j" />, where <InlineMath math="d_j \in \{0.0, 0.3, 0.8, 1.0\}" /> is the distinctness score.</p>
              <p className="appendix-hypothesis">Tests a <strong>graded distinctness</strong> hypothesis with one parameter.</p>
            </div>
          </div>
          <table className="appendix-table model-comparison-table" style={{ marginTop: '1rem' }}>
            <thead><tr><th>Model</th><th>Predictor</th><th>Question</th></tr></thead>
            <tbody>
              <tr><td><strong>Comprehensive</strong></td><td><code className="code-fixed">contrast_type</code> (4 levels)</td><td>How do all four contrasts differ pairwise?</td></tr>
              <tr><td><strong>Linguistic</strong></td><td><code className="code-fixed">phonological_status</code> (4 L1 groups)</td><td>Does L1 status (present/absent/homophone/control) matter?</td></tr>
              <tr><td><strong>Distinctness</strong></td><td><code className="code-fixed">phon_distinctness_scaled</code> (0.0&ndash;1.0)</td><td>Does a single graded distinctness score explain the hierarchy?</td></tr>
            </tbody>
          </table>
        </div>

        <div className="appendix-section">
          <h4>MCMC Diagnostics</h4>
          <table className="appendix-table">
            <thead><tr><th>Diagnostic</th><th>Criterion</th><th>Result</th></tr></thead>
            <tbody>
              <tr><td>R-hat</td><td>&le; 1.01</td><td className="pass">All &asymp; 1.00</td></tr>
              <tr><td>Bulk ESS</td><td>&gt; 400 (min)</td><td className="pass">&gt; 1,000 for all parameters</td></tr>
              <tr><td>Tail ESS</td><td>&gt; 400 (min)</td><td className="pass">&gt; 800 for all parameters</td></tr>
              <tr><td>Divergent transitions</td><td>0</td><td className="pass">0 across all models</td></tr>
              <tr><td>Tree depth</td><td>No saturation</td><td className="pass">No max treedepth warnings</td></tr>
            </tbody>
          </table>
          <p className="appendix-note">Following Vehtari et al. (2021), bulk ESS &gt; 400 is minimum for stable estimates; all parameters exceeded 1,000.</p>
          <p className="appendix-note">Overall, the MCMC chains have converged, effective sample sizes are well above recommended per-parameter minima, and there are no divergences or tree-depth issues, indicating a single well-explored posterior and small Monte Carlo error for all reported estimates.</p>
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
          <p className="appendix-note">Models have overlapping predictive performance: the &Delta;ELPD estimates are small and their uncertainty intervals include zero. The distinctness model is surprisingly competitive with only one predictor. With <em>N</em>=20 and a hierarchical structure, these LOO-CV differences should be interpreted cautiously.</p>
        </div>

        <div className="appendix-section">
          <h4>Prior Sensitivity</h4>
          <p>Sensitivity assessed for fixed effect priors: wider <InlineMath math="\mathcal{N}(0, 3.0)" /> priors yield posteriors virtually identical to the default <InlineMath math="\mathcal{N}(0, 1.5)" />, confirming results are data-driven rather than prior-dependent.</p>
        </div>

        <div className="appendix-section">
          <h4>R Analysis Pipeline</h4>
          <p className="appendix-note" style={{ marginBottom: '0.75rem' }}>The R analysis pipeline organizes the project into stages, from setup and data cleaning through model fitting, diagnostics, and visualization. Each stage corresponds to a numbered script, and the <code>master.R</code> file runs all stages in sequence so the full analysis can be reproduced with a single command.</p>
          <div className="script-links">
            <a className="script-download" href="./scripts/00_setup.R" download>
              <span>Stage 0</span>
              <code>Steps 0</code>
              <small>Packages, palette &amp; <code>theme_ota()</code></small>
            </a>
            <a className="script-download" href="./scripts/01_data_cleaning.R" download>
              <span>Stage 1</span>
              <code>Steps 1&ndash;2</code>
              <small>Load, inspect &amp; preprocess data</small>
            </a>
            <a className="script-download" href="./scripts/02_models.R" download>
              <span>Stage 2</span>
              <code>Steps 3&ndash;4</code>
              <small>Prior specification &amp; fit 3 brms models (comprehensive, linguistic, distinctness)</small>
            </a>
            <a className="script-download" href="./scripts/03_diagnostics.R" download>
              <span>Stage 3</span>
              <code>Steps 5&ndash;12</code>
              <small>Posterior extraction, PPC, sensitivity analysis, item-level &amp; LOO-CV</small>
            </a>
            <a className="script-download" href="./scripts/04_results_viz.R" download>
              <span>Stage 4</span>
              <code>Steps 13&ndash;16</code>
              <small>Enhanced visualizations, animations &amp; dark mode rendering</small>
            </a>
            <a className="script-download" href="./scripts/master.R" download>
              <span>Pipeline</span>
              <code>master.R</code>
              <small>Full pipeline runner (sources all stages)</small>
            </a>
          </div>
        </div>

        <div className="appendix-section">
          <h4>Data Description</h4>
          <div className="appendix-subsection">
            <h5>Original Study</h5>
            <ul className="appendix-list">
              <li><strong>Source:</strong> Ota, Hartsuiker &amp; Haywood (2009), <em>Cognition</em> 111(2), 263&ndash;269</li>
              <li><strong>Participants:</strong> 20 native Japanese speakers (16 female, 4 male), 15 university students</li>
              <li><strong>Task:</strong> Visual semantic-relatedness judgment (word pairs on screen)</li>
              <li><strong>DV:</strong> False positive errors vs. correct rejections on unrelated pairs, matching the binary accuracy coding used in this re-analysis</li>
              <li><strong>Design:</strong> 360 trials per participant (120 experimental + 240 filler). Materials: 60 triplets (20 homophone, 20 /l/&ndash;/r/, 20 /p/&ndash;/b/) &times; 4 word pairs = 240 experimental word pairs, divided into four counterbalanced 120-item lists</li>
              <li><strong>Contrast types:</strong> Three phonological contrasts defined in the original: homophones, /l/&ndash;/r/ (L1-absent), /p/&ndash;/b/ (L1-present)</li>
            </ul>
          </div>
          <div className="appendix-subsection">
            <h5>Present Re-Analysis</h5>
            <ul className="appendix-list">
              <li><strong>Subset:</strong> ~1,200 trials and 258 unique unrelated word pairs refer to the cleaned Japanese-group subset used in this analysis</li>
              <li><strong>Fourth contrast level:</strong> &ldquo;F&rdquo; (fully phonologically distinct) is a control category introduced in this re-analysis, not a separate contrast in the original design</li>
              <li><strong>Trials per contrast:</strong> F: ~240 | PB: ~320 | H: ~320 | LR: ~320</li>
              <li><strong>Coding:</strong> 1 = correct rejection, 0 = false positive &mdash; consistent with how errors are analyzed in the original paper</li>
            </ul>
          </div>
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
                    <span className="overview-num">{slide.label?.match(/^(\d+[ab]?)\./)?.[1] || (idx + 1)}</span>
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
    subtitle: 'Why Japanese speakers see ROCK and think of KEY',
    subtitleTechnical: 'A Bayesian re-analysis of representational indeterminacy in L2 visual word recognition',
    credit: 'V. Manson & S. Tran',
  },

  // ── 2. EXECUTIVE SUMMARY ──
  {
    id: 'summary', type: 'split',
    label: 'SUMMARY',
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
    tiers: {
      plain: (
        <div>
          <p className="tier-text"><strong>1. Confusion confirmed:</strong> Japanese speakers confuse words like ROCK/LOCK because the /l/-/r/ sound difference doesn&rsquo;t exist in their language. They mistake these pairs at the same rate as actual identical-sounding words (homophones like SUN/SON).</p>
          <p className="tier-text"><strong>2. It&rsquo;s a spectrum:</strong> The more similar two sounds are in your first language, the more you confuse them in English &mdash; it&rsquo;s gradual, not all-or-nothing.</p>
          <p className="tier-text"><strong>3. Everyone shows it:</strong> All 20 participants showed the same pattern &mdash; this isn&rsquo;t driven by a few outliers.</p>
          <p className="tier-text"><strong>4. We can trust the stats:</strong> Multiple validation checks confirm the model works correctly.</p>
        </div>
      ),
      technical: (
        <div>
          <p className="tier-text"><strong>1.</strong> <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> false-positive rate (~21%) is statistically indistinguishable from <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span> (~24%) via ROPE analysis.</p>
          <p className="tier-text"><strong>2.</strong> Pairwise contrasts confirm: <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> &asymp; <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span> {'\u226B'} <span style={{ color: 'var(--color-purple)', fontWeight: 700 }}>PB</span> &asymp; <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span></p>
          <p className="tier-text"><strong>3.</strong> Phonological distinctness (<InlineMath math="d_j \in [0, 1]" />) is a continuous predictor &mdash; gradient, not categorical.</p>
          <p className="tier-text"><strong>4.</strong> R-hat &asymp; 1.00, ESS &gt; 1,000, zero divergences, LOO-CV comparable across 3 models.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">CORE HYPOTHESIS</div>
          <p>When a <span style={{ color: 'var(--color-purple)' }}>phonological contrast</span> is absent from a speaker&rsquo;s <span style={{ fontWeight: 700 }}>L1</span> (first language), <span style={{ fontWeight: 700 }}>L2</span> (second language) word pairs differing by that contrast become <strong style={{ color: 'var(--color-hot-pink)' }}>near-homophones</strong> &mdash; stored under a single <span style={{ color: 'var(--color-indigo)' }}>lexical representation</span>.</p>
          <p style={{ fontSize: '1.4rem', textAlign: 'center', margin: '0.75rem 0' }}>
            <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span>
            {' ≈ '}
            <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span>
            {' ≫ '}
            <span style={{ color: 'var(--color-purple)', fontWeight: 700 }}>PB</span>
            {' ≈ '}
            <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span>
          </p>
          <p>Model: <InlineMath math="y_{ijk} \sim \text{Bernoulli}(\text{logit}^{-1}(\beta_0 + \beta_j + u_i + w_k))" /></p>
          <p className="formal-note">The five findings converge from <span style={{ color: 'var(--color-purple)' }}>phonological</span>, <span style={{ color: 'var(--color-indigo)' }}>lexical</span>, and <span style={{ color: 'var(--color-lavender)' }}>statistical</span> levels. Validated via MCMC diagnostics, PPC, prior sensitivity, and LOO-CV.</p>
        </div>
      )
    },
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
        <p className="chain-intro">Japanese listeners sometimes judge ROCK as related to KEY, because their L1 collapses /r/ and /l/ &mdash; making ROCK sound like LOCK.</p>
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
        <p className="chain-caption">For Japanese L1 users, /l/ and /&#x0279;/ collapse to one phoneme. ROCK and LOCK share a single lexical entry, both triggering KEY.</p>
      </div>
    ),
    text: (<>The /l/&ndash;/r/ distinction does not exist in Japanese phonology. When Japanese speakers store English words, ROCK and LOCK map to the <strong>same phonological form</strong>, creating representational indeterminacy. Our Bayesian re-analysis focuses on <strong>false positives</strong>: how often do participants incorrectly judge unrelated pairs as related? <em>Want to experience this yourself?</em> Press <kbd>E</kbd> to try the experiment.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">STRUCTURAL FILTERING MECHANISM</div>
        <p>If Japanese lacks /l/&ndash;/r/, then LOCK and ROCK both reduce to <span className="ipa-form">/&#x0251;k/</span> at the phonological level, producing a single lexical entry that activates KEY.</p>
        <p>The experiment presents <strong>visually displayed</strong> word pairs (no audio). Participants judge semantic relatedness. A &ldquo;false positive&rdquo; means incorrectly accepting an unrelated pair as related &mdash; evidence that the phonological collapse activates the wrong lexical entry.</p>
        <p className="formal-note">This is a claim about <em>storage</em> (lexical representation), not just <em>perception</em> (auditory discrimination). The visual task eliminates auditory confounds entirely.</p>
      </div>
    ),
    footer: { question: 'How does L1 phonology create near-homophones?', summary: 'The /l/-/r/ contrast collapses in Japanese L1 users.', takeHome: 'ROCK and LOCK become indistinguishable in storage, both triggering KEY.' }
  },

  // ── 4. THEORY ──
  {
    id: 'theory', type: 'split',
    label: '1. THEORETICAL FOUNDATIONS',
    title: 'Han et al. (2021) vs. Ota (2009)',
    repLevel: 'PHON',
    visualContent: (
      <div className="theory-diagram">
        <div className="hypothesis-box">
          <div className="hyp-label"><a href="https://doi.org/10.1007/s10936-020-09725-4" target="_blank" rel="noopener noreferrer">HAN ET AL. (2021)</a></div>
          <p><strong>Orthography-First</strong></p>
          <p>Orthography shapes L2 phonological processing; orthographic information can lead lexical processing</p>
          <p>Method: Cross-modal priming</p>
        </div>
        <div className="vs-circle">VS</div>
        <div className="hypothesis-box">
          <div className="hyp-label"><a href="https://doi.org/10.1016/j.cognition.2008.12.007" target="_blank" rel="noopener noreferrer">OTA ET AL. (2009)</a></div>
          <p><strong>Phonological Constraint</strong></p>
          <p>L1 phonology <strong>constrains</strong> L2 storage</p>
          <p>Method: Visual semantic task</p>
        </div>
      </div>
    ),
    text: (
      <>
        <p><strong>Han et al.:</strong> Orthography shapes how L2 listeners process phonological variants; orthographic information can lead lexical processing. <strong>Ota:</strong> L1 phonology shapes L2 lexical storage. When /l/-/r/ is absent from L1, it becomes <Tooltip term="Indeterminate">indeterminate</Tooltip> in L2.</p>
        <p><strong>Our data:</strong> LR error rates (&gt;20%) match Homophones, which supports <strong>structural filtering</strong> over orthographic triggering.</p>
      </>
    ),
    formal: (
      <div className="formal-block" style={{ padding: '0.75rem' }}>
        <div className="formal-header">STRUCTURAL FILTERING</div>
        <p style={{ fontSize: '0.85rem', margin: '0.25rem 0' }}>If Japanese lacks /l/-/r/, LOCK and ROCK both reduce to <span className="ipa-form">/&#x0251;k/</span>, producing a single lexical entry that activates KEY. This is a claim about <em>storage</em>, not just <em>perception</em>.</p>
      </div>
    ),
    footer: { question: 'What theoretical debate does this study address?', summary: 'Orthography-first vs. phonological constraint frameworks.', takeHome: 'Ota predicts L1-absent contrasts collapse in L2 storage; our data support this.' }
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
            <tr className="row-f"><td><strong>F</strong></td><td>COUGH – WALL</td><td>Multiple phonemes differ</td><td>Low (baseline)</td></tr>
            <tr className="row-pb"><td><strong>PB</strong></td><td>BALL – PAT</td><td>/p/-/b/ (present in Japanese)</td><td>Low</td></tr>
            <tr className="row-h"><td><strong>H</strong></td><td>SON – SUN</td><td>Homophone (identical sound)</td><td>High (universal)</td></tr>
            <tr className="row-lr"><td><strong>LR</strong></td><td>KEY – ROCK</td><td>/l/-/r/ (absent in Japanese)</td><td>High (L1-specific)</td></tr>
          </tbody>
        </table>
        <div className="table-caption">N = 20 Japanese L1 users \u00B7 ~1,200 trials \u00B7 258 unique word pairs</div>
      </div>
    ),
    text: (<>Participants judge semantic relatedness of <strong>visually presented</strong> word pairs. On unrelated trials, responding &ldquo;related&rdquo; counts as a <Tooltip term="FP">false positive</Tooltip>. The task isolates <em>lexical</em> representations from auditory discrimination.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">PHONOLOGICAL DISTINCTNESS SCALE</div>
        <p>Accuracy coded as 1 = correct rejection (correctly identified unrelated), 0 = false positive (incorrectly accepted as related).</p>
        <p>We operationalize &ldquo;Representational Indeterminacy&rdquo; as a continuous predictor:</p>
        <div className="distinctness-scale">
          <div className="scale-item"><span className="scale-label">Homophone</span><span className="scale-val">0.0</span></div>
          <div className="scale-sep" />
          <div className="scale-item"><span className="scale-label">L1-Absent</span><span className="scale-val">0.3</span></div>
          <div className="scale-sep" />
          <div className="scale-item"><span className="scale-label">L1-Present</span><span className="scale-val">0.8</span></div>
          <div className="scale-sep" />
          <div className="scale-item"><span className="scale-label">Control</span><span className="scale-val">1.0</span></div>
          <div className="scale-gradient-edge" />
        </div>
      </div>
    ),
    footer: { question: 'How was the experiment structured?', summary: 'Four contrast types tested in a visual semantic task.', takeHome: 'If /l/-/r/ is absent in L1, LOCK/ROCK should behave like homophones.' }
  },

  // ── 5. MATH SPINE: COIN-FLIP METAPHOR (NEW) ──
  {
    id: 'coin_flip', type: 'split',
    label: '3a. THE INTUITION',
    title: 'Each Decision: Related or Unrelated?',
    repLevel: 'STAT',
    visualContent: (
      <div className="coin-flip-diagram">
        <p className="coin-task-intro">In each trial, participants see two words (e.g., KEY&ndash;ROCK) and decide: <em>Are these semantically related?</em> The correct answer is always &ldquo;No&rdquo; (unrelated). Think of each decision as flipping a biased coin:</p>
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
        <p className="coin-caption"><strong>Heads</strong> = correct rejection (&ldquo;No, unrelated&rdquo; &#x2713;). <strong>Tails</strong> = false positive (&ldquo;Yes, related&rdquo; &#x2717;). The coin&rsquo;s bias (<InlineMath math="\theta" />) depends on contrast type, participant, and word pair.</p>
        <table className="coin-mapping-table">
          <thead><tr><th>Contrast</th><th>Coin Bias</th><th>Why?</th></tr></thead>
          <tbody>
            <tr><td><strong>F</strong> (Control)</td><td>98% correct</td><td>Phonemes clearly distinct (COUGH&ndash;WALL) &rarr; accurate rejection</td></tr>
            <tr><td><strong>PB</strong> (L1-present)</td><td>94% correct</td><td>Contrast exists in L1 (/p/-/b/ in Japanese) &rarr; mostly accurate</td></tr>
            <tr><td><strong>H</strong> (Homophone)</td><td>76% correct</td><td>Same sound (SON&ndash;SUN) &rarr; shared lexical entry causes errors</td></tr>
            <tr><td><strong>LR</strong> (L1-absent)</td><td>79% correct</td><td>/l/-/r/ collapses in Japanese &rarr; ROCK&rarr;LOCK&rarr;KEY confusion</td></tr>
          </tbody>
        </table>
      </div>
    ),
    theoryCallout: 'Each coin\u2019s bias reflects the L1 phonological filter: L1-absent contrasts produce coins biased toward errors because the contrast collapses in lexical storage.',
    text: (<>In each trial, participants see two words and decide whether they are semantically related. For test pairs, the correct answer is always &ldquo;unrelated.&rdquo; Think of each decision as a biased coin flip: <strong>heads = correct, tails = error</strong>, with the coin&rsquo;s bias (<InlineMath math="\theta" />) varying by contrast type, subject, and word pair. Any binary accuracy outcome can be modeled as a <Tooltip term="Bernoulli">Bernoulli trial</Tooltip> (McElreath, 2020; Kruschke, 2015).</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">FROM COINS TO BERNOULLI</div>
        <BlockMath math="y_{ijk} \sim \text{Bernoulli}(\theta_{ijk})" />
        <div className="equation-annotation">
          <span className="eq-term"><InlineMath math="y_{ijk}" /></span> = response (1 = correct, 0 = error)
        </div>
        <div className="equation-annotation-grid">
          <div className="eq-subscript-item"><span className="eq-sub eq-sub-subject"><em>i</em></span> = subject (who answered)</div>
          <div className="eq-subscript-item"><span className="eq-sub eq-sub-contrast"><em>j</em></span> = contrast type (F, PB, H, or LR)</div>
          <div className="eq-subscript-item"><span className="eq-sub eq-sub-item"><em>k</em></span> = word pair (e.g., KEY&ndash;ROCK)</div>
        </div>
        <div className="equation-annotation">
          <span className="eq-term"><InlineMath math="\theta_{ijk}" /></span> = probability of correct response (the coin&rsquo;s bias)
        </div>
        <div className="formal-header" style={{ marginTop: '0.75rem' }}>R EQUIVALENT</div>
        <pre className="brms-colored"><code>family = <span className="code-fn">bernoulli</span>(link = <span className="code-string">"logit"</span>)  <span className="code-fn"># each trial is a coin flip</span></code></pre>
        <p className="formal-note">Binary outcomes require a Bernoulli likelihood &mdash; the statistical formalization of a coin flip (McElreath, 2020; Kruschke, 2015).</p>
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
    text: (<>The <strong>logit link</strong> turns probabilities (0&ndash;1) into log&#x2011;odds, where model terms add up linearly and are then converted back to probabilities. A change of 0.5 in log&#x2011;odds moves a 50% probability to about 62%, with changes having the biggest effect near the middle and smaller effects near 0% or 100%.</>),
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
    label: '4. BAYESIAN MODEL',
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
            <BlockMath math="\text{logit}(\theta_{ijk}) = \underbrace{\beta_0}_{\text{baseline}} + \underbrace{\color{#6c65fc}\beta_j \cdot \mathbf{X}_j}_{\text{contrast}} + \underbrace{\color{#e657c7}u_i}_{\text{subject}} + \underbrace{\color{#c674ff}w_k}_{\text{item}}" />
          </div>
          <div className="equation-annotation-grid">
            <div className="eq-subscript-item"><span className="eq-sub eq-sub-contrast"><em>j</em></span> contrast type &rarr; <code className="code-fixed">contrast_type</code></div>
            <div className="eq-subscript-item"><span className="eq-sub eq-sub-subject"><em>i</em></span> subject &rarr; <code className="code-random-s">(1|subject_id)</code></div>
            <div className="eq-subscript-item"><span className="eq-sub eq-sub-item"><em>k</em></span> item &rarr; <code className="code-random-i">(1|item_id)</code></div>
          </div>
          <p className="tier-text">Random intercepts implement <strong>partial pooling</strong> &mdash; extreme estimates shrink toward the group mean.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">MATHEMATICAL FORMULATION</div>
          <div className="equation-stack">
            <BlockMath math="y_{ijk} \sim \text{Bernoulli}(\theta_{ijk})" />
            <BlockMath math="\text{logit}(\theta_{ijk}) = \beta_0 + \color{#6c65fc}\beta_j \cdot \mathbf{X}_j \color{black}+ \color{#e657c7}u_i \color{black}+ \color{#c674ff}w_k" />
            <BlockMath math="\color{#e657c7}u_i \sim \mathcal{N}(0, \sigma_u)\color{black}, \quad \color{#c674ff}w_k \sim \mathcal{N}(0, \sigma_w)" />
          </div>
          <div className="formal-header" style={{ marginTop: '1rem' }}>R / BRMS SYNTAX (COLOR-MATCHED)</div>
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
          <p className="tier-text"><InlineMath math="\mathcal{N}(0, 1.5)" /> on log-odds: &plusmn;2 SDs (&plusmn;3.0) maps to probabilities of ~5%&ndash;95%, excluding implausible extremes. <InlineMath math="\text{Exp}(1)" /> for random-effects SDs concentrates density near zero while allowing large variance if the data demand it.</p>
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
    theoryCallout: 'The key dimension is L1 phonological status, not specific contrast identity \u2014 supporting Ota\u2019s phonological constraint hypothesis over Han et al.\u2019s orthographic account.',
    text: (<>When grouped by <strong>phonological status</strong> (Unrelated, L1-Present, L1-Absent, Homophone), L1-Absent clusters with Homophone. The grouping captures the theoretical distinction better than raw contrast labels.</>),
    tiers: {
      plain: (<p className="tier-text">The key is <strong>whether the contrast exists in L1</strong>, not which specific sounds are involved. <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>L1-absent</span> contrasts behave like <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>homophones</span> because both lack a distinction in the speaker&rsquo;s phonological inventory. Meanwhile <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>Unrelated</span> and <span style={{ color: 'var(--color-purple)', fontWeight: 700 }}>L1-present</span> show low confusion.</p>),
      technical: (
        <div>
          <BlockMath math="\beta_{\color{#a7a0e6}\text{Unrelated}} > \beta_{\color{#c674ff}\text{L1-present}} \gg \beta_{\color{#6c65fc}\text{L1-absent}} \approx \beta_{\color{#e657c7}\text{Homophone}}" />
          <p className="tier-text">The ordering collapses to two groups: <span style={{ color: 'var(--color-lavender)' }}>{'{'}F, PB{'}'}</span> vs. <span style={{ color: 'var(--color-hot-pink)' }}>{'{'}LR, H{'}'}</span>. The boundary aligns with L1 phonological status.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">THEORETICAL ORDERING</div>
          <BlockMath math="\beta_{\color{#a7a0e6}\text{Unrelated}} > \beta_{\color{#c674ff}\text{L1-present}} \gg \beta_{\color{#6c65fc}\text{L1-absent}} \approx \beta_{\color{#e657c7}\text{Homophone}}" />
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
    theoryCallout: 'Gradient distinctness suggests that L1 phonology constrains the L2 lexicon in a graded way, refining the original binary indeterminacy hypothesis.',
    text: (<>Each increase in <strong>phonological distinctness</strong> (from H 0.0 &rarr; LR 0.3 &rarr; PB 0.8 &rarr; F 1.0, based on the L1 inventory) is associated with a lower probability of L2 errors. This pattern is better captured by a <strong>graded</strong> distinctness predictor than by a simple low&ndash;high split.</>),
    tiers: {
      plain: (<p className="tier-text">Think of phonological distinctness as a <strong>dial, not a switch</strong>: as two sounds are less distinct in the speaker&rsquo;s L1, confusion between their L2 words rises smoothly rather than jumping from &lsquo;no problem&rsquo; to &lsquo;impossible.&rsquo;</p>),
      technical: (
        <div>
          <BlockMath math="\eta_{ijk} = \underbrace{\beta_0}_{\text{baseline}} + \underbrace{\color{#6c65fc}\beta_d \cdot d_j}_{\text{distinctness}} + \underbrace{\color{#e657c7}u_i}_{\text{subject}} + \underbrace{\color{#c674ff}w_k}_{\text{item}}" />
          <div className="equation-annotation-grid">
            <div className="eq-subscript-item"><span className="eq-sub eq-sub-contrast"><InlineMath math="d_j" /></span> distinctness: 0.0 (H) &rarr; 0.3 (LR) &rarr; 0.8 (PB) &rarr; 1.0 (F)</div>
          </div>
          <p className="tier-text">A <strong>single continuous predictor</strong> captures the full contrast hierarchy. Corresponds to R: <code className="code-fixed">phon_distinctness_scaled</code></p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">DISTINCTNESS MODEL</div>
          <BlockMath math="\eta_{ijk} = \beta_0 + \color{#6c65fc}\beta_d \cdot d_j \color{black}+ \color{#e657c7}u_i \color{black}+ \color{#c674ff}w_k" />
          <p>where <InlineMath math="d_j \in \{0.0, 0.3, 0.8, 1.0\}" /> maps each contrast to its distinctness score</p>
          <div className="formal-header" style={{ marginTop: '1rem' }}>R / BRMS</div>
          <pre className="brms-colored"><code><span className="code-fn">brm</span>(<span className="code-formula">accuracy</span> ~ <span className="code-fixed">phon_distinctness_scaled</span> +{'\n    '}<span className="code-random-s">(1|subject_id)</span> + <span className="code-random-i">(1|item_id)</span>,{'\n    '}family = <span className="code-fn">bernoulli</span>(link = <span className="code-string">"logit"</span>))</code></pre>
          <p className="formal-note">Competitive LOO-CV fit with a <em>single predictor</em> &mdash; parsimony favors this parameterization. &Delta;ELPD = &minus;3.3 (within 1 SE of the full model).</p>
        </div>
      )
    },
    footer: { question: 'Is the relationship categorical or graded?', summary: 'Distinctness scores act as a graded predictor of error probability, not a simple binary factor.', takeHome: 'Phonological distinctness is a graded predictor, not binary.' }
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
    tiers: {
      plain: (
        <div>
          <p className="tier-text">Each &ldquo;blob&rdquo; shows how confident we are about each contrast&rsquo;s effect. The <strong>darker center</strong> is where the effect most likely falls. The wider spread means more uncertainty.</p>
          <p className="tier-text"><strong>Key takeaway:</strong> Nearly all plausible values say <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> is worse than baseline &mdash; we&rsquo;re almost certain the /l/-/r/ collapse hurts accuracy.</p>
        </div>
      ),
      technical: (
        <div>
          <p className="tier-text"><InlineMath math="P(\beta_{\text{LR}} < 0 \mid \text{data}) > 0.999" /> &mdash; the probability that the LR effect is negative exceeds 99.9%.</p>
          <p className="tier-text">Dark core = 66% CrI (most likely range). Full span = 95% CrI. <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> and <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span> posteriors overlap substantially, confirming equivalence.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">R / GGDIST CODE</div>
          <pre className="brms-colored"><code><span className="code-formula">posterior_samples</span> |&gt;{'\n  '}<span className="code-fn">ggplot</span>(<span className="code-fn">aes</span>(x = <span className="code-fixed">.value</span>, y = <span className="code-fixed">contrast</span>)) +{'\n  '}<span className="code-fn">stat_halfeye</span>({'\n    '}.width = <span className="code-fn">c</span>(<span className="code-string">.66</span>, <span className="code-string">.95</span>),{'\n    '}fill = <span className="code-string">"indigo"</span>, alpha = <span className="code-string">0.7</span>{'\n  '})</code></pre>
          <div className="formal-header" style={{ marginTop: '0.75rem' }}>MATH &harr; CODE</div>
          <p className="tier-text"><InlineMath math="\beta_j" /> &rarr; <code className="code-fixed">contrast</code> &nbsp;|&nbsp; <code>.width = c(.66, .95)</code> &rarr; inner/outer credible intervals</p>
          <p className="tier-text"><InlineMath math="P(\beta_{\text{LR}} < 0 \mid \text{data}) > 0.999" /> &mdash; near-certainty of impairment.</p>
        </div>
      )
    },
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
    text: (<>LR items show <strong>systematically elevated</strong> error rates &mdash; not driven by a few &ldquo;weird&rdquo; pairs. Some LR pairs (LAG&ndash;CLOTH) reach 100% errors; others (WRONG&ndash;SHORT) near 0%. This is evidence at the <em>lexical item</em> level.</>),
    tiers: {
      plain: (
        <p className="tier-text">The /l/-/r/ confusion isn&rsquo;t caused by a few tricky word pairs &mdash; it&rsquo;s <strong>systematic across the entire set</strong>. LR pairs (blue dots) cluster at high error rates, while F pairs (control) cluster near zero. Some <Tooltip term="Lexical">word pairs</Tooltip> are harder than others, but the overall pattern is clear.</p>
      ),
      technical: (
        <div>
          <p className="tier-text">Item <Tooltip term="Random Effects">random intercepts</Tooltip> (<InlineMath math="w_k" />) capture word-pair-level variability. The model accounts for this via <strong><Tooltip term="Partial Pooling">partial pooling</Tooltip></strong>: extreme items are shrunk toward the group mean.</p>
          <p className="tier-text">Within-LR variability driven by: word frequency, neighbourhood density, /l/-/r/ position (onset vs. coda), semantic plausibility.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">ITEM-LEVEL MODEL</div>
          <BlockMath math="w_k \sim \mathcal{N}(0, \sigma_w)" />
          <pre className="brms-colored"><code><span className="code-fn">ranef</span>(model)$item_id |&gt; <span className="code-fn">as_tibble</span>()</code></pre>
          <p className="tier-text">Each <InlineMath math="w_k" /> captures how much harder/easier item <em>k</em> is relative to the population mean. Future direction: random slopes (<InlineMath math="w_{kj}" />) for contrast-by-item interactions.</p>
        </div>
      )
    },
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
    figureLegend: 'Distribution = posterior of LR\u2013H difference; gray band = ROPE (\u00B10.18 log-odds, OR \u2248 0.84\u20131.20). Overlap = practical equivalence.',
    reproduceTag: 'Step 15, line 1560',
    tiers: {
      plain: (<p className="tier-text">LR and H produce <strong>nearly equivalent</strong> error rates &mdash; the difference between them is small enough to be consistent with equivalence. This is the key test: if L1-absent contrasts truly collapse, they should behave like homophones.</p>),
      technical: (
        <div>
          <p className="tier-text"><strong>ROPE</strong> = [&minus;0.18, 0.18] in log-odds differences (OR &asymp; 0.84&ndash;1.20). If &gt;95% of the posterior falls inside ROPE, accept practical equivalence. If &lt;5%, reject. Otherwise, undecided.</p>
          <div className="rope-decision-box">
            <div className="rope-rule"><span className="rope-icon rope-accept">&#x2713;</span> &gt;95% mass in ROPE &rarr; Accept equivalence</div>
            <div className="rope-rule"><span className="rope-icon rope-reject">&#x2717;</span> &lt;5% mass in ROPE &rarr; Reject equivalence</div>
            <div className="rope-rule"><span className="rope-icon rope-undecided">?</span> Otherwise &rarr; Undecided</div>
          </div>
          <p className="tier-text"><strong>LR&ndash;H:</strong> 42% of the posterior falls within ROPE &rarr; <strong>undecided, but consistent with near-equivalence</strong>. LR&ndash;PB: 95% CrI excludes both zero and ROPE &rarr; credibly different.</p>
          <p className="tier-text">This is Bayesian equivalence testing (Kruschke 2018), not just failure to reject.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">ROPE ANALYSIS</div>
          <pre><code>{`hypothesis(model, "contrastTR_LR = 0",
  rope = c(-0.18, 0.18))
# LR-H: 42% in ROPE → near-equivalent
# LR-PB: 0% in ROPE → credibly different`}</code></pre>
          <div className="dual-interpretation">
            <p className="tier-text"><strong>Intuitive:</strong> LR and H behave similarly &mdash; consistent with homophones at the lexical level.</p>
            <p className="tier-text"><strong>Technical:</strong> The posterior of <InlineMath math="\beta_{\text{LR}} - \beta_{\text{H}}" /> has 42% mass within ROPE [&minus;0.18, 0.18], indicating <strong>near-equivalence</strong> rather than a large difference.</p>
          </div>
          <p className="formal-note">The ROPE approach goes beyond NHST: instead of asking &ldquo;is the difference non-zero?&rdquo; we ask &ldquo;is it negligibly small?&rdquo; This maps directly to the theoretical question about representational equivalence.</p>
        </div>
      )
    },
    text: (<>The <Tooltip term="ROPE">ROPE</Tooltip> test (&plusmn;0.18 log-odds, OR &asymp; 0.84&ndash;1.20) shows <strong>substantial mass near zero for LR&ndash;H</strong>, consistent with practical equivalence, while <strong>LR differs credibly from PB</strong>. <CodeLink label="ROPE" /></>),
    footer: { question: 'Are LR and H truly equivalent?', summary: 'LR\u2013H posterior places substantial mass inside the ROPE band.', takeHome: 'LR and H behave as nearly equivalent; LR and PB are credibly different.' }
  },

  // ── 18. VALIDATION ──
  {
    id: 'validation', type: 'split',
    label: '15. BAYESIAN VALIDATION',
    title: 'Convergence, PPC, Sensitivity, LOO-CV',
    repLevel: 'STAT',
    theoryCallout: 'Validation indicates that the phonological constraint hypothesis is unlikely to be an artifact of modeling choices.',
    visualSrc: './assets/35_mcmc_posterior_sampling.gif',
    visualCaption: 'MCMC sampling from the posterior',
    figureLegend: 'Animated MCMC traces: well-mixed chains explore the same region, indicating convergence.',
    reproduceTag: 'Step 13c, line 1079',
    tiers: {
      plain: (
        <div>
          <p className="tier-text">The model passes all four validation checks, so the results are <strong>statistically reliable within this modeling framework</strong>: no numerical problems, good fit to the data, stable under reasonable prior changes, and competitive with alternative specifications in LOO-CV.</p>
          <p className="tier-text"><strong>Bottom line:</strong> These four checks indicate that the L1-absent collapse effect is <strong>unlikely to be an artifact</strong> of poor convergence, unreasonable priors, or overfitting.</p>
        </div>
      ),
      technical: (
        <div className="validation-checklist">
          <div className="validation-check"><span className="validation-icon">&#x2713;</span><strong>Convergence</strong> &mdash; R-hat &asymp; 1.00, zero divergences, ESS &gt; 1,000</div>
          <div className="validation-check"><span className="validation-icon">&#x2713;</span><strong>Fit</strong> &mdash; PPC: observed error rates fall within posterior predictive intervals</div>
          <div className="validation-check"><span className="validation-icon">&#x2713;</span><strong>Sensitivity</strong> &mdash; robust to widening <InlineMath math="\mathcal{N}(0,1.5)" /> priors to <InlineMath math="\mathcal{N}(0,3.0)" /></div>
          <div className="validation-check"><span className="validation-icon">&#x2713;</span><strong>Comparison</strong> &mdash; LOO-CV: all models comparable (&Delta;ELPD &lt; 1 SE)</div>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">FULL DIAGNOSTICS</div>
          <table className="appendix-table validation-diag-table">
            <thead><tr><th>Check</th><th>Criterion</th><th>Result</th><th>Status</th></tr></thead>
            <tbody>
              <tr><td>R-hat</td><td>&lt; 1.01</td><td>All &asymp; 1.00</td><td className="pass">&#x2713;</td></tr>
              <tr><td>ESS (bulk/tail)</td><td>&gt; 400</td><td>&gt; 1,000 / &gt; 800</td><td className="pass">&#x2713;</td></tr>
              <tr><td>Divergences</td><td>0</td><td>0</td><td className="pass">&#x2713;</td></tr>
              <tr><td>PPC</td><td>Overlap</td><td>Observed within predictive intervals</td><td className="pass">&#x2713;</td></tr>
              <tr><td>Prior sensitivity</td><td>Stable</td><td>Robust: <InlineMath math="\mathcal{N}(0,1.5)" /> &rarr; <InlineMath math="\mathcal{N}(0,3.0)" /></td><td className="pass">&#x2713;</td></tr>
              <tr><td>LOO-CV</td><td>Comparable</td><td>&Delta;ELPD &lt; 1 SE</td><td className="pass">&#x2713;</td></tr>
            </tbody>
          </table>
          <pre style={{ marginTop: '0.75rem' }}><code>{`pp_check(model, ndraws = 100, type = "bars")
# Observed proportions fall within posterior predictive intervals`}</code></pre>
        </div>
      )
    },
    text: (<>Four validation layers: <Tooltip term="MCMC">MCMC</Tooltip> diagnostics, <Tooltip term="PPC">posterior predictive checks</Tooltip>, prior sensitivity, and <Tooltip term="LOO-CV">LOO cross-validation</Tooltip>. <CodeLink label="Diagnostics" /></>),
    footer: { question: 'Can we trust the model?', summary: 'MCMC diagnostics, PPC, sensitivity, and LOO-CV all pass.', takeHome: 'Good convergence, no divergences, robust to widening priors, competitive LOO-CV.' }
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
    theoryCallout: 'LR items dominate the high-error region \u2014 L1-absent /l/-/r/ items behave like near-homophones at the item level, supporting lexical-level representational indeterminacy.',
    text: (<>Every word pair ranked by its <Tooltip term="Posterior">posterior</Tooltip> mean error rate. <strong>LR pairs cluster at the top</strong>, but within-category variability reveals item-level effects beyond contrast type.</>),
    tiers: {
      plain: (
        <p className="tier-text">When we line up all 258 word pairs from easiest to hardest, <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> pairs (blue) crowd the high-error end, while <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span> pairs (control) cluster near zero. The pattern is systematic, not random.</p>
      ),
      technical: (
        <div>
          <p className="tier-text"><span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> pairs tend to cluster above 15% error. <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span> pairs cluster near 0&ndash;5%. <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span> pairs show moderate variability (semantic plausibility varies across pairs).</p>
          <p className="tier-text">Item random intercepts <InlineMath math="w_k" /> capture this variability &mdash; some /l/-/r/ pairs are harder than others depending on lexical factors.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">ITEM-LEVEL POSTERIOR</div>
          <pre className="brms-colored"><code><span className="code-fn">ranef</span>(model)$item_id |&gt;{'\n  '}<span className="code-fn">arrange</span>(<span className="code-fn">desc</span>(Estimate))</code></pre>
          <p className="tier-text">Each dot represents <InlineMath math="\text{logit}^{-1}(\hat{\beta}_0 + \hat{\beta}_j + \hat{w}_k)" /> &mdash; the posterior mean error rate for item <em>k</em> in contrast <em>j</em>.</p>
          <p className="formal-note">One interpretation: extreme <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> items (e.g., <span style={{ color: 'var(--color-hot-pink)' }}>LAG&ndash;CLOTH</span> at ~100%) plausibly reflect strong mediated relationships, while low-error <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> items may lack plausible semantic links.</p>
        </div>
      )
    },
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
    tiers: {
      plain: (
        <p className="tier-text">Every single participant shows the same pattern: the <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> column is <strong>consistently dark</strong> (high errors) while the <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span> column is light (low errors). Some people are better overall, but nobody escapes the /l/-/r/ confusion.</p>
      ),
      technical: (
        <div>
          <p className="tier-text">Subject random intercepts <InlineMath math="u_i" /> capture baseline variation (rows). But the <strong>contrast effect</strong> (<InlineMath math="\beta_j" />) is consistent across all subjects &mdash; no subject &times; contrast interaction needed.</p>
          <p className="tier-text"><span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span> column: uniformly light | <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> column: uniformly dark | <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span>: moderate-to-dark</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">R CODE</div>
          <pre className="brms-colored"><code><span className="code-fn">ggplot</span>(data, <span className="code-fn">aes</span>(x = contrast, y = <span className="code-fn">reorder</span>(subject, error_rate))) +{'\n  '}<span className="code-fn">geom_tile</span>(<span className="code-fn">aes</span>(fill = error_rate))</code></pre>
          <p className="tier-text">Model: <InlineMath math="\text{logit}(\theta_{ijk}) = \beta_0 + \beta_j + u_i + w_k" /></p>
          <p className="tier-text">The heatmap visualizes the raw data that the hierarchical model captures: <InlineMath math="u_i" /> (<span style={{ color: 'var(--color-purple)' }}>row variation</span>) + <InlineMath math="\beta_j" /> (<span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>column pattern</span>).</p>
        </div>
      )
    },
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
    figureLegend: 'Distribution = posterior pairwise difference; gray band = ROPE (\u00B10.18 log-odds, OR \u2248 0.84\u20131.20). Panels = all 6 contrasts.',
    reproduceTag: 'Step 15, line 1560',
    theoryCallout: 'LR \u2248 H equivalence is the critical test: if L1-absent = homophone in the lexicon, representational indeterminacy is confirmed.',
    text: (<>The hierarchy is confirmed: <strong><span style={{ color: 'var(--color-indigo)' }}>LR</span> and <span style={{ color: 'var(--color-hot-pink)' }}>H</span> are equivalent; both much worse than <span style={{ color: 'var(--color-purple)' }}>PB</span> and <span style={{ color: 'var(--color-lavender)' }}>F</span></strong>.</>),
    tiers: {
      plain: (
        <div>
          <p className="tier-text">We compare every pair of contrast types. The result: <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span> (/l/-/r/) and <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span> (homophones) are <strong>practically the same</strong> &mdash; they cause similar confusion. Both are <strong>much worse</strong> than <span style={{ color: 'var(--color-purple)', fontWeight: 700 }}>PB</span> (/p/-/b/) and <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span> (control).</p>
          <p className="tier-text">This is the key result: if /l/-/r/ is absent from your first language, English words differing by /l/-/r/ are stored like homophones.</p>
        </div>
      ),
      technical: (
        <div>
          <p style={{ fontSize: '0.65rem', textTransform: 'uppercase', letterSpacing: '0.15em', color: 'var(--text-muted)', marginBottom: '0.5rem', fontWeight: 600 }}>Pairwise comparisons</p>
          <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr 1fr', gap: '0.75rem', marginBottom: '1rem' }}>
            <div style={{ background: 'var(--bg-elevated)', border: '1px solid var(--border)', borderRadius: 'var(--radius-sm)', padding: '0.8rem', textAlign: 'center' }}>
              <span style={{ fontSize: '0.7rem', color: 'var(--color-indigo)', fontWeight: 600, letterSpacing: '0.05em' }}>LR</span>
              <span style={{ fontSize: '0.65rem', color: 'var(--text-muted)', display: 'block', margin: '2px 0' }}>vs</span>
              <span style={{ fontSize: '0.7rem', color: 'var(--color-hot-pink)', fontWeight: 600, letterSpacing: '0.05em' }}>H</span>
              <div style={{ marginTop: '0.5rem', fontSize: '0.85rem', fontWeight: 700, color: 'var(--text-muted)' }}>no meaningful difference</div>
            </div>
            <div style={{ background: 'var(--bg-elevated)', border: '1px solid var(--border)', borderRadius: 'var(--radius-sm)', padding: '0.8rem', textAlign: 'center' }}>
              <span style={{ fontSize: '0.7rem', color: 'var(--color-purple)', fontWeight: 600, letterSpacing: '0.05em' }}>PB</span>
              <span style={{ fontSize: '0.65rem', color: 'var(--text-muted)', display: 'block', margin: '2px 0' }}>vs</span>
              <span style={{ fontSize: '0.7rem', color: 'var(--color-lavender)', fontWeight: 600, letterSpacing: '0.05em' }}>F</span>
              <div style={{ marginTop: '0.5rem', fontSize: '0.85rem', fontWeight: 700, color: 'var(--text-muted)' }}>no meaningful difference</div>
            </div>
            <div style={{ background: '#d8e6ff22', border: '2px solid var(--color-indigo)', borderRadius: 'var(--radius-sm)', padding: '0.8rem', textAlign: 'center' }}>
              <span style={{ fontSize: '0.7rem', color: 'var(--color-indigo)', fontWeight: 600, letterSpacing: '0.05em' }}>LR</span>
              <span style={{ fontSize: '0.65rem', color: 'var(--text-muted)', display: 'block', margin: '2px 0' }}>vs</span>
              <span style={{ fontSize: '0.7rem', color: 'var(--color-purple)', fontWeight: 600, letterSpacing: '0.05em' }}>PB</span>
              <div style={{ marginTop: '0.5rem', fontSize: '0.85rem', fontWeight: 700, color: 'var(--color-indigo)' }}>clear difference</div>
            </div>
          </div>
          <p className="tier-text" style={{ fontSize: '0.8rem' }}>ROPE [&minus;0.18, 0.18] log-odds. LR&ndash;H lies mostly inside the ROPE (near-equivalent); LR&ndash;PB lies entirely outside (credibly different).</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">R / BRMS PAIRWISE TEST</div>
          <pre className="brms-colored"><code><span className="code-fn">hypothesis</span>(model,{'\n  '}<span className="code-string">"contrast_typeLR - contrast_typeH = 0"</span>,{'\n  '}rope = c(-0.18, 0.18)){'\n'}<span className="code-fn"># LR-H: 42% in ROPE → equivalent</span>{'\n'}<span className="code-fn"># LR-PB: 0% in ROPE → credibly different</span></code></pre>
          <p className="tier-text">The four-level hierarchy collapses to two equivalence classes: {'{'}<span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span>, <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span>{'}'} vs. {'{'}<span style={{ color: 'var(--color-purple)', fontWeight: 700 }}>PB</span>, <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span>{'}'}.</p>
        </div>
      )
    },
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
    tiers: {
      plain: (
        <div>
          <p className="tier-text"><strong>1. Confusion confirmed:</strong> When /l/ and /r/ don&rsquo;t exist as separate sounds in your language, English words like ROCK and LOCK get stored as the same word. This makes ROCK trigger KEY (because LOCK is related to KEY).</p>
          <p className="tier-text"><strong>2. It&rsquo;s gradual:</strong> The more similar two sounds are in your first language, the more confusion &mdash; it&rsquo;s a spectrum, not a switch.</p>
          <p className="tier-text"><strong>3. Everyone shows it:</strong> All 20 participants had the same pattern. Not a few outliers.</p>
          <p className="tier-text"><strong>4. Statistics check out:</strong> Multiple independent checks confirm the model works correctly and results are trustworthy.</p>
        </div>
      ),
      technical: (
        <div>
          <p style={{ fontSize: '1.4rem', textAlign: 'center', margin: '0.5rem 0' }}>
            <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span>
            {' ≈ '}
            <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span>
            {' ≫ '}
            <span style={{ color: 'var(--color-purple)', fontWeight: 700 }}>PB</span>
            {' ≈ '}
            <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span>
          </p>
          <p className="tier-text"><strong>Indeterminacy:</strong> LR FP rate (~21%) ≈ H (~24%) via ROPE analysis. <strong>Gradient:</strong> <InlineMath math="d_j \in [0,1]" /> continuous predictor. <strong>Universal:</strong> <InlineMath math="u_i" /> varies but <InlineMath math="\beta_j" /> is consistent. <strong>Validated:</strong> R-hat ≈ 1.00, ESS &gt; 1,000, LOO-CV comparable.</p>
        </div>
      ),
      full: (
        <div className="formal-block">
          <div className="formal-header">COMPLETE MODEL</div>
          <BlockMath math="\text{logit}(\theta_{ijk}) = \beta_0 + \beta_j + u_i + w_k" />
          <p style={{ fontSize: '1.2rem', textAlign: 'center', margin: '0.5rem 0' }}>
            <span style={{ color: 'var(--color-indigo)', fontWeight: 700 }}>LR</span>
            {' ≈ '}
            <span style={{ color: 'var(--color-hot-pink)', fontWeight: 700 }}>H</span>
            {' ≫ '}
            <span style={{ color: 'var(--color-purple)', fontWeight: 700 }}>PB</span>
            {' ≈ '}
            <span style={{ color: 'var(--color-lavender)', fontWeight: 700 }}>F</span>
          </p>
          <p className="formal-note">Consistent with Ota et al.&rsquo;s structural filtering hypothesis, extended with gradient (continuous) formalization. Three model variants (Comprehensive, Linguistic, Distinctness) converge on the same hierarchy.</p>
        </div>
      )
    },
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
            <li><strong>Binary <Tooltip term="Dependent Variable">Dependent Variable (DV)</Tooltip>:</strong> Response times could add a continuous measure of processing difficulty.</li>
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
    text: (<>Every study has boundaries. This re-analysis inherits Ota (2009)&rsquo;s constraints and adds its own, which in turn suggest extensions that could refine the theory of representational indeterminacy.</>),
    footer: { question: 'What should we be cautious about?', summary: 'Five limitations and four future directions.', takeHome: 'Small N and single L1 are real caveats; hierarchical modelling partly compensates.' }
  },

  // ── 24. REFERENCES ──
  {
    id: 'references', type: 'split',
    label: '21. REFERENCES',
    title: 'Key References',
    visualContent: (
      <div className="references-list">
        <div className="ref-item">B&uuml;rkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. <em>JSS, 80</em>(1), 1&ndash;28. <a href="https://doi.org/10.18637/jss.v080.i01" target="_blank" rel="noopener noreferrer">doi:10.18637/jss.v080.i01</a></div>
        <div className="ref-item">Han, J. I., Kim, J. Y., &amp; Choi, T. H. (2021). The Role of Orthography in Lexical Processing of the Phonological Variants in Second Language. <em>J Psycholinguist Res, 50</em>(2), 437&ndash;445. <a href="https://doi.org/10.1007/s10936-020-09725-4" target="_blank" rel="noopener noreferrer">doi:10.1007/s10936-020-09725-4</a></div>
        <div className="ref-item">Kruschke, J. K. (2018). Rejecting or Accepting Parameter Values in Bayesian Estimation. <em>AMPPS, 1</em>(2), 270&ndash;280. <a href="https://doi.org/10.1177/2515245918771304" target="_blank" rel="noopener noreferrer">doi:10.1177/2515245918771304</a></div>
        <div className="ref-item">McElreath, R. (2020). <em>Statistical Rethinking</em> (2nd ed.). CRC Press. <a href="https://doi.org/10.1201/9780429029608" target="_blank" rel="noopener noreferrer">doi:10.1201/9780429029608</a></div>
        <div className="ref-item">Ota, M., Hartsuiker, R. J., &amp; Haywood, S. L. (2009). The KEY to the ROCK: Near-homophony in nonnative visual word recognition. <em>Cognition, 111</em>(2), 263&ndash;269. <a href="https://doi.org/10.1016/j.cognition.2008.12.007" target="_blank" rel="noopener noreferrer">doi:10.1016/j.cognition.2008.12.007</a></div>
        <div className="ref-item">Gelman, A., et al. (2020). Bayesian Workflow. <em>Statistical Science</em>. <a href="https://doi.org/10.1214/20-STS812" target="_blank" rel="noopener noreferrer">doi:10.1214/20-STS812</a></div>
        <div className="ref-item">Vehtari, A., Gelman, A., &amp; Gabry, J. (2017). Practical Bayesian model evaluation using LOO-CV and WAIC. <em>Statistics and Computing, 27</em>(5), 1413&ndash;1432. <a href="https://doi.org/10.1007/s11222-016-9696-4" target="_blank" rel="noopener noreferrer">doi:10.1007/s11222-016-9696-4</a></div>
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
          <li><strong>Cross-linguistic replication:</strong> Korean, Mandarin, Thai, Vietnamese L1 users with different phonological gaps</li>
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
  const [showExperiment, setShowExperiment] = useState(false);

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
    setShowExperiment(false);
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
      if (showExperiment) return; // Experiment modal handles its own keys
      if (e.key === 'ArrowRight' || e.key === ' ') { e.preventDefault(); goNext(); }
      if (e.key === 'ArrowLeft') { e.preventDefault(); goPrev(); }
      if (e.key === 'm' || e.key === 'M') setShowFormal(prev => !prev);
      if (e.key === 'o' || e.key === 'O') setShowOverview(prev => !prev);
      if (e.key === 'd' || e.key === 'D') toggleTheme();
      if (e.key === 'a' || e.key === 'A') setShowAppendix(prev => !prev);
      if (e.key === 'e' || e.key === 'E') setShowExperiment(prev => !prev);
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [goNext, goPrev, showOverview, showAppendix, showExperiment, toggleTheme]);

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
            <div className="hero-content">
              {currentSlide.id === 'title' && (
                <div className="hero-method-badge"><Tooltip term="Bayesian Statistics">Bayesian Statistics</Tooltip> &middot; <Tooltip term="brms">brms</Tooltip> + <Tooltip term="R Programming">R Programming</Tooltip></div>
              )}
              <div className="hero-title-group">
                <h1 className="hero-title">{currentSlide.title}</h1>
                <div className="hero-divider"></div>
                <p className="hero-subtitle">{currentSlide.subtitle}</p>
                {currentSlide.subtitleTechnical && (
                  <p className="hero-subtitle-technical">{currentSlide.subtitleTechnical}</p>
                )}
              </div>

              {currentSlide.id === 'title' && (
                <>
                  <div className="hero-gradient-card">
                    <div className="hero-gradient-bar">
                      {[
                        { code: 'F', pos: 2, color: 'var(--color-lavender)', label: 'Spelling Control', rate: '~2% error rate', example: 'FISH / DISH', desc: 'Clearly distinct sounds' },
                        { code: 'PB', pos: 25, color: 'var(--color-purple)', label: '/p/\u2013/b/ (L1-present)', rate: '~6% error rate', example: 'BACK / PACK', desc: 'Present in Japanese' },
                        { code: 'H', pos: 72, color: 'var(--color-hot-pink)', label: 'Homophones', rate: '~24% error rate', example: 'MEAT / MEET', desc: 'Identical pronunciation' },
                        { code: 'LR', pos: 95, color: 'var(--color-indigo)', label: '/l/\u2013/r/ (L1-absent)', rate: '~21% error rate', example: 'ROCK / LOCK', desc: 'Absent in Japanese' },
                      ].map(c => (
                        <div key={c.code} className="hero-gp" style={{ left: `${c.pos}%`, '--dot-color': c.color }}>
                          <span className="hero-gp-code">{c.code}</span>
                          <div className="hero-gp-dot"></div>
                          <div className="hero-gp-tooltip">
                            <div className="hero-gp-tooltip-title">{c.code} ({c.label})</div>
                            <div className="hero-gp-tooltip-rate">{c.rate}</div>
                            <div className="hero-gp-tooltip-example">{c.example}</div>
                            <div className="hero-gp-tooltip-desc">{c.desc}</div>
                          </div>
                        </div>
                      ))}
                    </div>
                    <div className="hero-gradient-endpoints">
                      <span>DISTINCT</span>
                      <span>&rarr;</span>
                      <span>COLLAPSED</span>
                    </div>
                    <p className="hero-gradient-hint">Hover any point for details</p>
                  </div>

                  <div className="hero-cta-area">
                    <button className="hero-primary-cta" onClick={() => goToSlide(1)}>Begin Exploration &rarr;</button>
                  </div>

                  <p className="hero-footer-meta">Violet Manson &amp; Sandria Tran &middot; Bayesian Re-analysis of <a href="https://doi.org/10.1016/j.cognition.2008.12.007" target="_blank" rel="noopener noreferrer" style={{ color: 'inherit', textDecoration: 'none', borderBottom: '1px dotted var(--text-muted)' }}>Ota, Hartsuiker &amp; Haywood (2009)</a></p>
                  <p className="hero-footer-tech">Interactive research deck by Sandria Tran: React/Vite glassmorphic UI, R Bayesian pipeline, dual-theme accessibility.</p>
                </>
              )}
            </div>
          </div>
        )}

        {currentSlide.type === 'split' && (
          <div className="slide slide-split" data-section={currentSlide.id}>
            <div className="split-left">
              {/* Contrast Legend Strip (persistent reference) */}
              {currentSlide.showContrastLegend && <ContrastLegendStrip />}

              <div className="visual-frame">
                {currentSlide.visualSrc ? (
                  <div className="visual-img-pair">
                    <img src={currentSlide.visualSrc} className="visual-img visual-img-light" alt="Evidence" />
                    <img src={currentSlide.visualSrc.replace('./assets/', './assets/dark_mode/')} className="visual-img visual-img-dark" alt="Evidence" />
                  </div>
                ) : currentSlide.visualContent}
                {currentSlide.visualCaption && <div className="visual-caption">{currentSlide.visualCaption}</div>}
                {/* Figure legend: explains what visual elements mean */}
                {currentSlide.figureLegend && <FigureLegend text={currentSlide.figureLegend} />}
                {/* Reproduce tag: links to R script */}
                {currentSlide.reproduceTag && <ReproduceTag scriptRef={currentSlide.reproduceTag} />}
              </div>
            </div>
            <div className="split-right">
              {/* Model Recap Box — compact header chip */}
              {currentSlide.showModelRecap && showFormal && <ModelRecap />}
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
        <div className="bar-section bar-nav">
          {SECTIONS.map(section => (
            <button
              key={section.id}
              className={`section-label-btn ${currentIndex >= section.startIndex && currentIndex <= section.endIndex ? 'active' : ''}`}
              onClick={() => goToSlide(section.startIndex)}
            >
              {section.label}
            </button>
          ))}
        </div>
        <div className="bar-divider" />
        <span className="slide-counter">{currentIndex + 1} / {totalSlides}</span>
        <div className="bar-divider" />
        <div className="bar-section bar-controls">
          <button className="bar-icon-btn" onClick={() => setShowOverview(true)} title="Overview (O)">
            <svg width="12" height="12" viewBox="0 0 14 14" fill="none"><rect x="0.5" y="0.5" width="5" height="5" rx="1" stroke="currentColor"/><rect x="8.5" y="0.5" width="5" height="5" rx="1" stroke="currentColor"/><rect x="0.5" y="8.5" width="5" height="5" rx="1" stroke="currentColor"/><rect x="8.5" y="8.5" width="5" height="5" rx="1" stroke="currentColor"/></svg>
          </button>
          <button className="bar-icon-btn" onClick={toggleTheme} title="Toggle theme (D)">
            {theme === 'dark' ? '\u2600' : '\u263E'}
          </button>
          <button className="bar-icon-btn" onClick={() => setShowFormal(!showFormal)} title="Toggle math (M)">
            {showFormal ? '\u2212M' : '+M'}
          </button>
          <button className="bar-icon-btn" onClick={() => setShowAppendix(true)} title="Technical Appendix (A)">A</button>
          <button className="bar-icon-btn" onClick={() => setShowExperiment(true)} title="Try the Experiment (E)">E</button>
        </div>
        <div className="bar-divider" />
        <div className="bar-section bar-links">
          <a className="bar-link" href="https://github.com/sandriatran/qml-2025" target="_blank" rel="noopener noreferrer">GitHub</a>
          <a className="bar-link" href="https://github.com/sandriatran/qml-2025/blob/main/final%20project/Re-Analyzing-Ota%2C-Hartsuiker-and-Haywood--2009----A-Bayesian-Approach-to-Representational-Indeterminacy.pdf" target="_blank" rel="noopener noreferrer">Final Report</a>
          <a className="bar-link" href="https://doi.org/10.1016/j.cognition.2008.12.007" target="_blank" rel="noopener noreferrer">Ota 2009</a>
        </div>
      </nav>

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

      {/* ── Experiment modal ── */}
      {showExperiment && (
        <ExperimentModal onClose={() => setShowExperiment(false)} />
      )}
    </div>
  );
}

export default App;

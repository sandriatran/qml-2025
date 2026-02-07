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
// SECTION MAP (for dot navigation)
// ============================================================
const SECTIONS = [
  { id: 'intro', label: 'Introduction', startIndex: 0, endIndex: 3 },
  { id: 'model', label: 'Model', startIndex: 4, endIndex: 5 },
  { id: 'results', label: 'Results', startIndex: 6, endIndex: 10 },
  { id: 'evidence', label: 'Evidence', startIndex: 11, endIndex: 15 },
  { id: 'deep', label: 'Deep Dive', startIndex: 16, endIndex: 18 },
  { id: 'synthesis', label: 'Synthesis', startIndex: 19, endIndex: 22 },
];

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
// SLIDE DATA
// ============================================================
const slides = [
  // ── 1. TITLE ──
  {
    id: 'title', type: 'hero',
    title: 'The Key to the Rock',
    subtitle: 'A Bayesian re-analysis of representational indeterminacy in non-native word recognition',
    meta: 'Final Project \u00B7 Bayesian Re-analysis of Ota, Hartsuiker & Haywood (2009)',
    credit: 'V. Manson & S. Tran',
  },

  // ── 2. EXECUTIVE SUMMARY ──
  {
    id: 'summary', type: 'split',
    label: 'EXECUTIVE SUMMARY',
    title: 'Key Findings at a Glance',
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
        <p>When a phonological contrast is absent from a speaker&rsquo;s L1, L2 word pairs differing by that contrast become <strong>near-homophones</strong>.</p>
        <div className="ipa-chain">
          <span className="ipa-word">ROCK</span>
          <span className="ipa-sep">&harr;</span>
          <span className="ipa-form">/&#x0279;&#x0251;k/</span>
          <span className="ipa-sep">&asymp;</span>
          <span className="ipa-form">/l&#x0251;k/</span>
          <span className="ipa-sep">&harr;</span>
          <span className="ipa-word">LOCK</span>
          <span className="ipa-sep">&rarr;</span>
          <span className="ipa-word">KEY</span>
        </div>
        <p className="formal-note">For Japanese speakers, ROCK and LOCK map to the same phonological form and both trigger KEY.</p>
      </div>
    ),
    footer: { question: 'What are the key findings?', summary: 'Five numbered results from the Bayesian re-analysis.', takeHome: 'L/R confusion matches homophones; the effect is gradient, robust, and generalizable.' }
  },

  // ── 3. THEORY ──
  {
    id: 'theory', type: 'split',
    label: '1. THEORETICAL FOUNDATIONS',
    title: 'Jiao (2024) vs. Ota (2009)',
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
        <p><strong>Jiao:</strong> Phonology aids meaning but is secondary. <strong>Ota:</strong> Phonology shapes L2 storage. When /l/-/r/ is absent in L1, it becomes <Tooltip term="Indeterminate">indeterminate</Tooltip> in L2.</p>
        <p><strong>Our data:</strong> LR error rates (&gt;20%) match Homophones.</p>
      </>
    ),
    formal: (
      <div className="formal-block">
        <div className="formal-header">MECHANISM</div>
        <p>If Japanese lacks /l/-/r/, then LOCK and ROCK both reduce to <span className="ipa-form">/&#x0251;k/</span> and both trigger KEY.</p>
      </div>
    ),
    footer: { question: 'What theoretical debate does this study address?', summary: 'Orthography-first vs. phonological constraint frameworks.', takeHome: 'Ota predicts L1-absent contrasts collapse in L2; our data confirms this.' }
  },

  // ── 4. DESIGN ──
  {
    id: 'design', type: 'split',
    label: '2. EXPERIMENTAL DESIGN',
    title: 'The Four Contrast Types',
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
    text: (<>Participants judge semantic relatedness of <strong>visually presented</strong> word pairs. On unrelated trials, responding &ldquo;related&rdquo; counts as a <Tooltip term="FP">false positive</Tooltip>.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">PHONOLOGICAL DISTINCTNESS SCALE</div>
        <p>We operationalize &ldquo;Representational Indeterminacy&rdquo; as continuous:</p>
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

  // ── 5. MODEL ──
  {
    id: 'model', type: 'split',
    label: '3. THE GENERATIVE MODEL',
    title: 'Bayesian Hierarchical Logistic Regression',
    visualSrc: './assets/22_mcmc_convergence_lr.gif',
    visualCaption: 'MCMC convergence (4 chains \u00D7 2,000 iterations)',
    text: (<>We use <code>brms</code> to fit a <Tooltip term="GLMM">GLMM</Tooltip> with Bernoulli likelihood, logit link, and <Tooltip term="Partial Pooling">partial pooling</Tooltip> for subjects and items. <CodeLink label="Model" /></>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">MATHEMATICAL FORMULATION</div>
        <div className="equation-stack">
          <BlockMath math="y_{ijk} \sim \text{Bernoulli}(\theta_{ijk})" />
          <BlockMath math="\text{logit}(\theta_{ijk}) = \beta_0 + \beta_j \cdot \mathbf{X}_j + u_i + w_k" />
          <BlockMath math="u_i \sim \mathcal{N}(0, \sigma_u), \quad w_k \sim \mathcal{N}(0, \sigma_w)" />
        </div>
        <div className="formal-header" style={{ marginTop: '1rem' }}>R / BRMS SYNTAX</div>
        <pre><code>{`model <- brm(
  accuracy ~ contrast_type + (1|subject_id) + (1|item_id),
  family = bernoulli(link = "logit"),
  prior = c(prior(normal(0, 1.5), class = Intercept),
            prior(normal(0, 1.5), class = b)),
  iter = 2000, chains = 4, seed = 2025)`}</code></pre>
      </div>
    ),
    footer: { question: 'What statistical model captures this?', summary: 'Bernoulli GLMM with partial pooling via brms.', takeHome: 'Hierarchical logistic regression captures binary accuracy with individual variation.' }
  },

  // ── 6. PRIORS ──
  {
    id: 'priors', type: 'split',
    label: '4. PRIOR SPECIFICATION',
    title: 'Weakly Informative Regularization',
    visualSrc: './assets/34_prior_to_posterior_updating.gif',
    visualCaption: 'Prior (wide) \u2192 Posterior (tight) updating',
    text: (<><Tooltip term="Weakly Informative">Weakly informative priors</Tooltip> regularize against overfitting with N=20. The prior is agnostic about effect direction.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">WHY NORMAL(0, 1.5)?</div>
        <BlockMath math="\beta \sim \mathcal{N}(0, 1.5)" />
        <p>On log-odds scale, \u00B12 SDs (\u00B13.0) map to probabilities of about 5% to 95%. This rules out implausible extremes without biasing direction.</p>
        <div className="formal-header" style={{ marginTop: '1rem' }}>WHY EXPONENTIAL(1)?</div>
        <BlockMath math="\sigma_u, \sigma_w \sim \text{Exp}(1)" />
        <p className="formal-note">Highest density near zero; allows large variance if data demand.</p>
      </div>
    ),
    footer: { question: 'How are priors specified?', summary: 'Prior-to-posterior updating shows data overwhelms priors.', takeHome: 'Normal(0, 1.5) is agnostic yet regularizing; results are data-driven.' }
  },

  // ── 7. FOREST ──
  {
    id: 'forest', type: 'split',
    label: '5. RESULTS: FOREST PLOT',
    title: 'Contrast Effects on Log-Odds',
    visualSrc: './assets/29_contrast_effect_intervals.gif',
    visualCaption: 'Posterior intervals stabilizing by contrast',
    text: (<>Negative log-odds = decreased probability of correct response. LR and H are shifted <strong>substantially left</strong>, with 95% credible intervals entirely excluding zero.</>),
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

  // ── 8. ERROR RATES ──
  {
    id: 'error_rates', type: 'split',
    label: '6. PREDICTED ERROR RATES',
    title: 'Probability-Scale Interpretation',
    visualSrc: './assets/30_error_growth_by_contrast.gif',
    visualCaption: 'Predicted error rates vs. observed data',
    text: (<>Transforming log-odds to <strong>probabilities</strong>: LR produces ~21% errors, 10\u00D7 higher than the F baseline (~2%).</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">ERROR RATE COMPARISON</div>
        <div className="stat-grid">
          <div className="stat-item"><span className="stat-label">F (Control)</span><span className="stat-val">~2%</span></div>
          <div className="stat-item"><span className="stat-label">PB</span><span className="stat-val">~6%</span></div>
          <div className="stat-item"><span className="stat-label">H</span><span className="stat-val">~24%</span></div>
          <div className="stat-item"><span className="stat-label">LR</span><span className="stat-val">~21%</span></div>
        </div>
        <p className="formal-note">LR and H are <strong>statistically indistinguishable</strong> \u2014 the core prediction of Representational Indeterminacy.</p>
      </div>
    ),
    footer: { question: 'What do effects mean on the probability scale?', summary: 'Predicted error rates: LR and H both near 20%, far above F baseline.', takeHome: 'LR and H are statistically indistinguishable \u2014 confirming the core prediction.' }
  },

  // ── 9. LINGUISTIC HIERARCHY ──
  {
    id: 'linguistic', type: 'split',
    label: '7. LINGUISTIC HIERARCHY',
    title: 'Grouping by Phonological Status',
    visualSrc: './assets/32_posterior_interference_strength.gif',
    visualCaption: 'Posterior interference strength by category',
    text: (<>When grouped by <strong>phonological status</strong> (Unrelated, L1-Present, L1-Absent, Homophone), L1-Absent clusters with Homophone.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">THEORETICAL ORDERING</div>
        <BlockMath math="\beta_{\text{Unrelated}} > \beta_{\text{L1-present}} \gg \beta_{\text{L1-absent}} \approx \beta_{\text{Homophone}}" />
        <p className="formal-note">The relevant dimension is not the <em>specific</em> contrast but <strong>whether it exists in L1</strong>.</p>
      </div>
    ),
    footer: { question: 'Does phonological status explain the pattern?', summary: 'Interference strength grouped by L1 phonological category.', takeHome: 'The key dimension is whether the contrast exists in L1, not which contrast.' }
  },

  // ── 10. DISTINCTNESS ──
  {
    id: 'distinctness', type: 'split',
    label: '8. THE MECHANISM',
    title: 'Gradient Distinctness',
    visualSrc: './assets/31_distinctness_predicts_errors.gif',
    visualCaption: 'Distinctness scores predicting error probabilities',
    text: (<>Each unit increase in <strong>phonological distinctness</strong> (based on L1 inventory) monotonically reduces L2 confusion. The mechanism is <strong>gradient</strong>, not categorical.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">DISTINCTNESS MODEL</div>
        <BlockMath math="\eta_{ijk} = \beta_0 + \beta_d \cdot d_j + u_i + w_k" />
        <p>where <InlineMath math="d_j \in \{0.0, 0.3, 0.8, 1.0\}" /></p>
        <div className="formal-header" style={{ marginTop: '1rem' }}>R / BRMS</div>
        <pre><code>{`model_dist <- brm(
  accuracy ~ phon_distinctness_scaled +
    (1|subject_id) + (1|item_id),
  family = bernoulli(link = "logit"))`}</code></pre>
      </div>
    ),
    footer: { question: 'Is the relationship categorical or gradient?', summary: 'Distinctness scores predict error probabilities monotonically.', takeHome: 'Phonological distinctness is a continuous predictor, not binary.' }
  },

  // ── 11. HALFEYE ──
  {
    id: 'halfeye', type: 'split',
    label: '9. POSTERIOR VISUALIZATION',
    title: 'Gradient-Shaded Posteriors',
    visualSrc: './assets/27_posterior_densities_by_contrast.gif',
    visualCaption: 'Posterior densities evolving',
    text: (<>The <code>ggdist</code> package reveals full distributional uncertainty. LR shows a <strong>narrow, dark core</strong> (high precision), while PB is diffuse near zero.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">R / GGDIST</div>
        <pre><code>{`posterior_samples |>
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(
    .width = c(.66, .95),
    fill = "indigo", alpha = 0.7
  )`}</code></pre>
        <p className="formal-note">The probability that the LR effect is negative exceeds 99.9% \u2014 near-certainty of impairment.</p>
      </div>
    ),
    footer: { question: 'What does the full posterior look like?', summary: 'Halfeye densities with narrow LR core showing high precision.', takeHome: 'Near-certainty that LR impairs accuracy (>99.9% posterior mass below zero).' }
  },

  // ── 12. ITEMS ──
  {
    id: 'items', type: 'split',
    label: '10. ITEM-LEVEL ROBUSTNESS',
    title: 'No Single Outlier Drives the Effect',
    visualSrc: './assets/12_item_level_robustness.png',
    visualCaption: 'Error rate for every word pair, grouped by contrast',
    text: (<>LR items show <strong>systematically elevated</strong> error rates \u2014 not driven by a few &ldquo;weird&rdquo; pairs. Some LR pairs (LAG\u2013CLOTH) reach 100% errors; others (WRONG\u2013SHORT) near 0%.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">WITHIN-LR VARIABILITY</div>
        <p>What drives item-level differences?</p>
        <ul className="findings-list">
          <li>Word frequency?</li>
          <li>Phonological neighbourhood density?</li>
          <li>Position of /l/-/r/ in the word?</li>
        </ul>
        <p className="formal-note">Future direction: lexical-item-level theory of Representational Indeterminacy.</p>
      </div>
    ),
    footer: { question: 'Do a few outlier items drive the effect?', summary: 'Error rates for all word pairs, grouped by contrast type.', takeHome: 'LR items are systematically elevated \u2014 not driven by a few outliers.' }
  },

  // ── 13. SUBJECTS ──
  {
    id: 'subjects', type: 'split',
    label: '11. SUBJECT-LEVEL UNIVERSALITY',
    title: 'Individual Differences',
    visualSrc: './assets/24_subject_caterpillar.png',
    visualCaption: 'Subject random intercepts (caterpillar plot)',
    text: (<>The <Tooltip term="Caterpillar Plot">caterpillar plot</Tooltip> reveals variation in baseline accuracy, but <Tooltip term="Partial Pooling">partial pooling</Tooltip> pulls extremes toward the mean.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">RANDOM EFFECTS</div>
        <pre><code>{`ranef(model)$subject_id |>
  as_tibble(rownames = "subject") |>
  ggplot(aes(y = reorder(subject, Estimate))) +
  geom_pointrange(aes(x = Estimate,
    xmin = Q2.5, xmax = Q97.5))`}</code></pre>
      </div>
    ),
    footer: { question: 'How much do individuals vary?', summary: 'Caterpillar plot of subject random intercepts with 95% CrIs.', takeHome: 'Individual differences exist but shrinkage pulls extremes toward the mean.' }
  },

  // ── 14. ACCUMULATION ──
  {
    id: 'accumulation', type: 'split',
    label: '12. EVIDENCE ACCUMULATION',
    title: 'Effect Emerges Early, Stays Stable',
    visualSrc: './assets/37_evidence_accumulation.gif',
    visualCaption: 'Cumulative accuracy as subjects are added (1 \u2192 20)',
    text: (<>By Subject 10 (halfway), the LR disadvantage is <strong>clearly established and stable</strong> \u2014 not a fragile artifact of a few extreme participants.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">RELIABILITY CHECK</div>
        <ul className="findings-list">
          <li>Effect size stabilizes early</li>
          <li>Adding more participants does not flip conclusions</li>
          <li>Variance decreases monotonically</li>
        </ul>
      </div>
    ),
    footer: { question: 'Is the effect robust to sample size?', summary: 'Cumulative accuracy stabilizing as participants are added.', takeHome: 'By Subject 10, the LR disadvantage is clearly established and stable.' }
  },

  // ── 15. ROPE ──
  {
    id: 'rope', type: 'split',
    label: '13. PAIRWISE INFERENCE',
    title: 'Region of Practical Equivalence',
    visualSrc: './assets/33_lr_indeterminacy_zoom.gif',
    visualCaption: 'LR \u2248 H equivalence (ROPE)',
    text: (<>The <Tooltip term="ROPE">ROPE</Tooltip> test (\u00B10.05 log-odds) shows <strong>LR and H are practically equivalent</strong>, while <strong>LR differs credibly from PB</strong>. <CodeLink label="ROPE" /></>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">R / BRMS</div>
        <pre><code>{`hypothesis(model, "contrastTR_LR = 0",
  rope = c(-0.05, 0.05))
# LR-H: 42% in ROPE → equivalent`}</code></pre>
      </div>
    ),
    footer: { question: 'Are LR and H truly equivalent?', summary: 'LR\u2013H difference falls within the ROPE band.', takeHome: 'LR and H are equivalent; LR and PB are credibly different.' }
  },

  // ── 16. VALIDATION ──
  {
    id: 'validation', type: 'split',
    label: '14. BAYESIAN VALIDATION',
    title: 'Convergence, PPC, Sensitivity, LOO-CV',
    visualSrc: './assets/35_mcmc_posterior_sampling.gif',
    visualCaption: 'MCMC sampling from the posterior',
    text: (<>Four validation layers: <Tooltip term="MCMC">MCMC</Tooltip> diagnostics, <Tooltip term="PPC">posterior predictive checks</Tooltip>, prior sensitivity, and <Tooltip term="LOO-CV">LOO cross-validation</Tooltip>. <CodeLink label="Diagnostics" /></>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">MCMC DIAGNOSTICS</div>
        <p>R-hat near 1.00 for all parameters (good convergence). No divergent transitions. Effective sample size &gt;400.</p>
        <div className="formal-header" style={{ marginTop: '1rem' }}>SENSITIVITY</div>
        <p>Wider priors (<InlineMath math="\mathcal{N}(0,3.0)" />) yield <strong>identical posteriors</strong>. Results are data-driven.</p>
        <div className="formal-header" style={{ marginTop: '1rem' }}>LOO-CV</div>
        <p>Comprehensive model and Linguistic model perform comparably. Distinctness model achieves competitive fit with a single predictor.</p>
      </div>
    ),
    footer: { question: 'Can we trust the model?', summary: 'MCMC diagnostics, PPC, sensitivity, and LOO-CV all pass.', takeHome: 'Good convergence, no divergences, prior-insensitive, competitive LOO-CV.' }
  },

  // ── 17. SPECTRUM (NEW) ──
  {
    id: 'spectrum', type: 'split',
    label: '15. WORD PAIR SPECTRUM',
    title: 'All 258 Pairs, Ranked',
    visualSrc: './assets/proto_A_ranked_dot_chart.png',
    visualCaption: 'All word pairs ranked by posterior error rate',
    text: (<>Every word pair ranked by its posterior mean error rate. <strong>LR pairs cluster at the top</strong>, but within-category variability reveals item-level effects beyond contrast type.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">ITEM-LEVEL INSIGHTS</div>
        <ul className="findings-list">
          <li>LR pairs dominate the high-error end</li>
          <li>Some LR pairs approach 100% error (e.g., LAG\u2013CLOTH)</li>
          <li>F pairs consistently cluster near 0%</li>
          <li>H pairs show moderate variability</li>
        </ul>
      </div>
    ),
    footer: { question: 'How do individual word pairs rank?', summary: 'All 258 pairs ranked by posterior error, coloured by contrast.', takeHome: 'LR pairs cluster high; within-LR variability suggests item-level effects.' }
  },

  // ── 18. HEATMAP (NEW) ──
  {
    id: 'heatmap', type: 'split',
    label: '16. SUBJECT \u00D7 CONTRAST',
    title: 'Who Struggles With What?',
    visualSrc: './assets/38_subject_contrast_heatmap.png',
    visualCaption: 'Subject \u00D7 Contrast heatmap',
    text: (<>The heatmap shows that <strong>LR difficulty is universal</strong> across all 20 subjects. No subgroup drives the effect \u2014 every participant shows elevated LR errors.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">INTERACTION PATTERN</div>
        <ul className="findings-list">
          <li>F column: uniformly light (low errors)</li>
          <li>LR column: uniformly dark (high errors)</li>
          <li>H column: moderate-to-dark</li>
          <li>Rows vary in baseline, but the LR effect persists</li>
        </ul>
      </div>
    ),
    footer: { question: 'Is the LR effect driven by a few subjects?', summary: 'Heatmap of error rates by subject and contrast.', takeHome: 'Every participant shows elevated LR errors \u2014 population-level effect.' }
  },

  // ── 19. PAIRWISE ROPE (NEW) ──
  {
    id: 'pairwise_rope', type: 'split',
    label: '17. FULL PAIRWISE COMPARISONS',
    title: 'The Complete Inferential Hierarchy',
    visualSrc: './assets/39_pairwise_contrast_rope.png',
    visualCaption: 'All 6 pairwise comparisons with ROPE',
    text: (<>All six pairwise differences tested against the <Tooltip term="ROPE">ROPE</Tooltip>. The hierarchy is confirmed: <strong>LR and H are equivalent; both are much worse than PB and F</strong>.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">PAIRWISE RESULTS</div>
        <div className="stat-grid">
          <div className="stat-item"><span className="stat-label">LR \u2212 H</span><span className="stat-val">Equivalent</span></div>
          <div className="stat-item"><span className="stat-label">LR \u2212 PB</span><span className="stat-val">Credible</span></div>
          <div className="stat-item"><span className="stat-label">PB \u2212 F</span><span className="stat-val">Equivalent</span></div>
        </div>
        <p className="formal-note">LR and H overlap within ROPE; LR and PB are credibly different. The L1-absent contrast collapses the distinction.</p>
      </div>
    ),
    footer: { question: 'Which contrasts are distinguishable?', summary: 'All six pairwise posterior differences with ROPE bands.', takeHome: 'LR and H equivalent; both credibly worse than PB and F.' }
  },

  // ── 20. SUMMARY (NEW) ──
  {
    id: 'findings_summary', type: 'split',
    label: '18. SUMMARY OF FINDINGS',
    title: 'What We Found',
    visualContent: (
      <div className="summary-grid">
        <div className="summary-card"><h3>Indeterminacy Confirmed</h3><p>L/R pairs produce <strong>~21% error rates</strong>, matching true homophones (~24%). The L1-absent contrast collapses in L2 storage.</p></div>
        <div className="summary-card"><h3>Gradient, Not Binary</h3><p>Phonological distinctness is a <strong>continuous predictor</strong>. Each unit increase monotonically reduces confusion.</p></div>
        <div className="summary-card"><h3>Universal Across Subjects</h3><p>All 20 participants show elevated LR errors. Hierarchical modeling confirms the effect <strong>generalizes</strong>.</p></div>
        <div className="summary-card"><h3>Bayesian-Validated</h3><p>Good convergence, zero divergences, prior-insensitive, competitive LOO-CV. Results are robust.</p></div>
      </div>
    ),
    text: (<>Four key findings, each supported by multiple converging lines of evidence.</>),
    formal: (
      <div className="formal-block">
        <div className="formal-header">EVIDENCE SUMMARY</div>
        <p className="formal-note" style={{ marginBottom: '0.5rem' }}>Key hierarchy: LR and H are similar; both much worse than PB and F:</p>
        <BlockMath math="\text{LR} \approx \text{H} \gg \text{PB} \approx \text{F}" />
      </div>
    ),
    footer: { question: 'What are the main takeaways?', summary: 'Four-box summary of the core findings.', takeHome: 'L1 phonology shapes L2 storage; the effect is gradient, universal, and validated.' }
  },

  // ── 21. LIMITATIONS (NEW) ──
  {
    id: 'limitations', type: 'split',
    label: '19. LIMITATIONS & FUTURE',
    title: 'Caveats and Next Steps',
    visualContent: (
      <div className="limitations-content">
        <div className="limitation-section">
          <h3 className="limitation-heading">Limitations</h3>
          <ul className="limitations-list">
            <li><strong>Small N (20):</strong> Partial pooling mitigates but does not eliminate sample-size concerns.</li>
            <li><strong>Single L1:</strong> Only Japanese speakers. Pattern may differ for Korean or Mandarin L1s.</li>
            <li><strong>Visual-only task:</strong> Eliminates auditory confounds but limits ecological validity.</li>
            <li><strong>Binary DV:</strong> Response times could add a continuous measure of processing difficulty.</li>
            <li><strong>Item selection:</strong> Word frequency and neighbourhood density not controlled.</li>
          </ul>
        </div>
        <div className="limitation-section">
          <h3 className="limitation-heading">Future Directions</h3>
          <ul className="limitations-list future-list">
            <li>Cross-linguistic replication (Korean, Mandarin, Thai L1)</li>
            <li>Item-level Bayesian models with lexical predictors</li>
            <li>Response time analysis alongside accuracy</li>
            <li>Longitudinal design tracking L2 proficiency</li>
          </ul>
        </div>
      </div>
    ),
    text: (<>Every study has boundaries. Ours are clearly defined and suggest productive extensions.</>),
    footer: { question: 'What should we be cautious about?', summary: 'Five limitations and four future directions.', takeHome: 'Small N and single L1 are real caveats; hierarchical modelling partly compensates.' }
  },

  // ── 22. REFERENCES (NEW) ──
  {
    id: 'references', type: 'split',
    label: '20. REFERENCES',
    title: 'Key References',
    visualContent: (
      <div className="references-list">
        <div className="ref-item">B\u00FCrkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. <em>Journal of Statistical Software, 80</em>(1), 1\u201328.</div>
        <div className="ref-item">Jiao, L., et al. (2024). The role of orthography in nonnative phonological processing. <em>Language Learning</em>.</div>
        <div className="ref-item">Kruschke, J. K. (2018). Rejecting or Accepting Parameter Values in Bayesian Estimation. <em>AMPPS, 1</em>(2), 270\u2013280.</div>
        <div className="ref-item">McElreath, R. (2020). <em>Statistical Rethinking</em> (2nd ed.). CRC Press.</div>
        <div className="ref-item">Ota, M., Hartsuiker, R. J., & Haywood, S. L. (2009). The KEY to the ROCK: Near-homophony in nonnative visual word recognition. <em>Cognition, 111</em>(2), 263\u2013269.</div>
        <div className="ref-item">Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model evaluation using LOO-CV and WAIC. <em>Statistics and Computing, 27</em>(5), 1413\u20131432.</div>
      </div>
    ),
    text: (<>Selected references for the theoretical framework, statistical methodology, and implementation.</>),
  },

  // ── 23. CONCLUSION ──
  {
    id: 'conclusion', type: 'hero',
    title: 'Structural. Gradient. Robust.',
    subtitle: 'The L1 phonological inventory shapes the L2 lexicon. The \u201CKey\u201D to the \u201CRock\u201D is indeed the \u201CLock.\u201D',
    credit: 'R + brms + ggdist + tidyverse \u00B7 React + KaTeX \u00B7 github.com/sandriatran/qml-2025',
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
    setTimeout(() => { setCurrentIndex(index); setIsTransitioning(false); }, 300);
  }, [currentIndex, isTransitioning, totalSlides]);

  const goNext = useCallback(() => goToSlide(currentIndex + 1), [currentIndex, goToSlide]);
  const goPrev = useCallback(() => goToSlide(currentIndex - 1), [currentIndex, goToSlide]);

  useEffect(() => {
    const handleKeyDown = (e) => {
      if (showOverview && e.key === 'Escape') { setShowOverview(false); return; }
      if (e.key === 'ArrowRight' || e.key === ' ') { e.preventDefault(); goNext(); }
      if (e.key === 'ArrowLeft') { e.preventDefault(); goPrev(); }
      if (e.key === 'm' || e.key === 'M') setShowFormal(prev => !prev);
      if (e.key === 'o' || e.key === 'O') setShowOverview(prev => !prev);
      if (e.key === 'd' || e.key === 'D') toggleTheme();
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [goNext, goPrev, showOverview, toggleTheme]);

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
              <div className="visual-frame">
                {currentSlide.visualSrc ? <img src={currentSlide.visualSrc} className="visual-img" alt="Evidence" /> : currentSlide.visualContent}
                {currentSlide.visualCaption && <div className="visual-caption">{currentSlide.visualCaption}</div>}
              </div>
            </div>
            <div className="split-right">
              <div className="content-header">
                <span className="slide-label">{currentSlide.label}</span>
                <h2 className="content-title">{currentSlide.title}</h2>
              </div>
              <div className="narrative-text">{currentSlide.text}</div>
              {showFormal && currentSlide.formal}
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
      </nav>

      {/* ── Keyboard hint (first slide only) ── */}
      {currentIndex === 0 && (
        <div className="keyboard-hint">&larr; &rarr; navigate &middot; M math &middot; D theme</div>
      )}
      {currentIndex > 0 && (
        <div className="keyboard-hint">{currentIndex + 1} / {totalSlides}</div>
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
    </div>
  );
}

export default App;

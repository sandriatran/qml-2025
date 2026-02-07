import React, { useState } from 'react';
import './GlossarySidebar.css';

const GLOSSARY = {
  // ── Linguistics ──
  'Phoneme': 'Smallest unit of sound that distinguishes meaning in a language.',
  'L1': 'First language (native). For this study: Japanese.',
  'L2': 'Second language. For this study: English.',
  'Homophone': 'Words with identical pronunciation but different meanings (e.g., SUN / SON).',
  'Near-Homophone': 'Words differing by a contrast absent in the speaker\'s L1, functionally equivalent to homophones.',
  'Phonological Contrast': 'Sound difference that distinguishes words in a language.',
  'Representational Indeterminacy': 'L1-absent contrast collapses in L2 storage; two distinct words share one lexical representation.',
  'False Positive': 'Incorrectly judging an unrelated word pair as semantically related (the DV in this study).',
  'L1-Absent': 'Contrast not in speaker\'s native language (e.g., /l/-/r/ for Japanese speakers).',
  'L1-Present': 'Contrast present in speaker\'s native language (e.g., /p/-/b/ for Japanese speakers).',
  'Semantic Relatedness': 'Degree of meaning-based relationship between two words.',
  'Orthography': 'The written or spelling system of a language.',

  // ── Bayesian Statistics ──
  'Posterior': 'Updated distribution after combining prior beliefs with observed data via Bayes\' theorem.',
  'Prior': 'Initial distribution expressing beliefs before seeing data.',
  'Credible Interval': 'Bayesian range containing the true parameter with specified probability (e.g., 95% CrI).',
  'ROPE': 'Region of Practical Equivalence (\u00B10.05 log-odds); differences inside are negligible.',
  'MCMC': 'Markov Chain Monte Carlo: sampling algorithm for approximating posterior distributions.',
  'Divergent Transitions': 'Sampling pathology in HMC/NUTS indicating geometric difficulties; zero is ideal.',
  'R-hat': 'Gelman\u2013Rubin convergence diagnostic. Values \u2248 1.00 mean chains mixed well.',
  'ESS': 'Effective Sample Size: independent draws equivalent to correlated MCMC output.',
  'LOO-CV': 'Leave-One-Out Cross-Validation: Bayesian model comparison via predictive accuracy (ELPD).',
  'PPC': 'Posterior Predictive Check: comparing model-simulated data to observed data.',
  'Weakly Informative': 'Prior constraining implausible extremes without biasing effect direction.',
  'Log-Odds': 'Scale of logistic regression coefficients. 0 = 50/50 probability.',
  'Odds Ratio': 'Ratio of odds between groups. OR < 1 = reduced odds; OR > 1 = increased odds.',

  // ── R / brms ──
  'brms': 'R package for Bayesian regression via Stan (Bayesian Regression Models using Stan).',
  'GLMM': 'Generalized Linear Mixed Model: regression for non-normal outcomes with random effects.',
  'Partial Pooling': 'Hierarchical shrinkage: individual estimates pulled toward the group mean, reducing overfitting.',
  'Random Effects': 'Subject- or item-level deviations from the population mean.',
  'Caterpillar Plot': 'Individual random effects with 95% credible intervals, ordered by magnitude.',
  'Halfeye Plot': 'Combined density + interval visualization from the ggdist package.',
  'Bernoulli': 'Distribution for binary outcomes (0/1). Likelihood for trial-level accuracy data.',
};

const SLIDE_TERMS = {
  'title': [],
  'summary': ['False Positive', 'ROPE', 'Posterior', 'Credible Interval'],
  'theory': ['Phoneme', 'L1', 'L2', 'Representational Indeterminacy', 'Phonological Contrast'],
  'design': ['False Positive', 'Homophone', 'Near-Homophone', 'L1-Absent', 'L1-Present'],
  'coin_flip': ['Bernoulli', 'Log-Odds', 'Random Effects'],
  'logit_link': ['GLMM', 'Partial Pooling', 'Random Effects', 'Log-Odds', 'brms'],
  'model': ['GLMM', 'Bernoulli', 'Partial Pooling', 'Random Effects', 'brms', 'Log-Odds'],
  'priors': ['Prior', 'Weakly Informative', 'Log-Odds'],
  'forest': ['Posterior', 'Credible Interval', 'Log-Odds', 'Odds Ratio'],
  'error_rates': ['False Positive', 'Log-Odds'],
  'linguistic': ['L1-Absent', 'L1-Present', 'Phonological Contrast'],
  'distinctness': ['Representational Indeterminacy', 'Random Effects'],
  'halfeye': ['Posterior', 'Halfeye Plot', 'Credible Interval'],
  'items': ['Random Effects', 'False Positive'],
  'subjects': ['Caterpillar Plot', 'Random Effects', 'Partial Pooling'],
  'accumulation': ['Posterior'],
  'rope': ['ROPE', 'Credible Interval', 'Posterior'],
  'validation': ['MCMC', 'R-hat', 'ESS', 'PPC', 'LOO-CV', 'Divergent Transitions'],
  'spectrum': ['False Positive', 'Near-Homophone', 'L1-Absent'],
  'heatmap': ['Random Effects', 'L1-Absent', 'False Positive'],
  'pairwise_rope': ['ROPE', 'Credible Interval', 'Posterior'],
  'findings_summary': ['Representational Indeterminacy', 'ROPE'],
  'limitations': ['Random Effects', 'LOO-CV', 'Partial Pooling'],
  'references': [],
  'conclusion': [],
};

const GlossarySidebar = ({ currentSlideId }) => {
  const [isOpen, setIsOpen] = useState(false);
  const terms = SLIDE_TERMS[currentSlideId] || [];

  return (
    <div className={`glossary-sidebar ${isOpen ? 'open' : 'closed'}`}>
      <button
        className="glossary-toggle"
        onClick={() => setIsOpen(!isOpen)}
        aria-label={isOpen ? 'Close glossary' : 'Open glossary'}
      >
        <span className="glossary-toggle-icon">{isOpen ? '\u00D7' : '?'}</span>
      </button>
      {isOpen && (
        <div className="glossary-panel">
          <h4 className="glossary-panel-title">Key Terms</h4>
          {terms.length === 0 ? (
            <p className="glossary-empty">No key terms for this slide.</p>
          ) : (
            <dl className="glossary-definitions">
              {terms.map(term => (
                <div key={term} className="glossary-entry">
                  <dt>{term}</dt>
                  <dd>{GLOSSARY[term]}</dd>
                </div>
              ))}
            </dl>
          )}
        </div>
      )}
    </div>
  );
};

export default GlossarySidebar;

import React, { useState } from 'react';
import './GlossarySidebar.css';

const GLOSSARY = {
  // ── Linguistics ──
  'Phoneme': 'Smallest unit of sound that distinguishes meaning in a language.',
  'L1': 'First acquired language. For this study: Japanese.',
  'LX': 'Any subsequently acquired language (replaces L2/L3/Ln). For this study: English.',
  'L2': 'Second language (see LX). For this study: English.',
  'Homophone': 'Words with identical pronunciation but different meanings (e.g., SUN / SON).',
  'Near-Homophone': 'Words differing by a contrast absent in the speaker\'s L1, functionally equivalent to homophones.',
  'Phonological Contrast': 'Sound difference that distinguishes words in a language.',
  'Representational Indeterminacy': 'L1-absent contrast collapses in L2 storage; two distinct words share one lexical representation.',
  'False Positive': 'Incorrectly judging an unrelated word pair as semantically related (the DV in this study).',
  'L1-Absent': 'Contrast not present in the speaker\'s L1 (e.g., /l/-/r/ for Japanese L1 users).',
  'L1-Present': 'Contrast present in the speaker\'s L1 (e.g., /p/-/b/ for Japanese L1 users).',
  'Semantic Relatedness': 'Degree of meaning-based relationship between two words.',
  'Orthography': 'The written or spelling system of a language.',

  // ── Bayesian Statistics ──
  'Posterior': 'Updated distribution after combining prior beliefs with observed data via Bayes\' theorem.',
  'Prior': 'Initial distribution expressing beliefs before seeing data.',
  'Credible Interval': 'Bayesian range containing the true parameter with specified probability (e.g., 95% CrI).',
  'ROPE': 'Region of Practical Equivalence (\u00B10.18 log-odds, OR \u2248 0.84\u20131.20); differences inside are negligible.',
  'MCMC': 'Markov Chain Monte Carlo: sampling algorithm for approximating posterior distributions.',
  'Divergent Transitions': 'Sampling pathology in HMC/NUTS indicating geometric difficulties; zero is ideal.',
  'R-hat': 'Gelman\u2013Rubin convergence diagnostic. Values \u2248 1.00 mean chains mixed well.',
  'ESS': 'Effective Sample Size: independent draws equivalent to correlated MCMC output.',
  'LOO-CV': 'Leave-One-Out Cross-Validation: Bayesian model comparison via predictive accuracy (ELPD).',
  'PPC': 'Posterior Predictive Check: comparing model-simulated data to observed data.',
  'Weakly Informative': 'Prior constraining implausible extremes without biasing effect direction.',
  'Log-Odds': 'Scale of logistic regression coefficients. 0 = 50/50 probability.',
  'Odds Ratio': 'Ratio of odds between groups. OR < 1 = reduced odds; OR > 1 = increased odds.',

  // ── Tools & Framework ──
  'Bayesian Statistics': 'A statistical framework that updates beliefs (priors) with observed data to produce posterior distributions, quantifying uncertainty directly.',
  'R Programming': 'Open-source programming language for statistical computing and graphics, widely used in academia and data science.',
  'brms': 'R package for Bayesian regression via Stan (Bayesian Regression Models using Stan).',
  'GLMM': 'Generalized Linear Mixed Model: regression for non-normal outcomes with random effects.',
  'Partial Pooling': 'Hierarchical shrinkage: individual estimates pulled toward the group mean, reducing overfitting.',
  'Random Effects': 'Subject- or item-level deviations from the population mean.',
  'Caterpillar Plot': 'Individual random effects with 95% credible intervals, ordered by magnitude.',
  'Halfeye Plot': 'Combined density + interval visualization from the ggdist package.',
  'Bernoulli': 'Distribution for binary outcomes (0/1). Like a coin flip \u2014 each trial has two possible results.',
  'Likelihood': 'How probable the observed data are, given a specific model. Higher = better fit.',
  'Hierarchical Model': 'A model with multiple levels (e.g., trials within subjects), sharing information across levels.',
  'Convergence': 'When the sampling algorithm has explored enough to give reliable estimates. Checked via R-hat.',
  'Sensitivity Analysis': 'Re-running the model with different assumptions to check if conclusions change.',
  'Phonological': 'Related to the sound system of a language \u2014 how speech sounds are organized and distinguished.',
  'Lexical': 'Related to the mental dictionary \u2014 how words are stored and accessed in the brain.',
  'Distinctness': 'How perceptually separable two sounds are. Ranges from 0 (identical) to 1 (completely different).',
};

const SLIDE_TERMS = {
  'title': ['Bayesian Statistics', 'brms', 'R Programming'],
  'summary': ['L1-Absent', 'False Positive', 'ROPE', 'Near-Homophone', 'Bernoulli', 'Phonological'],
  'phonological': ['Phoneme', 'L1', 'Representational Indeterminacy', 'False Positive', 'Near-Homophone'],
  'theory': ['L1', 'L2', 'Representational Indeterminacy', 'Phonological Contrast', 'Lexical'],
  'design': ['False Positive', 'Homophone', 'Near-Homophone', 'L1-Absent', 'L1-Present'],
  'coin_flip': ['Bernoulli', 'False Positive', 'L1-Absent'],
  'logit_link': ['Log-Odds', 'Partial Pooling', 'Random Effects', 'brms'],
  'model': ['GLMM', 'Bernoulli', 'Partial Pooling', 'Random Effects', 'brms', 'Log-Odds'],
  'priors': ['Prior', 'Weakly Informative', 'Log-Odds', 'Sensitivity Analysis'],
  'forest': ['Posterior', 'Credible Interval', 'Log-Odds', 'Odds Ratio'],
  'error_rates': ['False Positive', 'Log-Odds', 'Representational Indeterminacy'],
  'linguistic': ['L1-Absent', 'L1-Present', 'Phonological Contrast', 'LOO-CV', 'Homophone', 'Lexical'],
  'distinctness': ['Distinctness', 'Representational Indeterminacy', 'LOO-CV'],
  'halfeye': ['Posterior', 'Halfeye Plot', 'Credible Interval'],
  'items': ['Random Effects', 'Partial Pooling', 'L1-Absent'],
  'subjects': ['Caterpillar Plot', 'Random Effects', 'Partial Pooling'],
  'accumulation': ['Hierarchical Model', 'Partial Pooling'],
  'rope': ['ROPE', 'Credible Interval', 'Posterior'],
  'validation': ['MCMC', 'R-hat', 'ESS', 'PPC', 'LOO-CV', 'Divergent Transitions'],
  'spectrum': ['Posterior', 'Random Effects', 'L1-Absent'],
  'heatmap': ['Random Effects', 'L1-Absent', 'Partial Pooling'],
  'pairwise_rope': ['ROPE', 'Log-Odds', 'Posterior'],
  'findings_summary': ['Representational Indeterminacy', 'ROPE', 'LOO-CV'],
  'limitations': ['Partial Pooling', 'L1', 'Dependent Variable', 'Phonological'],
  'references': [],
  'conclusion': ['Representational Indeterminacy', 'L1', 'Lexical'],
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

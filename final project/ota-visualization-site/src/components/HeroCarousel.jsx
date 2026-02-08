import React, { useState, useEffect, useCallback } from 'react';
import './HeroCarousel.css';

// ── Frame metadata ──
const FRAMES = [
  { id: 'filter',   label: 'L1 Phonological Filter',     slideIndex: 2  },
  { id: 'gradient', label: 'Distinctness Gradient',       slideIndex: 12 },
  { id: 'coins',    label: 'Bernoulli Coin Model',        slideIndex: 5  },
  { id: 'forest',   label: 'Posterior Forest Plot',        slideIndex: 9  },
  { id: 'errors',   label: 'Error Rates by Contrast',     slideIndex: 10 },
];

// ── Frame 1: L1 Filter Mechanism ──
const FilterFrame = () => (
  <div className="cf-filter">
    <div className="cf-filter-row">
      <div className="cf-filter-node cf-node-word cf-anim-1">ROCK</div>
      <div className="cf-filter-arrow cf-anim-2">&rarr;</div>
      <div className="cf-filter-node cf-node-phon cf-anim-3">
        <span className="cf-ipa">/&#x0279;&#x0251;k/</span>
      </div>
      <div className="cf-filter-collapse cf-anim-4">
        <span className="cf-collapse-label">L1 filter</span>
        <span className="cf-collapse-arrows">&harr;</span>
      </div>
      <div className="cf-filter-node cf-node-phon cf-anim-5">
        <span className="cf-ipa">/l&#x0251;k/</span>
      </div>
      <div className="cf-filter-arrow cf-anim-6">&rarr;</div>
      <div className="cf-filter-node cf-node-word cf-anim-7">LOCK</div>
      <div className="cf-filter-arrow cf-anim-8">&rarr;</div>
      <div className="cf-filter-node cf-node-key cf-anim-9">KEY</div>
    </div>
    <div className="cf-filter-caption cf-anim-10">
      Japanese L1 speakers collapse /l/&ndash;/r/ &rarr; near-homophones activate related words
    </div>
  </div>
);

// ── Frame 2: Gradient Spectrum ──
const GradientFrame = () => (
  <div className="cf-gradient">
    <div className="cf-gradient-bar">
      <div className="cf-gradient-fill" />
      <div className="cf-gradient-markers">
        <div className="cf-gm cf-gm-f"  style={{ left: '2%' }}>
          <div className="cf-gm-dot" style={{ background: 'var(--color-lavender)' }} />
          <div className="cf-gm-label">F</div>
          <div className="cf-gm-val cf-count" data-target="0.02">0.02</div>
        </div>
        <div className="cf-gm cf-gm-pb" style={{ left: '25%' }}>
          <div className="cf-gm-dot" style={{ background: 'var(--color-purple)' }} />
          <div className="cf-gm-label">PB</div>
          <div className="cf-gm-val cf-count" data-target="0.30">0.30</div>
        </div>
        <div className="cf-gm cf-gm-h"  style={{ left: '72%' }}>
          <div className="cf-gm-dot" style={{ background: 'var(--color-hot-pink)' }} />
          <div className="cf-gm-label">H</div>
          <div className="cf-gm-val cf-count" data-target="0.82">0.82</div>
        </div>
        <div className="cf-gm cf-gm-lr" style={{ left: '95%' }}>
          <div className="cf-gm-dot" style={{ background: 'var(--color-indigo)' }} />
          <div className="cf-gm-label">LR</div>
          <div className="cf-gm-val cf-count" data-target="1.00">1.00</div>
        </div>
      </div>
    </div>
    <div className="cf-gradient-labels">
      <span>Fully Distinct</span>
      <span>Fully Indeterminate</span>
    </div>
  </div>
);

// ── Frame 3: Coin Bias Model ──
const COINS = [
  { code: 'F',  pct: 98, color: 'var(--color-lavender)',  desc: 'Control' },
  { code: 'PB', pct: 94, color: 'var(--color-purple)',    desc: 'L1-present' },
  { code: 'LR', pct: 79, color: 'var(--color-indigo)',    desc: 'L1-absent' },
  { code: 'H',  pct: 76, color: 'var(--color-hot-pink)',  desc: 'Homophone' },
];

const CoinsFrame = () => (
  <div className="cf-coins">
    {COINS.map((c, i) => (
      <div className="cf-coin-group" key={c.code} style={{ animationDelay: `${i * 0.15}s` }}>
        <div className="cf-coin" style={{ borderColor: c.color, animationDelay: `${i * 0.15 + 0.3}s` }}>
          <span className="cf-coin-code" style={{ color: c.color }}>{c.code}</span>
        </div>
        <div className="cf-coin-pct" style={{ color: c.color, animationDelay: `${i * 0.15 + 0.5}s` }}>
          {c.pct}%
        </div>
        <div className="cf-coin-desc">{c.desc}</div>
      </div>
    ))}
    <div className="cf-coins-formula">
      <span className="cf-coins-formula-text">
        y<sub>i</sub> ~ Bernoulli(&#x03B8;<sub>contrast</sub>)
      </span>
    </div>
  </div>
);

// ── Frame 4: Forest Plot ──
const FOREST_DATA = [
  { code: 'F',  center: 0,     lo: -0.3,  hi: 0.3,   color: 'var(--color-lavender)' },
  { code: 'PB', center: -0.45, lo: -1.0,  hi: 0.1,   color: 'var(--color-purple)' },
  { code: 'LR', center: -1.85, lo: -2.6,  hi: -1.1,  color: 'var(--color-indigo)' },
  { code: 'H',  center: -2.10, lo: -2.95, hi: -1.25,  color: 'var(--color-hot-pink)' },
];

// Map log-odds range [-3.5, 1] to percentage position
const lo2pct = (v) => ((v - (-3.5)) / (1 - (-3.5))) * 100;

const ForestFrame = () => (
  <div className="cf-forest">
    <div className="cf-forest-zero" style={{ left: `${lo2pct(0)}%` }}>
      <div className="cf-forest-zero-line" />
      <div className="cf-forest-zero-label">0</div>
    </div>
    {FOREST_DATA.map((d, i) => (
      <div className="cf-forest-row" key={d.code} style={{ animationDelay: `${i * 0.2}s` }}>
        <div className="cf-forest-label" style={{ color: d.color }}>{d.code}</div>
        <div className="cf-forest-track">
          <div
            className="cf-forest-interval"
            style={{
              left: `${lo2pct(d.lo)}%`,
              width: `${lo2pct(d.hi) - lo2pct(d.lo)}%`,
              background: d.color,
              animationDelay: `${i * 0.2 + 0.1}s`,
            }}
          />
          <div
            className="cf-forest-point"
            style={{
              left: `${lo2pct(d.center)}%`,
              background: d.color,
              animationDelay: `${i * 0.2 + 0.3}s`,
            }}
          />
        </div>
      </div>
    ))}
    <div className="cf-forest-axis-label">
      <span>&larr; More errors</span>
      <span>Log-odds</span>
      <span>Fewer errors &rarr;</span>
    </div>
  </div>
);

// ── Frame 5: Error Rate Bars ──
const ERROR_DATA = [
  { code: 'F',  pct: 2,  color: 'var(--color-lavender)' },
  { code: 'PB', pct: 6,  color: 'var(--color-purple)' },
  { code: 'LR', pct: 21, color: 'var(--color-indigo)' },
  { code: 'H',  pct: 24, color: 'var(--color-hot-pink)' },
];

const ErrorBarsFrame = () => (
  <div className="cf-errors">
    {ERROR_DATA.map((d, i) => (
      <div className="cf-error-row" key={d.code}>
        <div className="cf-error-label" style={{ color: d.color }}>{d.code}</div>
        <div className="cf-error-track">
          <div
            className="cf-error-bar"
            style={{
              '--bar-width': `${(d.pct / 30) * 100}%`,
              background: d.color,
              animationDelay: `${i * 0.15}s`,
            }}
          />
        </div>
        <div className="cf-error-pct" style={{ color: d.color, animationDelay: `${i * 0.15 + 0.4}s` }}>
          ~{d.pct}%
        </div>
      </div>
    ))}
    <div className="cf-errors-caption">False-positive error rates by contrast type</div>
  </div>
);

// ── Main Carousel ──
const HeroCarousel = ({ goToSlide }) => {
  const [activeFrame, setActiveFrame] = useState(0);
  const [isPaused, setIsPaused] = useState(false);
  const [fadeState, setFadeState] = useState('in'); // 'in' | 'out'

  const transitionTo = useCallback((nextIndex) => {
    setFadeState('out');
    setTimeout(() => {
      setActiveFrame(nextIndex);
      setFadeState('in');
    }, 350);
  }, []);

  // Auto-rotate
  useEffect(() => {
    if (isPaused) return;
    const timer = setInterval(() => {
      transitionTo((activeFrame + 1) % FRAMES.length);
    }, 6000);
    return () => clearInterval(timer);
  }, [isPaused, activeFrame, transitionTo]);

  const frame = FRAMES[activeFrame];

  const handleDotClick = (i) => {
    if (i === activeFrame) return;
    transitionTo(i);
  };

  return (
    <div
      className="hero-carousel-wrap"
      onMouseEnter={() => setIsPaused(true)}
      onMouseLeave={() => setIsPaused(false)}
    >
      <div
        className={`hero-carousel-frame ${fadeState === 'in' ? 'hc-fade-in' : 'hc-fade-out'}`}
        onClick={() => goToSlide(frame.slideIndex)}
        role="button"
        tabIndex={0}
        onKeyDown={(e) => { if (e.key === 'Enter') goToSlide(frame.slideIndex); }}
        title={`Go to: ${frame.label}`}
      >
        {activeFrame === 0 && <FilterFrame />}
        {activeFrame === 1 && <GradientFrame />}
        {activeFrame === 2 && <CoinsFrame />}
        {activeFrame === 3 && <ForestFrame />}
        {activeFrame === 4 && <ErrorBarsFrame />}
      </div>

      <div className="hero-carousel-footer">
        <span className="hero-carousel-label">{frame.label}</span>
        <div className="hero-carousel-dots">
          {FRAMES.map((f, i) => (
            <button
              key={f.id}
              className={`hero-carousel-dot ${i === activeFrame ? 'active' : ''}`}
              onClick={(e) => { e.stopPropagation(); handleDotClick(i); }}
              aria-label={f.label}
            />
          ))}
        </div>
      </div>
    </div>
  );
};

export default HeroCarousel;

import React, { useEffect, useState } from 'react';
import './WordPairFader.css';

// Centralized Data for WordPairFader (LR and F items)
const WORD_PAIRS = [
    { pair: "LOCK / ROCK", target: "ROCK", error: 0.60, cat: "LR", desc: "Classic L/R near-homophone" },
    { pair: "LANE / RAIN", target: "RAIN", error: 0.55, cat: "LR", desc: "High phonological confusion" },
    { pair: "LEAD / READ", target: "READ", error: 0.45, cat: "LR", desc: "Representational indeterminacy" },
    { pair: "GLASS / GRASS", target: "GRASS", error: 0.40, cat: "LR", desc: "Liquid liquid contrast" },
    { pair: "PILOT / PIRATE", target: "PIRATE", error: 0.35, cat: "LR", desc: "Medial /l/-/r/ confusion" },
    { pair: "FLY / FRY", target: "FRY", error: 0.28, cat: "LR", desc: "Consonant cluster contrast" },
    { pair: "HARD / LOCK", target: "ROCK", error: 0.52, cat: "LR", desc: "R-target semantic trap (KEY-ROCK)" },
    { pair: "LANE / WATER", target: "RAIN", error: 0.48, cat: "LR", desc: "R-target semantic trap (LANE-RAIN)" },
    { pair: "KING / WING", target: "KING", error: 0.04, cat: "F", desc: "Distinct L1 contrast (Control)" },
    { pair: "MAP / NAP", target: "MAP", error: 0.04, cat: "F", desc: "Distinct L1 contrast (Control)" },
    { pair: "CAT / BAT", target: "CAT", error: 0.02, cat: "F", desc: "Distinct L1 contrast (Control)" },
    { pair: "FISH / DISH", target: "FISH", error: 0.03, cat: "F", desc: "Distinct L1 contrast (Control)" },
    { pair: "SING / SINK", target: "SING", error: 0.05, cat: "F", desc: "Distinct L1 contrast (Control)" },
    { pair: "RIGHT / LIGHT", target: "LIGHT", error: 0.50, cat: "LR", desc: "Directional liquid pair" },
    { pair: "CLOUD / CROWD", target: "CROWD", error: 0.42, cat: "LR", desc: "L1-absent contrast" },
    { pair: "OFFICE / FOREST", target: "OFFICE", error: 0.02, cat: "F", desc: "Distinct Control Pair" },
    { pair: "TICKET / KITCHEN", target: "TICKET", error: 0.03, cat: "F", desc: "Distinct Control Pair" },
    { pair: "GARDEN / GARAGE", target: "GARDEN", error: 0.04, cat: "F", desc: "Distinct Control Pair" },
];

const WordPairFader = () => {
    const [visiblePairs, setVisiblePairs] = useState([]);

    useEffect(() => {
        // Helper to spawn a new pair
        const spawnPair = () => {
            const pairData = WORD_PAIRS[Math.floor(Math.random() * WORD_PAIRS.length)];
            const id = Date.now() + Math.random();

            // Random position avoiding the dead center (Title Area)
            let top = Math.random() * 80 + 10;
            let left = Math.random() * 85 + 5;

            const newPair = { ...pairData, id, top, left };
            setVisiblePairs(prev => [...prev.slice(-12), newPair]); // Keep max 12

            // Cleanup after animation
            setTimeout(() => {
                setVisiblePairs(prev => prev.filter(p => p.id !== id));
            }, 7000);
        };

        const interval = setInterval(spawnPair, 1500); // Slower spawn rhythm
        return () => clearInterval(interval);
    }, []);

    const getColor = (error, cat) => {
        if (cat === 'F') return 'rgba(108, 101, 252, 0.4)'; // Subdued Indigo/Blue
        if (error > 0.4) return `rgba(235, 87, 87, ${0.4 + error})`; // Vibrant Red
        return `rgba(255, 140, 0, ${0.4 + error})`; // Orangeish
    };

    return (
        <div className="word-pair-fader-container">
            {visiblePairs.map(p => (
                <div
                    key={p.id}
                    className="fading-word-pair"
                    style={{
                        top: `${p.top}%`,
                        left: `${p.left}%`,
                        color: getColor(p.error, p.cat),
                        animationDuration: '7s',
                        fontSize: `${0.8 + p.error * 1.5}rem`,
                        fontWeight: p.cat === 'LR' ? 600 : 400
                    }}
                >
                    {p.pair}
                    <div className="fader-tooltip">
                        <div className="tooltip-cat">{p.cat === 'LR' ? 'Near-Homophone' : 'Control'}</div>
                        <div className="tooltip-target">Target: <strong>{p.target}</strong></div>
                        <div className="tooltip-stat">{Math.round(p.error * 100)}% Error Rate</div>
                        <div className="tooltip-desc">{p.desc}</div>
                    </div>
                </div>
            ))}
        </div>
    );
};

export default WordPairFader;

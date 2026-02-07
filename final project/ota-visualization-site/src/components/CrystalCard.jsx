import React from 'react';
import './CrystalCard.css';

const CrystalCard = ({
    id,
    label,
    title,
    text,
    codeSnippet,
    visualSrc,
    visualIsImage = true, /* Default to static image for robustness */
    visualCaption
}) => {
    return (
        <section className="crystal-section" id={id}>
            <div className="crystal-card">
                {/* Header */}
                <div className="card-header">
                    <span className="card-label">{label}</span>
                    <h2 className="card-title">{title}</h2>
                </div>

                {/* Dynamic Visual Area */}
                <div className="card-visual-container">
                    {visualIsImage ? (
                        <img src={visualSrc} alt={title} className="card-visual-img" />
                    ) : (
                        <iframe src={visualSrc} title={title} className="card-visual-iframe" />
                    )}
                    {visualCaption && <div className="visual-caption">{visualCaption}</div>}
                </div>

                {/* Content Body */}
                <div className="card-body">
                    <div className="card-text">
                        {text}
                    </div>

                    {/* Embedded "Data Slip" Code Block */}
                    {codeSnippet && (
                        <div className="data-slip">
                            <div className="slip-header">R CODE SNIPPET</div>
                            <pre><code>{codeSnippet}</code></pre>
                        </div>
                    )}
                </div>
            </div>
        </section>
    );
};

export default CrystalCard;

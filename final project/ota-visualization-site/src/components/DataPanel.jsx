import React from 'react';
import './DataPanel.css';

const DataPanel = ({
    id,
    label,
    title,
    text,
    codeSnippet, // NEW: R/Math Code Snippet
    visualSrc,
    visualType = 'image',
    layout = 'split'
}) => {
    return (
        <div className={`data-panel layout-${layout}`} id={id}>
            <div className="panel-header">
                <span className="panel-label">{label}</span>
                <h2 className="panel-title">{title}</h2>
            </div>

            <div className="panel-body">
                <div className="panel-text">
                    {text}

                    {/* R Code Snippet Block */}
                    {codeSnippet && (
                        <div className="code-block">
                            <div className="code-header">R / BRMS</div>
                            <pre><code>{codeSnippet}</code></pre>
                        </div>
                    )}
                </div>

                <div className="panel-visual">
                    {visualType === 'image' ? (
                        <img src={visualSrc} alt={title} className="visual-asset" />
                    ) : (
                        <iframe src={visualSrc} title={title} className="visual-asset iframe" />
                    )}
                    <div className="visual-caption">
                        {/* Cleaner Caption Layout */}
                        {label.split(':')[0]}: {title}
                    </div>
                </div>
            </div>
        </div>
    );
};

export default DataPanel;

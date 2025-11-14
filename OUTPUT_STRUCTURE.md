# Output Folder Structure - Project qml-2025

## Summary
All R scripts in Week 8 and Week 9 have been configured to save output plots (PNG files) to **two locations** simultaneously:
1. **Week-specific folders**: `code/Week X/outputs/`
2. **Central archive**: `code/outputs/`

This dual-save structure ensures both local organization and centralized accessibility.

---

## Week 8 Output Configuration

### TaskA.r - Bernoulli Regression (Scalar Inference)
**Produces 5 plots:**
- `p1`: Posterior distribution of intercept (β₀)
- `p2`: Posterior distribution of slope (β₁ - semantic distance effect)
- `p3`: Joint posterior distribution
- `p4`: Posterior predictive check
- `p5`: Expected probability of SI across semantic distance

**Output Locations:**
```
code/Week 8/outputs/Week8_TaskA_p{1,2,3,4,5}.png
code/outputs/Week8_TaskA_p{1,2,3,4,5}.png
```

**Implementation (lines 282-291, 335-338):**
- Uses `walk2()` pattern for p1-p4 (dual ggsave within loop)
- Individual `ggsave()` calls for p5

---

### TaskB.r - Bernoulli Regression (Shallow Structure Hypothesis)
**Produces 5 plots:**
- `p1`: Posterior intercept (baseline accuracy)
- `p2`: Posterior relation type effects
- `p3`: Joint posterior (intercept vs Constituent effect)
- `p4`: Posterior predictive check
- `p5`: Expected accuracy by relation type

**Output Locations:**
```
code/Week 8/outputs/Week8_TaskB_p{1,2,3,4,5}.png
code/outputs/Week8_TaskB_p{1,2,3,4,5}.png
```

**Implementation (lines 267-275, 315-318):**
- Same dual-save pattern as TaskA

---

### TaskC.r - Log-Normal Regression (Reaction Times)
**Produces 5 plots:**
- `p1`: Posterior intercept (log RT baseline)
- `p2`: Posterior relation type effects (on log scale)
- `p3`: Posterior error SD (σ)
- `p4`: Posterior predictive check
- `p5`: Expected reaction time by relation type

**Output Locations:**
```
code/Week 8/outputs/Week8_TaskC_p{1,2,3,4,5}.png
code/outputs/Week8_TaskC_p{1,2,3,4,5}.png
```

**Implementation (lines 267-275, 314-317):**
- Same dual-save pattern as TaskA and TaskB

---

## Week 9 Output Configuration

### TaskA.r - Bernoulli Regression with Interaction
**Produces 5 plots:**
- `p1`: Posterior intercept
- `p2`: Posterior main effects
- `p3`: Posterior interaction effects (Group × Relation Type)
- `p4`: Posterior predictive check
- `p5`: Expected accuracy by group and relation type

**Output Locations:**
```
code/Week 9/outputs/Week9_TaskA_p{1,2,3,4,5}_*.png
code/outputs/Week9_TaskA_p{1,2,3,4,5}_*.png
```

**Implementation (lines 143-155, 181-191):**
- Uses `walk2()` pattern with enhanced naming

---

### TaskB.r - Gaussian Regression with Interaction
**Produces 6 plots:**
- `p1`: Posterior intercept (Young + Sparse baseline)
- `p2`: Posterior main effects (Age and Density)
- `p3`: Posterior interaction effect (Age × Density)
- `p4`: Posterior predictive check
- `p5`: Posterior error SD (σ)
- `p6`: Expected pupil width by age and density

**Output Locations:**
```
code/Week 9/outputs/Week9_TaskB_p{1,2,3,4,5,6}_*.png
code/outputs/Week9_TaskB_p{1,2,3,4,5,6}_*.png
```

**Implementation (lines 143-155, 183-191):**
- Same dual-save pattern as TaskA

---

## Dual-Save Implementation Pattern

### Method 1: Multiple Plots with walk2() (used for p1-p4)
```r
list(p1 = p1, p2 = p2, p3 = p3, p4 = p4) %>%
  walk2(names(.), function(plot, name) {
    # Save to central outputs folder
    ggsave(here("code", "outputs", paste0("Week8_TaskA_", name, ".png")),
           plot, width = 8, height = 6, dpi = 300)
    # Save to week-specific outputs folder
    ggsave(here("code", "Week 8", "outputs", paste0("Week8_TaskA_", name, ".png")),
           plot, width = 8, height = 6, dpi = 300)
  })
```

### Method 2: Individual Plots (used for p5 or special plots)
```r
# Save to both locations
ggsave(here("code", "outputs", "Week8_TaskA_p5.png"), p5,
       width = 8, height = 6, dpi = 300)
ggsave(here("code", "Week 8", "outputs", "Week8_TaskA_p5.png"), p5,
       width = 8, height = 6, dpi = 300)
```

### Key Benefits:
✅ **Cross-platform compatibility** via `here()` function
✅ **Single script execution** creates files in both locations
✅ **Week-specific archives** for easy task reference
✅ **Central repository** for consolidated viewing
✅ **DPI 300** for high-quality publication-ready plots
✅ **Consistent naming** using pattern `WeekX_TaskY_p{Z}_description.png`

---

## Git Configuration

### .gitignore
```
*.png
```
All PNG files are excluded from version control to avoid repository bloat. The folder structure is preserved via `.gitkeep` files.

### .gitkeep Files
```
code/Week 8/outputs/.gitkeep
code/Week 9/outputs/.gitkeep
```
These empty marker files ensure that the folder structure persists in git even when no PNG files are tracked.

---

## Current Output Status

### Week 8
| Task | Status | Files | Location |
|------|--------|-------|----------|
| TaskA | ✅ Generated | 5 PNG | Both locations |
| TaskB | Pending* | 0 PNG | Code-ready |
| TaskC | ✅ Generated | 5 PNG | Both locations |

*TaskB outputs require script execution to generate

### Week 9
| Task | Status | Files | Location |
|------|--------|-------|----------|
| TaskA | ✅ Generated | 5 PNG | Both locations |
| TaskB | ✅ Generated | 6 PNG | Both locations |

### Central Archive (code/outputs/)
- Week 8: 10 files (TaskA + TaskC)
- Week 9: 11 files (TaskA + TaskB)
- **Total: 21 files across both weeks**

---

## Folder Structure Visualization

```
qml-2025/
├── code/
│   ├── outputs/                    (Central archive)
│   │   ├── Week8_TaskA_p1.png
│   │   ├── Week8_TaskA_p2.png
│   │   ├── ... (10 more Week 8 files)
│   │   ├── Week9_TaskA_p1.png
│   │   ├── ... (11 more Week 9 files)
│   │
│   ├── Week 8/
│   │   ├── outputs/                (Week-specific)
│   │   │   ├── .gitkeep
│   │   │   ├── Week8_TaskA_p1.png
│   │   │   ├── Week8_TaskA_p2.png
│   │   │   ├── ... (3 more files)
│   │   │   ├── Week8_TaskC_p1.png
│   │   │   ├── ... (4 more files)
│   │   ├── TaskA.r
│   │   ├── TaskB.r
│   │   └── TaskC.r
│   │
│   └── Week 9/
│       ├── outputs/                (Week-specific)
│       │   ├── .gitkeep
│       │   ├── Week9_TaskA_p1.png
│       │   ├── ... (4 more files)
│       │   ├── Week9_TaskB_p1.png
│       │   ├── ... (5 more files)
│       ├── TaskA.r
│       └── TaskB.r
│
└── .gitignore (includes *.png rule)
```

---

## Quick Reference

### To verify all ggsave statements:
```bash
grep -n "ggsave" code/Week\ 8/Task*.r
grep -n "ggsave" code/Week\ 9/Task*.r
```

### To check current output files:
```bash
ls -lah code/Week\ 8/outputs/
ls -lah code/Week\ 9/outputs/
ls -lah code/outputs/ | grep Week
```

### To view recent commits:
```bash
git log --oneline -10 | grep -E "(ggsave|output|Week)"
```

---

## Task Completion

**Request:** "can you add all the outputs .png in week x outputs and outside outputs in code"

**Status:** ✅ **COMPLETE**

All R scripts (Week 8 TaskA/B/C and Week 9 TaskA/B) now implement dual-save functionality. When executed, each script generates PNG outputs to both:
1. `code/Week X/outputs/` (week-specific)
2. `code/outputs/` (central archive)

Last verified: November 14, 2025

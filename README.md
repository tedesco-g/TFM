# Master’s Thesis: Lobbying and Language in EU Tech Regulation

This repository contains the materials for my Master’s thesis:
**“Lobbying and Language in EU Tech Regulation: A Comparative Analysis of the GDPR, DMA, and AI Act”**
submitted in 2025 as part of the **Master in Computational Social Science** at **Universidad Carlos III de Madrid**.

The thesis investigates how corporate lobbying influences the linguistic framing of major EU digital regulations. By applying computational text analysis and statistical modeling, the study compares the **GDPR**, **DMA**, and **AI Act**, developed under varying lobbying intensities.

---

## Research Questions

* How does lobbying intensity shape regulatory language in EU digital laws?
* Do periods of high lobbying correlate with more corporate-friendly wording?
* What policy domains (e.g., risk management, competition) are most linguistically flexible to influence?
* Can computational text-as-data methods reliably detect patterns of lobbying influence?

---

## What This Research Is About

* **Methods**: Lexical scoring, topic modeling (LDA), ANOVA, regression, and bootstrapping.
* **Data**: Texts of GDPR, DMA, AI Act (publicly available EU legislation).
* **Framing**: Corporate-friendly vs. public-interest orientations.
* **Contribution**: Shows how lobbying shapes language selectively, especially in risk-management provisions of the AI Act.

---

## Repository Contents
## Thesis Documents

gdpr_prop.pdf – General Data Protection Regulation, proposal text
gdpr_final.pdf – General Data Protection Regulation, final text
dma_prop.pdf – Digital Markets Act, proposal text
dma.pdf – Digital Markets Act, final text
ai_act_2021.pdf – AI Act, 2021 proposal
ai_act_2023_parl.pdf – AI Act, European Parliament draft (2023)
ai_act_council.pdf – AI Act, Council version
ai_act_final.pdf – AI Act, final adopted version

Scripts (R): Numbered in the order they should be run:

01_data_preparation.R – Import and clean regulation texts
02_scoring_system.R – Lexical scoring (corporate vs. public-interest language)
03_comparative_analysis.R – Comparative analysis across GDPR, DMA, AI Act
04_visualizations.R – Generate plots, word clouds, and distribution graphs
05_export_results.R – Export tables and results for inclusion in thesis

## Key Findings

| Regulation | Lobbying Intensity | Corporate-Friendliness |
| ---------- | ------------------ | ---------------------- |
| GDPR       | Medium (2012–2016) | 28.3%                  |
| DMA        | High (2018–2022)   | 21.6%                  |
| AI Act     | Peak (2021–2024)   | 44.3%                  |

* The **AI Act** shows significantly higher corporate-friendly language than GDPR or DMA (*ANOVA p = 0.007, η² = 0.171*).
* **DMA** remains less corporate-friendly despite intense lobbying, due to its prohibition-centered legal framework.
* Influence concentrates in **technical provisions**, where flexibility can be justified.

---

## Limitations

* Analysis limited to regulatory text, not amendments or enforcement guidelines.
* Lobbying disclosures vary in completeness across years.
* Lexical approach cannot fully capture context or subtle semantic framing.

---

## Future Directions

* Extend to **Digital Services Act (DSA)** and subsequent AI governance reforms.
* Apply **transformer-based NLP models** for deeper semantic analysis.
* Compare **EU vs. US regulatory language** in tech law.

---
Email: tedesco.gina@gmail.com
LinkedIn: [in/tedescogina]([url](https://www.linkedin.com/in/tedescogina/))
---

## Citation

If you use this work, please cite as:

> Tedesco, G. (2025). *Lobbying and Language in EU Tech Regulation: A Comparative Analysis of the GDPR, DMA, and AI Act*. Master’s Thesis, Universidad Carlos III de Madrid.



👉 Question for you: do you want your README to also include a **contact section** (email, LinkedIn, ORCID), like your classmate did, or do you prefer to keep it clean and academic-only?

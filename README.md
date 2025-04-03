# 🇬🇧 green_policy_response

This repository supports the research paper _Environmental Protest and Legislative Response_ by Elisa D'Amico, Katie Nutley, and Aarushi Sharma.

The goal is to extract, filter, and analyse parliamentary speeches related to environmental issues across multiple countries. The workflow includes filtering speeches using an extensive climate-related keyword list, translating non-English content, applying topic modelling, and merging multilingual datasets for analysis.

📄 [See Overleaf Working Paper](https://www.overleaf.com/project/5729675688svrsgshpjvgn#28fbc3)

---

## 🔁 Project Pipeline

### Step 1: Initial Filtering (English-Language)

We begin by filtering parliamentary speeches to extract only those that relate to climate and environmental issues. This is done using a comprehensive set of keywords (see `data/keywords.csv`). 

Initial focus:  
- United Kingdom (filtered dataset already available)  
- New Zealand (pending)

Filtering is handled by `src/filter_speeches.R` using parallel processing for performance. This reduces over 1 million rows to ~148k in the UK dataset.

> Run via:
> ```bash
> ./run.sh filter
> ```

---

### Step 2: Multilingual Expansion

Once the filtering process is validated for English-speaking countries, it is expanded to include the following countries:

- Austria  
- Czech Republic  
- Germany  
- Denmark  
- Netherlands  
- Spain  
- Sweden  
- New Zealand  
- United Kingdom

Translation of non-English texts uses the Google Translate API, with validation by human experts where possible.

> Run a test translation via:
> ```bash
> ./run.sh translate
> ```

---

### Step 3: Topic Modelling

After filtering, we apply topic modelling (e.g. using STM or LDA models) to classify speeches into thematic areas (e.g. protest, governance, adaptation).

> Topic modelling is currently stubbed out in `src/topic_modeling.R`:
> ```bash
> ./run.sh topic
> ```

---

## 📁 Repository Structure

```
green_policy_response/
│
├── src/               # R scripts (pipeline steps)
│   ├── main.R
│   ├── filter_speeches.R
│   ├── translate.R
│   ├── topic_modeling.R
│   └── utils/
│       └── load_keywords.R
│
├── data/
│   ├── raw/           # Large .rds files (excluded from GitHub)
│   ├── processed/     # Filtered outputs
│   └── keywords.csv   # Environmental keywords (1 per row)
│
├── test/              # Tests (planned)
├── .gitignore
├── requirements.R     # R package dependencies
├── run.sh             # Bash entry point
└── README.md
```

---

## 📦 Setup Instructions

Install R dependencies:

```r
source("requirements.R")
```

If you'd like reproducibility/isolation, use `renv::init()` and commit the lockfile.

---

## 🧾 Instructions for Katie and Aarushi

### ✅ Phase 1: English-Speaking Countries

Start with UK and New Zealand.

1. UK filtering is complete (available in OneDrive: see below).
2. Run `src/filter_speeches.R` to filter New Zealand data using the same keywords.
3. Apply the same process to other countries as translation and keyword lists become available.

📎 Link to filtered UK dataset:
[UK Filtered Data via OneDrive](https://onedrive.com)

### 🔍 Phase 2: Pilot Topic Modelling in the UK

Start topic modelling using the filtered UK data first. Once refined, apply it to New Zealand and beyond.

**Key Decisions:**
- What inputs do you feed the model?
- What modelling approach? Why?
- How do you cluster/filter noise?
- What improves efficiency?

**Technical Notes:**
- Use parallelisation wherever possible (`furrr` is used in filtering).
- Work on a small subset before scaling up.
- Pre-cluster at a high level to reduce noise before deeper modelling.
- Keep a record of preprocessing and filtering decisions for reproducibility.

Contact Elisa with any blockers or questions.

---

## 🧊 Data Storage

Due to large file sizes, raw .rds datasets are not pushed to GitHub.

**OneDrive Storage (shared):**
[Access Raw + Filtered Data Here](https://onedrive.com)

Please download and place .rds files in `data/raw/` locally. `.gitignore` ensures they are not committed.

---

## ✏️ Development To Do List

- [ ] Translate non-English data (using translate.R)
- [ ] Create consistent topic modelling pipeline
- [ ] Merge multilingual datasets
- [ ] Write unit tests in test/
- [ ] Optional: create interactive Shiny dashboard for visualisation
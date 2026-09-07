# Wooden Man Weather — Business Plan

**Location focus:** Marquette, Michigan (NWS station USW00094850)  
**Product:** A locally grounded weather dashboard and Monday work-week briefing  
**Delivery:** Static Quarto site published on GitHub Pages, refreshed on a schedule  
**Draft date:** May 12, 2026

## Executive summary

Wooden Man Weather is a small, trust-first weather service for Marquette and the central Upper Peninsula. The public dashboard combines National Weather Service forecasts with twelve-month rolling comparisons of temperature, precipitation, and snowfall against long-term averages. Each Monday, a work-week briefing translates the forecast and seasonal context into practical tasks—when to swap snow tires, when soil is workable for early crops, when to watch for refreeze or heavy lake-effect snow.

The business is intentionally narrow: one place, one voice, one primary data station, with clear attribution and update timestamps. Revenue is secondary to audience and credibility in year one; paid briefings, sponsorships, and seasonal guides follow once the dashboard is reliable and the Monday memo has a regular readership.

## Problem and opportunity

National apps answer “what is the temperature?” but rarely answer “is this a normal May in Marquette?” or “what should I do in the yard this week given the thaw?” Marquette’s climate is Lake Superior–driven: heavy snow, a short growing season, rapid spring swings, and microclimates between lakeshore and inland hills. Residents, contractors, gardeners, marina operators, and NMU students all need the same underlying data explained with local judgment.

Wooden Man Weather fills that gap with observed and forecast data tied to a named station, charts that compare the rolling year to 1991–2020 normals, and a Monday memo that links thresholds to action.

## Target customers

**Primary (free tier)**  
- Homeowners and renters managing winter driving, snow removal, and yard work  
- Gardeners and small-scale growers timing planting and frost protection  
- Outdoor workers and recreation users planning around wind, snow, and mud season  

**Secondary (future paid or partner tiers)**  
- Contractors, landscapers, and plow operators who want a weekly PDF or email for crews  
- Marinas, ski areas, and tourism businesses that need a concise operations outlook  
- Property managers and second-home owners who are not in town year-round  

## Product and service lines

### 1. Live dashboard (core)

- Current conditions and a seven-day forecast from the NWS grid for Marquette  
- Active alerts and winter-weather products surfaced in plain language  
- Value boxes for month-to-date temperature, precipitation, and snow versus normal  
- Rolling twelve-month line charts for temperature, precipitation, and snowfall (current period versus climatological average)  
- Visible “last updated” time and data source notes on every view  

### 2. Monday work-week briefing

Published each Monday morning (site page plus optional email or RSS later):

- One-sentence headline for the week  
- Monday–Friday strip: highs, lows, precipitation or snow chances, wind  
- Week versus average for temperature and precipitation  
- Three to seven **Wooden Man tasks** with a short reason tied to forecast or calendar rules  
- One “watch” item when models or history support elevated risk (refreeze, wet snow load, etc.)  

### 3. Seasonal guides (year two)

Short paid or sponsor-supported PDFs: garden planting windows, winter tire and vehicle prep, lake and harbor season openers. These reuse the same data pipeline and task library as the dashboard.

## Data strategy and auto-update

**Sources (all with attribution on site)**  
- **Forecasts and alerts:** [NWS API](https://www.weather.gov/documentation/services-web-api) for the Marquette forecast grid  
- **Observations and history:** NOAA NCEI daily summaries for station USW00094850  
- **Normals:** NCEI 1991–2020 monthly normals for the same station  

**Update cadence**  
- Forecasts and alerts: refresh every fifteen to thirty minutes via scheduled GitHub Actions or a daily render with cached upstream pulls  
- Daily climate cache: append new observations once per day  
- Normals: static unless NOAA revises the baseline period  
- Monday briefing: regenerate on Monday mornings; optional manual review before publish in early months  

**Technical approach**  
- Quarto website with a dashboard home page (static HTML, no Shiny server)  
- R scripts in `R/` fetch and normalize API responses; optional `scripts/` jobs write cached CSV under `data/`  
- GitHub Actions render on push and on a cron schedule, publishing to GitHub Pages  

**Trust and limitations**  
- State that airport observations may not match lakeshore or hilltop conditions  
- Separate observed, forecast, and normal series in charts and copy  
- Do not present model output as certainty; use NWS wording where appropriate  

## Go-to-market

**Phase 1 — Credibility (months 1–3)**  
- Launch dashboard and Monday page on GitHub Pages  
- Share in local Facebook groups, garden clubs, contractor networks, and NMU-adjacent channels  
- Post one chart or task snippet per week on social media with a link back to the site  

**Phase 2 — Habit (months 4–9)**  
- Add email or RSS for the Monday briefing  
- Collect anonymous feedback (“was this useful?”) on the briefing page  
- Partner with one nursery, tire shop, or greenhouse for a single relevant sponsorship test  

**Phase 3 — Revenue experiments (month 10+)**  
- Paid Monday email or “operations” PDF for businesses  
- Seasonal guide sales or bundled annual subscription  
- Sponsored task slots only when they match the forecast and do not dilute trust  

## Revenue model

| Stream | Timing | Notes |
|--------|--------|-------|
| Free dashboard and Monday page | Launch | Builds audience and SEO for “Marquette weather” |
| Email / RSS subscription | Phase 2 | Low annual fee or tip jar |
| Business briefing tier | Phase 3 | Custom thresholds, PDF branding, early Monday delivery |
| Seasonal guides | Phase 2–3 | Garden, winter prep, lake season |
| Local sponsorship | Phase 2+ | One sponsor per season, clearly labeled |

Year-one financial goal is break-even on domain and email costs, not salary replacement.

## Operations

**Roles**  
- Founder: data pipeline, Quarto content, Monday copy, community outreach  
- Optional later: part-time editor for Monday voice, or a developer for alert SMS  

**Weekly rhythm**  
- Automated data pull and site render  
- Monday: review generated tasks, adjust copy if a major storm warrants it, publish briefing  
- Monthly: spot-check charts against NWS climate pages; note API or station changes  

**Legal and compliance**  
- NWS and NOAA attribution on every page  
- Privacy-minimal analytics if used; disclose in About  
- Avoid medical or emergency advice; link to official warnings for life-safety events  

## Competitive landscape

Wooden Man Weather competes with the NWS site, national apps, and local TV—not by replacing them, but by combining **local normals**, **rolling context**, and **actionable weekly tasks** in one Marquette-specific product. Differentiators: named station, twelve-month versus average charts, Monday operations memo, and a consistent Yooper-practical voice (“wooden man” folklore: durable, plain-spoken, steady through hard winters).

## Risks and mitigations

| Risk | Mitigation |
|------|------------|
| API outage or rate limits | Cache last-good data; show stale-data banner |
| Station not representative of user’s microclimate | Document limits; add second station later if justified |
| Bad task advice after unusual weather | Rule-based tasks with forecast overrides; manual Monday review in year one |
| Low monetization | Keep costs near zero on GitHub Pages; treat revenue as upside |
| Founder time | Automate pulls and renders; keep scope to three metrics and one briefing |

## Milestones (twelve months)

1. **M1:** Dashboard live with forecast, three rolling charts, and About page  
2. **M2:** Monday briefing generated from task library; weekly publish cadence  
3. **M3:** Daily climate cache and scheduled GitHub Actions render  
4. **M6:** Email or RSS; 100+ weekly briefing views (analytics or proxy)  
5. **M9:** First paid or sponsored pilot without compromising editorial voice  
6. **M12:** Evaluate second location, SMS alerts, or seasonal guide SKU  

## Success metrics

- Uptime and successful scheduled renders  
- Monday briefing return visits and email open rate (when launched)  
- Qualitative feedback from local gardeners and contractors  
- Chart accuracy complaints (target: near zero after normals labeling is clear)  
- Revenue covering hosting and optional email service by month twelve  

---

*This document is a living draft. Update milestones and revenue experiments as the product ships and audience feedback arrives.*

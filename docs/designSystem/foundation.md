# Foundation

The Spatial UI foundation defines the core visual elements that form the basis of our design system. These fundamentals ensure consistency and cohesion across all applications.

## Color System

Our color system is built around a primary color palette that establishes brand identity, complemented by a range of functional colors for various UI states and feedback mechanisms.

### Primary Colors

| Name | Value | Preview | Usage |
|------|-------|---------|-------|
| Primary Blue | `#4264FB` | ![Primary Blue](https://via.placeholder.com/24/4264FB/FFFFFF) | Primary actions, key UI elements |
| Primary Purple | `#8B5DFF` | ![Primary Purple](https://via.placeholder.com/24/8B5DFF/FFFFFF) | Accents, gradients, highlights |
| Primary Cyan | `#00D4FF` | ![Primary Cyan](https://via.placeholder.com/24/00D4FF/FFFFFF) | Secondary actions, selection states |

### Gradients

Gradients add depth and dimension to our interfaces, creating visual interest and guiding attention.

| Name | Values | Preview | Usage |
|------|--------|---------|-------|
| Primary Gradient | `#4264FB` → `#8B5DFF` | ![Primary Gradient](https://via.placeholder.com/120x24/4264FB/8B5DFF) | Buttons, headers, key UI elements |
| Accent Gradient | `#00D4FF` → `#4264FB` | ![Accent Gradient](https://via.placeholder.com/120x24/00D4FF/4264FB) | Feature highlights, cards, containers |

### Background Gradients

| Name | Values | Preview | Usage |
|------|--------|---------|-------|
| Light Background | `#F8F9FF` → `#E6F0FF` | ![Light BG](https://via.placeholder.com/120x24/F8F9FF/E6F0FF) | App backgrounds in light mode |
| Dark Background | `#0A0F1C` → `#1A1F2E` | ![Dark BG](https://via.placeholder.com/120x24/0A0F1C/1A1F2E) | App backgrounds in dark mode |

### Glassmorphism Colors

These colors are used to create glass-like effects with transparency and blur.

| Name | Value | Alpha | Usage |
|------|-------|-------|-------|
| Glass Light | `#FFFFFF` | 25% | Light mode glass containers |
| Glass Dark | `#FFFFFF` | 10% | Dark mode glass containers |
| Glass Border | `#FFFFFF` | 20% | Borders for glass effects |

### Semantic Colors

Colors that convey specific meanings and provide user feedback.

| Name | Value | Preview | Usage |
|------|-------|---------|-------|
| Success | `#00C851` | ![Success](https://via.placeholder.com/24/00C851/FFFFFF) | Confirmations, completions |
| Warning | `#FF8F00` | ![Warning](https://via.placeholder.com/24/FF8F00/FFFFFF) | Alerts, warnings |
| Error | `#FF4444` | ![Error](https://via.placeholder.com/24/FF4444/FFFFFF) | Errors, destructive actions |
| Info | `#33B5E5` | ![Info](https://via.placeholder.com/24/33B5E5/FFFFFF) | Information, help |

### Text Colors

| Name | Value | Preview | Usage |
|------|-------|---------|-------|
| Text Primary | `#1A1A1A` | ![Text Primary](https://via.placeholder.com/24/1A1A1A/FFFFFF) | Primary text (light mode) |
| Text Secondary | `#666666` | ![Text Secondary](https://via.placeholder.com/24/666666/FFFFFF) | Secondary text (light mode) |
| Text Tertiary | `#999999` | ![Text Tertiary](https://via.placeholder.com/24/999999/FFFFFF) | Hints, labels (light mode) |
| Text Light | `#FFFFFF` | ![Text Light](https://via.placeholder.com/24/FFFFFF/1A1A1A) | Text in dark mode |

### Surface Colors

| Name | Value | Preview | Usage |
|------|-------|---------|-------|
| Surface Light | `#FFFFFF` | ![Surface Light](https://via.placeholder.com/24/FFFFFF/1A1A1A) | Cards, dialogs (light mode) |
| Surface Dark | `#1E1E1E` | ![Surface Dark](https://via.placeholder.com/24/1E1E1E/FFFFFF) | Cards, dialogs (dark mode) |
| Surface Elevated | `#F5F5F5` | ![Surface Elevated](https://via.placeholder.com/24/F5F5F5/1A1A1A) | Elevated surfaces (light mode) |

## Typography

Our typography system uses Inter font family for its exceptional readability across devices and screen sizes.

### Text Styles

| Style | Font | Weight | Size | Line Height | Letter Spacing | Usage |
|-------|------|--------|------|-------------|---------------|-------|
| Display Large | Inter | 700 (Bold) | 32px | 1.2 | -0.5px | Major headlines |
| Display Medium | Inter | 600 (SemiBold) | 28px | 1.25 | -0.25px | Page titles |
| Display Small | Inter | 600 (SemiBold) | 24px | 1.3 | normal | Section titles |
| Headline Large | Inter | 600 (SemiBold) | 22px | 1.3 | normal | Card titles |
| Headline Medium | Inter | 500 (Medium) | 20px | 1.4 | normal | Section headings |
| Headline Small | Inter | 500 (Medium) | 18px | 1.4 | normal | Minor headings |
| Title Large | Inter | 600 (SemiBold) | 16px | 1.4 | normal | List titles, buttons |
| Title Medium | Inter | 500 (Medium) | 14px | 1.4 | normal | Subtitles |
| Title Small | Inter | 500 (Medium) | 12px | 1.4 | normal | Captions, labels |
| Body Large | Inter | 400 (Regular) | 16px | 1.5 | normal | Primary body text |
| Body Medium | Inter | 400 (Regular) | 14px | 1.5 | normal | Secondary body text |
| Body Small | Inter | 400 (Regular) | 12px | 1.5 | normal | Supporting text |
| Label Large | Inter | 500 (Medium) | 14px | 1.4 | normal | Important labels |
| Label Medium | Inter | 500 (Medium) | 12px | 1.4 | normal | Form labels |
| Label Small | Inter | 500 (Medium) | 10px | 1.4 | normal | Metadata, timestamps |

## Spacing

Consistent spacing helps create visual rhythm and proper hierarchy.

### Spacing Scale

| Name | Size | Usage |
|------|------|-------|
| Space XS | 4px | Minimal spacing, icons |
| Space SM | 8px | Tight spacing, dense UIs |
| Space MD | 16px | Standard spacing |
| Space LG | 24px | Generous spacing |
| Space XL | 32px | Major component spacing |
| Space 2XL | 48px | Section spacing |
| Space 3XL | 64px | Page-level spacing |

### Border Radius

| Name | Size | Usage |
|------|------|-------|
| Radius SM | 8px | Small elements (chips, tags) |
| Radius MD | 12px | Standard elements (buttons, inputs) |
| Radius LG | 16px | Larger elements (cards, modals) |
| Radius XL | 24px | Feature elements (bottom sheets) |

## Shadows & Elevation

Shadows create a sense of depth and hierarchy in the interface.

### Shadow Styles

| Name | Usage | Shadow Properties |
|------|-------|------------------|
| Spatial Shadow | Default elevation | 2 layers: blue tint (alpha 0.1) at 0px 8px 20px, black (alpha 0.05) at 0px 4px 10px |
| Elevated Shadow | Prominent elements | 2 layers: blue tint (alpha 0.15) at 0px 12px 30px, black (alpha 0.08) at 0px 6px 15px |
| Glow Shadow | Emphasis, selection | 1 layer: blue (alpha 0.3) at 0px 0px 25px |

## Animation

Thoughtful animations enhance the experience and provide visual feedback.

### Duration

| Name | Time | Usage |
|------|------|-------|
| Animation Fast | 150ms | Micro-interactions, state changes |
| Animation Normal | 300ms | Standard transitions |
| Animation Slow | 500ms | Major transitions, emphasis |

### Easing Curves

- **Standard**: `ease-in-out` - Most UI transitions
- **Decelerate**: `ease-out` - Elements entering the screen
- **Accelerate**: `ease-in` - Elements leaving the screen
- **Sharp**: `ease` - Quick, attention-grabbing animations

## Themes

Spatial UI supports both light and dark themes, with full compatibility across all components.

### Light Theme

- Background: Light gradient (`#F8F9FF` → `#E6F0FF`)
- Surfaces: White with subtle shadows
- Text: Dark gray on light backgrounds
- Glass Effect: White-tinted transparency

### Dark Theme

- Background: Dark gradient (`#0A0F1C` → `#1A1F2E`)
- Surfaces: Dark surfaces with subtle highlights
- Text: Light colors on dark backgrounds
- Glass Effect: Subtler transparency with light borders

---

© 2025 KTC Logistics | Design System Team

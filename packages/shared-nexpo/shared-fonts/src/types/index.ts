// Font Categories
export enum FontCategory {
  SANS_SERIF = 'sans-serif',
  SERIF = 'serif',
  DISPLAY = 'display',
  HANDWRITING = 'handwriting',
  MONOSPACE = 'monospace',
}

// Font Weight Values
export enum FontWeight {
  THIN = '100',
  EXTRA_LIGHT = '200',
  LIGHT = '300',
  REGULAR = '400',
  MEDIUM = '500',
  SEMI_BOLD = '600',
  BOLD = '700',
  EXTRA_BOLD = '800',
  BLACK = '900',
}

// Font Style
export enum FontStyle {
  NORMAL = 'normal',
  ITALIC = 'italic',
}

// Font Metadata
export interface FontMetadata {
  id: string;
  name: string;
  category: FontCategory;
  weights: FontWeight[];
  styles: FontStyle[];
  fallback: string[];
  description?: string;
  useCases?: string[];
  pairsWith?: string[];
  tags?: string[];
  license?: string;
  designer?: string;
}

// Font Loading Status
export interface FontLoadingStatus {
  isLoaded: boolean;
  isLoading: boolean;
  error?: Error;
}

// Font Configuration
export interface FontConfig {
  family: string;
  weight?: FontWeight;
  style?: FontStyle;
  fallback?: string[];
}

// Typography Scale
export interface TypographyScale {
  xs: number;
  sm: number;
  base: number;
  lg: number;
  xl: number;
  '2xl': number;
  '3xl': number;
  '4xl': number;
  '5xl': number;
  '6xl': number;
  '7xl': number;
  '8xl': number;
  '9xl': number;
}

// Typography Style
export interface TypographyStyle {
  fontFamily: string;
  fontSize: number;
  fontWeight: FontWeight;
  lineHeight: number;
  letterSpacing?: number;
  textTransform?: 'none' | 'uppercase' | 'lowercase' | 'capitalize';
}

// Typography System
export interface TypographySystem {
  scale: TypographyScale;
  fonts: {
    primary: FontConfig;
    secondary: FontConfig;
    display: FontConfig;
    mono: FontConfig;
    handwriting?: FontConfig;
  };
  styles: {
    h1: TypographyStyle;
    h2: TypographyStyle;
    h3: TypographyStyle;
    h4: TypographyStyle;
    h5: TypographyStyle;
    h6: TypographyStyle;
    body1: TypographyStyle;
    body2: TypographyStyle;
    caption: TypographyStyle;
    overline: TypographyStyle;
    button: TypographyStyle;
    code: TypographyStyle;
  };
}

// Platform-specific font loading
export interface FontLoader {
  loadFont(font: FontConfig): Promise<void>;
  loadFonts(fonts: FontConfig[]): Promise<void>;
  isLoaded(family: string): boolean;
  getLoadingStatus(family: string): FontLoadingStatus;
}

// Font Collection
export interface FontCollection {
  id: string;
  name: string;
  description: string;
  fonts: FontMetadata[];
  theme?: 'modern' | 'classic' | 'playful' | 'professional' | 'creative';
  recommendedPairings?: Array<{
    primary: string;
    secondary: string;
    use: string;
  }>;
}

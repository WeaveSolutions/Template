/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./apps/**/*.{js,jsx,ts,tsx}",
    "./packages/**/*.{js,jsx,ts,tsx}",
  ],
  darkMode: 'class', 
  theme: {
    extend: {
      colors: {
        border: "hsl(var(--border))",
        input: "hsl(var(--input))",
        ring: "hsl(var(--ring))",
        background: {
          DEFAULT: "hsl(var(--background))",
          dark: "#121212",
        },
        foreground: "hsl(var(--foreground))",
        primary: {
          DEFAULT: "#0066cc",
          50: "#e6f0ff",
          100: "#cce0ff",
          200: "#99c2ff",
          300: "#66a3ff",
          400: "#3385ff",
          500: "#0066cc",
          600: "#0052a3",
          700: "#003d7a",
          800: "#002952",
          900: "#001429",
          foreground: "hsl(var(--primary-foreground))",
          dark: "#0056b3",
        },
        secondary: {
          DEFAULT: "hsl(var(--secondary))",
          foreground: "hsl(var(--secondary-foreground))",
          dark: "#494c50",
        },
        destructive: {
          DEFAULT: "hsl(var(--destructive))",
          foreground: "hsl(var(--destructive-foreground))",
        },
        muted: {
          DEFAULT: "hsl(var(--muted))",
          foreground: "hsl(var(--muted-foreground))",
        },
        accent: {
          DEFAULT: "hsl(var(--accent))",
          foreground: "hsl(var(--accent-foreground))",
        },
        popover: {
          DEFAULT: "hsl(var(--popover))",
          foreground: "hsl(var(--popover-foreground))",
        },
        card: {
          DEFAULT: "hsl(var(--card))",
          foreground: "hsl(var(--card-foreground))",
        },
        text: {
          DEFAULT: "#212529",
          dark: "#E9ECEF",
        },
      },
      borderRadius: {
        lg: "var(--radius)",
        md: "calc(var(--radius) - 2px)",
        sm: "calc(var(--radius) - 4px)",
      },
    },
  },
  plugins: [],
}

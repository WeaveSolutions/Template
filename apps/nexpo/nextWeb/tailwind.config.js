/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './pages/**/*.{js,ts,jsx,tsx}',
    './components/**/*.{js,ts,jsx,tsx}',
    '../../packages/shared-ui/src/**/*.{js,ts,jsx,tsx}',
    '../../packages/shared-pages/src/**/*.{js,ts,jsx,tsx}',
    '../../packages/shared-components/src/**/*.{js,ts,jsx,tsx}'
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}

import { DefaultSeoProps } from 'next-seo';

// Default SEO configuration for the entire application
const defaultSEOConfig: DefaultSeoProps = {
  // Main title template that will be applied to all pages
  // %s will be replaced with the specific page title
  titleTemplate: '%s | Solito App',
  defaultTitle: 'Solito App - Cross-Platform React Native & Next.js',
  
  // Meta description that appears in search engines
  description: 'A cross-platform application built with React Native, Expo, and Next.js using the Solito framework',
  
  // Open Graph metadata for better social media sharing
  openGraph: {
    type: 'website',
    locale: 'en_US',
    url: 'https://your-app-url.com/', // Replace with your actual URL
    site_name: 'Solito App',
    title: 'Solito App - Cross-Platform React Native & Next.js',
    description: 'A cross-platform application built with React Native, Expo, and Next.js using the Solito framework',
    images: [
      {
        url: 'https://your-app-url.com/og-image.jpg', // Replace with your actual Open Graph image URL
        width: 1200,
        height: 630,
        alt: 'Solito App',
      },
    ],
  },
  
  // Twitter card metadata for Twitter sharing
  twitter: {
    handle: '@your_handle', // Replace with your Twitter handle
    site: '@your_site', // Replace with your site's Twitter handle
    cardType: 'summary_large_image',
  },
  
  // Additional metadata
  additionalMetaTags: [
    {
      name: 'viewport',
      content: 'width=device-width, initial-scale=1',
    },
    {
      name: 'application-name',
      content: 'Solito App',
    },
    {
      name: 'theme-color',
      content: '#ffffff',
    },
  ],
  
  // Additional link tags
  additionalLinkTags: [
    {
      rel: 'icon',
      href: '/favicon.ico',
    },
    {
      rel: 'apple-touch-icon',
      href: '/apple-touch-icon.png',
      sizes: '180x180',
    },
  ],
};

export default defaultSEOConfig;

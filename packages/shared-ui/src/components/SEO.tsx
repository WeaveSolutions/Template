import React from 'react';
import { Platform } from 'react-native';
// Static import for Next.js 15 compatibility
import Head from 'next/head';

interface SEOProps {
  title?: string;
  description?: string;
  titleKey?: string; // Add support for translation keys
  descriptionKey?: string; // Add support for translation keys
  [key: string]: any;
}

/**
 * SEO component that can be used across the app to set page-specific SEO
 * 
 * @param title - Page title
 * @param description - Page description
 * @param titleKey - Translation key for title
 * @param descriptionKey - Translation key for description
 * @param rest - Any other SEO props
 */
export const SEO = ({
  title = 'Nexpo Template',
  description = 'Build universal apps with Next.js, Expo, and Solito',
  titleKey,
  descriptionKey,
  ...rest
}: SEOProps) => {
  // Only use Head on web platform
  if (Platform.OS !== 'web') {
    return null;
  }

  // Use the title and description as provided
  // In a real app, you would use the translation keys with i18n

  return (
    <Head>
      <title>{title}</title>
      {description && <meta name="description" content={description} />}
      {/* Open Graph / Facebook */}
      <meta property="og:type" content="website" />
      <meta property="og:title" content={title} />
      {description && <meta property="og:description" content={description} />}
      {/* Twitter */}
      <meta property="twitter:card" content="summary_large_image" />
      <meta property="twitter:title" content={title} />
      {description && <meta property="twitter:description" content={description} />}
      {/* Pass through any additional props */}
      {Object.entries(rest).map(([key, value]) => (
        <meta key={key} name={key} content={value as string} />
      ))}
    </Head>
  );
};

/** @type {import('next-sitemap').IConfig} */
module.exports = {
  // Replace with your actual website URL
  siteUrl: process.env.SITE_URL || 'https://your-app-url.com',
  generateRobotsTxt: false, // We've already created a custom robots.txt
  exclude: ['/404'], // Pages to exclude from sitemap
  robotsTxtOptions: {
    additionalSitemaps: [
      'https://your-app-url.com/server-sitemap.xml', // Optional: for dynamic sitemaps
    ],
  },
  // Change the outDir if you have a custom build directory
  outDir: './public',
}

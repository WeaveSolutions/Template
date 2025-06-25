import { DashboardScreen } from '@shared/pages';
import { NextSeo } from 'next-seo';

export default function DashboardPage() {
  return (
    <>
      <NextSeo
        title="Dashboard - Next Solito Expo"
        description="Your personal dashboard"
      />
      <DashboardScreen />
    </>
  );
}

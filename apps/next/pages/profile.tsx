import { ProfileScreen } from '@shared/pages';
import { NextSeo } from 'next-seo';

export default function ProfilePage() {
  return (
    <>
      <NextSeo
        title="Profile - Next Solito Expo"
        description="Manage your profile"
      />
      <ProfileScreen />
    </>
  );
}

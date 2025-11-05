import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';
import { SocialConnections } from '../SocialConnections';

// Mock the useAuth hook
const mockSignInWithProvider = jest.fn();
const mockGetEnabledProviders = jest.fn();

jest.mock('../../auth/useAuth', () => ({
  useAuth: () => ({
    signInWithProvider: mockSignInWithProvider,
    getEnabledProviders: mockGetEnabledProviders,
  }),
}));

describe('TikTok Integration', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should show TikTok login button when enabled', () => {
    mockGetEnabledProviders.mockReturnValue(['tiktok']);

    render(<SocialConnections />);

    const tiktokButton = screen.getByText('Continue with TikTok');
    expect(tiktokButton).toBeInTheDocument();
    expect(tiktokButton).toHaveStyle({ backgroundColor: '#000000' });
  });

  it('should not show TikTok login button when disabled', () => {
    mockGetEnabledProviders.mockReturnValue(['google-oauth2']);

    render(<SocialConnections />);

    const tiktokButton = screen.queryByText('Continue with TikTok');
    expect(tiktokButton).not.toBeInTheDocument();
  });

  it('should call signInWithProvider with tiktok when button is clicked', async () => {
    mockGetEnabledProviders.mockReturnValue(['tiktok']);

    render(<SocialConnections />);

    const tiktokButton = screen.getByText('Continue with TikTok');
    fireEvent.press(tiktokButton);

    await waitFor(() => {
      expect(mockSignInWithProvider).toHaveBeenCalledWith('tiktok');
    });
  });

  it('should show TikTok film/video emoji icon', () => {
    mockGetEnabledProviders.mockReturnValue(['tiktok']);

    render(<SocialConnections />);

    const filmIcon = screen.getByText('🎬');
    expect(filmIcon).toBeInTheDocument();
  });

  it('should handle multiple providers including TikTok', () => {
    mockGetEnabledProviders.mockReturnValue(['google-oauth2', 'tiktok', 'snapchat', 'instagram']);

    render(<SocialConnections />);

    expect(screen.getByText('Continue with Google')).toBeInTheDocument();
    expect(screen.getByText('Continue with TikTok')).toBeInTheDocument();
    expect(screen.getByText('Continue with Snapchat')).toBeInTheDocument();
    expect(screen.getByText('Continue with Instagram')).toBeInTheDocument();
  });

  it('should display TikTok with correct brand color', () => {
    mockGetEnabledProviders.mockReturnValue(['tiktok']);

    render(<SocialConnections />);

    const tiktokButton = screen.getByText('Continue with TikTok');
    expect(tiktokButton).toHaveStyle({ backgroundColor: '#000000' });
  });

  it('should handle TikTok authentication errors gracefully', async () => {
    mockGetEnabledProviders.mockReturnValue(['tiktok']);
    mockSignInWithProvider.mockRejectedValue(new Error('TikTok auth failed'));

    render(<SocialConnections />);

    const tiktokButton = screen.getByText('Continue with TikTok');
    fireEvent.press(tiktokButton);

    await waitFor(() => {
      expect(mockSignInWithProvider).toHaveBeenCalledWith('tiktok');
    });
  });

  it('should display TikTok alongside video-focused providers', () => {
    mockGetEnabledProviders.mockReturnValue(['tiktok', 'instagram']);

    render(<SocialConnections />);

    // Both video/media-focused platforms should appear
    expect(screen.getByText('Continue with TikTok')).toBeInTheDocument();
    expect(screen.getByText('Continue with Instagram')).toBeInTheDocument();

    // Check their distinct icons
    expect(screen.getByText('🎬')).toBeInTheDocument(); // TikTok film icon
    expect(screen.getByText('📷')).toBeInTheDocument(); // Instagram camera icon
  });

  it('should maintain TikTok provider identity in social media ecosystem', () => {
    mockGetEnabledProviders.mockReturnValue(['tiktok', 'snapchat', 'instagram', 'twitter']);

    render(<SocialConnections />);

    // All major social media platforms should be available
    expect(screen.getByText('Continue with TikTok')).toBeInTheDocument();
    expect(screen.getByText('Continue with Snapchat')).toBeInTheDocument();
    expect(screen.getByText('Continue with Instagram')).toBeInTheDocument();
    expect(screen.getByText('Continue with X (Twitter)')).toBeInTheDocument();
  });
});

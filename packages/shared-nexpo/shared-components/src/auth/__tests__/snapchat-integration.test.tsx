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

describe('Snapchat Integration', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should show Snapchat login button when enabled', () => {
    mockGetEnabledProviders.mockReturnValue(['snapchat']);

    render(<SocialConnections />);

    const snapchatButton = screen.getByText('Continue with Snapchat');
    expect(snapchatButton).toBeInTheDocument();
    expect(snapchatButton).toHaveStyle({ backgroundColor: '#FFFC00' });
  });

  it('should not show Snapchat login button when disabled', () => {
    mockGetEnabledProviders.mockReturnValue(['google-oauth2']);

    render(<SocialConnections />);

    const snapchatButton = screen.queryByText('Continue with Snapchat');
    expect(snapchatButton).not.toBeInTheDocument();
  });

  it('should call signInWithProvider with snapchat when button is clicked', async () => {
    mockGetEnabledProviders.mockReturnValue(['snapchat']);

    render(<SocialConnections />);

    const snapchatButton = screen.getByText('Continue with Snapchat');
    fireEvent.press(snapchatButton);

    await waitFor(() => {
      expect(mockSignInWithProvider).toHaveBeenCalledWith('snapchat');
    });
  });

  it('should show Snapchat ghost emoji icon', () => {
    mockGetEnabledProviders.mockReturnValue(['snapchat']);

    render(<SocialConnections />);

    const ghostIcon = screen.getByText('👻');
    expect(ghostIcon).toBeInTheDocument();
  });

  it('should handle multiple providers including Snapchat', () => {
    mockGetEnabledProviders.mockReturnValue(['google-oauth2', 'snapchat', 'instagram', 'tiktok', 'github']);

    render(<SocialConnections />);

    expect(screen.getByText('Continue with Google')).toBeInTheDocument();
    expect(screen.getByText('Continue with Snapchat')).toBeInTheDocument();
    expect(screen.getByText('Continue with Instagram')).toBeInTheDocument();
    expect(screen.getByText('Continue with TikTok')).toBeInTheDocument();
    expect(screen.getByText('Continue with GitHub')).toBeInTheDocument();
  });
});

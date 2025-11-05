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

describe('Instagram Integration', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should show Instagram login button when enabled', () => {
    mockGetEnabledProviders.mockReturnValue(['instagram']);

    render(<SocialConnections />);

    const instagramButton = screen.getByText('Continue with Instagram');
    expect(instagramButton).toBeInTheDocument();
    expect(instagramButton).toHaveStyle({ backgroundColor: '#E4405F' });
  });

  it('should not show Instagram login button when disabled', () => {
    mockGetEnabledProviders.mockReturnValue(['google-oauth2']);

    render(<SocialConnections />);

    const instagramButton = screen.queryByText('Continue with Instagram');
    expect(instagramButton).not.toBeInTheDocument();
  });

  it('should call signInWithProvider with instagram when button is clicked', async () => {
    mockGetEnabledProviders.mockReturnValue(['instagram']);

    render(<SocialConnections />);

    const instagramButton = screen.getByText('Continue with Instagram');
    fireEvent.press(instagramButton);

    await waitFor(() => {
      expect(mockSignInWithProvider).toHaveBeenCalledWith('instagram');
    });
  });

  it('should show Instagram camera emoji icon', () => {
    mockGetEnabledProviders.mockReturnValue(['instagram']);

    render(<SocialConnections />);

    const cameraIcon = screen.getByText('📷');
    expect(cameraIcon).toBeInTheDocument();
  });

  it('should handle multiple providers including Instagram', () => {
    mockGetEnabledProviders.mockReturnValue(['google-oauth2', 'instagram', 'snapchat']);

    render(<SocialConnections />);

    expect(screen.getByText('Continue with Google')).toBeInTheDocument();
    expect(screen.getByText('Continue with Instagram')).toBeInTheDocument();
    expect(screen.getByText('Continue with Snapchat')).toBeInTheDocument();
  });

  it('should display Instagram with correct brand color', () => {
    mockGetEnabledProviders.mockReturnValue(['instagram']);

    render(<SocialConnections />);

    const instagramButton = screen.getByText('Continue with Instagram');
    expect(instagramButton).toHaveStyle({ backgroundColor: '#E4405F' });
  });

  it('should handle Instagram authentication errors gracefully', async () => {
    mockGetEnabledProviders.mockReturnValue(['instagram']);
    mockSignInWithProvider.mockRejectedValue(new Error('Instagram auth failed'));

    render(<SocialConnections />);

    const instagramButton = screen.getByText('Continue with Instagram');
    fireEvent.press(instagramButton);

    await waitFor(() => {
      expect(mockSignInWithProvider).toHaveBeenCalledWith('instagram');
    });
  });
});

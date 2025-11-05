// Shared Tauri utilities for cross-platform functionality
// Add utility functions as needed for cross-platform compatibility

export const formatDate = (date: Date): string => {
  return date.toLocaleDateString();
};

export const formatTime = (date: Date): string => {
  return date.toLocaleTimeString();
};

export const truncateText = (text: string, maxLength: number): string => {
  if (text.length <= maxLength) return text;
  return text.slice(0, maxLength) + '...';
};

export const getInitials = (name: string): string => {
  return name
    .split(' ')
    .map(word => word.charAt(0))
    .join('')
    .toUpperCase()
    .slice(0, 2);
};

export const formatUserId = (id: string): string => {
  return id.length > 8 ? `${id.slice(0, 8)}...` : id;
};

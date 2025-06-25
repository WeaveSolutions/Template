import React from 'react';
import { Button } from '@nexpo/shared-components';
import { formatDate } from '@nexpo/shared-utils';
import { useTheme } from '@nexpo/shared-ui';

export const TestShared: React.FC = () => {
  const theme = useTheme();
  const currentDate = formatDate(new Date());

  return (
    <div style={{ padding: '20px' }}>
      <h2>Shared Packages Test</h2>
      <p>Current Date: {currentDate}</p>
      <p>Theme: {JSON.stringify(theme, null, 2)}</p>
      <Button onClick={() => alert('Shared button clicked!')}>
        Test Shared Button
      </Button>
    </div>
  );
};

import React from 'react';

declare global {
  namespace JSX {
    interface IntrinsicElements {
      View: React.ComponentType<any> & { style?: any };
      Text: React.ComponentType<any> & { style?: any };
    }
  }
}

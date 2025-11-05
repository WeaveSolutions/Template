import React from 'react';

declare global {
  namespace JSX {
    interface IntrinsicElements {
      View: React.ComponentType<any>;
      Text: React.ComponentType<any>;
    }
  }
}

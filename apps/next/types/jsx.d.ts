// Add TypeScript JSX support for HTML elements in Next.js app
import React from 'react';

declare global {
  namespace JSX {
    interface IntrinsicElements {
      [elemName: string]: any;
      div: React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>;
      h1: React.DetailedHTMLProps<React.HTMLAttributes<HTMLHeadingElement>, HTMLHeadingElement>;
      p: React.DetailedHTMLProps<React.HTMLAttributes<HTMLParagraphElement>, HTMLParagraphElement>;
      span: React.DetailedHTMLProps<React.HTMLAttributes<HTMLSpanElement>, HTMLSpanElement>;
      a: React.DetailedHTMLProps<React.AnchorHTMLAttributes<HTMLAnchorElement>, HTMLAnchorElement>;
      img: React.DetailedHTMLProps<React.ImgHTMLAttributes<HTMLImageElement>, HTMLImageElement>;
      ul: React.DetailedHTMLProps<React.HTMLAttributes<HTMLUListElement>, HTMLUListElement>;
      li: React.DetailedHTMLProps<React.HTMLAttributes<HTMLLIElement>, HTMLLIElement>;
      button: React.DetailedHTMLProps<React.ButtonHTMLAttributes<HTMLButtonElement>, HTMLButtonElement>;
    }
  }
}

import * as React from 'react';

declare module 'react' {
  type FC<P = {}> = React.FunctionComponent<P>;
  type ReactNode = React.ReactNode;
  type FormEvent<T = Element> = React.FormEvent<T>;
  type StateType<T> = React.Dispatch<React.SetStateAction<T>>;
}

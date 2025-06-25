import type { CompositeScreenProps, NavigatorScreenParams } from '@react-navigation/native';
import type { BottomTabScreenProps } from '@react-navigation/bottom-tabs';
import type { NativeStackScreenProps } from '@react-navigation/native-stack';
import type { ComponentProps } from 'react';
import type { FontAwesome } from '@expo/vector-icons';

// Define parameter lists for different navigation stacks
export type RootStackParamList = {
  home: undefined;
  login: undefined;
  signup: undefined;
  tabs: NavigatorScreenParams<TabParamList> | undefined;
  exclusive: undefined;
  i18n: undefined;
};

export type RootStackScreenProps<Screen extends keyof RootStackParamList> = NativeStackScreenProps<
  RootStackParamList,
  Screen
>;

export type TabParamList = {
  home: undefined;
  exclusive: undefined;
  i18n: undefined;
};

export type TabScreenProps<Screen extends keyof TabParamList> = CompositeScreenProps<
  BottomTabScreenProps<TabParamList, Screen>,
  NativeStackScreenProps<RootStackParamList>
>;

export function useClientOnlyValue<S, C>(server: S, client: C): S | C;

// Re-export navigation types for convenience
export { Theme, NavigationProp, NavigatorScreenParams } from '@react-navigation/native';

declare global {
  namespace ReactNavigation {
    interface RootParamList extends RootStackParamList {}
  }
}

declare module '@react-navigation/bottom-tabs' {
  import { NavigatorScreenParams } from '@react-navigation/native';
  import { TabParamList } from './navigation';

  export function createBottomTabNavigator<ParamList extends object = TabParamList>(): {
    Navigator: React.ComponentType<any>;
    Screen: React.ComponentType<any>;
  };

  export type BottomTabNavigationProp<ParamList extends object = TabParamList, RouteName extends keyof ParamList = keyof ParamList> = {
    navigation: any;
    route: any;
  };
}

declare module '@react-navigation/native' {
  import { NavigatorScreenParams } from '@react-navigation/core';
  import { RootStackParamList, TabParamList } from './navigation';

  export function useNavigation<T = any>(): T;
  export function useRoute<T = any>(): T;
  export function createNavigatorFactory<T extends React.ComponentType<any>>(Navigator: T): () => T;

  export type Theme = {
    dark: boolean;
    colors: {
      primary: string;
      background: string;
      card: string;
      text: string;
      border: string;
      notification: string;
    };
  };

  export const DarkTheme: Theme;
  export const DefaultTheme: Theme;

  export type NavigationProp<ParamList extends object, RouteName extends keyof ParamList = keyof ParamList> = {
    navigation: any;
    route: any;
  };

  export type NavigatorScreenParams<ParamList extends object> = {
    screen: keyof ParamList;
    params?: ParamList[keyof ParamList];
  };
}

declare module '@react-navigation/native-stack' {
  import { RootStackParamList } from './navigation';

  export function createNativeStackNavigator<ParamList extends object = RootStackParamList>(): {
    Navigator: React.ComponentType<any>;
    Screen: React.ComponentType<any>;
  };

  export type NativeStackNavigationProp<ParamList extends object = RootStackParamList, RouteName extends keyof ParamList = keyof ParamList> = {
    navigation: any;
    route: any;
  };
}

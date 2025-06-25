// Define the types for navigation in the app
export type RootStackParamList = {
  Home: undefined;
  Exclusive: undefined;
  Login: undefined;
  Dashboard: undefined;
  Profile: undefined;
  ForgotPassword: undefined;
};

// Tab navigation types
export type RootTabParamList = {
  Home: undefined;
};

// Type for navigation prop
export type NavigationType = {
  navigate: (screen: keyof RootStackParamList) => void;
  getCurrentRoute?: () => { name: string } | undefined;
};

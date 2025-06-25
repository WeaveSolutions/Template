import React from 'react';
import { createBottomTabNavigator } from '@react-navigation/bottom-tabs';
import { HomeScreen } from '@shared/pages';
import { Platform } from 'react-native';
import { MobileNavBar } from '@shared/components';
import { RootTabParamList } from '../types/navigation';

// Declare the type for the bottom tab navigator
const Tab = createBottomTabNavigator<RootTabParamList>();

export function TabNavigator() {
  return (
    <Tab.Navigator
      screenOptions={{
        headerShown: false,
        tabBarStyle: {
          display: 'none',
        },
      }}
      tabBar={(props: any) => <MobileNavBar {...props} />}
    >
      <Tab.Screen 
        name="Home" 
        component={HomeScreen}
        options={{
          tabBarIcon: ({ color }: { color: string }) => (
            <span style={{ color }}>🏠</span>
          ),
        }}
      />
    </Tab.Navigator>
  );
}

export { TabNavigator as Tabs };

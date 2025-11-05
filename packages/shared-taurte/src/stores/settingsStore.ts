import { writable } from "svelte/store";

export interface Settings {
  theme: "light" | "dark" | "auto";
  notifications: boolean;
  language: string;
  fontSize: "small" | "medium" | "large";
  autoSync: boolean;
  [key: string]: any;
}

const defaultSettings: Settings = {
  theme: "auto",
  notifications: true,
  language: "en",
  fontSize: "medium",
  autoSync: true,
};

function createSettingsStore() {
  const { subscribe, set, update } = writable(defaultSettings);

  return {
    subscribe,
    updateSetting: (key: keyof Settings, value: any) => {
      update((settings) => ({
        ...settings,
        [key]: value,
      }));
    },
    updateSettings: (newSettings: Partial<Settings>) => {
      update((settings) => ({
        ...settings,
        ...newSettings,
      }));
    },
    reset: () => set(defaultSettings),
  };
}

export const settingsStore = createSettingsStore();

// @weave/bi-core - Shared BI Dashboard Core
// Platform-agnostic business intelligence infrastructure
// Works with Nexpo (React) and Taurte (Svelte) across Web, Mobile, and Desktop

// Re-export everything for convenience
export * from './types';
export * from './api';
export * from './utils';
export * from './mock-data';
export * from './config';

// Default export for easy imports
import * as types from './types';
import * as api from './api';
import * as utils from './utils';
import * as mockData from './mock-data';
import config from './config';

export default {
  types,
  api,
  utils,
  mockData,
  config
};

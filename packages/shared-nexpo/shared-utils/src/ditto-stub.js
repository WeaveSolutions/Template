export class OnlinePlaygroundAuthenticator {
  constructor(identity) {
    this.identity = identity;
  }
}

class CollectionStub {
  find(query = {}) {
    return {
      exec: async () => ({ items: [] }),
      subscribe: (cb) => ({ cancel: () => void 0 })
    };
  }
  findByID(id) {
    return {
      update: async () => void 0,
      remove: async () => void 0
    };
  }
  insert = async () => ({ inserted: 1 });
}

export class Ditto {
  constructor(identity, options) {
    this.identity = identity;
    this.options = options;
    this._collections = new Map();
    this.sync = {
      configure: async () => void 0
    };
  }
  collection(name) {
    if (!this._collections.has(name)) {
      this._collections.set(name, new CollectionStub());
    }
    return this._collections.get(name);
  }
  async startSync() { return; }
  async stopSync() { return; }
}

export default { Ditto, OnlinePlaygroundAuthenticator };

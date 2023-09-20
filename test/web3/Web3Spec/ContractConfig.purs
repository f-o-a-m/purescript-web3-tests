module Web3Spec.Live.ContractConfig where

import Chanterelle.Types.Deploy (ContractConfig, NoArgs, constructorNoArgs, noArgs)

complexStorageCfg :: ContractConfig NoArgs
complexStorageCfg =
  { filepath: "build/contracts/ComplexStorage.json"
  , name: "ComplexStorage"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

mockERC20Cfg :: ContractConfig NoArgs
mockERC20Cfg =
  { filepath: "build/contracts/MockERC20.json"
  , name: "MockERC20"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

multiFilterCfg :: ContractConfig NoArgs
multiFilterCfg =
  { filepath: "build/contracts/Multifilter.json"
  , name: "Multifilter"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

payableCfg :: ContractConfig NoArgs
payableCfg =
  { filepath: "build/contracts/PayableTest.json"
  , name: "PayableTest"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

simpleErrorCfg :: ContractConfig NoArgs
simpleErrorCfg =
  { filepath: "build/contracts/SimpleErrorTest.json"
  , name: "SimpleErrorTest"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

simpleStorageCfg :: ContractConfig NoArgs
simpleStorageCfg =
  { filepath: "build/contracts/SimpleStorage.json"
  , name: "SimpleStorage"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

# [Unreleased](https://github.com/symbiont-io/detsys-testkit/compare/v0.0.1...main) (2021-02-18)


### Bug Fixes

* **bazel:** fix deps for cli/cmd shouldn't be vendored ([1ba2932](https://github.com/symbiont-io/detsys-testkit/commit/1ba2932ada5db10ee78ab533dc9675c40940655f))
* **bazel:** remove more vendored deps ([b80b3ba](https://github.com/symbiont-io/detsys-testkit/commit/b80b3bafd89ef048d50989788ed579d44b49bd1b))
* **checker:** add back graalvm flag that got accidently removed ([1881ab8](https://github.com/symbiont-io/detsys-testkit/commit/1881ab890ae1f3cc1e9d7f65a020e2a01c2f1138))
* **checker:** test- and run-ids need to be parsed ([f36317c](https://github.com/symbiont-io/detsys-testkit/commit/f36317c97eb33c95c94e3894a9013ba48a3cbd57))
* **cli:** add ldfi to the output of the versions subcommand ([580952a](https://github.com/symbiont-io/detsys-testkit/commit/580952a5b1198e8ad022b0b930aa2c74f3ff28dc))
* **executor:** ScheduledEvent was not parsing meta field properly ([eb10a31](https://github.com/symbiont-io/detsys-testkit/commit/eb10a31bc1390d945fe84e1dac4397e33dd4afd4))
* **executor:** update comment about message shuffling and move inits code. ([4510c52](https://github.com/symbiont-io/detsys-testkit/commit/4510c52f4bed5360ca304b2c45b279c0ade06f42))
* **ldfi:** add db migration for faults ([22b43c3](https://github.com/symbiont-io/detsys-testkit/commit/22b43c37317152934a114179866b9d9239945f00))
* **ldfi:** avoid reintroducing the same crashes ([d195b2c](https://github.com/symbiont-io/detsys-testkit/commit/d195b2c469e11dfd3c6455973ab967978a2656c7))
* **ldfi:** crashes were not handled correctly, also disable debugging output ([0083094](https://github.com/symbiont-io/detsys-testkit/commit/0083094cab5805fa0fa0337d5a58c8b79e52da98))
* **ldfi:** fix bug where previous faults couldn't be reintroduced at all ([15ec138](https://github.com/symbiont-io/detsys-testkit/commit/15ec138f97d9528c66894b91a5a2e43d54426650))
* **ldfi:** negation of formula should only happen once ([3095355](https://github.com/symbiont-io/detsys-testkit/commit/309535598ad151fa733a9abb8558545951ea52ee))
* **ldfi:** previous faults constraint is too strong ([a6fd21b](https://github.com/symbiont-io/detsys-testkit/commit/a6fd21bc2000f7c87689295764b7d5d12eb586e7))
* **ldfi:** reenable crashes, if --crashes flag is greater than 0 ([561ab93](https://github.com/symbiont-io/detsys-testkit/commit/561ab937ed5035ccfa4be1d56d916263cd9a3565))
* **ldfi:** s/runid/run-id/ ([ffdad0e](https://github.com/symbiont-io/detsys-testkit/commit/ffdad0ebe023b1f20e67e306d2e5ebf32e2da36b))
* **ldfi2:** don't crash nodes until after they have sent something ([9b20522](https://github.com/symbiont-io/detsys-testkit/commit/9b2052277926c9672e79851708616b8276623412))
* **ldfi2:** marshalling of events and replace partial foldl1 ([99eae68](https://github.com/symbiont-io/detsys-testkit/commit/99eae68f426c4e36164626cebf867486ccaa650d))
* **logger:** add sleep to avoid 100% cpu issue, adjust buffer length and fix logging ([1942174](https://github.com/symbiont-io/detsys-testkit/commit/19421749b49d1c4fb1083092c1a7d9cb8eb1a3ba))
* **logger:** len(buffer) >= BUFFER_LEN rather than <= ([738807f](https://github.com/symbiont-io/detsys-testkit/commit/738807f4d93a97714c500d9dcc8d0bf44ff82848))
* **logger:** lower max starve count from 32 to 8. ([e654d3d](https://github.com/symbiont-io/detsys-testkit/commit/e654d3dfd94bf6f15a27131bb0472d82363a7002))
* **logger:** make a new buffer slice and fix logging ([a025d5f](https://github.com/symbiont-io/detsys-testkit/commit/a025d5f2752d34a7fd6215885f1405be30726749))
* **logger:** remove starve (thanks Daniel), and rename item to entry ([9e3b1a0](https://github.com/symbiont-io/detsys-testkit/commit/9e3b1a00d9aada1065cf1502b87129db64c83755))
* **logger:** use bufio reader rather than scanner ([68d5186](https://github.com/symbiont-io/detsys-testkit/commit/68d5186679924c0796fd9720a8bbe2d3a6232f8a))
* **logger:** use tab instead of semicolon for separator, make dequeue blocking ([1a0cbc5](https://github.com/symbiont-io/detsys-testkit/commit/1a0cbc5cf43e829e272890ec743a9f1e882fadef))
* **nix:** Update vendorSha256 ([6bb138f](https://github.com/symbiont-io/detsys-testkit/commit/6bb138fbf4639b98cf1f9a5a698b051e370d29e1))
* **readme:** spelling mistakes. ([9f13c5e](https://github.com/symbiont-io/detsys-testkit/commit/9f13c5e60badcae4bb5b8b6845c2e5ff34e2a038))
* **scheduler:** bug in execute-or-tick not returning a tuple. ([0ec0059](https://github.com/symbiont-io/detsys-testkit/commit/0ec0059b0413982ce15879aef8ea68da87e3a13d))
* **scheduler:** set :sent-logical-time on initial messages ([aa8a161](https://github.com/symbiont-io/detsys-testkit/commit/aa8a1619dfe618f163cf282b5f5bc65127cd8294))
* **sut:** make the testsuites compile again. ([e93bb7e](https://github.com/symbiont-io/detsys-testkit/commit/e93bb7ee528cc469fbaec2aba7b64c904fe7a29a))
* **sut/broadcast:** add event logger ([0e9055c](https://github.com/symbiont-io/detsys-testkit/commit/0e9055c7d2041ed1d227dc89840add3ee668802f))
* **sut/broadcast:** make round 2 fail in the right way ([bfa5eaf](https://github.com/symbiont-io/detsys-testkit/commit/bfa5eaf45cd61d0e6296d2515e6ead68dddfa5f8))
* **sut/broadcast:** make round 4 pass by unmarshaling ack properly ([3bbf4c4](https://github.com/symbiont-io/detsys-testkit/commit/3bbf4c40a0cd0529ccde21120fe624e004b29bca))
* **sut/broadcast:** use timers instead of ticks ([611a8ad](https://github.com/symbiont-io/detsys-testkit/commit/611a8ad4f81df24b720a033ae991af364fc6e97f))
* **sut/register:** fix module names and imports. ([f433103](https://github.com/symbiont-io/detsys-testkit/commit/f433103422ca43f0eb763846cc1c8e5983867d99))


### Code Refactoring

* **db:** squash all migrations ([aba11ab](https://github.com/symbiont-io/detsys-testkit/commit/aba11ab9ff2ebd951a324829d38fbaf5fe9baf8e))


### Features

* **bazel:** add and run gazelle ([df36561](https://github.com/symbiont-io/detsys-testkit/commit/df36561227f2100df2423d43f0a71f6b176a6b59))
* **cli:** add generator to the output of the versions subcommand ([a5ad6b4](https://github.com/symbiont-io/detsys-testkit/commit/a5ad6b4d9705c144cec82d71763a0beaa486fe99))
* **cli:** add logger up and down subcommands ([911101c](https://github.com/symbiont-io/detsys-testkit/commit/911101c06a64738748b7785b447eaf8fde4b2634))
* **cli:** make it possible to step through the scheduler ([5296932](https://github.com/symbiont-io/detsys-testkit/commit/529693241ed055b830e446602669bbf9a8f5583b))
* **db:** add down migration for event_log ([0fcf1ac](https://github.com/symbiont-io/detsys-testkit/commit/0fcf1acd0dbcce59a6fb1379867bcec69201db2d))
* **db:** CHECK constraint on json columns ([f6fb62d](https://github.com/symbiont-io/detsys-testkit/commit/f6fb62d66cfb8a183ba69a6d85434dfe4e204f5e))
* **executor:** Executor now sends the ExecutionStep event ([47a2b75](https://github.com/symbiont-io/detsys-testkit/commit/47a2b758e225dce4ee5f53c1b68e695ca906ce26))
* **executor+scheduler:** Add MetaInfo for each execution step ([f929d76](https://github.com/symbiont-io/detsys-testkit/commit/f929d768cca9af88ab782b612278007acd6191f0))
* **ldfi:** try to be deterministic by setting random_seed ([c2d6915](https://github.com/symbiont-io/detsys-testkit/commit/c2d6915ba7d00fccbc9160d764a09bcb8c8d3232))
* **ldfi2:** add a variant of the solve function which gives back all models ([82f5024](https://github.com/symbiont-io/detsys-testkit/commit/82f50245e04b4386949d821e40f821ede6355c2b))
* **ldfi2:** add command line parsing ([5235ca1](https://github.com/symbiont-io/detsys-testkit/commit/5235ca12b1a7e5f54a3a5a118a69817a87aff6d6))
* **ldfi2:** add database and git commit version support ([a472ca7](https://github.com/symbiont-io/detsys-testkit/commit/a472ca7e2b4c6d68108966ea01b6beee25e3d38d))
* **ldfi2:** add failure spec and at field to events ([f639a5a](https://github.com/symbiont-io/detsys-testkit/commit/f639a5a618cee032da7bcb83977b6f4921a15875))
* **ldfi2:** add iff formula and clean up z3_same ([45453d8](https://github.com/symbiont-io/detsys-testkit/commit/45453d892f23be74a75cdf031a8d6d0d60057198))
* **ldfi2:** add limit to how many solution solve all returns ([c3acb24](https://github.com/symbiont-io/detsys-testkit/commit/c3acb246c1d32a5c53824bab3e0ac375ca823d6e))
* **ldfi2:** add z3 haskell dependency ([c4ebdbc](https://github.com/symbiont-io/detsys-testkit/commit/c4ebdbc21f789ad925bb767751c2d8fe9760798b))
* **ldfi2:** introduce skeleton for translating formulae to sat ([f248786](https://github.com/symbiont-io/detsys-testkit/commit/f2487862ef0df5e62ec224d62578544a264ee785))
* **ldfi2:** introduce solver interface ([4f1683b](https://github.com/symbiont-io/detsys-testkit/commit/4f1683b3277dd656c22c1063d2347922e46dc238))
* **ldfi2:** Make events have a sent field, so we can be better at generating crashes ([74e0783](https://github.com/symbiont-io/detsys-testkit/commit/74e07831a50fcee0fd1656028ed481f0dd9420e5))
* **ldfi2:** Make ldfi output omissions/crashes ([89ddf06](https://github.com/symbiont-io/detsys-testkit/commit/89ddf062c8c4370c0fca391da1c3e36042a174e5))
* **ldfi2:** more work towards integrating library with executable ([c982c57](https://github.com/symbiont-io/detsys-testkit/commit/c982c57fca3dccf66ee9f85468cceac4402e7f44))
* **ldfi2:** parse command line args in executable ([878e18b](https://github.com/symbiont-io/detsys-testkit/commit/878e18b4c486cd864662b8879af2981a40fc0257))
* **ldfi2:** print unknown version of version env var is unset ([9f38a45](https://github.com/symbiont-io/detsys-testkit/commit/9f38a454d7cf2f3fcdc1412119ff7e48852b60e2))
* **ldfi2:** translate our formulae into sat and solve them ([7fdb7c9](https://github.com/symbiont-io/detsys-testkit/commit/7fdb7c956fb9dfdde93b19143861655654870d77))
* **ldfi2:** Use stevana's branch for z3 ([764e200](https://github.com/symbiont-io/detsys-testkit/commit/764e2008e902c6de6b3612c3f406ac9f381dc3c9))
* **lib:** add ability to broadcast a message to multiple recipients ([8516bef](https://github.com/symbiont-io/detsys-testkit/commit/8516befb89f99ed01663aa5efb06fb850b00565b))
* **lib:** add crashes to InjectFaults ([a5fab2c](https://github.com/symbiont-io/detsys-testkit/commit/a5fab2cb75d13f4ab5de8fd3b7e7ee1aebbe7905))
* **lib:** add Init() method to reactor interface. ([700bb5a](https://github.com/symbiont-io/detsys-testkit/commit/700bb5a4328afc63bb7fb01de8104c6137acbd44))
* **lib:** Add TimeFromString ([3febbc3](https://github.com/symbiont-io/detsys-testkit/commit/3febbc332623686092b6919dfcf1f66b53a27802))
* **lib:** Make `Topology` be a custom type ([b30d21e](https://github.com/symbiont-io/detsys-testkit/commit/b30d21edba2d4147e901bcdec999be38e429be55))
* **logger:** add first iteration of logging component ([751b7a3](https://github.com/symbiont-io/detsys-testkit/commit/751b7a3a3d8b79157ddf5b49b8cdfc28bf3a6986))
* **logger:** towards supporting linearisable reads ([43bdcd5](https://github.com/symbiont-io/detsys-testkit/commit/43bdcd59366765ef22cd9a26766b5f3801bb6715))
* **scheduler:** add new create-run-event! endpoint in Scheduler ([d9a957d](https://github.com/symbiont-io/detsys-testkit/commit/d9a957d59ef4a951bbc78137b522e50d4c9e43c7))
* **scheduler:** add support for crashes ([9d4fdfc](https://github.com/symbiont-io/detsys-testkit/commit/9d4fdfcbd74ea49393b668e0de497c21772b4d6f))
* **scheduler:** Emit CreateRun event + add view for runs ([a74608c](https://github.com/symbiont-io/detsys-testkit/commit/a74608c5bc80cf2eb42d4b4b1a9cfcc4ba16f55b))
* **scheduler:** Emit NetworkTrace event from the scheduler ([d5f6936](https://github.com/symbiont-io/detsys-testkit/commit/d5f693668600a7b41c7a2df8a0e98e4f79534588))
* **scheduler:** Support multiple receivers ([eddc4de](https://github.com/symbiont-io/detsys-testkit/commit/eddc4de3f5d86ede89671dc498493652d31b4c12))


### Performance Improvements

* **debugger:** Implement our own sequence diagrams ([cc4e1fd](https://github.com/symbiont-io/detsys-testkit/commit/cc4e1fd261c878691e560b5aef51fe11a663a50f)), closes [#66](https://github.com/symbiont-io/detsys-testkit/issues/66)
* **executor:** Don't do db-lookup to figure out topology ([b3a0139](https://github.com/symbiont-io/detsys-testkit/commit/b3a01395bb5fc92f054da8f1e92122e70bb28680))
* **executor+scheduler:** Remove events only used for profiling ([e5b17fd](https://github.com/symbiont-io/detsys-testkit/commit/e5b17fdced086e47fa9c0935e54a72fe742d709f))
* **scheduler:** Use connection pool ([6a2b843](https://github.com/symbiont-io/detsys-testkit/commit/6a2b8430a140fe871445f5b0c5eddadd2718fb40))


### BREAKING CHANGES

* **db:** migrating down will not work, the database file
(~/.detsys.db) needs to be deleted manually.

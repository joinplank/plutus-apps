Blockfrost endpoint integration for the PAB

How to integrate this version of plutus-apps into your contracts:

1. Change the location for the plutus-apps repository on the .project file to: https://github.com/joinplank/plutus-apps.git
2. Add `plutus-blockfrost` to the list of subdirs
3. Use the latest stable tag: `373db1f9b6583b29fa99270e799dbcf147355d64`
4. In the PAB configuration file of your smart-contract you have the option to choose querying blockfrost or chain index.

To choose blockfrost, you have to add the following configuration:

```yaml
blockfrostConfig:
  bfTokenPath: ./testnet-token
```
where bfTokenPath is the path to the file that holds you blockfrost token.

To choose chain index:

```yaml
chainIndexConfig:
   ciBaseUrl: http://35.223.197.209:9085
   ciWatchedAddresses: []
```

where ciBaseUrl is the url of the ChainIndex.

Keep in mind you can't have both configurations at the same time.
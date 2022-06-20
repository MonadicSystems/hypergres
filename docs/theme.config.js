// theme.config.js
export default {
  projectLink: 'https://github.com/MonadicSystems/hypergres', // GitHub link in the navbar
  docsRepositoryBase: 'https://github.com/MonadicSystems/hypergres/tree/main/docs', // base URL for the docs repository
  titleSuffix: ' – Hypergres',
  nextLinks: true,
  prevLinks: true,
  search: true,
  customSearch: null, // customizable, you can use algolia for example
  darkMode: true,
  footer: true,
  footerText: `MIT ${new Date().getFullYear()} © Rashad Gover`,
  footerEditLink: `Edit this page on GitHub`,
  logo: (
    <>
      <h1 class="font-light tracking-widest text-2xl">Hypergres</h1>
    </>
  ),
  head: (
    <>
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <meta name="description" content="Hypergres: Hypermedia + Postgres" />
      <meta name="og:title" content="Hypergres: Hypermedia + Postgres"/>
    </>
  ),
}


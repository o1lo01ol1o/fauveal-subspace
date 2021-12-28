const colors = require("tailwindcss/colors");

module.exports = {
  purge: {
    enabled: false, // Set to true on deploy only! see: https://tailwindcss.com/docs/optimizing-for-production
    content: ["./frontend/**/*.hs"],
  },
  // darkMode: false, // or 'media' or 'class'
  theme: {
  },
  plugins: ["@tailwindcss/forms"],
};

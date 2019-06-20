Fomantic edits

Various dimension definitions early in the generall CSS settings removed, as they are not compatible with shinydashboard Bootstrap settings

Changes to references to font files such as in "icon".
'theme' need to be a defined resourcePath in zzz.R

/*******************************
             Icon
*******************************/

@font-face {
  font-family: 'Icons';
  src: url("themes/default/assets/fonts/icons.eot");
  src: url("themes/default/assets/fonts/icons.eot?#iefix") format('embedded-opentype'), url("themes/default/assets/fonts/icons.woff2") format('woff2'), url("themes/default/assets/fonts/icons.woff") format('woff'), url("themes/default/assets/fonts/icons.ttf") format('truetype'), url("themes/default/assets/fonts/icons.svg#icons") format('svg');
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-decoration: inherit;
  text-transform: none;
}

/*******************************
         Outline Icons
*******************************/

/* Outline Icon */

/* Load & Define Icon Font */

@font-face {
  font-family: 'outline-icons';
  src: url("themes/default/assets/fonts/outline-icons.eot");
  src: url("themes/default/assets/fonts/outline-icons.eot?#iefix") format('embedded-opentype'), url("themes/default/assets/fonts/outline-icons.woff2") format('woff2'), url("themes/default/assets/fonts/outline-icons.woff") format('woff'), url("themes/default/assets/fonts/outline-icons.ttf") format('truetype'), url("themes/default/assets/fonts/outline-icons.svg#icons") format('svg');
  font-style: normal;
  font-weight: normal;
  font-variant: normal;
  text-decoration: inherit;
  text-transform: none;
}



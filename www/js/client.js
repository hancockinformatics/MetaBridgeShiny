
const handlers = {
  // We're passing a static object, so this will only run ONCE. However, we
  // could link this to button presses to make the loading even more dynamic.
  lazyLoadPackages: num => {
    // First argument is input name
    // Second argument is value to send
    Shiny.onInputChange('sessionInitialized', num);
  },
  initGetStarted: () => {
    const getStartedButton = document.getElementById('getStarted');
    const tutorialButton = document.getElementById('tutorial');
    const aboutButton = document.getElementById('about');
    // Change innerHTML (button text)
    getStartedButton.innerHTML = 'Get started';
    // Remove disabled class
    getStartedButton.classList.remove('disabled');
    // Unhide tutorial button
    tutorialButton.classList.remove('btn-hidden');
    // Unhide about button
    aboutButton.classList.remove('btn-hidden');
    // Add btn-tooltip class
    getStartedButton.classList.add('btn-tooltip');
    // Add title
    getStartedButton.setAttribute('title', "Let's Go!");
    // Activate tippy on this button!
    handlers.activateTooltips(['#getStarted']);
  },

  addToolTips: () => {
    // Disable viz panel on page load
    $("a[data-value='vizPanel']")
      .parent()
      .addClass('disabled');
    // Add tooltip class
    $("a[data-value='vizPanel']").addClass('panel-tooltip');
    // Add title class
    $("a[data-value='vizPanel']").attr(
      'title',
      'Please select a metabolite that has been mapped via KEGG'
    );
  },
  activateTooltips: selectors => {
    const tippyOptions = {
      size: 'big',
      duration: 150
    };
    selectors.map(selector => {
      tippy(selector, tippyOptions);
    });
  }
};

// We must use shiny:sessioninitialized, not DOM Content Loaded
$(document).on('shiny:sessioninitialized', () => {
  handlers.lazyLoadPackages(1);
  handlers.addToolTips();
  handlers.activateTooltips(['.panel-tooltip', '.btn-tooltip']);
});

window.onbeforeunload = () => {
  // First check to see whether Shiny has disconnected
  if (document.getElementById('shiny-disconnected-overlay') === null) {
    // If Shiny is NOT disconnected, confirm exit
    return 'If you navigate away, you will lose your results. Are you sure?';
  }
};

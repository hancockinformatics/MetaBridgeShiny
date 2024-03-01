const handlers = {
  lazyLoadPackages: num => {
    Shiny.onInputChange("sessionInitialized", num);
  },

  initGetStarted: () => {
    const getStartedButton = document.getElementById("getStarted");
    const tutorialButton = document.getElementById("tutorial");
    const aboutButton = document.getElementById("about");

    getStartedButton.innerHTML = "Get started";
    getStartedButton.classList.remove("disabled");
    tutorialButton.classList.remove("btn-hidden");
    aboutButton.classList.remove("btn-hidden");
  }
};

$(document).on("shiny:sessioninitialized", () => {
  handlers.lazyLoadPackages(1);
});

window.onbeforeunload = () => {
  if (document.getElementById("shiny-disconnected-overlay") === null) {
    return "Are you sure you want to leave?";
  }
};

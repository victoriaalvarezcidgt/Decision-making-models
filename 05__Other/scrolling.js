// Function to handle smooth scrolling when a list item is clicked
function scrollToSection(sectionId) {
  const section = document.getElementById(sectionId);
  section.scrollIntoView({ behavior: "smooth" });
}

// Add click event listeners to each list item
document.addEventListener("DOMContentLoaded", function () {
  const listItems = document.querySelectorAll("ul li a");
  listItems.forEach((item) => {
    item.addEventListener("click", function (event) {
      event.preventDefault(); // Prevent default anchor behavior
      const sectionId = item.getAttribute("href").slice(1);
      scrollToSection(sectionId);
    });
  });
});

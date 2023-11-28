/* This file automatically creates a simple blog engine for navigating my blog posts with tags */
/* Copyright (c) 2012 Pavel Panchekha. This code is under the MIT license.*/

// Firefox and Chrome are buggy in some way that makes this necessary
function $(id) {return document.getElementById(id);}
function $$(query, base) {return (base || document).querySelectorAll(query);}

// The main blog namespace, to not polute the global one
window.Blog = {
    // Function blogifies the first top-level heading as exported by Org-Mode
    blogify: function(sel) {
        var blog = $$(sel)[0];
        var all_tags = Blog.tags_used(blog);
        var taglist = document.createElement("ul");
        taglist.id = "taglist";
        for (var i = 0; i < all_tags.length; i++) {
            var taglist_elt = document.createElement("li");
            var link_elt = document.createElement("a");
            link_elt.innerHTML = all_tags[i];
            link_elt.href = "#" + all_tags[i];
            taglist_elt.appendChild(link_elt);
            taglist.appendChild(taglist_elt);
            link_elt.addEventListener("click", Blog.click_tag, false);
        }
        blog.insertBefore(taglist, blog.firstElementChild.nextElementSibling);
    },

    // Finds all tags used as section tags inside some element
    tags_used: function tags_used(base) {
        var seen_tags = {};
        var tag_elts = $$(".tag span", base);
        for (var i = 0; i < tag_elts.length; i++) {
            seen_tags[tag_elts[i].innerHTML] = true;
        }
        var tag_array = [];
        for (var tag in seen_tags) {
            tag_array.push(tag);
        }
        tag_array.sort();
        return tag_array;
    },

    // Sets the `display` CSS property of all elements matching a query
    display: function tags_set(query, display) {
        var elts = $$(query);
        for (var i = 0; i < elts.length; i++) {
            var elt = elts[i];
            var list_elt = elt.parentNode.parentNode;
            list_elt.style.display = display;
        }
    },

    // Simple function to turn a tag on or off (or all tags on or off)
    tag_on:  function(tag) {Blog.display(".tag span." + tag, "list-item")},
    tag_off: function(tag) {Blog.display(".tag span." + tag, "none")},
    all_on:  function()    {Blog.display(".tag span", "list-item")},
    all_off: function()    {Blog.display(".tag span", "none")},

    // The click handler for a tag
    click_tag: function(evt) {
        var elt = evt.target;
        
        if (elt.parentNode.className.match(/\bselected\b/)) {
            // Current node is selected; so, if we click it, we toggle it off, leaving nothing; so everythin on
            Blog.all_on();
            // Unmark all selected tags
            var old = $$(".selected");
            for (var i = 0; i < old.length; i++) {
                old[i].className = old[i].className.replace(" selected", "");
            }
        } else {
            // Current node is not selected, to turn currently-selected stuff off...
            Blog.all_off();
            // And turn on the current tag
            Blog.tag_on(elt.innerHTML);
            // Finally unmark the old selected tag
            var old = $$(".selected");
            for (var i = 0; i < old.length; i++) {
                old[i].className = old[i].className.replace(" selected", "");
            }
            // And mark the new one
            elt.parentNode.className += " selected";
        }
    }
};

// Actually blogify the page when it loads
window.addEventListener("load", function() {Blog.blogify("#content");}, false);

// Also, adjust the styling on the "Subpages" section
(function(){
    $("content").className += " posts";
})();

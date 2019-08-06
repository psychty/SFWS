var searchText;
var filterValue;

var $grid = $('.grid').isotope({
    itemSelector: '.grid-item',
    filter: function() {
        var $this = $(this);
        var ap_title = searchText ? $this.find('.ap_title').text().match(searchText) : true;
        var ap_text = searchText ? $this.find('.ap_text').text().match(searchText) : true;
        var success = searchText ? $this.find('.success').text().match(searchText) : true;
        var selectResult = filterValue ? $this.is(filterValue) : true;
        return ap_title || ap_text || success || selectResult;
    }
});

// Count the number of actions
function updateFilterCount() {
    $filterCount.text(iso.filteredItems.length + ' actions selected');
}

var iso = $grid.data('isotope');
var $filterCount = $('.filter-count');

// store filter for each group
var filters = [];

$('.filter').on('click', '.button', function (event) {
    var $button = $(event.currentTarget);
    // get group key
    var $buttonGroup = $button.parents('.button-group');
    var filterGroup = $buttonGroup.attr('data-filter-group');
    // set filter for group
    filters[filterGroup] = $button.attr('data-filter');
    // combine filters
    filterValue = concatValues(filters);
    // set filter for Isotope
    $grid.isotope({filter: filterValue});
});

updateFilterCount(); // Shows how many actions are selected

// Shows the user which filters are on
$('.button-group').each(function (i, buttonGroup) {
    var $buttonGroup = $(buttonGroup);
    $buttonGroup.on('click', 'button', function () {
        $buttonGroup.find('.is-checked').removeClass('is-checked');
        $(this).addClass('is-checked');
        setTimeout(function () {
            updateFilterCount();
        }, 200);
    });
});

var $anyButtons = $('.filter').find('button[data-filter-group="*"]');
 var $buttons = $('.filter button');

 $('.button--reset').on( 'click', function() {
   // reset filters
   filters = {};
   $grid.isotope({ filter: '*' });
   // reset buttons
   $buttons.removeClass('is-checked');
   $buttonGroup.find('.is-checked').addClass('is-checked');
 });

// flatten object by concatting values
function concatValues(obj) {
    var value = '';
    for (var prop in obj) {
        value += obj[prop];
    }
    return value;
}

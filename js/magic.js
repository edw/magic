var magic_form = function(form, url, destination, focus_element) {
	$.post(url, $(form).serialize(), function(data) {
		$(destination).html(data);
		if(focus_element) {
			var top = $(focus_element).top;
			$('html,body').animate({scrollTop: top}, 1000);
		};
            });
	return false;
};

var magic_link = function(url, destination) {
	$(destination).load(url);
	return false;
};

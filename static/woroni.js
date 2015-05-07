function verify() {
    document.getElementById('not-a-captcha-placeholder-i-promise-govna').style.display = 'none';
    var verify  = $('#verify');
    var captcha = $('#captcha');
    verify.html('Loading');

    $.getScript('https://www.google.com/recaptcha/api.js?onload=add_submit',
                function () {
        var submit = document.createElement('input');
        submit.setAttribute('id','submit');
        submit.setAttribute('type','submit');
        submit.setAttribute('value','Submit');
        captcha.append(submit);
        verify.remove();
        $('#captcha').css('display','block');
    });
};

function httpGet(theUrl) {
    var xmlHttp = null;
    console.log(theUrl);
    xmlHttp = new XMLHttpRequest();
    xmlHttp.open( "GET", theUrl, false);
    xmlHttp.send( null );
    return xmlHttp.responseText;
}

var selected = [];

function set_selected() {
    selected = [];
    tagIds = $('#tags .selected');

    for(var i = 0; i < tagIds.length; i ++) {
        var ix = tagIds[i].getAttribute("tag-id");
        selected[ix] = ix;
    }

    console.log(selected);
}

var this_post_id;

function load() {
    console.log('loaded');
    this_post_id = window.location.pathname.substr(6);
    set_selected();
}

function update_aside(elem) {
    console.log('hello');

    var tagId = elem.getAttribute('tag-id');
    
    if(selected[tagId] != undefined) {
        delete selected[tagId];
        elem.setAttribute('class','tag');
    } else {
        selected[tagId] = tagId;
        elem.setAttribute('class','selected');
    }

    set_selected();
    var url = "/tag/"  + selected.filter(Number) +
              "?page=" + Math.floor(this_post_id/15) +
              "&post=" + this_post_id;
    

    $('#summaries').html(httpGet(url));
    return false;
}

function reveal_credit(fig) {
    console.log('thingy');
    $("figure").each(function(index) {
        var fig    = this;
        var credit = $(fig).find('figcaption.credit');

        credit.css('display', 'none');
        credit.css('background-color', 'rgba(0,0,0,0.5)');
        credit.css('opacity', '1.0');
        credit.html("Image by " + credit.html()) 

        $(fig).hover(function() { credit.fadeIn(150); },
                     function() { credit.fadeOut(150); });
    });
}

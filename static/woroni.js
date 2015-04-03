function verify() {
    document.getElementById('not-a-captcha-placeholder-i-promise-govna').style.display = 'none';
    document.getElementById('captcha').style.display = 'block';
    var b0 = document.getElementById('verify');
    var b1 = document.createElement('input');
    b1.setAttribute('id','submit');
    b1.setAttribute('type','submit');
    b1.setAttribute('value','Submit');
    document.getElementById('captcha').appendChild(b1);
    b0.parentNode.removeChild(b0);
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
var asdf;

function set_selected() {
    selected = [];
    tagIds = $('#tags .selected .tag-id');

    for(var i = 0; i < tagIds.length; i ++) {
        var ix = tagIds[i].innerHTML;
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
    var span = elem.getElementsByClassName('tag-id');
    var ix   = span[0].innerHTML;

    console.log(ix);
    
    if(selected[ix] != undefined) {
        delete selected[ix];
        elem.setAttribute('class','tag');
    } else {
        selected[ix] = ix;
        elem.setAttribute('class','selected');
    }

    set_selected();
    var url = "/tag/" + selected.filter(Number) + '?aside=' + this_post_id;
    $('#summaries').html(httpGet(url));
    return false;
}

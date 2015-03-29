var ajaxGet = function (url, callback) {
    xhr = new XMLHttpRequest();
    xhr.open("GET", url,true);
    xhr.onreadystatechange=function() {
        if (xhr.readyState==4) {
            callback(xhr.responseText)
        }
    }
    xhr.send(null);
    return xhr;
}

ajaxGet('http://localhost:8080/post/1',
        function(response) {
            console.log(response);
            json = JSON.parse(response);
            console.log(json);
        })

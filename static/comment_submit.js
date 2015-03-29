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

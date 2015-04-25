<div id="thread">
  <h1 id="comments-title">Comments</h1>
  <post-comments/>
  <h1 id="comments-title"> Have your say! </h1>
  
  <div style="clear:both;"></div>

  <form id="comment-submit" method="POST">
    <input id="submit-name" type="text" name="name" placeholder="Name" />
    <input id="submit-email" type="text" name="email" placeholder="Authentication (optional)" />
    <div style="clear:both;"></div>
    <textarea id="submit-field" name="content" rows="4" placeholder="Comment"></textarea>
    <div style="clear:both;"></div>

    <a id="verify" onclick="verify()">Verify</a>

    <div id="not-a-captcha-placeholder-i-promise-govna"></div>

    <!--
    <div id="captcha" >
      <recaptcha-div/>
      <script src="https://www.google.com/recaptcha/api.js" async defer></script>
    </div>
    -->

    <div style="clear:both;"></div>
    
  </form>
</div>

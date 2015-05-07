<bind tag="page-title"> Woroni Search - <search-title/> </bind>
<apply template="default">
  <div id="search">
    <script type="text/javascript" src="/static/search.js"></script>
    <form onsubmit="update_search(); return false;" method="GET"
          id="search-field">
      <div class="pull-right field">
        <input id="search-text"
               type="text"
               name="terms"
               onkeypress="updated = true;"
               placeholder="Search terms"
               value="${search-title}">
      </div>
    </form>
    <div style="clear:both;"></div>
    <div id="search-summaries">
      <post-summaries/>
      <div style="clear:both"></div>
    </div>
  </div>
</apply>
<bind tag="page-title"> Woroni Search - <search-title/> </bind>
<apply template="default">
  <form id="search-field">
    <div class="pull-left field-label">Search</div>
    <div class="pull-right field">
      <input type="text" name="terms" placeholder="Search terms" search-terms>
    </div>
  </form>
  <div style="clear:both;"></div>
  <div id="home-summaries">
    <post-summaries/>
  </div>
</apply>
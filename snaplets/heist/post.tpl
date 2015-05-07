<post>
<apply template="default">

  <div id="authors-info">
    <h1>About the author<authors-plural/></h1>
    <authors-info/>
    <script src="/static/authors-info.js"></script>
  </div>

  <article>

    <time> <post-times/> </time>
    <h1> <page-title/> </h1>
    <h2 class="description">By <post-authors/> under <post-tags/></h2>
    <div style="clear:both;"></div>

    <div id="post-content">
      <post-content/>
    </div>

    <script>console.log('reveal_credit()'); reveal_credit();</script>

    <apply template="thread"/>

  </article>
  
  <aside onload="load();">
    <div id="tags">
      <all-tags/>
    </div>
    <div id="summaries">
      <post-aside />
    </div>
    <div style="clear:both;"></div>
  </aside>
</apply>
</post>

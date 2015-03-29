<!DOCTYPE html>
<html>
  <post>

  <head>
    <meta charset='utf-8'>
    <title> <post-title /> </title>
    <link title="woroni" rel="stylesheet" type="text/css" href="/static/woroni.css"/>
    <script type="text/javascript" src="/static/comment_submit.js"></script>

  </head>
  <body>

    <div id="everything"> 
      <apply template="header"/>
      <article>
        <time> <post-times/> </time>
        <h1> <post-title/> </h1>
        <h2 class="description">By <post-authors/> under <post-tags/></h2>
        <div style="clear:both;"></div>


        <div id="post-content">
          <post-content/>
        </div>

        <apply template="thread"/>

      </article>
  
      <aside>
        <div id="tags">
          <all-tags/>
        </div>

        <post-summaries />
        <div style="clear:both;"></div>
      </aside>
      <div style="clear:both;"></div>
   </div>
  </body>
        
  </post>
</html>

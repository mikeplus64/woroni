<bind tag="page-title"> Woroni </bind>
<apply template="default">
  <script src="/static/home.js"></script>
  <div id="home-summaries">

    <div id="new">
      <new-posts/>
    </div>

    <div id="features">
      <h1 id="features-heading">Featured articles</h1>
      <featured-posts/>
    </div>

    <div id="misc">
      <div id="now-playing" class="invisible-box">
        <iframe id="player"
                scrolling="no"
                src="https://mixlr.com/users/2179348/embed?color=fc9f3d&autoplay=false"
                frameborder="no"
                width="100%"
                height=148
                onload="show_player(this);"></iframe>
      </div>

      <div class="little-box">
        <h2>Get involved!</h2>
        <p>
        Woroni is a student newspaper; by students and for students. Want to
        write, produce, or simply get jerked around by petty nitpicks on
        the Stalkerspace? 
        <a href="mailto:editors@woroni.com.au">editors@woroni.com.au</a>.
        </p>
      </div>

      <a href="/feed.xml" class="little-box">
        <h2>RSS/Atom</h2>
        <p> Add us to your favourite feed reader! </p>
      </a>

      <div id="" class="little-box">
        <h2>Contributors</h2>
        <table id="contributors">

<tr> <td> <a class="author" href="/author/no_idea"> Ellen Tdevanion </a> </td> <td> 26 </td> </tr>
<tr> <td> <a class="author" href="/author/no_idea"> Zach Mackey </a> </td> <td> 25 </td> </tr>
<tr> <td> <a class="author" href="/author/no_idea"> Joshua Chu-Tan </a> </td> <td> 23 </td> </tr>
<tr> <td> <a class="author" href="/author/no_idea"> Robert Selth </a> </td> <td> 22 </td> </tr>
<tr> <td> <a class="author" href="/author/no_idea"> Mark Fabian </a> </td> <td> 21 </td> </tr>
<tr> <td> <a class="author" href="/author/no_idea"> Ben Latham </a> </td> <td> 20 </td> </tr>
<tr> <td> <a class="author" href="/author/no_idea"> Elise Horspool </a> </td> <td> 20 </td> </tr>
<tr> <td> <a class="author" href="/author/no_idea"> Woroni Developers </a> </td> <td> 20 </td> </tr>
<tr> <td> <a class="author" href="/author/no_idea"> 2fuddha </a> </td> <td> 19 </td> </tr>
<tr> <td> <a class="author" href="/author/no_idea"> Tara Shenoy </a> </td> <td> 18 </td> </tr>

        </table>

      </div>

    </div>

    <div style="clear:both"></div>
  </div>
</apply>

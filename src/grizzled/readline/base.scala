/**
 * Classes, traits, and objects that provide readline semantics. The main
 * <tt>grizzled.readline</tt> package provides the general interface, along
 * with some factory methods. Underlying implementations exist for GNU
 * Readline, JLine and a generic implementation.
 */
package grizzled.readline

/**
 * Models a Readline history: an object that holds previously read
 * lines.
 */
trait History
{
    /**
     * Add a line to the history. Does not add the line if it is
     * identical to the most recently added line.
     *
     * @param line  the line to add
     */
    def +=(line: String) =
    {
        last match
        {
            case None    => append(line)
            case Some(s) => if (s != line) append(line)
        }
    }

    /**
     * Unconditionally appends the specified line to the history.
     *
     * @param line  the line to add
     */
    protected def append(line: String)

    /**
     * Get the last (i.e., most recent) entry from the buffer.
     *
     * @return the most recent entry, as an <tt>Option</tt>, or
     *         <tt>None</tt> if the history buffer is empty
     */
    def last: Option[String]

    /**
     * Get the contents of the history buffer, in a list.
     *
     * @return the history lines
     */
    def get: List[String]

    /**
     * Clear the history buffer
     */
    def clear

    /**
     * Save the contents of the history to the specified path.
     *
     * @param path  where to save the history
     */
    def save(path: String) =
    {
        import _root_.java.io.FileWriter

        val f = new FileWriter(path)
        for (line <- get)
            f.write(line + "\n")
        f.close
    }

    /**
     * Load the contents of the history from the specified path, overwriting
     * any existing history data (i.e., clearing the history buffer first).
     *
     * @param path  where to save the history
     */
    def load(path: String) =
    {
        import _root_.java.io.{FileReader, BufferedReader}

        val f = new BufferedReader(new FileReader(path))
        clear

        def readHistoryLine: Unit =
        {
            val line = f.readLine
            if (line != null)
            {
                this += line
                readHistoryLine
            }
        }

        readHistoryLine
        f.close
    }
}

/**
 * Models a completer: An object that, given a line of input and a token
 * within that line, finds possible completions for the token.
 */
trait Completer
{
    /**
     * Get all completions for a token.
     *
     * @param token  the token being completed
     * @param line   the current input line, which includes the token
     *
     * @return a list of completions, or Nil if there are no matches
     */
    def complete(token: String, line: String): List[String]
}

/**
 * A completer that doesn't do anything. Useful as a default.
 */
class NullCompleter extends Completer
{
    def complete(token: String, line: String): List[String] = Nil
}

/**
 * Defines the readline-like functionality supported by this API. A
 * <tt>Readline</tt> class provides:
 * <ul>
 *   <li> a means to read lines of input from (presumably) a terminal
 *   <li> a history mechanism
 *   <li> an optional tab-completion capability
 * </ul>
 */
trait Readline
{
    /**
     * A printable name for the implementation.
     */
    val name: String

    /**
     * The completer, if any.
     */
    var completer: Completer = new NullCompleter

    /**
     * The history buffer. The actual implementation depends on the underlying
     * class.
     */
    val history: History

    /**
     * Whether or not to add lines to the history automatically. Defaults to
     * <tt>true</tt>. Some clients might want to handle the history themselves.
     */
    val autoAddHistory: Boolean = true

    /**
     * Read a line of input from the console.
     *
     * @param prompt  the prompt to display before reading.
     *
     * @return An <tt>Option</tt> containing the line (e.g., <tt>Some(...)</tt>)
     *         or <tt>None</tt> on EOF.
     */
    def readline(prompt: String): Option[String] =
    {
        val line = doReadline(prompt)
        if ((line != None) && autoAddHistory)
            history += line.get
            
        line
    }

    /**
     * The actual function that does the readline work, provided by the
     * concrete implementing class.
     *
     * @param prompt  the prompt to display before reading.
     *
     * @return An <tt>Option</tt> containing the line (e.g., <tt>Some(...)</tt>)
     *         or <tt>None</tt> on EOF.
     */
    private[readline] def doReadline(prompt: String): Option[String]

    /**
     * Produce a readable version of this object.
     */
    override def toString = name
}

/**
 * Companion factory object, used to instantiate particular readline
 * implementations.
 */
object Readline
{
    /**
     * An enumeration of the various underlying readline APIs supported by
     * this API. Note that a given API may or may not be available on a
     * particular machine. The following implementations are currently
     * supported:
     *
     * <ul>
     *   <li><tt>GNUReadline</tt>: The GNU Readline library. Requires the
     *       JavaReadline jar
     *       (<a href="http://java-readline.sourceforge.net/">http://java-readline.sourceforge.net/</a>)
     *       and the GNU Readline library
     *       (<a href="http://tiswww.case.edu/php/chet/readline/rltop.html">http://tiswww.case.edu/php/chet/readline/rltop.html</a>).
     *
     *   <li><tt>Editline</tt>: The Editline library, originally from BSD Unix.
     *       Requires the JavaReadline jar
     *       (<a href="http://java-readline.sourceforge.net/">http://java-readline.sourceforge.net/</a>)
     *       and the Editline library
     *       <a href="http://www.thrysoee.dk/editline/">http://www.thrysoee.dk/editline/</a>.
     *
     *   <li><tt>Getline</tt>: The Getline library. Requires the JavaReadline jar
     *       (<a href="http://java-readline.sourceforge.net/">http://java-readline.sourceforge.net/</a>)
     *       and the Getline library.
     *
     *   <li><tt>JLine</tt>: The JLine library. Requires the JLine jar
     *       (<a href="http://jline.sourceforge.net/">http://jline.sourceforge.net/</a>).
     *   <li><tt>Simple</tt>: A simple, not-editing, pure Java implementation
     * </ul>
     */
    object ReadlineType extends Enumeration
    {
        type ReadlineType = Value

        val GNUReadline = Value
        val EditLine = Value
        val GetLine = Value
        val JLine = Value
        val Simple = Value
    }

    import ReadlineType._

    /**
     * Get the specified <tt>Readline</tt> implementation.
     *
     * @param readlineType   the <tt>ReadlineType</tt> to use
     * @param appName        an arbitrary name of the calling application
     * @param autoAddHistory whether lines read by the function should
     *                       automatically be added to the history. If this
     *                       parameter is <tt>false</tt>, then the caller
     *                       is responsible for adding lines to the history.
     *
     * @return the appropriate <tt>Readline</tt> implementation.
     *
     * @throws UnsatisfiedLinkError can't find the underlying library
     */
    def apply(readlineType: ReadlineType,
              appName: String,
              autoAddHistory: Boolean): Readline =
    {
        val cls = readlineType match
        {
            case GNUReadline =>
                Class.forName("grizzled.readline.javareadline.GNUReadlineImpl")

            case EditLine =>
                Class.forName("grizzled.readline.javareadline.EditlineImpl")

            case GetLine =>
                Class.forName("grizzled.readline.javareadline.GetlineImpl")

            case JLine =>
                Class.forName("grizzled.readline.jline.JLineImpl")

            case Simple =>
                Class.forName("grizzled.readline.simple.SimpleImpl")
        }

        val constructor = cls.getConstructor(classOf[String], classOf[Boolean])
        val histFlagParam = autoAddHistory.asInstanceOf[Object]
        try
        {
            val rl = constructor.newInstance(appName, histFlagParam)
            rl.asInstanceOf[Readline]
        }

        catch
        {
            case e: java.lang.reflect.InvocationTargetException =>
                throw e.getCause
        }
    }

    /**
     * Get the specified <tt>Readline</tt> implementation, with
     * <tt>autoAddHistory</tt> set to <tt>true</tt>.
     *
     * @param readlineType  the <tt>ReadlineType</tt> to use
     * @param appName       an arbitrary name of the calling application
     *
     * @return the appropriate <tt>Readline</tt> implementation.
     *
     * @throws UnsatisfiedLinkError can't find the underlying library
     */
    def apply(readlineType: ReadlineType, appName: String): Readline =
        apply(readlineType, appName, true)
}

/**
 * Simple tester.
 */
object Test
{
    import Readline.ReadlineType._
    import Readline.ReadlineType
    import java.io.File

    val types = Map("readline" -> ReadlineType.GNUReadline,
                    "editline" -> ReadlineType.EditLine,
                    "getline"  -> ReadlineType.GetLine,
                    "jline"    -> ReadlineType.JLine,
                    "simple"   -> ReadlineType.Simple)

    def main(args: Array[String]) =
    {
        val t =
            if (args.length == 0)
                ReadlineType.GNUReadline
            else
                types(args(0))

        val r = Readline(t, "Test", false)
        println("Using: " + r)

        val HistoryPath = "/tmp/readline.hist"
        val historyFile = new File(HistoryPath)
        if (historyFile.exists)
        {
            println("Loading history file \"" + HistoryPath + "\"")
            r.history.load(HistoryPath)
        }

        object completer extends Completer
        {
            val completions = List("linux", "lisa", "mac", "freebsd", "freedos")
            def complete(token: String, line: String): List[String] =
            {
                {for (c <- completions; 
                      if (c.startsWith(token))) yield c}.toList
            }
        }

        r.completer = completer
        var line = r.readline("? ")
        while (line != None)
        {
            val s = line.get
            if ((s == "history") || (s == "h"))
            {
                for ((s, i) <- r.history.get.zipWithIndex)
                    println(i + ": " + s)
            }

            else
            {
                r.history += s
                println(s)
            }

            line = r.readline("? ")
        }

        println("Saving history to file \"" + HistoryPath + "\"")
        r.history.save(HistoryPath)
    }
}
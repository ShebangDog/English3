package dog.shebang.english

package dog.shebang

import cats.data.EitherT
import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}

object Excep extends Throwable

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val program = for
      input <- EitherT(Console[IO].readLine.attempt)
      tree <- EitherT.fromOption(
        parser.Parser.apply(
          lexer.Lexer.apply(input)
        ),
        Excep
      )
      output <- EitherT(Console[IO].println(tree).attempt)
    yield output

    program.value.map {
      case Right(_) => ExitCode.Success
      case Left(_) => ExitCode.Error
    }
  end run
end Main




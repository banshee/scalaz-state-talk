import scalaz._
import Scalaz._

object EitherTExample {

  trait Model
  trait StatsQuery
  trait QueryResult

  object BeforeEitherT {
    def runQuery(str: String, model: Model): String \/ QueryResult = for {
      query <- parseQuery(str)
      res <- performQuery(query, model)
    } yield res

    def parseQuery(s: String): String \/ StatsQuery = "TODO".left
    def performQuery(q: StatsQuery, m: Model): String \/ QueryResult = "TODO".left
  }

  object AfterEitherT {
    trait QueryState
    type QueryStateS[+A] = State[QueryState, A]

    type ET[F[+_], A] = EitherT[F, String, A]
    type QueryStateES[A] = ET[QueryStateS, A]
    object QueryStateES {
      def apply[A](st: QueryStateS[String \/ A]): QueryStateES[A] = EitherT(st)
      def liftE[A](ea: String \/ A): QueryStateES[A] = apply(Applicative[QueryStateS].point(ea))
      def liftS[A](st: QueryStateS[A]): QueryStateES[A] = MonadTrans[ET].liftM(st)

      implicit class HasLiftE[A](ea: String \/ A) {
        def liftE = QueryStateES.liftE[A](ea)
      }
      implicit class HasLiftS[A](qs: QueryStateS[A]) {
        def liftS: QueryStateES[A] = QueryStateES.liftS(qs)
      }
    }

    def runQuery(str: String, model: Model): QueryStateES[QueryResult] = for {
      query <- parseQuery(str)
      res <- performQuery(query, model)
    } yield res

    // Pull in the implicit classes from QueryStateES
    import QueryStateES._
    
    def parseQuery(s: String): QueryStateES[StatsQuery] = for {
      state <- QueryStateES.liftS(State.get[QueryState])
      result <- QueryStateES.liftE("TODO".left[StatsQuery])
      
      // Or, if you prefer to use implicit methods:
      state1 <- State.get[QueryState].liftS
      result1 <- "TODO".left[StatsQuery].liftE
      
      // state1 and result1 are almost the same as state and result, they're just
      // using implicit classes to allow using liftS and liftE as methods instead 
      // of functions.
    } yield result

    def performQuery(q: StatsQuery, m: Model): QueryStateES[QueryResult] = QueryStateES.liftE("TODO".left)
  }

}

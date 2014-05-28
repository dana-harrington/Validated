

object Basics {

  class Html

  object Html {
    def apply(html: String): Html = ???

    def apply(html: scala.xml.Elem): Html = ???
  }

  // A JSON value
  class JsValue

  // A HTTP response
  case class Result(code: Int,
                    content: Html,
                    headers: Map[String, String])

  trait Renderable[T] {
    def render: T
  }

  trait Cartridge extends Renderable[Html]

  trait Template extends Renderable[Result]

  case class UnknownTemplateException(name: String) extends Exception(s"Unknown template $name")

  class BellTemplate(header: Seq[Cartridge],
                     leftNav: Seq[Cartridge],
                     body: Seq[Cartridge],
                     footer: Seq[Cartridge]) extends Template {
    def render = {
      val headerContent: Seq[Html] = header.map(_.render)
      val leftNavContent: Seq[Html] = leftNav.map(_.render)
      val bodyContent: Seq[Html] = body.map(_.render)
      val footerContent: Seq[Html] = footer.map(_.render)
      val html = ???
      Result(200, html, Map())
    }
  }


  object EmptyCartridge extends Cartridge {
    def render = Html("")
  }

  case class TitleCartridge( title: String ) extends Cartridge {
    def render = Html(<h1>title</h1>)
  }

  object EndecaService {
    def getAdvice(query: String): ViewAdvice = ???
  }


  class ViewAdvice(json: JsValue) {
    val templateName: String = ???

    def zone( name: String ): Seq[JsValue] = ???
  }
}

object FakeData {
  val goodCartridgeJson =
    """
      { "@type" : "Title",
        "title" : "Cameras"
      }

    """

  val badJson = {
    """
      ...
      { "@error" : "com.endeca.navigation.ENEException: mumble mumble MDEX mumble"
        "description" : "Something horrible happened, your data is not here."
      }
      ...

    """
  }

}


object BasicPipeline {

  import Basics._

  object Template {
    def getName(json: JsValue): String = ???

    def apply( viewAdvice: ViewAdvice ): Template = {
      val templateName = viewAdvice.templateName
      templateName match {
        case "Bell" => BellTemplate(viewAdvice)
        case _      => throw UnknownTemplateException(templateName)
      }
    }
  }


  object TitleCartridge {
    def fromJson( json: JsValue ): TitleCartridge = ???
  }


  object Cartridge {
    def getType( json: JsValue ): String = ???

    def fromJson( json: JsValue ): Cartridge = {
      getType(json) match {
        case "Title" => TitleCartridge.fromJson(json)
        case _       => EmptyCartridge
      }
    }
  }


  object BellTemplate {
    def apply( viewAdvice: ViewAdvice ): BellTemplate = {
      val header = viewAdvice.zone("header").map(Cartridge.fromJson)
      val leftNav = viewAdvice.zone("leftNav").map(Cartridge.fromJson)
      val body = viewAdvice.zone("body").map(Cartridge.fromJson)
      val footer = viewAdvice.zone("footer").map(Cartridge.fromJson)
      new BellTemplate(header, leftNav, body, footer)
    }
  }


  object SearchController {
    def search(term: String, refinements: Seq[String]) = {
      val query = ???

      val viewAdvice = EndecaService.getAdvice(query)
      val template = Template(viewAdvice)
      template.render
    }
  }

}



object ValidatedImplementation {

  object Valid {
    def apply[A](a: A) = Validated(a, isValid = true)
  }

  object Invalid {
    def apply[A](a: A) = Validated(a, isValid = false)
  }

  case class Validated[+X]( value: X,
                            isValid: Boolean) {

    def map[Y](f: X => Y): Validated[Y] =
      Validated(f(value), isValid)

    def flatMap[Y](f: X => Validated[Y]): Validated[Y] = {
      f(value) match {
        case Validated(fv, fvIsValid) =>
          Validated(fv, this.isValid && fvIsValid)
      }
    }

    def filter(p: X => Boolean): Validated[X] = Validated(value, isValid && p(value))

    def orElse(that: Validated[X]): Validated[X] = if (this.isValid) this else that

    def validity: Validated[Boolean] = Validated(isValid, isValid = true)


    def toEither: Either[X, X] =
      if (this.isValid) Right(value)
      else Left(value)

    def toOption: Option[X] =
      if (this.isValid) Some(value) else None

  }

  object Validated {

    def apply[X](x: X) = Validated(x, isValid = true)

    implicit class ValidateSeq[X](val sv: Seq[Validated[X]]) extends AnyVal {
      def validate = Validated(sv.map(_.value), sv.forall(_.isValid))
      /* single pass version
      def validate = sv.foldRight(Validated(Seq[X](), true)){
        case (Validated(v, vIsValid), Validated(vs, vsIsValid)) =>
          val values = v +: vs
          val validity = vIsValid && vsIsValid
          Validated(values, validity)}
       */
    }
  }

}


object ValidatedVersion {

  import Basics._
  import ValidatedImplementation._
  import BasicPipeline.TitleCartridge

  object Cartridge {
    def getType( json: JsValue ): String = ???

    def fromJson( json: JsValue ): Validated[Cartridge] = {
      getType(json) match {
        case "Title" => TitleCartridge.fromJson(json)
        case _       => Validated(EmptyCartridge, false)
      }
    }
  }

  object Template {
    def getName(json: JsValue): String = ???

    def apply( viewAdvice: ViewAdvice ): Validated[Template] = {
      val templateName = viewAdvice.templateName
      templateName match {
        case "Bell" => BellTemplate(viewAdvice)
        case _      => throw UnknownTemplateException(templateName)
      }
    }
  }

  object BellTemplate {

    def apply(viewAdvice: ViewAdvice): Validated[BellTemplate] = {
      for {
        headZone <- viewAdvice.zone("head").map(Cartridge.fromJson).validate
        leftNavZone <- viewAdvice.zone("leftNav").map(Cartridge.fromJson).validate
        bodyZone <- viewAdvice.zone("body").map(Cartridge.fromJson).validate
        footerZone <- viewAdvice.zone("footer").map(Cartridge.fromJson).validate
      } yield new BellTemplate(headZone, leftNavZone, bodyZone, footerZone)
    }

  }

  object TitleCartridge {
    def fromJson( json: JsValue ): Validated[TitleCartridge] = {
      val getTitle: Option[String] = ???
      Validated( TitleCartridge(getTitle.getOrElse("")),
                 isValid = getTitle.isDefined)
    }
  }

  implicit class ValidatedTemplate(vt: Validated[Template]) extends Renderable[Result] {
    def render: Result =
      vt.map(_.render) match {
        case Validated(result, isCacheable@true) =>
          result
        case Validated(result, isCacheable@false) =>
          result.copy(headers = result.headers + ("Cache-control" -> "no-cache"))
      }
  }

  object SearchController {

    def search(term: String, refinements: Seq[String]): Result = {
      val query = ???

      val viewAdvice = EndecaService.getAdvice(query)
      val template: Validated[Template] = Template(viewAdvice)
      template.render
    }

  }
}

object ValidatedBuilderImplementation {

  import scala.concurrent.Future

  import Basics._
  import ValidatedImplementation._

  case class ValidatedBuilder[+X](asFuture: Future[Validated[X]]) {

    def map[Y](f: X => Y): ValidatedBuilder[Y] =
      new ValidatedBuilder[Y](asFuture.map(_.map(f)))

    def flatMap[Y](f: X => ValidatedBuilder[Y]): ValidatedBuilder[Y] = {
      new ValidatedBuilder(asFuture.flatMap {
        case Validated(va, aIsValid) =>
          val fva = f(va).asFuture
          fva map {
            case Validated(vb, bIsValid) =>
              Validated(vb, aIsValid && bIsValid)
          }
      })
    }
  }

  object ValidatedBuilder {
    def apply[X](x: X, isValid: Boolean): ValidatedBuilder[X] = {
      new ValidatedBuilder(Future.successful(Validated(x,isValid)))
    }

    def fromValidated[X](vx: Validated[X]) = new ValidatedBuilder(Future.successful(vx))

    implicit class ValidatedBuilderSeq[X](val sv: Seq[ValidatedBuilder[X]]) extends AnyVal {
      def validate: ValidatedBuilder[Seq[X]] = {
        val fsv = Future.sequence(sv.map(_.asFuture))
        new ValidatedBuilder(fsv.map(_.validate))
      }
    }
  }

  object Cartridge {
    def getType( json: JsValue ): String = ???

    def fromJson( json: JsValue ): ValidatedBuilder[Cartridge] = {
      getType(json) match {
        case "Title" => TitleCartridge.fromJson(json)
        case _       => ValidatedBuilder(EmptyCartridge, false)
      }
    }
  }

  object BellTemplate {

    def apply(viewAdvice: ViewAdvice): ValidatedBuilder[BellTemplate] = {
      for {
        headZone <- viewAdvice.zone("head").map(Cartridge.fromJson).validate
        leftNavZone <- viewAdvice.zone("leftNav").map(Cartridge.fromJson).validate
        bodyZone <- viewAdvice.zone("body").map(Cartridge.fromJson).validate
        footerZone <- viewAdvice.zone("footer").map(Cartridge.fromJson).validate
      } yield new BellTemplate(headZone, leftNavZone, bodyZone, footerZone)
    }

  }

  object Template {
    def getName(json: JsValue): String = ???

    def apply( viewAdvice: ViewAdvice ): ValidatedBuilder[Template] = {
      val templateName = viewAdvice.templateName
      templateName match {
        case "Bell" => BellTemplate(viewAdvice)
        case _      => throw UnknownTemplateException(templateName)
      }
    }
  }

  object TitleCartridge {
    def fromJson( json: JsValue ): ValidatedBuilder[TitleCartridge] = {
      val getTitle: Option[String] = ???
      ValidatedBuilder(TitleCartridge(getTitle.getOrElse("")),
                       isValid = getTitle.isDefined)
    }
  }

  case class ServiceCartridge( data: String ) extends Cartridge {
    def render: Html = ???
  }

  object ServiceCartridge {
    def fromJson( json: JsValue ) = ValidatedBuilder {
      def service(key: String): Future[String] = ???
      val key: String = ???
      for {
        response <- service(key)
      } yield Validated(ServiceCartridge(response), isValid = true)
    }
  }

  implicit class ValidatedBuilderTemplate(vt: ValidatedBuilder[Template]) extends Renderable[Result] {
    def render: Future[Result] =
      vt.map(_.render)
        .asFuture
        .map {
        case Validated(result, isCacheable@true ) =>
          result
        case Validated(result, isCacheable@false) =>
          result.copy(headers = result.headers + ("Cache-control" -> "no-cache"))
  }
}

  def search(term: String, refinements: Seq[String]): Future[Result] = {
    val query = ???

    val viewAdvice = EndecaService.getAdvice(query)
    val template: ValidatedBuilder[Template] = Template(viewAdvice)

    template.render
  }

}

object ValiadatedBuilderWithContext {

  import Basics._
  import scala.concurrent.Future
  import ValidatedImplementation._
  import ValidatedBuilderImplementation._

  case class ContextValidatedBuilder[+X, -Context](block: Context => Future[Validated[X]]) {

    def map[Y](f: X => Y): ContextValidatedBuilder[Y, Context] = {
      new ContextValidatedBuilder[Y, Context](context => block(context).map(_.map(f)))
    }

    // flatMap for ValidatedBuilder (i.e. Future[Validated[_]]), less the unapply-apply
    private def vbFM[Y](fv: Future[Validated[X]], f: X => Future[Validated[Y]]): Future[Validated[Y]] = {
      fv.flatMap {
        case Validated(va, aIsValid) =>
          val fva = f(va)
          fva map {
            case Validated(vb, bIsValid) =>
              Validated(vb, aIsValid && bIsValid)
          }
      }
    }

    def flatMap[Y](f: X => ContextValidatedBuilder[Y, Context]): ContextValidatedBuilder[Y, Context] = {
      new ContextValidatedBuilder[Y, Context]({
        context =>
          vbFM(block(context), x => f(x).block(context))
      })
    }
  }

  object ContextValidatedBuilder {
    def apply[A, Context](a: A, isValid: Boolean): ContextValidatedBuilder[A, Context] =
      new ContextValidatedBuilder((_: Context) => Future.successful(Validated(a, isValid)))

    implicit class CVBSeq[X, Context](val sv: Seq[ContextValidatedBuilder[X, Context]]) extends AnyVal {
      def validate: ContextValidatedBuilder[Seq[X], Context] = new ContextValidatedBuilder[Seq[X], Context](context => {
        Future.sequence(sv.map(_.block(context))).map(_.validate)
      })
    }
  }

  case class ServiceCartridge(data: String) extends Cartridge {
    def render: Html = ???
  }

  sealed trait Language

  case object En extends Language

  case object Fr extends Language

  case class BasicContext(language: Language)

  type Builder[+X] = ContextValidatedBuilder[X, BasicContext]

  object CartridgeBuilder {
    // The magnet pattern could be used here to allow overloading a single method (e.g. apply).
    def apply[C <: Cartridge](block: BasicContext => Validated[C]): Builder[C] =
      ContextValidatedBuilder(context => Future.successful(block(context)))
    def async[C <: Cartridge](block: BasicContext => Future[Validated[C]]): Builder[C] =
      ContextValidatedBuilder(block)
  }


  object TitleCartridge {
    def fromJson(json: JsValue): Builder[TitleCartridge] = CartridgeBuilder {
      context =>
        val getTitle: Option[String] = ???
        Validated(new TitleCartridge(getTitle.getOrElse("")),
          isValid = getTitle.isDefined)
    }
  }

  object Cartridge {
    def getType(json: JsValue): String = ???

    def fromJson(json: JsValue): ContextValidatedBuilder[Cartridge, BasicContext] = {
      getType(json) match {
        case "Title" => TitleCartridge.fromJson(json)
        case _ => ContextValidatedBuilder(EmptyCartridge, isValid = false)
      }
    }
  }

  object ServiceCartridge {
    def fromJson(json: JsValue) = CartridgeBuilder.async {
      context =>
        def service(key: String, lang: Language): Future[String] = ???
        val key: String = ???
        for {
          response <- service(key, context.language)
        } yield Validated(ServiceCartridge(response), isValid = true)
    }
  }

  object BellTemplate {

    def apply(viewAdvice: ViewAdvice): Builder[BellTemplate] = {
      for {
        headZone <- viewAdvice.zone("head").map(Cartridge.fromJson).validate
        leftNavZone <- viewAdvice.zone("leftNav").map(Cartridge.fromJson).validate
        bodyZone <- viewAdvice.zone("body").map(Cartridge.fromJson).validate
        footerZone <- viewAdvice.zone("footer").map(Cartridge.fromJson).validate
      } yield new BellTemplate(headZone, leftNavZone, bodyZone, footerZone)
    }

  }

  object Template {
    def getName(json: JsValue): String = ???

    def apply(viewAdvice: ViewAdvice): Builder[Template] = {
      val templateName = viewAdvice.templateName
      templateName match {
        case "Bell" => BellTemplate(viewAdvice)
        case _ => throw UnknownTemplateException(templateName)
      }
    }
  }

}

object BasicMonads {
  case class Validated[+X](value: X, isValid: Boolean)

  class Future

  class Option

  case class WithContext[-C, +X](v: C => X) {
    def map[Y](f: X => Y) = WithContext(c => f(v(c)))

    def flatMap[Y](f: X => WithContext[C, Y]) = {
      WithContext(c => f(v(c)).v(c))
    }
  }

  class JsError
  case class WithJsError[+X](value: X, errors: JsError)

  case class WithJsModules[+X](value: X, modules: Set[String]) {
    def map[Y](f: X => Y) = WithJsModules(f(value), modules)

    def flatMap[Y](f: X => WithJsModules[Y]) = {
      f(value) match {
        case WithJsModules(fv, fModules) => WithJsModules(fv, modules union fModules)
      }
    }
  }
}
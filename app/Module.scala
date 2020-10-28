import com.google.inject.AbstractModule
import javax.inject._
import net.codingwell.scalaguice.ScalaModule
import play.api.{Configuration, Environment}
import repo.{DataRepository, EmptyListRepository}

class Module(environment: Environment, configuration: Configuration)
  extends AbstractModule
    with ScalaModule {

  override def configure(): Unit = {
    bind(classOf[DataRepository]).to(classOf[EmptyListRepository]).in(classOf[Singleton])
  }
}

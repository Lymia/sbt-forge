package moe.lymia.forge.launcher

import java.net.Proxy
import java.util.{List => JavaList, Map => JavaMap, UUID}

import com.google.gson.GsonBuilder
import com.mojang.authlib.exceptions.AuthenticationException
import com.mojang.authlib.properties.PropertyMap
import com.mojang.authlib.yggdrasil.YggdrasilAuthenticationService
import com.mojang.authlib.{Agent, UserAuthentication}
import org.apache.commons.codec.binary.Base64
import sbt._
import play.api.libs.json._

import scala.collection.JavaConverters._

private final case class AccessToken(username: String, clientToken: String, data: Map[String, JsValue] = Map()) {
  def update(auth: UserAuthentication) =
    copy(data = auth.saveForStorage().asScala.mapValues {
      case x: String => Json.toJson(x)
      case x: UUID => Json.toJson(x.toString)
      case x: JavaList[_] =>
        println(x)
        Json.toJson(x.asScala.map(_.asInstanceOf[JavaMap[String, String]].asScala))
      case x => sys.error(s"Unknown value type in token save data: $x : ${x.getClass.getCanonicalName}")
    }.toMap)
  def toPropertiesMap =
    data.mapValues {
      case x: JsString => x.as[String]
      case x: JsArray => x.as[Seq[Map[String, String]]].map(_.asJava).asJava
      case _ => sys.error("Invalid JSON in token file.")
    }.asJava
}
private final case class AccessTokenWrapper(data: Option[AccessToken])

private object AuthManager {
  private implicit val accessTokenSerializer = Json.format[AccessToken]
  private implicit val accessTokenWrapperSerializer = Json.format[AccessTokenWrapper]

  private val rng = new java.security.SecureRandom()
  private def generateClientToken() =
    Base64.encodeBase64URLSafeString(rng.generateSeed(32))
}
private final class AuthManager(cacheDir: File, log: Logger) {
  import AuthManager._

  private lazy val authDataFile = cacheDir / "client_token.json"

  private def writeAuthInfo(info: Option[AccessToken]) =
    IO.write(authDataFile, Json.toJson(AccessTokenWrapper(info)).toString())
  private def readAuthInfo() =
    if(authDataFile.exists) try {
      Json.parse(IO.read(authDataFile)).as[AccessTokenWrapper].data
    } catch {
      case _: Throwable => None
    } else None

  private def doAuth[T](clientToken: String)(callback: UserAuthentication => Unit) =
    try {
      val yggdrasil = new YggdrasilAuthenticationService(Proxy.NO_PROXY, clientToken)
      val auth = yggdrasil.createUserAuthentication(Agent.MINECRAFT)
      callback(auth)
      auth.logIn()
      Some(auth)
    } catch {
      case e: AuthenticationException =>
        log.error("Authentication failed: "+e.getMessage)
        None
    }
  private def authWithToken(token: AccessToken) =
    doAuth(token.clientToken) { auth =>
      auth.loadFromStorage(token.toPropertiesMap)
    }
  private def authWithPassword(username: String, password: String) = {
    val clientToken = generateClientToken()
    doAuth(clientToken){ auth =>
      auth.setUsername(username)
      auth.setPassword(password)
    }.map(auth => (clientToken, auth))
  }

  lazy val gsonPropertySerializer =
    new GsonBuilder().registerTypeAdapter(classOf[PropertyMap], new PropertyMap.Serializer()).create()
  private def authToConfig(auth: UserAuthentication) = {
    assert(auth.isLoggedIn, "Account is not logged in??")
    val profile = auth.getSelectedProfile
    if (profile == null) sys.error("No profile selected! Do you own Minecraft on this account?")
    Seq(
      "--uuid"          , profile.getId.toString.replace("-", ""),
      "--username"      , profile.getName,
      "--accessToken"   , auth.getAuthenticatedToken,
      "--userType"      , auth.getUserType.getName,
      "--userProperties", gsonPropertySerializer.toJson(auth.getUserProperties)
    )
  }
  private lazy val ForgeDevAuthInfo = Seq(
    "--username", "ForgeDevName",
    "--accessToken", "FML",
    "--userProperties", "{}"
  )

  private def updateAuthInfo(auth: UserAuthentication, accessToken: AccessToken) = {
    log.info(s"Updating stored access token for user ${accessToken.username}")
    try {
      writeAuthInfo(Some(accessToken.update(auth)))
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }
  }
  def login(): Unit =
    while(true) {
      System.out.println(s"\nNote: An access token will be stored in plaintext at $authDataFile. "+
                          "Your password will not be stored at all.\n")
      val username = System.console.readLine(s"Username/Email: ")
      if (username == null) sys.error("Password authentication canceled")
      val password = System.console.readPassword(s"Password: ")
      if (password == null) sys.error("Password authentication canceled")

      authWithPassword(username, String.valueOf(password)) match {
        case Some((clientToken, auth)) =>
          log.info("Login successful!")
          if (auth.canPlayOnline) {
            updateAuthInfo(auth, AccessToken(username, clientToken))
            return
          } else {
            log.info("This account does not seem to own Minecraft!")
          }
        case None => println("Incorrect password!")
      }
    }
  def logout() = {
    log.info("Deleting stored access tokens...")
    writeAuthInfo(None)
  }

  def loginWithToken() =
    readAuthInfo().map { accessToken =>
      log.info("Attempting to log in with authentication token...")
      authWithToken(accessToken) match {
        case Some(auth) =>
          log.info("Login successful!")
          updateAuthInfo(auth, accessToken)
          authToConfig(auth)
        case None => ForgeDevAuthInfo
      }
    } getOrElse ForgeDevAuthInfo
}

package moe.lymia.sbt.forge

import sbt._
import play.api.libs.json._

import java.io._
import java.net.Proxy

import com.google.gson._
import com.mojang.authlib._
import com.mojang.authlib.exceptions._
import com.mojang.authlib.properties._
import com.mojang.authlib.yggdrasil._

import scala.collection.JavaConversions._

// Copying client auth tokens from Minecraft's configuration is not supported because
// of how the clientId is used. Using it would invalidate the login for the Minecraft
// launcher.
object AuthManager {
  case class AccessToken(token: String, clientToken: String)
  def writeTokenToCache(authCache: File, username: String, token: AccessToken) =
    IO.write(authCache / username, token.token+"\n"+token.clientToken)
  def readTokenFromCache(authCache: File, username: String) =
    if((authCache / username).exists) {
      val token = IO.read(authCache / username).split("\n")
      Some(AccessToken(token(0), token(1)))
    } else None

  private def doAuth(auth: UserAuthentication, log: Logger) = 
    try {
      auth.logIn()
      Some(auth)
    } catch {
      case e: AuthenticationException =>
        log.error("Authentication failed: "+e.getMessage)
        None
    }
  def authWithToken(username: String, token: AccessToken, log: Logger) = {
    log.info("Authenticating as "+username+" with access token...")
    val auth = new YggdrasilAuthenticationService(Proxy.NO_PROXY, token.clientToken).createUserAuthentication(Agent.MINECRAFT)
    auth.setUsername(username)
    auth.loadFromStorage(Map("accessToken" -> token.token))
    doAuth(auth, log)
  }
  val rng = new java.security.SecureRandom()
  private def generateClientToken() = new String((0 until 32).map{_ => 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-".charAt(rng.nextInt(64)).toByte}.toArray)
  def authWithPassword(username: String, password: String, log: Logger) = {
    log.info("Authenticating as "+username+" with password...")
    val clientToken = generateClientToken()
    val auth = new YggdrasilAuthenticationService(Proxy.NO_PROXY, clientToken).createUserAuthentication(Agent.MINECRAFT)
    auth.setUsername(username)
    auth.setPassword(password)
    doAuth(auth, log).map(auth => (clientToken, auth))
  }

  lazy val gsonBuilder = new GsonBuilder().registerTypeAdapter(classOf[PropertyMap], new PropertyMap.Serializer()).create()
  def authToConfig(auth: UserAuthentication) = Seq(
    "--uuid"          , auth.getSelectedProfile.getId.toString.replace("-", ""),
    "--username"      , auth.getSelectedProfile.getName,
    "--accessToken"   , auth.getAuthenticatedToken,
    "--userType"      , auth.getUserType.getName,
    "--userProperties", gsonBuilder.toJson(auth.getUserProperties)
  )

  def checkForAuthUpdate(authCache: File, name: String, auth: UserAuthentication, accessToken: AccessToken, log: Logger) =
    if(accessToken.token != auth.getAuthenticatedToken) {
      log.info("Updating stored access token for user "+name)
      writeTokenToCache(authCache, name, AccessToken(auth.getAuthenticatedToken, accessToken.clientToken))
    }
  def tryPasswordAuth(name: String, log: Logger): (AccessToken, UserAuthentication) = {
    for(i <- 0 until 5) {
      val password = System.console.readPassword("Password for "+name+": ")
      if(password == null) sys.error("Password authentication canceled")
      authWithPassword(name, String.valueOf(password), log) match {
        case Some((clientToken, auth)) =>
          return (AccessToken(auth.getAuthenticatedToken, clientToken), auth)
        case None => println("Incorrect password!")
      }
    }
    sys.error("Too many password attempts.")
  }
  def authenticate(authCache: File, name: String, log: Logger) =
    readTokenFromCache(authCache, name).flatMap { accessToken => 
      log.info("Attempting to log in with authentication token from cache.")
      authWithToken(name, accessToken, log).map { auth =>
        log.info("Login successful!")
        checkForAuthUpdate(authCache, name, auth, accessToken, log)
        authToConfig(auth)
      }
    } getOrElse {
      val (accessToken, auth) = tryPasswordAuth(name, log)
      log.info("Login successful!")
      writeTokenToCache(authCache, name, accessToken)
      authToConfig(auth)
    }
}

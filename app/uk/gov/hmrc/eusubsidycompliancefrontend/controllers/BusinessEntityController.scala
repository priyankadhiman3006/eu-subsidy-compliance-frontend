/*
 * Copyright 2022 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.eusubsidycompliancefrontend.controllers

import javax.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.eusubsidycompliancefrontend.actions.EscActionBuilders
import uk.gov.hmrc.eusubsidycompliancefrontend.config.AppConfig
import uk.gov.hmrc.eusubsidycompliancefrontend.connectors.EscConnector
import uk.gov.hmrc.eusubsidycompliancefrontend.models.types.{EORI, UndertakingName, UndertakingRef}
import uk.gov.hmrc.eusubsidycompliancefrontend.models.{BusinessEntity, ContactDetails, Undertaking}
import uk.gov.hmrc.eusubsidycompliancefrontend.services.{BusinessEntityJourney, Store}
import uk.gov.hmrc.eusubsidycompliancefrontend.views.html._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class BusinessEntityController @Inject()(
  mcc: MessagesControllerComponents,
  escActionBuilders: EscActionBuilders,
  store: Store,
  connector: EscConnector,
  addBusinessPage: AddBusinessPage,
  eoriPage: BusinessEntityEoriPage,
  businessEntityContactPage: BusinessEntityContactPage,
  businessEntityCyaPage: BusinessEntityCYAPage,
  removeBusinessPage: RemoveBusinessPage
)(
  implicit val appConfig: AppConfig,
  executionContext: ExecutionContext
) extends
  BaseController(mcc) {

  import escActionBuilders._

  def getAddBusinessEntity: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber

    for {
      b <- store.get[BusinessEntityJourney]
      c <- connector.retrieveUndertaking(eori)
      _ <- store.put[Undertaking](c.getOrElse(throw new IllegalStateException("missing undertaking on hod")))
    } yield (b, c) match {
      case (Some(journey), Some(u)) =>
        journey
          .addBusiness
          .value
          .fold(
              Ok(addBusinessPage(
                addBusinessForm,
                u.name,
                u.undertakingBusinessEntity
              ))
          ){x =>
              Ok(addBusinessPage(
                addBusinessForm.fill(FormValues(x.toString)),
                u.name,
                u.undertakingBusinessEntity
              ))
          }
    }
  }

  def postAddBusinessEntity: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[Undertaking].flatMap { undertaking =>
      val name: UndertakingName = undertaking.map(_.name).getOrElse(throw new IllegalStateException("missing undertaking name"))
      addBusinessForm.bindFromRequest().fold(
        errors => Future.successful(BadRequest(addBusinessPage(errors, name, List.empty))),
        form => {
          store.update[BusinessEntityJourney]({ x =>
            x.map { y =>
              y.copy(addBusiness  = y.addBusiness.copy(value = Some(form.value.toBoolean)))
            }
          }).flatMap(_.next)
        }
      )

    }
  }

  def getEori: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    getPrevious[BusinessEntityJourney](store).flatMap { previous =>
      store.get[BusinessEntityJourney].flatMap {
        case Some(journey) =>
          journey
            .eori
            .value
            .fold(
              Future.successful(
                Ok(eoriPage(
                  eoriForm,
                  previous
                ))
              )
            ) { x =>
              Future.successful(
                Ok(eoriPage(
                  eoriForm.fill(FormValues(x.toString)),
                  previous
                ))
              )
            }
      }
    }
  }

  def postEori: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    getPrevious[BusinessEntityJourney](store).flatMap { previous =>
      eoriForm.bindFromRequest().fold(
        errors => Future.successful(BadRequest(eoriPage(errors, previous))),
        form => {

          for {
            retrievedUndertaking <- connector.retrieveUndertaking(EORI(form.value))
          } yield {
            retrievedUndertaking match {
              case Some(_) => {
                Future(BadRequest(eoriPage(eoriForm.withError("businessEntityEori", "businessEntityEori.eoriInUse").fill(form), previous)))
              }
              case _ =>
                store.update[BusinessEntityJourney]({ x =>
                  x.map { y =>
                    y.copy(eori = y.eori.copy(value = Some(EORI(form.value))))
                  }
                }).flatMap(_.next)
            }
          }
        }.flatten
      )

    }
  }

  def getContact: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[BusinessEntityJourney].flatMap {
      case Some(journey) =>
        journey
          .contact
          .value
          .fold(
            Future.successful(
              Ok(businessEntityContactPage(
                contactForm,
                journey.previous
              ))
            )
          ){x =>
            Future.successful(
              Ok(businessEntityContactPage(
                contactForm.fill(OneOf(x.phone.map(_.toString), x.mobile.map(_.toString))),
                journey.previous
              ))
            )
          }
    }
  }

  def postContact: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    getPrevious[BusinessEntityJourney](store).flatMap { previous =>
      contactForm.bindFromRequest().fold(
        errors => Future.successful(BadRequest(businessEntityContactPage(errors, previous))),
        form => {
          store.update[BusinessEntityJourney]({ x =>
            x.map { y =>
              y.copy(contact = y.contact.copy(value = Some(form.toContactDetails)))
            }
          }).flatMap(_.next)
        }
      )
    }
  }

  def getCheckYourAnswers: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    store.get[BusinessEntityJourney].flatMap {
      case Some(journey) =>
        Future.successful(
          Ok(
            businessEntityCyaPage(
              eori,
              journey.contact.value.getOrElse(throw new IllegalStateException("missing contact details"))
            )
          )
        )
    }
  }

  def postCheckYourAnswers: Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber

    cyaForm.bindFromRequest().fold(
      errors =>  throw new IllegalStateException("value hard-coded, form hacking?"),
      form => {

//        // TODO try to get an undertaking for the eori of the added business, and only proceed if there isn't one
//        // TODO UX are figuring out the correct behaviour here so will come back to this
//        for {
//          retrievedUndertaking <- connector.retrieveUndertaking(EORI("GB123456789016"))
//          b <- store.get[BusinessEntityJourney]
//          journey = b.fold(throw new IllegalStateException("journey should be defined")) {
//            identity
//          }
//          cd = Some(
//            ContactDetails(
//              journey.contact.value.getOrElse(throw new IllegalStateException("contact should be defined")).phone,
//              journey.contact.value.getOrElse(throw new IllegalStateException("contact should be defined")).mobile
//            )
//          )
//        } yield {
//          retrievedUndertaking match {
//            case Some(_) => {
//              ??? //Future(BadRequest(businessEntityCyaPage(cyaForm.withError("businessEntityEori", "businessEntityEori.eoriInUse").fill(form))))
//            }
//          }
//        }


        for {
          a <- store.get[Undertaking]
          b <- store.get[BusinessEntityJourney]
          _ <- store.put(BusinessEntityJourney())
          ref = a.fold(throw new IllegalStateException("undertaking should be defined")) {
            _.reference
          }
          journey = b.fold(throw new IllegalStateException("journey should be defined")) {
            identity
          }
          _ <- connector.addMember(
            UndertakingRef(
              ref.getOrElse(throw new IllegalStateException("undertakingRef should be defined"))
            ),
            BusinessEntity(
              EORI(journey.eori.value.getOrElse(throw new IllegalStateException("eori should be defined"))),
              leadEORI = false,
              Some(
                ContactDetails(
                  journey.contact.value.getOrElse(throw new IllegalStateException("contact should be defined")).phone,
                  journey.contact.value.getOrElse(throw new IllegalStateException("contact should be defined")).mobile
                )
              )
            )
          )
        } yield Redirect(routes.BusinessEntityController.getAddBusinessEntity())
      })
  }

 def editBusinessEntity(eoriEntered: String): Action[AnyContent] = escAuthentication.async { implicit request =>
   implicit val eori111: EORI = request.eoriNumber

   for {
     a <- connector.retrieveUndertaking(eori111)
     b <- store.put(BusinessEntityJourney.businessEntityJourneyForEori(a, EORI(eoriEntered)))
   } yield b match {
     case journey =>
       Ok(
         businessEntityCyaPage(
           eoriEntered,
           journey.contact.value.getOrElse(throw new IllegalStateException("missing contact details"))
         )
       )
   }
 }

  def getRemoveBusinessEntity(eoriEntered: String): Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber

    for {
      a <- connector.retrieveUndertaking(EORI(eoriEntered))
    } yield a match {
      case Some(undertaking) => {
        val bs = undertaking.undertakingBusinessEntity
          .filter(a => a.businessEntityIdentifier == eoriEntered)
          .head
        Ok(removeBusinessPage(removeBusinessForm, bs))
      }
    }
  }

  def postRemoveBusinessEntity(eoriEntered: String): Action[AnyContent] = escAuthentication.async { implicit request =>
    implicit val eori: EORI = request.eoriNumber
    connector.retrieveUndertaking(EORI(eoriEntered)).flatMap {
      case Some(undertaking) => {
        val bs = undertaking.undertakingBusinessEntity
          .filter(a => a.businessEntityIdentifier == eoriEntered)
          .head
        removeBusinessForm.bindFromRequest().fold(
          errors => Future.successful(BadRequest(removeBusinessPage(errors, bs))),
          form => {
            form.value match {
              case "true" =>
                connector.removeMember(
                  UndertakingRef(undertaking.reference.getOrElse(throw new IllegalStateException("no undertakingRef"))),
                  bs)
                Future.successful(Redirect(routes.BusinessEntityController.getAddBusinessEntity()))
              case _ => Future(Redirect(routes.BusinessEntityController.getAddBusinessEntity()))
            }
          }
        )
    }
  }}

  lazy val addBusinessForm: Form[FormValues] = Form(
    mapping("addBusiness" -> mandatory("addBusiness"))(FormValues.apply)(FormValues.unapply))

  lazy val removeBusinessForm: Form[FormValues] = Form(
    mapping("removeBusiness" -> mandatory("removeBusiness"))(FormValues.apply)(FormValues.unapply))

  lazy val eoriForm: Form[FormValues] = Form(
    mapping("businessEntityEori" -> mandatory("businessEntityEori"))(FormValues.apply)(FormValues.unapply).verifying(
    "regex.error",
    fields => fields match {
      case a if a.value.matches(EORI.regex) => true
      case _ => false
    }
  ))

  lazy val cyaForm: Form[FormValues] = Form(
    mapping("cya" -> mandatory("cya"))(FormValues.apply)(FormValues.unapply))

}
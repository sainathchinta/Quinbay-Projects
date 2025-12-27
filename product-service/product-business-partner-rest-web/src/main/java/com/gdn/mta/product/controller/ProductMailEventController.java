package com.gdn.mta.product.controller;

import java.util.Calendar;
import java.util.Date;

import org.apache.http.protocol.HTTP;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.mta.product.service.ProductMailEventService;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = ProductMailEventController.BASE_PATH)
@Tag(name = "Product Mail Event Api")
public class ProductMailEventController {

  public static final String BASE_PATH = "/api/mail-event";
  public static final String SEND_MAIL = "send-mail";
  public static final String SEND_POST_LIVE_REVIEW_ACTIVATED_MAIL = "send-post-live-review-activated-mail";
  public static final String SEND_POST_LIVE_REVIEW_REJECTED_MAIL = "send-post-live-review-rejected-mail";
  public static final String SEND_MAIL_SUSPENSION = "send-mail-suspension";
  public static final String DELETE_OLD_RECORDS_BY_DAYS = "delete-old-records/{days}";
  public static final String POST_LIVE_REVIEW_ACTIVATED_PRODUCTS_MAIL_ERROR = "Error while sending post live review activated products mail ";
  public static final String POST_LIVE_REVIEW_REJECTION_PRODUCTS_MAIL_ERROR = "Error while sending post live review rejected products mail ";
  public static final String MAIL_SCHEDULERS_ERROR = "Error while running mail schedulers ";

  @Autowired
  private ProductMailEventService productMailEventService;

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductMailEventService.class);

  @Operation(summary = "Run Scheduled Mailer")
  @RequestMapping(value = SEND_MAIL, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSimpleResponse<String> sendMail(@RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam("storeId") String storeId, @RequestParam String username) {
    try {
      Calendar calendar = Calendar.getInstance();
      calendar.add(Calendar.DATE, -1);
      Date date = calendar.getTime();
      LOGGER.info("Sending product mail to business partner ,storeID = {}, date = {}",storeId,
          date.toString());
      productMailEventService.sendProductMailEventsToBusinessPartners(date);
    } catch (Exception ex) {
      LOGGER.error("error while sending mail ", ex);
      return new GdnRestSimpleResponse<>("Error while running mail schedulers " + ex.getMessage(), null, false, null, null);
    }
    return new GdnRestSimpleResponse<>(null, null, true, null, "ok");
  }

  @Operation(summary = "Run Scheduled Post live review products Mailer")
  @RequestMapping(value = SEND_POST_LIVE_REVIEW_ACTIVATED_MAIL, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSimpleResponse<String> sendPostLiveReviewActivatedMail(
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam("storeId") String storeId, @RequestParam String username) {
    try {
      Calendar calendar = Calendar.getInstance();
      calendar.add(Calendar.DATE, -1);
      Date date = calendar.getTime();
      LOGGER.info("Sending post live review products activation mail to business partner, storeId = {}, date = {}",storeId, date);
      productMailEventService.sendPostLiveReviewActiveProductMailEventsToBusinessPartners(date);
    } catch (Exception ex) {
      LOGGER.error(POST_LIVE_REVIEW_ACTIVATED_PRODUCTS_MAIL_ERROR, ex);
      return new GdnRestSimpleResponse<>(MAIL_SCHEDULERS_ERROR + ex.getMessage(), null, false, null, null);
    }
    return new GdnRestSimpleResponse<>(null, null, true, null, "ok");
  }

  @Operation(summary = "Run Scheduled Post live review rejection products Mailer")
  @RequestMapping(value = SEND_POST_LIVE_REVIEW_REJECTED_MAIL, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSimpleResponse<String> sendPostLiveReviewRejectedMail(
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam("storeId") String storeId, @RequestParam String username) {
    try {
      Calendar calendar = Calendar.getInstance();
      calendar.add(Calendar.DATE, -1);
      Date date = calendar.getTime();
      LOGGER.info("Sending post live review products rejection mail to business partner, storeId = {}, date = {}",storeId, date);
      productMailEventService.sendPostLiveReviewRejectProductMailEventsToBusinessPartners(date);
    } catch (Exception ex) {
      LOGGER.error(POST_LIVE_REVIEW_REJECTION_PRODUCTS_MAIL_ERROR, ex);
      return new GdnRestSimpleResponse<>(MAIL_SCHEDULERS_ERROR + ex.getMessage(), null, false, null, null);
    }
    return new GdnRestSimpleResponse<>(null, null, true, null, "ok");
  }

  @Operation(summary = "Run Scheduled Mailer for Suspension or Activation")
  @RequestMapping(value = SEND_MAIL_SUSPENSION, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSimpleResponse<String> sendMailForSuspensionOrActivation(@RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam("storeId") String storeId,
      @RequestParam String username) {
    try {
      Calendar calendar = Calendar.getInstance();
      calendar.add(Calendar.DATE, -1);
      Date date = calendar.getTime();
      LOGGER.info("Sending suspension or activation mails to business partners from date = {} ", date.toString());
      productMailEventService.sendProductMailEventsToBusinessPartnersForSuspension(date);
    } catch (Exception ex) {
      LOGGER.error("error while sending suspension or activation mails ", ex);
      return new GdnRestSimpleResponse<>(
          "Error while running mail schedulers for suspension or activation mails " + ex.getMessage(), null, false,
          null, null);
    }
    return new GdnRestSimpleResponse<>(null, null, true, null, "ok");
  }

  @Operation(summary = "Run Scheduled Deletion for Old Product mail Events")
  @DeleteMapping(value = DELETE_OLD_RECORDS_BY_DAYS, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSimpleResponse<String> deleteOldRecordsByDays(@RequestParam String username, @PathVariable int days) {
    try {
      LOGGER.info(
        "Executing Deletion of old product mail event records by user: {}  for days  = {} ", username, days);
      productMailEventService.deleteOldRecordsByDays(days);
    } catch (Exception e) {
      LOGGER.error("Error while Deletion of old product mail event : ", e);
      return new GdnRestSimpleResponse<>(
        "Error while running mail schedulers for Deletion of old product mail event " + e.getMessage(), null, false,
        null, null);
    }
    return new GdnRestSimpleResponse<>(null, null, true, null, HttpStatus.OK.name());
  }
}

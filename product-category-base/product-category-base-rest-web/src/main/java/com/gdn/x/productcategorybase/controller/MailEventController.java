package com.gdn.x.productcategorybase.controller;

import java.util.Calendar;
import java.util.Date;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.notification.dto.GdnRestSimpleResponse;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.service.MailService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(value = MailEventController.BASE_PATH)
@Tag(name = "Mail Event Api")
public class MailEventController {

  public static final String BASE_PATH = "/api/mails";
  public static final String SEND_CATEGORY_CONFIG_MAIL = "/review-config-mailers-category";
  public static final String SEND_MERCHANT_CONFIG_MAIL = "/review-config-mailers-merchant";

  @Autowired
  private MailService mailService;

  @Operation(summary = "Run Config Scheduled Mailer for category configurations")
  @RequestMapping(value = SEND_CATEGORY_CONFIG_MAIL, method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse sendConfigurationMailForCategory(@RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam("storeId") String storeId, @RequestParam String username) {
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
      MDC.put(GdnMandatoryParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
      Calendar calendar = Calendar.getInstance();
      calendar.add(Calendar.DATE, -1);
      Date date = calendar.getTime();
      log.info("Sending category configuration mail to internal user, storeId = {}, date = {}", storeId, date);
      mailService.sendConfigurationChangesMailForCategory(date);
    } catch (Exception e) {
      log.error("error while sending mail ", e);
      return new GdnRestSimpleResponse<>(ErrorMessage.SCHEDULAR_EVENT_ERROR.getMessage() + e.getMessage(), null, false,
          null, null);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @Operation(summary = "Run Config Scheduled Mailer for merchant configurations")
  @RequestMapping(value = SEND_MERCHANT_CONFIG_MAIL, method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse sendConfigurationMailForMerchant(@RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam("storeId") String storeId, @RequestParam String username) {
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
      MDC.put(GdnMandatoryParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
      Calendar calendar = Calendar.getInstance();
      calendar.add(Calendar.DATE, -1);
      Date date = calendar.getTime();
      log.info("Sending merchant configuration mail to internal user, storeId = {}, date = {}", storeId, date);
      mailService.sendConfigurationChangesMailForMerchant(date);
    } catch (Exception e) {
      log.error("error while sending mail ", e);
      return new GdnRestSimpleResponse<>(ErrorMessage.SCHEDULAR_EVENT_ERROR.getMessage() + e.getMessage(), null, false,
          null, null);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}

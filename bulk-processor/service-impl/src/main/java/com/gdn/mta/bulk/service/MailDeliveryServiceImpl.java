package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.bulk.dto.BulkDownloadMailRecipient;
import com.gdn.mta.bulk.dto.BulkPendingProductResponse;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static com.gdn.mta.bulk.models.EmailConstants.MAIL_SENDER_PARAM;
import static com.gdn.mta.bulk.models.EmailConstants.MAIL_SUBJECT_PARAM;
import static com.gdn.mta.bulk.models.EmailConstants.TEMPLATE_ID_PARAM;

/**
 * Created by keshashah on 13/11/16.
 */
@Service
@Slf4j
public class MailDeliveryServiceImpl implements MailDeliveryService {

  @Autowired
  private KafkaPublisher kafkaProducer;

  private static final Logger LOGGER = LoggerFactory.getLogger(MailDeliveryServiceImpl.class);
  private static final String TEMPLATE_ID_NOT_SPECIFIED = "template id not specified";
  private static final String SEMICOLON_FOR_CC_LIST = ";";
  public static final String STORE_COPY = "STORE_COPY";
  public static final String UPDATE_SALES_CATEGORY = "SALES_CATEGORY_UPDATE";
  public static final String MASTER_SKU_BULK_REVIEW = "MASTER_SKU_BULK_REVIEW";
  public static final String IPR_PORTAL_BULK_ADD_REVIEW = "IPR_PORTAL_BULK_ADD_REVIEW";
  public static final String MASTER_SKU_BULK_ASSIGNEE = "MASTER_SKU_BULK_ASSIGNEE";
  private static final String EMAIL_PATTERN = "^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@"
      + "[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$";

  @Override
  public void sendEmail(BulkDownloadRequest request, Map<String, Object> emailParams)
      throws Exception {
    String userName = request.getUsername();
    BulkDownloadMailRecipient bulkDownloadMailRecipient = new BulkDownloadMailRecipient();
    bulkDownloadMailRecipient.setEmailTo(request.getEmailTo());
    if(StringUtils.isNotEmpty(request.getEmailCc())) {
      bulkDownloadMailRecipient.setEmailCc(request.getEmailCc());
    }
    boolean isValid = validateParams(emailParams);
    if (!isValid) {
      LOGGER.error("Mandatory params missing for bulk download email {}", emailParams);
      return;
    }
    sendBulkDownloadEmail(emailParams.get(TEMPLATE_ID_PARAM).toString(),
        emailParams.get(MAIL_SENDER_PARAM).toString(),
        emailParams.get(MAIL_SUBJECT_PARAM).toString(), emailParams, "userName", userName,
        bulkDownloadMailRecipient);
    LOGGER.info(
        "postProcessing for Bulk Products Download successfully completed. bulkDownloadRequest: {}",
        request);
  }

  private boolean validateParams(Map<String, Object> emailParams) {
    return !(emailParams.get(TEMPLATE_ID_PARAM) == null
        || emailParams.get(MAIL_SENDER_PARAM) == null
        || emailParams.get(MAIL_SUBJECT_PARAM) == null);
  }

  @Override
  public void sendBulkDownloadEmail(String mailTemplateId, String sender, String subject,
      Map<String, Object> templateParams, String messageIdentifierKey,
      String messageIdentifierValue, BulkDownloadMailRecipient bulkDownloadMailRecipient)
      throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(mailTemplateId), TEMPLATE_ID_NOT_SPECIFIED);
    MessageEmailRequest email = new MessageEmailRequest();
    email.setMessageId(mailTemplateId);
    email.setMessageFrom(sender);
    email.setMessageSubject(subject);
    email.setMessageTo(bulkDownloadMailRecipient.getEmailTo());
    if (checkValidMailAddress(bulkDownloadMailRecipient.getEmailCc())) {
      email.setMessageCc(bulkDownloadMailRecipient.getEmailCc());
    } else {
      List<String> validEmailCcList = Arrays.asList(
          Optional.ofNullable(bulkDownloadMailRecipient.getEmailCc()).orElse(StringUtils.EMPTY)
              .split(SEMICOLON_FOR_CC_LIST)).stream()
          .filter(emailCcString -> checkValidMailAddress(emailCcString))
          .collect(Collectors.toList());
      if (CollectionUtils.isEmpty(validEmailCcList)) {
        LOGGER.info("Send cc mail address is not of proper format {} ", bulkDownloadMailRecipient.getEmailCc());
      } else {
        email.setMessageCc(bulkDownloadMailRecipient.getEmailCc());
      }
    }
    Map<String, Object> mailVariables = new HashMap<>();
    if (templateParams != null) {
      mailVariables.put("obj", templateParams);
    }
    log.info("mailVariables : {} ", mailVariables);
    email.setVariables(mailVariables);
    email.setMessageIdentifierKey(messageIdentifierKey);
    email.setMessageIdentifierValue(messageIdentifierValue);
    ConverterUtil.setMandatoryParamsToMessageEmailRequest(email);
    kafkaProducer.send(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT,
      email.getMessageIdentifierValue(), email);
    log.info("Email request has been sent for Bulk Download. mailTemplateId: {}, recipient: {}",
        mailTemplateId, bulkDownloadMailRecipient.getEmailTo());
  }

  @Override
  public void sendBulkDownloadErrorMail(BulkDownloadRequest request) {
    String userName = request.getUsername();
    BulkDownloadMailRecipient bulkDownloadMailRecipient = new BulkDownloadMailRecipient();
    bulkDownloadMailRecipient.setEmailTo(request.getEmailTo());
    try {
      sendBulkDownloadEmail(EmailConstants.BULK_DOWNLOAD_ERROR_TEMPLATE,
          EmailConstants.MAIL_SENDER,
          EmailConstants.BULK_DOWNLOAD_ERROR, new HashMap<String, Object>(), "userName", userName,
          bulkDownloadMailRecipient);
    } catch (Exception e) {
      LOGGER.error("Bulk Download : error occurred while sending failure mail for request {}",
          request.getRequestId(), e);
    }
  }

  private boolean checkValidMailAddress(String emailAddress) {
    if (StringUtils.isEmpty(emailAddress)) {
      return false;
    }
    Pattern pattern = Pattern.compile(EMAIL_PATTERN);
    Matcher matcher = pattern.matcher(emailAddress);
    return matcher.matches();
  }

  @Override
  public void sendPendingBulkRequestMail(String mailSendTo,
      List<BulkPendingProductResponse> bulkPendingProductResponseList,
      List<BulkPendingProductResponse> bulkInProgressProductResponseList,
      List<BulkPendingProductResponse> bulkAbortedProductResponseList, String abortedTime) throws Exception{
    LOGGER.info("The Pending Bulk Requests : {}", bulkPendingProductResponseList);
    LOGGER.info("Total No of Pending Bulk Requests : {}", bulkPendingProductResponseList.size());
    LOGGER.info("The Aborted Bulk Requests : {}", bulkAbortedProductResponseList);
    LOGGER.info("Total No of Aborted Bulk Requests : {}", bulkAbortedProductResponseList.size());
    Map<String, Object> mailObjectWrapper = new HashMap<>();
    Map<String, Object> emailObject = new HashMap<String, Object>();
    mailObjectWrapper.put(Constant.PENDING_OBJECT, bulkPendingProductResponseList);
    mailObjectWrapper.put(Constant.TOTAL_PENDING, bulkPendingProductResponseList.size());
    mailObjectWrapper.put(Constant.IN_PROGRESS_OBJECT, bulkInProgressProductResponseList);
    mailObjectWrapper.put(Constant.TOTAL_IN_PROGRESS, bulkInProgressProductResponseList.size());
    mailObjectWrapper.put(Constant.ABORTED_OBJECT, bulkAbortedProductResponseList);
    mailObjectWrapper.put(Constant.TOTAL_ABORTED, bulkAbortedProductResponseList.size());
    mailObjectWrapper.put(Constant.ABORTED_TIME, abortedTime);
    emailObject.put(Constant.EMAIL_OBJECT, mailObjectWrapper);
    MessageEmailRequest email = new MessageEmailRequest();
    email.setMessageId(EmailConstants.PENDING_REQUESTS_TEMPLATE_ID);
    email.setMessageFrom(EmailConstants.MAIL_SENDER);
    email.setMessageTo(mailSendTo);
    email.setMessageSubject(EmailConstants.PENDING_REQUESTS_SUBJECT);
    email.setMessageIdentifierKey(EmailConstants.PENDING_REQUESTS_TEMPLATE_ID);
    email.setMessageIdentifierValue(UUID.randomUUID().toString());
    email.setVariables(emailObject);
    ConverterUtil.setMandatoryParamsToMessageEmailRequest(email);
    try {
      kafkaProducer.send(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT,
        email.getMessageIdentifierValue(), email);
    } catch (Exception exp) {
      log.error("Pending Bulk Requests Email failed to send. Error - ", exp);
      throw exp;
    }
  }

  @Override
  public void sendEmailByForInternalBulkProcess(BulkInternalProcess bulkInternalProcess, String action) {
    switch (bulkInternalProcess.getProcessType()) {
      case STORE_COPY: {
        sendEmailByProcessType(bulkInternalProcess, EmailConstants.COPY_STORE_SUBJECT_MAP.get(action),
            EmailConstants.COPY_STORE_MESSAGE_MAP.get(action));
        break;
      }
      case UPDATE_SALES_CATEGORY: {
        sendEmailByProcessType(bulkInternalProcess, EmailConstants.UPDATE_SALES_CATEGORY_SUBJECT_MAP.get(action),
            EmailConstants.UPDATE_SALES_CATEGORY_MESSAGE_MAP.get(action));
        break;
      }
      case MASTER_SKU_BULK_REVIEW: {
        sendEmailByProcessType(bulkInternalProcess, EmailConstants.BULK_MASTER_SKU_REVIEW_SUBJECT_MAP.get(action),
            EmailConstants.BULK_MASTER_SKU_REVIEW_TEMPLATE_MAP.get(action));
        break;
      }
      case MASTER_SKU_BULK_ASSIGNEE: {
        sendEmailByProcessType(bulkInternalProcess, EmailConstants.BULK_MASTER_SKU_ASSIGNEE_SUBJECT_MAP.get(action),
            EmailConstants.BULK_MASTER_SKU_ASSIGNEE_TEMPLATE_MAP.get(action));
        break;
      }
      case IPR_PORTAL_BULK_ADD_REVIEW: {
        sendEmailByProcessType(bulkInternalProcess,
            EmailConstants.BULK_IPR_PRODUCT_ADD_REVIEW_SUBJECT_MAP.get(action),
            EmailConstants.BULK_IPR_PRODUCT_ADD_REVIEW_TEMPLATE_MAP.get(action));
        break;
      }
      case Constant.INTERNAL_BULK_UPLOAD:
        break;
      case Constant.RESTRICTED_KEYWORD_UPSERT:
      case Constant.RESTRICTED_KEYWORD_DELETE:
      case Constant.BULK_PRICE_UPDATE:
      case Constant.BULK_PRICE_PRODUCT_TYPE_TAGGING:
      case Constant.BULK_PRICE_REBATE:
      case Constant.BULK_SKU_LEVEL_REBATE:
      case Constant.BULK_PRICE_UPDATE_NEW:
        break;
      default:
        throw new IllegalStateException("Unexpected value: " + bulkInternalProcess.getProcessType());
    }
  }

  private void sendEmailByProcessType(BulkInternalProcess bulkInternalProcess, String subject,
      String messageId) {
    log.info("Sending mail for bulk request code : {} ", bulkInternalProcess.getInternalProcessRequestCode());
    Map<String, Object> emailParams = RequestHelper.getEmailParams(bulkInternalProcess);
    sendMail(emailParams, messageId,
        subject.replace(EmailConstants.REQUEST_CODE, bulkInternalProcess.getInternalProcessRequestCode()),
        bulkInternalProcess.getCreatedBy(), bulkInternalProcess.getInternalProcessRequestCode());
  }

  @Override
  public void sendMail(Map<String, Object> emailParams, String messageId,
      String emailSubject, String createdBy, String requestCode) {
    try {
      MessageEmailRequest email = new MessageEmailRequest();
      email.setMessageId(messageId);
      email.setMessageTo(createdBy);
      email.setMessageFrom(EmailConstants.MAIL_SENDER);
      email.setMessageIdentifierKey(messageId);
      email.setMessageIdentifierValue(UUID.randomUUID().toString());
      email.setMessageSubject(emailSubject);
      email.setVariables(emailParams);
      ConverterUtil.setMandatoryParamsToMessageEmailRequest(email);
      log.info("Sending mail for bulk request code : {} email : {} ", requestCode, email);
      kafkaProducer.send(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT, email.getMessageIdentifierValue(), email);
    } catch (Exception e) {
      log.info("exception sending mail for bulk request code : {}, error - ", requestCode, e);
    }
  }
}

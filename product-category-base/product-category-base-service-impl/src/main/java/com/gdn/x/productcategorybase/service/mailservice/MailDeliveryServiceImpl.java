package com.gdn.x.productcategorybase.service.mailservice;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import com.gdn.x.productcategorybase.domain.event.model.BrandAuthorisationWipActionEventModel;
import com.gdn.x.productcategorybase.domain.event.model.NearExpiryModelEvent;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.mail.MailRecipientRequest;

/**
 * Created by Vishal on 04/05/18.
 */

@Service
public class MailDeliveryServiceImpl implements MailDeliveryService {

  private static final Logger LOG = LoggerFactory.getLogger(MailDeliveryServiceImpl.class);
  private static final String TEMPLATE_ID_NOT_SPECIFIED = "template id not specified";
  private static final String SEMI_COLON = ";";

  private static final Pattern VALID_EMAIL_ADDRESS_REGEX = Pattern.compile(
      "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\""
          + "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09"
          + "\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9]"
          + "(?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}"
          + "(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:"
          + "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b"
          + "\\x0c\\x0e-\\x7f])+)\\])", Pattern.CASE_INSENSITIVE);
  private static final String MAIL_ADDRESS_IS_INCORRECT = "Mail address is incorrect";
  private static final String APPROVE = "APPROVE";
  private static final String REJECT = "REJECT";
  private static final String NEED_REVISION = "NEED_REVISION";
  private static final String NEAR_EXPIRY = "NEAR_EXPIRY";

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  public void sendMail(String templateId, String sender, String subject,
      Map<String, Object> templateParams, String messageIdentifierKey,
      String messageIdentifierValue, MailRecipientRequest mailRecipientRequest) {
    try {
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(templateId), TEMPLATE_ID_NOT_SPECIFIED);
      GdnPreconditions.checkArgument(validateEmails(mailRecipientRequest.getEmailTo()),
          MAIL_ADDRESS_IS_INCORRECT);
      MessageEmailRequest email = new MessageEmailRequest();
      email.setMessageId(templateId);
      email.setMessageFrom(sender);
      email.setMessageSubject(subject);
      email.setMessageTo(mailRecipientRequest.getEmailTo());
      if (StringUtils.isNotEmpty(mailRecipientRequest.getEmailCc())) {
        GdnPreconditions.checkArgument(validateEmails(mailRecipientRequest.getEmailCc()),
            MAIL_ADDRESS_IS_INCORRECT);
        email.setMessageCc(mailRecipientRequest.getEmailCc());
      }
      Map<String, Object> mailVariables = new HashMap<>();
      if (templateParams != null) {
        mailVariables.put("obj", templateParams);
      }
      email.setVariables(mailVariables);
      email.setMessageIdentifierKey(messageIdentifierKey);
      email.setMessageIdentifierValue(messageIdentifierValue);
      setMandatoryParamsToNotificationKafka(email);
      kafkaPublisher.send(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT, email.getMessageIdentifierValue(), email);
      LOG.info("Email request has been sent. mailTemplateId: {}, recipient: {}", templateId,
          mailRecipientRequest.getEmailTo());
    } catch (ApplicationRuntimeException e) {
      LOG.error("Email request has failed to sent. mailTemplateId: {}, recipient: {}", templateId,
          mailRecipientRequest.getEmailTo());
    }
  }

  private static Boolean validateEmails(String emailString) {
    String[] emailAddresses  = emailString.split(SEMI_COLON);
    Boolean isValidEmails = Boolean.TRUE;
    for(int i = 0; i < emailAddresses.length; i++){
      if(!VALID_EMAIL_ADDRESS_REGEX.matcher(emailAddresses[i]).matches()){
        isValidEmails = Boolean.FALSE;
        break;
      }
    }
    return isValidEmails;
  }

  private void setMandatoryParamsToNotificationKafka(MessageEmailRequest messageEmailRequest) {
    messageEmailRequest.setStoreId(mandatoryParameterHelper.getStoreId());
    messageEmailRequest.setRequestId(mandatoryParameterHelper.getRequestId());
    messageEmailRequest.setChannelId(mandatoryParameterHelper.getChannelId());
    messageEmailRequest.setUsername(mandatoryParameterHelper.getUsername());
    messageEmailRequest.setClientId(mandatoryParameterHelper.getClientId());
    if(StringUtils.isBlank(messageEmailRequest.getStoreId())) {
      messageEmailRequest.setStoreId(Constants.DEFAULT_STORE_ID);
    }
  }

  @Override
  @Async
  public void sendBrandAuthorisationActionMail(String businessPartnerCode,
      String brandAuthEmailType, String action, String status, String id,
      List<NearExpiryModelEvent> nearExpiryModelEvents) {
    BrandAuthorisationWipActionEventModel brandAuthorisationWipActionEventModel =
        BrandAuthorisationWipActionEventModel.builder().notificationType(brandAuthEmailType)
            .businessPartnerCode(businessPartnerCode).status(status).id(id)
            .nearExpiryModelEvents(nearExpiryModelEvents).build();
    String event = getKafkaTopicFromAction(action);
    if (StringUtils.isEmpty(event)) {
      return;
    }
    publishEmailEvent(event, businessPartnerCode, brandAuthorisationWipActionEventModel);
  }

  private String getKafkaTopicFromAction(String action) {
    return switch (action) {
      case APPROVE -> kafkaTopicProperties.getBrandAuthApproveEmailEvent();
      case REJECT -> kafkaTopicProperties.getBrandAuthRejectEmailEvent();
      case NEED_REVISION -> kafkaTopicProperties.getBrandAuthNeedRevisionEmailEvent();
      case NEAR_EXPIRY -> kafkaTopicProperties.getBrandAuthNearExpiryEmailEvent();
      default -> StringUtils.EMPTY;
    };
  }

  private void publishEmailEvent(String event, String businessPartnerCode,
      BrandAuthorisationWipActionEventModel brandAuthorisationWipActionEventModel) {
    kafkaPublisher.send(event, businessPartnerCode, brandAuthorisationWipActionEventModel);
  }
}

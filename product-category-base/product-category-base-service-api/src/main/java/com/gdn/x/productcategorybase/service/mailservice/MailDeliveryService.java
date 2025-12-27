package com.gdn.x.productcategorybase.service.mailservice;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.gdn.x.productcategorybase.domain.event.model.NearExpiryModelEvent;
import com.gdn.x.productcategorybase.dto.mail.MailRecipientRequest;

/**
 * Created by Vishal on 04/05/18.
 */
public interface MailDeliveryService {


  /**
   * send mail
   *
   * @param templateId templateID
   * @param sender sender of email
   * @param subject subject to mail body
   * @param templateParams variable needs to replace in email body
   * @param messageIdentifierKey message identifier key
   * @param messageIdentifierValue message identifier value
   * @param mailRecipientRequest mail recipients
   */
  void sendMail(String templateId, String sender, String subject,
      Map<String, Object> templateParams, String messageIdentifierKey,
      String messageIdentifierValue, MailRecipientRequest mailRecipientRequest);

  /**
   *
   * @param businessPartnerCode businessPartnerCode
   * @param brandAuthEmailType brandAuthEmailType
   * @param action action
   * @param status status
   * @param id id
   * @param nearExpiryModelEvents nearExpiryModelEvents
   */
  void sendBrandAuthorisationActionMail(String businessPartnerCode, String brandAuthEmailType,
      String action, String status, String id, List<NearExpiryModelEvent> nearExpiryModelEvents);
}

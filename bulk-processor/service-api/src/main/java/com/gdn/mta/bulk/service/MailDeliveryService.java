package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.BulkDownloadMailRecipient;
import com.gdn.mta.bulk.dto.BulkPendingProductResponse;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;

import java.util.List;
import java.util.Map;

/**
 * Created by keshashah on 13/11/16.
 */
public interface MailDeliveryService {

  /**
   * API to send email for bulk download entity
   *
   * @param request
   * @param emailParams
   * @throws Exception
   */
  void sendEmail(BulkDownloadRequest request, Map<String, Object> emailParams) throws Exception;

  /**
   * API to send bulk download email from specified template
   *
   * @param mailTemplateId
   * @param sender
   * @param subject
   * @param templateParams
   * @param messageIdentifierKey
   * @param messageIdentifierValue
   * @param bulkDownloadMailRecipient
   * @throws Exception
   */
  void sendBulkDownloadEmail(String mailTemplateId, String sender, String subject,
      Map<String, Object> templateParams, String messageIdentifierKey,
      String messageIdentifierValue, BulkDownloadMailRecipient bulkDownloadMailRecipient)
      throws Exception;

  /**
   * API to send bulk download email for error
   *
   * @param request
   */
  void sendBulkDownloadErrorMail(BulkDownloadRequest request);

  /**
   * API to send email for pending bulk requests
   *
   * @param mailSendTo
   * @param bulkPendingProductResponseList
   * @throws Exception
   */
  void sendPendingBulkRequestMail(String mailSendTo,
      List<BulkPendingProductResponse> bulkPendingProductResponseList,
      List<BulkPendingProductResponse> bulkInProgressProductResponseList,
      List<BulkPendingProductResponse> bulkAbortedProductResponseList, String abortedTime) throws Exception;

  /**
   * Send email on bulk internal process actions
   *
   * @param bulkInternalProcess
   * @param action
   */
  void sendEmailByForInternalBulkProcess(BulkInternalProcess bulkInternalProcess, String action);

  /**
   * Send mail
   *
   * @param emailParams
   * @param messageId
   * @param emailSubject
   * @param createdBy
   * @param requestCode
   */
  void sendMail(Map<String, Object> emailParams, String messageId, String emailSubject, String createdBy,
      String requestCode);
}

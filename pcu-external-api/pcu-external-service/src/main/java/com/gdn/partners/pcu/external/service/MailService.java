package com.gdn.partners.pcu.external.service;

public interface MailService {

  /**
   * API to send mail for merchant care for in-progress products
   *
   * @param businessPartnerCode
   * @return
   */
  void sendMail(String businessPartnerCode);

  /**
   * API to check mail visibility for merchant care for in-progress products
   *
   * @param businessPartnerCode
   * @return
   */
  boolean notifyMailVisibilityOptionForProductWip(String businessPartnerCode);
}

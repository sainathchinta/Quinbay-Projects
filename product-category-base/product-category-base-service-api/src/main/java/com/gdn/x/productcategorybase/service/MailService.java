package com.gdn.x.productcategorybase.service;

import java.util.Date;

public interface MailService {

  /**
   * send the configuration mail for category event by date
   *
   * @param date
   * @return
   */
  void sendConfigurationChangesMailForCategory(Date date) throws Exception;

  /**
   * send the configuration mail for merchant event by date
   *
   * @param date
   * @return
   */
  void sendConfigurationChangesMailForMerchant(Date date) throws Exception;
}

package com.gdn.mta.product.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.partners.pbp.entity.mailEvent.CategoryChangeMailEvent;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import com.gdn.x.businesspartner.dto.ProfileResponse;

public interface ProductMailEventService {
  /**
   * create and save product mail event
   * @param productCode
   * @param notes
   * @param event
   * @throws Exception
   */
  void createAndSaveMailEvent(String productCode, String notes, ProductMailEventsEnum event)
      throws Exception;

  /**
   * publish domain event for sent for correction
   * @param productCode
   * @param notes
   * @return
   * @throws Exception
   */
  void sendDomainEventForSentForCorrection(String productCode,
      String notes) throws Exception;

  /**
   * Add Category Chnage event in mail event db
   * @param productCollection
   * @param categoryChangeMailEvent
   * @throws Exception
   */
  void createAndSaveCategoryChangeMailEvent(ProductCollection productCollection,
      CategoryChangeMailEvent categoryChangeMailEvent) throws Exception;

  /**
   * Send Mail to business Partner
   * @param date
   */
  void sendProductMailEventsToBusinessPartners(Date date) throws Exception;


  /**
   *
   * @param date
   */
  void sendPostLiveReviewActiveProductMailEventsToBusinessPartners(Date date);


  /**
   *
   * @param date
   */
  void sendPostLiveReviewRejectProductMailEventsToBusinessPartners(Date date);

  /**
   * Send mail to business Partner on archiving itemSku
   *
   * @param emails
   * @param profileResponse
   * @throws Exception
   */
  void sendMailForArchivedItemSkuDueToOos(Map<String, List<List<String>>> emails, Map<String, ProfileResponse> profileResponse)
      throws Exception;

  /**
   * Get Final Cateory Change
   * @param storeId
   * @param gdnSku
   * @return
   * @throws Exception
   */
  CategoryChangeMailEvent getCategoryChangeMailEvent(String storeId, String gdnSku)
      throws Exception;

  /**
   * Create and save product mail event for suspended or re-activated products
   * @param event
   * @param merchantCode
   * @param notes
   * @param productCode
   * @param productSku
   */
  void createAndSaveMailEventForSuspensionOrReActivation(ProductMailEventsEnum event, String merchantCode, String notes,
      String productCode, String productSku) throws Exception;

  /**
   * Send mail to business partners for suspension or activation of a product
   * @param date
   */
  void sendProductMailEventsToBusinessPartnersForSuspension(Date date) throws Exception;

  /**
   * Scheduler to delete old records from product mail event repository in batches
   *
   * @param days    deletion to be done for records in table from past {days}
   */

  void deleteOldRecordsByDays(int days);
}

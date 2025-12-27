package com.gdn.x.mta.distributiontask.dao.api;

import com.gdn.x.mta.distributiontask.model.ProductEmails;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface ProductEmailsRepository extends JpaRepository<ProductEmails, String> {

  String DISTINCT_BP_CODE = "Select distinct p.business_partner_code from pdt_product_emails p "
    + "where p.store_id = ?1 and p.product_email_type = ?2 and p.status = ?3";

  String DELETE_MAIL_EVENTS = "DELETE FROM pdt_product_emails pe where pe.product_sku = ?1 and "
    + "pe.business_partner_code = ?2 and status = 'PENDING'";

  @Query(value = DISTINCT_BP_CODE, nativeQuery = true)
  List<String> findBusinessPartnerCodesAndProductEmailTypeAndStatus(String storeId,
    String productEmailType, String status);

  List<ProductEmails> findByStoreIdAndBusinessPartnerCodeAndProductEmailTypeAndStatus(
    String storeId, String businessPartnerCode, String productEmailType, String status);

  @Modifying
  @Query(value = ProductEmailsRepository.DELETE_MAIL_EVENTS, nativeQuery = true)
  int deleteMailRecord(String productSku, String businessPartnerCode);
}

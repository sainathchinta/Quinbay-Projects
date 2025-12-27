package com.gdn.x.mta.distributiontask.dao.api;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.mta.distributiontask.model.ReportProduct;

public interface ReportProductRepository extends JpaRepository<ReportProduct, String> {

  ReportProduct findByStoreIdAndMemberIdAndItemSkuAndReason(String storeId, String memberId, String itemSku, String reason);

}
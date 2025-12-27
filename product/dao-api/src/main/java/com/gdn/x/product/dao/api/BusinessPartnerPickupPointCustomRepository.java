package com.gdn.x.product.dao.api;

import java.util.List;
import java.util.Set;

import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;

public interface BusinessPartnerPickupPointCustomRepository {

  List<BusinessPartnerPickupPoint> findBusinessPartnerPickupPointData(String storeId, String businessPartnerCode,
      String keyword,  Boolean cncActivated,Boolean fbbActivated, Set<String> pickupPointSet);

  List<BusinessPartnerPickupPoint> findByBusinessPartnerCodeAndMarkForDeleteFalseAndArchivedFalse(String businessPartnerCode);
}

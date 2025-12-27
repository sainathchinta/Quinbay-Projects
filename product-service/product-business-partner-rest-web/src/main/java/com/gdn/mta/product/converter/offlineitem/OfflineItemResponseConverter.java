package com.gdn.mta.product.converter.offlineitem;

import com.gdn.partners.pbp.dto.offlineitem.OfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemResponseDetailResponse;
import com.gdn.partners.pbp.model.offlineitem.OfflineItemDetail;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;

import java.util.List;

public interface OfflineItemResponseConverter {
  List<OfflineItemDetailResponse> convertOfflineItemDetailResponses(
      List<OfflineItemDetail> offlineItemDetails);

  List<OfflineItemResponseDetailResponse> convertOfflineItemResponseDetailResponses(
      List<OfflineItemResponseDetail> offlineItemResponseDetails);
}

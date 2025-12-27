package com.gdn.mta.product.converter.offlineitem;

import com.gdn.partners.pbp.dto.offlineitem.OfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemResponseDetailResponse;
import com.gdn.partners.pbp.model.offlineitem.OfflineItemDetail;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import org.apache.commons.collections4.CollectionUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class OfflineItemResponseConverterBean implements OfflineItemResponseConverter {

  @Override
  public List<OfflineItemDetailResponse> convertOfflineItemDetailResponses(
      List<OfflineItemDetail> offlineItemDetails) {
    List<OfflineItemDetailResponse> responses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(offlineItemDetails)) {
      for (OfflineItemDetail offlineItemDetail : offlineItemDetails) {
        OfflineItemDetailResponse response = new OfflineItemDetailResponse();
        BeanUtils.copyProperties(offlineItemDetail, response);
        responses.add(response);
      }
    }
    return responses;
  }

  @Override
  public List<OfflineItemResponseDetailResponse> convertOfflineItemResponseDetailResponses(
      List<OfflineItemResponseDetail> offlineItemResponseDetails) {
    List<OfflineItemResponseDetailResponse> responses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(offlineItemResponseDetails)) {
      for (OfflineItemResponseDetail offlineItemResponseDetail : offlineItemResponseDetails) {
        OfflineItemResponseDetailResponse response = OfflineItemResponseDetailResponse.builder().build();
        BeanUtils.copyProperties(offlineItemResponseDetail, response);
        responses.add(response);
      }
    }
    return responses;
  }
}

package com.gdn.mta.bulk.repository;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import com.gdn.mta.bulk.feignConfig.PBPFeign;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.download.InstantPickupProductDownloadRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupRequest;

@Repository
public class ProductInstantPickupRepositoryBean implements ProductInstantPickupRepository {

  private static final String DEFAULT_USERNAME = "com.gdn.mta";

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public List<OfflineItemInstantPickupBulkDownloadResponse> findSummaryInstantPickupBulkDownload(
      InstantPickupProductDownloadRequest instantPickupProductDownloadRequest, int pageNumber,
      int pageSize) throws Exception {
    OfflineItemInstantPickupRequest offlineItemInstantPickupRequest = new OfflineItemInstantPickupRequest();
    BeanUtils.copyProperties(instantPickupProductDownloadRequest, offlineItemInstantPickupRequest);
    offlineItemInstantPickupRequest.setMerchantCode(instantPickupProductDownloadRequest.getBusinessPartnerCode());
    GdnRestListResponse<OfflineItemInstantPickupBulkDownloadResponse> response =
        pbpFeign.filterSummaryInstantPickupBulkDownload(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId())
                ? UUID.randomUUID().toString()
                : GdnMandatoryRequestParameterUtil.getRequestId(),
            StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ? DEFAULT_USERNAME
                : GdnMandatoryRequestParameterUtil.getUsername(),
            offlineItemInstantPickupRequest.getMerchantCode(),
            offlineItemInstantPickupRequest.getPickupPointCode(), pageNumber, pageSize);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.valueOf(response.getErrorCode()),
          response.getErrorMessage());
    }
    return Optional.of(response).map(GdnRestListResponse::getContent)
        .orElse(Collections.emptyList());
  }

}

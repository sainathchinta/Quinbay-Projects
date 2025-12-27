package com.gdn.mta.bulk.repository;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import com.gdn.mta.bulk.feignConfig.PBPFeign;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import com.gda.mta.product.dto.BrandAndCategoryItemSummaryRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.SuspensionProductRequestList;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;


@Repository
public class ProductLevel3RepositoryBean implements ProductLevel3Repository {

  @Autowired
  private PBPFeign pbpFeign;

  private static final String SYSTEM_USER = "Merchant API";


  @Override
  public BulkDownloadProductLevel3Response findSummaryByFilterForBulkDownload(
      String businessPartnerCode, Pageable pageable, ProductLevel3SummaryRequest productRequest) throws Exception {
    String requestId;
    if (StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId())) {
      requestId = UUID.randomUUID().toString();
    } else {
      requestId = GdnMandatoryRequestParameterUtil.getRequestId();
    }
    String userName;
    if (StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername())) {
      userName = SYSTEM_USER;
    } else {
      userName = GdnMandatoryRequestParameterUtil.getUsername();
    }
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response =
        pbpFeign.filterSummaryForBulkDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), requestId, userName,
            businessPartnerCode, pageable.getPageNumber(), pageable.getPageSize(), productRequest);
    if (!response.isSuccess()) {
      throw new Exception("PBP client error: " + response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<ProductLevel3SummaryResponse> findSummaryByCategoryAndBrand(int pageNumber,
      int pageSize, CampaignProductDownloadRequest campaignProductDownloadRequest)
      throws Exception {
    BrandAndCategoryItemSummaryRequest request = new BrandAndCategoryItemSummaryRequest();
    request.setBrands(campaignProductDownloadRequest.getCampaignItemSummaryRequest().getBrands());
    request.setCategories(campaignProductDownloadRequest.getCampaignItemSummaryRequest().getCategories());
    request.setMerchantCode(campaignProductDownloadRequest.getCampaignItemSummaryRequest().getMerchantCode());
    GdnRestListResponse<ProductLevel3SummaryResponse> response =
        pbpFeign.filterSummaryByCategoryAndBrand(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            campaignProductDownloadRequest.getRequestId(), null, pageNumber, pageSize, null, null,
            request);
    return Optional.ofNullable(response).map(GdnRestListResponse::getContent)
        .orElseGet(() -> Collections.emptyList());
  }

  @Override
  public List<SuspensionProductResponse> doBulkSuspensionProductsActions(
      List<SuspensionProductRequest> suspensionProductRequests, String requestId, String username) throws Exception {
    SuspensionProductRequestList suspensionProductRequestList = new SuspensionProductRequestList();
    suspensionProductRequestList.setSuspensionProductRequestList(suspensionProductRequests);
    GdnRestListResponse<SuspensionProductResponse> response =
        pbpFeign.doBulkSuspensionProductsActions(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), requestId, username,
            suspensionProductRequestList);
    if (Objects.isNull(response) || !response.isSuccess()) {
      String errorCode;
      if(StringUtils.isEmpty(response.getErrorCode())) {
        errorCode = ErrorCategory.UNSPECIFIED.toString();
      } else {
        errorCode = response.getErrorCode();
      }
      throw new ApplicationException(ErrorCategory.valueOf(errorCode),
          "PBP error for bulk suspension: " + response.getErrorMessage());
    }
    return response.getContent();
  }
}

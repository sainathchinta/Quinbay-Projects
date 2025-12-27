package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.models.FailedRebateResponse;
import com.gdn.mta.bulk.models.SkuRebateUpdateRequest;
import com.gdn.mta.bulk.models.SkuResponse;
import com.gdn.mta.bulk.models.download.BulkPriceRecommendationDownloadRequest;
import com.gdn.mta.bulk.models.download.UpdateRemoveProductTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.FailedReasonResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.dto.TaggedProductFilterResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PriceAnalyticsFeign;
import com.gdn.mta.bulk.models.download.responsedata.BulkRebateResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkRebateUpdateRequest;
import com.gdn.mta.bulk.models.download.responsedata.DownloadSkuResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class PriceAnalyticsServiceImpl implements PriceAnalyticsOutboundService {

  public static final int DEFAULT_PAGE = 0;
  public static final String CANNOT_EXCEED_THE_ROW_LIMIT = "Max %d Row limit Exceeded. ";
  public static final String NO_TAGGED_PRODUCTS_ARE_PRESENT_FOR_GIVEN_REQUEST =
      "No tagged Products are present for given request";
  @Autowired
  private PriceAnalyticsFeign priceAnalyticsFeign;

  @Value("${bulk.price.recommendation.batch.size}")
  public int bulkPriceRecommendationBatchSize;

  @Value("${bulk.product.type.tagging.batch.size}")
  public int bulkProductTypeTaggingBatchSize;

  @Value("${bulk.price.recommendation.excel.rows.size}")
  public int getBulkPriceRecommendationExcelRowsSize;

  @Value("${bulk.tagged.products.excel.rows.size}")
  public int getBulkTaggedProductsExcelRowsSize;


  @Override
  public List<DownloadSkuResponse> getDownloadSkus(String brEmailAddress,
      BulkPriceRecommendationDownloadRequest bulkPriceRecommendationDownloadRequest) {
    List<DownloadSkuResponse> downloadSkuResponseList = new ArrayList<>();
    int currentPage = DEFAULT_PAGE;
    long totalRecords = 0;
    do {
      GdnRestListResponse<DownloadSkuResponse> response =
          priceAnalyticsFeign.getSkusForDownload(GdnMandatoryRequestParameterUtil.getStoreId(),
              GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
              GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
              brEmailAddress, currentPage, bulkPriceRecommendationBatchSize, bulkPriceRecommendationDownloadRequest);
      if (!response.isSuccess() || Objects.isNull(response.getContent())) {
        log.error(
            "Exception while getting response for brEmailAddress : {}, bulkPriceRecommendationDownloadRequest : {}, error : {}",
            brEmailAddress, bulkPriceRecommendationDownloadRequest, response.getErrorMessage());
        throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
            ErrorCategory.COMMUNICATION_FAILURE.getMessage());
      }
      downloadSkuResponseList.addAll(response.getContent());
      totalRecords = response.getPageMetaData().getTotalRecords();
      if (downloadSkuResponseList.size() > getBulkPriceRecommendationExcelRowsSize) {
        downloadSkuResponseList = downloadSkuResponseList.subList(0, getBulkPriceRecommendationExcelRowsSize);
        break;
      }
      currentPage++;
    } while (downloadSkuResponseList.size() < totalRecords);

    return downloadSkuResponseList;
  }

  @Override
  public List<TaggedProductFilterResponse> getDownloadTaggedProducts(
      TaggedProductFilterRequest taggedProductFilterRequest) {
    List<TaggedProductFilterResponse> allTaggedProducts = new ArrayList<>();
    int currentPage = DEFAULT_PAGE;
    long totalRecords = 0;
    do {
      GdnRestListResponse<TaggedProductFilterResponse> response =
          priceAnalyticsFeign.getTaggedProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
              GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
              GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
              currentPage, bulkProductTypeTaggingBatchSize, taggedProductFilterRequest);
      if (!response.isSuccess()) {
        log.error("Exception while getting response from pricing for request : {}", taggedProductFilterRequest);
        throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
            ErrorCategory.COMMUNICATION_FAILURE.getMessage());
      }
      if(CollectionUtils.isEmpty(response.getContent())){
        log.error("No tagged products for request : {} ", taggedProductFilterRequest);
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
            NO_TAGGED_PRODUCTS_ARE_PRESENT_FOR_GIVEN_REQUEST);
      }
      allTaggedProducts.addAll(response.getContent());
      totalRecords = response.getPageMetaData().getTotalRecords();
      if (allTaggedProducts.size() > getBulkTaggedProductsExcelRowsSize) {
        allTaggedProducts = allTaggedProducts.subList(0, getBulkTaggedProductsExcelRowsSize);
        break;
      }
      currentPage++;
    } while (allTaggedProducts.size() < totalRecords);
    return allTaggedProducts;
  }

  @Override
  public String updatePriceRebate(BulkRebateUpdateRequest bulkRebateUpdateRequest, String officerEmailAddress) {
    GdnRestSingleResponse<BulkRebateResponse> response = priceAnalyticsFeign
        .updateRebate(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getUsername(),
            GdnMandatoryRequestParameterUtil.getRequestId(), officerEmailAddress, bulkRebateUpdateRequest);
    if (!response.isSuccess()) {
      log.error("Exception while updating Rebate with email {} and request {} ", officerEmailAddress,
          bulkRebateUpdateRequest);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getValue().getFailedReason();
  }

  @Override
  public List<FailedReasonResponse> updateTagging(String officerEmailAddress,
    List<UpdateRemoveProductTaggingRequest> updateRemoveProductTaggingRequest) {
    log.info("Calling promo analytics for updateTagging for : {} for updateTaggingRequests : {} ",
      officerEmailAddress, updateRemoveProductTaggingRequest);
    GdnRestListResponse<FailedReasonResponse> response =
      priceAnalyticsFeign.updateTagging(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
        officerEmailAddress, updateRemoveProductTaggingRequest);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      log.error("Exception while getting response for officerEmailAddress  : {}", officerEmailAddress);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getContent();
  }

  public List<FailedReasonResponse> removeTagging(String officerEmailAddress,
    List<UpdateRemoveProductTaggingRequest> removeTaggingRequests) {
    log.info("Calling promo analytics for removeTagging for : {} for removeTaggingRequests : {} ",
      officerEmailAddress, removeTaggingRequests);
    GdnRestListResponse<FailedReasonResponse> response =
      priceAnalyticsFeign.removeTagging(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getRequestId(),
        officerEmailAddress, removeTaggingRequests);
    if (!response.isSuccess() || Objects.isNull(response.getContent())) {
      log.error("Exception while getting response for officerEmailAddress  : {}", officerEmailAddress);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getContent();
  }

  @Override
  public String updateSkuRebate(SkuRebateUpdateRequest skuRebateUpdateRequest) {
    GdnRestSingleResponse<FailedRebateResponse> response = priceAnalyticsFeign.updateSkuRebate(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        skuRebateUpdateRequest);
    log.info("PriceAnalyticsServiceImpl.updateSkuRebate: Fetched response from price-analytics: {} for request: {}, username: {}",
        skuRebateUpdateRequest, response, GdnMandatoryRequestParameterUtil.getUsername());
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Exception while updating sku rebate for request: {} and username: {} ", skuRebateUpdateRequest,
          GdnMandatoryRequestParameterUtil.getUsername());
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getValue().getFailedReason();
  }

  @Override
  public Set<String> getOfficerTaggedSkus(Set<String> offlineItemIds) {
    GdnRestListResponse<SkuResponse> response = priceAnalyticsFeign.getOfficerTaggedSkus(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        offlineItemIds);
    log.info("PriceAnalyticsServiceImpl.getOfficerTaggedSkus: Fetched response from price-analytics: {} for request: {}, username: {}",
        offlineItemIds, response, GdnMandatoryRequestParameterUtil.getUsername());
    if (Objects.isNull(response) || !response.isSuccess()) {
      log.error("Exception while getting officer tagged skus for request: {} and username: {} ", offlineItemIds,
          GdnMandatoryRequestParameterUtil.getUsername());
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getContent().stream().map(SkuResponse::getItemPickupPointId).collect(Collectors.toSet());

  }
}

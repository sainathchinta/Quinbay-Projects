package com.gdn.mta.bulk.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.MasterSkuReviewFeign;
import com.gdn.mta.bulk.models.download.ClusterReviewFeedbackRequest;
import com.gdn.mta.bulk.models.download.DownloadItemsRequest;
import com.gdn.mta.bulk.models.download.responsedata.ClusterActionResponse;
import com.gdn.mta.bulk.models.download.responsedata.ItemSkuAndMasterSkuResponse;
import com.gdn.mta.bulk.models.download.responsedata.ItemsDownloadResponse;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import lombok.extern.slf4j.Slf4j;

@Repository
@Slf4j
public class MasterSkuItemsRepositoryBean implements MasterSkuItemsRepository {


  @Autowired
  private MasterSkuReviewFeign masterSkuReviewFeign;

  @Value("${master.sku.review.items.fetch.batch.size}")
  public int masterSkuReviewItemsFetchBatchSize;

  @Override
  public ItemsDownloadResponse getItemsForMasterSkuReview(DownloadItemsRequest request) throws Exception {
    List<ItemSkuAndMasterSkuResponse> resultList = new ArrayList<>();
    GdnRestSingleResponse<ItemsDownloadResponse> response = null;
    //for downloading all items with filters and batchSize
    if (CollectionUtils.isEmpty(request.getItemSkuList())) {
      int rangeLimit = request.getLimit();
      int startPage = (request.getLimit() * request.getPage()) / masterSkuReviewItemsFetchBatchSize;
      request.setLimit(masterSkuReviewItemsFetchBatchSize);
      do {
        request.setPage(startPage);
        response = fetchItemsForMasterSkuReviewAndValidateResponse(request, resultList);
        startPage++;
      } while (CollectionUtils.isNotEmpty(response.getValue().getItemSkuAndMasterSkuResponseList())
          && resultList.size() < rangeLimit);
    }
    // for selected items download
    else {
      fetchItemsForMasterSkuReviewAndValidateResponse(request, resultList);
    }
    return new ItemsDownloadResponse(resultList);
  }

  @Override
  public ClusterActionResponse performClusterAction(String masterSku,
      ClusterReviewFeedbackRequest clusterReviewFeedbackRequest, String username) throws Exception {
    GdnRestSingleResponse<ClusterActionResponse> response =
        masterSkuReviewFeign.performClusterReviewAction(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), username, masterSku,
            clusterReviewFeedbackRequest);
    validateClientResponse(response);
    return response.getValue();
  }

  private GdnRestSingleResponse<ItemsDownloadResponse> fetchItemsForMasterSkuReviewAndValidateResponse(
      DownloadItemsRequest request,
      List<ItemSkuAndMasterSkuResponse> resultList) throws Exception {
    GdnRestSingleResponse<ItemsDownloadResponse> response =
        masterSkuReviewFeign.downloadItemsForMasterSkuReview(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), request);
    validateClientResponse(response);
    resultList.addAll(response.getValue().getItemSkuAndMasterSkuResponseList());
    return response;
  }

  private void validateClientResponse(GdnRestSingleResponse response) throws ApplicationException {
    if (Objects.isNull(response)) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    if (!response.isSuccess() && ErrorCategory.VALIDATION.getCode().equals(response.getErrorCode())) {
      throw new ApplicationException(ErrorCategory.VALIDATION, response.getErrorMessage());
    } else if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, ErrorCategory.UNSPECIFIED.getMessage());
    }
  }
}
